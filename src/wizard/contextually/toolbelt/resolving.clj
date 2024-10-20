(ns wizard.contextually.toolbelt.resolving
  (:use wizard.toolbelt)
  (:require [wizard.contextually.toolbelt
             [marking :as marking]]
            [clojure
             [string :as str]
             [walk :as walk]]))

(defn- ctx-val-path
  [ctx-val]
  (cond
    (keyword? ctx-val) [ctx-val]
    (string? ctx-val)  (it-> ctx-val
                         (str/split it #"\.")
                         (map #(try (Integer/parseInt %)
                                    (catch Exception e (keyword %)))
                              it)
                         (vec it))
    :else              ctx-val))

(defn- ctx-val-name
  [ctx-val]
  (->> ctx-val
       (map name)
       (str/join ".")))

(defn- inform-ctx
  [ctx informing]
  (reduce-kv
   (fn [ctx k v]
     (update
      ((if (fn? v) update assoc) ctx k v)
      k #((:walk-and-resolve-when-resolvable ctx) ctx %)))
   ctx informing))

(defn- clean-ctx-of-intermediary-vals
  [ctx]
  (dissoc ctx ::informings :walk-and-resolve-when-resolvable))

(defn- resolve-ctx-val-in-ctx
  {:style/indent [1 :form]}
  [ctx ctx-val]
  (let [ctx-val (cond-> ctx-val
                  (and (map? ctx-val) (contains? ctx-val :ctx-val)) :ctx-val)]
    (if (= :ctx ctx-val)
     (clean-ctx-of-intermediary-vals ctx)
     (let [ctx-val (ctx-val-path ctx-val)
           source  (fetch-in ctx [(first ctx-val)])]
       (try
         (if (> (count ctx-val) 1)
           (fetch-in source (rest ctx-val))
           source)
         (catch clojure.lang.ExceptionInfo e
           (throw (ex-info (str "Could not resolve ctx value: " (ctx-val-name ctx-val))
                           (merge
                            (ex-data e)
                            {:source-name (first ctx-val)
                             :source      source})))))))))

(defn- resolve-resolvable-in-ctx
  {:style/indent [1 :form]}
  [ctx {:keys [exfer args exferrence-resolver]
        :or   {exferrence-resolver #(%)}
        :as   resolvable}]
  (if (marking/value? resolvable) (do (spyx :whoops resolvable)
                                      (throw (ex-info "whoops" {:resolvable resolvable}))))
  (exferrence-resolver
   #(apply
    exfer
    (map (partial (:walk-and-resolve-when-resolvable ctx) ctx) args))))

(defn- resolve-ctx-informing-resolvable
  {:style/indent [1 :form]}
  [ctx {:keys [informing] :as r}]
  (swap! (fetch-in ctx [::informings]) append informing)
  (resolve-resolvable-in-ctx ctx r))

(defn- indexed-map->vector
  [m]
  (reduce-kv insert-up-to [] m))

(defn- variables-match-variation?
  [variables variation]
  (let [variation (cond-> variation (map? variation) indexed-map->vector)
        variables (cond-> variables (not (vector? variables)) vector)]
    (if (fn? variation)
      (variation variables)
      (= variables variation))))

(defn- default-variator
  [variables variations]
  (->> (if->> variations map? (into ()) (partition 2))
       (some
        (fn [[variation result]]
          (when (variables-match-variation? variables variation)
            (if (fn? result) (result variables) result))))))

(defn- resolve-variating-resolvable-in-ctx
  {:style/indent [1 :form]}
  [ctx {:keys [variables variations]}]
  (resolve-resolvable-in-ctx ctx
    (apply
     marking/exfer-on
     (fn [& variables]
       (let [v (if (= 1 (count variables))
                 (first variables)
                 (vec variables))]
         (default-variator v variations)))
     ((:walk-and-resolve-when-resolvable ctx) ctx
      (resolve-resolvable-in-ctx ctx
        (apply marking/exfer vector variables))))))

(defn- add-informing
  [x informing]
  (update x :informing
          #(append
            (case %
              nil?    []
              vector? %
              [%])
            informing)))

(defn- resolve-base-throughout
  [ctx x]
  (if-not (contains? x :based-on)
    [ctx x]
    (let [based-on  (:based-on x)
          inf-count (count @(::informings ctx))
          base      ((:walk-and-resolve-when-resolvable ctx) ctx based-on)
          ctx       (apply
                     inform-ctx ctx
                     (drop inf-count @(::informings ctx)))]
      [ctx
       (cond-> x
         true (assoc :based-on base)
         (and (map? based-on)
              (contains? based-on :informing))
         (add-informing (:informing based-on)))])))

(defn- resolver-for
  [x]
  (cond
    (marking/informing-exferrence? x)
    resolve-ctx-informing-resolvable

    (marking/variating-exferrence? x)
    resolve-variating-resolvable-in-ctx

    (marking/value? x)
    resolve-ctx-val-in-ctx

    (marking/exferrence? x)
    resolve-resolvable-in-ctx))

(defn- walk-and-resolve-when-resolvable
  {:style/indent [1 :form]}
  [ctx form]
  (->> form
       (walk/prewalk
        (fn [x]
          (if-not (marking/exferrence? x)
            x
            (let [[ctx x] (resolve-base-throughout ctx x)
                  ctx     (cond-> ctx
                            (marking/informing-exferrence? x)
                            (inform-ctx (:informing x)))]
              (walk-and-resolve-when-resolvable ctx
                ((resolver-for x) ctx x))))))))

(defn resolve-throughout
  [ctx form]
  (let [ctx        (-> ctx
                       (assoc
                        ::informings (atom [])
                        :walk-and-resolve-when-resolvable
                        walk-and-resolve-when-resolvable))
        resolved   (walk-and-resolve-when-resolvable ctx form)
        informings @(::informings ctx)
        ctx        (reduce inform-ctx ctx informings)
        ctx        (clean-ctx-of-intermediary-vals ctx)]
    [ctx resolved]))

(defn resolve-in
  [ctx form]
  (last (resolve-throughout ctx form)))
