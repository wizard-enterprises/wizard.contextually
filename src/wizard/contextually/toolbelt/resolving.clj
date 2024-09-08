(ns wizard.contextually.toolbelt.resolving
  (:use wizard.toolbelt)
  (:require [wizard.contextually.toolbelt
             [marking :as marking]
             [exferring :as exferring]]
            [clojure
             [string :as str]
             [walk :as walk]]))

(defn- ctx-val-path
  [ctx-val]
  (cond
    (keyword? ctx-val) [ctx-val]
    (string? ctx-val)  (it-> ctx-val
                         (str/split it #"\.")
                         (map keyword it)
                         (vec it))
    :else              ctx-val))

(defn- ctx-val-name
  [ctx-val]
  (cond
    (string? ctx-val)  ctx-val
    (keyword? ctx-val) (name ctx-val)
    :else              (->> ctx-val
                            (map name)
                            (str/join "."))))

(defn- inform-ctx
  ([ctx informing & more]
   (apply inform-ctx (inform-ctx ctx informing) more))
  ([ctx informing]
   (reduce-kv
    (fn [ctx k v]
      (update
       ((if (fn? v) update assoc) ctx k v)
       k #((:walk-and-resolve-when-resolvable ctx) ctx %)))
    ctx informing))
  ([ctx] ctx))

(defn- clean-ctx-of-intermediary-vals
  [ctx]
  (dissoc ctx ::informings :walk-and-resolve-when-resolvable))

(defn- resolve-ctx-val-in-ctx
  [ctx ctx-val]
  (if (= :ctx ctx-val)
    (clean-ctx-of-intermediary-vals ctx)
    (let [ctx-val (ctx-val-path ctx-val)
          source  (fetch-in ctx [(first ctx-val)])]
      (try (if (> (count ctx-val) 1)
             (fetch-in source (rest ctx-val))
             source)
           (catch clojure.lang.ExceptionInfo e
             (throw (ex-info (str "Could not resolve ctx value: " (ctx-val-name ctx-val))
                             (merge
                              (ex-data e)
                              {:source-name (first ctx-val)
                               :source      source}))))))))

(defn- resolve-resolvable-in-ctx
  [ctx {:keys [ctx-val ctx-vals exfer based-on]
        :or   {exfer identity}
        :as   resolvable}]
  (let [args (map #(resolve-ctx-val-in-ctx ctx %)
                  (or ctx-vals (when ctx-val [ctx-val])))
        args (if (contains? resolvable :based-on)
               (prepend based-on args) args)]
    (if (and (empty? args) (not (fn? exfer)))
      exfer
      (apply exfer args))))

(defn- resolve-ctx-informing-resolvable
  [ctx {:keys [informing] :as r}]
  (swap! (fetch-in ctx [::informings]) append informing)
  (resolve-resolvable-in-ctx ctx r))

(defn- default-variator
  [variable opts]
  (->> (if->> opts map? (into ()) (partition 2))
       (some
        (fn [[opt result]]
          (when (if (fn? opt) (opt variable) (= variable opt))
            (if (fn? result) (result variable) result))))))

(defn- resolve-variating-resolvable-in-ctx
  [ctx {:keys [variables opts]}]
   (resolve-resolvable-in-ctx
    ctx
    (marking/exfer-on
     ((:walk-and-resolve-when-resolvable ctx)
      ctx
      (resolve-resolvable-in-ctx
       ctx
       (apply exferring/exfer-all (append variables list))))
     (fn [[& variables]]
       (if (fn? opts)
         (apply opts variables)
         (let [v (if (= 1 (count variables))
                   (first variables)
                   (vec variables))]
           (default-variator v opts)))))))

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
         true                            (assoc :based-on base)
         (and (map? based-on)
              (contains? based-on :informing))
         (add-informing (:informing based-on)))])))

(defn- walk-and-resolve-when-resolvable
  [ctx form]
  (let [ctx (-> ctx (assoc :walk-and-resolve-when-resolvable
                           walk-and-resolve-when-resolvable))]
    (walk/prewalk
     (fn [x]
       (if-not (marking/exferrence? x)
         x
         (let [[ctx x] (resolve-base-throughout ctx x)]
           (cond
             (marking/informing-exferrence? x)
             (let [ctx (inform-ctx ctx (:informing x))]
               (walk-and-resolve-when-resolvable
                ctx (resolve-ctx-informing-resolvable ctx x)))

             (marking/variating-exferrence? x)
             (walk-and-resolve-when-resolvable
              ctx (resolve-variating-resolvable-in-ctx ctx x))

             (marking/exferrence? x)
             (walk-and-resolve-when-resolvable
              ctx (resolve-resolvable-in-ctx ctx x))))))
     form)))

(defn resolve-throughout
  ([ctx form]
   (resolve-throughout ctx {} form))
  ([ctx informing form]
   (let [ctx        (-> ctx
                        (assoc
                         ::informings (atom [])
                         :walk-and-resolve-when-resolvable
                         walk-and-resolve-when-resolvable))
         ctx        (inform-ctx ctx informing)
         resolved   (walk-and-resolve-when-resolvable ctx form)
         informings @(::informings ctx)
         ctx        (reduce inform-ctx ctx informings)
         ctx        (clean-ctx-of-intermediary-vals ctx)]
     [ctx resolved])))

(defn resolve-in
  [ctx form]
  (last (resolve-throughout ctx form)))
