(ns wizard.contextually
  (:use wizard.toolbelt)
  (:refer-clojure :exclude [var])
  (:require [clojure
             [string :as str]
             [walk :as walk]]))

(defn- mark-for-resolve
  [obj]
  (vary-meta obj assoc :resolve? true))
(defn- marked-for-resolve?
  [obj]
  (= true (:resolve? (meta obj))))

(defn- mark-for-ctx-informing
  [obj]
  (mark-for-resolve
   (vary-meta obj assoc :informing? true)))
(defn- marked-for-ctx-informing?
  [obj]
  (= true (:informing? (meta obj))))

(defn var
  [ctx-var]
  (mark-for-resolve {:ctx-var ctx-var}))

(defn exfer
  [& args]
  (let [exfer    (last args)
        ctx-vars (drop-last 1 args)]
    (mark-for-resolve {:ctx-vars ctx-vars :exfer exfer})))

(defn exfer-on
  [exferrence & args]
  (mark-for-resolve
   (assoc (apply exfer args) :based-on exferrence)))

(defn inform
  [informing & args]
  (mark-for-ctx-informing
   (assoc (apply exfer args) :informing informing)))

(defn- ctx-var-path
  [ctx-var]
  (cond
    (keyword? ctx-var) [ctx-var]
    (string? ctx-var)  (it-> ctx-var
                         (str/split it #"\.")
                         (map keyword it)
                         (vec it))
    :else              ctx-var))

(defn- ctx-var-name
  [ctx-var]
  (cond
    (string? ctx-var)  ctx-var
    (keyword? ctx-var) (name ctx-var)
    :else              (->> ctx-var
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

(defn- resolve-ctx-var-in-ctx
  [ctx ctx-var]
  (if (= :ctx ctx-var)
    (clean-ctx-of-intermediary-vals ctx)
    (let [ctx-var (ctx-var-path ctx-var)
          source  (fetch-in ctx [(first ctx-var)])]
      (try (if (> (count ctx-var) 1)
             (fetch-in source (rest ctx-var))
             source)
           (catch clojure.lang.ExceptionInfo e
             (throw (ex-info (str "Could not resolve ctx var: " (ctx-var-name ctx-var))
                             (merge
                              (ex-data e)
                              {:source-name (first ctx-var)
                               :source      source}))))))))

(defn- resolve-resolvable-in-ctx
  [ctx {:keys [ctx-var ctx-vars exfer based-on]
        :or   {exfer identity}
        :as   resolvable}]
  (let [args (map #(resolve-ctx-var-in-ctx ctx %)
                  (or ctx-vars [ctx-var]))
        args (if (contains? resolvable :based-on)
               (prepend based-on args) args)]
    (apply exfer args)))

(defn- resolve-ctx-informing-resolvable
  [ctx {:keys [informing] :as r}]
  (swap! (fetch-in ctx [::informings]) append informing)
  (resolve-resolvable-in-ctx ctx r))

(defn- add-informing
  [x informing]
  (update x :informing
          #(append
            (case %
              nil?    []
              vector? %
              [%])
            informing)))

(defn- inform-ctx-based-on
  [ctx x]
  (if-not (and (marked-for-resolve? x) (contains? x :based-on))
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
  (let [ctx (assoc ctx :walk-and-resolve-when-resolvable
                   walk-and-resolve-when-resolvable)]
    (walk/prewalk
     (fn [x]
       (let [[ctx x] (inform-ctx-based-on ctx x)]
         (if-not (marked-for-resolve? x)
           x
           (cond
             (marked-for-ctx-informing? x)
             (let [ctx (inform-ctx ctx (:informing x))]
               (walk-and-resolve-when-resolvable
                ctx (resolve-ctx-informing-resolvable ctx x)))

             (marked-for-resolve? x)
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
                         :walk-and-resolve-when-resolvable walk-and-resolve-when-resolvable))
         ctx        (inform-ctx ctx informing)
         resolved   (walk-and-resolve-when-resolvable ctx form)
         informings @(::informings ctx)
         ctx        (reduce inform-ctx ctx informings)
         ctx        (clean-ctx-of-intermediary-vals ctx)]
     {:ctx      ctx
      :resolved resolved})))

(defn resolve-in
  [ctx form]
  (:resolved (resolve-throughout ctx form)))
