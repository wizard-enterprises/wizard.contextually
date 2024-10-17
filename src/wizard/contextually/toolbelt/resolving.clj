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
                         (map keyword it)
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
                            :source      source}))))))))

(defn- resolve-resolvable-in-ctx
  {:style/indent [1 :form]}
  [ctx {:keys [exfer args exferrence-resolver]
        :or   {exfer identity exferrence-resolver #(%)}
        :as   resolvable}]
  (exferrence-resolver
   #(apply
    exfer
    (map (partial (:walk-and-resolve-when-resolvable ctx) ctx) args))))

(defn- resolve-ctx-informing-resolvable
  {:style/indent [1 :form]}
  [ctx {:keys [informing] :as r}]
  (swap! (fetch-in ctx [::informings]) append informing)
  (resolve-resolvable-in-ctx ctx r))

(defn- walk-and-resolve-when-resolvable
  {:style/indent [1 :form]}
  [ctx form]
  (let [ctx (-> ctx (assoc :walk-and-resolve-when-resolvable
                           walk-and-resolve-when-resolvable))]
    (->> form
         (walk/prewalk
          (fn [x]
            (cond
              (marking/value? x)
              (walk-and-resolve-when-resolvable ctx
                (resolve-ctx-val-in-ctx ctx (:ctx-val x)))

              (marking/informing-exferrence? x)
              (let [ctx (inform-ctx ctx (:informing x))]
                (walk-and-resolve-when-resolvable ctx
                  (resolve-ctx-informing-resolvable ctx x)))

              (marking/variating-exferrence? x)
              (walk-and-resolve-when-resolvable ctx
                (resolve-variating-resolvable-in-ctx ctx x))

              (marking/exferrence? x)
              (walk-and-resolve-when-resolvable ctx
                (resolve-resolvable-in-ctx ctx x))

              :else x))))))

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
