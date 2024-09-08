(ns wizard.contextually.toolbelt.exferring
  (:use wizard.toolbelt)
  (:require [wizard.contextually.toolbelt.marking :as marking]
            [clojure.walk :as walk]))

(defn ctx-val-path?
  [thing]
  (or (keyword? thing)
      (and (vector? thing)
           (every? ctx-val-path? thing))))

(defn- index-exfer-parts
  [pred xs]
  (let [xs (filter-vals pred xs)]
    (map vec ((juxt keys vals) xs))))

(defn exfer-all
  [& xs]
  (let [[xs exfer-fn] [(drop-last xs) (last xs)]
        all-xs        (into {} (map-indexed #(vector %1 %2) xs))

        [ctx-vals-index ctx-vals] (index-exfer-parts ctx-val-path? all-xs)
        [bases-index bases]       (index-exfer-parts
                                   (complement ctx-val-path?) all-xs)]
    (apply
     marking/exfer-on
     `(~bases
       ~@ctx-vals
       ~(fn [[& bases] & ctx-vals]
          (->> (merge
                (into {} (zip bases-index bases))
                (into {} (zip ctx-vals-index ctx-vals)))
               (sort-by key)
               vals
               (into [])
               (apply exfer-fn)))))))

(defn variate-by
  [& vars-then-opts]
  (let [opts (last vars-then-opts)
        variables (drop-last vars-then-opts)]
    (marking/mark-for-variating
     {:variables variables :opts opts})))

(defmacro variate-on
  [variator & variations]
  `(let [variations#
         ~(when variations
            (->> variations
                 (walk/prewalk #(if (= '_ %) ::_ %))
                 (map-indexed (fn [a b] [a b]))
                 (filter (comp (partial not= ::_) second))
                 (into {})))
         variator#  ~variator
         variables# (into {} (map-indexed vector (:variables variator#)))
         variables# (merge variables# variations#)
         variator#  (-> ~variator
                        (assoc :variables (into [] (vals variables#)))
                        marking/mark-for-variating)]
     variator#))
