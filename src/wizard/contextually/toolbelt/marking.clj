(ns wizard.contextually.toolbelt.marking
  (:use wizard.toolbelt)
  (:refer-clojure :exclude [var]))

(defn- mark-for-exferring
  [obj]
  (vary-meta obj assoc :exfer? true))

(defn exferrence?
  [obj]
  (= true (:exfer? (meta obj))))

(defn- mark-for-ctx-informing
  [obj]
  (mark-for-exferring
   (vary-meta obj assoc :informing? true)))

(defn informing-exferrence?
  [obj]
  (= true (:informing? (meta obj))))

(defn value
  [ctx-val]
  (mark-for-exferring {:ctx-val ctx-val}))

(defn exfer
  [& args]
  (let [exfer    (last args)
        ctx-vals (drop-last 1 args)]
    (mark-for-exferring {:ctx-vals ctx-vals :exfer exfer})))

(defn exfer-on
  [exferrence & args]
  (mark-for-exferring
   (assoc (apply exfer args) :based-on exferrence)))

(defn inform
  [informing & args]
  (mark-for-ctx-informing
   (assoc (apply exfer args) :informing informing)))

(defn ctx-val?
  [thing]
  (or (keyword? thing)
      (and (vector? thing)
           (every? ctx-val? thing))))

(defn- index-exfer-parts
  [pred xs]
  (let [xs (filter-vals pred xs)]
    (map vec ((juxt keys vals) xs))))

(defn exfer-all
  [& xs]
  (let [[xs exfer-fn] [(drop-last xs) (last xs)]
        all-xs        (into {} (map-indexed #(vector %1 %2) xs))

        [ctx-vals-index ctx-vals] (index-exfer-parts ctx-val? all-xs)
        [bases-index bases]       (index-exfer-parts
                                   (complement ctx-val?) all-xs)]
    (apply
     exfer-on
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

(defn- has-fallback?
  [conds]
  (or (and (= 2 (count conds)) (fn? (first conds)))
      (odd? (count conds))))

(defn- default-variator
  [variable conds]
  (let [[conds fallback]
        (if (has-fallback? conds)
          [(drop-last conds) (last conds)]
          [conds nil])]
    (or
     (->> conds
          (partition 2)
          (some
           (fn [[cond result]]
             (when (= variable cond)
               (if (fn? result) (result variable) result)))))
     fallback)))

(defn variate-by
  [& vars-then-conds]
  (let [[variables conds] [(drop-last vars-then-conds)
                           (last vars-then-conds)]]
    (apply
     exfer-all
     (append variables
             (fn [& variables]
               (if (fn? conds)
                 (apply conds variables)
                 (let [v (if (= 1 (count variables))
                           (first variables)
                           (vec variables))]
                   (default-variator v conds))))))))
