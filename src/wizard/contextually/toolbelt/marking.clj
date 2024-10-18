(ns wizard.contextually.toolbelt.marking
  (:use wizard.toolbelt))

(defn mark-for-exferring [obj]
  (-> obj (vary-meta assoc ::exfer? true)))

(defn mark-as-value [x]
  (-> x
      mark-for-exferring
      (vary-meta assoc ::value? true)))

(defn value? [obj]
  (= true (::value? (meta obj))))

(defn exferrence? [obj]
  (= true (::exfer? (meta obj))))

(defn mark-for-ctx-informing [obj]
  (-> obj
   (vary-meta assoc ::informing? true)
   mark-for-exferring))

(defn informing-exferrence? [obj]
  (and (exferrence? obj)
       (= true (::informing? (meta obj)))))

(defn mark-for-variating [obj]
  (-> obj
      mark-for-exferring
      (vary-meta assoc ::variating? true)))

(defn variating-exferrence? [obj]
  (and (exferrence? obj)
       (= true (::variating? (meta obj)))))

(defn exfer-on
  [exf-fn & args]
  (let [[exf-fn args] (if (empty? args)
                        [identity (list exf-fn)]
                        [exf-fn args])]
    (mark-for-exferring
     {:args args :exfer exf-fn})))

(defn value
  [ctx-val]
  (mark-as-value {:ctx-val ctx-val}))

(defn ctx-val-path?
  [thing]
  (or (keyword? thing)
      (and (vector? thing)
           (keyword? (first thing))
           (every? (some-fn keyword? symbol? number?) thing))))

(defn exfer
  [exf-fn & args]
  (apply
   exfer-on
   exf-fn
   (map
    #(if (ctx-val-path? %) (value %) %)
    args)))

(defn inform
  [informing informed]
  (mark-for-ctx-informing
   (assoc (exfer identity informed) :informing informing)))

(defn with-exferrence-resolver
  [resolver exf]
  (-> exf
      (assoc :exferrence-resolver resolver)
      (with-meta (meta exf))))
