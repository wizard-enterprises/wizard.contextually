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

(defn exfer-on
  [& exfer-fn+args]
  (let [[exfer-fn args] (if (fn? (first exfer-fn+args))
                          [(first exfer-fn+args) (rest exfer-fn+args)]
                          [nil exfer-fn+args])]
    (mark-for-exferring
     (cond-> {:args args}
       (some? exfer-fn) (assoc :exfer exfer-fn)))))

(defn value
  [ctx-val]
  (mark-as-value {:ctx-val ctx-val}))

(defn ctx-val-path?
  [thing]
  (or (keyword? thing)
      (and (vector? thing)
           (every? keyword? thing))))

(defn exfer
  [& args]
  (apply
   exfer-on
   (map
    #(if (ctx-val-path? %) (value %) %)
    args)))

(defn inform
  [informing & args]
  (mark-for-ctx-informing
   (assoc (apply exfer args) :informing informing)))

(defn with-exferrence-resolver
  [resolver exf]
  (-> exf
      (assoc :exferrence-resolver resolver)
      (with-meta (meta exf))))
