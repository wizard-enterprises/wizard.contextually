(ns wizard.contextually.toolbelt.marking
  (:use wizard.toolbelt))

(defn mark-for-exferring [obj]
  (-> obj (vary-meta assoc ::exfer? true)))

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

(defn value [ctx-val]
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
