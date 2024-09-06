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

(defn var
  [ctx-var]
  (mark-for-exferring {:ctx-var ctx-var}))

(defn exfer
  [& args]
  (let [exfer    (last args)
        ctx-vars (drop-last 1 args)]
    (mark-for-exferring {:ctx-vars ctx-vars :exfer exfer})))

(defn exfer-on
  [exferrence & args]
  (mark-for-exferring
   (assoc (apply exfer args) :based-on exferrence)))

(defn inform
  [informing & args]
  (mark-for-ctx-informing
   (assoc (apply exfer args) :informing informing)))
