(ns wizard.contextually.toolbelt.exferring
  (:use wizard.toolbelt wizard.contextually.toolbelt.marking)
  (:require [clojure.math.combinatorics :as combo]))

(defn variate-by
  [variables variations]
  (mark-for-variating
   {:variations variations
    :variables (cond-> variables (not (vector? variables)) vector)}))

;; (defn variate-on
;;   [variating variations]
;;   (let [variating ~variating
;;         variables (into {} (map-indexed vector (:variables variating)))
;;         variables (merge variables ~variations)
;;         variating (-> ~variating
;;                       (assoc :variables (into [] (vals variables)))
;;                       mark-for-variating)]
;;     variating))

;; (defn variate-on-each-of
;;   [variations variating base reduce-variant-fn]
;;   (let [variations (into [] variations);; (into [] (combo/cartesian-product variations))
;;         ]
;;     (apply
;;      exfer
;;      (fn [& variants]
;;        (reduce reduce-variant-fn base variants))
;;      (map (partial variate-on variating) variations))))
