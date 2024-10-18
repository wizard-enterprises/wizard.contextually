(ns wizard.contextually.toolbelt
  (:use wizard.toolbelt)
  (:require [wizard.contextually.toolbelt
             [marking :as marking]
             [resolving :as resolving]
             [exferring :as exferring]]
            [clojure.walk :as walk]
            [clojure.math.combinatorics :as combo]))

(defn resolve-in
  "returns `form` as resolved in `ctx`"
  {:style/indent 1}
  [ctx form]
  (resolving/resolve-in ctx form))

(defn resolve-throughout
  "returns a vector with (1) `ctx` as informed by resolving (2) `form`"
  {:style/indent 1}
  [ctx form]
  (resolving/resolve-throughout ctx form))

(defn exferrence?
  "is `obj` in and of itself an exferrence?"
  [obj]
  (marking/exferrence? obj))

(defn informing-exferrence?
  "is `obj` in and of itself an informing exferrence?"
  [obj]
  (marking/informing-exferrence? obj))

(defn value
  "will exfer a value by name when resolved"
  [ctx-val]
  (marking/value ctx-val))

(defn exfer-on
  {:style/indent [:defn [:form]]}
  [exferrence & args]
  (apply marking/exfer-on exferrence args))

(defn exfer
  {:style/indent [1 [:form]]}
  [& args]
  (apply marking/exfer args))

(defn inform
  "takes a ctx informing, then `n` value paths and an exferring fn"
  {:style/indent [1 [:form]]}
  [informing & args]
  (apply marking/inform informing args))

(defn- norm-underscores
  [variations]
  (when variations
    (->> variations
         (walk/prewalk #(if (= '_ %) ::_ %)))))

(defn- norm-variations
  [variations]
  (when variations
    (->> variations
         norm-underscores
         (map-indexed (fn [a b] [a b]))
         (filter (comp #(and (not= % ::_) (not= % [::_])) second))
         (into {}))))

(defmacro variating
  {:style/indent [:form :form]}
  [& variation-variant-pairs]
  (norm-variations variation-variant-pairs))

(defmacro variate-by
  {:style/indent [:defn [:form]]}
  [& variables+variations]
  (let [[variables variations]
        [(drop-last variables+variations) (last variables+variations)]
        variations (->> variations
                        (map-indexed
                         (fn [i v]
                           (if (even? i)
                             (norm-variations (if (vector? v) v [v]))
                             v)))
                        (into []))]
    `(exferring/variate-by ~@variables ~variations)))

;; (defmacro variate-on
;;   {:style/indent [1 [:form]]}
;;   [variator & variations]
;;   `(exferring/variate-on ~variator ~(norm-variations variations)))

;; (defmacro variate-on-each-of
;;   [& variations+variating+base+reduce-variant-fn]
;;   (let [args                     variations+variating+base+reduce-variant-fn
;;         [args reduce-variant-fn] [(drop-last args) (take-last 1 args)]
;;         [args base]              (if (marking/variating-exferrence? (last args))
;;                                    [args nil]
;;                                    [(drop-last args) (take-last 1 args)])
;;         [variations variating]   [(drop-last args) (take-last 1 args)]
;;         variations (norm-underscores variations)
;;         variations (into [] (map norm-variations (spyx (apply combo/cartesian-product (map #(if (= % ::_) [::_] %) variations)))))]
;;     `(exferring/variate-on-each-of
;;       ~variations ~variating ~base ~reduce-variant-fn)))

(defn fallback
  {:style/indent [:defn [:form]]}
  [fallback form]
  (marking/with-exferrence-resolver
    #(try (%) (catch Exception e fallback))
    (exfer-on #(or % fallback) form)))
