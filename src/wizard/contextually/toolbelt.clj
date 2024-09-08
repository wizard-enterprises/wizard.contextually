(ns wizard.contextually.toolbelt
  (:use wizard.toolbelt)
  (:require [wizard.contextually.toolbelt
             [marking :as marking]
             [resolving :as resolving]
             [exferring :as exferring]]))

(defn resolve-in
  "returns `form` as resolved in `ctx`"
  [ctx form]
  (resolving/resolve-in ctx form))

(defn resolve-throughout
  "returns a vector with (1) `ctx` as informed by resolving (2) `form`"
  ([ctx form]
   (resolving/resolve-throughout ctx form))
  ([ctx informing form]
   (resolving/resolve-throughout ctx informing form)))

(defn exferrence?
  "is `obj` in and of itself an exferrence?"
  [obj]
  (marking/exferrence? obj))

(defn informing-exferrence?
  "is `obj` in and of itself an informing exferrence?"
  [obj]
  (marking/informing-exferrence? obj))

(defn variating-exferrence?
  "is `obj` in and of itself a variating exferrence?"
  [obj]
  (marking/variating-exferrence? obj))

(defn value
  "will exfer a value by name when resolved"
  [ctx-val]
  (marking/value ctx-val))

(defn exfer
  "takes `n` value paths, then an exferring fn taking `n` exferred values"
  [& args]
  (apply marking/exfer args))

(defn exfer-on
  "takes `exferrence` and `n` value paths, then an exferring fn a la `exfer`"
  [exferrence & args]
  (apply marking/exfer-on exferrence args))

(defn inform
  "takes a ctx informing, then `n` value paths and an exferring fn"
  [informing & args]
  (apply marking/inform informing args))

(defn variate-by
  [& variables-then-opts]
  (apply exferring/variate-by variables-then-opts))

(defmacro variate-on
  [variator & variations]
  `(exferring/variate-on ~variator ~@variations))

(defn exfer-all
  "takes `n` value paths and/or exferrences, then an exferring fn taking `n` args"
  [& things-then-exfer-fn]
  (apply exferring/exfer-all things-then-exfer-fn))

(defn fallback
  [fallback form]
  (exfer-on form #(or % fallback)))
