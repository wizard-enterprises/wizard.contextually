(ns wizard.contextually.toolbelt
  (:use wizard.toolbelt)
  (:require [wizard.contextually.toolbelt
             [marking :as marking]
             [resolving :as resolving]]))

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

(defn fallback
  {:style/indent [:defn [:form]]}
  [fallback form]
  (marking/with-exferrence-resolver
    #(try (%) (catch Exception e fallback))
    (exfer-on #(or % fallback) form)))
