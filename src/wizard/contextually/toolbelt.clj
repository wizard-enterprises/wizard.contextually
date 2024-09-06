(ns wizard.contextually.toolbelt
  (:use wizard.toolbelt)
  (:require [wizard.contextually.toolbelt
             [marking :as marking]
             [resolving :as resolving]]))

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

(defn var
  [ctx-var]
  (marking/var ctx-var))

(defn exfer
  [& args]
  (apply marking/exfer args))

(defn exfer-on
  [exferrence & args]
  (apply marking/exfer-on exferrence args))

(defn inform
  [informing & args]
  (apply marking/inform informing args))
