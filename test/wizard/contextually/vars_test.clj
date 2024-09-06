(ns wizard.contextually.vars-test
  (:use wizard.toolbelt.test.midje wizard.toolbelt)
  (:require [wizard.contextually :as ctx]))

(facts
 "about resolving ctx vars"
 (let [resolve (partial ctx/resolve-in {:foo 15})]
   (resolve (ctx/var :foo)) => 15
   (resolve (ctx/var "foo")) => 15
   (resolve {:a (ctx/var "foo")}) => {:a 15}
   (resolve [(ctx/var "foo")]) => [15]
   (resolve (ctx/var :ctx)) => {:foo 15})

 (ctx/resolve-in
  {:a 1 :b 2}
  {:a (ctx/var "a") :b (ctx/var "b")})
 => {:a 1 :b 2}

 (ctx/resolve-in
  {:a {:b {:c 15}}}
  {:x (ctx/var "a.b.c") :y (ctx/var [:a :b :c])})
 => {:x 15 :y 15}

 (ctx/resolve-in {:a 15} {:a (ctx/var [:a :b :c])})
 => (throws-ex-info "Could not resolve ctx var: a.b.c"
                    {:source-name :a
                     :source      15}))
