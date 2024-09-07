(ns wizard.contextually.vars-test
  (:use wizard.toolbelt.test.midje wizard.toolbelt)
  (:require [wizard.contextually :as ctx]))

(facts
 "about resolving ctx values"
 (let [resolve (partial ctx/resolve-in {:foo 15})]
   (resolve (ctx/value :foo)) => 15
   (resolve (ctx/value "foo")) => 15
   (resolve {:a (ctx/value "foo")}) => {:a 15}
   (resolve [(ctx/value "foo")]) => [15]
   (resolve (ctx/value :ctx)) => {:foo 15})

 (ctx/resolve-in
  {:a 1 :b 2}
  {:a (ctx/value "a") :b (ctx/value "b")})
 => {:a 1 :b 2}

 (ctx/resolve-in
  {:a {:b {:c 15}}}
  {:x (ctx/value "a.b.c") :y (ctx/value [:a :b :c])})
 => {:x 15 :y 15}

 (ctx/resolve-in {:a 15} {:a (ctx/value [:a :b :c])})
 => (throws-ex-info "Could not resolve ctx value: a.b.c"
                    {:source-name :a
                     :source      15}))
