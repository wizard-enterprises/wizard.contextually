(ns wizard.contextually.variating-by-test
  (:use wizard.toolbelt.test.midje wizard.toolbelt)
  (:require [wizard.contextually :as ctx]))

(defn resolves-identically
  [& forms]
  (let [](fact "resol")))

(facts
  "about variating by"
  (let [ctx     {:x \5 :y \9 :z {:a \1 :b \2}}
        resolve (partial ctx/resolve-in ctx)
        by-x    (partial ctx/variate-by :x)
        resolves-identically
        (fn [& forms]
          (let [forms (map resolve forms)]
            (fact "resolves identically"
                  (doseq [[a b] (partition 2 forms)]
                    a => b))
            (first forms)))]

    (fact
      "picks variation by value in context"
      (let [r 20]
        (resolves-identically
         (by-x [\3 \_ \5 r \7 \H])
         (by-x #(case % \3 \_ \5 r \7 \H))) => 20))

    (fact
      "picks variation by nested value"
      (let [r       20
            variate #(case % \1 :aaa \2 :bbb)]
        (resolve (ctx/variate-by [:z :a] variate)) => :aaa
        (resolve (ctx/variate-by [:z :b] variate)) => :bbb))

    (fact
      "uses fallback variation when one is provided"
      (let [f (ctx/value :y)]
        (resolves-identically
         (ctx/fallback f (by-x [\4 0 \6 0]))
         (ctx/fallback f (by-x #(case % \4 0 \6 0 nil))))) => \9)

    (fact
      "variates based on exferrence"
      (resolves-identically
       (ctx/variate-by (ctx/value :y) [\9 200])
       (ctx/variate-by (ctx/value :y) #(case % \9 200))) => 200)

    (fact
      "exfers variation"
      (let [r (ctx/value :y)]
        (resolves-identically
         (by-x [\5 r])
         (by-x #(case % \5 r))) => \9))

    (fact
      "variating variation by predicate"
      (resolve (by-x [#(= \5) 15])) => 15)

    (fact
      "exfers factory variation"
      (let [r (fn [x]
                (ctx/exfer
                 :y #(+ (Integer/parseInt (str x))
                        (Integer/parseInt (str %)))))]
        (resolves-identically
         (by-x [\5 r])
         (by-x #(case % \5 r))) => 14))

    (fact
      "variate by multiple values"
      (resolve
       (ctx/variate-by
        :x
        (ctx/value :y)
        [:z :a]
        (ctx/exfer :z #(:b %))
        :z
        [[\5 \9 \1 \2 {:a \1 :b \2}] 100
         #(do % false) 0]))
      => 100)))
