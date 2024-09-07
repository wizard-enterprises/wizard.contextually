(ns wizard.contextually.informing-throughout-test
  (:use wizard.toolbelt.test.midje wizard.toolbelt)
  (:require [wizard.contextually :as ctx]))

(facts
  "about resolving value throughout context"
  (fact
    "with ad-hoc informing"
    (let [ctx     {:a 1 :b 99}
          resolve (partial ctx/resolve-in ctx)]
      (resolve (ctx/inform {:a 2} 15)) => 15
      (resolve (ctx/inform {:a 2} (fn [] 15))) => 15
      (resolve (ctx/inform {:a 2} :a identity)) => 2
      (resolve (ctx/inform {:a 2} (ctx/value :a))) => 2

      (ctx/resolve-throughout
       ctx
       {:a (comp inc inc) :b 4 :c 2}
       (ctx/exfer :a :b :c *))
      => [{:a 3 :b 4 :c 2} 24]))

  (fact
    "with informing in form"
    (let [ctx {:a 1 :b 2 :c 3 :z 8}
          [ctx resolved]
          (ctx/resolve-throughout
           ctx
           (ctx/exfer
            :z
            (fn [z]
              {:x (ctx/inform
                   {:a #(* z %)}
                   :a :b :c *)
               :y (ctx/inform
                   {:c #(* z (inc %))}
                   :a :b :c +)})))]

      ctx => {:a 8 :b 2 :c 32 :z 8}
      resolved => {:x 48 :y 35}))

  (fact
    "informing w exferred value"
    (let [ctx {:a 2 :z 5}
          [ctx resolved]
          (ctx/resolve-throughout
           ctx
           (ctx/inform
            {:a (fn [a]
                  (ctx/exfer
                   :z (fn [z] (* a z))))
             :z 999}
            :a
            identity))]

      ctx => {:a 10 :z 999}
      resolved => 10)))
