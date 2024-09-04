(ns wizard.contextually.contextual-resolution-test
  (:use wizard.toolbelt.test.midje wizard.toolbelt)
  (:require [wizard.contextually :as ctx]
            [clojure.math :as math]))

(facts
  "about ctx resolutions"
  (facts
    "about resolving ctx vars"
    (let [ctx {:foo 15}]
      (ctx/resolve-in ctx (ctx/var :foo)) => 15
      (ctx/resolve-in ctx (ctx/var "foo")) => 15
      (ctx/resolve-in ctx {:a (ctx/var "foo")}) => {:a 15}
      (ctx/resolve-in ctx [(ctx/var "foo")]) => [15]
      (ctx/resolve-in ctx (ctx/var :ctx)) => {:foo 15})
    (ctx/resolve-in {:a 1 :b 2} {:a (ctx/var "a") :b (ctx/var "b")}) => {:a 1 :b 2}
    (ctx/resolve-in {:a {:b {:c 15}}} {:x (ctx/var "a.b.c") :y (ctx/var [:a :b :c])})
    => {:x 15 :y 15}
    (ctx/resolve-in {:a 15} {:a (ctx/var [:a :b :c])})
    => (throws-ex-info "Could not resolve ctx var: a.b.c"
                       {:source-name :a
                        :source      15}))

  (facts
    "about exferring from context"
    (let [ctx {:a 2}]
      (ctx/resolve-in ctx (ctx/exfer :a (partial - 7))) => 5
      (ctx/resolve-in ctx (ctx/exfer :a #(int (math/pow % %)))) => 4)

    (let [ctx {:x 5 :y 2}]
      (ctx/resolve-in ctx (ctx/exfer :x :y +)) => 7
      (ctx/resolve-in ctx (ctx/exfer :x :y -)) => 3
      (ctx/resolve-in ctx (ctx/exfer :x :y (comp int math/pow))) => 25))

  (fact
    "nesting resolvables"
    (let [ctx {:a 11 :b 21 :c 31}]
      (ctx/resolve-in
       ctx
       (ctx/exfer
        :a (fn [a]
             (ctx/exfer
              :b (fn [b]
                   (ctx/exfer
                    :c (fn [c]
                         (+ a b c))))))))
      => 63))

  (fact
    "about exferring on contextual exferrence"
    (let [ctx         {:a 2 :b 3}
          mul-a-by-10 (ctx/exfer :a #(* 10 %))
          mul-b-by-2  (ctx/exfer :b #(* 2 %))
          with-a-b-incd-summed
          (ctx/inform
           {:a inc :b inc}
           :a :b (fn [a b] (+ a b)))]

      (ctx/resolve-in ctx (ctx/exfer-on nil nil?)) => true

      (ctx/resolve-in
       ctx
       (ctx/exfer-on
        [(-> ctx :a inc) (-> ctx :b inc)]
        (fn [[a b]]
          (* a b))))
      => 12

      (ctx/resolve-in
       ctx
       (ctx/exfer-on
        mul-a-by-10
        (fn [a-1]
          (ctx/exfer
           :a
           (fn [a-2]
             (+ a-1 a-2))))))
      => 22

      (ctx/resolve-in
       ctx
       (ctx/exfer-on
        mul-b-by-2 :b
        (fn [b-1 b-2]
          (+ b-1 b-2))))
      => 9

      (ctx/resolve-in
       ctx
       (ctx/exfer-on
        with-a-b-incd-summed
        :a :b
        (fn [sum a b]
          (* sum a b))))
      => (* 7 3 4)))

  (facts
    "about resolving value throughout context"
    (fact
      "with ad-hoc informing"
      (let [ctx {:a 1 :b 99}]
        (ctx/resolve-throughout
         ctx
         {:a (comp inc inc) :b 4 :c 2}
         (ctx/exfer :a :b :c *))
        => {:resolved 24 :ctx {:a 3 :b 4 :c 2}}))

    (fact
      "with informing in form"
      (let [ctx {:a 1 :b 2 :c 3 :z 8}
            {:keys [ctx resolved]}
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
            {:keys [ctx resolved]}
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
        resolved => 10))))
