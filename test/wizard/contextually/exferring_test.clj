(ns wizard.contextually.exferring-test
  (:use wizard.toolbelt.test.midje wizard.toolbelt)
  (:require [wizard.contextually :as ctx]
            [clojure.math :as math]))

(facts
  "about exferring from context"
  (let [resolve (partial ctx/resolve-in {:a 2})]
    (resolve (ctx/exfer :a (partial - 7))) => 5
    (resolve (ctx/exfer :a #(int (math/pow % %)))) => 4)

  (let [resolve (partial ctx/resolve-in {:x 5 :y 2})]
    (resolve (ctx/exfer :x :y +)) => 7
    (resolve (ctx/exfer :x :y -)) => 3
    (resolve (ctx/exfer :x :y (comp int math/pow))) => 25)

  (fact
    "nesting exferrences"
    (let [resolve (partial ctx/resolve-in {:a 11 :b 21 :c 31})]
      (resolve
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
          resolve     (partial ctx/resolve-in ctx)
          mul-a-by-10 (ctx/exfer :a #(* 10 %))
          mul-b-by-2  (ctx/exfer :b #(* 2 %))
          with-a-b-incd-summed
          (ctx/inform
           {:a inc :b inc}
           :a :b (fn [a b] (+ a b)))]

      (let [x (ctx/exfer-on nil nil?)]
        (and (map? x) (ctx/exferrence? x)) => true
        (resolve x) => true
        (ctx/informing-exferrence? x) => false
        (ctx/informing-exferrence? (ctx/inform {} x)) => true)

      (resolve
       (ctx/exfer-on
        [(-> ctx :a inc) (ctx/exfer :b inc)]
        (fn [[a b]]
          (* a b))))
      => 12

      (resolve
       (ctx/exfer-on
        mul-a-by-10
        (fn [a-1]
          (ctx/exfer
           :a
           (fn [a-2]
             (+ a-1 a-2))))))
      => 22

      (resolve
       (ctx/exfer-on
        mul-b-by-2 :b
        (fn [b-1 b-2]
          (+ b-1 b-2))))
      => 9

      (resolve
       (ctx/exfer-on
        with-a-b-incd-summed :a :b
        (fn [sum a b]
          (* sum a b))))
      => (* 7 3 4))))
