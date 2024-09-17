(ns wizard.contextually.exferring-test
  (:use wizard.toolbelt.test wizard.toolbelt)
  (:require [wizard.contextually :as ctx]
            [clojure.math :as math]))

(deftest test-exferring-from-context
  (testing "exferring ctx values with functions"
    (are [r e-fn] (is (= r (ctx/resolve-in
                            {:a 2}
                            (ctx/exfer :a e-fn))))
      5 (partial - 7)
      4 #(int (math/pow % %)))

    (are [r e-fn] (is (= r (ctx/resolve-in
                            {:x 5 :y 2}
                            (ctx/exfer :x :y e-fn))))
      7  +
      3  -
      25 (comp int math/pow)))

  (testing "exferrences returning exferrences"
    (let [resolve (partial ctx/resolve-in {:a 11 :b 21 :c 31})]
      (is (= 63 (resolve
                 (ctx/exfer
                  :a (fn [a]
                       (ctx/exfer
                        :b (fn [b]
                             (ctx/exfer
                              :c (fn [c]
                                   (+ a b c))))))))))))

  (testing "exferring based on existing exferrence"
    (let [ctx     {:a 2 :b 3}
          resolve (partial ctx/resolve-in ctx)]

      (testing "nil base smoke tests"
        (let [x (ctx/exfer-on nil nil?)]
          (is (= true (and (map? x) (ctx/exferrence? x))))
          (is (= true (resolve x)))
          (is (= false (ctx/informing-exferrence? x)))
          (is (= true (ctx/informing-exferrence? (ctx/inform {} x))))))

      (testing "exferrence bases, direct and nested and extended"
        (are [r base] (is (= r (resolve (ctx/exfer-on base :b vector))))
          [2 3]      (ctx/value :a)
          [[2] 3]    [(ctx/value :a)]
          [{:a 2} 3] {:a (ctx/value :a)}
          [12 3]     (ctx/exfer-on
                      [(-> ctx :a inc) (ctx/exfer :b inc)]
                      (fn [[a b]] (* a b)))))

      (testing "exferring on all exferrences and variables"
        (are [r e] (is (= r (resolve e)))
          6  (ctx/exfer-all 1 2 3 +)
          6  (ctx/exfer-all 1 (ctx/value :a) 3 +)
          6  (ctx/exfer-all 1 2 (ctx/exfer :b identity) +)
          36 (ctx/exfer-all :a (ctx/exfer-all :a :b *) :b *)))

      (let [mul-a-by-10          (ctx/exfer :a #(* 10 %))
            mul-b-by-2           (ctx/exfer :b #(* 2 %))
            add-to-a             #(ctx/exfer :a (partial + %))
            with-a-b-incd-summed (ctx/inform
                                  {:a inc :b inc}
                                  :a :b (fn [a b] (+ a b)))]
        (are [r e] (is (= r (resolve e)))
          22        (ctx/exfer-on mul-a-by-10 add-to-a)
          9         (ctx/exfer-on mul-b-by-2 :b +)
          (* 7 3 4) (ctx/exfer-on with-a-b-incd-summed :a :b *))))))
