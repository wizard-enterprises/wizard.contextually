(ns wizard.contextually.exferring-test
  (:use wizard.toolbelt.test wizard.toolbelt)
  (:require [wizard.contextually :as ctx]
            [clojure.math :as math]))

(deftest test-exferring-from-context
  (testing "exferring ctx values with functions"
    (are [r e-fn] (is (= r (ctx/resolve-in {:a 2}
                             (ctx/exfer e-fn :a))))
      5 (partial - 7)
      4 #(int (math/pow % %)))

    (are [r e-fn] (is (= r (ctx/resolve-in
                               {:x 5 :y 2}
                             (ctx/exfer e-fn :x :y))))
      7  +
      3  -
      25 (comp int math/pow)))

  (testing "exferrences returning exferrences"
    (let [resolve (partial ctx/resolve-in {:a 11 :b 21 :c 31})]
      (is (= 63 (resolve (ctx/exfer + :a :b :c))))
      (is (= 63 (resolve
                 (ctx/exfer
                     (fn [a]
                       (ctx/exfer
                           (fn [b]
                             (ctx/exfer
                                 (fn [c] (+ a b c))
                               :c)) :b)) :a))))))

  (testing "exferrences with fallback"
    (let [resolve (partial ctx/resolve-in {:a 150})]
      (is (thrown? Exception (resolve (ctx/value :z))))
      (is (= 15 (resolve (ctx/fallback 15 (ctx/value :z)))))
      (is (= 150 (resolve (ctx/fallback (ctx/value :a) (ctx/value :z)))))))

  (testing "exferring based on existing exferrence"
    (let [ctx     {:a 2 :b 3 :x {:y {:z 5}}}
          resolve (partial ctx/resolve-in ctx)]

      (testing "nil base smoke tests"
        (let [x (ctx/exfer-on nil? nil)]
          (is (= true (and (map? x) (ctx/exferrence? x))))
          (is (= true (resolve x)))
          (is (= false (ctx/informing-exferrence? x)))
          (is (= true (ctx/informing-exferrence? (ctx/inform {} x))))))

      (testing "exferrence bases, direct and nested and extended"
        (are [r base] (is (= r (resolve (ctx/exfer-on vector base (ctx/value :b)))))
          [2 3]      (ctx/value :a)
          [[2] 3]    [(ctx/value :a)]
          [{:a 2} 3] {:a (ctx/value :a)}

          [[:x :y :z] 3] (ctx/exfer-on identity [:x :y :z])
          [5 3]          (ctx/exfer identity [:x :y :z])

          [12 3] (ctx/exfer-on
                   (fn [a b] (* a b))
                   (-> ctx :a inc) (ctx/exfer inc :b))
          [12 3] (ctx/exfer-on
                   (fn [[a b]] (* a b))
                   [(-> ctx :a inc) (ctx/exfer inc :b)])
          [12 3] (ctx/exfer-on
                   (fn [{:keys [a b]}] (* a b))
                   {:a (-> ctx :a inc) :b (ctx/exfer inc :b)})))

      (testing "exferring on all exferrences and variables"
        (are [r e] (is (= r (resolve e)))
          6  (ctx/exfer-on + 1 2 3)
          6  (ctx/exfer-on + 1 (ctx/value :a) 3)
          6  (ctx/exfer-on + 1 2 (ctx/exfer identity :b))
          36 (ctx/exfer-on *
               (ctx/value :a)
               (ctx/exfer * :a :b)
               (ctx/value :b))))

      (let [mul-a-by-10          (ctx/exfer #(* 10 %) :a)
            mul-b-by-2           (ctx/exfer #(* 2 %) :b)
            add-to-a             #(ctx/exfer (partial + %) :a)
            with-a-b-incd-summed (ctx/inform
                                     {:a inc :b inc}
                                   (ctx/exfer + :a :b))]
        (are [r e] (is (= r (resolve e)))
          22        (ctx/exfer add-to-a mul-a-by-10)
          9         (ctx/exfer + mul-b-by-2 :b)
          (* 7 2 3) (ctx/exfer * with-a-b-incd-summed :a :b))))))
