(ns wizard.contextually.informing-throughout-test
  (:use wizard.toolbelt.test wizard.toolbelt)
  (:require [wizard.contextually :as ctx]))

(deftest test-resolving-value-throughtout-context
  (let [ctx {:a 1 :b 99}]
    (testing "ad-hoc informing"
      (testing "informing within exferrence"
        (are [r e] (is (= r (ctx/resolve-in ctx e)))
          15 (ctx/inform {:a 2}
               15)
          15 (ctx/inform {:a 2}
               (fn [] 15))
          2  (ctx/inform {:a 2}
               :a identity)
          2  (ctx/inform {:a 2}
               (ctx/value :a))
          6  (ctx/inform {:a 999 :b 3}
               (ctx/inform {:a 2}
                 (ctx/exfer
                   :a :b *)))))

      (testing "informing when resolving throughout"
        (is (= [{:a 3 :b 4 :c 2} 24]
               (ctx/resolve-throughout ctx
                 (ctx/inform
                     {:a (comp inc inc) :b 4 :c 2}
                   (ctx/exfer :a :b :c *))))))))

  (testing "with informing in form"
    (is (= [{:a 8 :b 2 :c 32 :z 8}
            {:x 48 :y 35}]
           (ctx/resolve-throughout
            {:a 1 :b 2 :c 3 :z 8}
            (ctx/exfer :z
                       (fn [z]
                         {:x (ctx/inform {:a #(* z %)}
                                         :a :b :c *)
                          :y (ctx/inform {:c #(* z (inc %))}
                                         :a :b :c +)}))))))

  (testing "informing w exferred value"
    (is (= [{:a 10 :z 999} 10]
           (ctx/resolve-throughout
            {:a 2 :z 5}
            (ctx/inform
             {:a (fn [a]
                   (ctx/exfer :z
                              (fn [z]
                                (* a z))))
              :z 999}
             :a identity))))))
