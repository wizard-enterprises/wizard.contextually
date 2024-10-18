(ns wizard.contextually.ctx-vals-test
  (:use wizard.toolbelt.test wizard.toolbelt)
  (:require [wizard.contextually :as ctx]))

(deftest test-ctx-vals
  (testing "resolving ctx values"
    (are [r e] (is (= r (ctx/resolve-in {:foo 15} e)))
      15        (ctx/value :foo)
      15        (ctx/value "foo")
      {:a 15}   {:a (ctx/value "foo")}
      [15]      [(ctx/value "foo")]
      {:foo 15} (ctx/value :ctx)))

  (testing "resolving spreaded and nested ctx values"
    (is (= {:a 1 :b 2}
           (ctx/resolve-in
            {:a 1 :b 2}
            {:a (ctx/value "a") :b (ctx/value "b")})))

    (is (= {:x 15 :y 15}
           (ctx/resolve-in
            {:a {:b {:c 15}}}
            {:x (ctx/value "a.b.c") :y (ctx/value [:a :b :c])})))

    (let [r? #(ctx/resolve-in {:x [0 0 [0 [{:z 15}]]]} %)]
      (is (= 15 (r? (ctx/value [:x 2 1 0 :z]))))
      (is (= 15 (r? (ctx/value "x.2.1.0.z"))))
      (is (= 15 (r? (ctx/exfer identity [:x 2 1 0 :z]))))))

  (testing "failure on unresolvable value"
    (is (thrown-ex-info?
         "Could not resolve ctx value: a.b.c"
         {:source-name :a
          :source      15}
         (ctx/resolve-in {:a 15} {:a (ctx/value [:a :b :c])})))))
