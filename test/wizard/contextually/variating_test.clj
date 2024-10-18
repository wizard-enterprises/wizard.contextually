(ns wizard.contextually.variating-test
  (:use wizard.toolbelt.test wizard.toolbelt)
  (:require [wizard.contextually :as ctx]))

(deftest test-variating-by
  (let [ctx            {:x \5 :y \9 :z {:a \1 :b \2}}
        resolve        (partial ctx/resolve-in ctx)
        all-resolve-to (fn [r] #(for [e %&] (is (= r (resolve e)))))]

    (testing "picks variation by value in context"
      (is (= 20
             (resolve
              (ctx/variate-by :x
                [\3 \_
                 \5 20
                 \7 \H])))))

    ;; (testing "picks variation by nested value"
    ;;   (let [variations (ctx/variating \1 :aaa \2 :bbb _ :ccc)]
    ;;     (is (= :aaa (resolve (ctx/variate-by [:z :a] variations))))
    ;;     (is (= :bbb (resolve (ctx/variate-by [:z :b] variations))))
    ;;     (is (= :ccc (resolve (ctx/variate-by :x variations))))))

    (testing "variates based on exferrence"
      (is (= 200
             (resolve
              (ctx/variate-by (ctx/value :y)
                [\9 200])))))

    (testing "uses fallback variation when one is provided"
      (is (= \9
             (resolve
              (ctx/fallback (ctx/value :y)
                (ctx/variate-by :x
                  [\4 0 \6 0]))))))

    (testing "exfers variation"
      (is (= \9
             (resolve
              (ctx/variate-by :x
                [\5 (ctx/value :y)])))))

    ;; (testing "variating variation by predicate"
    ;;   (is (= 15 (resolve (ctx/variate-by :x
    ;;                        [#(= (

    ;; spyx :debug %) \5) 15])))))

    ;; (testing "exfers factory variation"
    ;;   (let [r (fn [x]
    ;;             (ctx/exfer
    ;;               :y #(+ (Integer/parseInt (str x))
    ;;                      (Integer/parseInt (str %)))))]
    ;;     (is (= 14
    ;;            (ctx/variate-by :x [\5 r])))))

    ;; (testing "variate by multiple values"
    ;;   (is (= 100
    ;;          (resolve
    ;;           (ctx/variate-by
    ;;             :x (ctx/value :y) [:z :a] (ctx/exfer :z #(:b %)) :z
    ;;             [[\5 \9 \1 \2 {:a \1 :b \2}] 100])))))

    ;; (testing "about variating on"
    ;;   (let [by-x (ctx/variate-by :x {\3 \_ \5 100 \7 \H})]
    ;;     (is (ctx/variating-exferrence? by-x))
    ;;     ((all-resolve-to 100)
    ;;      by-x
    ;;      (ctx/exfer-on by-x identity)
    ;;      (ctx/variate-on by-x)
    ;;      (ctx/variate-on by-x \5)
    ;;      (ctx/variate-on by-x _))

    ;;     (is (= \_ (resolve (ctx/variate-on by-x \3))))
    ;;     (is (= \H (resolve (ctx/variate-on by-x \7)))))

    ;;   (let [by-xy (ctx/variate-by
    ;;                 :x :y
    ;;                 [[\3 \3] -5
    ;;                  [\5 \5] -5
    ;;                  [\9 \9] -5
    ;;                  [\5 \9] 100])]
    ;;     ((all-resolve-to 100)
    ;;      (ctx/variate-on by-xy)
    ;;      (ctx/variate-on by-xy _)
    ;;      (ctx/variate-on by-xy \5)
    ;;      (ctx/variate-on by-xy \5 _)
    ;;      (ctx/variate-on by-xy _ \9)
    ;;      (ctx/variate-on by-xy \5 \9)
    ;;      (ctx/variate-on by-xy _ _))

    ;;     ((all-resolve-to -5)
    ;;      (ctx/variate-on by-xy \3 \3)
    ;;      (ctx/variate-on by-xy _ \5)
    ;;      (ctx/variate-on by-xy \9 _))))

    ;; (testing "variating on each of"
    ;;   (let [z (:z ctx)
    ;;         results
    ;;         {::z       z
    ;;          [1 10 z]  -5
    ;;          [8 10 z]  -5
    ;;          [1 80 z]  -5
    ;;          [8 80 z]  -5
    ;;          [\5 10 z] 50
    ;;          [\5 80 z] 50
    ;;          [1 \9 z]  90
    ;;          [8 \9 z]  90
    ;;          [\5 \9 z] 100}]
    ;;     (is (= results
    ;;            (resolve
    ;;             (ctx/variate-on-each-of
    ;;               [1 \5 _ 8] _ [10 \9 _ 80]
    ;;               (ctx/variate-by :x :z :y
    ;;                 [[\5 _ \9] 100
    ;;                  [\5 _ _] 50
    ;;                  [_ _ \9] 90
    ;;                  [_ _ _] -5])
    ;;               {::z (ctx/value :z)}
    ;;               assoc))))))
    )
  )
