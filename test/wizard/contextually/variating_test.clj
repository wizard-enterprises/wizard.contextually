(ns wizard.contextually.variating-test
  (:use wizard.toolbelt.test wizard.toolbelt)
  (:require [wizard.contextually :as ctx]))

(deftest test-variating-by
  (let [ctx     {:x \5 :y \9 :z {:a \1 :b \2}}
        resolve (partial ctx/resolve-in ctx)
        by-x    (partial ctx/variate-by :x)
        all-resolve-to (fn [r] #(for [e %&] (is (= r (resolve e)))))]

    (testing "picks variation by value in context"
      (let [r 20]
        ((all-resolve-to 20)
         (by-x [\3 \_ \5 r \7 \H])
         (by-x #(case % \3 \_ \5 r \7 \H)))))

    (testing "picks variation by nested value"
      (let [r       20
            variate #(case % \1 :aaa \2 :bbb)]
        (is (= :aaa (resolve (ctx/variate-by [:z :a] variate))))
        (is (= :bbb (resolve (ctx/variate-by [:z :b] variate))))))

    (testing "uses fallback variation when one is provided"
      ((all-resolve-to 9)
       (ctx/fallback (ctx/value :y) (by-x [\4 0 \6 0]))
       (ctx/fallback (ctx/value :y) (by-x #(case % \4 0 \6 0 nil)))))

    (testing "variates based on exferrence"
      ((all-resolve-to 200)
       (ctx/variate-by (ctx/value :y) [\9 200])
       (ctx/variate-by (ctx/value :y) #(case % \9 200))))

    (testing "exfers variation"
      ((all-resolve-to 9)
       (by-x [\5 (ctx/value :y)])
       (by-x #(case % \5 (ctx/value :y)))))

    (testing "variating variation by predicate"
      (is (= 15 (resolve (by-x [#(= % \5) 15])))))

    (testing "exfers factory variation"
      (let [r (fn [x]
                (ctx/exfer
                 :y #(+ (Integer/parseInt (str x))
                        (Integer/parseInt (str %)))))]
        ((all-resolve-to 14)
         (by-x [\5 r])
         (by-x #(case % \5 r)))))

    (testing "variate by multiple values"
      (is (= 100
             (resolve
              (ctx/variate-by
               :x
               (ctx/value :y)
               [:z :a]
               (ctx/exfer :z #(:b %))
               :z
               [[\5 \9 \1 \2 {:a \1 :b \2}] 100
                #(do % false) 0])))))

    (testing
     "about variating on"
     (let [by-x (by-x {\3 \_ \5 100 \7 \H})]
       (is (ctx/variating-exferrence? by-x))
       ((all-resolve-to 100)
        by-x
        (ctx/exfer-on by-x identity)
        (ctx/variate-on by-x)
        (ctx/variate-on by-x \5)
        (ctx/variate-on by-x _))

       (is (= \_ (resolve (ctx/variate-on by-x \3))))
       (is (= \H (resolve (ctx/variate-on by-x \7))))))

    (let [by-xy (ctx/variate-by
                 :x :y
                 [[\3 \3] -5
                  [\5 \5] -5
                  [\9 \9] -5
                  [\5 \9] 100])]
      ((all-resolve-to 100)
       (ctx/variate-on by-xy)
       (ctx/variate-on by-xy _)
       (ctx/variate-on by-xy \5)
       (ctx/variate-on by-xy \5 _)
       (ctx/variate-on by-xy _ \9)
       (ctx/variate-on by-xy \5 \9)
       (ctx/variate-on by-xy _ _))

      ((all-resolve-to -5)
       (ctx/variate-on by-xy \3 \3)
       (ctx/variate-on by-xy _ \5)
       (ctx/variate-on by-xy \9 _)))))
