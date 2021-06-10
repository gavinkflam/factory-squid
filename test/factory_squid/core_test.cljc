(ns factory-squid.core-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest testing is]]
            [factory-squid.core :as fs]))

;; Test data

(def ^:private dummy-factory-fn
  (fn [] {:name "Alice"}))

(s/def ::name string?)
(s/def ::with-name (s/keys :req-un [::name]))

;; Utilities

(defmacro ^:private catch!
  "Return the captured exception after executing the body.
  Throw if no exceptions were encountered."
  [& body]
  `(try
     ~@body
     (throw (Exception. "No exceptions caught"))
     (catch Exception e# e#)))

(defn- instrument-failure?
  "Return true if the given exception is a spec instrument failure.
  Return false if otherwise."
  [exception]
  (= :instrument
     (-> exception ex-data ::s/failure)))

(defn- spec-assertion-failure?
  "Return true if the given exception is a spec assertion failure.
  Return false if otherwise."
  [exception]
  (= :assertion-failed
     (-> exception ex-data ::s/failure)))

;; Test deep merge

(deftest test-deep-merge

  (testing "non-recursive merges"
    (is (= {:a 1 :b 2 :c 3 :d 4}
           (fs/deep-merge {:a 1 :b 2} {:c 3 :d 4})))
    (is (= {:a 1}
           (fs/deep-merge nil {:a 1})))
    (is (= {:a 1}
           (fs/deep-merge {:a 1} nil)))
    (is (= {:a 11 :b 2 :c 3}
           (fs/deep-merge {:a 1 :b 2} {:a 11 :c 3}))))

  (testing "recursive merges"
    (is (= {:a {:a1 11 :a2 12 :a3 13} :b 2 :c 3}
           (fs/deep-merge {:a {:a1 11} :b 2}
                          {:a {:a2 12 :a3 13} :c 3})))
    (is (= {:a {:a1 {:a11 111 :a12 112 :a13 113} :a2 12} :b 2 :c 3}
           (fs/deep-merge {:a {:a1 {:a11 111 :a12 112}} :b 2}
                          {:a {:a1 {:a13 113} :a2 12} :c 3})))))

;; Test modification functions

(deftest test-add-trait

  (let [factory {:factory-fn dummy-factory-fn}

        with-bob   {:factory-fn dummy-factory-fn
                    :traits {:bob {:fields {:name "Bob"}}}}
        with-bobby {:factory-fn dummy-factory-fn
                    :traits {:bob {:fields {:name "Bobby"}}}}
        with-bob-and-carol {:factory-fn dummy-factory-fn
                            :traits {:bob   {:fields {:name "Bob"}}
                                     :carol {:fields {:name "Carol"}}}}]

    (testing "on factory with no traits"
      (is (= with-bob
             (fs/add-trait factory :bob {:fields {:name "Bob"}}))))

    (testing "on factory with traits"
      (is (= with-bob-and-carol
             (fs/add-trait with-bob :carol {:fields {:name "Carol"}}))))

    (testing "override existing trait"
      (is (= with-bobby
             (fs/add-trait with-bob :bob {:fields {:name "Bobby"}}))))

    (testing "test instrumentation"
      (is (instrument-failure?
           (catch! (fs/add-trait "foobar" {:fields {:name "Bobby"}})))))))

(deftest test-add-post-build-fn

  (let [factory {:factory-fn dummy-factory-fn}
        fn1 (fn [_ _] {:a 1})
        fn2 (fn [_ _] {:a 2})

        with-fn1 {:factory-fn dummy-factory-fn
                  :post-build-fns [fn1]}
        with-fn1-and-2 {:factory-fn dummy-factory-fn
                        :post-build-fns [fn1 fn2]}]

    (testing "on factory with no post-build-fns"
      (is (= with-fn1
             (fs/add-post-build-fn factory fn1))))

    (testing "append to existing post-build-fns"
      (is (= with-fn1-and-2
             (fs/add-post-build-fn with-fn1 fn2))))

    (testing "test instrumentation"
      (is (instrument-failure? (catch! (fs/add-post-build-fn "foobar")))))))

(deftest test-spec

  (let [factory {:factory-fn dummy-factory-fn}
        s1 (s/map-of keyword? some?)
        s2 (s/map-of keyword? string?)

        with-s1 {:factory-fn dummy-factory-fn :spec s1}
        with-s2 {:factory-fn dummy-factory-fn :spec s2}]

    (testing "on factory with no spec"
      (is (= with-s1
             (fs/spec factory s1))))

    (testing "override existing spec"
      (is (= with-s2
             (fs/spec with-s1 s2))))

    (testing "test instrumentation"
      (is (instrument-failure? (catch! (fs/spec "foobar")))))))

(deftest test-build-args

  (let [factory {:factory-fn dummy-factory-fn}

        registered-2020 {:factory-fn dummy-factory-fn
                         :build-args {:registered {:y 2020 :m 2 :d 29}}}
        bob-2020        {:factory-fn dummy-factory-fn
                         :build-args {:registered {:y 2020 :m 2 :d 29} :name "Bob"}}
        registered-2021 {:factory-fn dummy-factory-fn
                         :build-args {:registered {:y 2021 :w 2}}}]

    (testing "on factory with no traits"
      (is (= registered-2020
             (fs/build-args factory {:registered {:y 2020 :m 2 :d 29}}))))

    (testing "on factory with traits"
      (is (= bob-2020
             (fs/build-args registered-2020 {:name "Bob"}))))

    (testing "override existing trait"
      (is (= registered-2021
             (fs/build-args registered-2021 {:registered {:y 2021 :w 2}}))))

    (testing "test instrumentation"
      (is (instrument-failure? (catch! (fs/build-args "foobar")))))))

(deftest test-fields

  (let [factory {:factory-fn dummy-factory-fn}

        with-bob     {:factory-fn dummy-factory-fn
                      :fields {:name {:first "Bob" :last "Doe"} :role "Admin"}}
        with-bob-roe {:factory-fn dummy-factory-fn
                      :fields {:name {:first "Bob" :last "Roe"} :role "Editor"}}]

    (testing "on factory with no fields"
      (is (= with-bob
             (fs/fields factory {:name {:first "Bob" :last "Doe"} :role "Admin"}))))

    (testing "recursively merge with existing fields"
      (is (= with-bob-roe
             (fs/fields with-bob {:name {:last "Roe"} :role "Editor"}))))

    (testing "test instrumentation"
      (is (instrument-failure? (catch! (fs/fields "foobar")))))))

(deftest test-trait

  (let [fn1 (fn [p _] (update p :name str " Doe"))
        fn2 (fn [p _] (assoc p :role "Admin"))
        fn3 (fn [p _] (assoc p :registered true))

        alice-trait {:build-args     {:registered true}
                     :fields         {:name "Alice"}
                     :post-build-fns [fn2 fn3]}
        bob-trait   {:fields         {:name "Bob"}}
        guest-trait {:build-args     {:registered false :role "Guest"}}

        factory {:factory-fn dummy-factory-fn
                 :traits {:alice alice-trait :bob bob-trait :guest guest-trait}
                 :build-args {:registered :n/a :role "Admin"}
                 :fields {:name "Carol" :role "Admin"}
                 :post-build-fns [fn1]}]

    (testing "apply trait of the given name"
      (is (= {:factory-fn dummy-factory-fn
              :traits {:alice alice-trait :bob bob-trait :guest guest-trait}
              :build-args {:registered true :role "Admin"}
              :fields {:name "Alice" :role "Admin"}
              :post-build-fns [fn1 fn2 fn3]}
             (fs/trait factory :alice))))

    (testing "apply traits with incomplete params"
      (is (= {:factory-fn dummy-factory-fn
              :traits {:alice alice-trait :bob bob-trait :guest guest-trait}
              :build-args {:registered :n/a :role "Admin"}
              :fields {:name "Bob" :role "Admin"}
              :post-build-fns [fn1]}
             (fs/trait factory :bob)))
      (is (= {:factory-fn dummy-factory-fn
              :traits {:alice alice-trait :bob bob-trait :guest guest-trait}
              :build-args {:registered false :role "Guest"}
              :fields {:name "Carol" :role "Admin"}
              :post-build-fns [fn1]}
             (fs/trait factory :guest))))

    (testing "throw when trait does not exist"
      (is (= {:failure :trait-not-exist :trait-name :carol}
             (ex-data (catch! (fs/trait factory :carol))))))

    (testing "test instrumentation"
      (is (instrument-failure? (catch! (fs/trait factory "guest")))))))

;; Test build

(deftest test-build

  (testing "build from just factory-fn"
    (is (= {:name "Bob"}
           (fs/build {:factory-fn (fn [_] {:name "Bob"})}))))

  (testing "build with build-args as second argument"
    (let [factory {:factory-fn (fn [f] {:factory f})}]
      (is (= {:factory (assoc factory :build-args {:registered true})}
             (fs/build factory {:registered true})))))

  (testing "fields should be merged into the product recursively"
    (is (= {:name {:first "Bob" :last "Roe"}}
           (fs/build {:factory-fn (fn [_] {:name {:first "Bob" :last "Doe"}})
                      :fields {:name {:last "Roe"}}}))))

  (testing "post-build-fns should be applied sequentially"
    (let [factory {:factory-fn (fn [_] {:name "Bob"})
                   :post-build-fns [(fn [product f]
                                      (-> product
                                          (update :name str " Doe")
                                          (assoc :factory f)))
                                    (fn [product _]
                                      (update product :name str " Roe"))]}]
      (is (= {:name "Bob Doe Roe"
              :factory factory}
             (fs/build factory)))))

  (testing "spec should be used to validate against the product"
    (is (= {:name "Bob"}
           (fs/build {:factory-fn (fn [_] {:name "Bob"})
                      :spec ::with-name})))
    (is (spec-assertion-failure?
         (catch! (fs/build {:factory-fn (fn [_] {:name nil})
                            :spec ::with-name})))))

  (testing "all factory map fields should be passed to factory-fn"
    (let [factory {:factory-fn (fn [f] {:factory f})
                   :spec ::with-name
                   :build-args {:registered true}
                   :traits {:alice {:fields {:name "Alice"}}}
                   :fields {:name "Bob"}
                   :post-build-fns [(fn [p _] p)]}]
      (is (= {:factory factory :name "Bob"}
             (fs/build factory)))))

  (testing "build steps should be applied in the correct order"
    (let [factory {:factory-fn (fn [{:keys [build-args]}]
                                 (if (:alice? build-args)
                                   {:name "Alice" :registered true} {}))
                   :spec ::with-name
                   :build-args {:alice? true}
                   :fields {:name "Bob" :role "Admin"}
                   :post-build-fns [(fn [product _] (update product :name str " Doe"))
                                    (fn [product _] (update product :name str " Roe"))]}]
      (is (= {:name "Bob Doe Roe" :registered true :role "Admin"}
             (fs/build factory)))))

  (testing "test instrumentation"
    (is (instrument-failure? (catch! (fs/build {:spec ::with-name}))))))
