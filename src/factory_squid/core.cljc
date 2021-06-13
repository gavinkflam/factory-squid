(ns factory-squid.core
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]))

;; Utilities

(defn deep-merge
  "Recursively merge two maps."
  [l r]

  (if (map? l)
    (merge-with deep-merge l r)
    r))

(defn- assert-spec
  "Return x if spec is nil, or x is valid against the given spec.

  If otherwise, throw an ex-info with the explain-data fields
  plus :clojure.spec.alpha/failure of :assertion-failed."
  [spec x]

  (when-not (or (nil? spec)
                (s/valid? spec x))
    (throw (ex-info (str "Spec assertion failed\n" (s/explain-str spec x))
                    (assoc (s/explain-data spec x)
                           ::s/failure :assertion-failed))))

  x)

(defn- spec?
  "Return true if x is a spec object, or x is an entry in the spec registry.
  Return false if otherwise."
  [x]
  (or (s/spec? x)
      (s/spec? (s/get-spec x))))

;; Specs

(s/def ::factory-fn     fn?)
(s/def ::post-build-fn  fn?)
(s/def ::post-build-fns (s/coll-of ::post-build-fn :into []))

(s/def ::spec spec?)
(s/def ::build-args map?)
(s/def ::fields     map?)

(s/def ::trait  (s/keys :opt-un [::spec ::build-args ::fields ::post-build-fns]))
(s/def ::traits (s/map-of keyword? ::trait))

(s/def ::factory (s/keys :req-un [::factory-fn]
                         :opt-un [::spec ::traits
                                  ::build-args ::fields ::post-build-fns]))

(s/fdef add-trait
  :args (s/cat :factory      ::factory
               :trait-name   keyword?
               :trait-params ::trait)
  :ret  ::factory)

(s/fdef add-post-build-fn
  :args (s/cat :factory       ::factory
               :post-build-fn ::post-build-fn)
  :ret  ::factory)

(s/fdef spec
  :args (s/cat :factory     ::factory
               :spec-object ::spec)
  :ret  ::factory)

(s/fdef build-args
  :args (s/cat :factory    ::factory
               :build-args ::build-args)
  :ret  ::factory)

(s/fdef fields
  :args (s/cat :factory ::factory
               :fields  ::fields)
  :ret  ::factory)

(s/fdef trait
  :args (s/cat :factory    ::factory
               :trait-name keyword?)
  :ret  ::factory)

(s/fdef build
  :args (s/or :with-build-args (s/cat :factory    ::factory
                                      :build-args ::build-args)
              :just-factory    (s/cat :factory ::factory))
  :ret  map?)

;; Modification functions

(defn add-trait
  "Add the given trait to the factory.

  Trait params could be applied to the factory with the `trait` function."

  [factory trait-name trait-params]
  (update factory :traits assoc trait-name trait-params))

(defn add-post-build-fn
  "Add the given post-build function to the factory.

  After the product is built, the post-build functions will be applied sequentially.

  A post-build function should be of arity 2, with the first argument being
  the product and the second argument being the factory map."

  [factory post-build-fn]
  (update factory :post-build-fns
          (fn [fns] (conj (into fns []) post-build-fn))))

(defn spec
  "Apply the given spec to the factory.

  Products will be validated against the spec when building."

  [factory spec-object]
  (assoc factory :spec spec-object))

(defn build-args
  "Add the given build arguments to the factory.
  Existing build argument of the same key will be overriden.

  Build arguments will be supllied to `factory-fn` when building."

  [factory build-args]
  (update factory :build-args merge build-args))

(defn fields
  "Add the given fields to the factory.
  The new fields will be merged recursively to the existing fields.

  Fields will be merged recursively to the products when building."

  [factory fields]
  (update factory :fields deep-merge fields))

(defn trait
  "Apply trait of the given name to the factory.

  Throw if a trait of the give name was not defined."

  [factory trait-name]

  (if-let [trait (get-in factory [:traits trait-name])]

    (let [steps (concat
                 (when (:spec trait)       [#(spec %1 (:spec trait))])
                 (when (:build-args trait) [#(build-args %1 (:build-args trait))])
                 (when (:fields trait)     [#(fields %1 (:fields trait))])
                 (map #(fn [f] (add-post-build-fn f %1))
                      (:post-build-fns trait)))]

      (reduce (fn [f step] (step f)) factory
              (filter some? steps)))

    ; Throw when trait not defined
    (throw (ex-info "Trait not found"
                    {:failure :trait-not-exist :trait-name trait-name}))))

;; Build

(defn build
  "Build a product with the factory.

  The products will be builts with the following step:

  1. the factory map will be supplied to `factory-fn` producing a product
  2. `fields` will be merged recursively to the product
  3. `post-build-fns` will be applied to the product sequentially
  4. the product will be validated against `spec`"

  ([factory -build-args]
   (-> factory (build-args -build-args) build))
  ([factory]
   (as-> factory $
     ((:factory-fn factory) $)
     (deep-merge $ (:fields factory))
     (reduce (fn [p next-fn] (next-fn p factory))
             $ (:post-build-fns factory))
     (assert-spec (:spec factory) $))))

;; Instrumentation

(st/instrument #{`add-trait `add-post-build-fn `spec `build-args `fields `trait
                 `build})
