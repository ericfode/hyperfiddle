(ns hyperfiddle.appfn.hydrate-requests-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datomic.api :as d]
            [hyperfiddle.appfn.fixtures :as fixtures]
            [hyperfiddle.appfn.hydrate-requests :as hydrate-requests]))


(def schema [{:db/ident :person/name
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}
             {:db/ident :person/age
              :db/valueType :db.type/long
              :db/cardinality :db.cardinality/one}])

(use-fixtures :each (fixtures/build-fixtures schema))

(deftest branch-once []
  (let [conn (d/connect fixtures/test-uri)
        staged-branches [{:branch-ident nil
                          :uri fixtures/test-uri
                          :tx [{:db/id "-1" :person/name "John" :person/age 30}]}]
        local-basis {fixtures/test-uri (d/basis-t (d/db conn))}
        get-secure-db-with (hydrate-requests/build-get-secure-db-with staged-branches (atom {}) local-basis)
        $ (:db (get-secure-db-with fixtures/test-uri "nil"))]
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $) 30))))

(deftest branch-once-stale []
  (let [conn (d/connect fixtures/test-uri)
        staged-branches [{:branch-ident nil
                          :uri fixtures/test-uri
                          :tx [{:db/id "-1" :person/name "John" :person/age 30}]}]
        local-basis {fixtures/test-uri (d/basis-t (d/db conn))} ; get a stale basis before another user transacts
        _ @(d/transact conn [{:db/id "-1" :person/name "Bob" :person/age 50}])
        get-secure-db-with (hydrate-requests/build-get-secure-db-with staged-branches (atom {}) local-basis)]
    (is (thrown-with-msg? RuntimeException (re-pattern hydrate-requests/ERROR-BRANCH-PAST)
                          (get-secure-db-with fixtures/test-uri "nil")))))

(deftest branch-popover []
  (let [conn (d/connect fixtures/test-uri)
        staged-branches [{:branch-ident nil
                          :uri fixtures/test-uri
                          :tx nil}
                         {:branch-ident "2"
                          :uri fixtures/test-uri
                          :tx [{:db/id "-1" :person/name "John" :person/age 30}]}]
        local-basis {fixtures/test-uri (d/basis-t (d/db conn))}
        get-secure-db-with (hydrate-requests/build-get-secure-db-with staged-branches (atom {}) local-basis)
        $-nil (:db (get-secure-db-with fixtures/test-uri "nil"))
        $-2 (:db (get-secure-db-with fixtures/test-uri "2"))]
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $-nil) nil))
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $-2) 30))))

(deftest branch-popover-stale []
  (let [conn (d/connect fixtures/test-uri)
        staged-branches [{:branch-ident nil
                          :uri fixtures/test-uri
                          :tx nil}
                         {:branch-ident "2"
                          :uri fixtures/test-uri
                          :tx [{:db/id "-1" :person/name "John" :person/age 30}]}]
        local-basis {fixtures/test-uri (d/basis-t (d/db conn))} ; get a stale basis before another user transacts
        _ @(d/transact conn [{:db/id "-1" :person/name "Bob" :person/age 50}])
        get-secure-db-with (hydrate-requests/build-get-secure-db-with staged-branches (atom {}) local-basis)
        $-nil (:db (get-secure-db-with fixtures/test-uri "nil"))]
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $-nil) nil))
    (is (thrown-with-msg? RuntimeException (re-pattern hydrate-requests/ERROR-BRANCH-PAST)
                          (get-secure-db-with fixtures/test-uri "2")))))

(deftest branch-twice []
  (let [conn (d/connect fixtures/test-uri)
        staged-branches [{:branch-ident nil
                          :uri fixtures/test-uri
                          :tx [{:db/id "-1" :person/name "John" :person/age 30}]}
                         {:branch-ident "2"
                          :uri fixtures/test-uri
                          :tx [{:db/id "-2" :person/name "Alice" :person/age 40}]}]
        local-basis {fixtures/test-uri (d/basis-t (d/db conn))}
        get-secure-db-with (hydrate-requests/build-get-secure-db-with staged-branches (atom {}) local-basis)
        $-nil (:db (get-secure-db-with fixtures/test-uri "nil"))
        $-2 (:db (get-secure-db-with fixtures/test-uri "2"))]
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $-nil) 30))
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "Alice"]] $-nil) nil))
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $-2) 30))
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "Alice"]] $-2) 40))))

(deftest branch-twice-stale []
  (let [conn (d/connect fixtures/test-uri)
        staged-branches [{:branch-ident nil
                          :uri fixtures/test-uri
                          :tx [{:db/id "-1" :person/name "John" :person/age 30}]}
                         {:branch-ident "2"
                          :uri fixtures/test-uri
                          :tx [{:db/id "-2" :person/name "Alice" :person/age 40}]}]
        local-basis {fixtures/test-uri (d/basis-t (d/db conn))} ; get a stale basis before another user transacts
        _ @(d/transact conn [{:db/id "-1" :person/name "Bob" :person/age 50}])
        get-secure-db-with (hydrate-requests/build-get-secure-db-with staged-branches (atom {}) local-basis)]
    (is (thrown-with-msg? RuntimeException (re-pattern hydrate-requests/ERROR-BRANCH-PAST)
                          (get-secure-db-with fixtures/test-uri "nil")))
    (is (thrown-with-msg? RuntimeException (re-pattern hydrate-requests/ERROR-BRANCH-PAST)
                          (get-secure-db-with fixtures/test-uri "2")))))
