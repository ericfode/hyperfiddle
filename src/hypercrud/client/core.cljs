(ns hypercrud.client.core
  (:refer-clojure :exclude [update])
  (:require [cljs.core.match :refer-macros [match]]
            [goog.Uri]
            [hypercrud.client.fetch :as fetch]
            [promesa.core :as p]
            [hypercrud.client.tx :as tx]
            [clojure.walk :as walk]))


(defprotocol Graph
  (select [this named-query])
  (entity [this eid])
  (with [this more-statements]))


(defprotocol Hypercrud
  ;(authenticate!* [this username password])     ; sets a http cookie with a token
  ;(whoami [this])             ; read the cookie
  (enter! [this graph-dependencies])
  (transact! [this txs])
  (tempid! [this]))


(defprotocol HypercrudSSR
  (ssr-context [this]))


(deftype HypercrudClient [user-token t force-render! entry-uri schema state request-state local-statements]
  Graph
  (select [this named-query]
    (get-in @state [:enter t :resultsets named-query]))


  (entity [this eid]
    (tx/apply-statements-to-entity
      schema
      (concat (get-in @state [:enter t :statements]) local-statements)
      {:db/id eid}))


  (with [this more-statements]
    (HypercrudClient.
      user-token t force-render! entry-uri schema state request-state (concat local-statements more-statements)))


  Hypercrud
  ;; set up the big query, send to the server, cache it and the t if we dind't already have it
  ;; Turn the pulled-tree back into statements and resultsets; the client thinks as a graph
  ;; resultsets: set of eids
  ;; statements: local portions of the graph
  (enter! [this graph-dependencies]
    (if-let [cached (get-in @state [:enter t])]
      (p/resolved nil)
      (-> (fetch/fetch! user-token entry-uri "/enter" graph-dependencies)
          (p/then (fn [response]

                    ;; rip apart the result into resultsets and statements
                    (let [pulled-trees (-> response :body :hypercrud)
                          statements (mapcat tx/pulled-tree-to-statements pulled-trees)
                          resultsets (map :db/id pulled-trees)]

                      (swap! state update-in [:enter t] merge {:statements statements
                                                               :resultsets resultsets})))))))


  (transact! [this tx]
    (-> (fetch/transact! user-token entry-uri tx)
        (p/then (fn [resp]
                  (force-render! (-> resp :body :hypercrud :t))
                  resp))))


  (tempid! [this]
    (let [eid (get-in @state [:next-tempid] -1)]
      (swap! state update-in [:next-tempid] (constantly (dec eid)))
      eid))


  HypercrudSSR
  (ssr-context [this] request-state)

  IHash
  (-hash [this]
    (hash (map hash [entry-uri schema state request-state local-statements])))

  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other))))
