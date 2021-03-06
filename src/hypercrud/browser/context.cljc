(ns hypercrud.browser.context
  (:require [clojure.string :as string]
            [hypercrud.browser.auto-anchor-formula :as auto-anchor-formula]
            [hypercrud.browser.routing :as routing]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.core :as util]))


(defn clean [ctx]
  ; why not code-database-uri and all the custom ui/render fns?
  (dissoc ctx
          :keep-disabled-anchors? :route
          :schemas :request :fiddle
          :uri :schema :user-with!
          :find-element :attribute :value
          :layout :field
          :cell :label                                      ; TODO :cell should cascade
          ))

(defn route [ctx route]
  (let [initial-repository (->> (get-in ctx [:domain :domain/code-databases])
                                (filter #(= (:dbhole/name %) (:code-database route)))
                                first
                                (into {}))
        repository (let [overrides (->> route
                                        ; todo this is not sufficient for links on the page to inherit this override
                                        ; on navigate, this context is gone
                                        (filter (fn [[k _]] (and (string? k) (string/starts-with? k "$"))))
                                        (into {}))]
                     (update initial-repository :repository/environment merge overrides))
        ctx (-> ctx
                (update-in [:domain :domain/code-databases]
                           (fn [repos]
                             (map #(if (= initial-repository) repository %) repos)))
                (assoc :repository repository))]
    (assoc ctx :route (routing/tempid->id route ctx))))

(defn anchor-branch [ctx link]
  (if (:link/managed? link)
    ; we should run the auto-formula logic to determine an appropriate auto-id fn
    (let [child-id-str (-> [(auto-anchor-formula/deterministic-ident ctx) (:db/id link)]
                           hash util/abs-normalized - str)
          branch (branch/encode-branch-child (:branch ctx) child-id-str)]
      (assoc ctx :branch branch))
    ctx))

(defn user-with [ctx branch uri tx]
  ; todo custom user-dispatch with all the tx-fns as reducers
  ((:dispatch! ctx) (actions/with (:peer ctx) (:foo ctx) branch uri tx)))

(defn relations [ctx relations]
  (assoc ctx :relations (reactive/atom relations)))

(defn relation [ctx relation]
  (assoc ctx :relation relation))

(defn find-element [ctx fe fe-pos]
  (-> (if-let [dbname (some-> (:source-symbol fe) str)]
        (let [uri (get-in ctx [:repository :repository/environment dbname])]
          (assoc ctx :uri uri
                     :schema (get-in ctx [:schemas dbname])
                     :user-with! (reactive/partial user-with ctx (:branch ctx) uri)))
        ctx)

      ; todo why is fe necessary in the ctx?
      (assoc :find-element fe :fe-pos fe-pos)))

(let [f (fn [cell-data ctx]
          (if-let [owner-fn (:owner-fn ctx)]
            (owner-fn @cell-data ctx)))]
  (defn cell-data [ctx]
    (assert (:relation ctx))
    (assert (:fe-pos ctx))
    (let [cell-data (reactive/cursor (:relation ctx) [(:fe-pos ctx)])]
      (assoc ctx :owner (reactive/track f cell-data ctx)
                 :cell-data cell-data))))

(defn attribute [ctx attr-ident]
  (assoc ctx :attribute (get-in ctx [:schema attr-ident])))

(defn value [ctx value]
  (assoc ctx :value value))
