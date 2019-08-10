(ns hypercrud.browser.base
  (:require
    [cats.core :as cats :refer [mlet return]]
    [cats.monad.either :as either]
    [clojure.string :as string]
    [contrib.datomic.common.query :as query]
    [contrib.reactive :as r]
    [contrib.reader :as reader :refer [memoized-read-edn-string+]]
    [contrib.try$ :refer [try-either]]
    [hypercrud.browser.context :as context]
    [hypercrud.types.DbName :refer [#?(:cljs DbName)]]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    [hypercrud.types.QueryRequest :refer [->QueryRequest]]
    [hypercrud.types.ThinEntity :refer [#?(:cljs ThinEntity)]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.fiddle :as fiddle]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime])
  #?(:clj
     (:import
       (hypercrud.types.DbName DbName)
       (hypercrud.types.ThinEntity ThinEntity))))


(defn legacy-fiddle-ident->lookup-ref [fiddle]              ; SHould be an ident but sometimes is a long today
  ; Keywords are not a db/ident, turn it into the fiddle-id lookup ref.
  ; Otherwise, pass it through, its already a lookup-ref or eid or whatever.
  (cond
    (keyword? fiddle) [:fiddle/ident fiddle]
    (uuid? fiddle) [:fiddle/uuid fiddle]
    :else fiddle))

(defn legacy-lookup-ref->fiddle-ident [lookup-ref]
  (if (coll? lookup-ref)
    (case (first lookup-ref)
      :fiddle/ident (second lookup-ref)
      :fiddle/uuid (second lookup-ref)
      lookup-ref)
    lookup-ref))

(defn- hydrate-fiddle-from-datomic+ [rt branch fiddle-ident] ; no ctx
  (mlet [:let [dbval (->DbRef (domain/fiddle-dbname (runtime/domain rt)) branch)
               request (->EntityRequest (legacy-fiddle-ident->lookup-ref fiddle-ident) dbval fiddle/browser-pull)]
         record @(runtime/hydrate rt branch request)
         record (if-not (:db/id record)
                  (either/left (ex-info (str :hyperfiddle.error/fiddle-not-found)
                                        {:ident :hyperfiddle.error/fiddle-not-found
                                         :error-msg "Fiddle not found"
                                         :human-hint "Did you just edit :fiddle/ident?"}))
                  (either/right record))
         fiddle (case (:fiddle/type record :blank)          ; defaults have NOT yet been applied
                  :query (either/right record)  #_(mlet [query-needle (memoized-read-edn-string+ (:fiddle/query-needle record))]
                                                    (return record))
                  :entity (either/right record) #_(mlet [pull (memoized-read-edn-string+ (:fiddle/pull record))]
                                                    (return (assoc record :fiddle/pull pull)))
                  :blank (either/right record))]
    (return (fiddle/apply-defaults fiddle))))

(defn- hydrate-fiddle+ [rt branch fiddle-ident]             ; no ctx
  (-> (if (domain/system-fiddle? (runtime/domain rt) fiddle-ident)
        (->> (domain/hydrate-system-fiddle (runtime/domain rt) fiddle-ident)
             (cats/fmap fiddle/apply-defaults))
        (hydrate-fiddle-from-datomic+ rt branch fiddle-ident))))

(def browser-query-limit 50)

(defn request-for-fiddle+ [rt branch route fiddle]          ; no ctx
  (case (:fiddle/type fiddle)
    :query (mlet [q (reader/memoized-read-string+ (:fiddle/query fiddle)) ; todo why read-string instead of read-edn-string?
                  needle-clauses (either/right nil) #_(reader/memoized-read-edn-string+ (:fiddle/query-needle fiddle))
                  :let [[inputs appended-$] (if-let [in (:in (query/q->map q))]
                                              [(mapv str in) false]
                                              [["$"] true])
                        [query-args unused appended-needle]
                        (loop [query-args []
                               [route-arg & next-route-args :as route-args] (::route/datomic-args route)
                               [hole & next-holes] inputs
                               appended-needle false]
                          (let [[query-arg next-route-args] (cond
                                                              (string/starts-with? hole "$")
                                                              (if false #_(instance? DbName route-arg)
                                                                [(->DbRef (:dbname route-arg) branch) next-route-args]
                                                                [(->DbRef hole branch) (seq route-args)])

                                                              (instance? ThinEntity route-arg)
                                                              ; I think it already has the correct identity and tempid is already accounted
                                                              #_(context/smart-entity-identifier ctx route-arg)
                                                              [(.-id route-arg) next-route-args] ; throw away dbname

                                                              :else [route-arg next-route-args])
                                query-args (conj query-args query-arg)]
                            (cond
                              next-holes (recur query-args next-route-args next-holes appended-needle)
                              (and next-route-args (some? needle-clauses)) (recur query-args next-route-args ["?hf-needle"] true)
                              :else [query-args next-route-args appended-needle])))]]
             (cond
               #_#_(seq unused) (either/left (ex-info "unused param" {:query q :params query-args :unused unused}))

               (not= (count query-args) (if appended-needle
                                          (+ 1 (count inputs))
                                          (count inputs)))
               (either/left (ex-info "missing params" {:query q :params query-args :unused unused}))

               :else (let [q (cond-> q
                               appended-needle (-> ((partial apply query/append-where-clauses) needle-clauses)
                                                   (as-> q (if appended-$
                                                             (query/append-inputs q '$ '?hf-needle)
                                                             (query/append-inputs q '?hf-needle))))

                               (seq (::route/where route)) ((partial apply query/append-where-clauses) (::route/where route)))]
                       (return (->QueryRequest q query-args {:limit (or (:fiddle/limit fiddle) browser-query-limit)})))))

    :entity
    (let [args (::route/datomic-args route)                 ; Missing entity param is valid state now https://github.com/hyperfiddle/hyperfiddle/issues/268
          [dbname ?e] (if false #_(instance? DbName (first args))
                        [(:dbname (first args)) (second args)]
                        [nil (first args)])]
      (if-let [dbname (or dbname (:fiddle/pull-database fiddle))]
        (let [db (->DbRef dbname branch)
              pull-exp (or (-> (memoized-read-edn-string+ (:fiddle/pull fiddle))
                               ; todo it SHOULD be assertable that fiddle/pull is valid edn (and datomic-pull) by now (but its not yet)
                               (either/branch (constantly nil) identity))
                           ; todo this default is garbage, fiddle/pull should never be nil (defaults already applied)
                           ; and invalid edn should just fail, not nil-pun
                           ['*])]
          (either/right (->EntityRequest (or (:db/id ?e) ?e) db pull-exp)))
        (either/left (ex-info "Missing :fiddle/pull-database" {:fiddle (:fiddle/ident fiddle)}))))

    :blank (either/right nil)))

(let [nil-or-hydrate+ (fn [rt branch request]
                        (if request
                          @(runtime/hydrate rt branch request)
                          (either/right nil)))]
  (defn browse-route+ [{rt :peer branch :branch :as ctx} route]
    (mlet [route (route/validate-route+ route)              ; terminate immediately on a bad route
           route (try-either (route/invert-route route (partial runtime/tempid->id! rt branch)))
           r-fiddle @(r/apply-inner-r (r/track hydrate-fiddle+ rt branch (::route/fiddle route)))
           r-request @(r/apply-inner-r (r/fmap->> r-fiddle (request-for-fiddle+ rt branch route)))
           r-result @(r/apply-inner-r (r/fmap->> r-request (nil-or-hydrate+ rt branch)))
           ctx (-> (context/clean ctx)
                   ; route should be a ref, provided by the caller, that we fmap over
                   ; because it is not, this is obviously fragile and will break on any change to the route
                   ; this is acceptable today (Jun-2019) because changing a route in ANY way assumes the entire iframe will be re-rendered
                   (assoc :hypercrud.browser/route (r/pure route))
                   (context/fiddle+ r-fiddle))
           :let [ctx (context/result ctx r-result)]]
      ; fiddle request can be nil for no-arg pulls (just draw readonly form)
      (return ctx))))

(defn browse-link+ [ctx link-ref]
  (mlet [ctx (context/refocus-to-link+ ctx link-ref)
         args (context/build-args+ ctx @link-ref)
         route (context/build-route+ args ctx)]
    (browse-route+ ctx route)))
