(ns hyperfiddle.ui.controls
  (:refer-clojure :exclude [boolean keyword long])
  (:require
    [clojure.set :refer [rename-keys]]
    [contrib.datomic-tx :as tx]
    [contrib.reactive :as r]
    [contrib.string :refer [empty->nil]]
    [contrib.ui :refer [debounced optimistic-updates]]      ; avoid collisions
    [contrib.ui.input :as input]
    [contrib.ui.recom-date :refer [recom-date]]
    [hypercrud.browser.context :as context]))


(defn readonly->disabled [props]                            ; this utility is harmful and should be removed
  ; :placeholder, etc is allowed and pass through
  (rename-keys props {:read-only :disabled}))

(defn on-change->tx [ctx o n]
  (let [id @(r/fmap :db/id (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))
        attribute @(context/hydrate-attribute ctx (last (:hypercrud.browser/path ctx)))]
    (tx/edit-entity id attribute o (empty->nil n))))

(defn entity-change->tx [ctx new-val]                       ; legacy todo remove
  (let [entity @(get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
        attr-ident (last (:hypercrud.browser/path ctx))
        {:keys [:db/cardinality :db/valueType]} @(context/hydrate-attribute ctx attr-ident)
        o (if (not= (:db/ident valueType) :db.type/ref)
            (get entity attr-ident)
            (case (:db/ident cardinality)
              :db.cardinality/one (get-in entity [attr-ident :db/id])
              :db.cardinality/many (->> (get entity attr-ident)
                                        (map :db/id))))]
    (on-change->tx ctx o new-val)))

(defn writable-entity? [entity-val]
  ; If the db/id was not pulled, we cannot write through to the entity
  (cljs.core/boolean (:db/id entity-val)))

(let [on-change (fn [ctx o n]
                  (->> (on-change->tx ctx o n)
                       (context/with-tx! ctx)))]
  (defn entity-props
    ([props ctx]
     (-> (assoc props :on-change (r/partial on-change ctx))
         (update :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))))
    ; val is complected convenience arity, coincidentally most consumers need to assoc :value
    ([val props ctx] (entity-props (assoc props :value val) ctx))))


(defn ^:export keyword [val ctx & [props]]
  (let [props (-> (entity-props val props ctx)
                  readonly->disabled)]
    [optimistic-updates props debounced input/keyword]))

(defn ^:export string [val ctx & [props]]
  (let [props (-> (entity-props val props ctx)
                  readonly->disabled)]
    [optimistic-updates props debounced input/text #_contrib.ui/textarea]))

(defn ^:export long [val ctx & [props]]
  (let [props (-> (entity-props val props ctx)
                  readonly->disabled)]
    [optimistic-updates props debounced input/long]))

(defn ^:export boolean [val ctx & [props]]
  [:div (let [props (readonly->disabled props)]
          (update props :class #(str % (if (:disabled props) " disabled"))))
   (let [props (-> (entity-props props ctx)
                   (readonly->disabled)
                   (assoc :checked val))]
     [contrib.ui/easy-checkbox props])])

(let [adapter (fn [e]
                (case (.-target.value e)
                  "" nil
                  "true" true
                  "false" false))]
  (defn ^:export tristate-boolean [val ctx & [props]]
    (let [option-props (-> (readonly->disabled props)
                           (update :disabled #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))))))]
      [:select (-> (dissoc props :label-fn)
                   (assoc :value (if (nil? val) "" (str val))
                          :on-change (r/comp
                                       (r/partial context/with-tx! ctx)
                                       (r/partial on-change->tx ctx val)
                                       adapter)))
       [:option (assoc option-props :key true :value "true") "True"]
       [:option (assoc option-props :key false :value "false") "False"]
       [:option (assoc option-props :key :nil :value "") "--"]])))

(defn ^:export ref [val ctx & [props]]
  (let [props (-> (entity-props val #_(or (:db/ident val) (:db/id val)) props ctx)
                  readonly->disabled)]
    [optimistic-updates props debounced input/edn]))

(defn ^:export dbid [val ctx & [props]]
  (let [props (-> (entity-props (:db/id val) props ctx)
                  readonly->disabled)]
    [optimistic-updates props debounced input/edn]))

(defn ^:export instant [val ctx & [props]]
  (let [props (-> (entity-props val props ctx)
                  readonly->disabled)]
    [recom-date props]))

(defn- code-comp [ctx]
  (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
    :hyperfiddle.ui.layout/block contrib.ui/code
    :hyperfiddle.ui.layout/table contrib.ui/code-inline-block))

(defn ^:export code [val ctx & [props]]
  (let [props (-> (entity-props val props ctx)
                  (update :mode #(or % "clojure"))
                  (assoc :parinfer @(r/fmap :hyperfiddle.ide/parinfer (:hyperfiddle.ide/user ctx))))]
    [optimistic-updates props debounced (code-comp ctx)]))

(defn ^:export css [val ctx & [props]]
  (let [props (-> (entity-props val props ctx)
                  (update :mode #(or % "css")))]
    [optimistic-updates props debounced (code-comp ctx)]))

(defn ^:export markdown-editor [val ctx & [props]]              ; This is legacy; :mode=markdown should be bound in userland
  (let [props (-> (entity-props val props ctx)
                  (assoc :mode "markdown"
                         :lineWrapping true))]
    [optimistic-updates props debounced (code-comp ctx)]))

(defn- edn-comp [ctx]
  (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
    :hyperfiddle.ui.layout/block contrib.ui/edn
    :hyperfiddle.ui.layout/table contrib.ui/edn-inline-block))

(defn ^:export edn-many [val ctx & [props]]
  (let [valueType @(context/hydrate-attribute ctx (last (:hypercrud.browser/path ctx)) :db/valueType :db/ident)
        val (set (if (= valueType :db.type/ref) (map :db/id val) val))
        props (entity-props val props ctx)]
    [optimistic-updates props debounced (edn-comp ctx)]))

(defn ^:export edn [val ctx & [props]]
  (let [props (entity-props val props ctx)]
    [optimistic-updates props debounced (edn-comp ctx)]))

;(defn textarea* [{:keys [value on-change] :as props}]       ; unused
;  (let [on-change #(let [newval (.. % -target -value)]
;                     (on-change [value] [newval]))]
;    [:textarea (assoc props :on-change on-change)]))
