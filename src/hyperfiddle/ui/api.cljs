(ns hyperfiddle.ui.api
  (:require
    [contrib.data :refer [unwrap]]
    [contrib.reactive :as r]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.link :as link]
    [hyperfiddle.data :as data]))


(declare api-data)
(declare with-result)

(defn recurse-from-link [link ctx]
  (->> (base/data-from-link link ctx)
       (unwrap)                                             ; todo cannot swallow this error
       (api-data)))

(defn head-field [relative-path ctx & _]                    ; params noisey because data/form has crap for UI
  (let [ctx (context/focus ctx (cons :head relative-path))] ; todo :head links should fall out with link/class
    (->> @(:hypercrud.browser/links ctx)
         (filter (link/same-path-as? (:hypercrud.browser/path ctx)))
         (map #(recurse-from-link % ctx))
         (apply merge))))

(defn body-field [relative-path ctx & _]                    ; params noisey because data/form has crap for UI
  (let [ctx (context/focus ctx relative-path)]
    (->> @(:hypercrud.browser/links ctx)
         (filter (link/same-path-as? (:hypercrud.browser/path ctx)))
         (map #(recurse-from-link % ctx))
         (apply merge)
         (into (let [child-fields? (not (some->> (:hypercrud.browser/fields ctx) (r/fmap nil?) deref))]
                 (if (and child-fields? (context/attribute-segment? (last (:hypercrud.browser/path ctx)))) ; ignore relation and fe fields
                   (with-result ctx)
                   {}))))))

(defn with-result [ctx]
  (condp = (:hypercrud.browser/data-cardinality ctx)
    :db.cardinality/one (->> ctx
                             (data/form (fn [path ctx & _]
                                          (merge (head-field path (context/focus ctx [:head]))
                                                 (body-field path (context/focus ctx [:body])))))
                             (apply merge))
    :db.cardinality/many (merge (->> (data/form head-field (context/focus ctx [:head]))
                                     (apply merge))
                                (->> (r/unsequence (:hypercrud.browser/data ctx)) ; the request side does NOT need the cursors to be equiv between loops
                                     (map (fn [[relation i]]
                                            (->> (context/body ctx relation)
                                                 (data/form body-field)
                                                 (apply merge))))
                                     (apply merge)))
    ; blank fiddles
    {}))

(defn- filter-inline [links] (filter :link/render-inline? links))
(defn- remove-managed [links] (remove :link/managed? links))

(defn api-data [ctx]
  ; at this point we only care about inline links
  ; also no popovers can be opened, so remove managed
  (let [ctx (update ctx :hypercrud.browser/links (partial r/fmap (r/comp remove-managed filter-inline)))]
    (merge (with-result ctx)
           (->> (link/links-here ctx)                       ; todo reactivity
                (map #(recurse-from-link % ctx))
                (apply merge))
           {(:route ctx) @(:hypercrud.browser/result ctx)})))
