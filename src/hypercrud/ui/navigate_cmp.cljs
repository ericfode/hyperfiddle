(ns hypercrud.ui.navigate-cmp
  (:require [hypercrud.browser.routing :as routing]
            [hypercrud.platform.native-event-listener :refer [native-listener]]
            [hypercrud.ui.tooltip :as tooltip]
            [re-com.core :as re-com]))

(defn dissoc-non-native-props [hypercrud-props]
  (dissoc hypercrud-props :route :tooltip :popover :hidden :external-hostname))

(defn anchor-cmp [hypercrud-props label]
  (let [anchor-props (-> hypercrud-props
                         (dissoc-non-native-props)
                         (assoc :href (if (:route hypercrud-props)
                                        (routing/encode (:route hypercrud-props) (:external-hostname hypercrud-props))
                                        nil #_"javascript:void 0;")))]
    ; Why would an anchor have an on-click? Is this historical.
    ; If legit it needs to respect disabled.
    [native-listener (select-keys anchor-props [:on-click])
     [:a (dissoc anchor-props :on-click) label]]))

(defn popover-cmp [hypercrud-props label]
  (let [{:keys [showing? body open! cancel!]} (:popover hypercrud-props)]
    [re-com/popover-anchor-wrapper
     :showing? showing?
     :position :below-center
     :anchor (let [btn-props (-> hypercrud-props
                                 (dissoc-non-native-props)
                                 (assoc :on-click open!))]
               [:button btn-props [:span label]])
     :popover [re-com/popover-content-wrapper
               :no-clip? true
               :body body]]))

; props = {
;   :route    {:keys [code-database fiddle-id request-params ...]}
;   :tooltip  [Keyword Hiccup]
;   :popover  {:showing?  Atom
;              :body      Component
;              :open!     () => ()
;              :cancel!   () => ()}
;   :hidden   Boolean
;
;   ; any other standard HTML anchor props, e.g.
;   :class    String
;   :style    Map[CSS-Key, CSS-Value]
;   :on-click (e) => ()
; }
; todo all HF prop values should be monads and
; tooltip can be generated within navigate-cmp by mapping over them
(defn navigate-cmp* [hypercrud-props label]
  ; why doesn't this just take anchor? - ctx should not leak up this high.
  ; props (links/build-link-props anchor ctx)
  ; and because nested router. Is that even needed now?
  (if-not (:hidden hypercrud-props)
    (let [hypercrud-props (update hypercrud-props :class #(str % " hf-auto-nav"))]
      (if (some-> hypercrud-props :popover :showing? deref)

        ; this means popover AND popover-showing - so omit the formula tooltip
        [popover-cmp hypercrud-props label]

        ; no popover showing - so can draw tooltip
        [tooltip/fast-hover-tooltip-managed
         (let [tooltip-config (:tooltip hypercrud-props)
               [status label] (if (string? tooltip-config)
                                [:info tooltip-config]
                                [(first tooltip-config) (second tooltip-config)])]
           {:status status :label label})
         (if (:popover hypercrud-props)
           [popover-cmp hypercrud-props label]
           [anchor-cmp hypercrud-props label])]))))

; act like a function down-stack
(defn navigate-cmp [props label]
  [navigate-cmp* props label])
