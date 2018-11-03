(ns chia.prosemirror.base
  (:require ["prosemirror-inputrules" :as input-rules]
            ["prosemirror-history" :refer [history]]
            ["prosemirror-keymap" :as keymap]
            ["prosemirror-commands" :as commands]
            ["prosemirror-state" :as state :refer [EditorState]]
            [chia.view :as v]
            [chia.util.js-interop :as j]
            [chia.prosemirror.core :as pm]))

;; todo
;; Editor accepts :default-value and :value but is not an ordinary controlled component.
;; Behaving as a controlled component could be problematic, because serializing on every
;; change would slow down editing on large documents (but be consistent w/ React inputs).
;; *INTERIM MEASURE* - a CHANGED :value prop will replace the current editor state, but
;; passing the same value does not prevent edits to the local editor state.


(v/defview Editor
  "A ProseMirror editor view."
  {:spec/props {:input-rules :Vector
                :doc {:spec object?
                      :doc "A prosemirror doc"}
                :serialize {:spec :Function
                            :doc "Should convert a ProseMirror doc to Markdown."}
                :parse {:spec :Function
                        :doc "Should convert a Markdown string ot a ProseMirror doc."}
                :schema {:spec #(and (j/contains? % :nodes)
                                     (j/contains? % :marks))
                         :doc "a ProseMirror schema"}
                :on-dispatch {:spec :Function
                              :doc "(this, EditorView) - called after every update."}
                :editor-view-props {:spec :Map
                                    :doc "Passed to the EditorView constructor."}
                :keymap {:spec :Map
                         :doc "Merged as the highest-priority keymap (http://prosemirror.net/docs/ref/#keymap)."}
                :default-value {:spec :String
                                :doc "Parsed as the initial editor state."
                                :pass true}
                :value {:spec :String
                        :doc "Behaves differently from ordinary React controlled inputs. When a *new/different* :value is passed, it replaces the current doc, but continuing to pass the same :value does not freeze local state."
                        :pass true}}
   :view/did-mount (fn [{:keys [value
                                default-value
                                on-dispatch
                                view/state
                                editor-view-props
                                input-rules
                                plugins
                                parse
                                doc
                                schema]
                         user-keymap :keymap
                         :as this}]
                     (let [editor-state (.create EditorState
                                                 #js {"doc" (or doc (parse (or value default-value "")))
                                                      "schema" schema
                                                      "plugins" (cond-> [(history)
                                                                         (input-rules/inputRules
                                                                          #js {:rules (to-array (into input-rules
                                                                                                      input-rules/allInputRules))})]
                                                                        user-keymap (conj (keymap/keymap (clj->js user-keymap)))
                                                                        plugins (into plugins)
                                                                        false (conj (keymap/keymap commands/baseKeymap))
                                                                        true (to-array))})
                           editor-view (-> (v/dom-node this)
                                           (pm/EditorView. (clj->js (merge editor-view-props
                                                                           {:state editor-state
                                                                            :spellcheck false
                                                                            :attributes {:class "outline-0"}
                                                                            :dispatchTransaction
                                                                            (fn [tr]
                                                                              (let [^js/pm.EditorView pm-view (get @state :pm-view)
                                                                                    prev-state (.-state pm-view)]
                                                                                (pm/transact! pm-view tr)
                                                                                (when-not (nil? on-dispatch)
                                                                                  (on-dispatch this pm-view prev-state))))}))))]
                       (set! (.-reView editor-view) this)
                       (reset! state {:pm-view editor-view})))

   :view/did-update (fn [{value :value
                          doc :doc
                          {prev-value :value
                           prev-doc :doc} :view/prev-props
                          :as this}]
                      (when (or (and value
                                     (not= value prev-value))
                                (and doc
                                     (not= doc prev-doc)))
                        (.resetDoc this (or doc value))))
   :view/will-unmount (fn [{:keys [view/state]}]
                        (pm/destroy! (:pm-view @state)))}
  [this]
  [:.prosemirror-content
   (-> (v/pass-props this)
       (assoc :dangerouslySetInnerHTML {:__html ""}))])

(v/extend-view Editor
  Object
  (resetDoc [{:keys [view/state parse schema]} new-value]
    (let [view (:pm-view @state)]
      (.updateState view
                    (.create EditorState #js {"doc" (cond-> new-value
                                                            (string? new-value) (parse))
                                              "schema" schema
                                              "plugins" (aget view "state" "plugins")}))))
  (pmView [{:keys [view/state]}]
    (:pm-view @state))
  (serialize [{:keys [view/state serialize]}]
    (some-> (:pm-view @state)
            (j/get-in [:state :doc])
            (serialize))))

