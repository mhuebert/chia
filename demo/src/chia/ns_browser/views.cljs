(ns chia.ns-browser.views
  (:require [chia.view :as v]
            [chia.triple-db :as d]
            [clojure.string :as str]
            [cljs.pprint :as pp]
            [goog.string :as gstr]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [goog.object :as gobj]
            [cljs.js :as cljsjs]
            [chia.view.util :as util]
            [cljs.analyzer :as ana]
            [cljs.env :as env]
            [chia.util :as u]))

(defonce compiler (cljsjs/empty-state))

(defn eval-in [ns form]
  (let [result (volatile! nil)]
    (cljsjs/eval compiler form {:ns ns
                                :eval cljsjs/js-eval}
                 #(vreset! result %))
    @result))

(def separator-ch \•)
(def separator [:span.opacity-30.pad-right-1 separator-ch])

(defn get-eggs
  "Finds registered eggs in a namespace"
  {:lark/egg (partial str "get-eggies: ")}
  [ns]
  (-> @compiler
      (get-in [:cljs.analyzer/namespaces ns :defs])
      (vals)
      (->> (filter (comp :lark/egg :meta)))
      (seq)))

(defn pre-wrap [forms]
  [:div {:classes [:text/pre-wrap.height-4]}
   (-> forms
       (pp/write :dispatch pp/code-dispatch)
       (with-out-str))])

(defn cljs? [ns]
  (or (gstr/startsWith (name ns) "cljs.")
      (gstr/startsWith (name ns) "clojure.")))

(defn macros? [ns]
  (gstr/endsWith (name ns) "$macros"))

(defn get-children [m]
  (seq (dissoc m :ns)))

(defn resolve-to-value [sym]
  (->> (.split (str sym) #"[./]")
       (map munge)
       (to-array)
       (apply gobj/getValueByKeys js/window)))

(defn resolve-sym [ns sym]
  (binding [env/*compiler* compiler]
    (ana/resolve-var (assoc @compiler :ns ns) sym)))

(v/defview view-egg
  {:key :name}
  [{the-var :view/props
    the-ns :ns
    var-meta :meta
    var-name :name
    doc :doc}]
  (let [expanded? (d/get var-name :expanded? true)]
    [:div {:classes [:border/radius-10
                     :overflow/hidden
                     :layout/inline-block
                     :clear/both
                     :align/top
                     :bg/darken-2
                     :text/size-5
                     :margin/all-2.left-0
                     :flex/column]}
     [:div
      {:classes [:pad/left-3
                 :flex/row.items-center
                 :cursor/pointer
                 :hover:parent/opacity]
       :on-click #(d/transact! [[:db/add var-name :expanded? (not expanded?)]])}
      [:div {:classes [:flex/auto
                       :pad/v-2]} (name var-name)]
      [:div {:classes [:pad/v-2.h-3
                       :text/size-4.center
                       :opacity-50
                       :child/opacity-100]} (if expanded? \– \+)]]
     (when doc
       [:div {:classes [:pad/v-2.h-3
                        :text/size-6
                        :bg/lighten-4]}
        doc])
     (when expanded?
       [:div
        {:classes [:bg/darken-1
                   :pad/v-2.h-3]}
        (let [value (resolve-to-value (:name the-var))
              egg (get var-meta :lark/egg)
              egg-view (or (cond (true? egg) (comp pre-wrap str)
                                 (symbol? egg) (->> egg
                                                    (resolve-sym the-ns)
                                                    :name
                                                    (resolve-to-value)))
                           (constantly (str "Egg must be true or a function")))]
          (egg-view value))])]))

(defn common-prefix-count [coll-a coll-b]
  (loop [coll-a coll-a
         coll-b coll-b
         i 0]
    (if (or (not= (first coll-a) (first coll-b))
            (nil? coll-a)
            (nil? coll-b))
      i
      (recur (next coll-a) (next coll-b) (inc i)))))



(defn format-segments [{:keys [segments
                               printed-segments]}]
  (let [path (drop-last segments)
        segment (last segments)]
    (list
     (->> (for [i (range (count path))
                :let [x (nth path i)]]
            [:span (when (< i printed-segments)
                     {:classes [:opacity-50]})
             [:a {:classes [:hover:text/underline
                            :pad/right-1]} x]])
          (interpose separator))
     separator
     [:span {:classes [:text/semi-bold
                       :pad/right-1]} segment])))

(defn doc-line [doc]
  (when doc
    [:div {:classes [:opacity-70]}
     (first (str/split-lines doc))]))

(v/defview view-def
  {:key :name}
  [{var-name :name
    the-ns :ns
    :keys [doc meta]
    {:keys [lark/egg]} :meta
    the-var :view/props}]
  (if egg
    (view-egg (assoc the-var :ns the-ns))
    [:div
     {:classes [:pad/v-1
                :flex/row
                :text/size-6]}
     [:div.text-nowrap.pad-right-2
      (name var-name)]
     (doc-line doc)]))

(v/defview ^:lark/egg view-namespace
  "Displays a namespace line-item"
  {:key :name}
  [{the-ns :view/props
    ns-name :name
    {ns-doc :doc} :meta
    :keys [segments
           printed-segments
           doc]
    :or {printed-segments 0}}]
  (let [{:keys [expanded? starred?]} (d/entity ns-name)
        {:keys [defs-with-doc
                macros
                defs-bare]} (-> @compiler
                                (get-in [::ana/namespaces ns-name])
                                (select-keys [:defs :macros])
                                (->> (sequence (comp (mapcat (comp vals second))
                                                     (map #(assoc % :ns the-ns)))))
                                (seq)
                                (->> (group-by (fn [{:keys [meta doc macro]}]
                                                 (cond macro :macros
                                                       (or (:lark/egg meta)
                                                           doc) :defs-with-doc
                                                       :else :defs-bare)))))]
    (list [:div
           [:div {:classes [:flex/row.items-center
                            :margin/v-2
                            :hover:parent/opacity]}
            [:div.material-icons.pointer.tc.transition.opacity-0.width-6
             {:classes (if starred? [:text/orange
                                     :opacity-100]
                                    [:text/darken-5
                                     :child/opacity-100
                                     :hover/opacity-70.text-orange])
              :on-click #(do (.preventDefault %)
                             (d/transact! [[:db/update-attr ns-name :starred? (comp not boolean)]]))}
             (if starred?
               "star"
               "star_border")]

            (format-segments {:segments segments
                              :printed-segments printed-segments})]]

          [:div.pad-left-6
           [:div {:classes [:margin/v-2
                            :text/height-5.size-5]}
            (doc-line ns-doc)]
           (some->> defs-with-doc
                    (sort-by (comp js/parseInt :line :meta))
                    (map view-def))

           ;; we should only show non-doc'd functions when a user has zoomed to the namespace, and checked "View All".. [ eggs • docs • all ]
           ])))

(defn add-print-info [namespaces]
  (->> namespaces
       (reduce (fn [out [_ {ns-name :name :as the-ns}]]
                 (let [segments (str/split (name ns-name) #"\.")]
                   (conj out
                         (-> the-ns
                             (assoc :segments segments
                                    :printed-segments
                                    (common-prefix-count (:segments (peek out))
                                                         segments)))))) [])))

(v/defview carton [{compiler :view/state}]
  [:div
   {:classes [:text/pre-wrap
              :pad/right-3]}
   (->> @compiler
        :cljs.analyzer/namespaces
        (remove (comp (util/any-pred cljs? macros? (complement get-eggs)) first))
        (add-print-info)
        (map view-namespace))])

(s/def ::ns-entry (s/cat :namespace-segment string?
                         :subtree map?))

(s/fdef view-tree
        :args (s/cat :map map?
                     :vector vector?))

(st/instrument)