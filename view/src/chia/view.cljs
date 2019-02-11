(ns chia.view
  (:require ["react-dom" :as react-dom]
            ["react" :as react]
            [chia.reactive :as r]
            [chia.view.render-loop :as render-loop]
            [chia.view.hiccup :as hiccup]
            [chia.view.hiccup.impl :as hiccup-impl]
            [chia.view.view-specs]
            [chia.view.util :as view-util]
            [chia.util :as u]
            [chia.util.js-interop :as j]
            [chia.view.registry :as registry]
            [clojure.core :as core]
            [clojure.spec.alpha :as s]
            [goog.object :as gobj])
  (:require-macros [chia.view :as v]))

(goog/exportSymbol "React" react)
(goog/exportSymbol "ReactDOM" react-dom)

(def create-element react/createElement)
(def is-valid-element? react/isValidElement)

(def Component react/Component)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Render loop

(def schedule! render-loop/schedule!)
(def force-update render-loop/force-update)
(def force-update! render-loop/force-update!)
(def flush! render-loop/flush!)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dynamic vars

(def ^:dynamic *current-view*
  "Tracks the currently-rendering component."
  nil)

(def ^:dynamic *reload*
  "When true, all components re-render, regardless of shouldComponentUpdate."
  false)

(def instance-counter
  "For tracking the order in which components have been constructed (parent components are always constructed before their children)."
  (volatile! 0))

(defn- update-change-prop [props]
  (cond-> props
          (contains? props :on-change) (update :on-change render-loop/apply-sync!)))

(defn- wrap-props
  "Wraps :on-change handlers of text inputs to apply changes synchronously."
  [props tag]
  (cond-> props
          (and ^boolean (or (identical? "input" tag)
                            (identical? "textarea" tag))) update-change-prop))

(defn- bind [f]
  (fn []
    (this-as ^js this
      (v/apply-fn f this))))

(defn get-derived-state-from-props [props $state]
  ;; when a component receives new props, update internal state.
  (j/assoc! $state
            :prev-props (j/!get $state :props)
            :props (j/!get props :props)
            :prev-children (j/!get $state :children)
            :children (j/!get props :children)))

(def default-methods
  {:view/should-update
                                        (fn []
                                          (this-as this
                                            (or (true? *reload*)
                                                (let [$state (j/!get this :state)]
                                                  (or (not= (j/!get $state :props)
                                                            (j/!get $state :prev-props))
                                                      (not= (j/!get $state :children)
                                                            (j/!get $state :prev-children))
                                                      (when-let [state (j/!get $state :state-atom)]
                                                        (not= @state (j/!get $state :prev-state))))))))
   :static/get-derived-state-from-props get-derived-state-from-props
   :view/will-unmount
                                        (fn []
                                          (this-as this
                                            ;; manually track unmount state, react doesn't do this anymore,
                                            ;; otherwise our async render loop can't tell if a component is still on the page.

                                            (some-> (:view/state this)
                                                    (remove-watch this))

                                            (doseq [f (some-> (j/!get this :chia$onUnmount)
                                                              (vals))]
                                              (when f (f this)))

                                            (r/dispose-reader! this)
                                            (render-loop/forget! this)))
   :view/did-update
                                        (fn []
                                          (this-as ^js this
                                            (let [$state (j/!get this :state)
                                                  state-atom (j/!get $state :state-atom)]
                                              (-> $state
                                                  (j/assoc! :prev-props (j/!get $state :props)
                                                            :prev-children (j/!get $state :children))
                                                  (cond-> state-atom (j/assoc! :prev-state @state-atom))))))})

(defn wrap-method [k f]
  (if-not f
    (default-methods k)
    (case k
      (:view/should-update
       :view/will-receive-state) (bind f)
      :view/will-unmount
      (fn []
        (this-as ^js this
          (v/apply-fn f this)
          (.call (default-methods :view/will-unmount) this)))
      :view/render
      (fn []
        (this-as ^js this
          (j/assoc! this :chia$toUpdate false)              ;; avoid double-render in render loop
          (r/with-dependency-tracking! this
            (v/apply-fn f this))))
      :view/did-update
      (fn []
        (this-as ^js this
          (v/apply-fn f this)
          (.call (default-methods :view/did-update) this)))
      :static/get-derived-state-from-props
      (fn [props state]
        (let [default (default-methods :static/get-derived-state-from-props)]
          (f props (default props state))))

      (if (fn? f)
        (case (namespace k)
          "view"
          (bind f)
          (fn [& args]
            (this-as this
              (apply f this args))))
        f))))

(defn- wrap-methods
  "Augment lifecycle methods with default behaviour."
  [methods required-keys]
  (->> (into required-keys (keys methods))
       (reduce (fn [obj k]
                 (j/assoc! obj
                           (view-util/lifecycle-keys k)
                           (wrap-method k (get methods k)))) #js {})))

(defn- init-state!
  "Bind a component to an IWatchable/IDeref thing."
  [^js this watchable]
  (-> (j/!get this :state)
      (j/assoc! :state-atom watchable
                :prev-state @watchable))

  (add-watch watchable this
             (fn [_ _ old-state new-state]
               (when (not= old-state new-state)
                 (j/assoc-in! this [:state :prev-state] old-state)
                 (when-let [^js will-receive (j/!get this :componentWillReceiveState)]
                   (.call will-receive this))
                 (when (and (not r/*silent*)
                            (if-let [^js should-update (j/!get this :shouldComponentUpdate)]
                              (.call should-update this)
                              true))
                   (force-update this)))))
  watchable)

(defn- populate-initial-state!
  "Populate initial state for `component`."
  [^js this ^js $props initial-state]
  (let [state-data (if (fn? initial-state)
                     (let [$state (j/!get this :state)]
                       (j/set! this :state (get-derived-state-from-props $props $state))
                       (apply initial-state this (:view/children this)))
                     initial-state)]
    (init-state! this (atom state-data)))
  this)

(defn- get-state!
  "Lazily create and bind a state atom for `component`"
  [this]
  (let [$state (j/get this :state)]
    (when-not (j/contains? $state :state-atom)
      (init-state! this (atom nil)))
    (j/!get $state :state-atom)))

(defn- get-special [this k not-found]
  (case k
    :view/state (get-state! this)
    (or (-> this
            (j/!get :state)
            (j/!get (name k)))
        not-found)))

(defn- get-prop [this k not-found]
  (get (get-special this :view/props nil) k not-found))

(when Component
  (extend-type Component
    ;; for convenience, we allow reading keys from a component's props by looking them up
    ;; directly on the component. this enables destructuring in lifecycle/render method arglist.
    ILookup
    (-lookup
      ([this k]
       (-lookup this k nil))
      ([^js this k not-found]
       (case (namespace k)
         "view" (get-special this k not-found)
         (get-prop this k not-found))))
    r/IReadReactively
    (-invalidate! [this _] (force-update this))
    INamed
    (-name [this] (j/!get this :displayName))
    (-namespace [this] nil)
    IPrintWithWriter
    (-pr-writer [this writer opts]
      (-write writer (str "👁[" (name this) "]")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- ^:export extend-constructor
  [{:keys [lifecycle-keys
           static-keys

           unqualified-keys
           qualified-keys]} constructor]

  (gobj/extend (.-prototype constructor)
               (.-prototype Component)
               (wrap-methods lifecycle-keys [:view/should-update
                                             :view/will-unmount
                                             :view/did-update]))

  (doto (.-prototype constructor)
    (j/assoc! "displayName" (.-displayName unqualified-keys))
    (cond-> qualified-keys
            (j/assoc! "chia$class" qualified-keys)))

  (gobj/extend constructor
               (wrap-methods static-keys [:static/get-derived-state-from-props])
               unqualified-keys)

  constructor)

(defn validate-specs [{prop-spec     :spec/props
                       children-spec :spec/children} props children]
  (when js/goog.DEBUG
    (some-> prop-spec
            (s/explain-data props)
            (js/console.warn))
    (some-> children-spec
            (s/explain-data children)
            (js/console.warn))))

(defn- element-key [props children constructor]
  (str (or (get props :key)
           (when-let [class-react-key (j/get constructor :key)]
             (cond (string? class-react-key) class-react-key
                   (keyword? class-react-key) (get props class-react-key)
                   (fn? class-react-key) (.apply class-react-key (assoc props :view/children children) (to-array children))
                   :else (throw (js/Error "Invalid key supplied to component"))))
           (j/!get constructor :displayName))))

(defn- view*
  "Return a React element factory."
  [view-base constructor]
  (let [constructor (extend-constructor view-base constructor)]
    (doto (fn [props & children]
            (let [[{:as   props
                    :keys [ref]} children] (if (or (map? props)
                                                   (nil? props))
                                             [props children]
                                             [nil (cons props children)])]
              (validate-specs (:spec-keys view-base) props children)

              (create-element constructor #js {"key"      (str (element-key props children constructor))
                                               "ref"      ref
                                               "props"    (some-> props
                                                                  (dissoc :ref))
                                               "children" children})))
      (j/assoc! :chia$constructor constructor))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn to-element [x]
  (hiccup/element {:wrap-props wrap-props} x))

(defn element? [x]
  (and x (is-valid-element? x)))

(defn component? [x]
  (identical? Component x))

(defn- resolve-node [node-or-id]
  (cond->> node-or-id
           (string? node-or-id)
           (.getElementById js/document)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn portal
  [react-element dom-element]
  (react-dom/createPortal (to-element react-element)
                          (resolve-node dom-element)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn render-to-dom
  "Render view to element, which should be a DOM element or id of element on page."
  ([react-element dom-element {:as   options
                               :keys [reload?]}]
   (if-not reload?
     (render-to-dom react-element dom-element)
     (binding [*reload* true]
       (render-to-dom react-element dom-element))))
  ([react-element dom-element]
   (react-dom/render (to-element react-element)
                     (resolve-node dom-element))))

(defn unmount-from-dom
  [dom-element]
  (react-dom/unmountComponentAtNode dom-element))

(defn dom-node
  "Return DOM node for component"
  [component]
  (react-dom/findDOMNode component))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn on-unmount!
  "Register an unmount callback for `component`. This is not a hook - can be used anywhere/anytime."
  [^js this key f]
  (j/update! this :chia$onUnmount assoc key f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn adapt-react-class
  ([the-class]
   (adapt-react-class nil the-class))
  ([{:keys [->element-keys
            ->js-keys
            lift-nses]} the-class]
   (fn [& args]
     (let [[props children] (if (or (map? (first args))
                                    (nil? (first args)))
                              [(first args) (rest args)]
                              [{} args])
           props (-> props
                     (u/update-some-keys ->element-keys to-element)
                     (u/update-some-keys ->js-keys clj->js)
                     (cond-> (seq lift-nses) (u/lift-nses lift-nses))
                     (update-change-prop)
                     (hiccup-impl/props->js))
           js-form (-> (cons props children)
                       (to-array)
                       (j/unshift! the-class))]
       (to-element js-form)))))

(defn merge-props
  "Merge props, concatenating :class props and merging styles."
  [m1 m2]
  (merge m1
         m2
         (merge-with #(str %1 " " %2)
                     (select-keys m1 [:class])
                     (select-keys m2 [:class]))
         (merge-with merge
                     (select-keys m1 [:style])
                     (select-keys m2 [:style]))))

(defn partial-props [view initial-props]
  (fn [props & children]
    (let [[props children] (if (or (map? props)
                                   (nil? props)) [props children]
                                                 [{} (cons props children)])]
      (into [view (merge-props initial-props props)] children))))

(defn class-get
  "Get (qualified) keys from the view's methods map.

   Does not return lifecycle methods"
  ([this k] (class-get this k nil))
  ([^js this k not-found]
   (or (when this
         (when-let [class (or (j/get this :chia$class)
                              (j/get-in this [:chia$constructor
                                              :prototype
                                              :chia$class]))]
           (get class k)))
       not-found)))

;; Hooks
;;
;; exploring functional components and hooks,
;; experimental / not stable

(def use-state* react/useState)
(def use-effect react/useEffect)
(def use-context react/useContext)
(def use-reducer react/useReducer)
(def use-callback react/useCallback)
(def use-memo react/useMemo)
(def use-ref react/useRef)
(def use-imperative-handle react/useImperativeHandle)
(def use-layout-effect react/useLayoutEffect)
(def use-debug-value react/use-debug-value)

(defn use-force-update! []
  (-> (use-reducer inc 0)
      (unchecked-get 1)))

(defn use-on-unmount [f]
  (use-effect (constantly f) #js []))

(defn use-chia [view-name]
  (let [force-update! (use-force-update!)
        chia$state (-> (use-ref #js {:chia$forceUpdate force-update!})
                       (j/get :current))]
    (use-on-unmount
     (fn []
       (r/dispose-reader! force-update!)
       (doseq [f (some-> (j/get chia$state :chia$onUnmount)
                         (vals))]
         (f))))
    chia$state))

(defn use-state
  ([] (use-state nil))
  ([initial-state]
   (let [force-update! (use-force-update!)
         state-atom (-> (use-ref (atom initial-state))
                        (j/get :current))]
     (use-effect (fn []
                   (add-watch state-atom :atom-hook
                              (fn [_ _ old-state new-state]
                                (when (not= old-state new-state)
                                  (force-update!))))
                   #(remove-watch state-atom :atom-hook)) #js [])
     state-atom)))
