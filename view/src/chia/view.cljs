(ns chia.view
  (:refer-clojure :exclude [partial])
  (:require [chia.reactive :as r]
            [chia.view.render-loop :as render-loop]
            [chia.view.hiccup :as hiccup]
            [chia.view.hiccup.impl :as hiccup-impl]
            [chia.view.view-specs :as vspec]
            [goog.object :as gobj]
            [chia.view.util :as u]
            ["react-dom" :as react-dom]
            ["react" :as react]
            [clojure.core :as core]
            [chia.util.js-interop :as j])
  (:require-macros [chia.view :as v]))

(def Component react/Component)

(goog-define devtools? false)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; API - render loop

(def schedule! render-loop/schedule!)
(def force-update render-loop/force-update)
(def force-update! render-loop/force-update!)
(def flush! render-loop/flush!)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dynamic state

(def ^:dynamic *trigger-state-render* true)
(def ^:dynamic *current-view* nil)
(def ^:dynamic *reload* false)

(def instance-counter
  "For tracking the order in which components have been constructed (parent components are always constructed before their children)."
  (volatile! 0))

(defn- wrap-props
  "Wraps :on-change handlers of text inputs to apply changes synchronously."
  [props tag]
  (cond-> props
          (and ^boolean (or (identical? "input" tag)
                            (identical? "textarea" tag))
               (contains? props :on-change)) (update :on-change render-loop/apply-sync!)))

(defn- bind [f]
  (fn []
    (this-as ^js this
      (v/apply-fn f this))))

(def default-methods
  {:view/should-update
   (fn []
     (this-as ^js this
       (or (true? *reload*)
           (let [$state (.-state this)]
             (or (not= (j/get $state :props)
                       (j/get $state :prev-props))
                 (not= (j/get $state :children)
                       (j/get $state :prev-children))
                 (when-let [state (j/get $state :state)]
                   (not= @state (j/get $state :prev-state))))))))
   :static/get-derived-state-from-props
   (fn [^js props ^js $state]
     ;; when a component receives new props, update internal state.
     (j/assoc! $state
               :prev-props (.-props $state)
               :props (.-props props)
               :prev-children (.-children $state)
               :children (.-children props)))
   :view/will-unmount
   (fn []
     (this-as ^js this
       ;; manually track unmount state, react doesn't do this anymore,
       ;; otherwise our async render loop can't tell if a component is still on the page.
       (render-loop/forget! this)

       (some-> (:view/state this)
               (remove-watch this))

       (doseq [f (some-> (.-chia$onUnmount this)
                         (vals))]
         (when f (f this)))

       (r/dispose-reader! this)))
   :view/did-update
   (fn []
     (this-as ^js this
       (let [$state (.-state this)
             state-atom (.-state $state)]
         (-> $state
             (j/assoc! :prev-props (.-props $state)
                       :prev-children (.-children $state))
             (cond-> state-atom (j/assoc! :prev-state @state-atom))))))})

(defn wrap-method [k f]
  (case k
    (:view/should-update
     :view/will-receive-state) (bind f)
    :view/initial-state f
    :view/will-unmount
    (fn []
      (this-as ^js this
        (v/apply-fn f this)
        (.call (get default-methods :view/will-unmount) this)))
    :view/render
    (fn []
      (this-as ^js this
        (set! (.-chia$toUpdate this) false)                 ;; for render loop
        (r/with-dependency-tracking! this
                                     (v/apply-fn f this))))
    :view/did-update
    (fn []
      (this-as ^js this
        (v/apply-fn f this)
        (.call (get default-methods :view/did-update) this)))
    :static/get-derived-state-from-props
    (fn [props state]
      (let [default-fn (get default-methods :static/get-derived-state-from-props)]
        (f props (default-fn props state))))

    (:view/will-update
     :view/will-mount
     :view/will-receive-props) (throw (ex-info "Deprecated lifecycle method" {:method k
                                                                              :fn f}))
    (if (fn? f)
      (case (namespace k)
        "view"
        (bind f)
        (fn [& args]
          (this-as this
            (apply f this args))))
      f)))

(defn- wrap-methods
  "Augment lifecycle methods with default behaviour."
  [methods required-keys]
  (assert (set? required-keys))
  (->> (into required-keys (keys methods))
       (reduce (fn [obj k]
                 (j/assoc! obj
                           (or (get u/lifecycle-keys k) (throw (ex-info "Unknown lifecycle method" {:k k})))
                           (or (some->> (get methods k)
                                        (wrap-method k))
                               (get default-methods k)))) #js {})))

(defn- init-state!
  "Bind a component to an IWatchable/IDeref thing."
  [^js this watchable]
  (let [$state (.-state this)]
    (j/assoc! $state
              :state watchable
              :prev-state @watchable)

    (add-watch watchable this
               (fn [_ _ old-state new-state]
                 (when (not= old-state new-state)
                   (j/assoc! $state :prev-state old-state)
                   (when-let [^js will-receive (j/get this :componentWillReceiveState)]
                     (.call will-receive this))
                   (when (and *trigger-state-render*
                              (if-let [^js should-update (.-shouldComponentUpdate this)]
                                (.call should-update this)
                                true))
                     (force-update this))))))
  watchable)

(defn- init-state-atom!
  "Populate initial state for `component`."
  [^js this ^js $props]
  (when $props
    (when-let [state (when-let [initial-state (.-chia$initialState this)]
                       (let [state-data (if (fn? initial-state)
                                          (.call initial-state this this)
                                          initial-state)]
                         (atom state-data)))]
      (init-state! this state)))
  this)

(defn- get-state!
  "Lazily create and bind a state atom for `component`"
  [^js this ^js $state]
  (when-not (.-state $state)
    (init-state! this (atom nil)))
  (.-state $state))

(defmulti component-lookup (fn [this k not-found] k))

(defmethod component-lookup :default
  [this k not-found]
  not-found)

(declare class-get)

(extend-type Component
  ;; for convenience, we allow reading keys from a component's props by looking them up
  ;; directly on the component. this enables destructuring in lifecycle/render method arglist.
  ILookup
  (-lookup
    ([^js this k]
     (-lookup this k nil))
    ([^js this k not-found]
     (let [^js $state (.-state this)]
       (if (= "view" (namespace k))
         (case k
           :view/state (get-state! this $state)
           (:view/props
            :view/children
            :view/prev-props
            :view/prev-state
            :view/prev-children) (j/get $state (name k) not-found)
           ;; extendable
           (component-lookup this k not-found))
         (get (.-props $state) k not-found)))))
  r/IReadReactively
  (invalidate! [this _] (force-update this))
  INamed
  (-name [^js this] (.-displayName this))
  (-namespace [this] nil)
  IPrintWithWriter
  (-pr-writer [this writer opts]
    (-write writer (str "👁[" (name this) "]"))))

(defn swap-silently!
  "Swap a component's state atom without forcing an update (render)"
  [& args]
  (binding [*trigger-state-render* false]
    (apply swap! args)))

(defn- get-element-key [props children constructor]
  (or (get props :key)
      (when-let [class-react-key (.-key constructor)]
        (cond (string? class-react-key) class-react-key
              (keyword? class-react-key) (get props class-react-key)
              (fn? class-react-key) (.apply class-react-key (assoc props :view/children children) (to-array children))
              :else (throw (js/Error "Invalid key supplied to component"))))
      (.-displayName constructor)))

(defn- ^:export extend-constructor
  [{:keys [lifecycle-keys
           static-keys

           unqualified-keys
           qualified-keys]} constructor]

  (gobj/extend (.-prototype constructor)
               (.-prototype Component)
               (wrap-methods lifecycle-keys #{:view/should-update
                                              :view/will-unmount
                                              :view/did-update}))

  (doto (.-prototype constructor)
    (j/assoc! "displayName" (.-displayName unqualified-keys))
    (cond-> qualified-keys
            (j/assoc! "chia$class" qualified-keys)))

  (gobj/extend constructor
               (wrap-methods static-keys #{:static/get-derived-state-from-props})
               unqualified-keys)

  constructor)

(defn normalize-spec-keys [{:keys [spec/props
                                   spec/children]
                            :as m}]
  (cond-> m
          props (update :spec/props vspec/normalize-props-map)
          children (update :spec/children vspec/resolve-spec-vector)))

(defn validate-args! [{:keys [display-name qualified-keys]} props children]
  (vspec/validate-props display-name (get qualified-keys :spec/props) props)
  (vspec/validate-children display-name (get qualified-keys :spec/children) children))

(defn- view*
  "Return a React element factory."
  [view-base constructor]
  (let [view-base (update view-base :qualified-keys normalize-spec-keys)
        constructor (extend-constructor view-base constructor)]
    (doto (fn [props & children]
            (let [[{:as props
                    :keys [ref]} children] (if (or (map? props)
                                                   (nil? props))
                                             [props children]
                                             [nil (cons props children)])]
              (when goog.DEBUG
                (validate-args! view-base props children))
              (react/createElement constructor #js {"key" (get-element-key props children constructor)
                                                    "ref" ref
                                                    "props" (cond-> props ref (dissoc :ref))
                                                    "children" children})))
      (j/assoc! :chia$constructor constructor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helper functions

(defn class-get
  "Get (qualified) keys from the view's methods map.

   Does not return lifecycle methods"
  ([this k] (class-get this k nil))
  ([^js this k not-found]
   (when this
     (or (some-> (.-chia$constructor this)
                 ;(.-prototype)
                 (class-get k not-found))
         (-> (j/get this :chia$class)
             (get k not-found))))))

(defn pass-props
  "Remove prop keys handled by component, useful for passing down unhandled props to a child component.
  By default, removes all keys listed in the component's :spec/props map. Set `:consume false` for props
  that should be passed through."
  [this]
  (apply dissoc
         (get this :view/props)
         (some-> (class-get this :spec/props)
                 (get :props/consumed))))

(defn combine-props
  "Combines props, merging maps and joining collections/strings."
  [m1 m2]
  (merge-with (fn [a b]
                (cond (string? a) (str a " " b)
                      (coll? a) (into a b)
                      :else b)) m1 m2))

(defn render-to-dom
  "Render view to element, which should be a DOM element or id of element on page."
  [react-element dom-element]
  (react-dom/render react-element (cond->> dom-element
                                           (string? dom-element)
                                           (.getElementById js/document))))

(defn unmount-from-dom
  [dom-element]
  (react-dom/unmountComponentAtNode dom-element))

(defn is-react-element? [x]
  (and x (react/isValidElement x)))

(defn dom-node
  "Return DOM node for component"
  [component]
  (react-dom/findDOMNode component))

(defn on-unmount!
  "Register an unmount callback for `component`."
  [^js this key f]
  (set! (.-chia$onUnmount this)
        ((fnil assoc {}) (.-chia$onUnmount this) key f)))

(defn adapt-react-class [the-class]
  (fn [& args]
    (to-array (cons the-class args))))
