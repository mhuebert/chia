;; EXPERIMENTAL

(ns chia.material-ui.build
  (:require ["fs" :as fs]
            ["typescript" :as ts]
            ["ts-morph" :as tsm]
            [clojure.string :as str]
            [applied-science.js-interop :as j]))

(defn walk-dir
  [path]
  (let [{files false
         dirs  true} (->> (fs/readdirSync path)
                          (map #(str path "/" %))
                          (group-by #(some-> ^js (fs/statSync %)
                                             (.isDirectory))))]
    (into files
          (mapcat walk-dir dirs))))

(defn all-components []
  (->> (walk-dir "node_modules/@material-ui/core")
       (filter #(and (str/ends-with? % ".d.ts")
                     (not (re-find #"/index\.|test" %))))
       (keep (fn [path]
               (when-let [Name (second (re-find #"([A-Z][^/]+)/\1\.d\.ts$" path))]
                 {:Name      Name
                  :full-path path
                  :path      (second (re-find (re-pattern (str ".*(@material-ui.*)/" Name ".d.ts")) path))})))))

(def syntax-kind (.-SyntaxKind ts))

(defn kind-name [kind]
  (keyword (j/get syntax-kind (str kind))))

(defn kind [node]
  (kind-name (.-kind node)))

(def primitives #{:AnyKeyword
                  :BooleanKeyword
                  :NumberKeyword
                  :StringKeyword})

(deftype Node [name type])

(defn add-child! [node name type]
  (let [child (Node. name type)]
    (j/update! node .-children (fnil j/push! #js[]) child)
    child))

(defn to-obj [node]
  (j/obj (.-name node)
         (if-some [children (.-children node)]
           (->> (mapv to-obj children)
                (reduce (fn [pv child]
                          (reduce (fn [pv k]
                                    (if (j/contains? pv k)
                                      (j/extend! pv child)
                                      (j/assoc! pv k (j/get child k)))
                                    pv) pv (js-keys child))) #js{}))
           (.-type node))))

(defn resolve-array-type [node]
  (loop [adeep 0
         node node]
    (if (= (kind node) :ArrayType)
      (recur (inc adeep) (.-elementType node))
      [node adeep])))

(defn id-name [node-name]
  (case (kind node-name)
    :IdentifierName (.text node-name)
    :QualifiedName (str (.-left node-name) "." (.-right node-name))
    (kind node-name)))

(defn visit [^js parent-node]
  (fn [^js node]
    (case (kind node)
      :ModuleDeclaration
      (ts/forEachChild node (visit (add-child! parent-node (.. node -name -text) nil)))

      :ModuleBlock
      (ts/forEachChild node (visit parent-node))

      :InterfaceDeclaration
      (let [i-name (.. node -name -text)]
        (j/assoc! parent-node i-name #js{})
        (ts/forEachChild
         node (visit
               (add-child! parent-node i-name nil))))

      :PropertySignature
      (let [prop-name (cond (string? node) node
                            (j/contains? node :name) (.-name node)
                            :else node)
            [prop-type a-deep] (resolve-array-type node)
            prop-kind (kind prop-type)]
        (if (= prop-kind :TypeReference)
          (let [real-type (.-typeName prop-type)]
            (prn :R prop-name :T prop-name)
            (add-child! parent-node prop-name
                        (str (.repeat "Array<" a-deep)
                             (if (= :QualifiedName (kind real-type))
                               (.getText real-type)
                               (j/get real-type :text real-type))
                             (.repeat ">" a-deep))))
          (if (primitives prop-kind)
            (add-child! parent-node prop-name prop-kind)
            (do
              (when (not= (.-text (.-name prop-type)) prop-name)
                #_(prn node (kind node) :not= prop-name (.-text (.-name prop-type)) #_(js-keys prop-type)))

              ))
          ))
      :ClassDeclaration nil #_(prn :ClassDeclaration (.. node -name -text) (js-keys node))
      :TypeAliasDeclaration nil
      :ExportDeclaration (prn :Export  (js-keys node))
      (if-let [name (j/get-in node [.-name .-text])]
        (do :ignore name (kind node))
        (prn :unknown (kind node))))))



(defn parse [filenames]
  (let [program (ts/createProgram (to-array filenames) #js{})
        node (Node. "root" nil)]
    (doseq [file (.getSourceFiles program)]
      (ts/forEachChild file (#'visit node)))
    (j/get (to-obj node) :root)))

(let [components (all-components)
      tree (parse ["node_modules/@material-ui/core/index.d.ts"])]
  (js-keys tree)
  (j/select-keys tree (mapv :Name components))

  (js-keys tree)
  nil
  #_(mapv :Name components)
  #_(for [file files
          :when (.isDeclarationFile file)
          :let [interfaces (.getInterfaces file)
                i (first interfaces)]
          :when (and i (.getExtends i))]
      {:s       (.getStructure i)
       :extends (mapv (comp #(.getText %) #(.getExpression %)) (.getExtends i))
       :members (mapv #(.getName %) (.getMembers i))
       :t       (.getText i)}
      #_{#_#_:t (.getText i)
         :members (mapv #(.getName %) (.getMembers i))
         :ext     (mapv #(.-kind %) (.getExtends i))
         :meth    (.getMethods i)
         :base-d  (.getBaseDeclarations i)
         :base-t  (.getBaseTypes i)}))

(defn camel-dash [s]
  (-> s
      (str/replace #"^[A-Z]" str/lower-case)
      (str/replace-all #"[A-Z]" #(str "-" (str/lower-case %)))))

(defn generate []
  (doseq [{:keys [Name path]} (all-components)
          :let [name (camel-dash Name)]]
    (fs/writeFileSync
     (str "src/chia/material_ui/" (munge name) ".cljs")
     (str "(ns chia.material-ui." name "
  (:require [\"" path "\" :as " Name "]))

(def " name " " Name "/default)"))))

