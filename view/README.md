

### Changed from re-view

- Deprecated anonymous views (`defview` is the only supported way to create a view)
- Deprecated the passing of a state-atom as a `:view/state` "prop". (Use chia.reactive/atom to reactively read from arbitrary atoms.)
- Deprecated :props/defaults
- Track component construction order, sort render loop by order (so parents render before children) and prevent double-renders
- Changes in custom keys on a component: unqualified keywords are added to the view's prototype itself and camelCased, eg. (.someMethod this).
  Qualified keywords are attached to the class and accessible via the v/class-get method, eg. (v/class-get the-component :some/keyword).
- Support for functional components, by adding ^:pure metadata to a view, eg. (v/defview ^:pure my-functional-component [& args] ...).
  With pure components, there is no "instance" to speak of, so arguments are passed normally. Multiple-arity functions are not supported.
- Changes to Hiccup syntax:
  - Vector syntax supported for calling Chia views: [my-view {..props..} & children]
  - #js array syntax for interop with other React components: #js [myComponent {..props..} & children]
    - If `props` is a Clojure map, it will be converted to a javascript map with the same formatting as Chia components.
- Support for CSS-in-CLJS directly on components by adding `:view/css {..styles..}` to a component's method map. The view will be given a unique name, and styles added to the page (once).