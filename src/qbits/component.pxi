(ns qbits.component
  (:require
   [qbits.dependency :as dep]
   [qbits.dependency.utils :refer [sort]]))

(defprotocol Lifecycle
  (start [component]
    "Begins operation of this component. Synchronous, does not return
  until the component is started. Returns an updated version of this
  component.")
  (stop [component]
    "Ceases operation of this component. Synchronous, does not return
  until the component is stopped. Returns an updated version of this
  component."))

(declare start-system stop-system)
(defn system?
  [this]
  (some-> this meta ::system?))

;; No-op implementation if one is not defined.
(extend-protocol Lifecycle
  IObject
  (start [this]
    (if (system? this)
      (start-system this)
      this))
  (stop [this]
    (if (system? this)
      (stop-system this)
      this)))

(defn dependencies
  "Returns the map of other components on which this component depends."
  [component]
  (::dependencies (meta component) {}))

(defn using
  "Associates metadata with component describing the other components
  on which it depends. Component dependencies are specified as a map.
  Keys in the map correspond to keys in this component which must be
  provided by its containing system. Values in the map are the keys in
  the system at which those components may be found. Alternatively, if
  the keys are the same in both the component and its enclosing
  system, they may be specified as a vector of keys."
  [component dependencies]
  (vary-meta
   component update-in [::dependencies] (fnil merge {})
   (cond
    (map? dependencies)
      dependencies
    (vector? dependencies)
      (into {} (map (fn [x] [x x]) dependencies))
    :else
    (throw [{:type ::InvalidDependencies
             :reason ::invalid-dependencies
             :component component
             :dependencies dependencies}
            "Dependencies must be a map or vector"]))))

(defn nil-component [system key]
  [{:type ::NilComponent
    :reason ::nil-component
    :system-key key
    :system system}
   (str "Component " key " was nil in system; maybe it returned nil from start or stop")])

(defn get-component [system key]
  (let [component (get system key ::not-found)]
    (when (nil? component)
      (throw (nil-component system key)))
    (when (= ::not-found component)
      (throw [{:type ::MissingComponent
               :reason ::missing-component
               :system-key key
               :system system}
              (str "Missing component " key " from system")]))
    component))

(defn get-dependency [system system-key component dependency-key]
  (let [dependency (get system system-key ::not-found)]
    (when (nil? dependency)
      (throw (nil-component system system-key)))
    (when (= ::not-found dependency)
      (throw [{:type ::MissingDependency
               :reason ::missing-dependency
               :system-key system-key
               :dependency-key dependency-key
               :component component
               :system system}
              (str "Missing dependency " dependency-key
                   " of " (type component)
                   " expected in system at " system-key)]))
    dependency))

(defn system-using
  "Associates dependency metadata with multiple components in the
  system. dependency-map is a map of keys in the system to maps or
  vectors specifying the dependencies of the component at that key in
  the system, as per 'using'."
  [system dependency-map]
  (reduce
   (fn [system [key dependencies]]
     (let [component (get-component system key)]
       (assoc system key (using component dependencies))))
   system
   dependency-map))

(defn dependency-graph
  "Returns a dependency graph, using the data structures defined in
  qbits.dependency, for the components found by
  (select-keys system component-keys)"
  [system component-keys]
  (reduce (fn [graph [key component]]
               (reduce #(dep/depend %1 key %2)
                       graph
                       (vals (dependencies component))))
             (dep/graph)
             (select-keys system component-keys)))

(defn assoc-dependency [system component dependency-key system-key]
  (let [dependency (get-dependency system system-key component dependency-key)]
    (assoc component dependency-key dependency)))

(defn assoc-dependencies [component system]
  (reduce #(assoc-dependency system %1 (key %2) (val %2))
             component
             (dependencies component)))

(defn try-action [component system key f args]
  (try (apply f component args)
       (catch t
           (throw [{:type ::ComponentFunctionThrewException
                    :reason ::component-function-threw-exception
                    :function f
                    :system-key key
                    :component component
                    :system system}
                   (str "Error in component " key
                        " in system " (type system)
                        " calling " f)]))))

(defn update-system
  "Invokes (apply f component args) on each of the components at
  component-keys in the system, in dependency order. Before invoking
  f, assoc's updated dependencies of the component."
  [system component-keys f & args]
  (let [graph (dependency-graph system component-keys)]
    (reduce (fn [system key]
              (assoc system key
                     (-> (get-component system key)
                         (assoc-dependencies system)
                         (try-action system key f args))))
            system
            (sort (dep/topo-comparator graph) component-keys))))

(defn update-system-reverse
  "Like update-system but operates in reverse dependency order."
  [system component-keys f & args]
  (let [graph (dependency-graph system component-keys)]
    (reduce (fn [system key]
              (assoc system key
                     (-> (get-component system key)
                         (assoc-dependencies system)
                         (try-action system key f args))))
            system
            (reverse (sort (dep/topo-comparator graph) component-keys)))))

(defn start-system
  "Recursively starts components in the system, in dependency order,
  assoc'ing in their dependencies along the way. component-keys is a
  collection of keys (order doesn't matter) in the system specifying
  the components to start, defaults to all keys in the system."
  ([system]
     (start-system system (keys system)))
  ([system component-keys]
     (update-system system component-keys (var start))))

(defn stop-system
  "Recursively stops components in the system, in reverse dependency
  order. component-keys is a collection of keys (order doesn't matter)
  in the system specifying the components to stop, defaults to all
  keys in the system."
  ([system]
     (stop-system system (keys system)))
  ([system component-keys]
     (update-system-reverse system component-keys (var stop))))

(defn system-map
  "Returns a system constructed of key/value pairs. The system has
  default implementations of the Lifecycle 'start' and 'stop' methods
  which recursively start/stop all components in the system.
  System maps print as #<SystemMap> to avoid overwhelming the printer
  with large objects. As a consequence, printed system maps cannot be
  'read'. To disable this behavior and print system maps like normal
  records, call
  (remove-method clojure.core/print-method qbits.component.SystemMap)"
  [& keyvals]
  (with-meta (apply hash-map keyvals)
    {::system? true}))

(defn ex-component?
  "True if the error has ex-data indicating it was thrown by something
  in the qbits.component namespace."
  [error]
  (some-> error ex-data :type namespace (= "qbits.component")))

(defn ex-without-components
  "If the error has ex-data provided by the qbits.component
  namespace, returns a new exception instance with the :component
  and :system removed from its ex-data. Preserves the other details of
  the original error. If the error was not created by this namespace,
  returns it unchanged. Use this when you want to catch and rethrow
  exceptions without including the full component or system."
  [error]
  (if (ex-component? error)
    (update-in error dissoc :component :system)
    error))

;; Copyright © 2015 Stuart Sierra

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
