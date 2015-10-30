(ns qbits.component.utils)
(defn vary-meta
  {:doc "Returns x with meta data updated with the application of f and args to it.
ex: (vary-meta x assoc :foo 42)"
   :signatures [[x f & args]]
   :added "0.1"}
  [x f & args]
  (with-meta x (apply f (meta x) args)))
