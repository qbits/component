(ns qbits.component.test.test-component
  (:require
   [qbits.component :as component :refer [start stop]]
   [qbits.dependency :refer [LLONG_MAX]]
   [pixie.test :as t]))

(def ^:dynamic *log* (atom []))

(defn log [& args]
  (swap! *log* conj args))

;; kind of, could rewrite as a transducer
(defn map-indexed
  [f xs]
  (loop [xs xs
         xs' []
         i 0]
    (if-let [x (first xs)]
      (recur (next xs)
             (conj xs' (f i x))
             (inc i))
      xs')))

(defn- ordering
  "Given an ordered collection of messages, returns a map from the
  head of each message to its index position in the collection."
  [log]
  (into {}
        (map-indexed (fn [i [message & _]] [message i]) log)))

(defn before?
  "In the collection of messages, does the message beginning with
  symbol a come before the message begging with symbol b?"
  [log sym-a sym-b]
  (let [order (ordering log)]
    (< (get order sym-a) (get order sym-b))))

(defn started? [component]
  (true? (:started? component)))

(defn stopped? [component]
  (false? (:started? component)))

(defrecord ComponentA [state started?]
  component/Lifecycle
  (start [this]
    (log 'ComponentA.start this)
    (assoc this :started? true))
  (stop [this]
    (log 'ComponentA.stop this)
    (assoc this :started? false)))

(defn component-a []
  (->ComponentA (rand-int LLONG_MAX) nil))

(defrecord ComponentB [state a started?]
  component/Lifecycle
  (start [this]
    (log 'ComponentB.start this)
    ;; (assert (started? a))
    (assoc this :started? true))
  (stop [this]
    (log 'ComponentB.stop this)
    ;; (assert (started? a))
    (assoc this :started? false)))

(defn component-b []
  (component/using
    (map->ComponentB {:state (rand-int LLONG_MAX)})
    [:a]))

(defrecord ComponentC [state a b started?]
  component/Lifecycle
  (start [this]
    (log 'ComponentC.start this)
    ;; (assert (started? a))
    ;; (assert (started? b))
    (assoc this :started? true))
  (stop [this]
    (log 'ComponentC.stop this)
    ;; (assert (started? a))
    ;; (assert (started? b))
    (assoc this :started? false)))

(defn component-c []
  (component/using
    (map->ComponentC {:state (rand-int LLONG_MAX)})
    [:a :b]))

(defrecord ComponentD [state my-c b started?]
  component/Lifecycle
  (start [this]
    (log 'ComponentD.start this)
    ;; (assert (started? b))
    ;; (assert (started? my-c))
    (assoc this :started? true))
  (stop [this]
    (log 'ComponentD.stop this)
    ;; (assert (started? b))
    ;; (assert (started? my-c))
    (assoc this :started? false)))

(defn component-d []
  (map->ComponentD {:state (rand-int LLONG_MAX)}))

(defrecord ComponentE [state started?]
  component/Lifecycle
  (start [this]
    (log 'ComponentE.start this)
    (assoc this :started? true))
  (stop [this]
    (log 'ComponentE.stop this)
    (assoc this :started? false)))

(defn component-e []
  (map->ComponentE {:state (rand-int LLONG_MAX)}))

(defrecord System1 [d a e c b]  ; deliberately scrambled order
  component/Lifecycle
  (start [this]
    (log 'System1.start this)
    (component/start-system this))
  (stop [this]
    (log 'System1.stop this)
    (component/stop-system this)))

(defn system-1 []
  (map->System1 {:a (component-a)
                 :b (component-b)
                 :c (component-c)
                 :d (component/using (component-d)
                      {:b :b
                       :my-c :c})
                 :e (component-e)}))

(defmacro with-log [& body]
  `(binding [*log* (atom [])]
     ~@body
     @*log*))

(t/deftest components-start-in-order
  (let [log (with-log (component/start (system-1)))]
    (t/assert (before? log 'ComponentA.start 'ComponentB.start))
    (t/assert (before? log 'ComponentA.start 'ComponentC.start))
    (t/assert (before? log 'ComponentB.start 'ComponentC.start))
    (t/assert (before? log 'ComponentC.start 'ComponentD.start))
    (t/assert (before? log 'ComponentB.start 'ComponentD.start))))

(t/deftest all-components-started
  (let [system (start (system-1))]
    (doseq [component (vals system)]
      (t/assert (started? component)))))

(t/deftest all-components-stopped
  (let [system (component/stop (component/start (system-1)))]
    (doseq [component (vals system)]
      (t/assert (stopped? component)))))

(t/deftest dependencies-satisfied
  (let [system (component/start (component/start (system-1)))]
    (started? (get-in system [:b :a]))
    (started? (get-in system [:c :a]))
    (started? (get-in system [:c :b]))
    (started? (get-in system [:d :my-c])) ))

;; (defrecord ErrorStartComponentC [state error a b]
;;   component/Lifecycle
;;   (start [this]
;;     (throw error))
;;   (stop [this]
;;     this))

;; (defn error-start-c [error]
;;   (component/using
;;     (map->ErrorStartComponentC {:error error})
;;     [:a :b]))

(defn setup-error
  ([]
     (setup-error ["Boom!" {}]))
  ([error]
     (try (component/start
           (assoc (system-1) :c (error-start-c error)))
          (catch x x))))

;; (t/deftest error-thrown-with-partial-system
;;   (let [ex (setup-error)]
;;     (t/assert (started? (-> ex ex-data :system :b :a)))))

;; (t/deftest error-thrown-with-component-dependencies
;;   (let [ex (setup-error)]
;;     (t/assert (started? (-> ex (get 2) :component :a)))
;;     (t/assert (started? (-> ex (get 2) :component :b)))))

;; (t/deftest error-thrown-with-cause
;;   (let [error (ex-info "Boom!" {})
;;         ex (setup-error error)]
;;     (t/assert (identical? error (.getCause ^Exception ex)))))

;; (t/deftest error-is-from-component
;;   (let [error (ex-info "Boom!" {})
;;         ex (setup-error error)]
;;     (t/assert (component/ex-component? ex))))

;; (t/deftest error-is-not-from-component
;;   (t/assert (not (component/ex-component? (ex-info "Boom!" {})))))

;; ;; (t/deftest remove-components-from-error
;; ;;   (let [error (ex-info (str (rand-int LLONG_MAX)) {})
;; ;;         ^Exception ex (setup-error error)
;; ;;         ^Exception ex-without (component/ex-without-components ex)]
;; ;;     (t/assert (contains? (ex-data ex) :component))
;; ;;     (t/assert (contains? (ex-data ex) :system))
;; ;;     (t/assert (not (contains? (ex-data ex-without) :component)))
;; ;;     (t/assert (not (contains? (ex-data ex-without) :system)))
;; ;;     (t/assert (= (.getMessage ex)
;; ;;            (.getMessage ex-without)))
;; ;;     (t/assert (= (.getCause ex)
;; ;;            (.getCause ex-without)))
;; ;;     (t/assert (java.util.Arrays/equals
;; ;;          (.getStackTrace ex)
;; ;;          (.getStackTrace ex-without)))))

(defrecord System2b [one]
  component/Lifecycle
  (start [this]
    ;; (assert (started? (get-in one [:b :a])))
    this)
  (stop [this]
    ;; (assert (started? (get-in one [:b :a])))
    this))

(defn system-2 []
  (component/system-map :alpha (system-1)
                        :beta (component/using (->System2b nil)
                                {:one :alpha})))

(t/deftest composed-systems
  (let [system (component/start (system-2))]
    (t/assert (get-in system [:beta :one :d :my-c]))))

(defn increment-all-components [system]
  (component/update-system
   system (keys system) update-in [:n] inc))

(defn assert-increments [system]
  (= 11 (get-in system [:a :n]))
  (= 11 (get-in system [:b :a :n]))
  (= 11 (get-in system [:c :a :n]))
  (= 11 (get-in system [:c :b :a :n]))
  (= 11 (get-in system [:e :d :b :a :n]))
  (= 21 (get-in system [:b :n]))
  (= 21 (get-in system [:c :b :n]))
  (= 21 (get-in system [:d :b :n]))
  (= 31 (get-in system [:c :n]))
  (= 41 (get-in system [:d :n]))
  (= 51 (get-in system [:e :n])))

(t/deftest update-with-custom-function-on-maps
  (let [system {:a {:n 10}
                :b (component/using {:n 20} [:a])
                :c (component/using {:n 30} [:a :b])
                :d (component/using {:n 40} [:a :b])
                :e (component/using {:n 50} [:b :c :d])}]
    (assert-increments (increment-all-components system))))

(t/deftest t-system-using
  (let [dependency-map {:b [:a]
                        :c [:a :b]
                        :d {:a :a :b :b}
                        :e [:b :c :d]}
        system {:a {:n 10}
                :b {:n 20}
                :c {:n 30}
                :d {:n 40}
                :e {:n 50}}
        system (component/system-using system dependency-map)]
    (assert-increments (increment-all-components system))))

(defrecord ComponentWithoutLifecycle [state])

(t/deftest component-without-lifecycle
  (let [c (->ComponentWithoutLifecycle nil)]
    (t/assert (= c (component/start c)))
    (t/assert (= c (component/stop c)))))

(defrecord ComponentReturningNil [state]
  component/Lifecycle
  (start [this]
    nil)
  (stop [this]
    nil))

;; (t/deftest component-returning-nil
;;   (let [a (->ComponentReturningNil nil)
;;         s (component/system-map :a a :b (component-b))
;;         e (try (component/start s)
;;                false
;;                (catch x x))]
;;     (t/assert (= ::component/nil-component (:reason (ex-data e))))))
