(ns discourje.core.async.monitors
  (:require [clojure.core.async :as a]
            [discourje.core.spec.lts :as lts]))

(deftype Monitor [lts current-states wait-notify])

(defn monitor
  [lts]
  {:pre [(lts/lts? lts)]}
  (->Monitor lts
             (atom (lts/initial-states lts))
             (let [a (atom {})]
               (fn [k]
                 (if-let [ch (get @a k)]
                   ch
                   (do (swap! a (fn [m] (if (contains? m k) m (assoc m k (a/chan)))))
                       (recur k)))))))

(defn monitor?
  [x]
  (= (type x) Monitor))

;;;;
;;;; Exceptions
;;;;

(defn- str-command-message [[op sender receiver] message]
  (str (case op
         :handshake "‽"
         :send "!"
         :receive "?"
         :close "C"
         (throw (Exception.))) "("
       (if message (str message ","))
       sender ","
       receiver ")"))

(defn- exception
  ([lts current-states commands-and-messages]
   (ex-info (str "[SESSION FAILURE] Action(s) "
                 (mapv (fn [[command message]] (str-command-message command message)) commands-and-messages)
                 " are not eventually enabled in current state(s): "
                 current-states
                 ". LTS in Aldebaran format:\n\n"
                 lts
                 "\n\n")
            {:commands-and-messages commands-and-messages}))
  ([lts current-states command message]
   (ex-info (str "[SESSION FAILURE] Action "
                 (str-command-message command message)
                 " is not eventually enabled in current state(s): "
                 current-states
                 ". LTS in Aldebaran format:\n\n"
                 lts
                 "\n\n")
            {:command command
             :message message})))

(defonce exception-dummy (Object.))

;;;;
;;;; Verification
;;;;

(defn wait! [monitor command]
  (a/<!! ((.-wait_notify monitor) command)))

(defn notify! [monitor command ok]
  (a/>!! ((.-wait_notify monitor) command) ok))

(defn verify-now!
  [monitor [op _ _ :as command] message f]
  {:pre [(or (monitor? monitor) (nil? monitor))]}
  (if monitor

    (if (and (= op :handshake) (nil? message))
      (let [ok (wait! monitor command)]
        (if ok
          [(f) nil]
          [nil exception-dummy]))

      (loop []
        (let [source-states @(.-current_states monitor)
              target-states (lts/traverse-now! source-states command message)
              ok (not (empty? target-states))]
          (if-let [v-and-e (locking monitor
                             (when (= @(.-current_states monitor) source-states)
                               (if (and (= op :handshake) (not (nil? message)))
                                 (notify! monitor command ok))
                               (if ok
                                 (do (reset! (.-current_states monitor) target-states)
                                     (.notifyAll monitor)
                                     [(f) nil])
                                 [nil exception-dummy])))]
            v-and-e
            (recur)))))
    [(f) nil]))

(defn verify-eventually!
  [monitor [op _ _ :as command] message]
  {:pre [(or (monitor? monitor) (nil? monitor))]}
  (if monitor

    (if (and (= op :handshake) (nil? message))
      (let [ok (wait! monitor command)]
        (if ok
          [true nil]
          [nil (exception (.-lts monitor) nil command nil)]))

      (loop []
        (let [source-states @(.-current_states monitor)
              ok (lts/traverse-eventually! source-states command message)]

          (if-let [v-and-e (locking monitor
                             (when (= @(.-current_states monitor) source-states)
                               (if (and (= op :handshake) (not (nil? message)))
                                 (notify! monitor command ok))
                               (if ok
                                 (do (if (not (lts/traverse-now! source-states command message))
                                       (.wait monitor))
                                     [true nil])
                                 [false (exception (.-lts monitor) source-states command message)])))]
            v-and-e
            (recur)))))
    [true nil]))

(defn verify-eventually-some!
  [monitor commands-and-messages]
  {:pre [(or (monitor? monitor) (nil? monitor))]}
  (if monitor

    (loop []
      (let [source-states @(.-current_states monitor)
            _ (.println (System/err) (str "foo"))

            ok (loop [commands-and-messages commands-and-messages]
                 (.println (System/err) (str "zzz"))
                 (if (empty? commands-and-messages)
                   false
                   (let [[command message] (first commands-and-messages)]
                     (if (lts/traverse-eventually! source-states command message)
                       true
                       (recur (rest commands-and-messages))))))
            _ (.println (System/err) (str "bar"))]

        (if-let [v-and-e (locking monitor
                           (when (= @(.-current_states monitor) source-states)
                             (if ok
                               (do (if (not (loop [commands-and-messages commands-and-messages]
                                              (if (empty? commands-and-messages)
                                                false
                                                (let [[command message] (first commands-and-messages)]
                                                  (if (lts/traverse-now! source-states command message)
                                                    true
                                                    (recur (rest commands-and-messages)))))))
                                     (.wait monitor))
                                   [true nil])
                               [false (exception (.-lts monitor) source-states commands-and-messages)])))]
          v-and-e
          (recur))))
    [true nil]))