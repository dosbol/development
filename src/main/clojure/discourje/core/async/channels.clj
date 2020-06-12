(ns discourje.core.async.channels
  (:require [clojure.core.async :as a]
            [discourje.core.async.buffers :as buffers]
            [discourje.core.async.monitors :as monitors]
            [discourje.core.spec.ast :as ast]
            [discourje.core.spec.interp :as interp]))

;;;;
;;;; Interfaces
;;;;

(definterface MutableSender
  (getSender [])
  (setSender [newSender]))

(definterface MutableReceiver
  (getReceiver [])
  (setReceiver [newReceiver]))

(definterface MutableMonitor
  (getMonitor [])
  (setMonitor [newMonitor]))

;;;;
;;;; Channels
;;;;

(deftype Channel
  [op-put
   op-take
   ch-top
   ch-mid
   ch-bot
   ^:volatile-mutable sender
   ^:volatile-mutable receiver
   ^:volatile-mutable monitor]

  Object
  (toString [_] (str "[" (if sender sender "-") " " (if receiver receiver "-") "]"))

  MutableSender
  (getSender [_] sender)
  (setSender [_ newSender] (set! sender newSender))

  MutableReceiver
  (getReceiver [_] receiver)
  (setReceiver [_ newReceiver] (set! receiver newReceiver))

  MutableMonitor
  (getMonitor [_] monitor)
  (setMonitor [_ newMonitor] (set! monitor newMonitor)))

(defn channel? [x]
  (= (type x) Channel))

(defn- channel [op-put op-take ch-top ch-mid ch-bot]
  {:pre []}
  (->Channel op-put op-take ch-top ch-mid ch-bot nil nil nil))

(defn unbuffered-channel []
  {:pre []}
  (let [ch (a/chan)]
    (channel :handshake :handshake ch ch ch)))

(defn buffered-channel [buffer]
  {:pre [(buffers/buffer? buffer)]}
  (let [chan (fn [buffer]
               (case (buffers/type buffer)
                 :fixed-buffer (a/chan (buffers/n buffer))
                 :dropping-buffer (a/chan (a/dropping-buffer (buffers/n buffer)))
                 :sliding-buffer (a/chan (a/sliding-buffer (buffers/n buffer)))
                 :promise-buffer (throw (IllegalArgumentException.))))]
    (channel :send :receive (chan buffer) (chan buffer) (chan buffer))))

(defn link [this r1 r2 m]
  {:pre [(channel? this)
         (or (ast/role? r1) (fn? r1))
         (or (ast/role? r2) (fn? r2))
         (monitors/monitor? m)]}
  (.setSender this (interp/eval-role r1))
  (.setReceiver this (interp/eval-role r2))
  (.setMonitor this m)
  this)

(defonce ^:private token true)

;;;;
;;;; close!
;;;;

(defn close!
  [channel]
  {:pre [(channel? channel)]}
  (let [sender (.getSender channel)
        receiver (.getReceiver channel)
        command [:close sender receiver]
        monitor (.getMonitor channel)]

    (loop []
      (let [[v e] (monitors/verify-now! monitor command nil #(a/close! (.-ch_mid channel)))]
        (if e
          (let [[v e] (monitors/verify-eventually! monitor command nil)]
            (if v
              (recur)
              (throw e)))
          (do (a/close! (.-ch_top channel))
              (a/close! (.-ch_bot channel))
              v))))))

;;;;
;;;; >!! and <!!
;;;;

(defn- >!!-step1
  [channel message]
  {:pre [(channel? channel)]}
  ;(.println (System/err) (str "[SESSION INFO] >!!-step1: " channel " " message))

  [(a/>!! (.ch_top channel) token) nil])

(defn- >!!-step2
  [channel message]
  {:pre [(channel? channel)]}
  ;(.println (System/err) (str "[SESSION INFO] >!!-step2: " channel " " message))

  (let [op (.op_put channel)
        sender (.getSender channel)
        receiver (.getReceiver channel)
        monitor (.getMonitor channel)]

    (let [[v e] (monitors/verify-now! monitor [op sender receiver] message #(a/>!! (.-ch_mid channel) message))
          channel-still-open (if e
                               (a/<!! (.-ch_top channel))
                               (a/>!! (.-ch_bot channel) token))]

      [v (if channel-still-open e)])))

(defn- >!!-step3
  [channel message]
  {:pre [(channel? channel)]}
  ;(.println (System/err) "[SESSION WARNING] Entering >!!-step3...")

  (let [op (.op_put channel)
        sender (.getSender channel)
        receiver (.getReceiver channel)
        monitor (.getMonitor channel)]

    (let [v-and-e (monitors/verify-eventually! monitor [op sender receiver] message)]
      v-and-e)))

(defn >!!
  [channel message]
  {:pre [(channel? channel)]}
  (loop []
    (let [[v _] (>!!-step1 channel message)]
      (if v
        (let [[v e] (>!!-step2 channel message)]
          (if e
            (let [[v e] (>!!-step3 channel message)]
              (if v
                (recur)
                (throw e)))
            v))
        nil))))

(defn <!!-step1
  [channel]
  {:pre [(channel? channel)]}
  ;(.println (System/err) (str "[SESSION INFO] <!!-step1: " channel))

  [(a/<!! (.ch_bot channel)) nil])

(defn- <!!-step2
  [channel]
  {:pre [(channel? channel)]}
  ;(.println (System/err) (str "[SESSION INFO] <!!-step2: " channel))

  (let [op (.op_take channel)
        sender (.getSender channel)
        receiver (.getReceiver channel)
        monitor (.getMonitor channel)]

    (let [[v e] (monitors/verify-now! monitor [op sender receiver] nil #(a/<!! (.ch_mid channel)))
          channel-still-open (if e
                               (a/>!! (.ch_bot channel) token)
                               (a/<!! (.ch_top channel)))]

      [v (if channel-still-open e)])))

(defn- <!!-step3
  [channel]
  {:pre [(channel? channel)]}
  ;(.println (System/err) "[SESSION WARNING] Entering <!!-step3...")

  (let [op (.op_take channel)
        sender (.getSender channel)
        receiver (.getReceiver channel)
        monitor (.getMonitor channel)]

    (let [v-and-e (monitors/verify-eventually! monitor [op sender receiver] nil)]
      v-and-e)))

(defn <!!
  [channel]
  {:pre [(channel? channel)]}
  (loop []
    (let [[v _] (<!!-step1 channel)]
      (if v
        (let [[v e] (<!!-step2 channel)]
          (if e
            (let [[v e] (<!!-step3 channel)]
              (if v
                (recur)
                (throw e)))
            v))
        nil))))

;;;;
;;;; >! and <!
;;;;

;; TODO

;;;;
;;;; alts! and alts!!
;;;;

;; TODO: alts!

(defn alts!!-step1
  [alternatives opts]
  {:pre [(every? #(or (and (vector? %) (= 2 (count %)) (channel? (first %)))
                      (channel? %))
                 alternatives)]}

  (let [ports (mapv #(if (vector? %)
                       [(.-ch_top (first %)) token]
                       (.-ch_bot %))
                    alternatives)

        [val port] (if opts
                     (if (contains? opts :default)
                       (if (contains? opts :priority)
                         (a/alts!! ports :default (:default opts) :priority (:priority opts))
                         (a/alts!! ports :default (:default opts)))
                       (if (contains? opts :priority)
                         (a/alts!! ports :priority (:priority opts))
                         (a/alts!! ports)))
                     (a/alts!! ports))

        alternative (loop [alternatives alternatives]
                      (if (empty? alternatives)
                        [:default val]
                        (let [alternative (first alternatives)
                              ch (if (vector? alternative)
                                   (.-ch_top (first alternative))
                                   (.-ch_bot alternative))]
                          (if (= port ch)
                            alternative
                            (recur (rest alternatives))))))]

    [[alternative [val (if (vector? alternative) (first alternative) alternative)]] nil]))

(defn alts!!-step2
  [alternative]
  (if (and (vector? alternative) (= :default (first alternative)))
    [[(second alternative) :default] nil]
    (if (vector? alternative)
      (let [[channel message] alternative
            [v e] (>!!-step2 channel message)]
        [[v channel] e])
      (let [channel alternative
            [v e] (<!!-step2 channel)]
        [[v channel] e]))))

(defn alts!!-step3
  [alternatives]
  ;(.println (System/err) "[SESSION WARNING] Entering alts!!-step3...")

  (let [monitors (map (fn [alternative]
                        (let [[channel _] (if (vector? alternative) alternative [alternative nil])
                              monitor (.getMonitor channel)]
                          monitor))
                      alternatives)
        commands-and-messages (mapv (fn [alternative]
                                      (let [[channel message] (if (vector? alternative) alternative [alternative nil])
                                            op (if message (.op_put channel) (.op_take channel))
                                            sender (.getSender channel)
                                            receiver (.getReceiver channel)]
                                        [[op sender receiver] message]))
                                    alternatives)]

    (let [v-and-e (monitors/verify-eventually-some! (first monitors) commands-and-messages)]
      v-and-e)))

(defn alts!!
  [alternatives opts]
  {:pre [(every? #(or (and (vector? %) (= 2 (count %)) (channel? (first %)))
                      (channel? %))
                 alternatives)]}
  (loop []
    (let [[v _] (alts!!-step1 alternatives opts)]
      (if (first (second v))
        (let [[v e] (alts!!-step2 (first v))]
          (if e
            (let [[v e] (alts!!-step3 alternatives)]
              (if v
                (recur)
                (throw e)))
            v))
        (second v)))))