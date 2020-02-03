(ns discourje.core.async
  (:gen-class)
  (:require [discourje.core.logging :refer :all]
            [clojure.walk :refer :all]
            [clojure.core.async :as async]
            [clojure.core.async.impl.protocols :as bufs])
  (:import (clojure.lang Seqable, Atom)))

(defn callable? [f]
  "Check whether f is a function, returns false if not"
  (fn? f))

(defn uuid []
  (.toString (java.util.UUID/randomUUID)))

;load helper namespace files!
(load "protocols"
      "macros"
      "interactions"
      "channels"
      "nestedMonitorLinking"
      "monitoring"
      "buffers")

(defn close-infrastructure!
  "Close all channels of the Discourje infrastructure"
  [infra]
  (doseq [c (get-channels infra)] (clojure.core.async/close! (get-chan c))))

(defn make-interaction [predicate sender receiver]
  "Creates an interaction object specifying sending action from sender to receiver."
  (->interaction (uuid) predicate sender receiver #{} nil))

(defn make-choice
  "Create a choice interaction"
  [branches]
  (->branch (uuid) branches nil))

(defn make-recursion
  "Generate recursion"
  [name recursion]
  (->recursion (uuid) name recursion nil))

(defn do-recur
  "do recur to start of recursion"
  [name]
  (->recur-identifier (uuid) name :recur nil))

(defn make-closer
  "Create a closer to close the channel with given sender and receiver pair."
  [sender receiver]
  (->closer (uuid) sender receiver nil))

(defn make-parallel
  "Generate parallel construct"
  [parallels]
  (->lateral (uuid) parallels nil))

(defn create-protocol
  "Generate protocol based on interactions"
  [interactions]
  (->protocol interactions))

(defn generate-monitor
  "Generate the monitor based on the given protocol"
  [protocol]
  (let [rec-table (atom {})
        linked-interactions (nest-mep (get-interactions protocol) rec-table)]
    (->monitor (uuid) (atom linked-interactions) rec-table)))

(defn- all-channels-implement-transportable?
  "Do all custom supplied channels implement the transportable interface?"
  [channels]
  (every? #(satisfies? transportable %) channels))

(defn channel-closed?
  "check whether a channel is closed"
  ([channel]
   (if (nil? channel)
     (do (log-error :invalid-channel "Cannot check if the given channel is closed, it is nil!")
         false)
     (if (clojure.core.async.impl.protocols/closed? (get-chan channel))
       true
       false)))
  ([sender receiver infra]
   (if (not (nil? infra))
     (channel-closed? (get-channel infra sender receiver))
     (log-error :invalid-channel (format "You are trying to close a channel from %s to %s but there is no infrastructure!" sender receiver)))))

(defn generate-infrastructure
  "Generate channels with monitor based on the protocol, also supports arity to give manually created custom channels. With for example: specific buffer requirements."
  ([protocol]
   (let [monitor (generate-monitor protocol)
         roles (get-distinct-role-pairs (get-interactions protocol))
         channels (generate-minimum-channels roles monitor 1)]
     (->infrastructure channels)))
  ([protocol channels]
   (if (all-channels-implement-transportable? channels)
     (let [roles (get-distinct-role-pairs (get-interactions protocol))
           control-channels (generate-minimum-channels roles monitor 1)
           all-channels-given? (nil? (some #(false? %) (for [channel control-channels] (not (empty? (filter (fn [c] (and (= (:provider c) (:provider channel)) (= (:consumers c) (:consumers channel)))) channels))))))]
       (if all-channels-given?
         (let [monitor (generate-monitor protocol)]
           (->infrastructure (vec (for [c channels] (assoc c :monitor monitor)))))
         (log-error :invalid-channels "Cannot generate infrastructure, make sure all channels required for the protocol are given!")))
     (log-error :invalid-channels "Cannot generate infrastructure, make sure all supplied channels implement the `transportable' protocol!"))))

(defmacro go
  "Discourje.core.async go macro"
  [& body]
  `(async/go ~@body))

(defmacro go-loop
  "Like (go (loop ...))"
  [bindings & body]
  `(async/go-loop ~bindings ~@body))

(defn- allow-send!!
  "Allow send message in channel"
  [channel message]
  (async/>!! (get-chan channel) message)
  (release-take channel)
  channel)

(defn- allow-put!
  "Allow send message in channel"
  [channel message callback on-caller?]
  (async/put! (get-chan channel) message (fn [x] (do (release-take channel) (callback x))) on-caller?)
  channel)

(defn- allow-puts!
  "Allow sending message on multiple channels"
  [channels message callback on-caller?]
  (let [counter (atom 0)
        synchronize-fn (fn [x] (do (swap! counter inc)
                                   (when (== @counter (count channels))
                                     (callback true))))]
    (doseq [c channels] (do (acquire-put c) (async/put! (get-chan c) message (fn [x] (do (release-take c) (synchronize-fn x))) on-caller?)))
    channels))

(defn- allow-take!
  "Allow a take! on the channel"
  [channel callback on-caller?]
  (async/take! (get-chan channel) (fn [channel-val] (do (release-put channel) (callback channel-val))) on-caller?)
  channel)

(defn- allow-receive!!
  "Allow a receive on the channel"
  [channel]
  (async/<!! (get-chan channel))
  (release-put channel)
  channel)

(defn- allow-sends!!
  "Allow sending message on multiple channels"
  [channels message]
  (doseq [c channels] (do (acquire-put c) (allow-send!! c message)))
  channels)

(defn all-valid-channels?
  "Do all channels comply with the monitor"
  [channels message]
  (when (not (empty? channels))
    (let [targets (for [c channels] (valid-send? (get-monitor c) (get-provider c) (get-consumer c) message))]
      (when (every? some? targets)
        (first targets)))))

(defn all-channels-open?
  "Are all channels open?"
  [channels]
  (and (not (empty? channels)) (every? #(not (channel-closed? %)) channels)))

(defn can-puts?
  "check if the buffer in full on all channels, when full wait until there is space in the buffer"
  [channel]
  (some #(buffer-full? (get-chan %)) channel))

(defn- all-channels-valid-for-send?
  "Are all channels valid for sending?"
  [channels message]
  (cond
    (false? (equal-senders? channels))
    (log-error :invalid-parallel-channels "Trying to send in multicast, but the sender of the channels is not the same!")
    (false? (equal-monitors? channels))
    (log-error :monitor-mismatch "Trying to send in multicast, but the channels do not share the same monitor!")
    (false? (all-channels-open? channels))
    (log-error :incorrect-communication "Trying to send in multicast, one or more of the channels is closed!")
    :else
    (all-valid-channels? channels message)))

(defn validate-send [channel message]
  (let [valid-interaction
        (cond
          (channel-closed? channel)
          (log-error :incorrect-communication (format "Invalid communication: you are trying to send but the channel is closed! From %s to %s" (get-provider channel) (get-consumer channel)))
          :else
          (valid-send? (get-monitor channel) (get-provider channel) (get-consumer channel) message))]
    (if (is-valid-for-swap? valid-interaction)
      (apply-send! (get-monitor channel) (get-valid valid-interaction) (get-pre-swap valid-interaction) (get-provider channel) (get-consumer channel) message)
      (log-error :incorrect-communication (format "Atomic-send communication invalid! message: %s, sender: %s, receiver: %s, while active interaction is: %s" message (get-provider channel) (get-consumer channel) (interaction-to-string (get-active-interaction (get-monitor channel))))))))

(defn validate-multicast [channels message]
  (let [first-chan (first channels)
        valid-interaction (all-channels-valid-for-send? channels message)]
    (if (is-valid-for-swap? valid-interaction)
      (apply-send! (get-monitor first-chan) (get-valid valid-interaction) (get-pre-swap valid-interaction) (get-provider first-chan) (vec (for [c channels] (get-consumer c))) message)
      (log-error :incorrect-communication "Trying to send in multicast, but the monitor is not correct for all channels!" (to-string first-chan) message))))

(defn- >E!! [channels message]
  "Send in multicast, blocking"
  (do (loop []
        (when (can-puts? channels) (recur)))
      (loop [send-result (validate-multicast channels message)]
        (if send-result
          (allow-sends!! channels message)
          (recur (validate-multicast channels message))))))

(defn- put-multicast! [channels message callback on-caller?]
  "Send in multicast, put!"
  (do (loop []
        (when (can-puts? channels) (recur)))
      (loop [send-result (validate-multicast channels message)]
        (if send-result
          (allow-puts! channels message callback on-caller?)
          (recur (validate-multicast channels message))))))

(defmacro >E! [channels message]
  "Send in multicast, in go-block"
  `(do (loop []
         (when (can-puts? ~channels) (recur)))
       (loop [~'send-result (validate-multicast ~channels ~message)]
         (if ~'send-result
           (do (doseq [~'c ~channels]
                 (do (acquire-put ~'c)
                     (async/>! (get-chan ~'c) ~message)
                     (release-take ~'c)))
               ~channels)
           (recur (validate-multicast ~channels ~message))))))

(defn >!!
  "Put on channel blocking"
  [channel message]
  (if (vector? channel)
    (>E!! channel message)
    (do (acquire-put channel)
        (loop [send-result (validate-send channel message)]
          (if send-result
            (allow-send!! channel message)
            (recur (validate-send channel message)))))))

(defmacro >!
  "Put on channel, in go-block"
  [channel message]
  `(if (vector? ~channel)
     (>E! ~channel ~message)
     (do (acquire-put ~channel)
         (loop [~'send-result (validate-send ~channel ~message)]
           (if ~'send-result
             (do (async/>! (get-chan ~channel) ~message)
                 (release-take ~channel)
                 ~channel)
             (recur (validate-send ~channel ~message)))))))

(defn put!
  "Put on channel, with callback"
  ([channel message]
   (put! channel message nil))
  ([channel message callback] (put! channel message callback true))
  ([channel message callback on-caller?]
   (if (vector? channel)
     (put-multicast! channel message callback on-caller?)
     (do (acquire-put channel)
         (loop [send-result (validate-send channel message)]
           (if send-result
             (allow-put! channel message callback on-caller?)
             (recur (validate-send channel message))))))
   ))

(defn take!
  "Take from channel, with callback"
  ([channel callback] (take! channel callback true))
  ([channel callback on-caller?]
   (do (acquire-take channel)
       (if (nil? (get-active-interaction (get-monitor channel)))
         (log-error :invalid-monitor "Please activate a monitor, your protocol has not yet started, or it is already finished!")
         (let [result (peek-channel (get-chan channel))
               valid-interaction (valid-receive? (get-monitor channel) (get-provider channel) (get-consumer channel) result)]
           (if-not (is-valid-for-swap? valid-interaction)
             (log-error :incorrect-communication (format "Atomic-receive communication invalid! sender: %s, receiver: %s with message %s , while active interaction is: %s" (get-provider channel) (get-consumer channel) result (to-string (get-active-interaction (get-monitor channel)))))
             (do (apply-receive! (get-monitor channel) (get-valid valid-interaction) (get-pre-swap valid-interaction) (get-provider channel) (get-consumer channel) result)
                 (allow-take! channel callback on-caller?))))))
   ))

(defn <!!
  "take form channel blocking"
  [channel]
  (do (acquire-take channel)
      (if (nil? (get-active-interaction (get-monitor channel)))
        (log-error :invalid-monitor "Please activate a monitor, your protocol has not yet started, or it is already finished!")
        (let [result (peek-channel (get-chan channel))
              valid-interaction (valid-receive? (get-monitor channel) (get-provider channel) (get-consumer channel) result)]
          (if-not (is-valid-for-swap? valid-interaction)
            (log-error :incorrect-communication (format "Atomic-receive communication invalid! sender: %s, receiver: %s with message %s , while active interaction is: %s" (get-provider channel) (get-consumer channel) result (to-string (get-active-interaction (get-monitor channel)))))
            (do (apply-receive! (get-monitor channel) (get-valid valid-interaction) (get-pre-swap valid-interaction) (get-provider channel) (get-consumer channel) result)
                (allow-receive!! channel)
                result))))))

(defmacro <!
  "take form channel, in go-block"
  [channel]
  `(do (acquire-take ~channel)
       (if (nil? (get-active-interaction (get-monitor ~channel)))
         (log-error :invalid-monitor "Please activate a monitor, your protocol has not yet started, or it is already finished!")
         (let [~'result (peek-channel (get-chan ~channel))
               ~'valid-interaction (valid-receive? (get-monitor ~channel) (get-provider ~channel) (get-consumer ~channel) ~'result)]
           (if-not (is-valid-for-swap? ~'valid-interaction)
             (log-error :incorrect-communication (format "Atomic-receive communication invalid! sender: %s, receiver: %s with message %s , while active interaction is: %s" (get-provider ~channel) (get-consumer ~channel) ~'result (to-string (get-active-interaction (get-monitor ~channel)))))
             (do (apply-receive! (get-monitor ~channel) (get-valid ~'valid-interaction) (get-pre-swap ~'valid-interaction) (get-provider ~channel) (get-consumer ~channel) ~'result)
                 (async/<! (get-chan ~channel))
                 (release-put ~channel)
                 ~'result))))))
(defn <!!!
  "take from channel (blocking) peeking, and delay receive when parallel
  This synchronizes all receives in a multi-cast"
  [channel]
  (do (acquire-take channel)
      (if (nil? (get-active-interaction (get-monitor channel)))
        (log-error :invalid-monitor "Please activate a monitor, your protocol has not yet started, or it is already finished!")
        (let [result (peek-channel (get-chan channel))
              is-multicast (is-current-multicast? (get-monitor channel) result)
              id (get-id (get-active-interaction (get-monitor channel)))
              valid-interaction (valid-receive? (get-monitor channel) (get-provider channel) (get-consumer channel) result)]
          (if-not (is-valid-for-swap? valid-interaction)
            (log-error :incorrect-communication (format "Atomic-receive communication invalid! sender: %s, receiver: %s, while active interaction is: %s" (get-provider channel) (get-consumer channel) (to-string (get-active-interaction (get-monitor channel)))))
            (do (apply-receive! (get-monitor channel) (get-valid valid-interaction) (get-pre-swap valid-interaction) (get-provider channel) (get-consumer channel) result)
                (allow-receive!! channel)
                (loop [par is-multicast
                       active-inter (get-active-interaction (get-monitor channel))]
                  (when (true? par) (recur (and (not= nil active-inter) (= id (get-id active-inter))) (get-active-interaction (get-monitor channel)))))
                result))))))

(defn close-channel!
  "Close a channel with the given sender and receiver"
  ([channel]
   (cond
     (nil? channel)
     (log-error :invalid-channel "Cannot close a channel, it's nil!")
     (channel-closed? channel)
     (log-error :invalid-channel (format "Cannot close the channel with pair %s %s since it is already closed!" (get-provider channel) (get-consumer channel)))
     (nil? (get-monitor channel))
     (log-error :invalid-monitor (format "Cannot close the channel with pair %s %s since it has no monitor!" (get-provider channel) (get-consumer channel)))
     (nil? (get-chan channel))
     (log-error :invalid-channel (format "Cannot close the channel with pair %s %s since the internal core.async channel is nil!" (get-provider channel) (get-consumer channel)))
     :else
     (let [valid-interaction (valid-close? (get-monitor channel) (get-provider channel) (get-consumer channel))]
       (if (is-valid-for-swap? valid-interaction)
         (apply-close! (get-monitor channel) (get-valid valid-interaction) (get-pre-swap valid-interaction) channel)
         (log-error :invalid-channel (format "Cannot close the channel with pair %s %s since another interaction is active!: %s" (get-provider channel) (get-consumer channel) (interaction-to-string (get-active-interaction (get-monitor channel)))))))))
  ([sender receiver infra]
   (if (not (nil? infra))
     (close-channel! (get-channel infra sender receiver))
     (log-error :invalid-channel (format "You are trying to close a channel from %s to %s but there is no infrastructure!" sender receiver)))))

(defn chan
  "create a custom channel"
  ([sender receiver buffer]
   (if (nil? buffer)
     (new-channel sender receiver (clojure.core.async/chan) nil nil)
     (new-channel sender receiver (clojure.core.async/chan buffer) buffer nil)))
  ([n sender receiver monitor]
   (new-channel (if (fn? sender) (sender) sender)
                (if (fn? receiver) (receiver) receiver)
                (clojure.core.async/chan n)
                n
                monitor)))

;; Load at the end, because it depends on definitions in this file
(load "dsl")