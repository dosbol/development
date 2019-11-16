(ns discourje.examples.predicates
  (:require [discourje.core.async :refer :all]
           [discourje.core.logging :refer :all]))


(defn default-message-check-fn
  "Predefined function for checking some message
  By predefining a function instead of an anonymous function in the protocol specification makes the protocol more easy to read!"
  [message]
  (string? message))

;  This function will generate a vector with 4 interactions to send and receive the greet message.
;  Notice how all communications have a different form of validating the message!
(def message-exchange-pattern
  (mep (-->> String "alice" "bob")                             ; Check by type
       (-->> "bob" "alice")                                    ; Wildcard, we do not care about the message, just the sender and receiver pair
       (-->> (fn [message] (string? message)) "alice" "carol") ; Single argument predicate boolean function for checking the message
       (-->> default-message-check-fn "carol" "alice")         ; By predefined function somewhere in the codebase
       (close "alice" "bob")
       (close "bob" "alice")
       (close "alice" "carol")
       (close "carol" "alice")))

;setup infrastructure, generate channels and add monitor
(def infrastructure (add-infrastructure message-exchange-pattern))
;Get the channels
(def alice-to-bob (get-channel infrastructure "alice" "bob"))
(def bob-to-alice (get-channel infrastructure "bob" "alice"))
(def alice-to-carol (get-channel infrastructure "alice" "carol"))
(def carol-to-alice (get-channel infrastructure "carol" "alice"))

(defn- greet-bob-and-carol
  "This function will use the protocol to send the greet message to bob and carol."
  []
  (>!! alice-to-bob "Greetings, from alice!")
  (log-message (<!! bob-to-alice))
  (>!! alice-to-carol "Greetings, from alice!")
  (log-message (<!! carol-to-alice))
  (close! alice-to-bob)
  (close! bob-to-alice)
  (close! alice-to-carol)
  (close! carol-to-alice))

(defn- receive-greet
  "This function will use the protocol to listen for the greet message."
  [input-channel output-channel]
  (let [message (<!! input-channel)]
    (log-message (format "Received message: %s to %s" message (get-consumer input-channel)))
    (>!! output-channel "Hi to you too!")))

;start the `greet-bob-and-carol' function on thread
(clojure.core.async/thread (greet-bob-and-carol))
;start the `receive-greet' function on thread and add the channels
(clojure.core.async/thread (receive-greet alice-to-bob bob-to-alice))
;start the `receive-greet' function on thread and add the channels
(clojure.core.async/thread (receive-greet alice-to-carol carol-to-alice))