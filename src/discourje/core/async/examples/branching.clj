(ns discourje.core.async.examples.branching
  (require [discourje.core.async.async :refer :all]
           [discourje.core.async.logging :refer :all]))

(defn- defineBranchProtocol
  "This function will generate a vector with 3 monitors to send and receive the number message.
  The protocol offers a choice (with internal monitors) to send messages called greaterThan or lessThan to alice depending on the data received"
  []
  (create-protocol [(-->> "number" "alice" "bob")
                    (make-choice [[(-->> "greaterThan" "bob" "alice")]
                                  [(-->> "lessThan" "bob" "alice")]])]))

;setup infrastructure, generate channels and add monitor
(def infrastructure (generate-infrastructure (defineBranchProtocol)))
;Get the channels
(def alice-to-bob (get-channel "alice" "bob" infrastructure))
(def bob-to-alice (get-channel "bob" "alice" infrastructure))

(defn- sendNumberAndAwaitResult
  "This function will use the protocol to send the number message to bob and wait for the result to know if it is greaterThan or lessThan threshold."
  [threshold]
  ;We send a map (data structure) in order to send both the threshold and the generated number
  (>!!! alice-to-bob (->message "number" {:threshold threshold :generatedNumber (rand-int (+ threshold 10))}))
  (let [response (<!!! bob-to-alice ["greaterThan" "lessThan"])]
    (cond
      (= (get-label response) "greaterThan") (log-message (format "greaterThan received with message: %s" (get-content response)))
      (= (get-label response) "lessThan") (log-message (format "lessThan received with message: %s" (get-content response))))))

(defn- receiveNumber
  "This function will use the protocol to listen for the number message. Check the number and threshold and send result"
  []
  (let [numberMap (<!!! alice-to-bob "number")
        threshold (:threshold (get-content numberMap))
        generated (:generatedNumber (get-content numberMap))]
    (if (> generated threshold)
      (>!!! bob-to-alice (->message "greaterThan" "Number send is greater!"))
      (>!!! bob-to-alice (->message "lessThan" "Number send is smaller!")))))

;start the `GreetBobAndCarol' function on thread and supply some threshold
(clojure.core.async/thread (sendNumberAndAwaitResult 10))
;start the `receiveGreet' function on thread
(clojure.core.async/thread (receiveNumber))
