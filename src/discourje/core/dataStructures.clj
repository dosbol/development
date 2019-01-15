(ns discourje.core.dataStructures)

;protocol to generate string of the current object (for exception/logging!)
(defprotocol stringify
  (to-string [this]))
;monitor for a single send action
(defrecord sendM [action from to]
  stringify
  (to-string [this] (format "monitor-send -> Action: %s From %s to %s" action from to)))
;monitor for a single recv action
(defrecord receiveM [action to from]
  stringify
  (to-string [this] (format "monitor-receive <- Action: %s From %s to %s" action from to)))
;We also need a data structure to create a conditional with branches.
;When the protocol encounters this it will check the conditional and continue on the correct branch.
(defrecord choice [trueBranch falseBranch]
  stringify
  (to-string [this] (format "monitor-choice |-| TrueBranch: %s | FalseBranch %s" trueBranch falseBranch)))
;recursion construct
(defrecord recursion [name protocol]
  stringify
  (to-string [this] (format "monitor-recursion O Name: %s" name)))
;recur or end the recursion block
(defrecord recur! [name status]
  stringify
  (to-string [this] (format "Do-(end)-recur - Name: %s Status: %s" name status)))
;protocol(interface) to implement in object
(defprotocol role
  (send-to [this action value to])
  (receive-by [this action from callback]))