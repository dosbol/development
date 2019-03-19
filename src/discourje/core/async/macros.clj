;macros.clj
(in-ns 'discourje.core.async.async)

(defmacro -->>
  "Create an Atomic-interaction"
  [action sender receiver]
  `(->interaction (uuid/v1) ~action ~sender ~receiver nil))

(defmacro rec
  "Generate recursion"
  [name interaction & more]
  `(->recursion (uuid/v1) ~name [~interaction ~@more] nil))

(defmacro continue
  "Continue recursion, matched by name"
  [name]
  `(->recur-identifier (uuid/v1) ~name :recur nil))

(defmacro choice
  "Generate choice"
  [branch & more]
  `(->branch (uuid/v1) [~branch ~@more] nil))

(defmacro mep
  "Generate message exchange pattern aka protocol"
  [interactions & more]
  `(->protocol [~interactions ~@more]))

(defmacro add-infrastructure
  "adds infrastructure to the mep (channels)"
  ([message-exchange-pattern]
  `(generate-infrastructure ~message-exchange-pattern))
  ([message-exchange-pattern custom-channels]
   `(generate-infrastructure ~message-exchange-pattern ~custom-channels)))

(defmacro create-channel
  "create a custom channel"
  [sender receiver buffer]
  `(if (nil? ~buffer)
    (->channel ~sender ~receiver (clojure.core.async/chan) nil nil)
    (->channel ~sender ~receiver (clojure.core.async/chan ~buffer) ~buffer nil)))

(defmacro msg
  "Generate a message"
  [label content]
  `(->message ~label ~content))