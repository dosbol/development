(ns discourje.core.spec.ast
  (:refer-clojure :exclude [sync send cat loop]))

;;;;
;;;; Registries
;;;;

(def role-names (atom (hash-map)))

(defn put-role-name!
  "Add a role to the rolenames, hashmap"
  [k name]
  {:pre [(keyword? k)
         (string? name)]}
  (swap! role-names (fn [m] (into m {k name}))))

(defn get-role-name
  "Get a name based on it's keyword"
  [k]
  {:pre [(keyword? k)]}
  (if-let [name (get @role-names k)]
    name
    (throw (Exception. (str "Unable to resolve key: " k " in registry of role names")))))

(def asts (atom (hash-map)))

(defn put-ast! [k vars body]
  "Add an ast to the AST hashmap. Mapped by keyword k, machted by the role."
  {:pre [(keyword? k)]}
  (swap! asts
         (fn [m]
           (if (contains? m k)
             (update m k #(into % {(count vars) {:vars vars, :body body}}))
             (into m {k {(count vars) {:vars vars, :body body}}})))))

(defn get-ast
  "Get an AST based on the keyword. (matched by role)"
  [k n]
  {:pre [(keyword? k)
         (number? n)]}
  (if-let [m (get @asts k)]
    (if-let [ast (get m n)]
      ast
      (throw (Exception. (str "Wrong number of args (" n ") passed to: " k))))
    (throw (Exception. (str "Unable to resolve key: " k " in registry of sessions")))))

;;;;
;;;; Predicates
;;;;

(defrecord Predicate [expr])

(defn predicate?
  "Check whether x is an instance of predicate"
  [x]
  (instance? Predicate x))

(defn predicate
  "Create a predicate of expression expr"
  [expr]
  {:pre []}
  (->Predicate expr))

;;;;
;;;; Roles
;;;;

(defrecord Role [name-expr index-exprs])

(defn role?
  "Check whether x is an instance of Role"
  [x]
  (instance? Role x))

(defn role
  "Create a role"
  ([expr]
   {:pre [(or (not (coll? expr)) (seq? expr))]}
   (cond
     (not (coll? expr)) (role expr [])
     (seq? expr) (role (first expr) (vec (rest expr)))))
  ([name-expr index-exprs]
   {:pre [(or (string? name-expr) (symbol? name-expr) (keyword? name-expr))
          (vector? index-exprs)]}
   (->Role name-expr index-exprs)))

;;;;
;;;; Actions
;;;;

(defrecord Action [type predicate sender receiver])

(defn action?
  "Check whether x is of type Action"
  [x]
  (= (type x) Action))

(defn action
  "Create an action."
  [type predicate sender receiver]
  {:pre [(contains? #{:sync :send :receive :close} type)
         (or (predicate? predicate) (symbol? predicate))
         (or (role? sender) (symbol? sender))
         (or (role? receiver) (symbol? receiver))]}
  (->Action type predicate sender receiver))

(defn sync
  "Create a sync action"
  [predicate sender receiver]
  (action :sync predicate sender receiver))

(defn send
  "Create a send action"
  [predicate sender receiver]
  (action :send predicate sender receiver))

(defn receive
  "Create a receive action"
  [sender receiver]
  (action :receive (predicate '(fn [_] true)) sender receiver))

(defn close
  "Create a close action"
  [sender receiver]
  (action :close (predicate '(fn [_] true)) sender receiver))

;;;;
;;;; Nullary operators
;;;;

(defrecord Nullary [type])

(defn end
  "Create end type"
  []
  (->Nullary :end))

;;;;
;;;; Multiary operators
;;;;

(defrecord Multiary [type branches])

(defn cat
  "Creat choises"
  [branches]
  (->Multiary :cat branches))

(defn alt
  "create alt"
  [branches]
  (->Multiary :alt branches))

(defn par
  "create parallel"
  [branches]
  (->Multiary :par branches))

(defrecord Every [type ast-f vars exprs branch])

(defn every
  ([ast-f bindings branch]
   (let [vars (take-nth 2 bindings)
         exprs (take-nth 2 (rest bindings))]
     (every ast-f vars exprs branch)))
  ([ast-f vars exprs branch]
   (->Every :every ast-f vars exprs branch)))

;;;;
;;;; "Special forms" operators
;;;;

(defrecord If [type test-expr then else])

(defn if
  "Create If monitor"
  [test-expr then else]
  (->If :if test-expr then else))

(defrecord Loop [type name vars exprs body])

(defn loop
  "Create loop monitor"
  ([name bindings body]
   (let [vars (take-nth 2 bindings)
         exprs (take-nth 2 (rest bindings))]
     (loop name vars exprs body)))
  ([name vars exprs body]
   (->Loop :loop name vars exprs body)))

(defrecord Recur [type name exprs])

(defn recur
  "Create recur, back to loop"
  [name exprs]
  (->Recur :recur name exprs))

;;;;
;;;; Misc operators
;;;;

(defn- parse-predicate
  "Evaluate predicate with string s"
  [s]
  (predicate (read-string s)))

(defn- parse-role
  "Parse a role"
  [s]
  (if (clojure.string/includes? s "[")
    (let [name-expr (subs s 0 (clojure.string/index-of s "["))
          index-exprs (mapv read-string
                            (clojure.string/split (subs s
                                                        (inc (clojure.string/index-of s "["))
                                                        (dec (count s)))
                                                  #"\]\["))]
      (role name-expr index-exprs))
    (role s)))

(defn- parse-action
  "Parse an action"
  ([s]
   (parse-action (case (first s)
                   \‽ :sync
                   \! :send
                   \? :receive
                   \C :close
                   (throw (Exception.)))
                 (subs s 2 (dec (count s)))))
  ([type s]
   (let [s (if (contains? #{:sync :send} type)
             s
             (str "(fn [_] true)," s))
         tokens (clojure.string/split s #"\,")
         predicate (parse-predicate (nth tokens 0))
         sender (parse-role (nth tokens 1))
         receiver (parse-role (nth tokens 2))]
     (action type predicate sender receiver))))

(defrecord Graph [type v edges])

(defn graph
  "Create a graph"
  [v0 edges]
  (->Graph :graph
           v0
           (clojure.core/loop [edges edges
                               source->action->targets {}]
             (if (empty? edges)
               source->action->targets
               (recur (rest edges)
                      (let [transition (first edges)
                            source (nth transition 0)
                            label (nth transition 1)
                            target (nth transition 2)
                            action (parse-action label)]
                        (if-let [action->targets (get source->action->targets source)]
                          (if-let [targets (get action->targets action)]
                            (assoc source->action->targets source (assoc action->targets action (conj targets target)))
                            (assoc source->action->targets source (assoc action->targets action [target])))
                          (assoc source->action->targets source {action [target]}))))))))

;;;;
;;;; Sessions
;;;;

(defrecord Session [type name exprs])

(defn session
  "Cretae a session with name and expressions"
  [name exprs]
  (->Session :session name exprs))