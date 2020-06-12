(ns discourje.core.spec
  (:gen-class)
  (:refer-clojure :exclude [if do let loop cat * + count empty? disj])
  (:require [clojure.walk :as w]
            [discourje.core.spec.ast :as ast]))

(require '[discourje.core.spec :as s])

(defn- smap [env]
  `(zipmap '~(keys env) [~@(keys env)]))

;;;;
;;;; Desugaring
;;;;

(defn- desugared-predicate [form]
  (cond
    ;; form ::= (predicate expr)
    (and (seq? form) (= 'discourje.core.spec.ast/predicate (first (macroexpand form))))
    form

    ;; form ::= x
    (and (symbol? form) (not (resolve form)))
    `'~form

    ;; form ::= expr
    :else `(predicate ~form)))

(defn- desugared-role [form]
  (cond
    ;; form ::= (role name-expr index-exprs)
    (and (seq? form) (= 'discourje.core.spec.ast/role (first (macroexpand form))))
    form

    ;; form ::= (name-expr index-expr1 index-expr2 ...)
    (seq? form)
    `(role ~(first form) ~(vec (rest form)))

    ;; form ::= x
    (symbol? form)
    `'~form

    ;; form ::= expr
    :else `(role ~form)))

(defn- desugared-spec [form]
  (cond
    ;; form ::= (:session x y z)
    (and (seq? form) (keyword? (first form)))
    `(session ~(first form) ~(vec (rest form)))

    ;; form ::= [x y z]
    (and (vector? form))
    `(cat ~@form)

    ;; form ::= ...
    :else form))

;;;;
;;;; Predicates
;;;;

(defmacro predicate
  [expr]
  `(ast/predicate '~expr))

;;;;
;;;; Roles
;;;;

(defmacro role
  ([name-expr]
   `(role ~name-expr []))
  ([name-expr index-exprs]
   `(ast/role (w/postwalk-replace ~(smap &env) '~name-expr)
              (w/postwalk-replace ~(smap &env) '~index-exprs))))

(defn defrole
  ([k]
   (defrole k (name k)))
  ([k name]
   (ast/put-role-name! k name)
   (when-let [ns (namespace k)]
     (ns-unmap (symbol ns) (symbol (clojure.core/name k)))
     (intern (symbol ns)
             (symbol (clojure.core/name k))
             (fn [& indices]
               (clojure.core/let [indices (vec indices)]
                 (role k indices)))))))

;;;;
;;;; Actions
;;;;

(defmacro -->
  ([sender receiver]
   `(--> ~'Object ~sender ~receiver))
  ([predicate sender receiver]
   (clojure.core/let [predicate (desugared-predicate predicate)
                      sender (desugared-role sender)
                      receiver (desugared-role receiver)]
     `(ast/handshake ~predicate ~sender ~receiver))))

(defmacro -->>
  ([sender receiver]
   `(-->> ~'Object ~sender ~receiver))
  ([predicate sender receiver]
   (clojure.core/let [predicate (desugared-predicate predicate)
                      sender (desugared-role sender)
                      receiver (desugared-role receiver)]
     `(ast/cat [(ast/send ~predicate
                          ~sender
                          ~receiver)
                (ast/receive ~sender
                             ~receiver)]))))

(defmacro close
  [sender receiver]
  (clojure.core/let [sender (desugared-role sender)
                     receiver (desugared-role receiver)]
    `(ast/close ~sender ~receiver)))

;;;;
;;;; Nullary operators
;;;;

(defmacro skip
  []
  `(ast/skip))

(defmacro any
  [roles]
  (clojure.core/let [object (ast/predicate 'Object)
                     roles (map desugared-role roles)
                     branches (mapv (fn [[sender receiver]]
                                      `(ast/alt [(ast/handshake ~object ~sender ~receiver)
                                                 (ast/send ~object ~sender ~receiver)
                                                 (ast/receive ~sender ~receiver)
                                                 (ast/close ~sender ~receiver)]))
                                    (for [sender roles
                                          receiver roles
                                          :when (not= sender receiver)]
                                      [sender receiver]))]
    `(ast/alt ~branches)))

;;;;
;;;; Unary operators
;;;;

(defmacro *
  ([body]
   `(* ~body (skip)))
  ([body continuation]
   (clojure.core/let [name `*#]
     `(loop ~name [] (alt (cat ~body (s/recur ~name)) ~continuation)))))

(defmacro +
  ([body]
   `(+ ~body (skip)))
  ([body continuation]
   `(cat ~body (* ~body ~continuation))))

(defmacro ?
  [body]
  `(alt ~body (skip)))

(defmacro async*
  ([body]
   `(async* ~body (skip)))
  ([body continuation]
   (clojure.core/let [name `*#]
     `(loop ~name [] (alt (async ~body (s/recur ~name)) ~continuation)))))

(defmacro async+
  ([body]
   `(async+ ~body (skip)))
  ([body continuation]
   `(async ~body (async* ~body ~continuation))))

(defmacro ω
  [body]
  (clojure.core/let [name `ω#]
    `(loop ~name [] ~body (s/recur ~name))))

(defmacro omega
  [body]
  `(ω ~body))

;;;;
;;;; Multiary operators
;;;;

;; TODO: Merge/shuffle on trajectories

(defn- multiary [f branches]
  (clojure.core/let [branches (mapv (comp macroexpand desugared-spec) branches)]
    (case (clojure.core/count branches)
      0 `(skip)
      1 (first branches)
      `(~f ~branches))))

(defmacro cat
  [& branches]
  (multiary `ast/cat branches))

(defmacro alt
  [& branches]
  (multiary `ast/alt branches))

(defmacro par
  [& branches]
  (multiary `ast/par branches))

(defmacro async
  [& branches]
  (multiary `ast/async branches))

(defn- every [f bindings branch]
  (clojure.core/let [branch (macroexpand (desugared-spec branch))]
    `(ast/every ~f ~bindings ~branch)))

(defmacro cat-every
  [bindings branch]
  (every `ast/cat `(w/postwalk-replace ~(smap &env) '~bindings) branch))

(defmacro alt-every
  [bindings branch]
  (every `ast/alt `(w/postwalk-replace ~(smap &env) '~bindings) branch))

(defmacro par-every
  [bindings branch]
  (every `ast/par `(w/postwalk-replace ~(smap &env) '~bindings) branch))

(defmacro async-every
  [bindings branch]
  (every `ast/async `(w/postwalk-replace ~(smap &env) '~bindings) branch))

;;;;
;;;; "Special forms" operators
;;;;

(defmacro if
  ([test-expr then]
   `(s/if ~test-expr ~then (skip)))
  ([test-expr then else]
   (clojure.core/let [then (macroexpand (desugared-spec then))
                      else (macroexpand (desugared-spec else))]
     `(ast/if (w/postwalk-replace ~(smap &env) '~test-expr) ~then ~else))))

(defmacro do
  [& body]
  `(cat ~@body))

(defmacro let
  [bindings & body]
  (clojure.core/let [body (macroexpand `(cat ~@body))]
    `(ast/loop nil
               (w/postwalk-replace ~(smap &env) '~bindings)
               ~body)))

(defmacro loop
  [name bindings & body]
  (clojure.core/let [body (macroexpand `(cat ~@body))]
    `(ast/loop (w/postwalk-replace ~(smap &env) '~name)
               (w/postwalk-replace ~(smap &env) '~bindings)
               ~body)))

(defmacro recur
  [name & exprs]
  `(ast/recur (w/postwalk-replace ~(smap &env) '~name)
              (w/postwalk-replace ~(smap &env) '[~@exprs])))

;;;;
;;;; Misc operators
;;;;

(defmacro graph [_ header & more]
  `(ast/graph (first '~header) '~more))

;;;;
;;;; Sessions
;;;;

(defmacro session
  [k exprs]
  `(ast/session ~k (w/postwalk-replace ~(smap &env) '~exprs)))

(defmacro defsession
  [k vars & body]
  (when-let [ns (namespace k)]
    (ns-unmap (symbol ns) (symbol (clojure.core/name k)))
    (intern (symbol ns)
            (symbol (clojure.core/name k))
            (fn [& vals]
              (clojure.core/let [indices (vec vals)]
                (session k indices)))))
  (clojure.core/let [body (macroexpand `(cat ~@body))]
    `(ast/put-ast! ~k (w/postwalk-replace ~(smap &env) '~vars) ~body)))

(s/defsession ::-->>not [t r1 r2]
  (s/-->> (fn [x] (not= (type x) t)) r1 r2))

(s/defsession ::pipe [t r-name min max]
  (s/loop pipe [i min]
          (s/if (< i (dec max))
            (s/cat (s/-->> t (r-name i) (r-name (inc i)))
                   (s/recur pipe (inc i))))))

(s/defsession ::pipe [t r-name n]
  (s/session ::pipe ['t r-name 0 n]))

;;;;
;;;; Set operations (convenience)
;;;;

(def empty? clojure.core/empty?)
(def count clojure.core/count)
(def disj clojure.core/disj)

(def empty-set #{})

(defn power-set [s]
  (if (empty? s)
    #{#{}}
    (reduce clojure.set/union
            (map (fn [subset] #{subset (clojure.set/union #{(first s)} subset)})
                 (power-set (rest s))))))

(def difference clojure.set/difference)
(def union clojure.set/union)