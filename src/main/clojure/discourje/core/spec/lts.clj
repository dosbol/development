(ns discourje.core.spec.lts
  (:require [clojure.set :refer [union]]
            [clojure.java.shell :refer [sh]]
            [discourje.core.spec.interp :as interp])
  (:import (java.util.function Function Predicate Supplier)
           (discourje.core.graph Graph Graphs Edge$Label)))

(defn- label [interp-action]
  {:pre [(interp/action? interp-action)]}
  (Edge$Label. (:name interp-action)
               [(:op interp-action) (:sender interp-action) (:receiver interp-action)]
               (reify
                 Predicate
                 (test [_ message] ((:predicate interp-action) message))
                 Supplier
                 (get [_] (:name interp-action)))))

;;;;
;;;; LTSs
;;;;

(defn lts? [x]
  (= (type x) Graph))

(defn lts [ast & {:keys [on-the-fly history]
                  :or   {on-the-fly false, history false}}]
  (let [initial (if history [ast []] ast)
        expander (if history
                   (reify
                     Function
                     (apply [_ [ast hist]]
                       (let [successors (interp/successors-with-hist ast hist)]
                         (zipmap (map #(label (interp/action %)) (keys successors))
                                 (vals successors)))))
                   (reify
                     Function
                     (apply [_ ast]
                       (let [successors (interp/successors ast)]
                         (zipmap (map #(label (interp/action %)) (keys successors))
                                 (vals successors))))))
        lts (Graph. #{initial} expander)]
    (if (not on-the-fly)
      (Graphs/expandRecursively (.getRoots lts)))
    lts))

(defn initial-states [lts]
  (.getRoots lts))

;(defn roles [lts]
;  (reduce clojure.set/union
;          (map (fn [^State s]
;                 (reduce clojure.set/union
;                         (map (fn [a] #{(.getSender a) (.getReceiver a)})
;                              (.getActions (.getTransitionsOrNull s)))))
;               (.getStates lts))))

(defn traverse-eventually! [source-states command message]
  (Graphs/traverseEventually source-states
                             command
                             message))

(defn traverse-now! [source-states command message]
  (Graphs/traverseNow source-states
                      command
                      message))

(defn bisimilar? [lts1 lts2]
  (Graphs/areBisimilar lts1 lts2))

(defn not-bisimilar? [lts1 lts2]
  (not (bisimilar? lts1 lts2)))