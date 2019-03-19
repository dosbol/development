(ns discourje.core.protocol
  (:require [discourje.core.dataStructures :refer :all]
            [clojure.core.async])
  (:import (clojure.lang Seqable)
           (discourje.core.dataStructures recursion recur! choice sendM receiveM))
  (use [discourje.core.validator :only [log-error]]))

(defn- generateChannel
  "Function to generate a channel between sender and receiver"
  ([sender receiver]
   (generateChannel sender receiver nil))
  ([sender receiver buffer]
   (if (nil? buffer)
     (->communicationChannel (str sender) (str receiver) (clojure.core.async/chan))
     (->communicationChannel (str sender) (str receiver) (clojure.core.async/chan buffer)))))

(defn uniqueCartesianProduct
  "Generate channels between all participants and filters out duplicates e.g.: buyer1<->buyer1"
  [x y]
  (filter some?
          (for [x x y y]
            (when (not (identical? x y))
              (vector x y)))))

(defn generateChannels
  "Generates communication channels between all participants"
  [participants buffer]
  (map #(apply (fn [s r] (generateChannel s r buffer)) %) (uniqueCartesianProduct participants participants)))


(defn- findAllRecursions
  "Find all recursions in protocol"
  ([protocol result]
   (let [result2 (flatten (vec (conj result [])))]
     (for [element protocol
           :when (or (instance? recursion element) (instance? choice element))]
       (cond
         (instance? recursion element)
         (flatten (vec (conj result2 [(:name element) (findAllRecursions (:protocol element) result2)])))
         (instance? choice element)
         (let [trueResult (findAllRecursions (:trueBranch element) result2)
               falseResult (findAllRecursions (:falseBranch element) result2)]
           (if (not (nil? trueResult))
             (flatten (vec (conj result2 trueResult)))
             (flatten (vec (conj result2 falseResult))))))))))

(defn- findNestedRecurByName
  "Find a (nested) recursion map in the protocol by name, preserves nested structure in result!"
  [protocol name]
  (for [element protocol
        :when (or (instance? recursion element) (instance? choice element))]
    (cond
      (instance? recursion element)
      (if (= (:name element) name)
        element
        (findNestedRecurByName (:protocol element) name))
      (instance? choice element)
      (let [trueResult (findNestedRecurByName (:trueBranch element) name)
            falseResult (findNestedRecurByName (:falseBranch element) name)]
        (if (not (nil? trueResult))
          trueResult
          falseResult)))))

(defn- findRecurByTag
  "Find a (nested) recursion :end or :recur in the protocol by name"
  [protocol name tag]
  (for [element protocol
        :when (or (instance? recursion element) (instance? choice element) (instance? recur! element))]
    (cond
      (instance? recursion element)
      (findRecurByTag (:protocol element) name tag)
      (instance? choice element)
      (let [trueResult (filter some? (findRecurByTag (:trueBranch element) name tag))
            falseResult (filter some? (findRecurByTag (:falseBranch element) name tag))]
        (if (not (empty? trueResult))
          trueResult
          falseResult))
      (instance? recur! element)
      (when (and
              (= (:name element) name)
              (= (:status element) tag))
        element))))

(defn duplicates?
  "Returns true when duplicates inside collection"
  [coll except]
  (let [except (set except)
        filtered (remove #(contains? except %) coll)]
    (not= filtered (distinct filtered))))

(defn containsDuplicates?
  "Checks the recursion vector for recursions"
  [definedRecursions]
  (duplicates? definedRecursions ""))

(defn findAllRecursionsInProtocol
  "Find all recursions inside protocol definition, this will include duplicates to later check for"
  [protocol]
  (let [x (findAllRecursions protocol [])]
    (vec (first (drop-while empty? x)))))

(defn findRecurByName
  "Find a (nested) recursion map in the protocol, returns the recursion map directly!"
  [protocol name]
  (println name)
  (let [x (findNestedRecurByName protocol name)]
    (first (drop-while empty? (flatten x)))))

(defn- hasCorrectRecurAndEnd?
  "Is the protocol correctly recurred and ended?"
  [protocol definedRecursions]
  (let [recurs (distinct definedRecursions)]
    (every? true? (for [rec recurs]
                    (let [recursionVector (findRecurByName protocol rec)
                          recurElement (first (filter some? (findRecurByTag (:protocol recursionVector) rec :recur)))
                          endElement (first (filter some? (findRecurByTag (:protocol recursionVector) rec :end)))]
                      (and (not (empty? recurElement)) (not (empty? endElement))))))))

(defn isProtocolValid?
  "Returns true when there are no duplicate recursion definitions and,=>
  we will include checking for proper recur! [recur, end] definitions!"
  [protocol]
  (let [definedRecursion (findAllRecursionsInProtocol protocol)]
    (and (not (containsDuplicates? definedRecursion)) (hasCorrectRecurAndEnd? protocol definedRecursion))))

(defn- validateRecursion
  "Checks the protocol for duplicate recursion definitions and if recur/ended correctly"
  [monitors]
  (isProtocolValid? monitors))


(defn- findAllParticipants
  "List all sender and receivers in the protocol"
  [protocol result]
  (let [result2 (flatten (vec (conj result [])))]
    (conj result2
          (flatten
            (for [element protocol]
              (cond
                (instance? recursion element)
                (flatten (vec (conj result2 (findAllParticipants (:protocol element) result2))))
                (instance? choice element)
                (let [trueResult (findAllParticipants (:trueBranch element) result2)
                      falseResult (findAllParticipants (:falseBranch element) result2)]
                  (if (not (nil? trueResult))
                    (flatten (vec (conj result2 trueResult)))
                    (flatten (vec (conj result2 falseResult)))))
                (or (instance? sendM element) (instance? receiveM element))
                (do
                  (if (instance? Seqable (:to element))
                    (conj result2 (flatten (:to element)) (:from element))
                    (conj result2 (:to element) (:from element))))))))))

(defn getDistinctParticipants
  "Get all distinct senders and receivers in the protocol"
  [monitors]
  (vec (filter some? (distinct (flatten (first (findAllParticipants monitors [])))))))

(defn generateProtocol
  "Generate the protocol, channels and set the first monitor active through supplied function"
  ([monitors activateFirstMonitor]
   (generateProtocol monitors activateFirstMonitor nil))
  ([monitors activateFirstMonitor buffer]
   (if (isProtocolValid? monitors)
     (let [protocol (->protocolInstance (generateChannels (getDistinctParticipants monitors) buffer) (atom monitors) (atom nil) monitors)]
       (activateFirstMonitor protocol)
       (atom protocol))
     (log-error :invalid-protocol "Supplied monitors are invalid! Make sure there are no duplicate monitor-recursion and that they are recurred and ended correctly!"))))