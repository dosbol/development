(ns discourje.examples.games.tic-tac-toe
  (:require [clojure.core.async]
            [discourje.core.async]
            [discourje.core.spec :as s]
            [discourje.examples.config :as config]))

(config/clj-or-dcj)

;;;;
;;;; Specification
;;;;

(s/defrole ::alice)
(s/defrole ::bob)

(s/defsession ::tic-tac-toe-unbuffered []
  (s/alt (::tic-tac-toe-turn-unbuffered ::alice ::bob)
         (::tic-tac-toe-turn-unbuffered ::bob ::alice)))

(s/defsession ::tic-tac-toe-turn-unbuffered [r1 r2]
  (s/cat (s/--> Long r1 r2)
         (s/alt (::tic-tac-toe-turn-unbuffered r2 r1)
                (s/par (s/close r1 r2)
                       (s/close r2 r1)))))

(s/defsession ::tic-tac-toe-buffered []
  (s/alt (::tic-tac-toe-turn-buffered ::alice ::bob)
         (::tic-tac-toe-turn-buffered ::bob ::alice)))

(s/defsession ::tic-tac-toe-turn-buffered [r1 r2]
  (s/async (s/-->> Long r1 r2)
           (s/alt (::tic-tac-toe-turn-buffered r2 r1)
                  (s/par (s/close r1 r2)
                         (s/close r2 r1)))))

;;;;
;;;; Implementation
;;;;

(def blank " ")
(def cross "x")
(def nought "o")

(def initial-grid
  [blank blank blank
   blank blank blank
   blank blank blank])

(defn get-blank [g]
  (loop [i (long (rand-int 9))]
    (if (= (nth g i) blank)
      i
      (recur (mod (inc i) 9)))))

(defn put [g i x-or-o]
  (assoc g i x-or-o))

(defn not-final? [g]
  (and (loop [i 0]
         (cond (= (nth g i) blank) true
               (= i 8) false
               :else (recur (inc i))))
       (every? #(= false %) (for [l [(set [(nth g 0) (nth g 1) (nth g 2)])
                                     (set [(nth g 3) (nth g 4) (nth g 5)])
                                     (set [(nth g 6) (nth g 7) (nth g 8)])
                                     (set [(nth g 0) (nth g 3) (nth g 6)])
                                     (set [(nth g 1) (nth g 4) (nth g 7)])
                                     (set [(nth g 2) (nth g 5) (nth g 8)])
                                     (set [(nth g 0) (nth g 4) (nth g 8)])
                                     (set [(nth g 2) (nth g 4) (nth g 6)])]]
                              (and (= (count l) 1) (not= (first l) blank))))))

(defn println-grid [g]
  (println)
  (println "+---+---+---+")
  (println "|" (nth g 0) "|" (nth g 1) "|" (nth g 2) "|")
  (println "+---+---+---+")
  (println "|" (nth g 3) "|" (nth g 4) "|" (nth g 5) "|")
  (println "+---+---+---+")
  (println "|" (nth g 6) "|" (nth g 7) "|" (nth g 8) "|")
  (println "+---+---+---+")
  (println))

(let [input config/*input*
      _ (:resolution input)
      buffered (:buffered input)]

  (let [;; Start timer
        begin (System/nanoTime)

        ;; Create channels
        a->b (if buffered (a/chan 1) (a/chan))
        b->a (if buffered (a/chan 1) (a/chan))

        ;; Link monitor [optional]
        _
        (if (= config/*lib* :dcj)
          (let [s (if buffered (tic-tac-toe-buffered) (tic-tac-toe-unbuffered))
                m (a/monitor s)]
            (a/link a->b alice bob m)
            (a/link b->a bob alice m)))

        ;; Spawn threads
        alice
        (a/thread (loop [g initial-grid]
                    (let [i (get-blank g)
                          g (put g i cross)]
                      (a/>!! a->b i)
                      (if (not-final? g)
                        (let [i (a/<!! b->a)
                              g (put g i nought)]
                          (if (not-final? g)
                            (recur g)))
                        (println-grid g))))
                  (a/close! a->b))

        bob
        (a/thread (loop [g initial-grid]
                    (let [i (a/<!! a->b)
                          g (put g i cross)]
                      (if (not-final? g)
                        (let [i (get-blank g)
                              g (put g i nought)]
                          (a/>!! b->a i)
                          (if (not-final? g)
                            (recur g)
                            (println-grid g))))))
                  (a/close! b->a))

        ;; Await termination
        output
        (do (a/<!! alice)
            (a/<!! bob)
            nil)

        ;; Stop timer
        end (System/nanoTime)]

    (set! config/*output* output)
    (set! config/*time* (- end begin))))