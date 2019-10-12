(ns discourje.examples.tacas2020.main
  (:gen-class))

(import discourje.examples.tacas2020.Benchmarks)
(import discourje.examples.tacas2020.clbg.spectralnorm.spectralnorm)
(import discourje.examples.tacas2020.misc.chess.Engine)

(defn bench
  [time f]
  (let [begin (System/nanoTime)
        deadline (+ begin (* time 1000 1000 1000))]
    (loop [n 0]
      (f)
      (let [end (System/nanoTime)
            n' (+ n 1)]
        (if (< end deadline)
          (recur n')
          (binding [*out* *err*]
            (println (- end begin) "ns,"
                     n' "runs,"
                     (quot (- end begin) n') "ns/run")))))))

(defn -main
  [& args]
  (try
    (when (< (count args) 4)
      (throw (Exception. "Not enough arguments")))

    (let [verify (nth args 0)
          k (nth args 1)
          time (nth args 2)
          program (nth args 3)]

      (cond
        (= verify "no")
        (do (Benchmarks/useClojure)
            (require '[clojure.core.async :refer [<!! >!! close! chan thread]]))
        (= verify "yes")
        (do (Benchmarks/useDiscourje)
            (require '[discourje.core.async :refer :all]))
        :else
        (throw (Exception. "<language>")))

      (try
        (if (> (Integer/parseInt k) 0)
          (do (Benchmarks/setK (Integer/parseInt k))
              (def K (Benchmarks/K)))
          (throw (Exception. "<k>")))
        (catch NumberFormatException _
          (throw (Exception. "<k>"))))

      (try
        (if (>= (Integer/parseInt time) 0)
          (do (Benchmarks/setTime (Long/parseLong time))
              (def TIME (Benchmarks/TIME)))
          (throw (Exception. "<time>")))
        (catch NumberFormatException _
          (throw (Exception. "<time>"))))

      (cond

        ;;
        ;; CLBG benchmarks
        ;;

        (= program "clbg/spectral-norm")
        (do (binding [*out* *err*] (print args "-> "))
            (bench (Benchmarks/TIME)
                   #(spectralnorm/main (into-array String [(nth args 4)]))))

        ;;
        ;; Micro benchmarks
        ;;

        (and (= program "micro/ring") (= verify "no"))
        (do (binding [*out* *err*] (print args "-> "))
            (require '[discourje.examples.tacas2020.micro.ring.clojure :refer :all])
            (def n-iter (Integer/parseInt (nth args 4)))
            (eval '(discourje.examples.tacas2020.micro.ring.clojure/run
                     discourje.examples.tacas2020.main/K
                     discourje.examples.tacas2020.main/TIME
                     discourje.examples.tacas2020.main/n-iter)))

        (and (= program "micro/ring") (= verify "yes"))
        (do (binding [*out* *err*] (print args "-> "))
            (require '[discourje.examples.tacas2020.micro.ring.discourje :refer :all])
            (def n-iter (Integer/parseInt (nth args 4)))
            (eval '(discourje.examples.tacas2020.micro.ring.discourje/run
                     discourje.examples.tacas2020.main/K
                     discourje.examples.tacas2020.main/TIME
                     discourje.examples.tacas2020.main/n-iter)))

        ;;
        ;; Misc
        ;;

        (and (= program "misc/chess") (= verify "no"))
        (do (binding [*out* *err*] (print args "-> "))
            (set! (. Engine STOCKFISH) (nth args 4))
            (if (>= (count args) 6)
              (set! (. Engine MAX_MOVES) (Integer/parseInt (nth args 5))))
            (if (>= (count args) 7)
              (set! (. Engine TIME) (Integer/parseInt (nth args 6))))
            (if (>= (count args) 8)
              (set! (. Engine MOVES_TO_GO) (Integer/parseInt (nth args 7))))
            (bench (Benchmarks/TIME)
                   #(load "/discourje/examples/tacas2020/misc/chess/clojure")))

        (and (= program "misc/chess") (= verify "yes"))
        (do (binding [*out* *err*] (print args "-> "))
            (set! (. Engine STOCKFISH) (nth args 4))
            (if (>= (count args) 6)
              (set! (. Engine MAX_MOVES) (Integer/parseInt (nth args 5))))
            (if (>= (count args) 7)
              (set! (. Engine TIME) (Integer/parseInt (nth args 6))))
            (if (>= (count args) 8)
              (set! (. Engine MOVES_TO_GO) (Integer/parseInt (nth args 7))))
            (bench (Benchmarks/TIME)
                   #(load "/discourje/examples/tacas2020/misc/chess/discourje")))

        (and (= program "misc/go-fish") (= verify "no"))
        (do (binding [*out* *err*] (print args "-> "))
            (require '[discourje.examples.tacas2020.misc.gofish.clojure :refer :all])
            (eval '(discourje.examples.tacas2020.misc.gofish.clojure/run
                     discourje.examples.tacas2020.main/K)))

        (and (= program "misc/go-fish") (= verify "yes"))
        (do (binding [*out* *err*] (print args "-> "))
            (require '[discourje.examples.tacas2020.misc.gofish.discourje :refer :all])
            (eval '(discourje.examples.tacas2020.misc.gofish.discourje/run
                     discourje.examples.tacas2020.main/K)))

        (and (= program "misc/ttt") (= verify "no"))
        (do (binding [*out* *err*] (print args "-> "))
            (bench (Benchmarks/TIME)
                   #(load "/discourje/examples/tacas2020/misc/ttt/clojure")))

        (and (= program "misc/ttt") (= verify "yes"))
        (do (binding [*out* *err*] (print args "-> "))
            (bench (Benchmarks/TIME)
                   #(load "/discourje/examples/tacas2020/misc/ttt/discourje")))

        :else
        (throw (Exception. "<program>"))))

    (catch Exception e
      (println "Error:" (.getMessage e))
      (println "Usage: java -jar tacas2020.jar <verify?> <k> <time> <program> ...")
      (println "  <verify?> in {no, yes}")
      (println "  <k>       in {0, 1, 2, ...}")
      (println "  <time>    in {0, 1, 2, ...}")
      (print "  <program> in {")
      (print "clbg/spectral-norm")
      (print ", ")
      (print "micro/ring")
      (print ", ")
      (print "misc/chess, misc/go-fish, misc/ttt")
      (println "}"))))

;(-main "yes" "2" "5" "clbg/spectral-norm" "5500")

;(-main "yes" "2" "5" "micro/ring" "1")

;(-main "yes" "4" "0" "misc/go-fish")
;(-main "yes" "2" "5" "misc/chess" "/Users/sung/Desktop/stockfish-10-64" "60")
;(-main "yes" "2" "5" "misc/ttt")