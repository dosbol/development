(ns discourje.async.monitorTests
  (:require [clojure.test :refer :all]
            [discourje.async.protocolTestData :refer :all]
            [discourje.core.async.async :refer :all]))

(deftest get-active-interaction-test
  (let [mon (generate-monitor (testDualProtocol))
        i0 (nth (:interactions mon) 0)
        i1 (nth (:interactions mon) 1)]
    (is (= "1" (get-action (get-active-interaction mon))))
    (is (= "A" (get-sender (get-active-interaction mon))))
    (is (= "B" (get-receivers (get-active-interaction mon))))
    (is (= (get-next i0) (get-id i1)))
    (is (= (get-next i1) nil))))

(deftest dual-protocol-monitor-test
  (let [mon (generate-monitor (testDualProtocol))]
    (is (= 2 (count (:interactions mon))))))

(deftest dual-protocol-ids-test
  (let [mon (generate-monitor (testDualProtocol))
        i0 (nth (:interactions mon) 0)
        i1 (nth (:interactions mon) 1)]
    (is (= (get-next i0) (get-id i1)))
    (is (= (get-next i1) nil))))

(deftest triple-protocol-monitor-test
  (let [mon (generate-monitor (testTripleProtocol))]
    (is (= 3 (count (:interactions mon))))))

(deftest triple-protocol-ids-test
  (let [mon (generate-monitor (testTripleProtocol))
        i0 (nth (:interactions mon) 0)
        i1 (nth (:interactions mon) 1)
        i2 (nth (:interactions mon) 2)]
    (is (= (get-next i0) (get-id i1)))
    (is (= (get-next i1) (get-id i2)))
    (is (= (get-next i2) nil))))

(deftest parallel-protocol-monitor-test
  (let [mon (generate-monitor (testParallelProtocol))]
    (is (= 4 (count (:interactions mon))))))

(deftest parallel-protocol-ids-test
  (let [mon (generate-monitor (testParallelProtocol))
        i0 (nth (:interactions mon) 0)
        i1 (nth (:interactions mon) 1)
        i2 (nth (:interactions mon) 2)
        i3 (nth (:interactions mon) 3)]
    (is (= (get-next i0) (get-id i1)))
    (is (= (get-next i1) (get-id i2)))
    (is (= (get-next i2) (get-id i3)))
    (is (= (get-next i3) nil))))

(deftest quad-protocol-monitor-test
  (let [mon (generate-monitor (testQuadProtocol))]
    (is (= 5 (count (:interactions mon))))))

(deftest quad-protocol-ids-test
  (let [mon (generate-monitor (testQuadProtocol))
        i0 (nth (:interactions mon) 0)
        i1 (nth (:interactions mon) 1)
        i2 (nth (:interactions mon) 2)
        i3 (nth (:interactions mon) 3)
        i4 (nth (:interactions mon) 4)]
    (is (= (get-next i0) (get-id i1)))
    (is (= (get-next i1) (get-id i2)))
    (is (= (get-next i2) (get-id i3)))
    (is (= (get-next i3) (get-id i4)))
    (is (= (get-next i4) nil))))

(deftest quad-protocol-monitor-test
  (let [mon (generate-monitor (testQuadProtocol))]
    (is (= 5 (count (:interactions mon))))))

(deftest single-choice-protocol-test
  (let [mon (generate-monitor (single-choice-protocol))]
    (is (= 1 (count (:interactions mon))))))

(deftest single-choice-in-middle-protocol-test
  (let [mon (generate-monitor (single-choice-in-middle-protocol))]
    (println (:interactions mon))
    (is (= 3 (count (:interactions mon))))))

(deftest single-choice-in-middle-protocol-ids-test
  (let [mon (generate-monitor (single-choice-in-middle-protocol))
        i0 (nth (:interactions mon) 0)
        i1 (nth (:interactions mon) 1)
        branch0 (nth (:branches i1) 0)
        i100 (nth branch0 0)
        i101 (nth branch0 1)
        branch1 (nth (:branches i1) 1)
        i110 (nth branch1 0)
        i111 (nth branch1 1)
        i2 (nth (:interactions mon) 2)]
    (println (:interactions mon))
    (is (= (get-next i0) (get-id i1)))
    (is (= (get-next i1) (get-id i2)))
    (is (= (get-next i100) (get-id i101)))
    (is (= (get-next i101) (get-id i2)))
    (is (= (get-next i110) (get-id i111)))
    (is (= (get-next i111) (get-id i2)))
    (is (= (get-next i2) nil))))

(deftest single-choice-5branches-protocol-test
  (let [mon (generate-monitor (single-choice-5branches-protocol))]
    (is (= 2 (count (:interactions mon))))))

(deftest single-choice-5branches-protocol-ids-test
  (let [mon (generate-monitor (single-choice-5branches-protocol))
        i0 (nth (:interactions mon) 0)
        i0b0 (nth (nth (:branches i0) 0) 0)
        i0b1 (nth (nth (:branches i0) 1) 0)
        i0b2 (nth (nth (:branches i0) 2) 0)
        i0b3 (nth (nth (:branches i0) 3) 0)
        i0b4 (nth (nth (:branches i0) 4) 0)
        i1 (nth (:interactions mon) 1)]
    (is (= (get-next i0) (get-id i1)))
    (is (= (get-next i0b0) (get-id i1)))
    (is (= (get-next i0b1) (get-id i1)))
    (is (= (get-next i0b2) (get-id i1)))
    (is (= (get-next i0b3) (get-id i1)))
    (is (= (get-next i0b4) (get-id i1)))
    (is (= (get-next i1) nil))))

(deftest dual-choice-protocol-test
  (let [mon (generate-monitor (dual-choice-protocol))]
    (is (= 2 (count (:interactions mon))))))

(deftest dual-choice-protocol-ids-test
  (let [mon (generate-monitor (dual-choice-protocol))
        i0 (nth (:interactions mon) 0)
        i0b0 (nth (nth (:branches i0) 0) 0)
        i0b10 (nth (nth (:branches i0) 1) 0)
        i0b11 (nth (nth (:branches i0) 1) 1)
        i0b1b10 (nth (nth (:branches i0b11)0)0)
        i0b1b11 (nth (nth (:branches i0b11)1)0)
        i1 (nth (:interactions mon) 1)]
    (is (= (get-next i0) (get-id i1)))
    (is (= (get-next i0b0) (get-id i1)))
    (is (= (get-next i0b10) (get-id i0b11)))
    (is (= (get-next i0b1b10) (get-id i1)))
    (is (= (get-next i0b1b11) (get-id i1)))
    (is (= (get-next i1) nil))))


(deftest multiple-nested-choice-branch-protocol-test
  (let [mon (generate-monitor (multiple-nested-choice-branch-protocol))]
    (is (= 1 (count (:interactions mon))))))

(deftest multiple-nested-choice-branch-protocol-ids-test
  (let [mon (generate-monitor (multiple-nested-choice-branch-protocol))
        i0 (nth (:interactions mon) 0)
        i0b00 (nth (nth (:branches i0) 0) 0)
        i0b00b00 (nth (nth (:branches i0b00) 0) 0)
        i0b00b10 (nth (nth (:branches i0b00) 1) 0)
        i0b10 (nth (nth (:branches i0) 1) 0)
        i0b10b00 (nth (nth (:branches i0b10) 0) 0)
        i0b10b10 (nth (nth (:branches i0b10) 1) 0)]
    (is (= (get-next i0) nil))
    (is (= (get-next i0b00) nil))
    (is (= (get-next i0b10) nil))
    (is (= (get-next i0b00b00) nil))
    (is (= (get-next i0b00b10) nil))
    (is (= (get-next i0b10b00) nil))
    (is (= (get-next i0b10b10) nil))))

(deftest single-choice-multiple-interactions-protocol-test
  (let [mon (generate-monitor (single-choice-multiple-interactions-protocol))]
    (is (= 6 (count (:interactions mon))))))

(deftest multiple-nested-branches-protocol-test
  (let [mon (generate-monitor (multiple-nested-branches-protocol))]
    (is (= 2 (count (:interactions mon))))))

(deftest multiple-nested-branches-protocol-ids-test
  (let [mon (generate-monitor (multiple-nested-branches-protocol))
        i0 (nth (:interactions mon) 0)
        i0b0 (nth (:branches i0)0)
        i0b0b00 (nth (nth (:branches (nth i0b0 0))0)0)
        i0b0b01 (nth (nth (:branches (nth i0b0 0))0)1)
        i0b0b10 (nth (nth (:branches (nth i0b0 0))1)0)
        i0b1  (nth (:branches i0) 1)
        i0b1b0 (nth (nth (:branches (nth i0b1 0))0)0)
        i0b1b0b0 (nth (:branches i0b1b0)0)
        i0b1b0b0b00 (nth (nth (:branches (nth i0b1b0b0 0))0)0)
        i0b1b0b0b10 (nth (nth (:branches (nth i0b1b0b0 0))1)0)
        i0b1b0b0b11 (nth (nth (:branches (nth i0b1b0b0 0))1)1)
        i0b1b0b0b12 (nth (nth (:branches (nth i0b1b0b0 0))1)2)
        i0b1b0b10 (nth (nth (:branches i0b1b0)1)0)
        i0b1b11 (nth (nth (:branches (nth i0b1 0))1)0)
        i1 (nth (:interactions mon) 1)
       ]
    (is (= (get-next i0) (get-id i1)))
    (is (= (get-next i0b0b00) (get-id i0b0b01)))
    (is (= (get-next i0b0b01) (get-id i1)))
    (is (= (get-next i0b0b10) (get-id i1)))
    (is (= (get-next i0b1b11) (get-id i1)))
    (is (= (get-next i0b1b0b10) (get-id i1)))
    (is (= (get-next i0b1b0b0b00) (get-id i1)))
    (is (= (get-next i0b1b0b0b10) (get-id i0b1b0b0b11)))
    (is (= (get-next i0b1b0b0b11) (get-id i0b1b0b0b12)))
    (is (= (get-next i0b1b0b0b12) (get-id i1)))
    ))

(deftest single-recur-protocol-monitor-test
  (let [mon (generate-monitor (single-recur-protocol))]
    (is (= 3 (count (:interactions mon))))))

(deftest single-recur-protocol-ids-test
  (let [mon (generate-monitor (single-recur-protocol))
        i0 (nth (:interactions mon) 0)
        i1 (nth (:interactions mon) 1)
        i1r0 (nth (:recursion i1) 0)
        i1r1 (nth (:recursion i1) 1)
        i1r1b00 (nth (nth (:branches i1r1)0) 0)
        i1r1b01 (nth (nth (:branches i1r1)0) 1)
        i1r1b02 (nth (nth (:branches i1r1)0) 2)
        i1r1b10 (nth (nth (:branches i1r1)1) 0)
        i1r1b11 (nth (nth (:branches i1r1)1) 1)
        i2 (nth (:interactions mon) 2)]
    (is (= (get-next i0) (get-id i1)))
    (is (= (get-next i1r0) (get-id i1r1)))
    (is (= (get-next i1r1b00) (get-id i1r1b01)))
    (is (= (get-next i1r1b01) (get-id i1r1b02)))
    (is (= (get-next i1r1b02) (get-id i1)))
    (is (= (get-next i1r1b10) (get-id i1r1b11)))
    (is (= (get-next i1r1b11) (get-id i2)))
    (is (= (get-next i1) (get-id i2)))
    (is (= (get-next i2) nil))))

(deftest nested-recur-protocol-monitor-test
  (let [mon (generate-monitor (nested-recur-protocol))]
    (is (= 2 (count (:interactions mon))))))

(deftest nested-recur-protocol-ids-test
  (let [mon (generate-monitor (nested-recur-protocol))
        i0 (nth (:interactions mon) 0)
        i0r0 (nth (:recursion i0) 0)
        i0r0i0 (nth (:recursion i0r0) 0)
        i0r0i1 (nth (:recursion i0r0) 1)
        i0r0i1b00 (nth (nth (:branches i0r0i1)0)0)
        i0r0i1b01 (nth (nth (:branches i0r0i1)0)1)
        i0r0i1b02 (nth (nth (:branches i0r0i1)0)2)
        i0r0i1b10 (nth (nth (:branches i0r0i1)1)0)
        i0r0i1b11 (nth (nth (:branches i0r0i1)1)1)
        i0r0i2  (nth (:recursion i0r0) 2)
        i0r0i2b00 (nth (nth (:branches i0r0i2)0)0)
        i0r0i2b01 (nth (nth (:branches i0r0i2)0)1)
        i0r0i2b02 (nth (nth (:branches i0r0i2)0)2)
        i0r0i2b10 (nth (nth (:branches i0r0i2)1)0)
        i0r0i2b11 (nth (nth (:branches i0r0i2)1)1)
        i1 (nth (:interactions mon) 1)]
    (println (:interactions mon))
    (println i0r0i1b02)
    (is (= (get-next i0) (get-id i1)))
    (is (= (get-next i0r0i0) (get-id i0r0i1)))
    (is (= (get-next i0r0i1b00) (get-id i0r0i1b01)))
    (is (= (get-next i0r0i1b01) (get-id i0r0i1b02)))
    (is (= (get-next i0r0i1b02) (get-id i0r0)))
    (is (= (get-next i0r0i1b10) (get-id i0r0i1b11)))
    ; (is (= (get-next i0r0i1b11) (get-id i1))) ;Todo do we really need an end-recur identifier?

    (is (= (get-next i0r0i2b00) (get-id i0r0i2b01)))
    (is (= (get-next i0r0i2b01) (get-id i0r0i2b02)))
    (is (= (get-next i0r0i2b02) (get-id i0)))
    (is (= (get-next i0r0i2b10) (get-id i0r0i2b11)))
    (is (= (get-next i0r0i2b11) (get-id i1)))
    (is (= (get-next i1) nil))))

(deftest one-recur-with-choice-protocol-monitor-test
  (let [mon (generate-monitor (one-recur-with-choice-protocol))]
    (is (= 1 (count (:interactions mon))))))

(deftest one-recur-with-choice-protocol-ids-test
  (let [mon (generate-monitor (one-recur-with-choice-protocol))
        i0 (nth (:interactions mon) 0)
        i0r0 (nth (:recursion i0) 0)
        i0r0b00 (nth (nth (:branches i0r0) 0) 0)
        i0r0b01 (nth (nth (:branches i0r0) 0) 1)
        i0r0b10 (nth (nth (:branches i0r0) 1) 0)
        i0r0b11 (nth (nth (:branches i0r0) 1) 1)
        ]
    (is (= (get-next i0) nil))
    (is (= (get-next i0r0) nil))
    (is (= (get-next i0r0b00) (get-id i0r0b01)))
    (is (= (get-next i0r0b01) (get-id i0)))
    (is (= (get-next i0r0b10) (get-id i0r0b11)))
    (is (= (get-next i0r0b11) nil))))

(deftest one-recur-with-startchoice-and-endchoice-protocol-monitor-test
  (let [mon (generate-monitor (one-recur-with-startchoice-and-endchoice-protocol))]
    (is (= 1 (count (:interactions mon))))))

(deftest one-recur-with-startchoice-and-endchoice-protocol-ids-test
  (let [mon (generate-monitor (one-recur-with-startchoice-and-endchoice-protocol))
        i0 (nth (:interactions mon) 0)
        i0b0r0 (nth (nth (:branches i0) 0) 0)
        i0b0r00 (nth (:recursion i0b0r0) 0)
        i0b0r00b00 (nth (nth (:branches i0b0r00) 0) 0)
        i0b0r00b01 (nth (nth (:branches i0b0r00) 0) 1)
        i0b0r00b10 (nth (nth (:branches i0b0r00) 1) 0)
        i0b0r00b11 (nth (nth (:branches i0b0r00) 1) 1)
        i0b10 (nth (nth (:branches i0) 1) 0)
        ]
    (is (= (get-next i0) nil))
    (is (= (get-next i0b0r0) nil))
    (is (= (get-next i0b10) nil))
    (is (= (get-next i0b0r00) nil))
    (is (= (get-next i0b0r00b00) (get-id i0b0r00b01)))
    (is (= (get-next i0b0r00b01) (get-id i0b0r0)))
    (is (= (get-next i0b0r00b10) (get-id i0b0r00b11)))
    (is (= (get-next i0b0r00b11) nil))))

(deftest two-buyer-protocol-monitor-test
  (let [mon (generate-monitor (two-buyer-protocol))]
    (is (= 1 (count (:interactions mon))))))

(deftest two-buyer-protocol-ids-test
  (let [mon (generate-monitor (two-buyer-protocol))
        i0 (nth (:interactions mon) 0)
        i0r0i0 (nth (:recursion i0)0)
        i0r0i1 (nth (:recursion i0)1)
        i0r0i2 (nth (:recursion i0)2)
        i0r0i3 (nth (:recursion i0)3)
        i0r0i3b00 (nth (nth (:branches i0r0i3)0)0)
        i0r0i3b01 (nth (nth (:branches i0r0i3)0)1)
        i0r0i3b02 (nth (nth (:branches i0r0i3)0)2)
        i0r0i3b10 (nth (nth (:branches i0r0i3)1)0)
        i0r0i3b11 (nth (nth (:branches i0r0i3)1)1)
        ]
    (is (= (get-next i0) nil))
    (is (= (get-next i0r0i0) (get-id i0r0i1)))
    (is (= (get-next i0r0i1) (get-id i0r0i2)))
    (is (= (get-next i0r0i2) (get-id i0r0i3)))
    (is (= (get-next i0r0i3) nil))
    (is (= (get-next i0r0i3b00) (get-id i0r0i3b01)))
    (is (= (get-next i0r0i3b01) (get-id i0r0i3b02)))
    (is (= (get-next i0r0i3b02) (get-id i0)))
    (is (= (get-next i0r0i3b10) (get-id i0r0i3b11)))
    (is (= (get-next i0r0i3b11) nil))
    ))

(deftest apply-atomic-test
  (let [mon (generate-monitor (testDualProtocol))
        message (->message "1" "hello world")]
    (apply-interaction mon "A" "B" (get-label message))
    (is (= "2" (get-action (get-active-interaction mon))))
    (is (= "B" (get-sender (get-active-interaction mon))))
    (is (= "A" (get-receivers (get-active-interaction mon))))))