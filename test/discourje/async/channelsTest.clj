(ns discourje.async.channelsTest
  (:require [clojure.test :refer :all]
            [discourje.async.protocolTestData :refer :all]
            [discourje.core.async.async :refer :all]))

(deftest equal-senders-test
  (let [chans[(->channel 1 2 nil nil nil)
         (->channel 1 3 nil nil nil)
         (->channel 1 4 nil nil nil)]]
    (is (= true (equal-senders? chans)))))

(deftest not-equal-senders-test
  (let [chans[(->channel 1 2 nil nil nil)
              (->channel 5 3 nil nil nil)
              (->channel 1 4 nil nil nil)]]
    (is false? (equal-senders? chans))))



(deftest dual-channels-test
  (let [roles (get-distinct-roles (get-interactions (testDualProtocol)))
        channels (generate-channels roles nil 1)]
    (is (= 2 (count channels)))))

(deftest triple-channels-test
  (let [roles (get-distinct-roles (get-interactions (testTripleProtocol)))
        channels (generate-channels roles nil 1)]
    (is (= 6 (count channels)))))

(deftest triple-channels-roles-test
  (let [roles (get-distinct-roles (get-interactions (testParallelProtocol)))
        channels (generate-channels roles nil 1)]
    (is (= 6 (count channels)))))

(deftest quad-channels-roles-test
  (let [roles (get-distinct-roles (get-interactions (testQuadProtocol)))
        channels (generate-channels roles nil 1)]
    (is (= 12 (count channels)))))


