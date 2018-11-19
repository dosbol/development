(ns projectSetup.ProjectSetupTests
  (:require [clojure.test :refer :all])
  (:require [ProjectSetup.ProjectSetup :refer :all]))

(deftest sumTest
  (is (= 6 (sum 1 2 3))))

(deftest flatListTest
  (is (= `(1 2 3 4 5 6)) flatList))
e
(deftest valueTest
  (is (= 10) value))

(deftest unlessTrue
  (is (= true?) (unless false true)))

(deftest unlessFalse
  (is (= false?) (unless true true)))

(deftest unlessMultiple
  (is (= `(\a \b \c)) (unless false \a \b \c)))

(deftest unlessDoc
  (is (= string?) (clojure.repl/doc unless)))