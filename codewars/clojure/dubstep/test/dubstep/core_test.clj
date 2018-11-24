(ns dubstep.core-test
  (:require [clojure.test :refer :all]
            [dubstep.core :refer :all]))

(deftest basic-tests
  (is (= (song-decoder "AWUBBWUBC") "A B C"))
  (is (= (song-decoder "AWUBWUBWUBBWUBWUBWUBC") "A B C"))
  (is (= (song-decoder "WUBAWUBBWUBCWUB") "A B C"))
)
