(ns my-day10.core-spec
  (:require [clojure.string :refer [split-lines]]
            [my-day10.core :refer :all]
            [speclj.core :refer :all]))

(def test-input-1 "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4")
(def test-input-2 "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3")
(def input (slurp "input"))

(defn to-ints [s]
  (let [lines (split-lines s)]
    (mapv #(Integer/parseInt %) lines)))

(describe "tools"
          (it "should calculate differences"
              (should= [1 2 3] (get-differences [1 2 4 7]))))

(describe "solution"
          (it "should find solution problem 1"
              (should= 9 (solve-1 [1 2 4 7 8 11])))

          (it "should find solution problem 2"
              (should= 3 (solve-2 [1 2 4 7 8 11]))))

(describe "Acceptance Tests"
          (it "should solve problem 1 test-data"
              (should= 35 (solve-1 (to-ints test-input-1)))
              (should= 220 (solve-1 (to-ints test-input-2))))

          (it "should solve problem 2 test-data"
              (should= 8 (solve-2 (to-ints test-input-1)))
              (should= 19208 (solve-2 (to-ints test-input-2)))))

(describe "solutions"
          (it "should solve problem 1"
              (should= 2112 (solve-1 (to-ints input))))

          (it "should solve problem 2"
              (should= 3022415986688 (solve-2 (to-ints input)))))