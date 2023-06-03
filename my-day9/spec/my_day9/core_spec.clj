(ns my-day9.core-spec
  (:require [clojure.string :as str]
            [my-day9.core :refer :all]
            [speclj.core :refer :all]))

(defn parse-input [input]
  (->> input
       str/split-lines
       (mapv #(Long/parseLong %))))

(let [input (parse-input
              "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576")]

  (describe "Acceptance Tests"
            (it "passes problem 1 acceptance test"
                (should= 127 (find-sum-error 5 input)))

            (it "passes problem 2 acceptance test"
                (should= 62 (find-encryption-weakness 127 input)))
            ))

(describe "tools"
          (it "should find combinations of sub sequence"
              (should= [[3 4]
                        [3 5]
                        [4 5]
                        ] (get-combinations 2 3 [1 2 3 4 5 6 7])))

          (it "should find sums"
              (should= [7 8 9] (get-sums (get-combinations 2 3 [1 2 3 4 5 6 7]))))
          )

(describe "solvers"
          (it "should solve day 9 problem 1"
              (should= 4 (find-sum-error 2 [1 2 3 4 5 6 7]))))


(let [input (parse-input (slurp "input"))]

  (describe "solutions"
            (it "solves the first problem"
                (should= 400480901 (find-sum-error 25 input)))

            (it "solves the second problem"
                (should= 67587168 (find-encryption-weakness 400480901 input)))
            ))