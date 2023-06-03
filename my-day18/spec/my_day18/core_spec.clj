(ns my-day18.core-spec
  (:require [my-day18.core :refer :all]
            [speclj.core :refer :all]))



(describe "parsing"
          (it "should parse empty expression"
              (should= [] (parse-expression "")))
          (it "should parse one number"
              (should= [3] (parse-expression "3")))
          (it "should parse one operation"
              (should= '[3 + 4] (parse-expression "3 + 4")))
          (it "should parse expression"
              (should= '[3 * 6 + 8 * [8 * 4]] (parse-expression "3 * 6 + 8 * (8 * 4)"))
              (should= '[3 * 3] (parse-expression "3 * 3"))
              (should= '[[3 * 3]] (parse-expression "(3 * 3)"))
              (should= '[[[3 * [6 + 8]] * [8 * 4]]] (parse-expression "((3 * (6 + 8)) * (8 * 4))"))
              ))


(describe "Evaluator"
          (it "should evaluate one number"
              (should= 17 (evaluate [17])))

          (it "should evaluate one operation"
              (should= 3 (evaluate '[1 + 2]))
              (should= 4 (evaluate '[2 * 2])))

          (it "should evaluate more than one operation"
              (should= 3 (evaluate '[1 + 1 + 1]))
              (should= 6 (evaluate '[1 + 2 * [1 + 1]]))))

(describe "Acceptance Tests"
          (it "should pass acceptance tests part 1"
              (should= 71 (evaluate '[1 + 2 * 3 + 4 * 5 + 6]))
              (should= 51 (evaluate (parse-expression "1 + (2 * 3) + (4 * (5 + 6))")))
              (should= 26 (evaluate (parse-expression "2 * 3 + (4 * 5)")))
              (should= 437 (evaluate (parse-expression "5 + (8 * 3 + 9 + 3 * 4 * 3)")))
              (should= 12240 (evaluate (parse-expression "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")))
              (should= 13632 (evaluate (parse-expression "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))))

            (it "should pass acceptance tests part 2"
                (should= 231 (evaluate-2 '[1 + 2 * 3 + 4 * 5 + 6]))
                (should= 51 (evaluate-2 (parse-expression "1 + (2 * 3) + (4 * (5 + 6))")))
                (should= 46 (evaluate-2 (parse-expression "2 * 3 + (4 * 5)")))
                (should= 1445 (evaluate-2 (parse-expression "5 + (8 * 3 + 9 + 3 * 4 * 3)")))
                (should= 669060 (evaluate-2 (parse-expression "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")))
                (should= 23340 (evaluate-2 (parse-expression "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")))))

(describe "Solutions"
          (it "should solve part 1"
              (should= 5374004645253 (solve-1)))

          (it "should solve part 2"
              (should= 88782789402798 (solve-2))))