(ns my-day17.core-spec
  (:require [my-day17.core :refer :all]
            [speclj.core :refer :all]))

(def empty-world #{})

(describe "Day 17"
          (context "Parsing"
                   (context "part 1"
                            (it "should be able to parse get the size of input"
                                (should= [2 3] (:dimensions (parse-input "..\n..\n..")))
                                (should= [0 0] (:dimensions (parse-input ""))))

                            (it "should be able to build a set of cells 3d"
                                (should= empty-world (:map (parse-input "")))
                                (should= empty-world (:map (parse-input "..\n..\n..")))
                                (should= #{[0 1 0] [1 1 0]} (:map (parse-input "..\n##\n..")))
                                (should= #{[3 0 0] [1 1 0] [2 1 0] [0 0 0]} (:map (parse-input "#..#\n.##.")))))
                   (context "part 2"
                            (it "should be able to build a set of cells 4d"
                                (should= empty-world (parse-input-2 ""))
                                (should= empty-world (parse-input-2 "..\n..\n.."))
                                (should= #{[0 1 0 0] [1 1 0 0]} (parse-input-2 "..\n##\n.."))
                                (should= #{[3 0 0 0] [1 1 0 0] [2 1 0 0] [0 0 0 0]}
                                         (parse-input-2 "#..#\n.##.")))))

          (context "utilities"
                   (context "part 1"
                            (it "should find 26 neighbors"
                                (should= 26 (count (neighbors-of [0 0 0])))
                                (should= #{[0 1 0] [1 0 2] [0 1 1] [0 0 2] [2 0 2] [1 0 0]
                                           [1 2 1] [0 1 2] [0 0 1] [2 2 2] [2 2 0] [0 2 0]
                                           [2 2 1] [1 2 0] [2 1 1] [1 1 0] [1 1 2] [2 0 0]
                                           [0 2 2] [2 1 2] [2 1 0] [1 2 2] [1 0 1] [0 0 0]
                                           [2 0 1] [0 2 1]}
                                         (neighbors-of [1 1 1])))

                            (it "should find active neighbor count"
                                (should= 0 (neighbor-count [0 0 0] empty-world))
                                (should= 1 (neighbor-count [0 0 0] #{[1 1 1]}))
                                (should= 1 (neighbor-count [0 0 0] #{[-1 -1 -1]}))
                                (should= 2 (neighbor-count [0 0 0] #{[1 1 1] [1 0 -1]})))

                            (it "should remove cells with less than two or more than three neighbors"
                                (should= empty-world (get-cells-for-removal empty-world))
                                (should= #{[0 0 0]} (get-cells-for-removal #{[0 0 0]}))
                                (should= #{[0 0 0] [2 0 0]} (get-cells-for-removal (:map (parse-input "###"))))
                                (should= #{[1 0 0] [1 1 0]} (get-cells-for-removal (:map (parse-input "###\n###")))))

                            (it "should find the bounds of the world"
                                (should= [[0 0] [0 0] [0 0]] (get-bounds empty-world))
                                (should= [[1 1] [1 1] [1 1]] (get-bounds #{[1 1 1]}))
                                (should= [[3 1] [3 1] [3 1]] (get-bounds #{[1 1 1] [3 3 3]})))

                            (it "should add cells with 3 neighbors"
                                (should= empty-world (get-new-cells empty-world))
                                (should= #{[1 -1 -1] [1 -1 1] [1 -1 0]
                                           [1 1 1] [1 1 -1] [1 1 0]
                                           [1 0 1] [1 0 -1]} (get-new-cells (:map (parse-input "###"))))))
                   (context "part 2"
                            (it "should find 80 neighbors"
                                (should= 80 (count (neighbors-of-4d [0 0 0 0])))
                                (should= #{[0 1 0 0] [1 0 2 0] [0 1 1 0] [0 0 2 0] [2 0 2 0] [1 0 0 0]
                                           [1 2 1 0] [0 1 2 0] [0 0 1 0] [2 2 2 0] [2 2 0 0] [0 2 0 0]
                                           [2 2 1 0] [1 2 0 0] [2 1 1 0] [1 1 0 0] [1 1 2 0] [2 0 0 0]
                                           [0 2 2 0] [2 1 2 0] [2 1 0 0] [1 2 2 0] [1 0 1 0] [0 0 0 0]
                                           [2 0 1 0] [0 2 1 0]
                                           [0 1 0 1] [1 0 2 1] [0 1 1 1] [0 0 2 1] [2 0 2 1] [1 0 0 1]
                                           [1 2 1 1] [0 1 2 1] [0 0 1 1] [2 2 2 1] [2 2 0 1] [0 2 0 1]
                                           [2 2 1 1] [1 2 0 1] [2 1 1 1] [1 1 0 1] [1 1 2 1] [2 0 0 1]
                                           [0 2 2 1] [2 1 2 1] [2 1 0 1] [1 2 2 1] [1 0 1 1] [0 0 0 1]
                                           [2 0 1 1] [0 2 1 1]
                                           [0 1 0 2] [1 0 2 2] [0 1 1 2] [0 0 2 2] [2 0 2 2] [1 0 0 2]
                                           [1 2 1 2] [0 1 2 2] [0 0 1 2] [2 2 2 2] [2 2 0 2] [0 2 0 2]
                                           [2 2 1 2] [1 2 0 2] [2 1 1 2] [1 1 0 2] [1 1 2 2] [2 0 0 2]
                                           [0 2 2 2] [2 1 2 2] [2 1 0 2] [1 2 2 2] [1 0 1 2] [0 0 0 2]
                                           [2 0 1 2] [0 2 1 2] [1 1 1 2] [1 1 1 0]}
                                         (neighbors-of-4d [1 1 1 1])))

                            (it "should find active neighbor count"
                                (should= 0 (neighbor-count-4d [0 0 0 0] empty-world))
                                (should= 1 (neighbor-count-4d [0 0 0 0] #{[1 1 1 1]}))
                                (should= 1 (neighbor-count-4d [0 0 0 0] #{[-1 -1 -1 -1]}))
                                (should= 2 (neighbor-count-4d [0 0 0 0] #{[1 1 1 1] [1 0 -1 0]})))

                            (it "should remove cells with less than two or more than three neighbors"
                                (should= empty-world (get-cells-for-removal-4d empty-world))
                                (should= #{[0 0 0 0]} (get-cells-for-removal-4d #{[0 0 0 0]}))
                                (should= #{[0 0 0 0] [2 0 0 0]}
                                         (get-cells-for-removal-4d (parse-input-2 "###")))
                                (should= #{[1 0 0 0] [1 1 0 0]}
                                         (get-cells-for-removal-4d (parse-input-2 "###\n###"))))

                            (it "should find the bounds of the world"
                                (should= [[0 0] [0 0] [0 0] [0 0]] (get-bounds-4d empty-world))
                                (should= [[1 1] [1 1] [1 1] [1 1]] (get-bounds-4d #{[1 1 1 1]}))
                                (should= [[3 1] [3 1] [3 1] [3 1]] (get-bounds-4d #{[1 1 1 1] [3 3 3 3]})))

                            (it "should add cells with 3 neighbors"
                                (should= empty-world (get-new-cells-4d empty-world))
                                (should= #{[1 1 -1 1] [1 1 0 -1] [1 0 -1 -1]
                                           [1 0 0 -1] [1 -1 -1 -1] [1 -1 0 0]
                                           [1 -1 1 1] [1 1 0 1] [1 0 1 0]
                                           [1 -1 -1 1] [1 1 -1 -1] [1 1 1 1]
                                           [1 1 0 0] [1 0 -1 0] [1 0 1 -1]
                                           [1 0 -1 1] [1 1 -1 0] [1 1 1 0]
                                           [1 -1 1 0] [1 1 1 -1] [1 -1 -1 0]
                                           [1 -1 1 -1] [1 0 0 1] [1 -1 0 1]
                                           [1 0 1 1] [1 -1 0 -1]} (get-new-cells-4d (parse-input-2 "###"))))))

          (let [input (:map (parse-input ".#.\n..#\n###"))
                input-4d (parse-input-2 ".#.\n..#\n###")]

            (context "Acceptance Tests"
                     (it "should pass part 1"
                         (should= 11 (count (do-turn input)))
                         (should= 21 (count (do-turns 2 input)))
                         (should= 112 (count (do-turns 6 input))))

                     (it "should pass part 2"
                         (should= 29 (count (do-turn-2 input-4d)))
                         (should= 60 (count (do-turns-2 2 input-4d)))
                         (should= 848 (count (do-turns-2 6 input-4d))))))

          (context "Solutions"
                   (it "should solve part 1"
                       (should= 362 (solve-1)))

                   (it "should solve part 2"
                       (should= 1980 (solve-2))))
          )