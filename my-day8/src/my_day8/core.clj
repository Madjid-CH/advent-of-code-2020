(ns my-day8.core
  (:require [clojure.string :as str]))


(defn nop [machine]
  (update machine :pc inc)
  )

(defn acc [arg {:keys [pc ac]}]
  {:ac (+ ac arg) :pc (inc pc)}
  )

(defn jmp [arg machine]
  (update machine :pc + arg)
  )

(defn execute-instruction [machine program]
  (let [[op-code arg] (nth program (:pc machine))
        history (:history machine)
        pc (:pc machine)
        new-machine (condp = op-code
                      :nop (nop machine)
                      :jmp (jmp arg machine)
                      :acc (acc arg machine)
                      machine)
        new-machine (assoc new-machine :history (conj history pc))]
    new-machine))

(defn execute [program]
  (loop [machine {:ac 0 :pc 0 :history []}]
    (if (or (>= (:pc machine) (count program))
            (contains? (set (:history machine)) (:pc machine)))
      machine
      (recur (execute-instruction machine program)))))

(defn parse-instruction [instruction]
  (let [[_ op-code arg] (re-matches #"([a-z]{3}) (.\d+)" instruction)]
    [(keyword op-code) (Integer/parseInt arg)]))

(defn parse-instructions [instructions]
  (map parse-instruction (str/split instructions #"\n")))