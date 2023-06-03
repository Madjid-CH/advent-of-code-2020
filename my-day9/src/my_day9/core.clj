(ns my-day9.core
  (:require [clojure.math.combinatorics :refer [combinations]]))



(defn get-combinations [start length numbers]
  (let [sub (subvec numbers start (+ start length))]
    (combinations sub 2)
    ))

(defn get-sums [combinations]
  (map #(reduce + %) combinations))

(defn get-unmatched-sums [indices n numbers]
  (for [index indices]
    (let [start (- index n)
          combinations (get-combinations start n numbers)
          sums (get-sums combinations)
          number (nth numbers index)
          matches (filter #(= number %) sums)
          no-match? (empty? matches)]
      (when no-match? number))))

(defn find-sum-error [n numbers]
  (let [indices (range n (count numbers))
        unmatched (get-unmatched-sums indices n numbers)]
    (first (remove nil? unmatched))))

(defn find-encryption-weakness [n numbers]
  (let [indices (range (dec (count numbers)))
        ranges (combinations indices 2)
        spans (map #(subvec numbers (first %) (inc (second %))) ranges)
        sums (map #(list (reduce + %) %) spans)
        [_ span] (first (filter #(= n (first %)) sums))
        smallest (apply min span)
        largest (apply max span)]
    (+ smallest largest)))

