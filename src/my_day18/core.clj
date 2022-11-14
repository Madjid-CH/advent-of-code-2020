(ns my-day18.core
  (:require [clojure.string :as str]))


(def ^:dynamic _ clojure.core/resolve)

(defn parse-expression [input]
  (if (empty? input)
    []
    (let [input (replace {\( \[ \) \]} input)
          input (list \[ input \])
          input (flatten input)
          expression (apply str input)]
      (read-string expression))))


(def *ops* '[+ *])
(def ^:dynamic rank (zipmap *ops* (iterate identity 1)))
(def ^:dynamic _ clojure.core/resolve)

(defn infix
  [[a b & [c d e & more] :as v]]
  (cond
    (vector? a) (recur (list* (infix a) b c d e more))
    (vector? c) (recur (list* a b (infix c) d e more))
    (rank b) (if (and d (< (rank b 0) (rank d 0)))
               (recur (list a b (infix (list* c d e more))))
               (recur (list* (list (_ b) a c) d e more)))
    :else a))

(defn infix-reader [form]
  (binding [_ identity]
    (infix form)))

(defn evaluate [args]
  (eval (infix args)))

(def input (slurp "input"))

(defn solve-1 []
  (let [input (str/split-lines input)
        expressions (mapv parse-expression input)
        results (mapv (comp eval infix) expressions)]
    (reduce + results)))

(defn evaluate-2 [args]
  (binding [rank (zipmap *ops* (iterate dec 2))]
    (eval (infix args))))

(defn solve-2 []
  (binding [rank (zipmap *ops* (iterate dec 2))]
    (solve-1)))

