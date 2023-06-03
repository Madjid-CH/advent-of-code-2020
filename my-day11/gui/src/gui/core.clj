(ns gui.core
  (:require [gui.data :as data]
            [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  (q/frame-rate 1)
  (q/color-mode :rgb)
  {:world data/real-data})

(defn update-state [{:keys [world] :as state}]
  (assoc state :world (data/update-automaton-2 world)))

(defn draw-empty-chair [r c]
  (q/fill 255)
  (q/stroke 0)
  (q/ellipse r c 8 8))

(defn draw-person [r c]
  (q/fill 255 0 0)
  (q/ellipse r c 8 8))

(defn draw-floor [r c]
  )

(defn draw-state [{:keys [world]}]
  (q/background 255)
  (q/translate 10 10)
  (doseq [r (range (count world)) c (range (count (first world)))]
    (let [y (* r 10.7), x (* c 10.7)]
      (condp = (data/get-cell world r c)
        :empty (draw-empty-chair y x)
        :person (draw-person y x)
        :floor (draw-floor y x)))))


(defn ^:export -main [& args]
  (q/defsketch gui
               :title "Advent of Code day 11 Cellular Automaton"
               :size [1000 1000]
               :setup setup
               :update update-state
               :draw draw-state
               :features [:keep-on-top]
               :middleware [m/fun-mode])
  args)
