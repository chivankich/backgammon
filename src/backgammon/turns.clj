(ns backgammon.turns
  (:gen-class)
  (:use backgammon.init_game)
  (:use backgammon.generic_methods)
  (:use backgammon.move)
  (:use backgammon.choices)
  (:use backgammon.io)
  (:use backgammon.artificial_intelligence)
  (:use backgammon.preparation))

(declare win-check)

(defn turn-white []
  (let [dice (roll-dice)]
    (if (pair? dice)
      (swap! available-moves-white assoc :pair true))
    (swap! available-moves-white assoc 1 (first dice))
    (swap! available-moves-white assoc 2 (second dice))
    (cond
      (and
        (empty? (@board :waiting-white))
        (pair? dice))
      (do
        (show)
        (dotimes [_ 2]
          (move-empty-waiting-white dice))
        (win-check :white))
      (and
        (empty? (@board :waiting-white))
        (not (pair? dice)))
      (do
        (show)
        (move-empty-waiting-white dice)
        (win-check :white))
      (and
        (not (empty? (@board :waiting-white)))
        (pair? dice))
      (do
        (show)
        (dotimes [_ 2]
          (white-out dice))
        (win-check :white))
      (and
        (not (empty? (@board :waiting-white)))
        (not (pair? dice)))
      (do
        (show)
        (white-out dice)
        (win-check :white)))))

(defn turn-black []
  (let [dice (roll-dice)]
    (if (pair? dice)
      (swap! available-moves-black assoc :pair true))
    (swap! available-moves-black assoc 1 (first dice))
    (swap! available-moves-black assoc 2 (second dice))
    (cond
      (and
        (empty? (@board :waiting-black))
        (pair? dice))
        (do
          (show)
          (dotimes [_ 2]
            (if @versus-cpu?
              (move-empty-waiting-black-cpu dice)
              (move-empty-waiting-black dice)))
          (win-check :black))
      (and
        (empty? (@board :waiting-black))
        (not (pair? dice)))
        (do
          (show)
          (if @versus-cpu?
            (move-empty-waiting-black-cpu dice)
            (move-empty-waiting-black dice))
          (win-check :black))
      (and
        (not (empty? (@board :waiting-black)))
        (pair? dice))
      (do
        (show)
        (dotimes [_ 2]
          (if @versus-cpu?
            (black-out-cpu dice)
            (black-out dice)))
        (win-check :black))
      (and
        (not (empty? (@board :waiting-black)))
        (not (pair? dice)))
      (do
        (show)
        (if @versus-cpu?
          (black-out-cpu dice)
          (black-out dice))
        (win-check :black)))))

(defn win-check [color]
  (if-not (= color :white)
    (if (win? :black)
      (do
        (println "=====================================================")
        (println "|   Congratulations, Black! You are the winner! :)  |")
        (println "====================================================="))
      (turn-white))
  (if (win? :white)
    (do
      (println "=====================================================")
      (println "|   Congratulations, White! You are the winner! :)  |")
      (println "====================================================="))
    (turn-black))))

(defn black-or-white? []
  (let [dice (roll-dice)]
    (cond
      (< (first dice) (second dice))
      (do
        (println "So... " (first dice) "<" (second dice) "----> black are first!")
        (Thread/sleep 4000)
        (turn-black))
      (> (first dice) (second dice))
      (do
        (println "So..." (first dice) ">" (second dice) "----> white are first!")
        (Thread/sleep 4000)
        (turn-white))
      :else
      (do
        (println "Ooops! Draw! :)")
        (black-or-white?)))))