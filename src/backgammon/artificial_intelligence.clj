(ns backgammon.artificial_intelligence
  (:gen-class)
  (:use backgammon.init_game)
  (:use backgammon.generic_methods)
  (:use backgammon.move)
  (:use backgammon.choices)
  (:use backgammon.io))

(defn random-position [moves]
  (let [number (rand-int (count moves))]
    (nth moves number)))

(defn store-the-best [moves with]
  (doseq [position moves]
    (let [to (- position with)
          first-counter (count (@board position))
          second-counter (count (@board to))]
      (cond
        (and
          (black? to)
          (= 1 second-counter)
          (>= first-counter 3))
        (swap! possible assoc 1 position)
        (and
          (black? to)
          (= 1 second-counter))
        (swap! possible assoc 2 position)
        (and
          (black? to)
          (>= first-counter 3))
        (swap! possible assoc 3 position)
        (and
          (white? to)
          (>= first-counter 3))
        (swap! possible assoc 4 position)
        (and
          (= 1 first-counter)
          (empty? (@board to)))
        (swap! possible assoc 5 position)
        :else
          (swap! possible assoc 6 (random-position moves))))))

(defn choose-the-best-one []
  (first
    (filter
      (complement nil?) (for [move (map inc (range 6))]
                                    (if-not (nil? (@possible move))
                                      (@possible move))))))

(defn correct-choice-black-cpu [with]
  (if (empty? (available :black with))
    (do
      (println "Sorry, but you can't do any moves... (such a loser!)")
      -1)
    (do
      (store-the-best (available :black with) with)
      (choose-the-best-one))))

(defn move-empty-waiting-black-cpu [dice]
  (println "=====================================================")
  (println "Black! They have " (first dice) "and " (second dice))
  (println "Black are choosing a pull... ")
  (let [pull (correct-choice-black-cpu (first dice))]
    (do
      (println "Black choosed " pull)
      (if-not (= pull -1)
        (if (all-prepared? :black)
          (try-pulling-out :black pull (first dice))
          (move pull (first dice)))
        (println "They can't do any moves..."))))
  (reset! possible {})
  (println "And now waiting for the second dice...")
  (let [pull (correct-choice-black-cpu (second dice))]
    (do
      (println "Black choosed" pull)
      (if-not (= pull -1)
        (if (all-prepared? :black)
          (try-pulling-out :black pull (second dice))
          (move pull (second dice)))
        (println "You can't do any moves..."))))
  (reset! possible {})
  (swap! available-moves-black dissoc 1)
  (swap! available-moves-black dissoc 2))

(defn black-out-cpu [dice]
  (cond
    (empty? (@board :waiting-black))
      (move-empty-waiting-black-cpu dice)
    (and
      (is-in? (first dice) (free-for :black))
      (is-in? (second dice) (free-for :black))
      (< 1 (count (@board :waiting-black))))
      (do
        (println "=====================================================")
        (println "Black! They have |"(first dice)"| and |"(second dice)"|")
        (println "Well, they have to use both numbers to pop pulls! Let's do it!")
        (let [number1 (first dice)]
          (back-in-the-game :black number1))
        (println "Now, Black move their second dice!")
        (let [number2 (second dice)]
          (back-in-the-game :black number2))
        (swap! available-moves-black dissoc 1)
        (swap! available-moves-black dissoc 2)
        (swap! available-moves-black dissoc :pair))
    (and
      (is-in? (first dice) (free-for :black))
      (is-in? (second dice) (free-for :black))
      (= 1 (count (@board :waiting-black))))
      (do
        (println "=====================================================")
        (println "Black! They have |"(first dice)"| and |"(second dice)"|!")
        (let [choice1 (first dice)]
          (back-in-the-game :black choice1))
        (let [sec (second dice)]
          (println "And now Black chosed their second dice!")
          (let [pull (correct-choice-black-cpu (second dice))]
            (if-not (= pull -1)
              (move pull sec)
              (println "You can't do any moves..."))))
        (swap! available-moves-black dissoc 1)
        (swap! available-moves-black dissoc 2)
        (swap! available-moves-black dissoc :pair))
    (and
      (is-in? (first dice) (free-for :black))
      (not (is-in? (second dice) (free-for :black)))
      (<= 1 (count (@board :waiting-black))))
      (do
        (println "=====================================================")
        (println "Black! They have |"(first dice)"| and |"(second dice)"|")
        (println "They use the FIRST number to pop out their pull! Yeah!")
        (back-in-the-game :black (first dice))
        (println "Well, now they select a pull for their |"(second dice)"|")
        (let [pull (correct-choice-black-cpu (second dice))]
          (if-not (= pull -1)
            (move pull (second dice))
            (println "You can't do any moves...")))
        (swap! available-moves-black dissoc 1)
        (swap! available-moves-black dissoc 2)
        (swap! available-moves-black dissoc :pair))
    (and
      (is-in? (second dice) (free-for :black))
      (not (is-in? (first dice) (free-for :black)))
      (<= 1 (count (@board :waiting-black))))
      (do
        (println "=====================================================")
        (println "Black! They have |"(first dice)"| and |"(second dice)"|")
        (println "They use the SECOND number to pop out their pull! Yeah!")
        (back-in-the-game :black (second dice))
        (println "Well, they select a pull for their |"(first dice)"|")
        (let [pull (correct-choice-black-cpu (first dice))]
          (if-not (= pull -1)
            (move pull (first dice))
            (println "You can't do any moves...")))
        (swap! available-moves-black dissoc 1)
        (swap! available-moves-black dissoc 2)
        (swap! available-moves-black dissoc :pair))
    :else
      (do
        (println "=====================================================")
        (println "Black! They have |"(first dice)"| and |"(second dice)"|")
        (println "A-hi-hi, they can't pop any of their pulls!")
        (swap! available-moves-black dissoc 1)
        (swap! available-moves-black dissoc 2)
        (swap! available-moves-black dissoc :pair))))