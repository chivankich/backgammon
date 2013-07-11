(ns backgammon.io
  (:gen-class)
  (:use backgammon.init_game)
  (:use backgammon.generic_methods)
  (:use backgammon.move)
  (:use backgammon.choices))

(defn move-empty-waiting-white [dice]
  (println "=====================================================")
  (println "White! You have |"(first dice)"| and |"(second dice)"|")
  (println "Please, type 1 for |"(first dice)"| and 2 for |"(second dice)"| ...")
  (let [choice1 (correct-choice-between)
        number (@available-moves-white choice1)]
    (println "And now select a pull for the |"number"|")
    (let [pull (correct-choice :white number)]
      (if-not (= pull -1)
        (if (all-prepared? :white)
          (try-pulling-out :white pull number)
          (move pull number))
        (println "You can't do any moves...")))
    (if-not (@available-moves-white :pair)
      (swap! available-moves-white dissoc choice1)))
  (let [choice2 (first (vals @available-moves-white))]
    (println "And now select a pull for the second dice, that's" choice2)
    (let [pull (correct-choice :white choice2)]
      (if-not (= pull -1)
        (if (all-prepared? :white)
          (try-pulling-out :white pull choice2)
          (move pull choice2))
        (println "You can't do any moves..."))))
  (if-not (@available-moves-white :pair)
    (swap! available-moves-white dissoc (first (keys @available-moves-white))))
  (swap! available-moves-white dissoc :pair))

(defn move-empty-waiting-black [dice]
  (println "=====================================================")
  (println "Black! You have |"(first dice)"| and |"(second dice)"|")
  (swap! available-moves-black assoc 1 (first dice))
  (swap! available-moves-black assoc 2 (second dice))
  (println "Please, type 1 for |"(first dice)"| and 2 for |"(second dice)"| ...")
  (let [choice1 (correct-choice-between)
        number (@available-moves-black choice1)]
    (println "And now select a pull for the |"number"|")
    (let [pull (correct-choice :black number)]
      (if-not (= pull -1)
        (if (all-prepared? :black)
          (try-pulling-out :black pull number)
          (move pull number))
        (println "You can't do any moves...")))
    (if-not (@available-moves-black :pair)
      (swap! available-moves-black dissoc choice1)))
  (let [choice2 (first (vals @available-moves-black))]
    (println "And now select a pull for the second dice, that's |"choice2"|")
    (let [pull (correct-choice :black choice2)]
      (if-not (= pull -1)
        (if (all-prepared? :black)
          (try-pulling-out :black pull choice2)
          (move pull choice2))
        (println "You can't do any moves..."))))
  (if-not (@available-moves-black :pair)
    (swap! available-moves-black dissoc (first (keys @available-moves-white))))
  (swap! available-moves-black dissoc :pair))

(defn white-out [dice]
  (cond
    (empty? (@board :waiting-white))
      (move-empty-waiting-white dice)
    (and
      (is-in? (first dice) (free-for :white))
      (is-in? (second dice) (free-for :white))
      (< 1 (count (@board :waiting-white))))
      (do
        (println "=====================================================")
        (println "White! You have |"(first dice)"| and |"(second dice)"|")
        (println "Well, you have to use both numbers to pop pulls! Let's do it!")
        (println "Choose 1 or 2 to pop out one of the frozen pulls!")
        (let [choice1 (correct-choice-between)
              number1 (@available-moves-white choice1)]
          (back-in-the-game :white number1)
          (if-not (@available-moves-white :pair)
            (swap! available-moves-white dissoc choice1)))
        (println "Now, choose a pull for your second frozen pull!")
        (let [number2 (first (vals (@available-moves-white)))]
          (back-in-the-game :white number2)
          (if-not (@available-moves-white :pair)
            (swap! available-moves-white dissoc (first (keys (@available-moves-white))))))
        (swap! available-moves-white dissoc :pair))
    (and
      (is-in? (first dice) (free-for :white))
      (is-in? (second dice) (free-for :white))
      (= 1 (count (@board :waiting-white))))
      (do
        (println "=====================================================")
        (println "White! You have |"(first dice)"| and |"(second dice)"|, choose 1 or 2 to pop out the pull!")
        (let [choice1 (correct-choice-between)
              number1 (@available-moves-white choice1)]
          (back-in-the-game :white number1)
          (if-not (@available-moves-white :pair)
            (swap! available-moves-white dissoc choice1)))
        (let [choice2 (first (vals @available-moves-white))]
          (println "Well, select a pull for your |"choice2"|")
          (let [pull (correct-choice :white choice2)]
            (if-not (= pull -1)
              (move pull choice2)
              (println "You can't do any moves...")))
          (if-not (@available-moves-white :pair)
            (swap! available-moves-white dissoc (first (keys @available-moves-white))))
          (swap! available-moves-white dissoc :pair)))
    (and
      (is-in? (first dice) (free-for :white))
      (not (is-in? (second dice) (free-for :white)))
      (<= 1 (count (@board :waiting-white))))
      (do
        (println "=====================================================")
        (println "White! You have |"(first dice)"| and |"(second dice)"|")
        (println "You use the FIRST number to pop out your pull! Yeah!")
        (back-in-the-game :white (first dice))
        (println "Well, select a pull for your |"(second dice)"|")
        (let [pull (correct-choice :white (second dice))]
          (if-not (= pull -1)
            (move pull (second dice))
            (println "You can't do any moves...")))
        (if-not (@available-moves-white :pair)
          (do
            (swap! available-moves-white dissoc 1)
            (swap! available-moves-white dissoc 2)))
        (swap! available-moves-white dissoc :pair))
    (and
      (is-in? (second dice) (free-for :white))
      (not (is-in? (first dice) (free-for :white)))
      (<= 1 (count (@board :waiting-white))))
      (do
        (println "=====================================================")
        (println "White! You have |"(first dice)"| and |"(second dice)"|")
        (println "You use the SECOND number to pop out your pull! Yeah!")
        (back-in-the-game :white (second dice))
        (println "Well, select a pull for your |"(first dice)"|")
        (let [pull (correct-choice :white (first dice))]
          (if-not (= pull -1)
            (move pull (first dice))
            (println "You can't do any moves...")))
        (if-not (@available-moves-white :pair)
          (do
            (swap! available-moves-white dissoc 1)
            (swap! available-moves-white dissoc 2)))
        (swap! available-moves-white dissoc :pair))
    :else
      (do
        (println "=====================================================")
        (println "White! You have |"(first dice)"| and |"(second dice)"|")
        (println "Sorry, but you can't pop any of your pulls! :(")
        (if-not (@available-moves-white :pair)
          (do
            (swap! available-moves-white dissoc 1)
            (swap! available-moves-white dissoc 2)))
        (swap! available-moves-white dissoc :pair))))

(defn black-out [dice]
  (cond
    (empty? (@board :waiting-black))
      (move-empty-waiting-black dice)
    (and
      (is-in? (first dice) (free-for :black))
      (is-in? (second dice) (free-for :black))
      (< 1 (count (@board :waiting-black))))
      (do
        (println "=====================================================")
        (println "Black! You have |"(first dice)"| and |"(second dice)"|")
        (println "Well, you have to use both numbers to pop pulls! Let's do it!")
        (println "Choose 1 or 2 to pop out one of the frozen pulls!")
        (let [choice1 (correct-choice-between)
              number1 (@available-moves-black choice1)]
          (back-in-the-game :black number1)
          (if-not (@available-moves-black :pair)
            (swap! available-moves-black dissoc choice1)))
        (println "Now, choose a pull for your second frozen pull!")
        (let [number2 (first (vals (@available-moves-black)))]
          (back-in-the-game :black number2)
          (if-not (@available-moves-black :pair)
            (swap! available-moves-black dissoc (first (keys (@available-moves-black))))))
        (swap! available-moves-black dissoc :pair))
    (and
      (is-in? (first dice) (free-for :black))
      (is-in? (second dice) (free-for :black))
      (= 1 (count (@board :waiting-black))))
      (do
        (println "=====================================================")
        (println "Black! You have |"(first dice)"| and |"(second dice)"|, choose 1 or 2 to pop out the pull!")
        (let [choice1 (correct-choice-between)
              number1 (@available-moves-black choice1)]
          (back-in-the-game :black number1)
          (if-not (@available-moves-black :pair)
            (swap! available-moves-black dissoc choice1)))
        (let [choice2 (first (vals @available-moves-black))]
          (println "Well, select a pull for your |"choice2"|")
          (let [pull (correct-choice :black choice2)]
            (if-not (= pull -1)
              (move pull choice2)
              (println "You can't do any moves...")))
          (if-not (@available-moves-black :pair)
            (swap! available-moves-black dissoc (first (keys @available-moves-black))))
          (swap! available-moves-black dissoc :pair)))
    (and
      (is-in? (first dice) (free-for :black))
      (not (is-in? (second dice) (free-for :black)))
      (<= 1 (count (@board :waiting-black))))
      (do
        (println "=====================================================")
        (println "Black! You have |"(first dice)"| and |"(second dice)"|")
        (println "You use the FIRST number to pop out your pull! Yeah!")
        (back-in-the-game :black (first dice))
        (println "Well, select a pull for your |"(second dice)"|")
        (let [pull (correct-choice :black (second dice))]
          (if-not (= pull -1)
            (move pull (second dice))
            (println "You can't do any moves...")))
        (if-not (@available-moves-black :pair)
          (do
            (swap! available-moves-black dissoc 1)
            (swap! available-moves-black dissoc 2)))
        (swap! available-moves-black dissoc :pair))
    (and
      (is-in? (second dice) (free-for :black))
      (not (is-in? (first dice) (free-for :black)))
      (<= 1 (count (@board :waiting-black))))
      (do
        (println "=====================================================")
        (println "Black! You have |"(first dice)"| and |"(second dice)"|")
        (println "You use the SECOND number to pop out your pull! Yeah!")
        (back-in-the-game :black (second dice))
        (println "Well, select a pull for your |"(first dice)"|")
        (let [pull (correct-choice :black (first dice))]
          (if-not (= pull -1)
            (move pull (first dice))
            (println "You can't do any moves...")))
        (if-not (@available-moves-black :pair)
          (do
            (swap! available-moves-black dissoc 1)
            (swap! available-moves-black dissoc 2)))
        (swap! available-moves-black dissoc :pair))
    :else
      (do
        (println "=====================================================")
        (println "Black! You have |"(first dice)"| and |"(second dice)"|")
        (println "Sorry, but you can't pop any of your pulls! :(")
        (if-not (@available-moves-black :pair)
          (do
            (swap! available-moves-black dissoc 1)
            (swap! available-moves-black dissoc 2)))
        (swap! available-moves-black dissoc :pair))))