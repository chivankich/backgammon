(declare turn-black turn-white)

(def board (atom {1 [0, 0]
                  2 []
                  3 []
                  4 []
                  5 []
                  6 [1, 1, 1, 1, 1]
                  7 []
                  8 [1, 1, 1]
                  9 []
                  10 []
                  11 []
                  12 [0, 0, 0, 0, 0]
                  13 [1, 1, 1, 1, 1]
                  14 []
                  15 []
                  16 []
                  17 [0, 0, 0]
                  18 []
                  19 [0, 0, 0, 0, 0]
                  20 []
                  21 []
                  22 []
                  23 []
                  24 [1, 1]
                  :waiting-black []
                  :waiting-white []}))

(def available-moves-white (atom {}))

(def available-moves-black (atom {}))

(defn calculate [coll1 coll2]
  (let [value1 (cond
                (= (count coll1) 0)
                  0
                (= (count coll1) 1)
                  1
                :else
                  (+ (count coll1) (- (count coll1) 1)))
        value2 (cond
                (= (count coll2) 0)
                  0
                (= (count coll2) 1)
                  1
                :else
                  (+ (count coll2) (- (count coll2) 1)))
        value3 (- 47 (+ value1 value2))]
    (clojure.string/join (repeat value3 " "))))

(defn show []
  (println)
  (println "*************************|BACKGAMMON|************************")
  (println "-------------------------------------------------------------")
  (println "24 " (@board 24) (calculate (@board 24) (@board 1)) (@board 1) " 1")
  (println "23 " (@board 23) (calculate (@board 23) (@board 2)) (@board 2) " 2")
  (println "22 " (@board 22) (calculate (@board 22) (@board 3)) (@board 3) " 3")
  (println "21 " (@board 21) (calculate (@board 21) (@board 4)) (@board 4) " 4")
  (println "20 " (@board 20) (calculate (@board 20) (@board 5)) (@board 5) " 5")
  (println "19 " (@board 19) (calculate (@board 19) (@board 6)) (@board 6) " 6")
  (println "-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -")
  (println "18 " (@board 18) (calculate (@board 18) (@board 7)) (@board 7) " 7")
  (println "17 " (@board 17) (calculate (@board 17) (@board 8)) (@board 8) " 8")
  (println "16 " (@board 16) (calculate (@board 16) (@board 9)) (@board 9) " 9")
  (println "15 " (@board 15) (calculate (@board 15) (@board 10)) (@board 10) " 10")
  (println "14 " (@board 14) (calculate (@board 14) (@board 11)) (@board 11) " 11")
  (println "13 " (@board 13) (calculate (@board 13) (@board 12)) (@board 12) " 12")
  (println "-------------------------------------------------------------")
  (println "*************************************************************")
  (println "White out: " (@board :waiting-white))
  (println "Black out: " (@board :waiting-black)))

(defn roll-dice []
  (let [dice1 (+ (rand-int 6) 1)
        dice2 (+ (rand-int 6) 1)]
    [dice1 dice2]))

(defn black? [pulls]
  (if (= (first (@board pulls)) 1)
    true))

(defn white? [pulls]
  (if (= (first (@board pulls)) 0)
    true))

(defn free-for-white []
  (filter (complement nil?) (for [position (map inc (range 6))]
                              (let [pull (first (@board position))]
                                (if (or (= 0 pull)
                                    (nil? pull)
                                    (and (= 1 (count (@board position))) (= 1 pull)))
                                  position)))))
(defn free-for-black []
  (filter (complement nil?) (for [position (map inc (range 6))]
                              (let [pull (first (@board (- 25 position)))]
                                (if (or (= 1 pull)
                                    (nil? pull)
                                    (and (= 1 (count (@board (- 25 position)))) (= 0 pull)))
                                  position)))))

(defn is-in? [number coll]
  (if (empty? coll)
    false
      (reduce #(or %1 %2) (for [element coll]
                            (if (= number element)
                              true)))))

(defn all-white-prepared? []
  (and (reduce #(and %1 %2) (for [position (range 1 19)]
                              (if (white? position)
                                false true)))
        (empty? (@board :waiting-white))))

(defn all-black-prepared? []
  (and (reduce #(and %1 %2) (for [position (range 7 25)]
                              (if (black? position)
                                false true)))
        (empty? (@board :waiting-black))))

(defn is-there-behind-white? [from]
  (if (<= from 19)
    false
      (reduce #(or %1 %2) (for [pull (range 1 from)]
                            (if (or
                                  (empty? (@board pull))
                                  (black? pull))
                              false true)))))

(defn is-there-behind-black? [from]
  (if (>= from 6)
    false
      (reduce #(or %1 %2) (for [pull (range (inc from) 7)]
                            (if (or
                                  (empty? (@board pull))
                                  (white? pull))
                              false true)))))

(defn move [from with]
  (cond
    (and (black? from)
          (white? (- from with))
          (= 1 (count (@board (- from with)))))
      (let [to (- from with)
            new-config-to (conj [] (first (@board from)))
            new-config-from (pop (@board from))
            new-waiting (conj (@board :waiting-white) (first (@board to)))]
        (swap! board assoc :waiting-white new-waiting)
        (swap! board assoc to new-config-to)
        (swap! board assoc from new-config-from))
    (and (white? from)
          (black? (+ from with))
          (= 1 (count (@board (+ from with)))))
      (let [to (+ from with)
            new-config-to (conj [] (first (@board from)))
            new-config-from (pop (@board from))
            new-waiting (conj (@board :waiting-black) (first (@board to)))]
        (swap! board assoc :waiting-black new-waiting)
        (swap! board assoc to new-config-to)
        (swap! board assoc from new-config-from))
    (or
      (and (black? from) (black? (- from with)))
      (and (black? from) (empty? (@board (- from with)))))
      (let [to (- from with)
            new-config-to (conj (@board to) (first (@board from)))
            new-config-from (pop (@board from))]
        (swap! board assoc to new-config-to)
        (swap! board assoc from new-config-from))
    (or
      (and (white? from) (white? (+ from with)))
      (and (white? from) (empty? (@board (+ from with)))))
    (let [to (+ from with)
          new-config-to (conj (@board to) (first (@board from)))
          new-config-from (pop (@board from))]
      (swap! board assoc to new-config-to)
      (swap! board assoc from new-config-from))))

(defn try-pulling-out-white [from with]
  (let [to (+ from with)]
    (cond
      (= 25 to)
        (swap! board assoc from (pop (@board from)))
      (> 25 to)
        (move from with)
      (< 25 to)
        (if-not (is-there-behind-white? from)
          (swap! board assoc from (pop (@board from)))))))

(defn try-pulling-out-black [from with]
  (let [to (- from with)]
    (cond
      (= 0 to)
        (swap! board assoc from (pop (@board from)))
      (< 0 to)
        (move from with)
      (> 0 to)
        (if-not (is-there-behind-black? from)
          (swap! board assoc from (pop (@board from)))))))

(defn white-win? []
  (let [amount1 (apply + (for [pulls (range 1 25)]
                          (if (white? pulls)
                            (count (@board pulls))
                            0)))
        amount2 (count (@board :waiting-white))
        real-amount (+ amount1 amount2)]
    (if (= 0 real-amount)
      true
      false)))

(defn black-win? []
  (let [amount1 (apply + (for [pulls (range 1 25)]
                          (if (black? pulls)
                            (count (@board pulls))
                            0)))
        amount2 (count (@board :waiting-black))
        real-amount (+ amount1 amount2)]
    (if (= 0 real-amount)
      true
      false)))

(defn black-wall? [from with]
  (let [to (+ from with)]
    (if (and
          (black? to)
          (<= 2 (count (@board to))))
      true false)))

(defn white-wall? [from with]
  (let [to (- from with)]
    (if (and
          (white? to)
          (<= 2 (count (@board to))))
      true false)))

(defn white-available [with]
  (if-not (all-white-prepared?)
    (filter (complement nil?) (for [position (map inc (range 24))]
                                (if (and
                                      (white? position)
                                      (not (black-wall? position with))
                                      (< (+ position with) 25))
                                  position)))
    (filter (complement nil?) (for [position (range 19 25)]
                                (if (and
                                      (white? position)
                                      (not (black-wall? position with))
                                      (not (and
                                            (> (+ position with) 25)
                                            (is-there-behind-white? position))))
                                  position
                                  (if (and
                                        (white? position)
                                        (not (black-wall? position with))
                                        (and
                                          (> (+ position with) 25)
                                          (not (is-there-behind-white? position))))
                                    position))))))

(defn correct-choice-white [with]
  (let [pull (read)]
    (cond
      (empty? (white-available with))
        (do
          (println "Sorry, but you can't do any moves...")
          -1)
      (not (is-in? pull (white-available with)))
        (do
          (println "!!! Your choice is not correct! !!!")
          (correct-choice-white with))
      :else
        pull)))

(defn black-available [with]
  (if-not (all-black-prepared?)
    (filter (complement nil?) (for [position (map inc (range 24))]
                                (if (and
                                      (black? position)
                                      (not (white-wall? position with))
                                      (> (- position with) 0))
                                  position)))
    (filter (complement nil?) (for [position (range 1 7)]
                                (if (and
                                      (black? position)
                                      (not (white-wall? position with))
                                      (not (and
                                            (< (- position with) 0)
                                            (is-there-behind-black? position))))
                                  position
                                  (if (and
                                        (black? position)
                                        (not (white-wall? position with))
                                        (and
                                          (< (- position with) 0)
                                          (not (is-there-behind-black? position))))
                                    position))))))

(defn correct-choice-black [with]
  (let [pull (read)]
    (cond
      (empty? (black-available with))
        (do
          (println "Sorry, but you can't do any moves...")
          -1)
      (not (is-in? pull (black-available with)))
        (do
          (println "!!! Your choice is not correct! !!!")
          (correct-choice-black with))
      :else
        pull)))

(defn back-in-the-game-white [number]
  (if (black? number)
    (let [to (@board number)
          new-configure-to (vec [0])
          new-configure-from (pop (@board :waiting-white))
          new-configure-to-black (conj (@board :waiting-black) (first to))]
      (swap! board assoc number new-configure-to)
      (swap! board assoc :waiting-white new-configure-from)
      (swap! board assoc :waiting-black new-configure-to-black))
  (let [new-configure-to (conj (@board number) 0)
        new-configure-from (pop (@board :waiting-white))]
    (swap! board assoc number new-configure-to)
    (swap! board assoc :waiting-white new-configure-from))))

(defn back-in-the-game-black [number]
  (if (white? (- 25 number))
    (let [to (@board (- 25 number))
          new-configure-to (vec [1])
          new-configure-from (pop (@board :waiting-black))
          new-configure-to-white (conj (@board :waiting-white) (first to))]
      (swap! board assoc (- 25 number) new-configure-to)
      (swap! board assoc :waiting-black new-configure-from)
      (swap! board assoc :waiting-white new-configure-to-white))
  (let [new-configure-to (conj (@board (- 25 number)) 1)
        new-configure-from (pop (@board :waiting-black))]
    (swap! board assoc (- 25 number) new-configure-to)
    (swap! board assoc :waiting-black new-configure-from))))

(defn black-win-check []
  (if (black-win?)
    (do
      (println "=====================================================")
      (println "|   Congratulations, Black! You are the winner! :)  |")
      (println "====================================================="))
    (turn-white)))

(defn white-win-check []
  (if (white-win?)
    (do
      (println "=====================================================")
      (println "|   Congratulations, White! You are the winner! :)  |")
      (println "====================================================="))
    (turn-black)))

(defn pair? [dice]
  (= (first dice) (second dice)))

(defn correct-choice []
  (let [choice (read)]
    (if-not (or (= choice 1) (= choice 2))
      (do
        (println "!!! Please, choose between 1 and 2! Try again...!!!")
        (correct-choice))
      choice)))

(defn move-empty-waiting-white [dice]
  (println "=====================================================")
  (println "White! You have |"(first dice)"| and |"(second dice)"|")
  (println "Please, type 1 for |"(first dice)"| and 2 for |"(second dice)"| ...")
  (let [choice1 (correct-choice)
        number (@available-moves-white choice1)]
    (println "And now select a pull for the |"number"|")
    (let [pull (correct-choice-white number)]
      (if-not (= pull -1)
        (if (all-white-prepared?)
          (try-pulling-out-white pull number)
          (move pull number))
        (println "You can't do any moves...")))
    (if-not (@available-moves-white :pair)
      (swap! available-moves-white dissoc choice1)))
  (let [choice2 (first (vals @available-moves-white))]
    (println "And now select a pull for the second dice, that's" choice2)
    (let [pull (correct-choice-white choice2)]
      (if-not (= pull -1)
        (if (all-white-prepared?)
          (try-pulling-out-white pull choice2)
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
  (let [choice1 (correct-choice)
        number (@available-moves-black choice1)]
    (println "And now select a pull for the |"number"|")
    (let [pull (correct-choice-black number)]
      (if-not (= pull -1)
        (if (all-black-prepared?)
          (try-pulling-out-black pull number)
          (move pull number))
        (println "You can't do any moves...")))
    (if-not (@available-moves-black :pair)
      (swap! available-moves-black dissoc choice1)))
  (let [choice2 (first (vals @available-moves-black))]
    (println "And now select a pull for the second dice, that's |"choice2"|")
    (let [pull (correct-choice-black choice2)]
      (if-not (= pull -1)
        (if (all-black-prepared?)
          (try-pulling-out-black pull choice2)
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
      (is-in? (first dice) (free-for-white))
      (is-in? (second dice) (free-for-white))
      (< 1 (count (@board :waiting-white))))
      (do
        (println "=====================================================")
        (println "White! You have |"(first dice)"| and |"(second dice)"|")
        (println "Well, you have to use both numbers to pop pulls! Let's do it!")
        (println "Choose 1 or 2 to pop out one of the frozen pulls!")
        (let [choice1 (correct-choice)
              number1 (@available-moves-white choice1)]
          (back-in-the-game-white number1)
          (if-not (@available-moves-white :pair)
            (swap! available-moves-white dissoc choice1)))
        (println "Now, choose a pull for your second frozen pull!")
        (let [number2 (first (vals (@available-moves-white)))]
          (back-in-the-game-white number2)
          (if-not (@available-moves-white :pair)
            (swap! available-moves-white dissoc (first (keys (@available-moves-white))))))
        (swap! available-moves-white dissoc :pair))
    (and
      (is-in? (first dice) (free-for-white))
      (is-in? (second dice) (free-for-white))
      (= 1 (count (@board :waiting-white))))
      (do
        (println "=====================================================")
        (println "White! You have |"(first dice)"| and |"(second dice)"|, choose 1 or 2 to pop out the pull!")
        (let [choice1 (correct-choice)
              number1 (@available-moves-white choice1)]
          (back-in-the-game-white number1)
          (if-not (@available-moves-white :pair)
            (swap! available-moves-white dissoc choice1)))
        (let [choice2 (first (vals @available-moves-white))]
          (println "Well, select a pull for your |"choice2"|")
          (let [pull (correct-choice-white choice2)]
            (if-not (= pull -1)
              (move pull choice2)
              (println "You can't do any moves...")))
          (if-not (@available-moves-white :pair)
            (swap! available-moves-white dissoc (first (keys @available-moves-white))))
          (swap! available-moves-white dissoc :pair)))
    (and
      (is-in? (first dice) (free-for-white))
      (not (is-in? (second dice) (free-for-white)))
      (<= 1 (count (@board :waiting-white))))
      (do
        (println "=====================================================")
        (println "White! You have |"(first dice)"| and |"(second dice)"|")
        (println "You use the FIRST number to pop out your pull! Yeah!")
        (back-in-the-game-white (first dice))
        (println "Well, select a pull for your |"(second dice)"|")
        (let [pull (correct-choice-white (second dice))]
          (if-not (= pull -1)
            (move pull (second dice))
            (println "You can't do any moves...")))
        (if-not (@available-moves-white :pair)
          (do
            (swap! available-moves-white dissoc 1)
            (swap! available-moves-white dissoc 2)))
        (swap! available-moves-white dissoc :pair))
    (and
      (is-in? (second dice) (free-for-white))
      (not (is-in? (first dice) (free-for-white)))
      (<= 1 (count (@board :waiting-white))))
      (do
        (println "=====================================================")
        (println "White! You have |"(first dice)"| and |"(second dice)"|")
        (println "You use the SECOND number to pop out your pull! Yeah!")
        (back-in-the-game-white (second dice))
        (println "Well, select a pull for your |"(first dice)"|")
        (let [pull (correct-choice-white (first dice))]
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
      (is-in? (first dice) (free-for-black))
      (is-in? (second dice) (free-for-black))
      (< 1 (count (@board :waiting-black))))
      (do
        (println "=====================================================")
        (println "Black! You have |"(first dice)"| and |"(second dice)"|")
        (println "Well, you have to use both numbers to pop pulls! Let's do it!")
        (println "Choose 1 or 2 to pop out one of the frozen pulls!")
        (let [choice1 (correct-choice)
              number1 (@available-moves-black choice1)]
          (back-in-the-game-black number1)
          (if-not (@available-moves-black :pair)
            (swap! available-moves-black dissoc choice1)))
        (println "Now, choose a pull for your second frozen pull!")
        (let [number2 (first (vals (@available-moves-black)))]
          (back-in-the-game-black number2)
          (if-not (@available-moves-black :pair)
            (swap! available-moves-black dissoc (first (keys (@available-moves-black))))))
        (swap! available-moves-black dissoc :pair))
    (and
      (is-in? (first dice) (free-for-black))
      (is-in? (second dice) (free-for-black))
      (= 1 (count (@board :waiting-black))))
      (do
        (println "=====================================================")
        (println "Black! You have |"(first dice)"| and |"(second dice)"|, choose 1 or 2 to pop out the pull!")
        (let [choice1 (correct-choice)
              number1 (@available-moves-black choice1)]
          (back-in-the-game-black number1)
          (if-not (@available-moves-black :pair)
            (swap! available-moves-black dissoc choice1)))
        (let [choice2 (first (vals @available-moves-black))]
          (println "Well, select a pull for your |"choice2"|")
          (let [pull (correct-choice-black choice2)]
            (if-not (= pull -1)
              (move pull choice2)
              (println "You can't do any moves...")))
          (if-not (@available-moves-black :pair)
            (swap! available-moves-black dissoc (first (keys @available-moves-black))))
          (swap! available-moves-black dissoc :pair)))
    (and
      (is-in? (first dice) (free-for-black))
      (not (is-in? (second dice) (free-for-black)))
      (<= 1 (count (@board :waiting-black))))
      (do
        (println "=====================================================")
        (println "Black! You have |"(first dice)"| and |"(second dice)"|")
        (println "You use the FIRST number to pop out your pull! Yeah!")
        (back-in-the-game-black (first dice))
        (println "Well, select a pull for your |"(second dice)"|")
        (let [pull (correct-choice-black (second dice))]
          (if-not (= pull -1)
            (move pull (second dice))
            (println "You can't do any moves...")))
        (if-not (@available-moves-black :pair)
          (do
            (swap! available-moves-black dissoc 1)
            (swap! available-moves-black dissoc 2)))
        (swap! available-moves-black dissoc :pair))
    (and
      (is-in? (second dice) (free-for-black))
      (not (is-in? (first dice) (free-for-black)))
      (<= 1 (count (@board :waiting-black))))
      (do
        (println "=====================================================")
        (println "Black! You have |"(first dice)"| and |"(second dice)"|")
        (println "You use the SECOND number to pop out your pull! Yeah!")
        (back-in-the-game-black (second dice))
        (println "Well, select a pull for your |"(first dice)"|")
        (let [pull (correct-choice-black (first dice))]
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
        (white-win-check))
      (and
        (empty? (@board :waiting-white))
        (not (pair? dice)))
      (do
        (show)
        (move-empty-waiting-white dice)
        (white-win-check))
      (and
        (not (empty? (@board :waiting-white)))
        (pair? dice))
      (do
        (show)
        (dotimes [_ 2]
          (white-out dice))
        (white-win-check))
      (and
        (not (empty? (@board :waiting-white)))
        (not (pair? dice)))
      (do
        (show)
        (white-out dice)
        (white-win-check)))))

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
          (move-empty-waiting-black dice))
        (black-win-check))
      (and
        (empty? (@board :waiting-black))
        (not (pair? dice)))
      (do
        (show)
        (move-empty-waiting-black dice)
        (black-win-check))
      (and
        (not (empty? (@board :waiting-black)))
        (pair? dice))
      (do
        (show)
        (dotimes [_ 2]
          (black-out dice))
        (black-win-check))
      (and
        (not (empty? (@board :waiting-black)))
        (not (pair? dice)))
      (do
        (show)
        (black-out dice)
        (black-win-check)))))

(turn-white)