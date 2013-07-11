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

(defn free-for [color]
  (if (= color :white)
    (filter (complement nil?) (for [position (map inc (range 6))]
                                (let [pull (first (@board position))]
                                  (if (or
                                        (= 0 pull)
                                        (nil? pull)
                                        (and (= 1 (count (@board position))) (= 1 pull)))
                                    position))))
    (filter (complement nil?) (for [position (map inc (range 6))]
                                (let [pull (first (@board (- 25 position)))]
                                  (if (or
                                        (= 1 pull)
                                        (nil? pull)
                                        (and (= 1 (count (@board (- 25 position)))) (= 0 pull)))
                                    (prn position)))))))

(prn (free-for :black))

(defn is-in? [number coll]
  (if (empty? coll)
    false
      (reduce #(or %1 %2) (for [element coll]
                            (if (= number element)
                              true)))))

(defn all-prepared? [color]
  (if (= color :white)
    (and (reduce #(and %1 %2) (for [position (range 1 19)]
                                (if (white? position)
                                  false true)))
          (empty? (@board :waiting-white)))
    (and (reduce #(and %1 %2) (for [position (range 7 25)]
                                (if (black? position)
                                  false true)))
          (empty? (@board :waiting-black)))))

(defn is-there-behind? [color from]
  (if (= color :white)
    (if (<= from 19)
      false
        (reduce #(or %1 %2) (for [pull (range 1 from)]
                              (if (or
                                    (empty? (@board pull))
                                    (black? pull))
                                false true))))
    (if (>= from 6)
      false
        (reduce #(or %1 %2) (for [pull (range (inc from) 7)]
                              (if (or
                                    (empty? (@board pull))
                                    (white? pull))
                                false true))))))

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

(defn try-pulling-out [color from with]
  (if (= color :white)
    (let [to (+ from with)]
      (cond
        (= 25 to)
          (swap! board assoc from (pop (@board from)))
        (> 25 to)
          (move from with)
        (< 25 to)
          (if-not (is-there-behind? :white from)
            (swap! board assoc from (pop (@board from))))))
    (let [to (- from with)]
      (cond
        (= 0 to)
          (swap! board assoc from (pop (@board from)))
        (< 0 to)
          (move from with)
        (> 0 to)
          (if-not (is-there-behind? :black from)
            (swap! board assoc from (pop (@board from))))))))

(defn win? [color]
  (if (= color :white)
    (let [amount1 (apply + (for [pulls (range 1 25)]
                            (if (white? pulls)
                              (count (@board pulls))
                              0)))
          amount2 (count (@board :waiting-white))
          real-amount (+ amount1 amount2)]
      (if (= 0 real-amount)
        true
        false))
    (let [amount1 (apply + (for [pulls (range 1 25)]
                            (if (black? pulls)
                              (count (@board pulls))
                              0)))
          amount2 (count (@board :waiting-black))
          real-amount (+ amount1 amount2)]
      (if (= 0 real-amount)
        true
        false))))

(defn wall? [color from with]
  (if (= color :white)
    (let [to (+ from with)]
      (if (and
            (black? to)
            (<= 2 (count (@board to))))
        true false))
    (let [to (- from with)]
      (if (and
            (white? to)
            (<= 2 (count (@board to))))
        true false))))

(defn available [color with]
  (if (= color :white)
    (if-not (all-prepared? :white)
      (filter (complement nil?) (for [position (map inc (range 24))]
                                  (if (and
                                        (white? position)
                                        (not (wall? :white position with))
                                        (< (+ position with) 25))
                                    position)))
      (filter (complement nil?) (for [position (range 19 25)]
                                  (if (and
                                        (white? position)
                                        (not (wall? :white position with))
                                        (not (and
                                              (> (+ position with) 25)
                                              (is-there-behind? :white position))))
                                    position
                                    (if (and
                                          (white? position)
                                          (not (wall? :white position with))
                                          (and
                                            (> (+ position with) 25)
                                            (not (is-there-behind? :white position))))
                                      position)))))
    (if-not (all-prepared? :black)
      (filter (complement nil?) (for [position (map inc (range 24))]
                                  (if (and
                                        (black? position)
                                        (not (wall? :black position with))
                                        (> (- position with) 0))
                                    position)))
      (filter (complement nil?) (for [position (range 1 7)]
                                  (if (and
                                        (black? position)
                                        (not (wall? :black position with))
                                        (not (and
                                              (< (- position with) 0)
                                              (is-there-behind? :black position))))
                                    position
                                    (if (and
                                          (black? position)
                                          (not (wall? :black position with))
                                          (and
                                            (< (- position with) 0)
                                            (not (is-there-behind? :black position))))
                                      position)))))))

(defn correct-choice [color with]
  (if (= color :white)
    (let [pull (read)]
      (cond
        (empty? (available :white with))
          (do
            (println "Sorry, but you can't do any moves...")
            -1)
        (not (is-in? pull (available :white with)))
          (do
            (println "!!! Your choice is not correct! !!!")
            (correct-choice :white with))
        :else
          pull))
    (let [pull (read)]
      (cond
        (empty? (available :black with))
          (do
            (println "Sorry, but you can't do any moves...")
            -1)
        (not (is-in? pull (available :black with)))
          (do
            (println "!!! Your choice is not correct! !!!")
            (correct-choice :black with))
        :else
          pull))))

(defn back-in-the-game [color number]
  (if (= color :white)
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
      (swap! board assoc :waiting-white new-configure-from)))
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
      (swap! board assoc :waiting-black new-configure-from)))))

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

(defn pair? [dice]
  (= (first dice) (second dice)))

(defn correct-choice-between []
  (let [choice (read)]
    (if-not (or (= choice 1) (= choice 2))
      (do
        (println "!!! Please, choose between 1 and 2! Try again...!!!")
        (correct-choice-between))
      choice)))

;====================================================================================================

(def possible (atom {}))

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

(def versus-cpu? (atom false))

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
          (let [pull (correct-choice-black-cpu)]
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
        (let [pull (correct-choice-black-cpu)]
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
        (let [pull (correct-choice-black-cpu)]
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

        ;=================================================================================================

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

(defn question []
  (println "=====================================================")
  (println "Do you want to play against CPU? (0 / 1)")
  (let [q (read)]
    (if (= 1 q)
      (swap! versus-cpu? (fn [_] true))
      (if-not (= 0 q)
        (do
          (println "!!! Your choice is not correct! !!!")
          (question))))))

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