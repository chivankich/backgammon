; (defproject backgammon "0.1.0-SNAPSHOT"
;   :description "FIXME: write description"
;   :url "http://example.com/FIXME"
;   :license {:name "Eclipse Public License"
;             :url "http://www.eclipse.org/legal/epl-v10.html"}
;   :dependencies [[org.clojure/clojure "1.4.0"]])

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

(declare turn-black turn-white)

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


(defn black? [pulls]
  (if (= (first (@board pulls)) 1)
    true))

(defn white? [pulls]
  (if (= (first (@board pulls)) 0)
    true))

(defn free-for-white []
  (filter (complement nil?) (for [position (map inc (range 6))]
                              (let [pull (first (@board position))]
                                (if (or (= 0 pull) (nil? pull) (and (= 1 (count (@board position))) (= 1 pull)))
                                  position)))))
(defn free-for-black []
  (filter (complement nil?) (for [position (map inc (range 6))]
                              (let [pull (first (@board (- 25 position)))]
                                (if (or (= 1 pull) (nil? pull) (and (= 1 (count (@board (- 25 position)))) (= 0 pull)))
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
      (do
        (swap! board assoc to new-config-to)
        (swap! board assoc from new-config-from)))
    (or
      (and (white? from) (white? (+ from with)))
      (and (white? from) (empty? (@board (+ from with)))))
    (let [to (+ from with)
          new-config-to (conj (@board to) (first (@board from)))
          new-config-from (pop (@board from))]
      (do
        (swap! board assoc to new-config-to)
        (swap! board assoc from new-config-from)))))

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
          (println "Sorry, but you can't do any moves... (such a loser!)")
          -1)
      (not (is-in? pull (white-available with)))
        (do
          (println "Your choice is not correct! Oh my godness...")
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
          (println "Sorry, but you can't do any moves... (such a loser!)")
          -1)
      (not (is-in? pull (black-available with)))
        (do
          (println "Your choice is not correct! Apffff....")
          (correct-choice-black with))
      :else
        pull)))

(defn roll-dice []
  (let [dice1 (+ (rand-int 6) 1)
        dice2 (+ (rand-int 6) 1)]
    [dice1 dice2]))

(defn out-white-with [number]
  (if (black? number)
    (let [to (@board number)
          new-configure-to (vec [0])
          new-configure-from (pop (@board :waiting-white))
          new-configure-to-black (conj (@board :waiting-black) (first to))]
      (do
        (swap! board assoc number new-configure-to)
        (swap! board assoc :waiting-white new-configure-from)
        (swap! board assoc :waiting-black new-configure-to-black)))
  (let [new-configure-to (conj (@board number) 0)
        new-configure-from (pop (@board :waiting-white))]
    (do
      (swap! board assoc number new-configure-to)
      (swap! board assoc :waiting-white new-configure-from)))))

(defn out-black-with [number]
  (if (white? (- 25 number))
    (let [to (@board (- 25 number))
          new-configure-to (vec [1])
          new-configure-from (pop (@board :waiting-black))
          new-configure-to-white (conj (@board :waiting-white) (first to))]
      (do
        (swap! board assoc (- 25 number) new-configure-to)
        (swap! board assoc :waiting-black new-configure-from)
        (swap! board assoc :waiting-white new-configure-to-white)))
  (let [new-configure-to (conj (@board (- 25 number)) 1)
        new-configure-from (pop (@board :waiting-black))]
    (do
      (swap! board assoc (- 25 number) new-configure-to)
      (swap! board assoc :waiting-black new-configure-from)))))

(defn move-empty-waiting-white [dice]
  (println "=====================================================")
  (println "White! You have " (first dice) "and " (second dice))
  (println "Please, select a pull for the first dice, man: ")
  (let [pull (correct-choice-white (first dice))]
    (if-not (= pull -1)
      (if (all-white-prepared?)
        (try-pulling-out-white pull (first dice))
        (move pull (first dice)))
      (println "You can't do any moves... (loser!)")))
  (println "And now select a pull for the second dice, dude: ")
  (let [pull (correct-choice-white (second dice))]
    (if-not (= pull -1)
      (if (all-white-prepared?)
        (try-pulling-out-white pull (second dice))
        (move pull (second dice)))
      (println "You can't do any moves... (loser!)"))))

(defn move-empty-waiting-black [dice]
  (println "=====================================================")
  (println "Black! You have " (first dice) "and " (second dice))
  (println "Please, select a pull for the first dice, man: ")
  (let [pull (correct-choice-black (first dice))]
    (if-not (= pull -1)
      (if (all-black-prepared?)
        (try-pulling-out-black pull (first dice))
        (move pull (first dice)))
      (println "You can't do any moves... (loser!)")))
  (println "And now select a pull for the second dice, dude: ")
  (let [pull (correct-choice-black (second dice))]
    (if-not (= pull -1)
      (if (all-black-prepared?)
        (try-pulling-out-black pull (second dice))
        (move pull (second dice)))
      (println "You can't do any moves... (loser!)"))))

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
          (println "White! You have " (first dice) "and " (second dice))
          (println "Well, you have to use both numbers to pop pulls! Let's do it!")
          (out-white-with (first dice))
          (out-white-with (second dice)))
    (and (is-in? (first dice) (free-for-white)) (= 1 (count (@board :waiting-white))))
      (do
        (println "=====================================================")
        (println "White! You have " (first dice) "and " (second dice))
        (println "You use the first number to pop your pull! Yeah!")
        (out-white-with (first dice))
        (println "Well, select a pull for your fuckin' " (second dice) "(Second number)")
        (let [pull (correct-choice-white (second dice))]
          (if-not (= pull -1)
            (move pull (second dice)))))
    (and (is-in? (second dice) (free-for-white)) (= 1 (count (@board :waiting-white))))
      (do
        (println "=====================================================")
        (println "White! You have " (first dice) "and " (second dice))
        (println "You use the second number to pop out your pull! Yeah!")
        (out-white-with (second dice))
        (println "Well, select a pull for your " (first dice) "(First number)")
        (let [pull (correct-choice-white (first dice))]
          (if-not (= pull -1)
            (move pull (first dice)))))
    :else
      (do
        (println "=====================================================")
        (println "White! You have " (first dice) "and " (second dice))
        (println "Sorry, but you can't pop any of your fuckin' pulls! :("))))

(defn pair? [dice]
  (= (first dice) (second dice)))

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
          (println "Blacks! You have " (first dice) "and " (second dice))
          (println "Well, you have to use both numbers to pop pulls! Let's do it!")
          (out-black-with (first dice))
          (out-black-with (second dice)))
    (and (is-in? (first dice) (free-for-black)) (= 1 (count (@board :waiting-black))))
      (do
        (println "=====================================================")
        (println "Blacks! You have " (first dice) "and " (second dice))
        (println "You use the first number to pop your pull! Yeah!")
        (out-black-with (first dice))
        (println "Well, select a pull for your " (second dice) "(Second number)")
        (let [pull (correct-choice-black (second dice))]
          (if-not (= pull -1)
            (move pull (second dice)))))
    (and (is-in? (second dice) (free-for-black)) (= 1 (count (@board :waiting-black))))
      (do
        (println "=====================================================")
        (println "Blacks! You have " (first dice) "and " (second dice))
        (println "You use the second number to pop out your pull! Yeah!")
        (out-black-with (second dice))
        (println "Well, select a pull for your " (first dice) "(First number)")
        (let [pull (correct-choice-black (first dice))]
          (if-not (= pull -1)
            (move pull (first dice)))))
    :else
      (do
        (println "=====================================================")
        (println "Blacks! You have " (first dice) "and " (second dice))
        (println "Sorry, but you can't pop any of your fuckin' pulls! :("))))

(defn turn-white []
  (let [dice (roll-dice)]
    (cond
      (and
        (empty? (@board :waiting-white))
        (pair? dice))
      (do
        (show)
        (dotimes [_ 2]
          (move-empty-waiting-white dice))
        (if (white-win?)
          (println "Congratulations, Whites! You are the fuckin' winner! :)")
          (turn-black)))
      (and
        (empty? (@board :waiting-white))
        (not (pair? dice)))
      (do
        (show)
        (move-empty-waiting-white dice)
        (if (white-win?)
          (println "Congratulations, Whites! You are the fuckin' winner! :)")
          (turn-black)))
      (and
        (not (empty? (@board :waiting-white)))
        (pair? dice))
      (do
        (show)
        (dotimes [_ 2]
          (white-out dice))
        (if (white-win?)
          (println "Congratulations, Whites! You are the fuckin' winner! :)")
          (turn-black)))
      (and
        (not (empty? (@board :waiting-white)))
        (not (pair? dice)))
      (do
        (show)
        (white-out dice)
        (if (white-win?)
            (println "Congratulations, Whites! You are the fuckin' winner! :)")
            (turn-black))))))

(defn turn-black []
  (let [dice (roll-dice)]
    (cond
      (and
        (empty? (@board :waiting-black))
        (pair? dice))
      (do
        (show)
        (dotimes [_ 2]
          (move-empty-waiting-black dice))
        (if (black-win?)
          (println "Congratulations, Blacks! You are the fuckin' winner! :)")
          (turn-white)))
      (and
        (empty? (@board :waiting-black))
        (not (pair? dice)))
      (do
        (show)
        (move-empty-waiting-black dice)
        (if (black-win?)
          (println "Congratulations, Blacks! You are the fuckin' winner! :)")
          (turn-white)))
      (and
        (not (empty? (@board :waiting-black)))
        (pair? dice))
      (do
        (show)
        (dotimes [_ 2]
          (black-out dice))
        (if (black-win?)
          (println "Congratulations, Blacks! You are the fuckin' winner! :)")
          (turn-white)))
      (and
        (not (empty? (@board :waiting-black)))
        (not (pair? dice)))
      (do
        (show)
        (black-out dice)
        (if (black-win?)
            (println "Congratulations, Blacks! You are the fuckin' winner! :)")
            (turn-white))))))

(turn-white)