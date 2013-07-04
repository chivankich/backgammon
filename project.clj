; (defproject backgammon "0.1.0-SNAPSHOT"
;   :description "FIXME: write description"
;   :url "http://example.com/FIXME"
;   :license {:name "Eclipse Public License"
;             :url "http://www.eclipse.org/legal/epl-v10.html"}
;   :dependencies [[org.clojure/clojure "1.4.0"]])

(def board (atom {1 [0, 0]
                  2 []
                  3 []
                  4 []
                  5 [1, 1, 1]
                  6 []
                  7 [1, 1, 1, 1, 1]
                  8 []
                  9 []
                  10 []
                  11 []
                  12 [0, 0, 0, 0, 0]
                  13 [1, 1, 1, 1, 1]
                  14 []
                  15 []
                  16 []
                  17 []
                  18 [0, 0, 0, 0, 0]
                  19 []
                  20 [0, 0, 0]
                  21 []
                  22 []
                  23 []
                  24 [1, 1]
                  :waiting-black []
                  :waiting-white []}))

(declare turn-black turn-white)

(defn black? [pulls]
  (if (= (first (@board pulls)) 1) true
    false))

(defn white? [pulls]
  (if (= (first (@board pulls)) 0) true
    false))

(defn free-for-white []
  (filter (complement nil?) (for [position (map inc (range 6))]
                              (let [pull (first (@board position))]
                                (if (or (= 0 pull) (= nil pull) (and (= 1 (count (@board position))) (= 1 pull)))
                                  position)))))
(defn free-for-black []
  (filter (complement nil?) (for [position (map #(+ % 19) (range 6))]
                              (let [pull (first (@board position))]
                                (if (or (= 1 pull) (= nil pull) (and (= 1 (count (@board position))) (= 0 pull)))
                                  position)))))

(defn is-in? [number coll]
  (reduce #(or %1 %2) (for [element coll]
                        (if (= number element)
                          true false))))

(defn move [from with]
  (cond
    (and (black? from) (white? (- from with)) (= 1 (count (@board (- from with)))))
      (let [to (- from with)
            new-config-to (conj [] (first (@board from)))
            new-config-from (pop (@board from))
            new-waiting (conj (@board :waiting-white) (first (@board to)))]
        (swap! board assoc :waiting-white new-waiting)
        (swap! board assoc to new-config-to)
        (swap! board assoc from new-config-from))
    (and (white? from) (black? (+ from with)) (= 1 (count (@board (+ from with)))))
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
  (if (black? number)
    (let [to (@board number)
          new-configure-to (vec [1])
          new-configure-from (pop (@board :waiting-white))
          new-configure-to-black (conj (@board :waiting-black) (first to))]
      (do
        (swap! board assoc number new-configure-to)
        (swap! board assoc :waiting-white new-configure-from)
        (swap! board assoc :waiting-black new-configure-to-black)))
  (let [new-configure-to (conj (@board number) 1)
        new-configure-from (pop (@board :waiting-white))]
    (do
      (swap! board assoc number new-configure-to)
      (swap! board assoc :waiting-white new-configure-from)))))

(defn white-out [dice]
  (cond
    (and (is-in? (first dice) (free-for-white)) (= 1 (count (@board :waiting-white))))
      (do
        (println "White! You have " (first dice) "and " (second dice))
        (println "You use the first number to pop your pull! Yeah!")
        (out-white-with (first dice))
        (println "Well, select a pull for your " (second dice) "(Second number)")
        (let [pull (read)]
          (move pull (second dice)))
        (turn-black))
    (and (is-in? (second dice) (free-for-white)) (= 1 (count (@board :waiting-white))))
      (do
        (println "White! You have " (first dice) "and " (second dice))
        (println "You use the second number to pop out your pull! Yeah!")
        (out-white-with (second dice))
        (println "Well, select a pull for your " (first dice) "(First number)")
        (let [pull (read)]
          (move pull (first dice)))
        (turn-black))
    (and
      (is-in? (first dice) (free-for-white))
      (is-in? (second dice) (free-for-white))
      (< 1 (count (@board :waiting-white))))
      (do
        (println "Well, you have to use both numbers to pop pulls! Let's do it!")
        (out-white-with (first dice))
        (out-white-with (second dice))
        (turn-black))
    :else
      (do
        (println "White! You have " (first dice) "and " (second dice))
        (println "Sorry, but you can't pop any of your fuckin' pulls! :(")
        (turn-black))))

(defn black-out [dice]
  (cond
    (and (is-in? (first dice) (free-for-black)) (= 1 (count (@board :waiting-black))))
      (do
        (println "Black! You have " (first dice) "and " (second dice))
        (println "You use the first number to pop your pull! Yeah!")
        (out-black-with (first dice))
        (println "Well, select a pull for your " (second dice) "(Second number)")
        (let [pull (read)]
          (move pull (second dice)))
        (turn-white))
    (and (is-in? (second dice) (free-for-black)) (= 1 (count (@board :waiting-black))))
      (do
        (println "Black! You have " (first dice) "and " (second dice))
        (println "You use the second number to pop out your pull! Yeah!")
        (out-black-with (second dice))
        (println "Well, select a pull for your " (first dice) "(First number)")
        (let [pull (read)]
          (move pull (first dice)))
        (turn-white))
    (and
      (is-in? (first dice) (free-for-black))
      (is-in? (second dice) (free-for-black))
      (< 1 (count (@board :waiting-black))))
      (do
        (println "Well, you have to use both numbers to pop pulls! Let's do it!")
        (out-black-with (first dice))
        (out-black-with (second dice))
        (turn-white))
    :else
      (do
        (println "Black! You have " (first dice) "and " (second dice))
        (println "Sorry, but you can't pop any of your fuckin' pulls! :(")
        (turn-white))))

(defn all-white-prepared? []
  (let [amount (apply + (for [pulls (range 20 25)]
                          (if (white? pulls)
                            (count (@board pulls))
                            0)))]
    (if (= amount 15)
      true
      false)))

(defn all-black-prepared? []
  (let [amount (apply + (for [pulls (range 1 6)]
                          (if (black? pulls)
                            (count (@board pulls))
                            0)))]
    (if (= amount 15)
      true
      false)))

(defn is-there-behind-white? [from]
  (if (<= from 20)
    false
      (reduce #(or %1 %2) (for [pull (range from 25)]
                            (if (or
                                  (empty? (@board pull))
                                  (black? pull))
                              false true)))))

(defn is-there-behind-black? [from]
  (if (>= from 5)
    false
      (reduce #(or %1 %2) (for [pull (range from 6)]
                            (if (or
                                  (empty? (@board pull))
                                  (white? pull))
                              false true)))))

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

(defn turn-black []
  (if (black-win?)
    (println "Congratulations, Blacks! You are the fuckin' winner! :)")
      (let [dice (roll-dice)]
        (if (empty? (@board :waiting-black))
          (do
            (println @board)
            (println "Black! You have " (first dice) "and " (second dice))
            (println "Please, select a pull for the first dice: ")
            (let [pull (read)]
              (do
                (if (all-black-prepared?)
                  (try-pulling-out-black pull (first dice))
                  (move pull (first dice)))))
            (println "And now select a pull for the second dice: ")
            (let [pull (read)]
              (do
                (if (all-black-prepared?)
                  (try-pulling-out-black pull (second dice))
                  (move pull (second dice)))))
            (turn-white))
          (black-out dice)))))

(defn turn-white []
  (if (white-win?)
    (println "Congratulations, Whites! You are the fuckin' winner! :)")
      (let [dice (roll-dice)]
        (if (empty? (@board :waiting-white))
          (do
            (println @board)
            (println "White! You have " (first dice) "and " (second dice))
            (println "Please, select a pull for the first dice: ")
            (let [pull (read)]
              (do
                (if (all-white-prepared?)
                  (try-pulling-out-white pull (first dice))
                  (move pull (first dice)))))
            (println "And now select a pull for the second dice: ")
            (let [pull (read)]
              (do
                (if (all-white-prepared?)
                  (try-pulling-out-white pull (second dice))
                  (move pull (second dice)))))
            (turn-black))
          (white-out dice)))))

(turn-white)