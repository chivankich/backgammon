; (defproject backgammon "0.1.0-SNAPSHOT"
;   :description "FIXME: write description"
;   :url "http://example.com/FIXME"
;   :license {:name "Eclipse Public License"
;             :url "http://www.eclipse.org/legal/epl-v10.html"}
;   :dependencies [[org.clojure/clojure "1.4.0"]])

(def board (atom {1 []
                  2 []
                  3 [1, 1]
                  4 [1]
                  5 [1, 1, 1]
                  6 []
                  7 [1, 1, 1, 1, 1]
                  8 []
                  9 []
                  10 []
                  11 []
                  12 []
                  13 [1, 1, 1, 1, 1]
                  14 []
                  15 []
                  16 []
                  17 []
                  18 [0, 0, 0, 0, 0]
                  19 []
                  20 [0]
                  21 [1]
                  22 [0, 0, 0]
                  23 []
                  24 [1, 1]
                  :waiting-black []
                  :waiting-white [0]}))

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

(defn white-out [dice]
  (cond
    (and (is-in? (first dice) (free-for-white)) (= 1 (count (@board :waiting-white))))
      (do
        (println "White! You have " (first dice) "and " (second dice))
        (println "You use the first number to pop your pull! Yeah!")
        (out-white-with (first dice))
        (println "Well, select a pull for your " (second dice) "(Second number)")
        (let [pull (read)]
          (move pull (second dice))))
    (and (is-in? (second dice) (free-for-white)) (= 1 (count (@board :waiting-white))))
      (do
        (println "White! You have " (first dice) "and " (second dice))
        (println "You use the second number to pop out your pull! Yeah!")
        (out-white-with (second dice))
        (println "Well, select a pull for your " (first dice) "(First number)")
        (let [pull (read)]
          (move pull (first dice))))
    (and
      (is-in? (first dice) (free-for-white))
      (is-in? (second dice) (free-for-white))
      (< 1 (count (@board :waiting-white))))
      (do
        (println "Well, you have to use both numbers to pop pulls! Let's do it!")
        (out-white-with (first dice))
        (out-white-with (second dice)))
    :else
      (do
        (println "White! You have " (first dice) "and " (second dice))
        (println "Sorry, but you can't pop any of your fuckin' pulls! :("))))

(defn all-white-prepared? []
  (let [amount (apply + (for [pulls (range 20 25)]
                          (if (white? pulls)
                            (count (@board pulls))
                            0)))]
    (if (= amount 15)
      true
      false)))

(defn is-there-behind-white? [from]
  (if (<= from 20)
    false
      (reduce #(or %1 %2) (for [pull (range 20 from)]
                            (if (or
                                  (empty? (@board pull))
                                  (black? pull))
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




; (defn out-of-play-white [from real-number]
;   (let [number (+ 18 real-number)]
;     (if-not (and (empty? (@board from)) ()
;       ()






(defn turn-white []
  (let [dice (roll-dice)]
    (if (empty? (@board :waiting-white))
      (do
        (println "White! You have " (first dice) "and " (second dice))
        (println "Please, select a pull for the first dice: ")
        (let [pull (read)]
          (do
            (move pull (first dice))))
        (println "And now select a pull for the second dice: ")
        (let [pull (read)]
          (do
            (move pull (second dice)))))
      (white-out dice))))


(prn (is-there-behind-white? 22))