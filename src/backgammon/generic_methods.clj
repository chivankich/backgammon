(ns backgammon.generic_methods
  (:gen-class)
  (:use backgammon.init_game))

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
                                    position))))))

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

(defn pair? [dice]
  (= (first dice) (second dice)))