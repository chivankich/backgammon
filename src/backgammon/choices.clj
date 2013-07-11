(ns backgammon.choices
  (:gen-class)
  (:use backgammon.init_game)
  (:use backgammon.generic_methods)
  (:use backgammon.move))

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

(defn correct-choice-between []
  (let [choice (read)]
    (if-not (or (= choice 1) (= choice 2))
      (do
        (println "!!! Please, choose between 1 and 2! Try again...!!!")
        (correct-choice-between))
      choice)))