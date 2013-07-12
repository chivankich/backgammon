(ns backgammon.core-test
  (:use clojure.test
  		backgammon.init_game
        backgammon.generic_methods
        backgammon.move
        backgammon.choices
        backgammon.io
        backgammon.artificial_intelligence
        backgammon.preparation
        backgammon.turns
        backgammon.core))

(def original {1 [0, 0] 2 [] 3 [] 4 [] 5 [] 6 [1, 1, 1, 1, 1] 7 [] 8 [1, 1, 1] 9 [] 10 []
                11 [] 12 [0, 0, 0, 0, 0] 13 [1, 1, 1, 1, 1] 14 [] 15 [] 16 [] 17 [0, 0, 0]
                18 [] 19 [0, 0, 0, 0, 0] 20 [] 21 [] 22 [] 23 [] 24 [1, 1]
                :waiting-black [] :waiting-white []})

(deftest calculate-test
  (is (= (calculate [0 1 2] [2 3 4]) "                                     ")))

(deftest black-pulls-test
  (is (= (black? 24)) true)
  (is (= (black? 23)) false)
  (is (= (black? 1)) false))

(deftest white-pulls-test
  (is (= (white? 12)) true)
  (is (= (white? 18)) false)
  (is (= (white? 24)) false))

(deftest free-for-white-test
  (is (= (free-for :white) '(1 2 3 4 5))))

(deftest free-for-black-test
  (is (= (free-for :black) '(1 2 3 4 5))))

(deftest is-in-test
  (is (= (is-in? 4 [1 2 3 4 5]) true))
  (is (= (is-in? 2 [1 4 5 6 7]) nil)))

(deftest all-prepared-test
  (is (= (all-prepared? :white)) false)
  (is (= (all-prepared? :black)) false))

(deftest is-there-behind-test
  (is (= (is-there-behind? :white 1)) false)
  (is (= (is-there-behind? :white 24)) true)
  (is (= (is-there-behind? :black 24)) false)
  (is (= (is-there-behind? :black 1)) true))

(deftest choose-the-best-one-test
  (store-the-best [8 13] 5)
  (is (= (choose-the-best-one) 13))
  (swap! possible (fn [_] {})))

(deftest moves-test
  (let [result1 {1 [0] 2 [] 3 [] 4 [] 5 [0] 6 [1, 1, 1, 1, 1] 7 [] 8 [1, 1, 1] 9 []
                 10 [] 11 [] 12 [0, 0, 0, 0, 0] 13 [1, 1, 1, 1, 1] 14 [] 15 [] 16 []
                 17 [0, 0, 0] 18 [] 19 [0, 0, 0, 0, 0] 20 [] 21 [] 22 [] 23 [] 24 [1, 1]
                 :waiting-black [] :waiting-white []}
        result2 {1 [0, 0] 2 [] 3 [] 4 [] 5 [] 6 [1, 1, 1, 1, 1] 7 [] 8 [1, 1, 1, 1] 9 []
                 10 [] 11 [] 12 [0, 0, 0, 0, 0] 13 [1, 1, 1, 1] 14 [] 15 [] 16 []
                 17 [0, 0, 0] 18 [] 19 [0, 0, 0, 0, 0] 20 [] 21 [] 22 [] 23 [] 24 [1, 1]
                 :waiting-black [] :waiting-white []}
        result3 {1 [0, 0] 2 [] 3 [] 4 [] 5 [] 6 [1, 1, 1, 1, 1] 7 [] 8 [1, 1, 1] 9 []
                 10 [] 11 [] 12 [0, 0, 0, 0] 13 [1, 1, 1, 1, 1] 14 [0] 15 [] 16 []
                 17 [0, 0, 0] 18 [] 19 [0, 0, 0, 0, 0] 20 [] 21 [] 22 [] 23 [] 24 [1, 1]
                 :waiting-black [] :waiting-white []}]
    (move 1 4)
    (is (= @board result1))
    (swap! board (fn [_] original))
    (move 13 5)
    (is (= @board result2))
    (swap! board (fn [_] original))
    (move 12 2)
    (is (= @board result3))
    (swap! board (fn [_] original))))

(deftest win-test
  (is (= (win? :white) false))
  (is (= (win? :black) false)))

(deftest wall-test
  (is (= (wall? :white 6 5)) true)
  (is (= (wall? :black 12 1)) true)
  (is (= (wall? :white 6 2)) false)
  (is (= (wall? :black 17 6)) false))

(deftest available-test
  (is (= (available :white 6) '(1 12 17)))
  (is (= (available :white 1) '(1 17 19)))
  (is (= (available :black 6) '(8 13 24)))
  (is (= (available :black 1) '(6 8 24))))

(deftest try-pulling-out-test
  (let [result1 {1 [] 2 [] 3 [] 4 [] 5 [] 6 [1, 1, 1, 1, 1] 7 [] 8 [1, 1, 1, 1, 1] 9 [] 10 []
                  11 [] 12 [] 13 [1, 1, 1, 1, 1] 14 [] 15 [] 16 [] 17 []
                  18 [] 19 [0, 0, 0, 0, 0] 20 [] 21 [0, 0, 0] 22 [0, 0] 23 [0, 0, 0] 24 [0]
                  :waiting-black [] :waiting-white []}
        result2 {1 [] 2 [] 3 [] 4 [] 5 [] 6 [1, 1, 1, 1, 1] 7 [] 8 [1, 1, 1, 1, 1] 9 [] 10 []
                  11 [] 12 [] 13 [1, 1, 1, 1, 1] 14 [] 15 [] 16 [] 17 []
                  18 [] 19 [0, 0, 0, 0] 20 [] 21 [0, 0, 0] 22 [0, 0] 23 [0, 0, 0] 24 [0]
                  :waiting-black [] :waiting-white []}
        result3 {1 [1, 1, 1] 2 [1, 1, 1, 1] 3 [1, 1] 4 [1] 5 [] 6 [1, 1, 1, 1, 1] 7 [] 8 [] 9 [] 10 []
                  11 [] 12 [0, 0, 0, 0, 0] 13 [] 14 [] 15 [] 16 [] 17 [0, 0, 0]
                  18 [0, 0] 19 [0, 0, 0, 0, 0] 20 [] 21 [] 22 [] 23 [] 24 []
                  :waiting-black [] :waiting-white []}
        result4 {1 [1, 1, 1] 2 [1, 1, 1, 1] 3 [1, 1] 4 [] 5 [] 6 [1, 1, 1, 1, 1] 7 [] 8 [] 9 [] 10 []
                  11 [] 12 [0, 0, 0, 0, 0] 13 [] 14 [] 15 [] 16 [] 17 [0, 0, 0]
                  18 [0, 0] 19 [0, 0, 0, 0, 0] 20 [] 21 [] 22 [] 23 [] 24 []
                  :waiting-black [] :waiting-white []}
        result5 {1 [] 2 [] 3 [] 4 [] 5 [] 6 [1, 1, 1, 1, 1] 7 [] 8 [1, 1, 1, 1, 1] 9 [] 10 []
                  11 [] 12 [] 13 [1, 1, 1, 1, 1] 14 [] 15 [] 16 [] 17 []
                  18 [] 19 [] 20 [] 21 [0, 0, 0] 22 [0, 0, 0, 0, 0, 0] 23 [0, 0, 0] 24 [0, 0, 0]
                  :waiting-black [] :waiting-white []}
        result6 {1 [] 2 [] 3 [] 4 [] 5 [] 6 [1, 1, 1, 1, 1] 7 [] 8 [1, 1, 1, 1, 1] 9 [] 10 []
                  11 [] 12 [] 13 [1, 1, 1, 1, 1] 14 [] 15 [] 16 [] 17 []
                  18 [] 19 [] 20 [] 21 [0, 0] 22 [0, 0, 0, 0, 0, 0] 23 [0, 0, 0] 24 [0, 0, 0]
                  :waiting-black [] :waiting-white []}]
    (swap! board (fn [_] result1))
    (try-pulling-out :white 19 6)
    (is (= @board result2))
    (swap! board (fn [_] result3))
    (try-pulling-out :black 4 4)
    (is (= @board result4))
    (swap! board (fn [_] result5))
    (try-pulling-out :white 21 6)
    (is (= @board result6))
    (swap! board (fn [_] original))))

(deftest back-in-the-game-test
  (let [result1 {1 [0] 2 [] 3 [] 4 [] 5 [] 6 [1, 1, 1, 1, 1] 7 [] 8 [1, 1, 1] 9 [] 10 []
                  11 [] 12 [0, 0, 0, 0, 0] 13 [1, 1, 1, 1, 1] 14 [] 15 [] 16 [] 17 [0, 0, 0]
                  18 [] 19 [0, 0, 0, 0, 0] 20 [] 21 [] 22 [] 23 [] 24 [1, 1]
                  :waiting-black [] :waiting-white [0]}
        result2 {1 [0] 2 [] 3 [] 4 [0] 5 [] 6 [1, 1, 1, 1, 1] 7 [] 8 [1, 1, 1] 9 [] 10 []
                  11 [] 12 [0, 0, 0, 0, 0] 13 [1, 1, 1, 1, 1] 14 [] 15 [] 16 [] 17 [0, 0, 0]
                  18 [] 19 [0, 0, 0, 0, 0] 20 [] 21 [] 22 [] 23 [] 24 [1, 1]
                  :waiting-black [] :waiting-white []}
        result3 {1 [0, 0] 2 [] 3 [] 4 [] 5 [] 6 [1, 1, 1, 1, 1] 7 [] 8 [1, 1, 1] 9 [] 10 []
                  11 [] 12 [0, 0, 0, 0, 0] 13 [1, 1, 1, 1, 1] 14 [] 15 [] 16 [] 17 [0, 0, 0]
                  18 [] 19 [0, 0, 0, 0, 0] 20 [] 21 [] 22 [] 23 [] 24 [1]
                  :waiting-black [1] :waiting-white []}
        result4 {1 [0, 0] 2 [] 3 [] 4 [] 5 [] 6 [1, 1, 1, 1, 1] 7 [] 8 [1, 1, 1] 9 [] 10 []
                  11 [] 12 [0, 0, 0, 0, 0] 13 [1, 1, 1, 1, 1] 14 [] 15 [] 16 [] 17 [0, 0, 0]
                  18 [] 19 [0, 0, 0, 0, 0] 20 [] 21 [] 22 [] 23 [1] 24 [1]
                  :waiting-black [] :waiting-white []}]
    (swap! board (fn [_] result1))
    (back-in-the-game :white 4)
    (is (= @board result2))
    (swap! board (fn [_] result3))
    (back-in-the-game :black 2)
    (is (= @board result4))
    (swap! board (fn [_] original))))

(deftest pair-dice-test
  (is (= (pair? [1, 1]) true))
  (is (= (pair? [2, 4]) false)))

(deftest random-position-test
  (is (= (is-in? (random-position [1 4 12 17]) [1 4 12 17]) true)))