(ns backgammon.init_game
  (:gen-class))

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

(def possible (atom {}))

(def versus-cpu? (atom false))