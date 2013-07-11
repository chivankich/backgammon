(ns backgammon.core
  (:gen-class)
  (:use backgammon.init_game
        backgammon.generic_methods
        backgammon.move
        backgammon.choices
        backgammon.io
        backgammon.artificial_intelligence
        backgammon.preparation
        backgammon.turns))

(defn -main [& args]
  (question)
  (black-or-white?))


