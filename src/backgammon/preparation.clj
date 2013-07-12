(ns backgammon.preparation
  (:gen-class)
  (:use backgammon.init_game)
  (:use backgammon.generic_methods)
  (:use backgammon.move)
  (:use backgammon.choices)
  (:use backgammon.io)
  (:use backgammon.artificial_intelligence))

(defn question []
  (println "=====================================================")
  (println "Do you want to play against CPU? (0 / 1)")
  (let [q (read)]
    (if (= q 1)
      (swap! versus-cpu? (fn [_] true))
      (if-not (= 0 q)
        (do
          (println "!!! Your choice is not correct! !!!")
          (question))))))