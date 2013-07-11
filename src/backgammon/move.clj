(ns backgammon.move
  (:gen-class)
  (:use backgammon.init_game)
  (:use backgammon.generic_methods))

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