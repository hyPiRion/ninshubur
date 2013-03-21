(ns ninshubur.term
  (:require [lanterna.terminal :as t]))

(def ^:dynamic *t*)

(def area-height 15)
(def area-width 20)

(defn draw-border []
  (t/set-fg-color *t* :white)
  (t/set-bg-color *t* :black)
  (t/move-cursor *t* 0 0)
  (dotimes [x (+ area-width 2)]
    (t/put-character *t* \#))
  (dotimes [y area-height]
    (t/move-cursor *t* 0 (inc y))
    (t/put-character *t* \#)
    (t/move-cursor *t* (inc area-width) (inc y))
    (t/put-character *t* \#))
  (t/move-cursor *t* 0 (+ 1 area-height))
  (dotimes [x (+ area-width 2)]
    (t/put-character *t* \#)))

(defn draw-object [object]
  (let [{:keys [color char y x length]} object]
    (t/set-fg-color *t* color)
    (t/move-cursor *t* x y)
    (dotimes [i length]
      (t/put-character *t* char))))

(defn draw-simulation [objects]
  (draw-border)
  (doseq [obj objects]
    (draw-object obj)))

(defmacro in-term [& body]
  `(t/in-terminal *t* ~@body))