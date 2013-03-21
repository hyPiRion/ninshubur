(ns ninshubur.term
  (:require [lanterna.terminal :as t]
            [ninshubur.vars :as v]))

(def ^:dynamic *t*)

(defn draw-border []
  (t/set-fg-color *t* :white)
  (t/set-bg-color *t* :black)
  (t/move-cursor *t* 0 0)
  (dotimes [x (+ v/*area-width* 2)]
    (t/put-character *t* \#))
  (dotimes [y v/*area-height*]
    (t/move-cursor *t* 0 (inc y))
    (t/put-character *t* \#)
    (t/move-cursor *t* (inc v/*area-width*) (inc y))
    (t/put-character *t* \#))
  (t/move-cursor *t* 0 (+ 1 v/*area-height*))
  (dotimes [x (+ v/*area-width* 2)]
    (t/put-character *t* \#)))

(defn draw-object [object]
  (let [{:keys [color char y x length]} object]
    (t/set-fg-color *t* color)
    (t/move-cursor *t* (inc x) (inc y))
    (dotimes [i length]
      (t/put-character *t* char))))

(defn draw-simulation [objects]
  (t/clear *t*)
  (draw-border)
  (doseq [obj objects]
    (draw-object obj)))

(defn get-key []
  (t/get-key-blocking *t*))

(defmacro in-term [& body]
  `(binding [*t* (t/get-terminal :text)]
     (t/in-terminal *t* ~@body)))
