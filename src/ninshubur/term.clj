(ns ninshubur.term
  (:require [lanterna.terminal :as t]
            [ninshubur.vars :as v]))

(def ^:dynamic *t*)
(def ^:dynamic *border-char* \#)

(defn draw-border []
  (t/set-fg-color *t* :white)
  (t/move-cursor *t* 0 0)
  (dotimes [x (+ v/*area-width* 2)]
    (t/put-character *t* *border-char*))
  (dotimes [y v/*area-height*]
    (t/move-cursor *t* 0 (inc y))
    (t/put-character *t* *border-char*)
    (t/move-cursor *t* (inc v/*area-width*) (inc y))
    (t/put-character *t* *border-char*))
  (t/move-cursor *t* 0 (+ 1 v/*area-height*))
  (dotimes [x (+ v/*area-width* 2)]
    (t/put-character *t* *border-char*)))

(defn draw-object [object]
  (let [{:keys [color char y x length]} object]
    (t/set-fg-color *t* color)
    (dotimes [i length]
      (t/move-cursor *t* (inc (mod (+ x i) v/*area-width*)) (inc y))
      (t/put-character *t* char))))

(defn draw-simulation [objects]
  (t/clear *t*)
  (draw-border)
  (doseq [obj objects]
    (draw-object obj))
  (t/move-cursor *t* 0 0))

(defn get-key []
  (t/get-key-blocking *t*))

(defmacro in-term [& body]
  `(binding [*t* (t/get-terminal :text)]
     (t/in-terminal *t* ~@body)))
