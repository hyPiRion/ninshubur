(ns ninshubur.simulate
  (:require [ninshubur.term :as t]
            [lanterna.terminal :as tty]))

(defn start-term []
  (t/in-term
   (t/draw-simulation #{{:char \x :color :red :y 3 :x 5 :length 6}})
   (tty/get-key-blocking t/term)))
