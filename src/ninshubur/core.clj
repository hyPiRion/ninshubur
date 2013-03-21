(ns ninshubur.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp :refer [cl-format]]
            [ninshubur.neuron :as nn])
  (:import (java.net Socket))
  (:gen-class))

(defn fmt [out string & args]
  (cl-format *out* (str "out=> " string) args)
  (cl-format out string args))

(defn ert->clj [str]
  (-> str
      (.replace \{ \[)
      (.replace \} \])))

(defn -main
  "Basic connector to Genetica."
  [& args]
  (let [lguy (atom nil)]
    (with-open [s (Socket. "localhost" 1055)]
      (let [lcount (atom 0)]
        (with-open [in (io/reader s)
                    out (io/writer s)]
          (fmt out (str "[100, 200, roulette, 60, 0.3, rank, "
                        "generational_mixing, 50, cognitive, 0.2, 10].~%"))
          (doseq [line (line-seq in)]
            (let [res (read-string (ert->clj line))]
              (swap! lcount inc)
              (reset! lguy (last res))
              (println @lcount "=>" (get res 2)))))))
    (println (nn/translate-cluster @lguy))))
