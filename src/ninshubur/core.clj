(ns ninshubur.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp :refer [cl-format]])
  (:import (java.net Socket))
  (:gen-class))

(defn fmt [out string & args]
  (cl-format *out* (str "out=> " string) args)
  (cl-format out string args))

(defn -main
  "Basic connector to Genetica."
  [& args]
  (let [s (Socket. "localhost" 1055)
        lcount (atom 0)]
    (try
      (with-open [in (io/reader s)
                  out (io/writer s)]
        (fmt out (str "[100, 120, roulette, 60, 0.3, fitness, "
                      "full_replacement, 10, one_max, 40, 0.2, 0.001, "
                      "1, 0.7].~%"))
        (doseq [line (line-seq in)]
          (swap! lcount inc)
          (println @lcount "=>" (read-string line))))
      (finally
        (.close s)))))
