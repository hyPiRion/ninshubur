(ns ninshubur.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp :refer [cl-format]])
  (:import (java.net Socket))
  (:gen-class))

(defn -main
  "Basic connector to Genetica."
  [& args]
  (let [s (Socket. "localhost" 1055)]
    (try
      (with-open [in (io/reader s)
                  out (io/writer s)]
        (cl-format out "[100, foobar].~%")
        (doseq [line (line-seq in)]
          (println (read-string line))))
      (finally
        (.close s)))))
