(defproject ninshubur "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clojure-lanterna "0.9.3"]
                 [org.clojure/tools.cli "0.2.2"]
;                 [incanter "1.5.0-SNAPSHOT"
;                  :exclusions [incanter/incanter-mongodb]]
                 ]
  :main ninshubur.main
  :uberjar-name "ninshubur.jar")
