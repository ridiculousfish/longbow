(defproject longbow "0.1.0-SNAPSHOT"
  :description "A shooter of long shots"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [aysylu/loom "1.0.0"]
                 [cljfmt "0.1.5"]]
  :plugins [[lein-cljfmt "0.5.7"]]
  :main ^:skip-aot longbow.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

