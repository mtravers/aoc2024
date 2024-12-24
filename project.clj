(defproject aoc2024 "0.1.0-SNAPSHOT"
  :description "solutions to Advent of Code 2024"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.candelbio/multitool "0.1.7"]
                 [org.clojure/math.numeric-tower "0.1.0"]
                 [org.clojure/math.combinatorics "0.3.0"]
                 [io.github.nextjournal/clerk "0.17.1102"]
                 ]
  :main ^:skip-aot aoc2024.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
