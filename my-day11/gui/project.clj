(defproject gui "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [quil "3.1.0"]]
  :main gui.core
  :profiles {:dev {:dependencies [[speclj "3.4.1"]]}}
  :plugins [[speclj "3.4.1"]]
  :test-paths ["spec"]
  )
