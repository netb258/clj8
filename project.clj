(defproject chip8 "0.1.0-SNAPSHOT"
  :description "Chip8 emulator written in Clojure. Uses Quil for rendering."
  :url "http://example.com/FIXME"
  :plugins [[cider/cider-nrepl "0.14.0"]]
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [quil "2.7.1"]]
  :main chip8.core
  :aot :all)
