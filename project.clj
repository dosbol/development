(defproject discourje "6.1.0"
  :description "Discourje is a library to describe communication between systems as protocols.\nA protocol acts as an agreement on how participants interact with each other.\nAll communication between participants is monitored by the protocol to ensure the correct flow of communication."
  :url "https://gitlab.com/ruben.hamers/Discourje"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.async "0.4.500"]
                 [org.clojure/tools.nrepl "0.2.13"]
                 [str-to-argv "0.1.1"]
                 [clojure-complete "0.2.5"]]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :java-source-paths ["src/main/java"]
  :profiles {:examples  {:main         discourje.examples.main
                         :aot          [discourje.core.async
                                        discourje.core.spec
                                        discourje.examples.main]
                         :uberjar-name "discourje-examples.jar"}})

;; $ lein with-profile examples uberjar