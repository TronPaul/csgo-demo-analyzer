(defproject csgo-demo-reader "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-protobuf "0.4.0"]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.flatland/protobuf "0.8.1"]            ; THIS INCLUDES protobuf-java SOURCE for 2.5.0 (why? no idea).
                                                            ; DON'T CHANGE THE VERSION.
                 [funcool/octet "0.2.0"]]
  :source-paths ["src/main/clojure"]
  :java-source-paths ["src/main/java"]
  :profiles {:dev {:jvm-opts ["-Dcom.sun.management.jmxremote"
                              "-Dcom.sun.management.jmxremote.ssl=false"
                              "-Dcom.sun.management.jmxremote.authenticate=false"
                              "-Dcom.sun.management.jmxremote.port=43210"]
                   }})
