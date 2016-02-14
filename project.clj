(defproject csgo-demo-analyzer "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-protobuf "0.4.0"]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.google.protobuf/protobuf-java "2.5.0"]
                 [protobuf "0.6.2"]
                 [funcool/octet "0.2.0"]]
  :source-paths ["src/main/clojure"])
