(ns csgo-demo-reader.core-test
  (:require [clojure.test :refer :all]
            [csgo-demo-reader.core :refer :all]))

(deftest parse-game-event-list-test
  (testing "Creating the game event list data structure"
    (is (= {0 {:eventid 0 :name "server_spawn" :keys [{:type 1 :name "hostname"}]}}
           (parse-game-event-list {:descriptors [{:eventid 0 :name "server_spawn" :keys [{:type 1 :name "hostname"}]}]})))))

(deftest parse-game-event-test
  (testing "Creating the game event data structure"
    (is (= {:eventid 0 :name "server_spawn" :data {"hostname" "example.com"}}
           (parse-game-event {:eventid 0 :keys [{:type 1 :val-string "example.com"}]} {0 {:eventid 0 :name "server_spawn" :keys [{:type 1 :name "hostname"}]}})))))

(deftest int-log2-test
  (testing "int-log2 gets the number of bits in a number"
    (is (= 2 (int-log2 2)))
    (is (= 4 (int-log2 8)))
    (is (= 4 (int-log2 9)))))
