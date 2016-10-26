(ns test-dribbble.core-test
  (:require [clojure.test :refer :all]
            [test-dribbble.core :refer :all]))

(deftest test-str->int-int
  (is (= (str->int "12345")
        12345)))

(deftest test-str->int-str
  (is (= (str->int "1234asd")
         "1234asd")))

(deftest test-parse-keys
  (is (= (parse-keys "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);")
         (list "host(dribbble.com)" "path(shots/?id)" "queryparam(offset=?offset)"))))

(deftest test-recognize-twitter
  (is (= (recognize twitter "http://twitter.com/bradfitz/status/562360748727611392")
         [[:user "bradfitz"] [:id 562360748727611392]])))

(deftest test-recognize-dribble
  (is (= (recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
         [[:id "1905065-Travel-Icons-pack"] [:offset 1]])))

(deftest test-fail-recognize-offset-missing
  (is (= (recognize dribbble "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users")
         nil)))

(deftest test-fail-recognize-dribble2
  (is (= (recognize dribbble2 "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
         [[:id "1905065-Travel-Icons-pack"] [:offset 1] [:type "users"]])))
