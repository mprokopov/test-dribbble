(ns test-dribbble.core
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]))


;; (let [[_ _ user id]  (re-matches #"[a-z]+://(twitter.com)/(.+)/status/(.+)"   "http://twitter.com/bradfitz/status/562360748727611392")]
;;   [:id id
;;    :user user])

(defn map->vec [input]
  (reduce #(into %1 %2) [] input))

(defn str->int [input-string]
  (when (and input-string (string? input-string))
    (let [is-int (re-find #"^\d+$" input-string)]
      (if is-int (Long. input-string) input-string)))) ;; (str->int "asdf")

(defn url->map [url]
  "returns URL parsed to map schema host path and queryparam"
  (let [[_ schema domain path queryparam]  (re-matches #"([a-z]+)://([^/]+)/([^\?]*)[\?]?(.*)"   url)]
    {:schema schema
     :host domain
     :path path
     :queryparam queryparam}))

;; (url->map "http://twitter.com/bradfitz/status/562360748727611392")
;; (url->map "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
;; (url->map "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users")

(defn parse-keys [input]
  "returns keys for URL parsing"
  (map str/trim (str/split input #";")))

;; (def arr-1 (first (parse-keys "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);")))
;; (def arr-2 (second (parse-keys "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);")))
;; (def arr-3 (nth (parse-keys "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);") 2))
;; (def arr-4 (last (parse-keys "host(twitter.com); path(?user/status/?id);"))) ;; "http://twitter.com/bradfitz/status/562360748727611392"

;; (defn get-vals [input-text]
;;   "replace ?val to regexp matcher and retrieve keys"
;;   (let [[_ kkey vval] (re-matches #"(.+)\((.+)\)" input-text)
;;         [_ extra] (re-matches #".*\?(.*)" vval)
;;         replaced (str/replace vval (re-pattern (str "\\?" extra)) "(.*)")]
;;     [kkey vval extra replaced]))

(defn retrieve-pattern [input-pattern]
  "returns array with pattern, keys and regex for qyery"
  (let [[_ kkey body] (re-matches #"(.+)\((.+)\)" input-pattern) ;; all inside ( ) is body
         keys-arr (into [] (map second (re-seq #"\?([^/]+)" body)))
         replaced (str/replace body #"\?([^/]+)" "([^&]*)")]
     [kkey body keys-arr replaced]))

;; (re-seq #"list=([^&]*)" "list=users&offset=1")


;; (let [[_ kkey body] (re-matches #"(.+)\((.+)\)" "path(?user/status/?id)") ;; all inside ( ) is body
;;        keys-arr (into [] (map second (re-seq #"\?([^/]+)" body)))
;;        replaced (str/replace body #"\?([^/]+)" "(.*)")]
;;    [kkey body keys-arr replaced])

;; [:queryparam ["user" "id" "(.*)/status/(.*)"]]

;; (map second (re-seq #"\?([^/]+)" "?user/status/?id"))
;; (re-seq #"\?([^/]+)" "?user/?status/sdfid")
;; (re-seq #"\?([^/]+)" "user/status/?sdfid")
;; (str/replace "?user/status/?id" #"\?([^/]+)" "(.*)")

;; (retrieve-pattern arr-1)
;; (retrieve-pattern arr-2)
;; (retrieve-pattern arr-3)
;; (retrieve-pattern arr-4)

;; (re-matches #".*\?(.*)" "list=?type")

;; (defn parse-str-keys [strn arr-keys]
;;   (let [url-map (url->map strn)
;;         [k1 _ key-name rexp] arr-keys
;;         tested-val ((keyword k1) url-map)
;;         [_ matched] (re-matches (re-pattern (str ".*" rexp)) tested-val)]
;;     [(keyword key-name) matched]))

(defn retrieve-values [url pattern]
  "retrieve values by pattern applying regex transformation or nil if pattern not matches"
  (let [url-map (url->map url) ;; (url->map "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
        [k1 _ keys-arr rexp] (retrieve-pattern pattern) ;; (retrieve-pattern arr-1)
        tested-val ((keyword k1) url-map) ;; (re-find (re-pattern "(.*)/status/(.*)") "bradfitz/status/562360748727611392")
        [is-found & matched] (re-find (re-pattern rexp) tested-val) ;; (re-find (re-pattern "dribbble.com") "dribbble.com")
        result-map (zipmap (map keyword keys-arr) (map str->int matched))]
    (when is-found result-map)))

;; (re-find (re-pattern "list=([^&]*)") "list=users&offset=1")

;; (let [[is-found & found] (re-find (re-pattern "(.*)/status/(.*)") "bradfitz/status/562360748727611392")]
;;   is-found)

;; (let [[is-found & found] (re-find (re-pattern "(.*)/status/(.*)") "bradfitz/status/562360748727611392")]
;;   (vec found))


;; (retrieve-values "http://twitter.com/bradfitz/status/562360748727611392" arr-4)
;; [[:user "bradfitz"] [:id "562360748727611392"]]
;; (retrieve-values "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1" arr-1)
;; []
;; (retrieve-values "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1" arr-2)
;; [[:id "1905065-Travel-Icons-pack"]]
;; (retrieve-values "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1" arr-3)
;; [[:offset "1"]]

;; (let [url "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"
;;       patterns (parse-keys "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);")
;;       pattern (last patterns)]
;;   (map #(retrieve-values url %) patterns))


(defn recognize2 [url pattern]
  (let [patterns (parse-keys pattern)
        coll (map #(retrieve-values url %) patterns)
        is-invalid (some nil? coll)]
    (when-not is-invalid (map->vec (remove empty? coll)))))


;; (recognize2 "http://twitter.com/mprokopov/status/5748727611392/asdfsdf" "host(twitter.com); path(?user/status/?id/?sdf);")
;; (recognize2 "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1" "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);")
;; (recognize2 "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1" "host(dribbble.com); path(shots/?id); queryparam(offset=?offset); queryparam(list=?type);")


;; (parse-keys "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);")
;; (parse-keys "host(dribbble.com); path(shots/?id); queryparam(offset=?offset); queryparam(list=?type);")
;; (retrieve-pattern "queryparam(list=?type)")

;; (def ttt (recognize2 "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1" "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);"))

;; DONE: -> transform value to int
;; DONE: cleanup output array for empty values
(defprotocol Service
  (recognize [pattern url]))

(defrecord Pattern [pattern]
  Service
  (recognize [pattern url]
             (recognize2 url (:pattern pattern))))

(def twitter (Pattern. "host(twitter.com); path(?user/status/?id);"))
(recognize twitter "http://twitter.com/bradfitz/status/562360748727611392")
;; => [[:id 562360748727611392] [:user "bradfitz"]]

(def dribbble (Pattern. "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);"))
(recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
;; => [[:id "1905065-Travel-Icons-pack"] [:offset "1"]]
(recognize dribbble "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
;; => nil ;; host mismatch
(recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users")
;; => nil ;; offset queryparam missing

(def dribbble2 (Pattern. "host(dribbble.com); path(shots/?id); queryparam(offset=?offset); queryparam(list=?type);"))

(use 'clojure.test)

(deftest test-str->int
  (is (= (str->int "12345")
        12345)))

(deftest test-str->int-fail
  (is (= (str->int "1234asd")
         "1234asd")))

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

(test-ns 'test-dribbble.core)
