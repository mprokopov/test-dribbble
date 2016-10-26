(ns test-dribbble.core
  (:require [clojure.string :as str]))

(defn map->vec [input]
  (reduce #(into %1 %2) [] input))

(defn str->int [input-string]
  "convert to long if possible"
  (when (and input-string (string? input-string))
    (let [is-int (re-find #"^\d+$" input-string)]
      (if is-int (Long. input-string) input-string))))

(defn url->map [url]
  "returns URL parsed to map schema host path and queryparam"
  (let [[_ schema domain path queryparam]  (re-matches #"([a-z]+)://([^/]+)/([^\?]*)[\?]?(.*)"   url)]
    {:schema schema
     :host domain
     :path path
     :queryparam queryparam}))

(defn parse-keys [input]
  "returns keys for URL parsing"
  (map str/trim (str/split input #";")))

(defn retrieve-pattern [input-pattern]
  "returns array with pattern, keys and regex for qyery"
  (let [[_ kkey body] (re-matches #"(.+)\((.+)\)" input-pattern)
         keys-arr (into [] (map second (re-seq #"\?([^/]+)" body)))
         replaced (str/replace body #"\?([^/]+)" "([^&]*)")]
     [kkey body keys-arr replaced]))


(defn retrieve-values [url pattern]
  "retrieve values by pattern applying regex transformation or nil if pattern not matches"
  (let [url-map (url->map url)
        [k1 _ keys-arr rexp] (retrieve-pattern pattern)
        tested-val ((keyword k1) url-map)
        [is-found & matched] (re-find (re-pattern rexp) tested-val)
        result-map (zipmap (map keyword keys-arr) (map str->int matched))]
    (when is-found result-map)))


(defprotocol Service
  (recognize [pattern url]))

(defrecord Pattern [pattern]
  Service
  (recognize [pattern url]
     (let [patterns (parse-keys (:pattern pattern))
           coll (map #(retrieve-values url %) patterns)
           is-invalid (some nil? coll)]
      (when-not is-invalid (map->vec (remove empty? coll))))))


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

;; (use 'clojure.test)
;; (test-ns 'test-dribbble.core)
