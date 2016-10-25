(ns test-dribbble.core
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(defprotocol Service
  (recognize [proto pattern])
  (get-params [this]))

;; (defn recognize [pat strn]
;;   [1 2 3])

(defrecord Pattern [pattern]
  Service
  (get-params [pattern] pattern)
  (recognize [proto pattern]
             proto))

(def twitter (Pattern. "host(twitter.com); path(?user/status/?id);"))

(recognize twitter "http://twitter.com/bradfitz/status/562360748727611392")
;; => [[:id 562360748727611392] [:user "bradfitz"]]

;; (let [[_ _ user id]  (re-matches #"[a-z]+://(twitter.com)/(.+)/status/(.+)"   "http://twitter.com/bradfitz/status/562360748727611392")]
;;   [:id id
;;    :user user])


(defn url->map [url]
  "returns URL parsed to map"
  (let [[_ schema domain path queryparam]  (re-matches #"([a-z]+)://([^/]+)/([^\?]*)[\?]?(.*)"   url)]
    {:schema schema
     :host domain
     :path path
     :queryparam queryparam}))

(url->map "http://twitter.com/bradfitz/status/562360748727611392")
(url->map "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
(url->map "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users")

(defn parse-keys [input]
  "returns keys for URL parsing"
  (map str/trim (str/split input #";")))

(def arr-1 (first (parse-keys "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);")))
(def arr-2 (second (parse-keys "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);")))
(def arr-3 (nth (parse-keys "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);") 2))
(def arr-4 (last (parse-keys "host(twitter.com); path(?user/status/?id);"))) ;; "http://twitter.com/bradfitz/status/562360748727611392"

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
         replaced (str/replace body #"\?([^/]+)" "(.*)")]
     [kkey body keys-arr replaced]))

;; (let [[_ kkey body] (re-matches #"(.+)\((.+)\)" "path(?user/status/?id)") ;; all inside ( ) is body
;;        keys-arr (into [] (map second (re-seq #"\?([^/]+)" body)))
;;        replaced (str/replace body #"\?([^/]+)" "(.*)")]
;;    [kkey body keys-arr replaced])

;; [:queryparam ["user" "id" "(.*)/status/(.*)"]]

(map second (re-seq #"\?([^/]+)" "?user/status/?id"))
(re-seq #"\?([^/]+)" "?user/?status/sdfid")
(re-seq #"\?([^/]+)" "user/status/?sdfid")
(str/replace "?user/status/?id" #"\?([^/]+)" "(.*)")

;; (get-vals arr-2)
;; (get-vals arr-3)
;; (get-vals arr-4)
(retrieve-pattern arr-1)
(retrieve-pattern arr-2)
(retrieve-pattern arr-3)
(retrieve-pattern arr-4)

;; (re-matches #".*\?(.*)" "list=?type")

;; (defn parse-str-keys [strn arr-keys]
;;   (let [url-map (url->map strn)
;;         [k1 _ key-name rexp] arr-keys
;;         tested-val ((keyword k1) url-map)
;;         [_ matched] (re-matches (re-pattern (str ".*" rexp)) tested-val)]
;;     [(keyword key-name) matched]))

(defn retrieve-values [url pattern]
  (let [url-map (url->map url) ;; (url->map "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
        [k1 _ keys-arr rexp] (retrieve-pattern pattern) ;; (retrieve-pattern arr-1)
        tested-val ((keyword k1) url-map) ;; (re-find (re-pattern "(.*)/status/(.*)") "bradfitz/status/562360748727611392")
        [is-found & matched] (re-find (re-pattern rexp) tested-val) ;; (re-find (re-pattern "dribbble.com") "dribbble.com")
        result-map (zipmap (map keyword keys-arr) matched)]
    (when is-found (vec result-map))))

;; (re-find (re-pattern "offset=(.*)") "list=users&offset=1")

;; (let [[is-found & found] (re-find (re-pattern "(.*)/status/(.*)") "bradfitz/status/562360748727611392")]
;;   is-found)

;; (let [[is-found & found] (re-find (re-pattern "(.*)/status/(.*)") "bradfitz/status/562360748727611392")]
;;   (vec found))


(retrieve-values "http://twitter.com/bradfitz/status/562360748727611392" arr-4)
(retrieve-values "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1" arr-1)
(retrieve-values "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1" arr-2)
(retrieve-values "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1" arr-3)

;; (let [url "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"
;;       patterns (parse-keys "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);")
;;       pattern (last patterns)]
;;   (map #(retrieve-values url %) patterns))

(defn recognize2 [url pattern]
  (let [patterns (parse-keys pattern)
        coll (map #(retrieve-values url %) patterns)
        is-invalid (some nil? coll)]
    (when-not is-invalid coll)))

(recognize2 "http://twitter.com/mprokopov/status/5748727611392/asdfsdf" "host(twitter.com); path(?user/status/?id/?sdf);")
(recognize2 "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1" "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);")

;; (re-matches #"(.*)/status/(.*)" "/bradfitz/status/562360748727611392")


;; (def pat (re-pattern "http[s]?://dribbble.com/shots/(.*)\\?list=users&offset=(.+)"))
;; (re-matches pat "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")

;; (let [host "dribbble.com"
;;       path "shots/"
;;       pat (str "http[s]?://" host "/" path "(.*)\\?list=users&offset=(.+)")]
;;   (re-matches (re-pattern pat) "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"))

(def dribbble (Pattern. "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);"))
(recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
;; => [[:id "1905065-Travel-Icons-pack"] [:offset "1"]]
(recognize dribbble "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
;; => nil ;; host mismatch
(recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users")
;; => nil ;; offset queryparam missing

(def dribbble2 (Pattern. "host(dribbble.com); path(shots/?id); queryparam(offset=?offset); queryparam(list=?type);"))
