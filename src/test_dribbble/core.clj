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

(let [[_ _ user id]  (re-matches #"[a-z]+://(twitter.com)/(.+)/status/(.+)"   "http://twitter.com/bradfitz/status/562360748727611392")]
  [:id id
   :user user])


(defn parse [url]
  (let [[_ schema domain path queryparam]  (re-matches #"([a-z]+)://([^/]+)(/[^\?]*)[\?]?(.*)"   url)]
    {:schema schema
     :domain domain
     :path path
     :queryparam queryparam}))

(parse "http://twitter.com/bradfitz/status/562360748727611392")
(parse "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
(parse "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users")

(defn parse-keys [input]
  (map str/trim (str/split input #";")))

(def arr-1 (first (parse-keys "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);")))
(def arr-2 (second (parse-keys "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);")))
(def arr-3 (nth (parse-keys "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);") 2))

(defn get-vals [input-text]
  (let [[_ kkey vval] (re-matches #"(.+)\((.+)\)" input-text)
        [_ extra] (re-matches #".*\?(.*)" vval)
        replaced (str/replace vval (re-pattern (str "\\?" extra)) "(.*)")]
    [kkey vval extra replaced]))

(get-vals arr-2)
(get-vals arr-3)


(re-matches #".*\?(.*)" "list=?type")

(defn parse-str-keys [strn arr-keys]
  (let [a (parse strn)
        [k1 _ key-name rexp] arr-keys
        tested-val ((keyword k1) a)
        [_ matched] (re-matches (re-pattern (str ".*" rexp)) tested-val)]
    {(keyword key-name) matched}))

(parse-str-keys "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1" (get-vals arr-2))
(parse-str-keys "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1" (get-vals arr-3))

(parse-str-keys "http://twitter.com/bradfitz/status/562360748727611392" (last (parse-keys "host(twitter.com); path(?user/status/?id);")))
;; (let [host "host(dribbble.com)"
;;       path "path(shots/?id)"
;;       queryparam "123"]
;;   (match [host path queryparam]
;;          ["host(dribbble.com)" "path(shots/?id)" _] 1))


(def pat (re-pattern "http[s]?://dribbble.com/shots/(.*)\\?list=users&offset=(.+)"))
(re-matches pat "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")

(let [host "dribbble.com"
      path "shots/"
      pat (str "http[s]?://" host "/" path "(.*)\\?list=users&offset=(.+)")]
  (re-matches (re-pattern pat) "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"))

(def dribbble (Pattern. "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);"))
(recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
;; => [[:id "1905065-Travel-Icons-pack"] [:offset "1"]]
(recognize dribbble "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
;; => nil ;; host mismatch
(recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users")
;; => nil ;; offset queryparam missing

(def dribbble2 (Pattern. "host(dribbble.com); path(shots/?id); queryparam(offset=?offset); queryparam(list=?type);"))
