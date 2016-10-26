(ns test-dribbble.core
  (:require [clojure.string :as str]
            [clojure.data.json :as json]))

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
(def access-token "d967dfcaa929b629ff6cf84c437dceea795dd3ba49260225dc5d03672446912e")
(defn followers-url [user] (str "users/" user "/followers"))
(defn shots-url [user] (str "users/" user "/shots"))
(defn shot-likes-url [shot] (str "shots/" shot "/likes"))
(defn api-url [part] (str "https://api.dribbble.com/v1/" part "?access_token=" access-token))

(api-url (followers-url "simplebits"))
(def data (json/read-str (-> "simplebits" followers-url api-url slurp)))

(json/pprint-json data)
(count data) ;; собрать follower_ids в массив
(def -username (get-in (first data) ["follower" "username"]))

(defn get-shots [username] (json/read-str (-> username shots-url api-url slurp)))
(def shots (get-shots "Fireart-d"))

;; (defn get-shots-likes [shots]
;;     (map #(get % "likes_count") shots))

(defn get-shots-likes [shot]
  (json/read-str (-> shot shot-likes-url api-url slurp)))

(get-shots-likes 2810652)
(json/pprint-json (get-shots-likes 2810652))

(defn get-followers [username]
  (let [data (json/read-str (-> username followers-url api-url slurp))]
   (map #(get-in % ["follower" "username"]) data)))

(get-followers "Fireart-d")

(defn append-username [users-map username]
  (if (contains? users-map username)
    (update users-map username inc)
    (assoc users-map username 1)))

(defn aggregate-likers [username] ;; "Fireart-d"
  "returns "
  (let [coll {}
        followers (get-followers username)
        first-follower (last followers)
;;         follower-shots (get-shots first-follower) ;; (get-shots "chris_sukovich")
        follower-shots  (get-shots "chris_sukovich")
        shot-id (get (first follower-shots) "id")
        likes (get-shots-likes shot-id)
        likers (map #(get-in % ["user" "username"]) likes)]
    (reduce append-username coll likers)))

(aggregate-likers "Fireart-d")

;; (def likers ["BrettCallaghan" "AldoHysenaj" "leolu" "angerka" "huseyinemanet" "angelalejandro" "tangxiangle" "DesignAvenger" "abhishek_omninos" "Coredevs" "xalion" "JasonZjc"])
;; (def likers2 ["BrettCallaghan" "AldoHysenaj" "leolu" "DesignAvenger" "abhishek_omninos" "Coredevs" "xalion" "JasonZjc"])



;; (def global-likers (reduce append-username {} likers))
;; (reduce append-username global-likers likers2)

;; (map #(get-in % ["user" "username"]) likes)
;; (json/pprint-json likes)
