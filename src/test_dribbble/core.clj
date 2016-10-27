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
(recognize dribbble2 "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
;; (use 'clojure.test)
;; (test-ns 'test-dribbble.core)

(def access-token "d967dfcaa929b629ff6cf84c437dceea795dd3ba49260225dc5d03672446912e")
(defn followers-url [user] (str "users/" user "/followers"))
(defn shots-url [user] (str "users/" user "/shots"))
(defn shot-likes-url [shot] (str "shots/" shot "/likes"))
(defn api-url [part] (str "https://api.dribbble.com/v1/" part "?access_token=" access-token))

;; (api-url (followers-url "simplebits"))
;; (def data (json/read-str (-> "simplebits" followers-url api-url slurp)))

(defn get-shots [username] (when username (json/read-str (-> username shots-url api-url slurp))))
;; (def shots (get-shots "Fireart-d"))

(defn get-shots-likes [shot]
  "returns likes struct for given shot"
  (when shot
    (json/read-str (-> shot shot-likes-url api-url slurp))))

;; (get-shots-likes 2810652)
;; (json/pprint-json (get-shots-likes 2810652))

(defn get-followers [username]
  "returns vector of followers usernames"
  (let [data (json/read-str (-> username followers-url api-url slurp))]
   (map #(get-in % ["follower" "username"]) data)))

;; (get-followers "Fireart-d")

(defn append-username [users-map username]
  "inc counter for existing or append new username"
  (if (contains? users-map username)
    (update users-map username inc)
    (assoc users-map username 1)))

;; (def sample-shots (read-string (slurp "resources/shots.edn")))


(defn aggregate-follower-shots [shots coll]
  "returns aggregated for followers and likes count for shot"
  (let [shot (first shots)
        shot-id (get shot "id")
        likes (get-shots-likes shot-id)
        likers (map #(get-in % ["user" "username"]) likes)]
      (if (empty? shots)
        coll
        (recur (rest shots) (reduce append-username coll likers)))))

(defn get-followers-likes2 [username]
  "returns full map of followers with likes count for their shots"
  (let [ followers (get-followers username)]
    (loop [followers (get-followers username)
           acc {}]
      (let [follower (first followers)]
        (if (empty? followers)
          acc
          (recur (rest followers) (aggregate-follower-shots (get-shots follower) acc)))))))

(defn top-10-likers [username]
  (let [likes-counted (get-followers-likes2 username)]
    (take 10 (reverse (sort-by val likes-counted)))))

(top-10-likers "Fireart-d")
;; (["qpoziomek" 10] ["YZ0117" 9] ["sThig" 6] ["karliszarins" 6] ["basovdesign" 6] ["CoupleInTheShuttle" 6] ["worawaluns" 5] ["mlsdev" 5] ["XC_Design" 5] ["angerka" 4])

