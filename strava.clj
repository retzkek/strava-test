(ns strava
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.core.async :refer [chan >!! <!!]]
            [babashka.http-client :as http]
            [clojure.pprint :refer [pprint]]
            [cheshire.core :as json]
            [org.httpkit.server :as hk]
            [ring.util.codec :as codec])
  (:use [clojure.java.browse]))

(def config-path (fs/path (fs/xdg-config-home) "strava" "api.edn"))

(def default-config
  {:url "https://www.strava.com/api/v3"
    :client-id  "109202"
    :client-secret ""
    :access-token ""
    :access-expires-at 0
    :refresh-token ""
    :local-oauth-server-port 3172})

(defn load-config []
  (merge
   default-config
   (when (fs/exists? config-path)
     (edn/read-string (slurp (fs/unixify config-path))))))

(def api-config (atom (load-config)))

(defn write-config []
  (spit (fs/unixify config-path) @api-config))

(defn init []
  (let [conf {:client-id
              (do
                (print "Client ID: ")
                (flush)
                (read-line))
              :client-secret
              (do
                (print "Client secret: ")
                (flush)
                (read-line))}]
    (swap! api-config merge conf)
    (write-config)))

(def oauth-done (chan))

(defn oauth-app [req]
  (pprint req)
  (case (:uri req)
    "/ping"
    {:status 200
     :headers {"Content-Type" "text/plain"}
     :body "hello"}
    "/authorize"
    (do
      (>!! oauth-done (codec/form-decode (req :query-string)))
      {:status 200
       :headers {"Content-Type" "text/plain"}
       :body "got it! You can close this window."})
    {:status 404
     :headers {"Content-Type" "text/plain"}
     :body "404 not found"}))

(comment
  (def oauth-server (hk/run-server oauth-app {:port 3172}))
  (oauth-server)
  )

(defn get-refresh-token [code]
  (try
    (->
     (http/post "https://www.strava.com/api/v3/oauth/token"
                {:form-params {"client_id" (@api-config :client-id)
                               "client_secret" (@api-config :client-secret)
                               "code" code
                               "grant_type" "authorization_code"}
                 :headers {"Accept" "application/json"}})
     :body
     (json/parse-string true))
    (catch Exception e (pprint (:body (ex-data e))))))

(defn save-tokens [& {:keys [refresh_token access_token expires_at]}]
  (swap! api-config merge {:access-token access_token
                            :access-expires-at expires_at
                            :refresh-token refresh_token})
  (write-config))

(defn login []
  (let [srv (hk/run-server oauth-app {:ip "127.0.0.1" :port (@api-config :local-oauth-server-port)})]
    (try
      (do
        (browse-url (str  "https://www.strava.com/oauth/authorize?"
                          "client_id=" (@api-config :client-id)
                          "&redirect_uri=" (java.net.URLEncoder/encode (format "http://localhost:%d/authorize" (@api-config :local-oauth-server-port)))
                          "&response_type=code&scope=read_all,activity:read_all"))
        (-> (<!! oauth-done)
            (get "code")
            get-refresh-token
            save-tokens
            ))
      (catch Exception e (str "exception opening oauth URL: " (.getMessage e)))
      (finally
        (srv)))))

(defn strava-get [endpoint params]
  (->
   (http/get (str (@api-config :url) endpoint)
             {:headers {
                        "Accept" "application/json"
                        "Authorization" (str "Bearer " (@api-config :access-token))
                        }
              :query-params params
              })
   :body
   (json/parse-string true)))

(defn epoch [date]
  (cond
    (number? date) date
    (instance? java.time.Instant date) (.getEpochSecond date)
    :else (.getEpochSecond (java.time.Instant/now))))

(defn strava-get-activities [from to]
  (strava-get "/athlete/activities"
              {:after (epoch from)
               :before (epoch to)}))


(comment
  (strava-get "/athlete" nil)
  ;; => {:summit true,
  ;;     :sex "M",
  ;;     :follower nil,
  ;;     :lastname "Retzke",
  ;;     :profile_medium
  ;;     "https://dgalywyr863hv.cloudfront.net/pictures/athletes/34311679/27579643/1/medium.jpg",
  ;;     :city "Indianapolis",
  ;;     :username "kevin_retzke",
  ;;     :badge_type_id 1,
  ;;     :state "Indiana",
  ;;     :resource_state 2,
  ;;     :updated_at "2023-06-14T19:26:53Z",
  ;;     :firstname "Kevin",
  ;;     :bio nil,
  ;;     :weight 125.191,
  ;;     :id 34311679,
  ;;     :premium true,
  ;;     :friend nil,
  ;;     :country "United States",
  ;;     :profile
  ;;     "https://dgalywyr863hv.cloudfront.net/pictures/athletes/34311679/27579643/1/large.jpg",
  ;;     :created_at "2018-08-30T01:28:29Z"}
  (strava-get-activities 1687040000 nil)
  ;; => clojure.lang.ExceptionInfo: Exceptional status code: 401 user /home/kretzke/src/strava-test/strava.clj:3:4

  )
