(require '[babashka.http-client :as http]
         '[org.httpkit.server :as hk])

(use 'clojure.java.browse)

(def config-path (fs/path (fs/xdg-config-home) "strava"))

(def api-config
  (let [f (fs/path config-path "api.edn")]
    (merge
     {:url "https://www.strava.com/api/v3"
      :client-id 109202
      :access-token ""
      :refresh-token ""
      :local-oauth-server-port 3172}
     (when (fs/exists? f)
       (edn/read-string (slurp (fs/unixify f)))))))


(defn oauth-app [req]
  {:status 200
   :headers {"Content-Type" "text/plain"}
   :body "got it!"})

(comment
  (def oauth-server (hk/run-server oauth-app {:port 3172}))
  (oauth-server)
  )

(defn login []
  (let [srv (hk/run-server oauth-app {:ip "127.0.0.1" :port (api-config :local-oauth-server-port)})]
    (browse-url (str  "https://www.strava.com/oauth/authorize?"
                      "client_id=" (api-config :client-id)
                      "&redirect_uri=" (java.net.URLEncoder/encode (format "http://localhost:%d/authorize" (api-config :local-oauth-server-port)))
                      "&response_type=code&scope=read_all,activity:read_all"))
    (srv)))

(defn strava-get [endpoint params]
  (->
   (http/get (str strava-api-url endpoint)
             {:headers {
                        "Accept" "application/json"
                        "Authorization" (str "Bearer " strava-api-token)
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


(http/get (str strava-api-url "/athlete/activities"))

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
