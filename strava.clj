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

(defn save-tokens [& {:keys [refresh_token access_token expires_at]}]
  (swap! api-config merge {:access-token access_token
                            :access-expires-at expires_at
                            :refresh-token refresh_token})
  (write-config))

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
     (json/parse-string true)
     save-tokens)
    (catch Exception e (do
                         (print "exception getting refresh token: " (.getMessage e))
                         (pprint (:body (ex-data e)))))))

(defn refresh-token []
  (try
    (->
     (http/post "https://www.strava.com/api/v3/oauth/token"
                {:form-params {"client_id" (@api-config :client-id)
                               "client_secret" (@api-config :client-secret)
                               "refresh_token" (@api-config :refresh-token)
                               "grant_type" "refresh_token"}
                 :headers {"Accept" "application/json"}})
     :body
     (json/parse-string true)
     save-tokens)
    (catch Exception e (do
                         (print "exception getting access token: " (.getMessage e))
                         (pprint (:body (ex-data e)))))))

(defn access-expired? []
  (> (epoch nil) (@api-config :access-expires-at)))

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
  (when (access-expired?) (refresh-token))
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
  (strava-get-activities 1687040000 nil);; => ({:average_watts 62.2,
;;      :moving_time 6799,
;;      :workout_type nil,
;;      :location_state nil,
;;      :timezone "(GMT-05:00) America/Indiana/Indianapolis",
;;      :upload_id_str "9972743470",
;;      :private false,
;;      :start_latlng [39.77491367608309 -86.16309304721653],
;;      :average_heartrate 98.3,
;;      :elev_low 208.2,
;;      :name "Afternoon Ride",
;;      :flagged false,
;;      :trainer false,
;;      :sport_type "Ride",
;;      :has_heartrate true,
;;      :achievement_count 6,
;;      :average_speed 4.091,
;;      :location_city nil,
;;      :total_photo_count 0,
;;      :type "Ride",
;;      :end_latlng [39.77508785203099 -86.1633194424212],
;;      :start_date_local "2023-06-19T13:02:11Z",
;;      :resource_state 2,
;;      :kudos_count 0,
;;      :device_watts false,
;;      :manual false,
;;      :utc_offset -14400.0,
;;      :from_accepted_tag false,
;;      :upload_id 9972743470,
;;      :start_date "2023-06-19T17:02:11Z",
;;      :comment_count 0,
;;      :external_id "garmin_ping_280380475751",
;;      :id 9296838001,
;;      :max_heartrate 134.0,
;;      :athlete_count 1,
;;      :gear_id nil,
;;      :elapsed_time 7619,
;;      :elev_high 222.8,
;;      :display_hide_heartrate_option true,
;;      :commute false,
;;      :photo_count 0,
;;      :suffer_score 16.0,
;;      :distance 27814.8,
;;      :max_speed 6.501,
;;      :has_kudoed false,
;;      :pr_count 0,
;;      :location_country "United States",
;;      :heartrate_opt_out false,
;;      :total_elevation_gain 76.0,
;;      :map
;;      {:id "a9296838001",
;;       :summary_polyline
;;       "ytwqFvv{lOyDIy@JIPKx@F|CE`FwAG}@N{@SMBQvACzEJjACnAS`BIlOFLJ?vC_GRLf@IRRfBLbMNy@zgAYhKKtA_CfJk@Ey@HgBKe@Pu@GeJDgAYi@HQSgASKMk@VEGCaIGGaEPcBjKUb@o@`@k@Fs@EeAWo@_@SPQBsEUmABu@f@}AzAuCrF{AhGc@zDI~CMzAw@jFg@dCi@nBcAtCyA~Cq@`As@h@{AZWY{Bl@i@h@uBlDw@vBsDpEyBfBm@ZqC`A}ATsCm@m@g@o@cAwD{MsA}DiAyAwAo@mCUgDAkHWeAB}C~@yCtAi@FiClAcCp@}@h@mCn@cCBaCu@iAq@o@c@sBaCm@g@cHiIyAqC}@mB[yAUUU@qAPCH?vAWf@URMAqMiIaFgCBy@GOYIIKZTqOiEqAa@O[_AFoBWuFDgDKaAMeBw@aD}@{HYwBa@KS_@DiA]iJk@oCi@i@e@}DwByEyD{@g@aC}@cAy@iAaCu@_AkAiEe@}Bm@mB_E_KgBaEg@w@{AeB}ImFuCoAiE}A_GwC}JyFkHoD{EsDoAk@sC_AyA}@cDmCm@_@iCaCwCyBiEoFmCwDuA_BiIyHmH_L|HdLpCfCtEzE`KvM|@n@rJdIxCnBnFrBdDjCvKfGvIjExDtBhBz@~Bp@|CtA~IpFfC`DrD`JR\\SQDJxBbFb@lArAxFd@tAx@fAz@jBz@r@bCbAr@b@tGhF|CvA^`@d@@xBb@pIr@rAZJN`ABnAT|FNjALtGvB`GNxDC|En@`@TzHlBvFdBDJIf@BPjEvBdFdD`Ad@|CfCp@VJG^aADuAv@IZMd@?HLFv@Rl@pA|Br@|AfHxIbC`CR`@lA|@z@j@lCt@lCI|A_@bB{@`D_AvAu@jAYpCqAxCs@bPVhC\\fAl@rAhBdAvC`DpLn@|AbA~@l@VnBXdAIvCcAx@c@tAiAdAgAfC_DVe@Nq@jAwBjAgBn@m@fBc@NBBTbB]v@k@Ze@|BgFfAeDf@}BfA_HPiB?iAPoCd@_DnA_FlC_F^m@v@u@bAs@l@KfGVXQt@b@dBX~@KZU`@u@vA}JLK|DCFHD~HlAc@^D|AnARFbN?l@KjB?Lc@j@P`BwGb@{BXwJLsSFs@@}NRiNLiRFA?o@uGQwFAaAWa@FWMsCtFE@G[HqNRgCGaB@_DLuCPc@z@Xx@QtADJ_BNiJnEP",
;;       :resource_state 2},
;;      :visibility "everyone",
;;      :athlete {:id 34311679, :resource_state 1},
;;      :kilojoules 423.2}
;;     {:average_watts 399.7,
;;      :moving_time 4357,
;;      :workout_type 0,
;;      :location_state nil,
;;      :timezone "(GMT-05:00) America/Indiana/Indianapolis",
;;      :upload_id_str "9965544677",
;;      :private false,
;;      :start_latlng [39.77541700936854 -86.1630052048713],
;;      :average_heartrate 151.4,
;;      :elev_low 208.2,
;;      :max_watts 636,
;;      :name "Morning Run",
;;      :flagged false,
;;      :trainer false,
;;      :sport_type "Run",
;;      :has_heartrate true,
;;      :achievement_count 3,
;;      :average_speed 2.329,
;;      :location_city nil,
;;      :total_photo_count 1,
;;      :type "Run",
;;      :end_latlng [39.77514040656388 -86.16338892839849],
;;      :start_date_local "2023-06-18T10:25:12Z",
;;      :resource_state 2,
;;      :kudos_count 0,
;;      :device_watts true,
;;      :manual false,
;;      :utc_offset -14400.0,
;;      :from_accepted_tag false,
;;      :upload_id 9965544677,
;;      :start_date "2023-06-18T14:25:12Z",
;;      :comment_count 0,
;;      :external_id "garmin_ping_280178607554",
;;      :id 9290025887,
;;      :weighted_average_watts 392,
;;      :max_heartrate 166.0,
;;      :athlete_count 1,
;;      :gear_id nil,
;;      :elapsed_time 4561,
;;      :elev_high 229.4,
;;      :average_cadence 74.3,
;;      :display_hide_heartrate_option true,
;;      :commute false,
;;      :photo_count 0,
;;      :suffer_score 105.0,
;;      :distance 10145.5,
;;      :max_speed 3.804,
;;      :has_kudoed false,
;;      :pr_count 0,
;;      :location_country "United States",
;;      :heartrate_opt_out false,
;;      :total_elevation_gain 29.0,
;;      :map
;;      {:id "a9290025887",
;;       :summary_polyline
;;       "cwwqFbw{lO}@@cAEg@@EDGfA?hBE`C@\\C`@?d@ELQFOEUDQCKGG@ELEBMDW@KAYMUEOHEREb@@n@CnA@pAEV?TDV?PHX?j@Ch@MZIJCTBb@CJBXEnABVC`@B|AAp@@r@GtAAr@BNLAHILc@d@y@d@eA`@u@RUHBLPH?PCLFJJL@tC?f@Fj@Et@BZAh@HfBHJ?TC@@BCHAl@BJFPDh@IhB@JBd@C~@JhB?PBf@@|@CNBLFF@dADv@Ad@Dh@?FBdB?f@Ej@Bd@Ah@DRFZBn@@RAZ@r@ATKVATBnAn@RVIfE@|@Bh@Al@Ed@O\\Ih@@nCAl@AF?~@AROn@Ej@A|CEv@?v@I|@S^Wv@GNQhAU^ALS`AILERKZCZEXGNOTE^GTOlACd@QdAEvA?`BAT@l@AxABZSdCKn@E^GJ?NHTCPg@nAMf@Sh@SVOb@k@bAg@jAUZIZo@dAg@d@cAzAGBm@~@oAxAI^SZ_@LSLQTq@j@eA|@c@XYVm@\\uEdDy@^[XMLi@PuB`B_@VYNw@j@aAh@u@Vw@^_B`@cAP]P_@H_@Hu@D}@@k@CQGKAWK[Eq@Y]Q}BeBv@gCBS|@oCRw@L[VaBFMJCD@bBbAr@\\ZFb@BPHHH^j@JJPCZKVS\\e@NCZAh@Qp@i@hAi@fBeAl@YZU^Kv@_AbB{A\\_@r@i@NGJKXSTKPQXWx@c@`AaALGTWLI`@g@PKj@q@JInAoARMTc@L_@Vg@lA_Dr@aBX_AHq@J}ADsDFuBHw@b@eBl@gDb@gAf@gCZy@h@aAD{@LkAASC?EH_@|Be@n@QLU`@IAXo@j@iAxAyEr@yALQNc@|@_B\\e@@IDUDo@PiAJoAAQNeB@]H{@?aATeAT}B?KCGIGKCm@LUBkACs@JIEo@ASIa@GuBGaB?g@CmB@y@CKCEEm@GS@uAEyA?mAGwBB[Ao@H]Ki@Bi@AkAKkDG]@}@EeB@_@CKCOQYOGASBOLUn@UZ}A~CGFICAIAa@NcOFu@FO@k@?c@Ic@DqE@QCq@B]H_@BEFCN@XNLDNCb@MZ@TBXAFBBCNgAB_ABQBgHDcCDu@DCX@^Cr@?JDj@CR@",
;;       :resource_state 2},
;;      :visibility "everyone",
;;      :athlete {:id 34311679, :resource_state 1},
;;      :kilojoules 1741.6})


  )
