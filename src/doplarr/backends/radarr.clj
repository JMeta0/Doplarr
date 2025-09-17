(ns doplarr.backends.radarr
  (:require
   [clojure.core.async :as a]
   [doplarr.backends.radarr.impl :as impl]
   [doplarr.state :as state]
   [doplarr.utils :as utils]
   [fmnoise.flow :refer [then]]
   [taoensso.timbre :refer [warn]]))

(defn search [term _]
  (utils/request-and-process-body
   impl/GET
   #(map utils/process-search-result %)
   "/movie/lookup"
   {:query-params {:term term}}))

(defn additional-options [_ _ guild-id]
  (a/go
    (let [quality-profiles (a/<! (impl/quality-profiles))
          rootfolders (a/<! (impl/rootfolders))
          tags (when guild-id (a/<! (impl/tags)))
          {:keys [radarr/quality-profile radarr/rootfolder]} @state/config
          default-profile-id (utils/id-from-name quality-profiles quality-profile)
          default-root-folder (utils/id-from-name rootfolders rootfolder)
          notification-tag (when guild-id (utils/find-or-suggest-notification-tag tags guild-id))]
      (when (and quality-profile (nil? default-profile-id))
        (warn "Default quality profile in config doesn't exist in backend, check spelling"))
      (when (and rootfolder (nil? default-root-folder))
        (warn "Default root folder in config doesn't exist in backend, check spelling"))
      {:quality-profile-id
       (cond
         default-profile-id             default-profile-id
         (= 1 (count quality-profiles)) (:id (first quality-profiles))
         :else quality-profiles)
       :rootfolder-id
       (cond
         default-root-folder default-root-folder
         (= 1 (count rootfolders)) (:id (first rootfolders))
         :else rootfolders)
       :discord-notification (if guild-id
                               (list {:name "No Notification" :id 0}
                                     {:name (str "Enable Discord notifications for this server"
                                                (when (and notification-tag (:suggested notification-tag)) " (will create tag)"))
                                      :id 1})
                               0)})))

(defn request-embed [{:keys [title quality-profile-id tmdb-id rootfolder-id]} _]
  (a/go
    (let [rootfolders (a/<! (impl/rootfolders))
          quality-profiles (a/<! (impl/quality-profiles))
          details (a/<! (impl/get-from-tmdb tmdb-id))]
      {:title title
       :overview (:overview details)
       :poster (:remote-poster details)
       :media-type :movie
       :request-formats [""]
       :rootfolder (utils/name-from-id rootfolders rootfolder-id)
       :quality-profile (utils/name-from-id quality-profiles quality-profile-id)})))

(defn request [payload _]
  (a/go
    (let [status (impl/status (a/<! (impl/get-from-tmdb (:tmdb-id payload))))
          rfs (a/<! (impl/rootfolders))
          payload (assoc payload :root-folder-path (utils/name-from-id rfs (:rootfolder-id payload)))
          ;; Handle Discord notification tag
          payload (if (and (= 1 (:discord-notification payload)) (:guild-id payload))
                    (let [guild-id (:guild-id payload)
                          tag-name (utils/discord-notification-tag-name guild-id)
                          tags (a/<! (impl/tags))
                          existing-tag (first (filter #(= (:name %) tag-name) tags))
                          tag-id (if existing-tag
                                   (:id existing-tag)
                                   (a/<! (impl/create-tag tag-name)))]
                      (assoc payload :tag-ids [tag-id]))
                    payload)]
      (if status
        status
        (->> (a/<! (impl/POST "/movie" {:form-params (utils/to-camel (impl/request-payload payload))
                                        :content-type :json}))
             (then (constantly nil)))))))
