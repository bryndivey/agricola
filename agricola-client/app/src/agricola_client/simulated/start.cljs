(ns agricola-client.simulated.start
  (:require [io.pedestal.app :as app]
            [io.pedestal.app.render.push.handlers.automatic :as d]
            [agricola-client.start :as start]
            [agricola-client.simulated.services :as services]
            [agricola-client.rendering :as rendering]
            [goog.Uri]
            ;; This needs to be included somewhere in order for the
            ;; tools to work.
            [io.pedestal.app-tools.tooling :as tooling]))

(defn param [name]
  (let [uri (goog.Uri. (.toString  (.-location js/document)))]
    (.getParameterValue uri name)))

(defn ^:export main []
  ;; Create an application which uses the data renderer. The :data-ui
  ;; aspect is configured to run this main function. See
  ;;
  ;; config/config.edn
  ;;
  (let [app (start/create-app (if (= "auto" (param "renderer"))
                                d/data-renderer-config
                                (rendering/render-config)))]
    (app/consume-effects (:app app) services/services-fn)))
