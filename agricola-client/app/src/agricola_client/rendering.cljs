(ns agricola-client.rendering
  (:require [domina :as dom]
            [io.pedestal.app.render.push :as render]
            [io.pedestal.app.render.push.templates :as templates]
            [io.pedestal.app.render.push.handlers.automatic :as d])
  (:require-macros [agricola-client.html-templates :as html-templates]))

;; Load templates.

(def templates (html-templates/agricola-client-templates))


(defn render-board [renderer [_ path] transmitter]
  (let [;; The renderer that we are using here helps us map changes to
        ;; the UI tree to the DOM. It keeps a mapping of paths to DOM
        ;; ids. The `get-parent-id` function will return the DOM id of
        ;; the parent of the node at path. If the path is [:a :b :c]
        ;; then this will find the id associated with [:a :b]. The
        ;; root node [] is configured when we created the renderer.
        parent (render/get-parent-id renderer path)
        ;; Use the `new-id!` function to associate a new id to the
        ;; given path. With two arguments, this function will generate
        ;; a random unique id. With three arguments, the given id will
        ;; be associated with the given path.
        id (render/new-id! renderer path)
        ;; Get the dynamic template named :agricola-client-page
        ;; from the templates map. The `add-template` function will
        ;; associate this template with the node at
        ;; path. `add-template` returns a function that can be called
        ;; to generate the initial HTML.
        html (templates/add-template renderer path (:board-page templates))]
    ;; Call the `html` function, passing the initial values for the
    ;; template. This returns an HTML string which is then added to
    ;; the DOM using Domina.
    (dom/append! (dom/by-id parent) (html {:id id}))))

(defn render-slot [renderer [_ path] transmitter]
  (let [parent (render/get-parent-id renderer path)
        id (render/new-id! renderer path)
        html (templates/add-template renderer path (:slot templates))]
    (templates/update-t renderer (butlast path)
                        {:slots 
                         (html {:id id :action "No action" :performed false})})))

(defn update-slot [renderer [_ path _ new-value] transmitter]
  (templates/update-t renderer path {:action (:action new-value)
                                     :performed (:performed new-value)}))

(defn render-message [renderer [_ path _ new-value] transmitter]
  ;; This function responds to a :value event. It uses the
  ;; `update-t` function to update the template at `path` with the new
  ;; values in the passed map.
  (templates/update-t renderer path {:message new-value}))

;; The data structure below is used to map rendering data to functions
;; which handle rendering for that specific change. This function is
;; referenced in config/config.edn and must be a function in order to
;; be used from the tool's "render" view.

(defn render-config []
  [;; All :node-create deltas for the node at :greeting will
   ;; be rendered by the `render-page` function. The node name
   ;; :greeting is a default name that is used when we don't
   ;; provide our own derives and emits. To name your own nodes,
   ;; create a custom derive or emit in the application's behavior.
   [:node-create  [:game :slots] render-board]
   ;; All :node-destroy deltas for this path will be handled by the
   ;; library function `d/default-exit`.
   [:node-destroy   [:game :slots] d/default-exit]
   ;; All :value deltas for this path will be handled by the
   ;; function `render-message`.
   [:node-create [:game :slots :*] render-slot]
   [:node-destroy [:game :slots :*] d/default-exit]
   [:value [:game :slots :*] update-slot]
   
   [:value [:greeting] render-message]])

;; In render-config, paths can use wildcard keywords :* and :**. :*
;; means exactly one segment with any value. :** means 0 or more
;; elements.
