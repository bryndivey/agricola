(ns agricola-client.html-templates
  (:use [io.pedestal.app.templates :only [tfn dtfn tnodes]]))

(defmacro agricola-client-templates
  []
  ;; Extract the 'hello' template from the template file agricola-client.html.
  ;; The 'dtfn' function will create a dynamic template which can be
  ;; updated after it has been attached to the DOM.
  ;;
  ;; To see how this template is used, refer to
  ;;
  ;; app/src/agricola_client/rendering.cljs
  ;;
  ;; The last argument to 'dtfn' is a set of fields that should be
  ;; treated as static fields (may only be set once). Dynamic templates
  ;; use ids to set values so you cannot dynamically set an id.
  {:board-page (dtfn (tnodes "agricola-client.html" "board-page" [[:#slots] [:#players]]) #{})
   :slot (dtfn (tnodes "agricola-client.html" "slot") #{:id})
   :player (dtfn (tnodes "agricola-client.html" "player" [[:.board]]) #{:id})
   :board-space (dtfn (tnodes "agricola-client.html" "board-space") #{:id})})

;; Note: this file will not be reloaded automatically when it is changed.
