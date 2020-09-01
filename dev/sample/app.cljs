(ns sample.app
  (:require [goog.dom :as gdom]
            [reagent.dom :as rd]
            [re-frame.core :as rf]
            [reagent.core :as r]))

(rf/reg-event-db :initialize (fn [_ _] {::counter 0}))
(rf/reg-event-db :inc (fn [{::keys [counter]} _] {::counter (inc counter)}))
(rf/reg-event-db :dec (fn [{::keys [counter]} _] {::counter (dec counter)}))
(rf/reg-sub :counter (fn [{::keys [counter]} _] counter))

(defn current-state
  []
  [:h1
   (pr-str @(rf/subscribe [:counter]))])

(defn controls
  []
  [:<>
   [:button {:on-click #(rf/dispatch [:dec])}
    "-"]
   [:button {:on-click #(rf/dispatch [:inc])}
    "+"]])

(defn ui
  []
  [:main
   [current-state]
   [controls]])

(defn ^:export start
  [target]
  (let [el (gdom/getElement target)]
    (rf/dispatch-sync [:initialize])
    (rd/render [ui] el)))
