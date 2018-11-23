(ns test-react-hooks.core
  (:require [test-react-hooks.hooks :as h]
            [reagent.core :as r :refer [atom]]))


(enable-console-print!)


(defn use-undo [initial-state]
  (let [history (volatile! ())
        [value set-value] (h/use-state initial-state)]
    [value
     #(do
        (vswap! history conj @value)
        (set-value %))
     #(let [current (first @history)]
        (vswap! history pop)
        (set-value current))]))

(defn use-window-title [atom value]
  (h/use-watch atom #(set! (.-title js/document) (value %))))

(defn counter-reducer [state action]
  (case action
    :inc (update state :counter inc)
    :dec (update state :counter dec)))

(defn counter []
  (let [[val set-val undo] (use-undo 0)
        _ (use-window-title val identity)]
    (fn []
      [:div {:style {:margin 10}}
       [:button {:on-click #(set-val (inc @val))} "+"]
       [:label (str "Counter: " @val)]
       [:button {:on-click #(set-val (dec @val))} "-"]
       [:button {:on-click undo} "Undo"]])))


(defn mouse-tracker [offset]
  (let [[x, set-x] (h/use-state 0)
        [y, set-y] (h/use-state 0)
        div (h/use-ref)
        _ (h/use-effect (fn []
                          (let [handler #(let [x (-> % .-clientX)
                                               y (-> % .-clientY)]
                                           (set-x x)
                                           (set-y y)
                                           (when-let [d @div]
                                             (set! (.-transform (.-style d))
                                                   (str "translate(" (+ offset x) "px, " (+ offset y) "px)"))))]
                            (.addEventListener js/document "mousemove" handler)
                            #(.removeEventListener js/document "mousemove" handler))))]
    (fn []
      [:div {:ref div :style {:position "absolute"
                              :transform "translate(0px, 0px)"}}
       (str @x "px " @y "px")])))


(defn counters-reducers [state action]
  (case action
    :add (update state :counters inc)
    :remove (update state :counters dec)))

(defn hello-world []
  (let [[state dispatch] (h/use-reducer counters-reducers {:counters 3})
        _ (use-window-title state :counters)]
    (fn []
      [:div
       [mouse-tracker 10]
       [mouse-tracker -30]
       [:button {:on-click #(dispatch :add)} "Add counter"]
       [:button {:on-click #(dispatch :remove)} "Remove counter"]
       (for [i (range (:counters @state))]
          ^{:key i}
          [counter])])))

(r/render-component [hello-world]
                    (. js/document (getElementById "app")))

(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)

