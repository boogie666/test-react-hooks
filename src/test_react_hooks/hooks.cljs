(ns test-react-hooks.hooks
  (:require [reagent.core :as r]))


(defn use-state [value]
  (let [r (r/atom value)]
    [r #(reset! r %)]))


(defn use-ref []
  (let [a (volatile! nil)]
    (reify
      IDeref
      (-deref [_]
        @a)
      IFn
      (-invoke [this value]
        (vreset! a value)))))



(defn use-effect [f]
  (let [current-component (r/current-component)
        did-mount (.-componentDidMount current-component)
        will-unmount (.-componentWillUnmount current-component)
        handler (volatile! nil)]

    (set! (.-componentDidMount current-component) (fn [& args]
                                                    (vreset! handler (f))
                                                    (apply did-mount args)))
    (set! (.-componentWillUnmount current-component) (fn [& args]
                                                       (when-let [h @handler] (h))
                                                       (apply will-unmount args)))))


(defn use-watch [atom f]
  (use-effect (fn []
                (f @atom)
                (add-watch atom ::ref-watcher
                            (fn [_ _ _ new-val]
                              (f new-val)))
                #(remove-watch atom ::ref-watcher))))




(defn use-reducer [reducer initial-state]
  (let [val (r/atom initial-state)]
    [val #(swap! val reducer %)]))
