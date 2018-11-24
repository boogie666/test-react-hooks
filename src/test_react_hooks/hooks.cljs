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

(defn use-update [f]
  (let [current-component (r/current-component)
        did-update (.-componentDidUpdate current-component)]

    (set! (.-componentDidUpdate current-component)
          (fn [& args]
            (f)
            (when (fn? did-update)
              (apply did-update args))))))


(defn use-effect [f]
  (let [current-component (r/current-component)
        did-mount (.-componentDidMount current-component)
        will-unmount (.-componentWillUnmount current-component)
        handler (volatile! nil)]

    (set! (.-componentDidMount current-component)
          (fn [& args]
            (let [h (f)]
              (when (fn? h)
                (vreset! handler h)))
            (when (fn? did-mount)
              (apply did-mount args))))

    (set! (.-componentWillUnmount current-component)
          (fn [& args]
            (when (fn? @handler) (@handler))
            (when (fn? will-unmount)
              (apply will-unmount args))))))

(defn use-watch [atom f]
  (use-effect (fn []
                (let [ref-watcher (keyword (gensym "ref-counter"))]
                  (add-watch atom ref-watcher #(f %4))
                  #(remove-watch atom ref-watcher)))))

(defn use-reducer [reducer initial-state]
  (let [val (r/atom initial-state)]
    [val #(swap! val reducer %)]))

