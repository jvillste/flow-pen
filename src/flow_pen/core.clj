(ns flow-pen.core
  (:require [clojure.core.async :as async]
            [flow-gl.csp :as csp]
            (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [layouts :as layouts]
                         [layout-dsl :as l]
                         [controls :as controls]
                         [gui :as gui]
                         [events :as events]
                         [layoutable :as layoutable]
                         [transformer :as transformer])
            (flow-gl.opengl.jogl [quad :as quad]
                                 [render-target :as render-target]
                                 [opengl :as opengl])
            (flow-gl.tools [profiler :as profiler]
                           [trace :as trace])
            (flow-gl.graphics [font :as font]))
  (:import [javax.media.opengl.awt GLCanvas]
           [java.awt Frame Toolkit Point Button BorderLayout]
           [java.awt.event WindowListener WindowAdapter]
           [java.awt.image MemoryImageSource]
           [javax.media.opengl GLEventListener GL GL2 GL3 GLCapabilities GLProfile DefaultGLCapabilitiesChooser]
           [javax.media.opengl.awt GLCanvas]
           [java.nio FloatBuffer]
           [com.jogamp.common.nio Buffers]
           [jpen.event PenListener]
           [javax.swing JFrame JButton SwingUtilities]
           [jpen PButtonEvent PenManager PKindEvent PLevelEvent PScrollEvent PLevel PLevel$Type]))


(defn pen-event-listener [pen-event-channel]
  (let [values (atom {})]
    (proxy [PenListener] []
      (penLevelEvent [^PLevelEvent p-level-event]
        (swap! values (fn [values]
                        (reduce (fn [values level]
                                  (let [key (condp = (.getType level) 
                                              PLevel$Type/X :x
                                              PLevel$Type/Y :y
                                              PLevel$Type/PRESSURE :p
                                              (.getType level))]
                                    (assoc values key (.value level))))
                                values
                                (.levels p-level-event))))
        (async/put! pen-event-channel @values))
      (penButtonEvent [p-level-event])
      (penKindEvent [p-level-event])
      (penScrollEvent [p-level-event])
      (penTock [millis]))))



(defn text
  ([value]
   (text value [255 255 255 255]))

  ([value color]
   (drawable/->Text (str value)
                    (font/create "LiberationSans-Regular.ttf" 15)
                    color)))

(defn paint-view [view-context state]
  (text (str "pen:" (:pen-state state))))

(defn create-paint [pen-event-channel]
  (fn [view-context]
    (async/go-loop []
      (let [pen-states (csp/drain pen-event-channel nil)]
        (if pen-states
          (do (gui/apply-to-state view-context assoc :pen-state (last pen-states))
              (recur)))))

    {:local-state {:pen-state {}}
     :view #'paint-view}))


(defn start []
  (.start (Thread. (fn []
                     (let [pen-event-channel (async/chan 100)]
                       (gui/start-control (create-paint pen-event-channel)
                                          (fn [canvas]
                                            (let [pen-manager (new PenManager canvas)]
                                              (.. pen-manager pen (addListener (pen-event-listener pen-event-channel)))))))))))





