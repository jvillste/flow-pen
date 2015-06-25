(ns flow-pen.core
  (:require (flow-gl.gui [drawable :as drawable]
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


(def pen-event-listener
  (proxy [PenListener] []
    (penLevelEvent [^PLevelEvent ev]
      (let [level-to-tuple 
            (fn [^PLevel level]
              (condp = (. level getType) 
                PLevel$Type/X {:x (. level value)}
                PLevel$Type/Y {:y (. level value)}
                PLevel$Type/PRESSURE {:p (. level value)}
                {}))
            values (reduce merge {} (map level-to-tuple (. ev levels)))]
        (println (:x values) (:y values) (:p values))))
    (penButtonEvent [ev])
    (penKindEvent [ev])
    (penScrollEvent [ev])
    (penTock [millis])))



(defn text
  ([value]
   (text value [255 255 255 255]))

  ([value color]
   (drawable/->Text (str value)
                    (font/create "LiberationSans-Regular.ttf" 15)
                    color)))

(defn paint-view [view-context state]
  (text "as fasf asdf asf asdf asdf ads faas fas fasdf"))

(defn paint [view-context]
  {:local-state {}
   :view #'paint-view})


(defn start []
  (.start (Thread. (fn []
                     (gui/start-control paint (fn [canvas]
                                                (let [pen-manager (new PenManager canvas)]
                                                  (.. pen-manager pen (addListener pen-event-listener)))))))))


#_(defn start2 []
  (let [frame (Frame. "Juu")
        caps (new GLCapabilities (GLProfile/get GLProfile/GL2))
        chooser (new DefaultGLCapabilitiesChooser)]
    (. caps setDoubleBuffered true)
    (. caps setHardwareAccelerated true)
    (. caps setSampleBuffers true)
    (. caps setNumSamples 4)
    (let [gl-canvas (new GLCanvas caps chooser nil nil)
          pen-manager (new PenManager gl-canvas)]
      (. gl-canvas addGLEventListener gl-event-listener)
      (.. pen-manager pen (addListener pen-event-listener))
      (. frame add gl-canvas)
      (let [pixels (int-array 256)
            image (. (Toolkit/getDefaultToolkit) createImage (new MemoryImageSource 16 16 pixels 0 16))
            cursor (. (Toolkit/getDefaultToolkit) createCustomCursor image (new Point 0 0) "invisibleCursor")]
        (. frame setCursor cursor))
      (. frame setSize 300 300)
      (. frame
         (addWindowListener
          (proxy [WindowAdapter] []
            (windowClosing [event]
              (println "closing")
              (. System exit 0)))))
      (println "showing")
      (.setVisible frame  true))))


