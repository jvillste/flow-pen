(ns flow-pen.simple
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
                         [transformer :as transformer]
                         [window :as window]
                         [renderer :as renderer])
            (flow-gl.opengl.jogl [quad :as quad]
                                 [render-target :as render-target]
                                 [opengl :as opengl])
            (flow-gl.tools [profiler :as profiler]
                           [trace :as trace])
            (flow-gl.graphics [font :as font])
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [window :as jogl-window]
                                 [triangle-list :as triangle-list]
                                 [textured-quad :as textured-quad]
                                 [texture :as texture])
            (flow-gl.graphics [buffered-image :as buffered-image]
                              [font :as font]
                              [text :as text]))
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
           [jpen.owner.multiAwt AwtPenToolkit]
           [jpen PButtonEvent PenManager PKindEvent PLevelEvent PScrollEvent PLevel PLevel$Type]))


(defn pen-event-listener [pen-event-channel]
  (proxy [PenListener] []
    (penLevelEvent [^PLevelEvent event]
      (async/>!! pen-event-channel
                 (reduce (fn [values level]
                           (let [key (condp = (.getType level) 
                                       PLevel$Type/X :x
                                       PLevel$Type/Y :y
                                       PLevel$Type/PRESSURE :p
                                       (.getType level))]
                             (assoc values key (.value level))))
                         {}
                         (.levels event))))
    (penButtonEvent [event])
    (penKindEvent [event])
    (penScrollEvent [event])
    (penTock [millis])))



(defn text
  ([value]
   (text value [255 255 255 255]))

  ([value color]
   (drawable/->Text (str value)
                    (font/create "LiberationSans-Regular.ttf" 15)
                    color)))

(def paint-transformer
  {:transformer (fn [layout gpu-state state]
                  (let [gl (:gl gpu-state)
                        width 400
                        height 400]

                    [(if-let [render-target (:paint-render-target-2 gpu-state)]
                       (assoc (drawable/->Quad ["texture" (:texture render-target)]
                                               []
                                               quad/fragment-shader-source
                                               0 0 width height)
                              :time (java.util.Date.))
                       layout)
                     gpu-state
                     state]))
   
   :destructor (fn [state gl])})

(defn paint-view [view-context state]
  (-> (text state)
      (assoc :transformer (assoc paint-transformer
                                 :id :transformer-2)
             ))
  #_(l/absolute (assoc (drawable/->Rectangle 10 10 [255 255 255 255])
                       :x (-> state :pen-state :x)
                       :y (-> state :pen-state :y)))
  #_(text (str "pen:" (:pen-state state))))

(defn ensure-render-target [render-target width height gl]
  (if render-target
    (if (and (= width
                (:width render-target))
             (= height
                (:height render-target)))
      render-target
      (do (render-target/delete render-target gl)
          (render-target/create width height gl)))
    (render-target/create width height gl)))

  (def paint-fragment-shader-source "
  #version 140

  in vec2 texture_coordinate;

  uniform sampler2D texture;
  uniform float x;
  uniform float y;
  uniform float p;

  out vec4 outColor;

  void main() {
  vec4 color = texture(texture, texture_coordinate);
  
  float texture_x = int(texture_coordinate.x * 400.0);
  float texture_y = int((1 - texture_coordinate.y) * 400.0);

  float distance = sqrt((texture_x - x)*(texture_x - x) + (texture_y - y)*(texture_y - y));
  

  vec4 paint_color = vec4(0.0, 1.0, 0.0, 1.0);

  int radius = int(20.0 * p) + 3;
  int feather = 4;

  if(distance < radius - feather)
  {

  outColor = p*paint_color + (1-p) * color;

  } else if(distance < radius) {

  float ratio = (1 - (distance - (radius - feather)) / feather) * p;
  outColor = ratio * paint_color + (1 - ratio) * color;

  }else{
  //discard;
  outColor = vec4(color.r , color.g, color.b, 1.0);;

  }

  }

")

  (def stroke-fragment-shader-source "
  #version 140

  in vec2 texture_coordinate;

  uniform isampler1D pen_states;

  out vec4 outColor;

  void main() {
  vec4 color = texture(texture, texture_coordinate);
  
  float texture_x = int(texture_coordinate.x * 400.0);
  float texture_y = int((1 - texture_coordinate.y) * 400.0);

  int x = texture1d(pen_states,0);
  int y = texture1d(pen_states,1);
  float p = texture1d(pen_states, 2) / 1024;
  float distance = sqrt((texture_x - x) * (texture_x - x) + (texture_y - y)*(texture_y - y));
  
  vec4 paint_color = vec4(0.0, 1.0, 0.0, 1.0);

  int radius = int(20.0 * p) + 3;
  int feather = 4;

  if(distance < radius - feather)
  {

  outColor = p*paint_color + (1-p) * color;

  } else if(distance < radius) {

  float ratio = (1 - (distance - (radius - feather)) / feather) * p;
  outColor = ratio * paint_color + (1 - ratio) * color;

  }else{

  outColor = vec4(color.r , color.g, color.b, 1.0);;

  }

  }

")

#_(defn render-stroke [gpu-state gl pen-states]
  (let [width 400
        height 400

        pen-states-texture (or (:stroke-pen-states-texture gpu-state)
                               (texture/create-texture-object gl))

        _ (texture/load-1d-int gl pen-states-texture 9 [10 10 500
                                                        100 100 1024
                                                        10 100 500])
        
        renderers (renderer/map-for-renderers renderer/start-frame gl (:renderers gpu-state))
        renderers (render-target/render-to render-target-2 gl
                                           (renderer/render-frame-drawables
                                            [(drawable/->Quad ["texture" (:texture render-target-1)]
                                                              [:1f "resolution" width
                                                               :1f "x" (:x pen-state)
                                                               :1f "y" (:y pen-state)
                                                               :1f "p" (:p pen-state)]
                                                              paint-fragment-shader-source
                                                              0 0 width height)]
                                            gl
                                            renderers))
        _ (opengl/clear gl 0 0 0 1)
        renderers (renderer/render-frame-drawables
                   [(drawable/->Quad ["pen_states" ]
                                     []
                                     quad/fragment-shader-source
                                     0 0 400 400)
                    (assoc (text (str "x: " (:x pen-state)))
                           :x 0 :y 0 :width 100 :height 100)]
                   gl renderers)
        
        renderers (renderer/map-for-renderers renderer/end-frame gl renderers)]

    (assoc gpu-state
           :renderers renderers
           :paint-render-target-1 render-target-2
           :paint-render-target-2 render-target-1)))

(defn render-pen-state [gpu-state gl pen-state]
  (let [width 400
        height 400
        render-target-1 (or (:paint-render-target-1 gpu-state)
                            (render-target/create width height gl))
        
        render-target-2 (or (:paint-render-target-2 gpu-state)
                            (render-target/create width height gl))

        ;;_ (println (:x pen-state) (:y pen-state))

        renderers (renderer/map-for-renderers renderer/start-frame gl (:renderers gpu-state))
        renderers (render-target/render-to render-target-2 gl
                                           #_(opengl/clear gl 0 255 0 255)
                                           (renderer/render-frame-drawables
                                            [(drawable/->Quad ["texture" (:texture render-target-1)]
                                                              [:1f "resolution" width
                                                               :1f "x" (:x pen-state)
                                                               :1f "y" (:y pen-state)
                                                               :1f "p" (:p pen-state)]
                                                              paint-fragment-shader-source
                                                              0 0 width height)]
                                            gl
                                            renderers))
        _ (opengl/clear gl 0 0 0 1)
        renderers (renderer/render-frame-drawables
                   [(drawable/->Quad ["texture" #_(:texture gpu-state) (:texture render-target-2)]
                                     []
                                     quad/fragment-shader-source
                                     0 0 400 400)
                    (assoc (text (str "x: " (:x pen-state)))
                           :x 0 :y 0 :width 100 :height 100)]
                   gl renderers)
        
        renderers (renderer/map-for-renderers renderer/end-frame gl renderers)]

    (assoc gpu-state
           :renderers renderers
           :paint-render-target-1 render-target-2
           :paint-render-target-2 render-target-1)))

(defn event-loop []
  (let [pen-event-channel (async/chan)
        pen-state-channel (async/chan 100)
        window (jogl-window/create 400
                                   400
                                   :profile :gl3
                                   :init opengl/initialize
                                   :close-automatically true
                                   :use-awt true
                                   :awt-init (fn [canvas]
                                               (AwtPenToolkit/addPenListener canvas (pen-event-listener pen-event-channel))))]

    (async/go-loop [pen-state {:p 0}]
      (let [levels (async/<! pen-event-channel)
            pen-state (conj pen-state levels)]
        (async/>! pen-state-channel pen-state)
        (recur pen-state)))

    (try
      (loop [gpu-state {:renderers (window/with-gl window gl
                                     [(renderer/create-quad-renderer gl)
                                      (renderer/create-quad-view-renderer gl)])
                        :texture (window/with-gl window gl
                                   (texture/create-for-file "pumpkin.png" gl))}]
        (let [pen-states #_(async/<!! pen-state-channel) (csp/drain pen-state-channel nil)]
          (if pen-states
            (let [gpu-state (window/with-gl window gl
                              (render-pen-state gpu-state gl (last pen-states)))]
              (window/swap-buffers window)
              (recur gpu-state)))))

      (println "exiting")
      (catch Exception e
        (println "exception")
        (window/close window)
        (throw e)))))

(defn start []
  
  #_(.start (Thread. (fn []
                       (trace/untrace-ns 'flow-pen.simple)
                       (trace/trace-var* 'flow-pen.simple/render-pen-state)
                       (trace/with-trace
                         (event-loop)))))

  (.start (Thread. event-loop))

  #_(let [frame (javax.swing.JFrame.)]
      (let [label (javax.swing.JLabel. "Haa")]
        (let [pen-manager (new PenManager label)]
          #_(-> pen-manager
                (.getDevice 1)
                (.setEnabled false))
          (println (jpen.demo.StatusReport. pen-manager))
          (.. pen-manager pen (addListener #_(JPenExample.) #_(FlowPenListener.) #_(listener/pen-event-listener (async/chan 100)))))
        (.add (.getContentPane frame) label)
        (doto frame
          #_(.addWindowListener (proxy [java.awt.event.WindowAdapter] []
                                  (windowClosing [event]
                                    (.dispose frame))))
          (.setSize 100 100)
          (.setVisible true)))))


(def omat 160000)

(def hinta 400000)

(- hinta omat)
