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
           [jpen PButtonEvent PenManager PKindEvent PLevelEvent PScrollEvent PLevel PLevel$Type]))


(defn pen-event-listener [pen-event-channel]
  (let [values (atom {})]
    (proxy [PenListener] []
      #_(penLevelEvent [^PLevelEvent p-level-event]
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
      (penLevelEvent [^PLevelEvent p-level-event]
        (async/put! pen-event-channel
                    (reduce (fn [values level]
                              (let [key (condp = (.getType level) 
                                          PLevel$Type/X :x
                                          PLevel$Type/Y :y
                                          PLevel$Type/PRESSURE :p
                                          (.getType level))]
                                (assoc values key (.value level))))
                            {}
                            (.levels p-level-event))))
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

  out vec4 outColor;

  void main() {
  vec4 color = texture(texture, texture_coordinate);
  
  float texture_x = int(texture_coordinate.x * 400.0);
  float texture_y = int((1 - texture_coordinate.y) * 400.0);
  
  if(texture_x == int(x) && texture_y == int(y))
  {
  outColor = vec4(1.0 , color.g, color.b, 1.0);
  //outColor = vec4(1.0, 1.0, 1.0, 1.0);
  } else {
  outColor = vec4(color.r , color.g, color.b, 1.0);;


  }


  //outColor = vec4(x/400.0, 0, 0, 1);

  //outColor = vec4(texture_coordinate.x, 0, 0, 1);
  }

")

(defn create-paint [pen-event-channel]
  (fn [view-context]
    (async/go-loop []
      (let [pen-states (csp/drain pen-event-channel nil)]
        (if pen-states
          (do (let [pen-state (last pen-states)]
                (gui/with-gl view-context (fn [gpu-state]
                                            (let [gl (:gl gpu-state)
                                                  width 400
                                                  height 400
                                                  render-target-1 (or (:paint-render-target-1 gpu-state)
                                                                      (render-target/create width height gl))
                                                  
                                                  render-target-2 (or (:paint-render-target-2 gpu-state)
                                                                      (render-target/create width height gl))

                                                  gpu-state (render-target/render-to render-target-2 gl
                                                                                     (opengl/clear gl 0 0 0 255)
                                                                                     (-> (assoc gpu-state :drawables [(drawable/->Quad ["texture" (:texture render-target-1)]
                                                                                                                                       [:1f "resolution" width
                                                                                                                                        :1f "x" (:x pen-state)
                                                                                                                                        :1f "y" (:y pen-state)]
                                                                                                                                       paint-fragment-shader-source
                                                                                                                                       0 0 width height)])
                                                                                         (gui/render-drawables)))]

                                              (assoc gpu-state
                                                     :paint-render-target-1 render-target-2
                                                     :paint-render-target-2 render-target-1)))))
              
              (gui/apply-to-state view-context assoc :pen-state (last pen-states))
              (recur)))))

    {:local-state {:pen-state {}}
     :view #'paint-view}))


(defn start-2 []
  (.start (Thread. (fn []
                     (let [pen-event-channel (async/chan 100)]
                       (gui/start-control (create-paint pen-event-channel)
                                          (fn [canvas]
                                            (let [pen-manager (new PenManager canvas)]
                                              (.. pen-manager pen (addListener (pen-event-listener pen-event-channel)))))))))))



(defn render-pen-state [gpu-state window pen-state]
  (window/with-gl window gl
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
                                                                 :1f "y" (:y pen-state)]
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
             :paint-render-target-2 render-target-1))))

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
                                               (let [pen-manager (new PenManager canvas)]
                                                 (.. pen-manager pen (addListener (pen-event-listener pen-event-channel))))))]
    (println "window created")
    (async/go-loop [pen-state {}]
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
        (println "loop")
        (let [pen-states #_(async/<!! pen-state-channel) (csp/drain pen-state-channel nil)]
          (if pen-states
            (let [gpu-state (render-pen-state gpu-state window (last pen-states))] 
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

  (.start (Thread. event-loop)))


