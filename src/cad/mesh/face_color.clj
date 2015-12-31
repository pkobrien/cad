(ns cad.mesh.face-color
  (:require [clisk.live :as clisk]
            [thi.ng.color.core :as col]
            [thi.ng.geom.core :as gc]
            [thi.ng.geom.core.utils :as gu]
            [thi.ng.math.core :as math]
            [cad.mesh.core :as mc]
            [cad.mesh.util :as mu]))


; ==============================================================================
; Single Color Functions

(defn hsva [hue sat val alpha]
  (let [color @(col/as-rgba (col/hsva hue sat val alpha))]
    (fn [mesh] [mesh (fn [_ _] color)])))

(defn rgba [r g b alpha]
  (let [color [r g b alpha]]
    (fn [mesh] [mesh (fn [_ _] color)])))


; ==============================================================================
; Face Characteristic Color Functions

(defn area []
  (fn [mesh]
    (let [mesh (mc/assoc-face-area-map mesh)
          min-area (get-in mesh [:face-area :min])
          max-area (get-in mesh [:face-area :max])
          fc (fn [mesh face]
               (let [face-area (get-in mesh [:face-area :map face])
                     norm-area (math/map-interval face-area min-area max-area 0.0 1.0)
                     hue norm-area
                     color (col/as-rgba (col/hsva hue 1.0 1.0 1.0))]
                 @color))]
      [mesh fc])))

(defn circumference []
  (fn [mesh]
    (let [mesh (mc/assoc-face-circ-map mesh)
          min-circ (get-in mesh [:face-circ :min])
          max-circ (get-in mesh [:face-circ :max])
          fc (fn [mesh face]
               (let [face-circ (get-in mesh [:face-circ :map face])
                     norm-circ (math/map-interval face-circ min-circ max-circ 0.0 1.0)
                     hue norm-circ
                     color (col/as-rgba (col/hsva hue 1.0 1.0 1.0))]
                 @color))]
      [mesh fc])))

(defn distance
  ([]
   (distance nil))
  ([point]
   (fn [mesh]
     (let [point (or point (gc/centroid mesh))
           mesh (mc/assoc-face-dist-map mesh point)
           min-dist (get-in mesh [:face-dist :min])
           max-dist (get-in mesh [:face-dist :max])
           fc (fn [mesh face]
                (let [face-dist (get-in mesh [:face-dist :map face])
                      norm-dist (math/map-interval face-dist min-dist max-dist 0.0 1.0)
                      hue norm-dist
                      color (col/as-rgba (col/hsva hue 1.0 1.0 1.0))]
                  @color))]
       [mesh fc]))))

(defn normal-rgb []
  (fn [mesh]
    (let [fc (fn [mesh face]
               (let [[r g b] (mapv #(math/map-interval % -1.0 1.0 0.0 1.0)
                                   (mc/ortho-normal face))
                     alpha 1.0]
                 [r g b alpha]))]
      [mesh fc])))

(defn normal-abs-rgb []
  (fn [mesh]
    (let [fc (fn [mesh face]
               (let [[r g b] (mapv mu/abs (mc/ortho-normal face))
                     alpha 1.0]
                 [r g b alpha]))]
      [mesh fc])))

(defn normal-mod1-rgb []
  (fn [mesh]
    (let [fc (fn [mesh face]
               (let [[r g b] (mapv #(mod % 1) (mc/ortho-normal face))
                     alpha 1.0]
                 [r g b alpha]))]
      [mesh fc])))

(defn normal-cie1931 []
  (fn [mesh]
    (let [fc (fn [mesh face]
               (let [[x y z] (mapv #(math/map-interval % -1.0 1.0 0.0 1.0)
                                   (mc/ortho-normal face))
                     color (col/as-rgba (col/cie1931 [x y z 1.0]))]
                 @color))]
      [mesh fc])))

(defn normal-abs-cie1931 []
  (fn [mesh]
    (let [fc (fn [mesh face]
               (let [[x y z] (mapv mu/abs (mc/ortho-normal face))
                     color (col/as-rgba (col/cie1931 [x y z 1.0]))]
                 @color))]
      [mesh fc])))

(defn normal-sum-hue []
  (fn [mesh]
    (let [fc (fn [mesh face]
               (let [[x y z] (mc/ortho-normal face)
                     max-sum (* 3 (Math/sqrt (/ 1 3)))
                     min-sum (- max-sum)
                     hue (-> (+ x y z) (math/map-interval min-sum max-sum 0.0 1.0))
                     sat 1.0 val 1.0 alpha 1.0
                     color (col/as-rgba (col/hsva hue sat val alpha))]
                 @color))]
      [mesh fc])))

(defn normal-mod1-sum-hue []
  (fn [mesh]
    (let [fc (fn [mesh face]
               (let [[x y z] (mapv #(mod % 1) (mc/ortho-normal face))
                     hue (math/map-interval (+ x y z) 0.0 3.0 0.0 1.0)
                     sat 1.0 val 1.0 alpha 1.0
                     color (col/as-rgba (col/hsva hue sat val alpha))]
                 @color))]
      [mesh fc])))

(defn normal-sum-mod1-hue []
  (fn [mesh]
    (let [fc (fn [mesh face]
               (let [[x y z] (mc/ortho-normal face)
                     hue (-> (+ x y z) (mod 1))
                     sat 1.0 val 1.0 alpha 1.0
                     color (col/as-rgba (col/hsva hue sat val alpha))]
                 @color))]
      [mesh fc])))

(defn normal-max-hue []
  (fn [mesh]
    (let [fc (fn [mesh face]
               (let [[x y z] (mc/ortho-normal face)
                     hue (math/map-interval (max x y z) -1.0 1.0 0.0 1.0)
                     sat 1.0 val 1.0 alpha 1.0
                     color (col/as-rgba (col/hsva hue sat val alpha))]
                 @color))]
      [mesh fc])))

(defn normal-max-abs-hue []
  (fn [mesh]
    (let [fc (fn [mesh face]
               (let [[x y z] (mc/ortho-normal face)
                     hue (math/map-interval (mu/abs (max x y z)) 0.0 1.0 0.0 1.0)
                     sat 1.0 val 1.0 alpha 1.0
                     color (col/as-rgba (col/hsva hue sat val alpha))]
                 @color))]
      [mesh fc])))

(defn normal-mod1-max-hue []
  (fn [mesh]
    (let [fc (fn [mesh face]
               (let [[x y z] (mapv #(mod % 1) (mc/ortho-normal face))
                     hue (math/map-interval (max x y z) 0.0 1.0 0.0 1.0)
                     sat 1.0 val 1.0 alpha 1.0
                     color (col/as-rgba (col/hsva hue sat val alpha))]
                 @color))]
      [mesh fc])))

(defn normal-min-hue []
  (fn [mesh]
    (let [fc (fn [mesh face]
               (let [[x y z] (mc/ortho-normal face)
                     hue (math/map-interval (min x y z) -1.0 1.0 0.0 1.0)
                     sat 1.0 val 1.0 alpha 1.0
                     color (col/as-rgba (col/hsva hue sat val alpha))]
                 @color))]
      [mesh fc])))

(defn normal-min-abs-hue []
  (fn [mesh]
    (let [fc (fn [mesh face]
               (let [[x y z] (mc/ortho-normal face)
                     hue (math/map-interval (mu/abs (min x y z)) 0.0 1.0 0.0 1.0)
                     sat 1.0 val 1.0 alpha 1.0
                     color (col/as-rgba (col/hsva hue sat val alpha))]
                 @color))]
      [mesh fc])))

(defn normal-mod1-min-hue []
  (fn [mesh]
    (let [fc (fn [mesh face]
               (let [[x y z] (mapv #(mod % 1) (mc/ortho-normal face))
                     hue (math/map-interval (min x y z) 0.0 1.0 0.0 1.0)
                     sat 1.0 val 1.0 alpha 1.0
                     color (col/as-rgba (col/hsva hue sat val alpha))]
                 @color))]
      [mesh fc])))


; ==============================================================================
; Face Color Blending Functions

(defn- blend-with-neighbors [f-neigh t mesh face]
  (let [face-color-map (:face-color-map mesh)
        old-color (col/rgba (face-color-map face))
        neighbors (f-neigh mesh face)
        neighbor-colors (mapv #(col/rgba (face-color-map %)) neighbors)
        blend #(col/blend %1 %2 t)
        color (reduce blend old-color neighbor-colors)]
    @color))

(defn blend-with-edge-neighbors [t]
  (fn [mesh]
    (let [mesh (mc/assoc-edge-faces-map mesh)]
      [mesh (partial blend-with-neighbors mc/face-edge-neighbors t)])))

(defn blend-with-vertex-neighbors [t]
  (fn [mesh]
    (let [mesh (mc/assoc-vert-npfs-map mesh)]
      [mesh (partial blend-with-neighbors mc/face-vert-neighbors t)])))

(defn blend-with-vertex-only-neighbors [t]
  (fn [mesh]
    (let [mesh (mc/assoc-edge-faces-map mesh)
          mesh (mc/assoc-vert-npfs-map mesh)]
      [mesh (partial blend-with-neighbors mc/face-vert-only-neighbors t)])))


; ==============================================================================
; Image-Based Functions

(defn clisk-sampler [colorer]
  (fn [mesh]
    (let [sampler (clisk/sampler (clisk/node colorer))
          fc (fn [_ face]
               (let [[x y z] (gu/centroid face)
                     color (sampler [x y z 0])]
                 color))]
      [mesh fc])))


; ==============================================================================
; Experimental

(defn kitchen-sink []
  (fn [mesh]
    (let [mesh-centroid (gc/centroid mesh)
          mesh (mc/assoc-face-area-map mesh)
          mesh (mc/assoc-face-circ-map mesh)
          mesh (mc/assoc-face-dist-map mesh mesh-centroid)
          min-area (get-in mesh [:face-area :min])
          max-area (get-in mesh [:face-area :max])
          min-circ (get-in mesh [:face-circ :min])
          max-circ (get-in mesh [:face-circ :max])
          min-dist (get-in mesh [:face-dist :min])
          max-dist (get-in mesh [:face-dist :max])
          fc (fn [mesh face]
               (let [[x y z] (mapv mu/abs (mc/ortho-normal face))
                     [nx ny nz] (mapv #(mod % 1) (mc/ortho-normal face))
                     delta (- (max x y z) (min x y z))
                     face-area (get-in mesh [:face-area :map face])
                     norm-area (math/map-interval face-area min-area max-area 0.0 1.0)
                     area-tenths (mod (* 10 norm-area) 1)
                     face-circ (get-in mesh [:face-circ :map face])
                     norm-circ (math/map-interval face-circ min-circ max-circ 0.0 1.0)
                     face-dist (get-in mesh [:face-dist :map face])
                     norm-dist (math/map-interval face-dist min-dist max-dist 0.0 1.0)
                     hue 1.0
                     sat 1.0
                     val 1.0
                     ;hue (m/map-interval (+ norm-area norm-circ norm-dist) 0.0 3.0 1.0 0.8)
                     ;hue (m/map-interval (+ norm-area norm-dist) 0.0 2.0 1.0 0.8)
                     ;hue (m/map-interval (+ norm-circ norm-dist) 0.0 2.0 1.0 0.6)
                     ;hue (m/map-interval (+ area-mod1) 0.0 1.0 1.0 0.0)
                     hue (math/map-interval (+ norm-circ norm-dist nx ny nz)
                                            0.0 5.0 0.0 1.0)
                     ;sat (m/map-interval norm-dist 0.0 1.0 1.0 0.6)
                     ;sat (m/map-interval (+ norm-area x) 0.0 2.0 0.4 1.0)
                     ;val (m/map-interval norm-dist 0.0 1.0 0.4 1.0)
                     color (col/as-rgba (col/hsva hue sat val 1.0))
                     ;comp? (odd? (Math/round (* 10 (+ delta))))
                     comp? false
                     ;comp? (when ())
                     color (if comp? (col/complementary color) color)]
                 @color))]
      [mesh fc])))

(defn alien []
  (fn [mesh]
    (let [fc (fn [mesh face]
               (let [[x y z] (mc/ortho-normal face)
                     [nx ny nz] (mapv mu/abs [x y z])
                     average (/ (+ nx ny nz) 3.0)
                     hue average
                     sat (- 1.0 nx)
                     val (- 1.0 ny)
                     alpha (- 1.0 nz)
                     color (col/as-rgba (col/hsva hue sat val alpha))
                     ;color (col/as-rgba (col/hsva average nx ny nz))
                     comp? (neg? (+ x y z))
                     color (if comp? (col/complementary color) color)]
                 @color))]
      [mesh fc])))
