(ns cad.mesh.color
  (:require [clisk.live :as clisk]
            [thi.ng.color.core :as col]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.core.utils :as gu]
            [thi.ng.math.core :as m]
            [cad.mesh.ops :as op]))


; ==============================================================================
; Helper Functions

(defn cb
  "Returns a function that will modify a color."
  [f]
  (fn [rgba]
    @(f (col/rgba rgba))))


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
    (let [mesh (op/calc-face-area-map mesh)
          min-area (get-in mesh [:face-area :min])
          max-area (get-in mesh [:face-area :max])
          fc (fn [mesh face]
               (let [face-area (get-in mesh [:face-area :map face])
                     norm-area (m/map-interval face-area min-area max-area 0.0 1.0)
                     hue norm-area
                     color (col/as-rgba (col/hsva hue 1.0 1.0 1.0))]
                 @color))]
      [mesh fc])))

(defn circumference []
  (fn [mesh]
    (let [mesh (op/calc-face-circ-map mesh)
          min-circ (get-in mesh [:face-circ :min])
          max-circ (get-in mesh [:face-circ :max])
          fc (fn [mesh face]
               (let [face-circ (get-in mesh [:face-circ :map face])
                     norm-circ (m/map-interval face-circ min-circ max-circ 0.0 1.0)
                     hue norm-circ
                     color (col/as-rgba (col/hsva hue 1.0 1.0 1.0))]
                 @color))]
      [mesh fc])))

(defn distance
  ([]
   (distance nil))
  ([point]
   (fn [mesh]
     (let [point (or point (g/centroid mesh))
           mesh (op/calc-face-dist-map mesh point)
           min-dist (get-in mesh [:face-dist :min])
           max-dist (get-in mesh [:face-dist :max])
           fc (fn [mesh face]
                (let [face-dist (get-in mesh [:face-dist :map face])
                      norm-dist (m/map-interval face-dist min-dist max-dist 0.0 1.0)
                      hue norm-dist
                      color (col/as-rgba (col/hsva hue 1.0 1.0 1.0))]
                  @color))]
       [mesh fc]))))

(defn normal []
  (fn [mesh]
    (let [mesh (op/compute-face-normals mesh)
          fc (fn [mesh face]
               (let [[r g b] (mapv #(mod % 1) (g/face-normal mesh face))
                     alpha 1.0]
                 [r g b alpha]))]
      [mesh fc])))

(defn normal-abs []
  (fn [mesh]
    (let [mesh (op/compute-face-normals mesh)
          fc (fn [mesh face]
               (let [[r g b] (mapv op/abs (g/face-normal mesh face))
                     alpha 1.0]
                 [r g b alpha]))]
      [mesh fc])))


; ==============================================================================
; Face Color Blending Functions

(defn- blend-with-neighbors [f-neigh t mesh face]
  (let [fcolors (:fcolors mesh)
        old-color (col/rgba (fcolors face))
        neighbors (f-neigh mesh face)
        neighbor-colors (mapv #(col/rgba (fcolors %)) neighbors)
        blend #(col/blend %1 %2 t)
        color (reduce blend old-color neighbor-colors)]
    @color))

(defn blend-with-edge-neighbors [t]
  (fn [mesh] [mesh (partial blend-with-neighbors op/face-edge-neighbors t)]))

(defn blend-with-vertex-neighbors [t]
  (fn [mesh] [mesh (partial blend-with-neighbors op/face-vertex-neighbors t)]))

(defn blend-with-vertex-only-neighbors [t]
  (fn [mesh] [mesh (partial blend-with-neighbors op/face-vertex-only-neighbors t)]))


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
    (let [mesh-centroid (g/centroid mesh)
          mesh (op/calc-face-area-map mesh)
          mesh (op/calc-face-circ-map mesh)
          mesh (op/calc-face-dist-map mesh mesh-centroid)
          mesh (op/compute-face-normals mesh)
          min-area (get-in mesh [:face-area :min])
          max-area (get-in mesh [:face-area :max])
          min-circ (get-in mesh [:face-circ :min])
          max-circ (get-in mesh [:face-circ :max])
          min-dist (get-in mesh [:face-dist :min])
          max-dist (get-in mesh [:face-dist :max])
          fc (fn [mesh face]
               (let [[x y z] (mapv op/abs (g/face-normal mesh face))
                     [nx ny nz] (mapv #(mod % 1) (g/face-normal mesh face))
                     delta (- (max x y z) (min x y z))
                     face-area (get-in mesh [:face-area :map face])
                     norm-area (m/map-interval face-area min-area max-area 0.0 1.0)
                     area-mod1 (mod (* 10 norm-area) 1)
                     face-circ (get-in mesh [:face-circ :map face])
                     norm-circ (m/map-interval face-circ min-circ max-circ 0.0 1.0)
                     face-dist (get-in mesh [:face-dist :map face])
                     norm-dist (m/map-interval face-dist min-dist max-dist 0.0 1.0)
                     hue 1.0
                     sat 1.0
                     val 1.0
                     ;hue (m/map-interval (+ norm-area norm-circ norm-dist) 0.0 3.0 1.0 0.8)
                     ;hue (m/map-interval (+ norm-area norm-dist) 0.0 2.0 1.0 0.8)
                     ;hue (m/map-interval (+ norm-circ norm-dist) 0.0 2.0 1.0 0.6)
                     ;hue (m/map-interval (+ area-mod1) 0.0 1.0 1.0 0.0)
                     hue (m/map-interval (+ area-mod1 norm-circ norm-dist) 0.0 3.0 1.0 0.0)
                     sat (m/map-interval norm-area 0.0 1.0 0.4 1.0)
                     ;sat (m/map-interval (+ norm-area x) 0.0 2.0 0.4 1.0)
                     val (m/map-interval norm-area 0.0 1.0 0.4 1.0)
                     color (col/as-rgba (col/hsva hue sat val 1.0))
                     ;comp? (odd? (Math/round (* 10 (+ delta))))
                     comp? false
                     color (if comp? (col/complementary color) color)]
                 @color))]
      [mesh fc])))

(defn alien []
  (fn [mesh]
    (let [mesh (op/compute-face-normals mesh)
          fc (fn [mesh face]
               (let [[x y z] (g/face-normal mesh face)
                     [nx ny nz] (mapv op/abs [x y z])
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
