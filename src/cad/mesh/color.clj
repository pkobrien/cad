(ns cad.mesh.color
  (:require [bardo.ease :as be]
            [bardo.interpolate :as bi]
            [thi.ng.color.core :as col]
            [thi.ng.geom.core :as g]
            [thi.ng.math.core :as m]
            [cad.mesh.ops :as op]))


; ==============================================================================
; Face Color Functions

(defn get-face-color- [mesh]
  (let [mesh (op/compute-face-normals mesh)
        get-fc (fn [mesh face]
                 (let []
                   ))]
    [mesh get-fc]))

(defn get-face-color-hsva [hue sat val alpha]
  (let [color @(col/as-rgba (col/hsva hue sat val alpha))]
    (fn [mesh] [mesh (fn [_ _] color)])))

;(op/colorize (mc/get-face-color-hsva 0.25 0.5 0.5 1.0))

(defn get-face-color-abs-normal [mesh]
  (let [mesh (op/compute-face-normals mesh)
        get-fc (fn [mesh face]
                 (let [[r g b] (mapv op/abs (g/face-normal mesh face))
                       alpha 1.0]
                   [r g b alpha]))]
    [mesh get-fc]))

(defn get-face-color-abs-normal-invert [mesh]
  (let [mesh (op/compute-face-normals mesh)
        get-fc (fn [mesh face]
                 (let [normal (g/face-normal mesh face)
                       get-color (fn [n] (- 1.0 (op/abs n)))
                       color (-> (mapv get-color normal) (conj 1.0))]
                   color))]
    [mesh get-fc]))

(defn get-face-color-average-complementary-normal [mesh]
  (let [mesh (op/compute-face-normals mesh)
        get-fc (fn [mesh face]
                 (let [[x y z] (mapv op/abs (g/face-normal mesh face))
                       average (/ (+ x y z) 3.0)
                       comp? (neg? (apply + (g/face-normal mesh face)))
                       color (col/as-rgba (col/hsva average 1.0 1.0))
                       color (if comp? (col/complementary color) color)]
                   @color))]
    [mesh get-fc]))

(defn get-face-color-average-complementary-plus-normal [mesh]
  (let [mesh (op/compute-face-normals mesh)
        get-fc (fn [mesh face]
                 (let [[x y z] (mapv op/abs (g/face-normal mesh face))
                       average (/ (+ x y z) 3.0)
                       comp? (neg? (apply + (g/face-normal mesh face)))
                       hue (- 1.0 x)
                       sat (- 1.0 y)
                       val (- 1.0 z)
                       color (col/as-rgba (col/hsva average hue sat val))
                       ;color (col/as-rgba (col/hsva average x y z))
                       color (if comp? (col/complementary color) color)]
                   @color))]
    [mesh get-fc]))

(defn get-face-color-blend-edge-neighbors [mesh]
  (let [get-fc (fn [mesh face]
                 (let [fcolors (:fcolors mesh)
                       old-color (col/rgba (fcolors face))
                       neighbors (op/face-edge-neighbors mesh face)
                       neighbor-colors (mapv #(col/rgba (fcolors %)) neighbors)
                       blend #(col/blend %1 %2 0.25)
                       color (reduce blend old-color neighbor-colors)]
                   @color))]
    [mesh get-fc]))

(defn get-face-color-area-max [mesh]
  (let [mesh (op/calc-face-area-map mesh)
        get-fc (fn [mesh face]
                 (let [face-area (get-in mesh [:face-area :map face])
                       max-area (get-in mesh [:face-area :max])
                       hue (/ face-area max-area)
                       color (col/as-rgba (col/hsva hue 1.0 1.0 1.0))]
                   @color))]
    [mesh get-fc]))

(defn get-face-color-area-max-invert [mesh]
  (let [mesh (op/calc-face-area-map mesh)
        get-fc (fn [mesh face]
                 (let [face-area (get-in mesh [:face-area :map face])
                       max-area (get-in mesh [:face-area :max])
                       hue (- 1.0 (/ face-area max-area))
                       color (col/as-rgba (col/hsva hue 1.0 1.0 1.0))]
                   @color))]
    [mesh get-fc]))

(defn get-face-color-area-max-normal [mesh]
  (let [mesh (op/calc-face-area-map mesh)
        mesh (op/compute-face-normals mesh)
        get-fc (fn [mesh face]
                 (let [[x y z] (mapv op/abs (g/face-normal mesh face))
                       face-area (get-in mesh [:face-area :map face])
                       max-area (get-in mesh [:face-area :max])
                       hue (/ face-area max-area)
                       sat (max x y z 0.4)
                       color (col/as-rgba (col/hsva hue sat 1.0 1.0))]
                   @color))]
    [mesh get-fc]))

(defn get-face-color-area-mod1 [mesh]
  (let [mesh (op/calc-face-area-map mesh)
        get-fc (fn [mesh face]
                 (let [face-area (get-in mesh [:face-area :map face])
                       hue (mod face-area 1)
                       color (col/as-rgba (col/hsva hue 1.0 1.0 1.0))]
                   @color))]
    [mesh get-fc]))

(defn get-face-color-area-mod10 [mesh]
  (let [mesh (op/calc-face-area-map mesh)
        get-fc (fn [mesh face]
                 (let [face-area (get-in mesh [:face-area :map face])
                       hue (mod (* 10 face-area) 1)
                       color (col/as-rgba (col/hsva hue 1.0 1.0 1.0))]
                   @color))]
    [mesh get-fc]))

(defn get-face-color-area-distance [mesh]
  (let [mesh-centroid (g/centroid mesh)
        mesh (op/calc-face-area-map mesh)
        mesh (op/calc-face-distance-map mesh mesh-centroid)
        get-fc (fn [mesh face]
                 (let [[x y z] (mapv op/abs (g/face-normal mesh face))
                       delta (- (max x y z) (min x y z))
                       face-area (get-in mesh [:face-area :map face])
                       face-area-mod10 (mod (* 10 face-area) 1)
                       min-area (get-in mesh [:face-area :min])
                       max-area (get-in mesh [:face-area :max])
                       face-dist (get-in mesh [:face-dist :map face])
                       min-dist (get-in mesh [:face-dist :min])
                       max-dist (get-in mesh [:face-dist :max])
                       ;hue (m/map-interval face-area min-area max-area 0.8 1.0)
                       hue (m/map-interval face-dist min-dist max-dist 0.5 1.0)
                       sat (m/map-interval face-area-mod10 0.0 1.0 0.2 1.0)
                       val (m/map-interval delta 0.0 1.0 0.3 0.9)
                       color (col/as-rgba (col/hsva hue sat val 1.0))]
                   @color))]
    [mesh get-fc]))

(defn get-face-color-new-01 [mesh]
  (let [mesh (op/compute-face-normals mesh)
        get-fc (fn [mesh face]
                 (let [[x y z] (mapv op/abs (g/face-normal mesh face))
                       hue (min x y z)
                       sat (max x y z)
                       val (- 1.0 (max x y z))
                       color (col/as-rgba (col/hsva hue sat val 1.0))]
                   @color))]
    [mesh get-fc]))
