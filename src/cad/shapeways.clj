(ns cad.shapeways
  (:require [cad.ops :as op]
            [cad.x3d :as x3d]
            [clojure.java.io :as io]
            [clj-time.format :as tf]
            [clj-time.core :as time]
            [thi.ng.color.core :as col]
            [thi.ng.geom.cuboid :as cu]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.gmesh :as gm]
            [thi.ng.geom.core.utils :as gu]
            [thi.ng.geom.mesh.polyhedra :as ph]))


; ==============================================================================
; Shared constants and functions

(defn save-x3d
  [path mesh & {:keys [indent?] :or {indent? false}}]
  (let [now (time/now)
        date (tf/unparse (tf/formatters :rfc822) now)
        year (tf/unparse (tf/formatters :year) now)
        copy (str "Copyright " year " Patrick K. O'Brien")
        meta (array-map
               :creator "Patrick K. O'Brien"
               :created date
               :copyright copy
               :generator "Custom Clojure Code")
        units [(array-map
                 :category "length"
                 :name "millimeters"
                 :conversionFactor "0.001")]]
    (with-open [out (io/writer path)]
      (x3d/write-x3d out mesh :indent? indent? :meta meta))))

(defn seed->mesh
  "Returns a mesh for a seed collection of vertices."
  [seed]
  (g/into (gm/gmesh) seed))


; ==============================================================================
; Face Color Functions

(defn get-face-color-abs-normal [mesh face]
  (let [[r g b] (mapv #(Math/abs %) (g/face-normal mesh face))
        alpha 1.0]
    [r g b alpha]))


; ==============================================================================
; Designs

(defn hexa-kis-cc3-kis
  "
  http://shpws.me/L0c3
  https://www.shapeways.com/model/3dtools/4110302/1/26?key=3be3891e92cfa7cb33ad3a71008c1106
  "
  []
  (let [seed (cu/cuboid -5 10)
        mesh (seed->mesh seed)
        mesh (-> mesh (op/kis #(op/calc-vertex %2 :height 10)))
        mesh (op/catmull-clark mesh)
        mesh (op/catmull-clark mesh)
        mesh (op/catmull-clark mesh)
        mesh (-> mesh (op/kis #(op/calc-vertex %2 :height -0.25)))
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-abs-normal)]
    mesh))

;(time (save-x3d "output/shapeways/hexa-kis-cc3-kis.x3d" (hexa-kis-cc3-kis)))
