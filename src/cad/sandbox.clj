(ns cad.sandbox
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
            [thi.ng.geom.mesh.polyhedra :as ph]
            [thi.ng.geom.mesh.subdivision :as sd]))


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

;(defn get-face-color [mesh face]
;  (let [normal (g/face-normal mesh face)
;        get-color (fn [n] (Math/abs n)
;                    #_(let [n (if-not (zero? n) n (if (.equals n -0.0) -0.1 0.1))]
;                        (if (pos? n) n
;                                     (+ 1.0 n))))
;        color (-> (mapv get-color normal) (conj 1.0))]
;    color))

(defn get-face-color-abs-normal [mesh face]
  (let [[r g b] (mapv #(Math/abs %) (g/face-normal mesh face))
        alpha 1.0]
    [r g b alpha]))

;(defn get-face-color-abs-normal [mesh face]
;  (let [normal (g/face-normal mesh face)
;        get-color (fn [n] (Math/abs n))
;        color (-> (mapv get-color normal) (conj 1.0))]
;    color))

(defn get-face-color-invert-abs-normal [mesh face]
  (let [normal (g/face-normal mesh face)
        get-color (fn [n] (- 1.0 (Math/abs n)))
        color (-> (mapv get-color normal) (conj 1.0))]
    color))

(defn get-face-color-average-normal [mesh face]
  (let [[x y z] (mapv #(Math/abs %) (g/face-normal mesh face))
        average (/ (+ x y z) 3.0)
        color @(col/as-rgba (col/hsva average 1.0 1.0))]
    color))

(defn get-face-color-average-comp-normal [mesh face]
  (let [[x y z] (mapv #(Math/abs %) (g/face-normal mesh face))
        average (/ (+ x y z) 3.0)
        comp? (neg? (apply + (g/face-normal mesh face)))
        color (col/as-rgba (col/hsva average 1.0 1.0))
        color (if comp? (col/complementary color) color)]
    @color))

(defn get-face-color-average-comp-plus-normal [mesh face]
  (let [[x y z] (mapv #(Math/abs %) (g/face-normal mesh face))
        average (/ (+ x y z) 3.0)
        comp? (neg? (apply + (g/face-normal mesh face)))
        color (col/as-rgba (col/hsva average (- 1.0 x) (- 1.0 y) (- 1.0 z)))
        ;color (col/as-rgba (col/hsva average x y z))
        color (if comp? (col/complementary color) color)]
    @color))

(defn get-face-color-blend-neighbors [mesh face]
  (let [fcolors (:fcolors mesh)
        old-color (col/rgba (fcolors face))
        neighbors (op/face-neighbors mesh face)
        neighbor-colors (mapv #(col/rgba (fcolors %)) neighbors)
        color (reduce #(col/blend %1 %2 0.25) old-color neighbor-colors)]
    @color))

(defn get-face-color-new [mesh face]
  (let [[x y z] (g/face-normal mesh face)
        average (Math/abs (/ (+ x y z) 3.0))
        comp? (or (neg? z) (.equals -0.0 z))
        color (col/as-rgba (col/hsva average 1.0 1.0 1.0))
        ;color (if comp? (col/complementary color) color)
        ]
    @color))


; ==============================================================================
; Operator Tests

(defn ambo-test-01 []
  (let [seed (ph/dodecahedron 10)
        mesh (seed->mesh seed)
        mesh (-> mesh (op/ambo))
        mesh (-> mesh (op/ambo))
        mesh (-> mesh (op/ambo))
        mesh (-> mesh (op/kis :height 2.5 :n-sides #{3}))
        mesh (-> mesh (op/kis :height -10 :n-sides #{5}))
        ;mesh (-> mesh (op/kis :height 0.5 :n-sides #{3}))
        ;mesh (-> mesh (op/ortho :height -2 :n-sides #{4}))
        mesh (op/catmull-clark mesh)
        mesh (op/catmull-clark mesh)
        mesh (op/catmull-clark mesh)
        ;mesh (op/catmull-clark mesh)
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-average-comp-plus-normal)]
    mesh))

(defn ambo-test-02 []
  (let [seed (ph/dodecahedron 10)
        mesh (seed->mesh seed)
        mesh (-> mesh (op/ambo))
        mesh (-> mesh (op/ambo))
        mesh (-> mesh (op/ambo))
        mesh (-> mesh (op/kis :height 3 :n-sides #{5}))
        mesh (-> mesh (op/kis :height -0.05 :n-sides #{3 4}))
        mesh (-> mesh (op/ortho :height 0.05 :n-sides #{4}))
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-abs-normal)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        ]
    mesh))

(defn complexify-test-01 []
  (let [seed (ph/icosahedron 10)
        mesh (seed->mesh seed)
        f-f 0.3
        v-f 0.3
        mesh (-> mesh (op/complexify :f-factor 0.4 :v-factor 0.25))
        mesh (-> mesh (op/complexify :f-factor 0.2 :v-factor 0.5))
        mesh (-> mesh (op/complexify :f-factor 0.1 :v-factor 0.75))
        ;mesh (-> mesh (op/complexify :f-factor f-f :v-factor v-f))
        ;mesh (-> mesh (op/complexify :f-factor f-f :v-factor v-f))
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh)]
    mesh))

;(time (save-x3d "output/sandbox/complexify-test-01.x3d" (complexify-test-01)))

(defn complexify-test-02 []
  (let [seed (ph/dodecahedron 10)
        mesh (seed->mesh seed)
        mesh (-> mesh (op/complexify :f-factor 0.5 :v-factor 0.25))
        get-vertex (fn [mesh face]
                     (if-let [height ({3 -0.1, 4 +2, 5 -7} (count face))]
                       (op/calc-vertex face :height height)))
        mesh (-> mesh (op/kis :get-vertex get-vertex))
        mesh (-> mesh (op/complexify :f-factor 0.5 :v-factor 0.25))
        get-vertex (fn [mesh face]
                     (op/calc-vertex face :height -0.2))
        mesh (-> mesh (op/kis :get-vertex get-vertex))
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-average-comp-normal)]
    mesh))

(defn complexify-test-03 []
  (let [seed (ph/dodecahedron 10)
        mesh (seed->mesh seed)
        mesh (-> mesh (op/complexify :f-factor 0.25 :v-factor 0.25))
        get-vertex (fn [mesh face]
                     (if-let [height ({4 +3, 5 -7} (count face))]
                       (op/calc-vertex face :height height)))
        mesh (-> mesh (op/kis :get-vertex get-vertex))
        mesh (-> mesh (op/complexify :f-factor 0.5 :v-factor 0.25))
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-average-comp-plus-normal)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        mesh (op/colorize mesh get-face-color-blend-neighbors)
        ]
    mesh))

(defn kis-test-01 []
  (let [seed (cu/cuboid -5 10)
        mesh (seed->mesh seed)
        mesh (-> mesh (op/kis :height 5))
        mesh (op/catmull-clark mesh)
        mesh (-> mesh (op/kis :height -2))
        mesh (op/catmull-clark mesh)
        mesh (op/catmull-clark mesh)
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-average-comp-plus-normal)]
    mesh))

(defn kis-test-02 []
  (let [seed (cu/cuboid -5 10)
        mesh (seed->mesh seed)
        mesh (-> mesh (op/kis :height 10))
        mesh (op/catmull-clark mesh)
        mesh (op/catmull-clark mesh)
        mesh (op/catmull-clark mesh)
        mesh (-> mesh (op/kis :height -1))
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-abs-normal)]
    mesh))

(defn kis-test-03 []
  (let [seed (ph/icosahedron 10)
        mesh (seed->mesh seed)
        mesh (-> mesh (op/kis :height 10))
        mesh (op/catmull-clark mesh)
        mesh (op/catmull-clark mesh)
        mesh (op/catmull-clark mesh)
        mesh (-> mesh (op/kis :height 0.05))
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-invert-abs-normal)]
    mesh))

(defn kis-test-04 []
  (let [seed (ph/octahedron 10)
        mesh (seed->mesh seed)
        mesh (-> mesh (op/ortho :height 5))
        mesh (op/catmull-clark mesh)
        mesh (-> mesh (op/kis :height -2))
        mesh (op/catmull-clark mesh)
        mesh (op/catmull-clark mesh)
        mesh (op/catmull-clark mesh)
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-new)]
    mesh))

;(time (save-x3d "output/sandbox/kis-test-04.x3d" (kis-test-04)))

(defn ortho-test-01 []
  (let [seed (ph/dodecahedron 10)
        mesh (seed->mesh seed)
        mesh (-> mesh (op/ortho #(op/calc-vertex %2 :height 0)))
        get-vertex (fn [mesh face]
                     (if-let [height ({3 -0.2, 4 +2, 5 -7} (count face))]
                       (op/calc-vertex face :height height)))
        ;mesh (-> mesh (op/ortho get-vertex))
        mesh (op/catmull-clark mesh)
        mesh (op/catmull-clark mesh)
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-abs-normal)]
    mesh))

;(time (save-x3d "output/sandbox/ortho-test-01.x3d" (ortho-test-01)))

;(defn truncate-test-01 []
;  (let [seed (cu/cuboid -5 10)
;        mesh (seed->mesh seed)
;        mesh (-> mesh (op/truncate :percent 50))
;        mesh (g/tessellate mesh)
;        mesh (op/colorize mesh get-face-color-abs-normal)]
;    mesh))

;(time (save-x3d "output/sandbox/truncate-test-01.x3d" (truncate-test-01)))


; ==============================================================================
; Research & Development

(defn cc-test-01 []
  (let [seed (ph/dodecahedron 10)
        mesh (seed->mesh seed)
        ;mesh (-> mesh (op/ortho))
        mesh (op/catmull-clark mesh)
        mesh (op/catmull-clark mesh)
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-abs-normal)]
    mesh))

;(time (save-x3d "output/sandbox/cc-test-01.x3d" (cc-test-01)))

(defn cc-test-02 []
  (let [seed (ph/dodecahedron 10)
        mesh (seed->mesh seed)
        ;mesh (-> mesh (op/ortho))
        mesh (op/not-quite-catmull-clark mesh)
        mesh (op/not-quite-catmull-clark mesh)
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-abs-normal)]
    mesh))

;(time (save-x3d "output/sandbox/cc-test-02.x3d" (cc-test-02)))

(defn cc-test-03 []
  (let [seed (ph/dodecahedron 10)
        mesh (seed->mesh seed)
        ;mesh (-> mesh (op/ortho))
        mesh (-> mesh (op/complexify :f-factor 0.3 :v-factor 0.2))
        mesh (-> mesh (op/complexify :f-factor 0.3 :v-factor 0.2))
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-abs-normal)]
    mesh))

;(time (save-x3d "output/sandbox/cc-test-03.x3d" (cc-test-03)))

(defn foobar-test-01 []
  (let [seed (ph/dodecahedron 10)
        mesh (seed->mesh seed)
        mesh (-> mesh (op/complexify :f-factor 0.5 :v-factor 0.25))
        get-vertex (fn [mesh face]
                     (if-let [height ({3 -0.2, 4 +2, 5 -7} (count face))]
                       (op/calc-vertex face :height height)))
        mesh (-> mesh (op/kis get-vertex))
        mesh (-> mesh (op/complexify :f-factor 0.5 :v-factor 0.25))
        mesh (-> mesh (op/complexify :f-factor 0.5 :v-factor 0.25))
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-average-comp-normal)]
    mesh))

;(time (save-x3d "output/sandbox/foobar-test-01.x3d" (foobar-test-01)))

(defn skeletonize-test-01 []
  (let [seed (ph/dodecahedron 10)
        mesh (seed->mesh seed)
        mesh (-> mesh (op/skeletonize :thickness 3
                                      :get-f-factor (fn [_ _] 0.5)))
        ;mesh (-> mesh (op/skeletonize :thickness 1
        ;                              :get-f-factor (fn [_ _] 0.5)))
        ;mesh (op/catmull-clark mesh)
        ;mesh (op/catmull-clark mesh)
        ;mesh (op/catmull-clark mesh)
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh)]
    mesh))

(time (save-x3d "output/sandbox/skeletonize-test-01.x3d" (skeletonize-test-01)))

;(def foo (seed->mesh (cu/cuboid -5 10)))
;(def bar (g/tessellate foo))
;(def baz (g/compute-vertex-normals foo))
