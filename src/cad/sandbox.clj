(ns cad.sandbox
  (:require [cad.core :as cad]
            [cad.ops :as op]
            [thi.ng.color.core :as col]
            [thi.ng.geom.cuboid :as cu]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.gmesh :as gm]
            [thi.ng.geom.core.utils :as gu]
            [thi.ng.geom.mesh.polyhedra :as ph]
            [thi.ng.geom.mesh.subdivision :as sd]))


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

(defn get-face-color-average-complementary-normal [mesh face]
  (let [[x y z] (mapv #(Math/abs %) (g/face-normal mesh face))
        average (/ (+ x y z) 3.0)
        comp? (neg? (apply + (g/face-normal mesh face)))
        color (col/as-rgba (col/hsva average 1.0 1.0))
        color (if comp? (col/complementary color) color)]
    @color))

(defn get-face-color-average-complementary-plus-normal [mesh face]
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
  (-> (ph/dodecahedron 10)
      (op/seed->mesh)
      (op/rep op/ambo 3)
      (op/kis (op/get-v-edge-count-height {3 2.5, 5 -10}))
      (op/rep op/catmull-clark 3)
      (g/tessellate)
      (op/colorize get-face-color-average-complementary-plus-normal)))

;(time (cad/save-x3d "output/sandbox/ambo-test-01.x3d" (ambo-test-01)))

(defn ambo-test-02 []
  (-> (ph/dodecahedron 10)
      (op/seed->mesh)
      (op/rep op/ambo 3)
      (op/kis (op/get-v-edge-count-height {5 3.0}))
      (op/kis (op/get-v-edge-count-height {3 -0.05, 4 -0.05}))
      (g/tessellate)
      (op/colorize get-face-color-abs-normal)
      (op/rep #(op/colorize % get-face-color-blend-neighbors) 12)))

;(time (cad/save-x3d "output/sandbox/ambo-test-02.x3d" (ambo-test-02)))

(defn ambo-test-03 []
  (-> (ph/dodecahedron 10)
      (op/seed->mesh)
      (op/rep op/ambo 3)
      (op/kis (op/get-v-edge-count-height {3 0.25, 4 -0.5, 5 -7.0}))
      (g/tessellate)
      (op/colorize get-face-color-abs-normal)
      (op/rep #(op/colorize % get-face-color-blend-neighbors) 12)))

;(time (cad/save-x3d "output/sandbox/ambo-test-03.x3d" (ambo-test-03)))

(defn complexify-test-01 []
  (let [seed (ph/icosahedron 10)
        mesh (op/seed->mesh seed)
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

;(time (cad/save-x3d "output/sandbox/complexify-test-01.x3d" (complexify-test-01)))

(defn complexify-test-02 []
  (let [seed (ph/dodecahedron 10)
        mesh (op/seed->mesh seed)
        mesh (-> mesh (op/complexify :f-factor 0.5 :v-factor 0.25))
        get-vertex (fn [mesh face]
                     (if-let [height ({3 -0.1, 4 +2, 5 -7} (count face))]
                       (op/calc-vertex face :height height)))
        mesh (-> mesh (op/kis get-vertex))
        mesh (-> mesh (op/complexify :f-factor 0.5 :v-factor 0.25))
        get-vertex (fn [mesh face]
                     (op/calc-vertex face :height -0.2))
        mesh (-> mesh (op/kis get-vertex))
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-average-complementary-normal)]
    mesh))

(defn complexify-test-03 []
  (let [seed (ph/dodecahedron 10)
        mesh (op/seed->mesh seed)
        mesh (-> mesh (op/complexify :f-factor 0.25 :v-factor 0.25))
        get-vertex (fn [mesh face]
                     (if-let [height ({4 +3, 5 -7} (count face))]
                       (op/calc-vertex face :height height)))
        mesh (-> mesh (op/kis get-vertex))
        mesh (-> mesh (op/complexify :f-factor 0.5 :v-factor 0.25))
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-average-complementary-plus-normal)
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
        mesh (op/seed->mesh seed)
        mesh (-> mesh (op/kis :height 5))
        mesh (op/catmull-clark mesh)
        mesh (-> mesh (op/kis :height -2))
        mesh (op/catmull-clark mesh)
        mesh (op/catmull-clark mesh)
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-average-complementary-plus-normal)]
    mesh))

(defn kis-test-02 []
  (let [seed (cu/cuboid -5 10)
        mesh (op/seed->mesh seed)
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
        mesh (op/seed->mesh seed)
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
        mesh (op/seed->mesh seed)
        mesh (-> mesh (op/ortho :height 5))
        mesh (op/catmull-clark mesh)
        mesh (-> mesh (op/kis :height -2))
        mesh (op/catmull-clark mesh)
        mesh (op/catmull-clark mesh)
        mesh (op/catmull-clark mesh)
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-new)]
    mesh))

;(time (cad/save-x3d "output/sandbox/kis-test-04.x3d" (kis-test-04)))

(defn ortho-test-01 []
  (let [seed (ph/dodecahedron 10)
        mesh (op/seed->mesh seed)
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

;(time (cad/save-x3d "output/sandbox/ortho-test-01.x3d" (ortho-test-01)))

;(defn truncate-test-01 []
;  (let [seed (cu/cuboid -5 10)
;        mesh (seed->mesh seed)
;        mesh (-> mesh (op/truncate :percent 50))
;        mesh (g/tessellate mesh)
;        mesh (op/colorize mesh get-face-color-abs-normal)]
;    mesh))

;(time (cad/save-x3d "output/sandbox/truncate-test-01.x3d" (truncate-test-01)))


; ==============================================================================
; Research & Development

(defn cc-test-01 []
  (let [seed (ph/dodecahedron 10)
        mesh (op/seed->mesh seed)
        ;mesh (-> mesh (op/ortho))
        mesh (op/catmull-clark mesh)
        mesh (op/catmull-clark mesh)
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-abs-normal)]
    mesh))

;(time (cad/save-x3d "output/sandbox/cc-test-01.x3d" (cc-test-01)))

(defn cc-test-02 []
  (let [seed (ph/dodecahedron 10)
        mesh (op/seed->mesh seed)
        ;mesh (-> mesh (op/ortho))
        mesh (op/not-quite-catmull-clark mesh)
        mesh (op/not-quite-catmull-clark mesh)
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-abs-normal)]
    mesh))

;(time (cad/save-x3d "output/sandbox/cc-test-02.x3d" (cc-test-02)))

(defn cc-test-03 []
  (let [seed (ph/dodecahedron 10)
        mesh (op/seed->mesh seed)
        ;mesh (-> mesh (op/ortho))
        mesh (-> mesh (op/complexify :f-factor 0.3 :v-factor 0.2))
        mesh (-> mesh (op/complexify :f-factor 0.3 :v-factor 0.2))
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-abs-normal)]
    mesh))

;(time (cad/save-x3d "output/sandbox/cc-test-03.x3d" (cc-test-03)))

(defn foobar-test-01 []
  (let [seed (ph/dodecahedron 10)
        mesh (op/seed->mesh seed)
        mesh (-> mesh (op/complexify :f-factor 0.5 :v-factor 0.25))
        get-vertex (fn [mesh face]
                     (if-let [height ({3 -0.2, 4 +2, 5 -7} (count face))]
                       (op/calc-vertex face :height height)))
        mesh (-> mesh (op/kis get-vertex))
        mesh (-> mesh (op/complexify :f-factor 0.5 :v-factor 0.25))
        mesh (-> mesh (op/complexify :f-factor 0.5 :v-factor 0.25))
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh get-face-color-average-complementary-normal)]
    mesh))

;(time (cad/save-x3d "output/sandbox/foobar-test-01.x3d" (foobar-test-01)))

(defn skeletonize-test-01 []
  (let [seed (ph/dodecahedron 10)
        mesh (op/seed->mesh seed)
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

;(time (cad/save-x3d "output/sandbox/skeletonize-test-01.x3d" (skeletonize-test-01)))

(defn skeletonize-test-02 []
  (let [seed (cu/cuboid -5 10)
        mesh (op/seed->mesh seed)
        ;mesh (-> mesh (op/complexify :f-factor 0.5 :v-factor 0.25))
        mesh (-> mesh (op/skeletonize :thickness 4
                                      :get-f-factor (fn [_ _] 0.25)))
        ;mesh (-> mesh (op/skeletonize :thickness 2
        ;                              :get-f-factor (fn [_ _] 0.5)))
        get-vertex (fn [mesh face]
                     (if-let [height ({3 +0, 4 +0.2, 5 +0} (count face))]
                       (op/calc-vertex face :height height)))
        mesh (-> mesh (op/kis get-vertex))
        mesh (op/catmull-clark mesh)
        mesh (-> mesh (op/kis #(op/calc-vertex %2 :height -0.1)))
        mesh (op/catmull-clark mesh)
        ;mesh (op/catmull-clark mesh)
        mesh (g/tessellate mesh)
        mesh (op/colorize mesh)]
    mesh))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-02.x3d" (skeletonize-test-02)))

;(def foo (seed->mesh (cu/cuboid -5 10)))
;(def bar (g/tessellate foo))
;(def baz (g/compute-vertex-normals foo))
