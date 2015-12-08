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
  (-> (ph/dodecahedron 10)
      (op/seed->mesh)
      (op/complexify :f-factor 0.4 :v-factor 0.25)
      (op/complexify :f-factor 0.2 :v-factor 0.50)
      (op/complexify :f-factor 0.1 :v-factor 0.75)
      (g/tessellate)
      (op/colorize)))

;(time (cad/save-x3d "output/sandbox/complexify-test-01.x3d" (complexify-test-01)))

(defn complexify-test-02 []
  (-> (ph/dodecahedron 10)
      (op/seed->mesh)
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/kis (op/get-v-edge-count-height {3 -0.1, 4 +2, 5 -7}))
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/kis (op/get-v-height -0.2))
      (g/tessellate)
      (op/colorize get-face-color-average-complementary-normal)))

;(time (cad/save-x3d "output/sandbox/complexify-test-02.x3d" (complexify-test-02)))

(defn complexify-test-03 []
  (-> (ph/dodecahedron 10)
      (op/seed->mesh)
      (op/complexify :f-factor 0.25 :v-factor 0.25)
      (op/kis (op/get-v-edge-count-height {4 +3, 5 -7}))
      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (g/tessellate)
      (op/colorize get-face-color-average-complementary-normal)
      (op/rep #(op/colorize % get-face-color-blend-neighbors) 6)))

;(time (cad/save-x3d "output/sandbox/complexify-test-03.x3d" (complexify-test-03)))

(defn kis-test-01 []
  (-> (cu/cuboid -5 10)
      (op/seed->mesh)
      (op/kis (op/get-v-height 5))
      (op/catmull-clark)
      (op/kis (op/get-v-height -2))
      (op/rep op/catmull-clark 2)
      (g/tessellate)
      (op/colorize get-face-color-average-complementary-plus-normal)))

;(time (cad/save-x3d "output/sandbox/kis-test-01.x3d" (kis-test-01)))

(defn kis-test-02 []
  (-> (cu/cuboid -5 10)
      (op/seed->mesh)
      (op/kis (op/get-v-height 10))
      (op/rep op/catmull-clark 3)
      (op/kis (op/get-v-height -1))
      (g/tessellate)
      (op/colorize get-face-color-abs-normal)))

;(time (cad/save-x3d "output/sandbox/kis-test-02.x3d" (kis-test-02)))

(defn kis-test-03 []
  (-> (ph/icosahedron 10)
      (op/seed->mesh)
      (op/kis (op/get-v-height 10))
      (op/rep op/catmull-clark 3)
      (op/kis (op/get-v-height 0.05))
      (g/tessellate)
      (op/colorize get-face-color-invert-abs-normal)))

;(time (cad/save-x3d "output/sandbox/kis-test-03.x3d" (kis-test-03)))

(defn kis-test-04 []
  (-> (ph/octahedron 10)
      (op/seed->mesh)
      (op/ortho (op/get-v-height 5))
      (op/catmull-clark)
      (op/kis (op/get-v-height -2))
      (op/rep op/catmull-clark 3)
      (g/tessellate)
      (op/colorize get-face-color-new)))

;(time (cad/save-x3d "output/sandbox/kis-test-04.x3d" (kis-test-04)))

(defn ortho-test-01 []
  (-> (ph/dodecahedron 10)
      (op/seed->mesh)
      (op/ortho (op/get-v-height 0))
      ;(op/ortho (op/get-v-edge-count-height {3 -0.2, 4 +2, 5 -7}))
      (op/rep op/catmull-clark 2)
      (g/tessellate)
      (op/colorize get-face-color-abs-normal)))

;(time (cad/save-x3d "output/sandbox/ortho-test-01.x3d" (ortho-test-01)))


; ==============================================================================
; Research & Development

(defn skeletonize-test-01 []
  (let [mesh (-> (cu/cuboid -5 10)
                 ;(ph/dodecahedron 10)
                 (op/seed->mesh))
        original-faces (:faces mesh)
        mesh (-> mesh
                 (op/skeletonize :thickness 5
                                 :get-f-factor (fn [{:keys [faces]} face]
                                                 (when (= face (last faces)) 0.5)))
                 (op/skeletonize :thickness 3
                                 :get-f-factor (fn [_ face]
                                                 (when (original-faces face) 0.25)))
                 (op/rep op/catmull-clark 4)
                 (g/tessellate)
                 (op/colorize))]
    mesh))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-01.x3d" (skeletonize-test-01)))

(defn skeletonize-test-02 []
  (-> (cu/cuboid -5 10)
      ;(ph/dodecahedron 10)
      (op/seed->mesh)
      (op/skeletonize :thickness 4 :get-f-factor (fn [_ _] 0.25))
      (op/kis (op/get-v-edge-count-height {3 +0, 4 +0.2, 5 +0}))
      (op/catmull-clark)
      (op/kis (op/get-v-height -0.1))
      (op/catmull-clark)
      (g/tessellate)
      (op/colorize)))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-02.x3d" (skeletonize-test-02)))

(defn skeletonize-test-03 []
  (let [mesh (-> (cu/cuboid -5 10)
                 ;(ph/dodecahedron 10)
                 (op/seed->mesh))
        original-faces (:faces mesh)
        mesh (-> mesh
                 (op/skeletonize :thickness 4
                                 :get-f-factor (fn [{:keys [faces]} face]
                                                 (when (= face (last faces)) 0.25)))
                 (op/skeletonize :thickness 2
                                 :get-f-factor (fn [_ face]
                                                 (when (original-faces face) 0.25)))
                 (op/rep op/catmull-clark 3)
                 (op/kis (op/get-v-height -0.05))
                 (g/tessellate)
                 (op/colorize get-face-color-invert-abs-normal))]
    mesh))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-03.x3d" (skeletonize-test-03)))


;(def foo (seed->mesh (cu/cuboid -5 10)))
;(def bar (g/tessellate foo))
;(def baz (g/compute-vertex-normals foo))
