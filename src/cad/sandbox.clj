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
  (let [[r g b] (mapv op/abs (g/face-normal mesh face))
        alpha 1.0]
    [r g b alpha]))

(defn get-face-color-invert-abs-normal [mesh face]
  (let [normal (g/face-normal mesh face)
        get-color (fn [n] (- 1.0 (op/abs n)))
        color (-> (mapv get-color normal) (conj 1.0))]
    color))

(defn get-face-color-average-complementary-normal [mesh face]
  (let [[x y z] (mapv op/abs (g/face-normal mesh face))
        average (/ (+ x y z) 3.0)
        comp? (neg? (apply + (g/face-normal mesh face)))
        color (col/as-rgba (col/hsva average 1.0 1.0))
        color (if comp? (col/complementary color) color)]
    @color))

(defn get-face-color-average-complementary-plus-normal [mesh face]
  (let [[x y z] (mapv op/abs (g/face-normal mesh face))
        average (/ (+ x y z) 3.0)
        comp? (neg? (apply + (g/face-normal mesh face)))
        color (col/as-rgba (col/hsva average (- 1.0 x) (- 1.0 y) (- 1.0 z)))
        ;color (col/as-rgba (col/hsva average x y z))
        color (if comp? (col/complementary color) color)]
    @color))

(defn get-face-color-blend-neighbors [mesh face]
  (let [fcolors (:fcolors mesh)
        old-color (col/rgba (fcolors face))
        neighbors (op/face-edge-neighbors mesh face)
        neighbor-colors (mapv #(col/rgba (fcolors %)) neighbors)
        color (reduce #(col/blend %1 %2 0.25) old-color neighbor-colors)]
    @color))

(defn get-face-color-area-max [mesh face]
  (let [face-area (get-in mesh [:face-area :map face])
        max-area (get-in mesh [:face-area :max])
        color (col/as-rgba (col/hsva (/ face-area max-area) 1.0 1.0 1.0))]
    @color))

(defn get-face-color-area-max-invert [mesh face]
  (let [face-area (get-in mesh [:face-area :map face])
        max-area (get-in mesh [:face-area :max])
        color (col/as-rgba (col/hsva (- 1.0 (/ face-area max-area)) 1.0 1.0 1.0))]
    @color))

(defn get-face-color-area-mod1 [mesh face]
  (let [face-area (get-in mesh [:face-area :map face])
        color (col/as-rgba (col/hsva (mod face-area 1) 1.0 1.0 1.0))]
    @color))

(defn get-face-color-area-mod10 [mesh face]
  (let [face-area (get-in mesh [:face-area :map face])
        color (col/as-rgba (col/hsva (mod (* 10 face-area) 1) 1.0 1.0 1.0))]
    @color))

(defn get-face-color-new-01 [mesh face]
  (let [[x y z] (mapv op/abs (g/face-normal mesh face))
        color (col/as-rgba (col/hsva (min x y z)
                                     (max x y z)
                                     (- 1.0 (max x y z))
                                     1.0))]
    @color))

(defn get-face-color-new-02 [mesh face]
  (let [hue 0.25 sat 0.5 val 0.5
        color (col/as-rgba (col/hsva hue sat val 1.0))]
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
      (op/colorize get-face-color-new-01)))

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

(defn skeletonize-test-01 []
  (let [mesh (-> ;(cu/cuboid -5 10)
               (ph/dodecahedron 10)
               (op/seed->mesh))
        original-faces (:faces mesh)
        mesh (-> mesh
                 (op/skeletonize :thickness 5
                                 :get-f-factor (fn [{:keys [faces]} face]
                                                 (when (= face (last faces)) 0.5)))
                 (op/skeletonize :thickness 2
                                 :get-f-factor (fn [_ face]
                                                 (when (original-faces face) 0.25)))
                 (op/rep op/catmull-clark 1)
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

(defn skeletonize-test-04 []
  (let [mesh (-> (cu/cuboid -5 10)
                 (op/seed->mesh))
        [wf sf ff nf bf ef] (sort (:faces mesh))
        mesh (-> mesh
                 (op/skeletonize
                   :thickness 6
                   :get-f-factor (fn [{:keys [faces]} face]
                                   (when (#{nf sf} face) 0.5)))
                 (op/skeletonize
                   :thickness 2
                   :get-f-factor (fn [_ face]
                                   (when (#{ef wf ff bf} face) 0.25)))
                 (op/rep op/catmull-clark 3)
                 (g/tessellate)
                 (op/colorize get-face-color-invert-abs-normal))]
    mesh))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-04.x3d" (skeletonize-test-04)))

(defn skeletonize-test-05 []
  (let [mesh (-> (cu/cuboid -5 10)
                 (op/seed->mesh))
        [wf sf ff nf bf ef] (sort (:faces mesh))
        mesh (-> mesh
                 (op/skeletonize
                   :thickness 5
                   :get-f-factor (fn [{:keys [faces]} face]
                                   (when (#{nf sf} face) 0.2)))
                 (op/skeletonize
                   :thickness 1
                   :get-f-factor (fn [_ face]
                                   (when ((set (map op/ortho-normal #{ef wf ff bf}))
                                           (op/ortho-normal face)) 0.1)))
                 (op/rep op/catmull-clark 2)
                 (op/kis)
                 (op/kis (op/get-v-edge-count-height {3 -0.01}))
                 (g/tessellate)
                 (op/colorize get-face-color-invert-abs-normal))]
    mesh))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-05.x3d" (skeletonize-test-05)))

(defn skeletonize-test-06 []
  (-> (ph/dodecahedron 10)
      (op/seed->mesh)
      ;(op/skeletonize :thickness 2 :get-f-factor (fn [{:keys [faces]} face]
      ;                                             (when (= face (last faces)) 0.5)))
      (op/complexify :f-factor 0.4 :v-factor 0.2)
      ;(op/kis (op/get-v-edge-count-height {4 -0.25}))
      ;(op/ortho (op/get-v-edge-count-height {5 0.25}))
      ;(op/kis (op/get-v-edge-count-height {4 -0.05}))
      ;(op/kis (op/get-v-edge-count-height {3 -0.05}))
      ;(op/kis (op/get-v-edge-count-height {4 0}))
      ;(op/ortho)
      (op/ortho (op/get-v-edge-count-height {5 0}))
      ;(op/ortho)
      ;(op/kis (op/get-v-edge-count-height {4 0}))
      ;(op/kis (op/get-v-edge-count-height {3 0}))
      ;(op/kis)
      (op/prn-sides)
      (op/tess)
      (op/prn-sides)
      (op/calc-face-area-map) (op/colorize get-face-color-area-max)))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-06.x3d" (skeletonize-test-06)))

(defn skeletonize-test-07 []
  (-> (ph/icosahedron 10)
      (op/seed->mesh)
      (op/skeletonize :thickness 1 :get-f-factor (fn [{:keys [faces]} face]
                                                   (when (= face (last faces)) 0.5)))
      (op/complexify :f-factor 0.4 :v-factor 0.2)
      (op/rep op/catmull-clark 2)
      (op/kis)
      (op/calc-face-area-map) (op/colorize get-face-color-area-max)))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-07.x3d" (skeletonize-test-07)))

(defn skeletonize-test-08 []
  (-> (ph/dodecahedron 10)
      ;(ph/icosahedron 10)
      (op/seed->mesh)
      (op/skeletonize :thickness 1 :get-f-factor (fn [{:keys [faces]} face]
                                                   (when (not= face (last faces)) 0.25)))
      ;(op/complexify :f-factor 0.5 :v-factor 0.2)
      (op/rep op/catmull-clark 3)
      (op/kis)
      (op/calc-face-area-map)
      (op/colorize get-face-color-area-max)))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-08.x3d" (skeletonize-test-08)))

(defn skeletonize-test-09 []
  (-> (ph/dodecahedron 10)
      (op/seed->mesh)
      (op/skeletonize :thickness 1 :get-f-factor (fn [{:keys [faces]} face]
                                                   (when ((set (take 9 faces)) face) 0.5)))
      (op/rep op/catmull-clark 3)
      (op/kis)
      (op/calc-face-area-map)
      (op/colorize get-face-color-area-mod10)))

;(time (cad/save-x3d "output/sandbox/skeletonize-test-09.x3d" (skeletonize-test-09)))

(defn davinci [seed]
  (-> seed
      (op/seed->mesh) (op/prn-fev "Seed") (op/prn-sides)

      ;(op/ortho) (prn-fev "Ortho 1")
      ;
      ;(op/ortho) (prn-fev "Ortho 2")

      (op/complexify :f-factor 0.5 :v-factor 0.25)
      (op/prn-fev "Complexify") (op/prn-sides)

      ;(op/ortho (op/get-v-edge-count-height {4 0})) (op/prn-fev "Ortho 1")
      ;
      ;(op/ortho (op/get-v-edge-count-height {8 0})) (op/prn-fev "Ortho 2")
      ;
      ;(op/ortho (op/get-v-edge-count-height {8 0})) (op/prn-fev "Ortho 3")

      (op/skeletonize :thickness 0.5
                      :get-f-factor (fn [{:keys [faces]} face]
                                      (when (= 4 (count face)) 0.25)))
      (op/prn-fev "Skeletonize") (op/prn-sides)

      ;(op/kis)

      (op/rep op/catmull-clark 3) (op/prn-fev "CC") (op/prn-sides)

      ;(op/rep op/catmull-clark 2)
      ;(op/complexify :f-factor 0.5 :v-factor 0.25)
      ;(op/catmull-clark)

      (op/tess)
      (op/prn-fev "Tess")
      (op/calc-face-area-map)
      (op/prn-fev "Area Map")
      (op/colorize get-face-color-area-mod10)
      (op/prn-fev "Final")))

(time (cad/save-x3d "output/sandbox/davinci-tetra-test-01.x3d"
                    (davinci (ph/tetrahedron 10))))

(time (cad/save-x3d "output/sandbox/davinci-hexa-test-01.x3d"
                    (davinci (cu/cuboid -5 10))))

(time (cad/save-x3d "output/sandbox/davinci-octo-test-01.x3d"
                    (davinci (ph/octahedron 10))))

(time (cad/save-x3d "output/sandbox/davinci-dodeca-test-01.x3d"
                    (davinci (ph/dodecahedron 10))))

(time (cad/save-x3d "output/sandbox/davinci-icosa-test-01.x3d"
                    (davinci (ph/icosahedron 10))))


; ==============================================================================
; Research & Development

;(defn davinci [seed]
;  (-> seed
;      (op/seed->mesh)
;      (op/skeletonize :thickness 0.5 :get-f-factor (fn [mesh face] 0.2))
;      (op/kis)
;      (op/calc-face-area-map)
;      (op/colorize get-face-color-new-05)))
;
;(defn davinci-test-01 []
;  (-> (cu/cuboid -5 10)
;      (davinci)))

;(def test-mesh (skeletonize-test-06))
;
;(time (cad/save-x3d "output/sandbox/development-test-01.x3d" test-mesh))


#_(def foo (-> (ph/dodecahedron 10)
               (op/seed->mesh)
               (op/skeletonize :thickness 2.0 :get-f-factor (fn [{:keys [faces]} face]
                                                              (when (= face (last faces)) 0.5)))
               (op/complexify :f-factor 0.5 :v-factor 0.25)))

;(def foo (op/seed->mesh (cu/cuboid -5 10)))
;(def bar (g/tessellate foo))
;(def baz (op/compute-vertex-normals foo))
