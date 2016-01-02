(ns cad.mesh.polyhedron
  (:require [thi.ng.geom.cuboid :as cu]
            [cad.mesh.face-mesh :as fm]
            [thi.ng.geom.core :as gc]
            [cad.mesh.util :as mu]
            [thi.ng.geom.mesh.polyhedra :as ph]
            [cad.mesh.core :as mc]
            [clojure.core.matrix :as mx]))


(defn vertex
  [scale [x y z]]
  (-> (mc/point x y z) (mx/normalise!) (mx/mul! scale)))


; ==============================================================================
; Platonic Solids

(defn tetra
  ([]
   (tetra 12))
  ([scale]
   (-> (ph/tetrahedron scale) (fm/fmesh))))

(defn hexa
  ([]
   (hexa 10))
  ([scale]
   (let [origin (- (/ scale 2))]
     (-> (cu/cuboid origin scale) (gc/faces) (fm/fmesh)))))

(defn octahedron-vertices []
  (let [p (/ (* 2.0 mu/SQRT2)), p' (- p), q 0.5, q' (- q)]
    [[p' 0 p] [p 0 p] [p 0 p'] [p' 0 p'] [0 q 0] [0 q' 0]]))

(defn octahedron
  [scale]
  (let [[a b c d e f] (map (partial vertex scale)
                           (octahedron-vertices))]
    [[d a e] [c d e] [b c e] [a b e]
     [d c f] [a d f] [c b f] [b a f]]))

(defn octa
  ([]
   (octa 8))
  ([scale]
   (-> (octahedron scale) (fm/fmesh))))

(defn dodeca
  ([]
   (dodeca 7))
  ([scale]
   (-> (ph/dodecahedron scale) (fm/fmesh))))

(defn icosa
  ([]
   (icosa 7.5))
  ([scale]
   (-> (ph/icosahedron scale) (fm/fmesh))))
