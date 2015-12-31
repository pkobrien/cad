(ns cad.mesh.polyhedron
  (:require [thi.ng.geom.cuboid :as cu]
            [thi.ng.geom.core :as gc]
            [cad.mesh.core :as mc]
            [thi.ng.geom.mesh.polyhedra :as ph]))


; ==============================================================================
; Platonic Solids

(defn tetra
  ([]
   (tetra 12))
  ([scale]
   (-> (ph/tetrahedron scale) (mc/fmesh))))

(defn hexa
  ([]
   (hexa 10))
  ([scale]
   (let [origin (- (/ scale 2))]
     (-> (cu/cuboid origin scale) (gc/faces) (mc/fmesh)))))

(defn octa
  ([]
   (octa 8))
  ([scale]
   (-> (ph/octahedron scale) (mc/fmesh))))

(defn dodeca
  ([]
   (dodeca 7))
  ([scale]
   (-> (ph/dodecahedron scale) (mc/fmesh))))

(defn icosa
  ([]
   (icosa 7.5))
  ([scale]
   (-> (ph/icosahedron scale) (mc/fmesh))))
