(ns cad.geom
  (:require [thi.ng.geom.aabb :as ab]
            [thi.ng.geom.basicmesh :as bm]
            [thi.ng.geom.bezier :as bz]
            [cad.core :as cad]
            [thi.ng.geom.circle :as ci]
            [thi.ng.geom.mesh.csg :as csg]
            [thi.ng.geom.cuboid :as cu]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.gmesh :as gm]
            [thi.ng.geom.core.utils :as gu]
            [thi.ng.geom.voxel.isosurface :as iso]
            [thi.ng.geom.line :as ln]
            [thi.ng.math.core :as m :refer [*eps* HALF_PI PHI PI SQRT2 SQRT3 TWO_PI]]
            [thi.ng.geom.core.matrix :as mat :refer [M32 M44]]
            [thi.ng.geom.mesh.io :as mio]
            [cad.mesh.ops :as op]
            [thi.ng.geom.mesh.ops :as ops]
            [thi.ng.geom.polygon :as pg]
            [thi.ng.geom.mesh.polyhedra :as ph]
            [thi.ng.geom.plane :as pl]
            [thi.ng.geom.quad :as qd]
            [thi.ng.geom.core.quaternion :as quat]
            [thi.ng.geom.rect :as ra]
            [thi.ng.geom.sphere :as sp]
            [thi.ng.geom.mesh.subdivision :as sd]
            [thi.ng.math.simplexnoise :as sn]
            [thi.ng.geom.voxel.svo :as svo]
            [thi.ng.geom.triangle :as tr]
            [thi.ng.geom.types.utils :as tu]
            [thi.ng.geom.types :as types]
            [thi.ng.geom.core.vector :as v :refer [vec2 vec3 V3Y V3Z]]))


; ==============================================================================
; Shared constants and functions

(defn p-mesh
  [f scale]
  (g/into (gm/gmesh) (f scale)))

(defn mesh-union
  ([meshes]
   (mesh-union (bm/basic-mesh) 1e-3 meshes))
  ([target eps meshes]
   (-> (reduce g/into target meshes)
       (ops/canonicalize-vertices eps)
       (first)
       (ops/remove-internal))))


; ==============================================================================
; Experiments with thi.ng/geom

(defn hexahedron []
  (-> (cu/cuboid -5 10) (g/as-mesh)))

;(time (cad/save-x3d "output/geom/hexahedron.x3d" (hexahedron)))

(defn platonic-solids []
  (let [th (-> ph/tetrahedron (p-mesh 12) (g/translate (vec3 0 0 0)))
        hh (-> (cu/cuboid -5 10) (g/as-mesh) (g/translate (vec3 20 0 0)))
        oh (-> ph/octahedron (p-mesh 8) (g/translate (vec3 40 0 0)))
        dh (-> ph/dodecahedron (p-mesh 7) (g/translate (vec3 60 0 0)))
        ih (-> ph/icosahedron (p-mesh 7.5) (g/translate (vec3 80 0 0)))
        mesh (mesh-union [th oh hh ih dh])
        mesh (op/colorize mesh)]
    mesh))

(time (cad/save-x3d "output/sandbox/platonic-solids.x3d" (platonic-solids)))
