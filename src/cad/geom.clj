(ns cad.geom
  (:refer-clojure :exclude [import use])
  (:require [clojure.java.io :as io]
            [cad.core :refer [cartesian-product fillet spit-scad]]
            [scad-clj.model :refer :all]
            [thi.ng.geom.aabb :as ab]
            [thi.ng.geom.basicmesh :as bm]
            [thi.ng.geom.bezier :as bz]
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
            [thi.ng.geom.types :as tp]
            [thi.ng.geom.triangle :as tr]
            [thi.ng.geom.types.utils :as tu]
            [thi.ng.geom.core.vector :as v :refer [vec2 vec3 V3Y V3Z]]))


; ==============================================================================
; Shared constants and functions

(defn save-stl
  [path mesh]
  (with-open [out (io/output-stream path)]
    (mio/write-stl
      (mio/wrapped-output-stream out)
      (g/tessellate mesh))))

;(defn p-mesh
;  [f scale]
;  (g/into (gm/gmesh) (f scale)))

(defn p-mesh
  [f scale]
  (g/into (bm/basic-mesh) (f scale)))

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

;(time (save-stl "output/geom/hexahedron.stl" (hexahedron)))

(defn platonic-solids []
  (let [th (-> ph/tetrahedron (p-mesh 10) (g/translate (vec3 0 0 0)))
        oh (-> ph/octahedron (p-mesh 10) (g/translate (vec3 20 0 0)))
        hh (-> (cu/cuboid -5 10) (g/as-mesh) (g/translate (vec3 40 0 0)))
        ih (-> ph/icosahedron (p-mesh 10) (g/translate (vec3 60 0 0)))
        dh (-> ph/dodecahedron (p-mesh 10) (g/translate (vec3 80 0 0)))
        mesh (mesh-union [th oh hh ih dh])]
    mesh))

;(time (save-stl "output/geom/platonic-solids.stl" (platonic-solids)))

(defn tetrahedron-test-01 []
  (let [mesh (ph/polyhedron-mesh ph/tetrahedron 10)]
    mesh))

(defn tetrahedron-test-02 []
  (let [t1 (-> ph/tetrahedron
               (p-mesh 10)
               (g/translate (vec3 0 0 0))
               (csg/mesh->csg))
        t2 (-> ph/tetrahedron
               (p-mesh 10)
               (g/translate (vec3 5 0 0))
               ;(sd/catmull-clark)
               (csg/mesh->csg))
        ;t3 (-> ph/tetrahedron
        ;       (p-mesh 10)
        ;       (g/translate (vec3 10 0 0))
        ;       (sd/catmull-clark)
        ;       (csg/mesh->csg))
        ;object (reduce csg/union [t1 t2 t3])
        object (reduce csg/union [t1 t2])
        mesh (csg/csg->mesh object)
        ;mesh (csg/csg->mesh t2)
        ;mesh t2
        ]
    mesh))

;(time (save-stl "output/geom/tetrahedron-test-02.stl" (tetrahedron-test-02)))

(comment
  (time (save-stl "output/geom/platonic-solids.stl"
                  (platonic-solids)))
  (time (save-stl "output/geom/tetrahedron-test-01.stl"
                  (tetrahedron-test-01)))
  (time (save-stl "output/geom/tetrahedron-test-02.stl"
                  (tetrahedron-test-02)))
  (time (save-stl "output/geom/tetrahedron-test-smooth-01.stl"
                  (ph/polyhedron-mesh ph/tetrahedron sd/catmull-clark 10 1)))
  (time (save-stl "output/geom/tetrahedron-test-smooth-02.stl"
                  (ph/polyhedron-mesh ph/tetrahedron sd/catmull-clark 10 2)))
  (time (save-stl "output/geom/tetrahedron-test-smooth-03.stl"
                  (ph/polyhedron-mesh ph/tetrahedron sd/catmull-clark 10 3)))
  (time (save-stl "output/geom/tetrahedron-test-smooth-04.stl"
                  (ph/polyhedron-mesh ph/tetrahedron sd/catmull-clark 10 4)))
  )
