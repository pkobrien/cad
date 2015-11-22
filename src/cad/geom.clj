(ns cad.geom
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.xml :as xml]
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

(defn xml-emit
  "Prints the given Element tree as XML text to stream.
   Options:
    :encoding <str>          Character encoding to use
    :doctype  <str>          Document type (DOCTYPE) declaration to use"
  [e ^java.io.Writer stream & {:keys [encoding doctype] :or {encoding "UTF-8"}}]
  (let [^javax.xml.stream.XMLStreamWriter writer (-> (javax.xml.stream.XMLOutputFactory/newInstance)
                                                     (.createXMLStreamWriter stream))]
    (when (instance? java.io.OutputStreamWriter stream)
      (xml/check-stream-encoding stream encoding))
    (.writeStartDocument writer encoding "1.0")
    (when doctype
      (.writeDTD writer doctype))
    (doseq [event (xml/flatten-elements [e])]
      (xml/emit-event event writer))
    (.writeEndDocument writer)
    stream))

(defn xml-emit-indented
  "Emits the XML and indents the result.  WARNING: this is slow
   it will emit the XML and read it in again to indent it.  Intended for
   debugging/testing only."
  [e ^java.io.Writer stream & {:as opts}]
  (let [sw (java.io.StringWriter.)
        _ (apply xml-emit e sw (apply concat opts))
        source (-> sw .toString java.io.StringReader. javax.xml.transform.stream.StreamSource.)
        result (javax.xml.transform.stream.StreamResult. stream)]
    (.transform (xml/indenting-transformer) source result)))

(defn write-x3d
  "Writes the given mesh as X3D XML to output stream wrapper."
  [out mesh indent? units meta]
  (let [faces (g/faces mesh)
        vertices (vec (g/vertices mesh))
        vindex (zipmap vertices (range))
        fcolors (into {} (for [face faces] [face [1 0 0 1]]))
        colors (vec (set (vals fcolors)))
        cindex (zipmap colors (range))
        fnormals (g/face-normals mesh true)
        get-normal (fn [face] (or (get fnormals face) (gu/ortho-normal face)))
        normals (vec (set (map get-normal faces)))
        nindex (zipmap normals (range))
        per-v-format (fn [f coll] (str (string/join " -1 " (map f coll)) " -1"))
        coord-format (fn [face] (string/join " " (mapv #(get vindex %) face)))
        coord-index (per-v-format coord-format faces)
        color-format (fn [face] (get cindex (get fcolors face)))
        color-index (string/join " " (map color-format faces))
        normal-format (fn [face] (get nindex (get-normal face)))
        normal-index (string/join " " (map normal-format faces))
        list-format (fn [coll] (string/join " " (apply concat coll)))
        point-list (list-format vertices)
        color-list (list-format colors)
        normal-list (list-format normals)
        contents (xml/sexp-as-element
                   [:X3D {:version "3.3" :profile "Immersive"}
                    (when (seq meta)
                      [:head
                       (for [unit units]
                         [:unit unit])
                       (for [[k v] meta]
                         [:meta {:name (name k) :content (str v)}])])
                    [:Scene
                     [:Shape
                      [:IndexedFaceSet {:solid "false"
                                        :ccw "true"
                                        :colorPerVertex "false"
                                        :convex "true"
                                        :creaseAngle "0"
                                        :normalPerVertex "false"
                                        :coordIndex coord-index
                                        :colorIndex color-index
                                        :normalIndex normal-index}
                       [:Coordinate {:point point-list}]
                       [:ColorRGBA {:color color-list}]
                       [:Normal {:vector normal-list}]
                       ]]]])
        doctype "<!DOCTYPE X3D PUBLIC \"ISO//Web3D//DTD X3D 3.3//EN\" \"http://www.web3d.org/specifications/x3d-3.3.dtd\">"
        emit (if indent? xml-emit-indented xml-emit)]
    (emit contents out :doctype doctype)
    out))

(defn save-x3d
  [path mesh & {:keys [indent? units meta] :or {indent? false}}]
  (with-open [out (io/writer path)]
    (write-x3d out (g/tessellate mesh) indent? units meta)))

(defn pob-save-x3d
  [path mesh & {:keys [indent?] :or {indent? false}}]
  (let [meta (array-map
               :creator "Patrick K. O'Brien"
               :created "22 November 2015"
               :copyright "Copyright 2015 Patrick K. O'Brien"
               :generator "Custom Clojure Code")
        units [(array-map
                 :category "length"
                 :name "millimeters"
                 :conversionFactor "0.001")]]
    #_(save-x3d path mesh :indent? indent? :units units :meta meta)
    (save-x3d path mesh :indent? indent? :meta meta)))

(defn x3d-test-mesh []
  (-> (cu/cuboid -5 10) (g/as-mesh)))

(time (pob-save-x3d "output/geom/x3d-test-mesh.x3d" (x3d-test-mesh)))
(time (pob-save-x3d "output/geom/x3d-test-mesh-indented.x3d" (x3d-test-mesh) :indent? true))


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
