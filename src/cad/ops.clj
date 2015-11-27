(ns cad.ops
  (:require [cad.x3d :as x3d]
            [clojure.java.io :as io]
            [thi.ng.geom.cuboid :as cu]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.gmesh :as gm]
            [thi.ng.geom.core.utils :as gu]
            [thi.ng.geom.mesh.io :as mio]
            [thi.ng.geom.core.vector :as v :refer [vec3]]))


; ==============================================================================
; Shared constants and functions

(defn save-stl
  [path mesh]
  (with-open [out (io/output-stream path)]
    (mio/write-stl
      (mio/wrapped-output-stream out)
      (g/tessellate mesh))))

(defn save-x3d
  [path mesh & {:keys [indent?] :or {indent? false}}]
  (let [meta (array-map
               :creator "Patrick K. O'Brien"
               :created "24 November 2015"
               :copyright "Copyright 2015 Patrick K. O'Brien"
               :generator "Custom Clojure Code")
        units [(array-map
                 :category "length"
                 :name "millimeters"
                 :conversionFactor "0.001")]]
    #_(x3d/save-x3d path mesh :indent? indent? :units units :meta meta)
    (x3d/save-x3d path mesh :indent? indent? :meta meta)))

(defn seed->mesh
  "Returns a mesh for a seed collection of vertices."
  [seed]
  (g/into (gm/gmesh) seed))

(defn face-loop-triples
  "Takes a mesh face (vector of points) and returns lazyseq of successive
  point triples: [prev curr next]"
  [face]
  (->> face (first) (conj face) (cons (peek face)) (partition 3 1)))

(defn centroid-map
  "Returns a map of [vertices centroid] pairs for a collection of vertices."
  [coll]
  (let [xf (map (fn [verts] [verts (gu/centroid (seq verts))]))]
    (into {} xf coll)))

(defn calc-vertex
  "Returns a vertex at height distance from face-point along the normal."
  [face face-point height]
  (-> (take 3 face)
      (gu/ortho-normal)
      (g/* height)
      (g/+ (vec3 face-point))))

(defn quad-divide-face
  "Returns a vector of new faces."
  [face new-vertex e-points]
  (let [xf (map (fn [[p c n]]
                  [(e-points #{p c}) c (e-points #{c n}) new-vertex]))]
    (into [] xf (face-loop-triples face))))

(defn quad-divide-faces
  "Returns a vector of new faces."
  [f-points e-points height n-sides]
  (let [xf (mapcat (fn [[face face-point]]
                     (if (or (nil? n-sides) (n-sides (count face)))
                       (let [new-vertex (calc-vertex face face-point height)]
                         (quad-divide-face face new-vertex e-points))
                       [face])))]
    (into [] xf f-points)))

(defn tri-divide-face
  "Returns a vector of new faces."
  [face new-vertex]
  (let [xf (map (fn [[prev curr next]]
                  [curr next new-vertex]))]
    (into [] xf (face-loop-triples face))))

(defn tri-divide-faces
  "Returns a vector of new faces."
  [f-points height n-sides]
  (let [xf (mapcat (fn [[face face-point]]
                     (if (or (nil? n-sides) (n-sides (count face)))
                       (let [new-vertex (calc-vertex face face-point height)]
                         (tri-divide-face face new-vertex))
                       [face])))]
    (into [] xf f-points)))


; ==============================================================================
; Conway Operators

(defn expand [mesh]
  ; Same as: Doo-Sabin subdivision
  )

(defn kis
  "Return new mesh with each n-sided face divided into n triangles."
  [{:keys [faces] :as mesh} & {:keys [height n-sides]}]
  (let [f-points (centroid-map faces)]
    (->> (tri-divide-faces f-points height n-sides)
         (g/into (g/clear* mesh)))))

(defn ortho
  "Return new mesh with each n-sided face divided into n quadrilaterals."
  [{:keys [faces edges] :as mesh} & {:keys [height n-sides]}]
  (let [f-points (centroid-map faces)
        e-points (centroid-map (keys edges))]
    (->> (quad-divide-faces f-points e-points height n-sides)
         (g/into (g/clear* mesh)))))


; ==============================================================================
; Conway Operator Tests

(def foo (seed->mesh (cu/cuboid -5 10)))

(defn kis-test-01 []
  (let [seed (cu/cuboid -5 10)
        mesh (seed->mesh seed)
        mesh (-> mesh (kis :height 0))]
    mesh))

(time (save-x3d "output/geom/kis-test-01.x3d" (kis-test-01)))

(defn ortho-test-01 []
  (let [seed (cu/cuboid -5 10)
        mesh (seed->mesh seed)
        mesh (-> mesh (ortho :height -4))]
    mesh))

(time (save-x3d "output/geom/ortho-test-01.x3d" (ortho-test-01)))


; ==============================================================================
; Other Operators: Shell/Hollow/Carve

(defn shell [mesh _]
  (let [mesh mesh]
    mesh))

(defn shell-test-01 []
  (let [seed (cu/cuboid -5 10)
        mesh (g/into (gm/gmesh) seed)
        mesh (-> mesh (shell 2))]
    mesh))

;(time (save-x3d "output/geom/shell-test-01.x3d" (shell-test-01)))

(comment
  "
  split vertex (replace vertex with a face)
  contract edge (replace edge with a vertex)
  add diagonal (divide quadrangle into two triangles)

  Dual
  Truncate == split vertices
  Ambo == split vertices, contract edges
  Bevel == TA
  Expand == AA
  Snub == contract edges, add diagonals

  Kis == dual of Truncate
  Join == dual of Ambo
  Meta == dual of Bevel
  Ortho == dual of Expand
  Gyro == dual of Snub
  ")


; ==============================================================================
; Subdivision Operators

(defn cc-face-points
  "Returns a map of [face centroid-point] pairs."
  [faces]
  (let [xf (map (fn [face] [face (gu/centroid face)]))]
    (into {} xf faces)))

(defn cc-edge-points
  "Returns a map of [edge new-edge-point] pairs."
  [edges f-points]
  (let [xf (map (fn [[e e-faces]]
                  [e (-> (mapv f-points e-faces)
                         (conj (first e))
                         (conj (second e))
                         (gu/centroid))]))]
    (into {} xf edges)))

(defn cc-subdiv-face
  "Returns a vector of new faces."
  [face fp e-points]
  (let [xf (map (fn [[p c n]] [(e-points #{p c}) c (e-points #{c n}) fp]))]
    (into [] xf (face-loop-triples face))))

(defn cc-subdiv-faces
  "Returns a vector of new faces."
  [f-points e-points]
  (let [xf (mapcat (fn [[f fp]] (cc-subdiv-face f fp e-points)))]
    (into [] xf f-points)))

(defn cc-replace-vertices
  "Returns a vector of new faces."
  [mesh f-points sd-faces]
  (let [xf (map (fn [v]
                  (let [f (gu/centroid (mapv f-points (gm/vertex-faces* mesh v)))
                        vn (gm/vertex-neighbors* mesh v)
                        n (count vn)
                        r (gu/centroid (mapv #(g/mix v %) vn))]
                    [v (g/addm (g/madd r 2.0 f) (g/* v (- n 3)) (/ 1.0 n))])))
        new-verts (into {} xf (keys (:vertices mesh)))]
    (map (fn [f] (replace new-verts f)) sd-faces)))

(defn catmull-clark
  "Return a mesh with additional faces and edge points for a smoothing effect."
  [{:keys [faces edges] :as mesh}]
  (let [f-points (cc-face-points faces)
        e-points (cc-edge-points edges f-points)]
    (->> (cc-subdiv-faces f-points e-points)
         (cc-replace-vertices mesh f-points)
         (g/into (g/clear* mesh)))))
