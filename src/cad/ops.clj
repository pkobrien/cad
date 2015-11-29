(ns cad.ops
  (:require [thi.ng.geom.core :as g]
            [thi.ng.geom.gmesh :as gm]
            [thi.ng.geom.core.utils :as gu]
            [thi.ng.geom.core.vector :refer [vec3]]))


; ==============================================================================
; Shared constants and functions

(defn face-edges
  "Returns a lazy seq of edges for a face."
  [face]
  (->> face (first) (conj face) (partition 2 1)))

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

(defn vertex-edges
  "Returns a vector of edges for a vertex."
  [mesh vertex]
  (let [dataset ((:vertices mesh) vertex)
        xf (map (fn [datamap] [(:next datamap) (:prev datamap)]))
        np-map (into {} xf dataset)
        start (first (sort (keys np-map)))
        verts (reduce (fn [acc _]
                        (conj acc (np-map (last acc))))
                      [start] (range (dec (count np-map))))
        edges (map (fn [e-vert] [vertex e-vert]) verts)]
    edges))


; ==============================================================================
; Conway Operators

(defn ambo
  "Returns mesh with new vertices added mid-edge and old vertices removed."
  [{:keys [faces vertices] :as mesh}]
  (let [f-faces (map (fn [face]
                       (map gu/centroid (face-edges face))) faces)
        v-faces (map (fn [vert]
                       (map gu/centroid (vertex-edges mesh vert))) (keys vertices))]
    (->> (concat f-faces v-faces)
         (g/into (g/clear* mesh)))))

;(defn expand [mesh]
;  ; Same as: Doo-Sabin subdivision
;  )

(defn kis
  "Returns mesh with each n-sided face divided into n triangles."
  [{:keys [faces] :as mesh} & {:keys [height n-sides]
                               :or {height 0 n-sides nil}}]
  (let [f-points (centroid-map faces)]
    (->> (tri-divide-faces f-points height n-sides)
         (g/into (g/clear* mesh)))))

(defn ortho
  "Returns mesh with each n-sided face divided into n quadrilaterals."
  [{:keys [faces edges] :as mesh} & {:keys [height n-sides]
                                     :or {height 0 n-sides nil}}]
  (let [f-points (centroid-map faces)
        e-points (centroid-map (keys edges))]
    (->> (quad-divide-faces f-points e-points height n-sides)
         (g/into (g/clear* mesh)))))

;(defn truncate
;  "Returns mesh with new vertices added along edge and old vertices removed,
;   i.e. each vertex is replaced with a face."
;  [{:keys [faces edges] :as mesh} & {:keys [percent n-folds]
;                                     :or {percent 10 n-folds nil}}]
;  (let [f-points (centroid-map faces)
;        e-points (centroid-map (keys edges))]
;    (->> (quad-divide-faces f-points e-points percent n-folds)
;         (g/into (g/clear* mesh)))))


; ==============================================================================
; Other Operators: Shell/Hollow/Carve/Skeletonize

(defn colorize
  "Returns mesh with face colors, defaults to color based on face normal."
  ([mesh]
   (let [get-color (fn [mesh face]
                     (let [[r g b] (mapv #(Math/abs %) (g/face-normal mesh face))
                           alpha 1.0]
                       [r g b alpha]))]
     (colorize mesh get-color)))
  ([mesh get-color]
   (let [mesh (assoc mesh :fnormals (g/face-normals mesh true))
         xf (map (fn [face] [face (get-color mesh face)]))
         fcolors (->> mesh (g/faces) (into {} xf))
         mesh (assoc mesh :fcolors fcolors)]
     mesh)))

;(defn shell [mesh _]
;  (let [mesh mesh]
;    mesh))


; ==============================================================================
; Catmull-Clark Subdivision Operator

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


; ==============================================================================
; Doo-Sabin Subdivision Operator
