(ns cad.mesh.face
  (:refer-clojure :exclude [+ - * / == min max])
  (:require [clojure.core.matrix.operators :refer :all]
            [cad.mesh.core :as mc]
            [thi.ng.math.macros :as mm]
            [cad.mesh.protocol :as mp]
            [clojure.core.matrix :as mx]))


; ==============================================================================
; Base Functions

(defn unique-verts? [face]
  "Returns true if there are no duplicate vertices within the face."
  (= (count face) (count (set face))))

(defn edges
  "Returns a lazy seq of edges for a face."
  [face]
  (let [face (vec face)]
    (map set (partition 2 1 (conj face (first face))))))

(defn vert-pairs
  "Returns a lazy seq of vertex pairs for a face."
  [face]
  (let [face (vec face)]
    (partition 2 1 (conj face (first face)))))

(defn vert-triples
  "Returns a lazy seq of vertex triples - (prev curr next) - for a face."
  [face]
  (let [face (vec face)]
    (partition 3 1 (cons (peek face) (conj face (first face))))))


; ==============================================================================
; Geometry Functions

(defn- mag [[x y z]]
  (Math/sqrt (mm/madd x x y y z z)))

(defn- norm-sign3
  [a b c] (mag (mx/cross (- b a) (- c a))))

(defn- tri-area3
  [a b c] (* 0.5 (norm-sign3 a b c)))

(defn centroid
  [face]
  (mc/centroid face))

(defn area
  [face]
  ((if (= 3 (count face))
     (apply tri-area3 face)
     (let [cent (centroid face)]
       (reduce + (map (fn [[v1 v2]] (tri-area3 cent v1 v2))
                      (vert-pairs face)))))))

(defn circumference
  [face]
  (reduce + (map (fn [[v1 v2]] (mx/distance v1 v2)) (vert-pairs face))))

(defn distance
  [face point]
  (mx/distance (centroid face) point))

(defn normal
  "Returns the ortho normal for a face based on the first three vertices."
  ([[a b c]] (normal a b c))
  ([a b c] (mc/normal a b c)))

(defn tessellate-with-point
  ([face]
   (tessellate-with-point (centroid face) face))
  ([point face]
   (mapv (fn [[v1 v2]] [point v1 v2]) (vert-pairs face))))

(defn tessellate
  [face]
  (condp = (count face)
    3 [face]
    4 (let [[a b c d] face] [[a b c] [a c d]])
    (tessellate-with-point face)))


; ==============================================================================
; Point Functions

(defn get-point
  "Returns a point at height distance from face-point along the face normal."
  [face & {:keys [point height] :or {height 0}}]
  (let [point (or point (centroid face))]
    (-> (normal face) (* height) (+ point))))

(defn get-centroid
  [_ face]
  (centroid face))

(defn get-point-at-height
  "Returns a function that returns a face paoint based on the given height."
  [height]
  (fn [_ face]
    (get-point face :height height)))

(defn get-point-at-edge-count-height
  "Returns a function that returns a vertex based on the number of face sides."
  [edge-count-height-map]
  (fn [_ face]
    (if-let [height (edge-count-height-map (count face))]
      (get-point face :height height))))


; ==============================================================================
; Neighbor Functions

(defn edge-neighbors
  "Returns a set of faces that neighbor one of the given face's edges."
  [mesh face]
  (let [edge-faces-map (mp/edge-faces-map mesh)
        xf (mapcat (fn [edge] (edge-faces-map (set edge))))
        faces (into #{} xf (vert-pairs face))
        faces (clojure.set/difference faces #{face})]
    faces))

(defn vert-neighbors
  "Returns a set of faces that share one of the given face's vertices."
  [mesh face]
  (let [vert-npfs-map (mp/vert-npfs-map mesh)
        xf (mapcat (fn [vert] (map :face (vert-npfs-map vert))))
        faces (into #{} xf face)
        faces (clojure.set/difference faces #{face})]
    faces))

(defn vert-only-neighbors
  "Returns a set of faces that share one of the given face's vertices but do
   not share any edges."
  [mesh face]
  (clojure.set/difference (vert-neighbors mesh face)
                          (edge-neighbors mesh face)))
