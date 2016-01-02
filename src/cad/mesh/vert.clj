(ns cad.mesh.vert
  (:require [thi.ng.dstruct.core :as dc]
            [cad.mesh.core :as mc]
            [cad.mesh.protocol :as mp]
            [clojure.core.matrix :as mx]))


; ==============================================================================
; Vertex Functions

(defn edges
  "Returns a vector of edges for a vertex in ccw order."
  [mesh vert]
  (let [vert-npfs-map (mp/vert-npfs-map mesh)
        np (fn [{:keys [next prev]}] [next prev])
        np-map (into {} (map np (vert-npfs-map vert)))
        start (first (sort (keys np-map)))
        e-verts (reduce (fn [acc _]
                          (conj acc (np-map (last acc))))
                        [start] (range (dec (count np-map))))
        edges (mapv (fn [e-vert] [vert e-vert]) e-verts)]
    edges))

(defn faces
  "Returns a vector of faces for a vertex in ccw order."
  [mesh vert]
  (let [vert-npfs-map (mp/vert-npfs-map mesh)
        npf (fn [{:keys [next prev face]}] [next {:prev prev :face face}])
        npf-map (or (get-in mesh [:vert-next-pf-map vert])
                    (into {} (map npf (vert-npfs-map vert))))
        start (first (sort (keys npf-map)))
        e-verts (reduce (fn [acc _]
                          (conj acc (get-in npf-map [(last acc) :prev])))
                        [start] (range (dec (count npf-map))))
        faces (mapv (fn [vert] (get-in npf-map [vert :face])) e-verts)]
    faces))

(defn neighbors
  [mesh vert]
  (let [vert-npfs-map (mp/vert-npfs-map mesh)]
    (clojure.set/union
      (dc/value-set :next vert-npfs-map vert)
      (dc/value-set :prev vert-npfs-map vert))))

(defn normal
  "Returns the normal for the vertex as the average of the face normals."
  [mesh vert]
  (let [face-normal-map (mp/face-normal-map mesh)
        vert-faces-map (mp/vert-faces-map mesh)
        xf (comp (map face-normal-map) (distinct))
        vnorm (fn [vert]
                (->> (vert-faces-map vert)
                     (transduce xf + (mc/point 0.0 0.0 0.0))
                     (mx/normalise)))]
    (vnorm vert)))
