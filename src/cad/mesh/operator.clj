(ns cad.mesh.operator
  (:require [clojure.set]
            [cad.mesh.face-color :as fc]
            [thi.ng.geom.core :as gc]
            [thi.ng.geom.core.utils :as gu]
            [cad.mesh.core :as mc]))


; ==============================================================================
; General Operators

(defn rep
  "Repeat operation f on mesh n times."
  [mesh f n]
  (nth (iterate f mesh) n))

(defn tess
  "Returns a tesselated mesh."
  [mesh]
  (mc/fmesh (into [] (comp (map gu/tessellate-3) cat) (gc/faces mesh))))


; ==============================================================================
; Conway Operators

(defn ambo
  "Returns mesh with new vertices added mid-edge and old vertices removed."
  [mesh]
  (let [mesh (mc/assoc-vert-npfs-map mesh)
        verts (keys (:vert-npfs-map mesh))
        f-faces (map (fn [face]
                       (map gu/centroid (mc/face-vert-pairs face)))
                     (gc/faces mesh))
        v-faces (map (fn [vert]
                       (map gu/centroid (mc/vert-edges mesh vert)))
                     verts)
        faces (concat f-faces v-faces)]
    (mc/fmesh faces)))

;(defn expand
;  [mesh thickness]
;  ; Same as: Doo-Sabin subdivision
;  )

(defn kis
  "Returns mesh with each n-sided face divided into n triangles."
  ([mesh]
   (kis mesh mc/get-face-centroid))
  ([mesh get-f-point]
   (let [new-faces (fn [face f-point]
                     (mapv (fn [[c n]] [c n f-point])
                           (mc/face-vert-pairs face)))
         subdivide (fn [face]
                     (if-let [f-point (get-f-point mesh face)]
                       (new-faces face f-point)
                       [face]))
         faces (mapcat subdivide (gc/faces mesh))]
     (mc/fmesh faces))))

(defn ortho
  "Returns mesh with each n-sided face divided into n quadrilaterals."
  ([mesh]
   (ortho mesh mc/get-face-centroid))
  ([mesh get-f-point]
   (let [mesh (mc/assoc-edge-faces-map mesh)
         edges (keys (:edge-faces-map mesh))
         get-e-point (fn [edge] (gu/centroid (vec edge)))
         new-face (fn [[p c n] f-point e-points]
                    [(e-points #{p c}) c (e-points #{c n}) f-point])
         new-faces (fn [face f-point e-points]
                     (mapv #(new-face % f-point e-points)
                           (mc/face-vert-triples face)))
         subdivide (fn [face e-points]
                     (if-let [f-point (get-f-point mesh face)]
                       (new-faces face f-point e-points)
                       (let [edged-f (vec (mapcat (fn [[c n]]
                                                    [c (e-points #{c n})])
                                                  (mc/face-vert-pairs face)))]
                         [edged-f])))
         e-points (into {} (map (fn [edge]
                                  [edge (get-e-point edge)])
                                edges))
         faces (mapcat #(subdivide % e-points) (gc/faces mesh))]
     (mc/fmesh faces))))

;(defn truncate
;  "Returns mesh with new vertices added along edge and old vertices removed,
;   i.e. each vertex is replaced with a face."
;  [{:keys [faces edges] :as mesh} & {:keys [percent n-folds]
;                                     :or {percent 10 n-folds nil}}]
;  )


; ==============================================================================
; Other Operators

(defn color-faces
  "Returns mesh with face colors, defaults to color based on face normal."
  ([mesh]
   (color-faces mesh (fc/normal-abs-rgb) nil))
  ([mesh get-f-color]
   (color-faces mesh get-f-color nil))
  ([mesh get-f-color cb]
   (let [[mesh get-fc] (get-f-color mesh)
         fcolors (into {} (for [face (gc/faces mesh)]
                            (let [color (get-fc mesh face)
                                  color (if cb (cb color) color)]
                              [face color])))
         mesh (assoc mesh :face-color-map fcolors)]
     mesh)))

(defn complexify
  "Symetrical edge smoothing while mostly maintaining bounding box dimensions."
  [mesh & {:keys [f-factor v-factor] :or {f-factor 0.5 v-factor 0.25}}]
  (let [mesh (mc/assoc-edge-faces-map mesh)
        mesh (mc/assoc-vert-next-pf-map mesh)
        mesh (mc/assoc-vert-npfs-map mesh)
        edges (keys (:edge-faces-map mesh))
        verts (keys (:vert-next-pf-map mesh))
        offset (fn [vert face] (gc/mix vert (gu/centroid face) f-factor))
        fv-map (into {} (for [face (gc/faces mesh)]
                          [face (into {} (for [vert face]
                                           [vert (offset vert face)]))]))
        e-faces (for [edge edges]
                  (let [[v1 v2] (sort (vec edge))
                        f1 (get-in mesh [:vert-next-pf-map v1 v2 :face])
                        f2 (get-in mesh [:vert-next-pf-map v2 v1 :face])
                        va (get-in fv-map [f1 v2])
                        vb (get-in fv-map [f1 v1])
                        vc (get-in fv-map [f2 v1])
                        vd (get-in fv-map [f2 v2])]
                    [va vb vc vd]))
        f-faces (for [face (gc/faces mesh)]
                  (for [vert face]
                    (get-in fv-map [face vert])))
        v->faces (fn [vert]
                   (let [vf-verts (mapv #(get-in fv-map [% vert])
                                        (mc/vert-faces mesh vert))
                         vf-vert (gc/mix (gu/centroid vf-verts) vert v-factor)
                         vf-edges (mc/face-vert-pairs vf-verts)]
                     (mapv #(conj % vf-vert) vf-edges)))
        v-faces (mapcat v->faces verts)
        faces (concat e-faces f-faces v-faces)]
    (mc/fmesh faces)))

(defn skeletonize
  "Return mesh with all the flesh removed."
  [mesh & {:keys [thickness get-f-factor] :or {thickness 1}}]
  (let [get-f-fact (fn [_ _] 0.25)
        get-f-factor (or get-f-factor get-f-fact)
        vert-normal-map (mc/mesh-vert-normal-map (tess mesh))
        offset (fn [vert face f-factor] (gc/mix vert (gu/centroid face) f-factor))
        offset-face (fn [face f-factor] (mapv #(offset % face f-factor) face))
        opposite-face (fn [outer-face thickness]
                        (vec (for [vert (reverse outer-face)]
                               (gc/+ vert (gc/* (vert-normal-map vert)
                                                (- thickness))))))
        new-face (fn [[c n] [c-off n-off]]
                   [c n n-off c-off])
        new-faces (fn [face face-off]
                    (mapv #(new-face %1 %2)
                          (mc/face-vert-pairs face)
                          (mc/face-vert-pairs face-off)))
        subdivide (fn [outer-f]
                    (let [inner-f (opposite-face outer-f thickness)]
                      (if-let [f-factor (get-f-factor mesh outer-f)]
                        (let [outer-off (offset-face outer-f f-factor)
                              inner-off (offset-face inner-f f-factor)]
                          (concat
                            (new-faces outer-f outer-off)
                            (new-faces inner-f inner-off)
                            (new-faces outer-off (reverse inner-off))))
                        [outer-f inner-f])))
        faces (mapcat subdivide (gc/faces mesh))]
    (mc/fmesh faces)))


; ==============================================================================
; Catmull-Clark Subdivision Operator

(defn catmull-clark
  "Return a mesh with additional faces and edge points for a smoothing effect."
  [mesh & {:keys [get-f-point get-e-point get-v-point]}]
  (let [mesh (mc/assoc-edge-faces-map mesh)
        mesh (mc/assoc-vert-npfs-map mesh)
        edge-faces-map (:edge-faces-map mesh)
        verts (keys (:vert-npfs-map mesh))
        get-ep (fn [edge e-faces f-points]
                 (gu/centroid (concat (vec edge) (mapv f-points e-faces))))
        get-vp (fn [mesh vertex]
                 (let [f (gu/centroid (mapv gu/centroid
                                            (mc/vert-faces mesh vertex)))
                       vn (mc/vert-neighbors mesh vertex)
                       n (count vn)
                       r (gu/centroid (mapv #(gc/mix vertex %) vn))]
                   (gc/addm (gc/madd r 2.0 f) (gc/* vertex (- n 3)) (/ 1.0 n))))
        get-f-point (or get-f-point mc/get-face-centroid)
        get-e-point (or get-e-point get-ep)
        get-v-point (or get-v-point get-vp)
        new-face (fn [[p c n] f-point e-points]
                   [(e-points #{p c}) c (e-points #{c n}) f-point])
        new-faces (fn [face f-point e-points]
                    (mapv #(new-face % f-point e-points)
                          (mc/face-vert-triples face)))
        subdivide (fn [[face f-point] e-points]
                    (new-faces face f-point e-points))
        f-points (into {} (map (fn [face]
                                 [face (get-f-point mesh face)])
                               (gc/faces mesh)))
        e-points (into {} (map (fn [[edge e-faces]]
                                 [edge (get-e-point edge e-faces f-points)])
                               edge-faces-map))
        v-points (into {} (map (fn [vertex]
                                 [vertex (get-v-point mesh vertex)])
                               verts))
        v-replace (partial replace v-points)
        faces (->> (mapcat #(subdivide % e-points) f-points) (map v-replace))]
    (mc/fmesh faces)))


; ==============================================================================
; Not Quite Catmull-Clark Subdivision Operator

(defn not-quite-catmull-clark
  "Return a mesh with additional faces and edge points for a smoothing effect."
  [mesh]
  (let [get-e-point (fn [edge _ _] (gu/centroid (vec edge)))]
    (catmull-clark mesh :get-e-point get-e-point)))
