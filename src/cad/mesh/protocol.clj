(ns cad.mesh.protocol)

; ==============================================================================
; Mesh Protocols

(defprotocol IPolygonMesh
  (faces [m])
  (edges [m])
  (verts [m])
  (assoc-edge-faces-map [m])
  (assoc-face-area-map [m])
  (assoc-face-circ-map [m])
  (assoc-face-dist-map [m point])
  (assoc-vert-npfs-map [m])
  (centroid [m])
  (edge-faces-map [m])
  (face-color-map [m])
  (face-normal-map [m])
  (scale [m factor])
  (scale! [m factor])
  (vert-faces-map [m])
  (vert-normal-map [m])
  (vert-npfs-map [m]))
