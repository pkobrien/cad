(ns cad.x3d
  (:require [clojure.string :as string]
            [clojure.data.xml :as xml]
            [thi.ng.geom.core :as g]))


; ==============================================================================
; X3D Output

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
  [out mesh & {:keys [indent? units meta] :or {indent? false}}]
  (let [faces (g/faces mesh)
        vertices (vec (g/vertices mesh))
        vindex (zipmap vertices (range))
        fcolors (:fcolors mesh)
        colors (vec (set (vals fcolors)))
        cindex (zipmap colors (range))
        fnormals (g/face-normals mesh true)
        ; Clojure has a bug involving equality of 0.0 and -0.0
        ; http://dev.clojure.org/jira/browse/CLJ-1860
        ; So we get around this by indexing the string value of the normals.
        ;normals (vec (set (vals fnormals)))
        normals (vec (set (map str (vals fnormals))))
        nindex (zipmap normals (range))
        per-v-fmt (fn [coll] (str (string/join " -1 " coll) " -1"))
        viface-fmt (fn [face] (string/join " " (mapv #(get vindex %) face)))
        vertex-index (per-v-fmt (map viface-fmt faces))
        get-cindex (fn [face] (get cindex (get fcolors face)))
        color-index (string/join " " (map get-cindex faces))
        ;get-nindex (fn [face] (get nindex (get fnormals face)))
        get-nindex (fn [face] (get nindex (str (get fnormals face))))
        normal-index (string/join " " (map get-nindex faces))
        list-format (fn [coll] (string/join " " (apply concat coll)))
        vertex-list (list-format vertices)
        color-list (list-format colors)
        ;normal-list (list-format normals)
        normal-list (string/join " " (map #(subs % 1 (dec (count %))) normals))
        contents (xml/sexp-as-element
                   [:X3D {:version "3.3" :profile "Immersive"}
                    (when (or (seq units) (seq meta))
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
                                        :coordIndex vertex-index
                                        :colorIndex color-index
                                        :normalIndex normal-index}
                       [:Coordinate {:point vertex-list}]
                       [:ColorRGBA {:color color-list}]
                       [:Normal {:vector normal-list}]
                       ]]]])
        doctype "<!DOCTYPE X3D PUBLIC \"ISO//Web3D//DTD X3D 3.3//EN\" \"http://www.web3d.org/specifications/x3d-3.3.dtd\">"
        emit (if indent? xml-emit-indented xml-emit)]
    (emit contents out :doctype doctype)
    out))
