(ns edna-sandbox.core
  (:require [edna.core :as edna]))

(def ^:private note-char? (set "abcdefg"))

(def ^:private octave-modifiers
  (concat (map str (range -9 -1))
          ["-" "" "+"]
          (map #(str "+" %) (range 2 10))

(defn- select-indexes [v indexes]
  (-> (select-keys v indexes)
      vals
      vec))

(defn raise-octave [k]
  (let [k-str         (name k)
        [octave note] (map #(apply str %)
                           (split-with (complement note-char?) k-str))
        next-octave (zipmap octave-modifiers (drop 1 octave-modifiers))]
    (keyword (str (next-octave octave) note))))

(defn scale-of
  ([root]
   (scale-of root :chromatic))
  ([root steps]
   (let [chromatic [:a :a# :b :c :c# :d :d# :e :f :f# :g :g#]
         chromatic-at-root (as-> chromatic $
                             (drop-while #(not= root %) $)
                             (vec $)
                             (into $ (map raise-octave chromatic))
                             (take 12 $)
                             (vec $))]
     (condp = steps
       :chromatic chromatic-at-root
       :major (select-indexes chromatic-at-root [0 2 4 5 7 9 11])
       :minor (select-indexes chromatic-at-root [0 2 3 5 7 8 10])
       (select-indexes chromatic-at-root steps)))))

(defn chord-of
  ([root steps]
   (set
    (condp = steps
      :maj (scale-of root [0 4 7])
      :min (scale-of root [0 3 7])
      :aug (scale-of root [0 4 8])
      :dim (scale-of root [0 3 6])
      :sus2 (scale-of root [0 2 7])
      :sus4 (scale-of root [0 5 7])
      (scale-of root steps)))))

(def melody
  [1/8 :f# :+a 3/4 :+2d
   1/8 :+c :+a 3/4 :+e
   1/8 :f# :+a :+2d :+2c# :+2e :+2d :+b :+2c#
   1/2 :+a :+e])

(def harmony
  [1 (chord-of :d :sus2)
   1 (chord-of :e :maj)
   1 (chord-of :d :sus2)
   1 #{:e :+a}])

(def music
  [{:octave 4 :tempo 72}
   #{(into [:midi-electric-grand-piano] melody)
     (into [:midi-acoustic-grand-piano] harmony)}])

(defonce state (atom nil))
(swap! state edna/stop!)
(reset! state (edna/play! music))

