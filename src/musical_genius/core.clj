(ns musical-genius.core)

(def what-key :c                                            ;Set the key of your music generator"
  )

(def notes
  '(:c :c# :d :d# :e :f :f# :g :g# :a :a# :b))

(def note-map
  {1 :c, 2 :c#, 3 :d, 4 :d#, 5 :e, 6 :f, 7 :f#, 8 :g, 9 :g#, 10 :a, 11 :a#, 12 :b})

(def note-vals
  {:c 1, :c# 2, :d 3, :d# 4, :e 5, :f 6, :f# 7, :g 8, :g# 9, :a 10, :a# 11, :b 12})

(def major-scale
  "Modify the mode accordingly"
  [1 0 1 0 1 1 0 1 0 1 0 1])


(defn rotate [v n]
  (let [cv (count v), n (mod n cv)]
    (concat (subvec v n cv) (subvec v 0 n))))

(defn map-key-sig [root]
  "Rotate the major scale to fit the key
  For reference: C is 0, C#/Db is 1, etc"
  (let [position (- (note-vals root) 1)]
    (vec (remove nil? (map #(note-map %1)
                           (map #(* %1 %2) (rotate [1 2 3 4 5 6 7 8 9 10 11 12] position) major-scale))))))

(def pitch-bag
  '("+2" "+" "+" "+" "" "" "" "" "" "" "" "" "" "" "" "" "-" "-" "-" "-2"))

(def length
  '("1/16" "1/8" "1/4" "1/2" "1"))

(def smallest-value "1/" 8)

(defn measure-template [num]
  (sort (take num (repeatedly #(rand-int num)))))

(defn generate-lengths [num]
  (let [template (measure-template num)]
    (mapv #(/ (second %1) num) (frequencies template))))

(defn generate-notes [num]
  (vec (take num (repeatedly #(rand-nth notes)))))

(defn generate-pitch [num]
  (vec (take num (repeatedly #(rand-nth pitch-bag)))))

(defn translate-notes [length note pitch]
  (str length " " pitch note " "))

(def fitness-vals
  {1/4 10, 1/8 6, 1/2 6, 1 4, 1/16 4, 3/8 2, 5/8 2,
   ""  8, "-" 4, "+" 4, "-2" 2, "+2" 2,
   })

(defn trait-fitness [pitch]
  (reduce + (let [pitch-freq (frequencies pitch)]
              (map (fn [x]
                     (* (fitness-vals (first x)) (second x))) pitch-freq))))

(def key-sig (map-key-sig what-key))

(defn note-in-key [note]
  (if (nil? (note-vals (some #{note} key-sig)))
    4
    10))

(defn note-fitness [note]
  (reduce + (map #(note-in-key %1) note)))

(defn fitness [l n p]
  (/ (+ (+ (note-fitness n) (trait-fitness l))
        (trait-fitness p)) (count l)))

(defn new-individual []
  (let [l (generate-lengths 8)
        n (generate-notes (count l))
        p (generate-pitch (count l))]
    {:lengths     l
     :notes       n
     :pitches     p
     :final-notes (mapv #(translate-notes %1 %2 %3) l n p)
     :fitness     (double (fitness l n p))}))



(def example-individual (new-individual))