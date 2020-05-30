(ns musical-genius.core)

(def what-key
  "Set the key of your music generator"
  :c#)

(def notes
  '(:c :c# :d :d# :e :f :f# :g :g# :a :a# :b))

(def note-map-sans-r
  {1 :c, 2 :c#, 3 :d, 4 :d#, 5 :e, 6 :f, 7 :f#, 8 :g, 9 :g#, 10 :a, 11 :a#, 12 :b})

(def note-map
  {0 :r, 1 :c, 2 :c#, 3 :d, 4 :d#, 5 :e, 6 :f, 7 :f#, 8 :g, 9 :g#, 10 :a, 11 :a#, 12 :b})

(def note-vals-sans-r
  {:c 1, :c# 2, :d 3, :d# 4, :e 5, :f 6, :f# 7, :g 8, :g# 9, :a 10, :a# 11, :b 12})

(def note-vals
  {:r 0, :c 1, :c# 2, :d 3, :d# 4, :e 5, :f 6, :f# 7, :g 8, :g# 9, :a 10, :a# 11, :b 12})

(def pitch-vals
  {"+2" 0, "+" 1, "" 2, "-" 3, "-2" 4})

(def pitch-map
  {0 "+2",1 "+",2 "",3 "-",4 "-2"})

(def major-scale
  "Modify the mode accordingly"
  [1 0 1 0 1 1 0 1 0 1 0 1])

(defn rotate [v n]
  (let [cv (count v), n (mod n cv)]
    (concat (subvec v n cv) (subvec v 0 n))))

(defn abs [n]
  (if (< n 0)
    (* -1 n)
    n))

(defn carry-note [n]
  (if (< n 0) (+ n 13)
    n))

(defn carry-pitch [n]
  (if (< n 0) (+ n 5)
              n))

(defn map-key-sig [root]
  "Rotate the major scale to fit the key
  For reference: C is 0, C#/Db is 1, etc"
  (let [position (- (note-vals-sans-r root) 1)]
    (vec (remove nil? (map #(note-map-sans-r %1)
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

(defn create-genome [l n p]
  (mapv #(vector %1 %2 %3) l p n))

(defn new-individual []
  (let [l (generate-lengths 8)
        n (generate-notes (count l))
        p (generate-pitch (count l))]
    {:genome      (create-genome l n p)
     :lengths     l
     :notes       n
     :pitches     p
     :final-notes (mapv #(translate-notes %1 %2 %3) l n p)
     :fitness     (double (fitness l n p))}))

(defn mutate-note [gene]
  (if (< (rand) 0.15) (assoc gene 2 (note-map (mod (carry-note (+ (rand-nth (range -2 3)) (note-vals (nth gene 2)))) 13)))
                      gene))

(defn mutate-length [gene]

  gene)

(def example-gene
  [1/4 "" :d])

(defn mutate-pitch [gene]
  (if (< (rand) 0.95) (assoc gene 1 (pitch-map (mod (carry-pitch (+ (rand-nth (range -1 2)) (pitch-vals (second gene)))) 5)))
                      gene))

(defn mutate [genome]
  (let [chance (rand)]
    (cond
      (>= chance 1) (map #(mutate-length %1) genome)
      (>= chance 0.5) (map #(mutate-pitch %1) genome)
      (>= chance 0) (map #(mutate-note %1) genome)
      :else genome)))

(defn best [individuals]
  "Returns the best of the given individuals."
  (reduce (fn [i1 i2]
            (if (> (:fitness i1) (:fitness i2))
              i1
              i2))
          individuals))

(defn select [population]
  "Returns an individual selected from population using a tournament."
  (best (repeatedly 2 #(rand-nth population))))

(defn make-child [population]
  (let [new-genome (mutate (:genome (select population)))
        l (mapv #(first %1) new-genome)
        n (mapv #(nth %1 2) new-genome)
        p (mapv #(second %1) new-genome)]
    {:genome new-genome
     :lengths l
     :notes n
     :pitches p
     :final-notes (mapv #(translate-notes %1 %2 %3) l n p)
     :fitness (double (fitness l n p))}
    ))

(defn report [generation population]
  (let [current-best (best population)]
    (println {:generation generation
              :best-fitness (:fitness current-best)
              :best-genome (:genome current-best)})))

(defn run [population-size generations]
  (loop [population (repeatedly population-size #(new-individual))
         generation 0]
    (report generation population)
    (if (>= generation generations)
      (best population)
      (recur (repeatedly population-size #(make-child population))
             (inc generation)))))

(def example-individual (new-individual))