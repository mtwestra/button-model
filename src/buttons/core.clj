; this is an implementation of Stuart Kauffman's button model
; introduced in his book 'At Home in the Universe', 1995


(ns core.buttons)

(defn add-to-set
  "adds a button to the button set"
  [new-button button-set]
  (if (nil? button-set)
        #{new-button}
        (conj button-set new-button)))

(defn rand-distinct
  "picks a random number from 0 (inclusive) to limit (exclusive)
   excluding the value val"
  [limit val]
  (let [rand1 (rand-int limit)]
    (loop [r rand1]
      (if (not= r val)
        r
        (recur (rand-int limit))))))

; total-steps the total number of steps (threads) to take
(def total-steps 100)

; the results will be averaged over average-run runs
; when this is one, only a single run is done
(def average-runs 1)

; the total number of buttons
(def total-buttons 50)

(defn single-step
 "selects two (different) buttons and connects them"
  [m]
   (let [rand-button1 (+ 1 (rand-int total-buttons))
         rand-button2 (+ 1 (rand-distinct total-buttons (- rand-button1 1)))
         
         ; find the existing set of connections in the map
         conn-set1 (m rand-button1)
         conn-set2 (m rand-button2)
         
         ; add button 1 to set 2 and the other way around
         new-set1 (add-to-set rand-button2 conn-set1)
         new-set2 (add-to-set rand-button1 conn-set2)
         
         ; place the new sets in the map and return the [map, button1]
         ; we need the button1 later to determine the size of the largest cluster
         new-map (assoc m rand-button1 new-set1)] 
           [(assoc new-map rand-button2 new-set2) rand-button1]))


(defn non-visited-neighbours
  "get list of non-visited neighbours"
  [button-map visited-set button]
  ; create a set of all the 'neighbours' (connected buttons) 
  ; of this button, by looking up the set in the map
  (let [all-neighbours (button-map button)]
    ; remove the buttons that are already present in the visited map
    (remove #(visited-set %) all-neighbours)))

(defn mark-neighbours
  "mark unvisited neighbours as visited recursively, starting from a single button"
  [button-map visited-set button]
  ; create a set of non-visited neighbours of a button
  (let [non-visited-nb (non-visited-neighbours button-map visited-set button)
        ; add the button to the visited set
        new-visited-set (conj visited-set button)]
    
    ; if the non-visited-nb set is not empty, call mark-neighbours on it 
    (if (not-empty non-visited-nb)
      ;apply the mark-neighbours function on the set of non-visited neighbours
     (reduce (fn [vis-set button]
                (mark-neighbours button-map vis-set button))
                 new-visited-set
                 (seq non-visited-nb))
           ; else return the visited-set
           new-visited-set
      )))

(defn get-size-cluster
  "get the size of the cluster connected to a button"
  [button-map button]
  ; call mark-neighbours function with empty visited map
  (let [visited-set (mark-neighbours button-map #{} button)]
    ; count the resulting set
    (count visited-set)))

(defn do-run 
  "do a single run of total-steps"
  []
  (loop [i 1, m {}, largest-cluster 0, cluster-size-map {}]
    (if (> i total-steps)
      ; if we have taken total-steps steps, return the 
      ; cluster size map as the result
      cluster-size-map
      ; otherwise, compute the next step
      (let [[new-map,button] (single-step m)
          ; get the size of the cluster using the new map and one
            ; of the chosen buttons
          size-cluster (get-size-cluster new-map button)
          ; keep the largest of the two
          new-largest-cluster (if (> size-cluster largest-cluster)
                                size-cluster
                                largest-cluster)
          ; put the newest value in the cluster-size-map at step i
          new-cluster-size-map (assoc cluster-size-map i new-largest-cluster)]
      (recur (inc i) new-map new-largest-cluster new-cluster-size-map)))))


(defn do-n-runs
  "compute the averages over n runs"
  [n]
  (loop [i n, result {}]
    (if (zero? i)
      ; if we have done n runs, return a sorted map with the average values
      (sort (into {} (for [[k v] result] [k (double(/ v n))])))
      
      ; if we have not done n runs yet, do another and add the
      ; result to the former results
      (let [new-run (do-run)
        new-stats (merge-with + new-run result)]
       (recur (dec i) new-stats)))))

; do average-runs and print out the results as a comma-separated list
(do (let [stats (do-n-runs average-runs)]
  (println "results with" total-buttons "buttons," total-steps "threading steps, averaged over" average-runs "runs.")
  (doseq [[k v] stats] (println k "," v) ) 
  ))
 