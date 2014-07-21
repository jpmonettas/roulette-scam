(defn spin-roulette []
  (int (rand 36)))

(defn roulette-lazy-seq []
  (cons (spin-roulette)
        (lazy-seq (roulette-lazy-seq))))

(defn number-color [n]
  (cond
   (zero? n) :green
   (even? n) :black
   (odd? n) :red))

(defn gen-cycle
  "Given a roulette-spins, an initial state {:money :bet} and a play function that
moves between states, returns a seq of your states flow"
  [init-state play-func roulette-spins]
  (->> roulette-spins
       (map number-color)
       (reductions play-func init-state)))

(defn gen-all-cycles
  "Generates n roulettes and play them with play-func"
  [n init-state play-func]
  (->> (repeatedly n roulette-lazy-seq)
       (map (partial gen-cycle init-state play-func))))

(defn write-cycles-to-file
  "Write a sequence of cycles to a file, so we can plot it with octave"
  [file-path cycles]
  (with-open [wtr (clojure.java.io/writer file-path)]
    (doseq [cycle cycles]
      (doseq [n cycle]
        (.write wtr (str n" ")))
      (.write wtr "\n"))))

(defn play-scam
  "Given a roulette state(money and bet) and a color, calculates
the next roulette state assuming we are always betting on red with the rules
described in the scam site :
http://www.instant-income-system.com/members/uk/index.html "
  [{money :money bet :bet} color]
  (let [we-won (= color :red)]
    (if we-won
      {:money (+ money (* bet 2))
       :bet 1}
      {:money (- money (* bet 2))
       :bet (* bet 2)})))

#_(->> (gen-all-cycles 10
                       {:money 100 :bet 1}
                       play-scam)
       (map #(take-while pos? (map :money %)))
       (write-cycles-to-file "/home/jmonetta/new-roulette.data"))
