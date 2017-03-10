(ns clojure-wtf.core
  (:gen-class))


(comment "
  > 	Increment the pointer.
  < 	Decrement the pointer.
  + 	Increment the byte at the pointer.
  - 	Decrement the byte at the pointer.
  . 	Output the byte at the pointer.
  , 	Input a byte and store it in the byte at the pointer.
  [ 	Jump forward past the matching ] if the byte at the pointer is zero.
  ] 	Jump backward to the [matching unless the byte at the pointer is zero.
  ")


(def initial {:instptr 0
              :instrs ""
              :memptr 0
              :mem [0 0 0 0 0 0 0 0 0 0]})

(def increment #(+ %1 1))
(def decrement #(- %1 1))

; state modification functions

(defn advance
  ([state] (assoc state :instptr (+ 1 (:instptr state))))
  ([state pos] (assoc state :instptr pos)))

(defn setdata [state fun]
  (let [memptr (:memptr state)
        mem (:mem state)
        newval (fun (mem memptr))]
    (assoc state :mem (assoc mem memptr newval))))

(defn find-matching-internal [str pos openchar closechar depth mod]
  (let [curr (nth str pos)
        newpos (mod pos)]
    (cond
      (= openchar curr) (find-matching-internal str newpos openchar closechar (increment depth) mod)
      (and (= closechar curr) (> depth 1)) (find-matching-internal str newpos openchar closechar (decrement depth) mod)
      (not= closechar curr) (find-matching-internal str newpos openchar closechar depth mod)
      :else pos))
  )


(defn find-matching [str from close direction]
  (if (= \] close)
    (find-matching-internal str from \[ \] 0 direction)
    (find-matching-internal str from \] \[ 0 direction)))

; operations

(defn forward [state]
  (let [memptr (:memptr state)]
    (assoc (advance state) :memptr (+ memptr 1))))

(defn backward [state]
    (let [memptr (:memptr state)]
      (assoc (advance state) :memptr (- memptr 1))))

(defn add [state]
  (let [memptr (:memptr state)
        mem (:mem state)
        newval (+ (mem memptr) 1)]
    (advance (setdata state #(+ %1 1)))))

(defn subtract [state]
    (let [memptr (:memptr state)
          mem (:mem state)
          newval (- (mem memptr) 1 )]
      (advance (setdata state #(- %1 1)))))


(defn printmem [state]
  (let [memptr (:memptr state)
        mem (:mem state)]
    (do
      (print (char (mem memptr)))
      (advance state))))

(defn readin [state]
  (let [memptr (:memptr state)
        mem (:mem state)
        input (read-line)]
    (advance (setdata state (fn [_] (byte (first input)))))))

(defn jumpforward [state]
  (let [instptr (:instptr state)
        instrs (:instrs state)
        memptr (:memptr state)
        mem (:mem state)
        curr (mem memptr)]
    (if (= 0 curr)
      (advance state (find-matching instrs instptr \] increment))
      (advance state)))
  )

(defn jumpbackward [state]
  (let [instptr (:instptr state)
        instrs (:instrs state)
        memptr (:memptr state)
        mem (:mem state)
        curr (mem memptr)]
    (if (not= 0 curr)
      (advance state (find-matching instrs instptr \[ decrement))
      (advance state)))
  )

(defn skip [state] (advance state))

; this applies the right function
; could use this same pattern and return the function
; even better could use a map token -> function and not even have this
(defn evaltoken [token]
  (case token
    \> forward
    \< backward
    \+ add
    \- subtract
    \. printmem
    \, readin
    \[ jumpforward
    \] jumpbackward
    skip)
  )

(defn done [state]
  (>= (:instptr state) (count (:instrs state))))

(defn findtoken [state]
  (nth (:instrs state) (:instptr state)))

(defn bf-eval-internal [state]
  (if (done state) state
    (bf-eval-internal ((evaltoken (findtoken state)) state)))
  )

(defn bf-eval [program]
  (bf-eval-internal (assoc initial :instrs program)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [prog (nth args 0)]
    (do
      (println "Evaluating brainfuck program" prog)
      (bf-eval prog)
      (println))))