(ns org.whitlark.bencode
  (:use [clojure.contrib.test-is])
  (:use [clojure.contrib.str-utils :only (re-split str-join)])
)

(defmacro debug
  [expr]
  `(let [value# ~expr]
     (println '~expr "=>" value#)
     (flush)
     value#))


(defn digit? [c]
  (contains? (set (map #(str %)(range 10))) (str c)))

(defn drop-first-and-last [s]
  (apply str (drop-last (rest s))))

(defn get-str-length [s]
  (Integer/parseInt (apply str (take-while #(not (= \: %)) s))))

;; -------------------- encode --------------------
(defn- seq-k-and-v [m]
  "There must be a better way"
  (interleave (keys m) (vals m)))

(defn- make-str-and-delimit [delimiter f coll]
  (concat delimiter (map f coll) "e"))

(defn- bencode-type [x & _]
  (cond
    (integer? x)			:integer
    (string? x)				:string
    (map? x)				:map
    (and (not (map? x)) (coll? x))	:coll))

(defmulti encode bencode-type)

(defmethod encode :integer [x]
  (format "i%se" x))

(defmethod encode :string [x]
  (format "%s:%s" (count x) x))

(defmethod encode :coll [x]
  "Not sure if lazy-seq has any meaning in this case, how to test?"
  (apply str (lazy-seq (make-str-and-delimit "l" encode x))))

(defmethod encode :map [x]
  (apply str (lazy-seq (make-str-and-delimit "d" encode (seq-k-and-v x)))))

;; -------------------- decode --------------------

;; Switch to tokenize style

;; (defn decode-any [x]
;;   (cond
;;     (= \i (first x)) (decode-i x)
;;     (digit? (first x)) (decode-s x)
;;   ))

;; (defn decode-i [i]
;;   (Integer/parseInt (drop-first-and-last i)))

;; (defn decode-s [s]
;;   (let [len (get-str-length s)]
;;     (apply str (take len (drop (inc (count (str len))) s)))
;;     ))

;; ---------- Tests ----------

(deftest digit-test
  (is (digit? "1"))
  (is (not (digit? "a")))
)

(deftest encode-any-test
  (is (= (encode "Fish") "4:Fish"))
  (is (= (encode 10) "i10e"))
  (is (= (encode []) "le"))
  (is (= (encode [1 2 3]) "li1ei2ei3ee"))
  (is (= (encode ["Fish" "Cat"]) "l4:Fish3:Cate"))
  (is (= (encode ["Fish" "Cat" 5]) "l4:Fish3:Cati5ee"))
  (is (= (encode {}) "de"))
  (is (= (encode {"name" "bob" "age" 34}) "d4:name3:bob3:agei34ee"))
  (is (= (encode {"Names" ["Bob" "Jane" "Clara" "Jen"] "Count" 3})
  	 "d5:Namesl3:Bob4:Jane5:Clara3:Jene5:Counti3ee"))

  )

;; (deftest decode-any-test
;;   (is (= (decode-any "4:Fish") "Fish" ))
;;   (is (= (decode-any "i10e") 10 ))
;;   )


;; (deftest decode-string
;;   (is (= (decode-s "4:Fish") "Fish"))
;; )

;; (deftest decode-int
;;   (is (= (decode-i "i10e") 10))
;; )

;; (deftest encode-list

;;   ;; (is (= (encode-any ["Fish" "Cat"]) "l4:Fish3:Cate"))

;;   ;; (is (= (encode-any ["Fish" "Cat" 5]) "l4:Fish3:Cati5ee"))

;;   )

;; (deftest encode-dict
;;   (is (= (encode-d {}) "de"))
;;   (is (= (encode-d {"name" "bob" "age" 34}) "d4:name3:bob3:agei34ee"))
;;   (is (= (encode-d {"Names" ["Bob" "Jane" "Clara" "Jen"] "Count" 3})
;; 	 "d5:Namesl3:Bob4:Jane5:Clara3:Jene5:Counti3ee"))
;; )

(deftest encode-many
  (is (= (encode {"Name" "James"
		      "Age" 22
		      "Relatives" ["Bob" "James" "Jenny"]
		      "Address" {"Street" "Smith"
				 "Suburb" "Preston"
				 "Postcode" 5425}})
	 "d4:Name5:James3:Agei22e9:Relativesl3:Bob5:James5:Jennye7:Addressd6:Street5:Smith6:Suburb7:Preston8:Postcodei5425eee"))
)

;; Final test
;; d
;;   4:Name       5:James
;;   3:Age        i22e
;;   9:Relatives  l3:Bob5:James5:Jennye
;;   7:Address
;;   d
;;     6:Street   5:Smith
;;     6:Suburb   7:Preston
;;     8:Postcode i5425e
;;   e
;; e


(run-tests)
