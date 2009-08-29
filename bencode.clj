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

;; -------------------- encode --------------------
(defn- seq-k-and-v [m]
  "There must be a better way"
  (interleave (keys m) (vals m)))

(defn- handle-collection [delimiter f coll]
  (apply str (lazy-seq (concat delimiter (map f coll) "e"))))

(defn- encode-bencode-type [x & _]
  (cond
    (integer? x)			:integer
    (string? x)				:string
    (map? x)				:map
    (and (not (map? x)) (coll? x))	:coll))

(defmulti encode encode-bencode-type)

(defmethod encode :integer [x]
  (format "i%se" x))

(defmethod encode :string [x]
  (format "%s:%s" (count x) x))

(defmethod encode :coll [x]
  "Not sure if lazy-seq has any meaning in this case, how to test?"
  (handle-collection "l" encode x))

(defmethod encode :map [x]
  (handle-collection "d" encode (seq-k-and-v x)))

;; -------------------- decode --------------------
(defn- digit? [c]
  (contains? (set (map #(str %)(range 10))) (str c)))

(defn- drop-first-and-last [s]
  (apply str (drop-last (rest s))))

(defn- get-str-length [s]
  (Integer/parseInt (apply str (take-while #(not (= \: %)) s))))

(defn- decode-bencode-type [x & _]
  (cond
    (= \i (first x))	:integer
    (digit? (first x))	:string
    (= \d (first x))	:map
    (= \l (first x))	:coll
;;    (empty? x)          :done
    ))

(defmulti decode decode-bencode-type)

(defmethod decode :integer [remaining stack accum]
  (let [end-idx (.indexOf remaining "e")
	integer (Integer/parseInt (.substring remaining 1 end-idx))]
    integer))

(defmethod decode :string [remaining stack accum]
  (let [str-len (get-str-length remaining)
	len-len (inc (count (str str-len)))
	total-len (+ len-len str-len)
	string (.substring remaining len-len total-len)]
    string))

;; (defmethod decode :done [remaining stack accum]
;;   accum)

;; (defmethod decode :coll [x]
;;   (
;; ---------- Tests ----------

(deftest digit-test
  (is (digit? "1"))
  (is (not (digit? "a")))
  (is (= (decode-bencode-type "i33e") :integer))
  (is (= (decode-bencode-type "4:Fish") :string ))
  (is (= (decode-bencode-type "li1ei2ei3ee") :coll))
  (is (= (decode-bencode-type  "d4:name3:bob3:agei34ee") :map))
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

(deftest decode-any-test
  (is (= (decode "4:Fish" [] []) "Fish" ))
  (is (= (decode "i10e" [] []) 10 ))
  ;; (is (= (decode "le") [] ))
  ;; le -> :list-start :list-end
  ;; l4:Fishi10ee -> :list-start 4:Fish i10e :list-end
  )

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
