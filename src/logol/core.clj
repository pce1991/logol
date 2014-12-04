(ns logol.core
  (:require [clojure.string :as s]))

;;; The goal of this library being to find patterns like these in literature
;;; and other instances of text (especially records of unscripted spoken dialogue) 

(def ALPHABET (into [] (seq "abcdefghijklmnopqrstuvwxyz")))

(defn alphabetic-value [letter]
  (inc (.indexOf ALPHABET letter)))

(defn letter [n]
  "returns the letter whose index is n, which must be between 1 and 26"
  (ALPHABET (dec n)))

;;; let this take a negative n
(defn circular-alphabet [n]
  "Takes an int n and returns the letter at that index, but it wraps around so 27 would be A again."
  (if (<= n 26)
    (letter n)
    (let [wrapped (- n (* 26 (if (= 0 (mod n 26))
                               (dec (quot n 26))
                               (quot n 26))))]
      (letter wrapped))))

;;; n must be between -26 and 26
(defn shift-letter [letter n]
  "Takes a letter and returns a new letter that is n spaces to its left or right in the circular alphabet."
  (if (> n 0)
    (circular-alphabet (+ (alphabetic-value letter) n))
    (circular-alphabet (+ (alphabetic-value letter) (+ 26 n)))))

(defn lettershift [word n]
  "Takes a word and shifts each letter n spaces."
  (apply str (map shift-letter word (repeat (count word) n))))

(defn strings->regex [strings]
  (re-pattern (s/join  (interpose "|" strings))))

;;; on these its important to normalize the string to lowercase. 

;;; modify these to return a frequencies map and not just the letters

;;; if its false return which letters were found? return empty if none were?
(defn lipogram? [string omitions]
  "Returns true if the string contains none of the characters in omitions, and if it does, returns which ones."
  (let [found (re-seq (strings->regex (map str omitions)) string)]
                                        ;re-seq so we get all the ones it finds.
    (if found
      found
      true)))
;;; might be more useful to return a map of where the word was found?
;;; that might be best for another function, or a seperate check for the
;;; use case.

(defn char->pattern [char]
  (re-pattern (str char)))

(defn pattern->char [pat]
  (first (seq (str pat))))

(defn re-missing [pattern string]
  "Inversion of re-find, where if it isnt found, returns the missing pattern"
  (if-not (re-find pattern string)
    pattern
    nil))

;;; its important that they be individual letters, or maybe characters,
;;; not a full string. maybe use & instead of a vector?
;;; combine all the letters into a regex that uses and instead of or
(defn antilipo? [string letters]
  "Returns true if word contains them all and if not which letters werent found everywhere."
  (let [missing (map re-missing (map char->pattern letters)
                     (repeat (count letters) string))]
    (if (every? nil? missing)
      true
      (remove nil? (map pattern->char missing))))) 

(defn pangram? [string]
  "Retruns true if the string contains at least one instance of every letter in the alphabet."
  (antilipo? string ALPHABET))

(defn words [string]
  (s/split (s/lower-case string) #" "))

;;; change these four functions to take a string and turn it into words
(defn first-letters [words]
  "Takes a seq of words and returns a seq of the first letters of each word"
  (map str (map first words)))

(defn last-letters [words]
  (map str (map last words)))

(defn first-nth-letters [words n]
  "Returns the first n letters where n is less than the length of the word, and if greater returns the whole word."
  (map #(subs % 0 (if (> n (count %))
                    (count %)
                    n))
       words))

(defn last-nth-letters [words n]
  (map #(subs % (if (< n (count %))
                  (- (count %) n)
                  0))
       words))

;;; a scheme can be generated by using every character in the string (ignoring spaces). What other kind of kinds of schemes can be generated?
(defn generate-scheme [string]
  "Generates a scheme from a string, ignoring whitespace."
  (map s/lower-case (remove #(= % " ") (s/split string #""))))

;;; scheme may just be a seq of letters which must pair up with the string
;;; what if they arent the same length? throw an error, or wrap around
;;; allow scheme to be a word, and if it is generate a scheme on it.
;;; allow spill over, showing which words didnt fit scheme or the runoff
;;; of the scheme that didnt match up against the words. 
(defn acrostic? [string scheme]
  (let [w (words string)
        letters (first-letters w)
        s (if (string? scheme)
            (generate-scheme scheme)
            scheme)]
    (cond (= (count scheme) (count letters))
          (= scheme letters)
          (< (count scheme) (count letters))
          (= letters (repeat (/ (count letters) (count scheme)) scheme))
          ;; scheme may excede or fall short of letter-count, allow overflow?
          (> (count scheme) (count letters))
          false ;maybe allow overflow acrostic, so it fits most of the pattern but not all.
          ;; how to handle first letter of each line spelling out something?
          ;; just give it the first letter of each line.
          ;; what about each word beginning with the end of the last word
          ;; to the nth place. 
          )))




;;; acrostic lines will just take a seq of text and check every beginning


;;; tautonyms, should tell you how many parts it is. 

;;; charade sentences, same as scriptio continua. disregards punct, spacing

;;; word graphs: use Loom

(defn str-reverse [string]
  (apply str (reverse string)))

;;; palindromes: words, sentences, and acronyms
(defn palindrome? [word]
  (= word (str-reverse)))

;;; modify this to look in the dictionary and in wordnet?
(defn word? [word]
  "Returns true if the word is found in UNIX words"
  )


(defn reversal? [word]
  "A reversal is a word that is another word (that is not itself) spelled backwards."
  (and (word? (str-reverse word))
       (not (palindrom? word))))


