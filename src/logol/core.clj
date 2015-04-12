(ns logol.core
  (:require [clojure.string :as s]))

;;; The goal of this library being to find patterns like these in literature
;;; and other instances of text (especially records of unscripted spoken dialogue) 

(defn letters [str]
  (into [] (seq str)))

(def ALPHABET (letters "abcdefghijklmnopqrstuvwxyz"))

(defn alphabetic-value [letter]
  (inc (.indexOf ALPHABET letter)))

(defn letter [n]
  "returns the letter whose index is n, which must be between 1 and 26"
  (ALPHABET n))

;;; let n be negative?
(defn circular-alphabet [n]
  "Takes an int n and returns the letter at that index, but it wraps around so 27 would be A again."
  (letter (mod n 26)))

(defn read-seq
  "Opens a files and line-seqs on the rdr"
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (into [] (line-seq rdr))))

(def CORPUS (into [] (read-seq "resources/words")))
(def MAX-LENGTH (apply max (map count CORPUS)))

;;; modify this to look in the dictionary and in wordnet?
;;; is a word only an english one? More interesting to see what english words
;;; can become a word from another language when shifted, reversed, etcetera
(defn word? [word]
  "Returns true if the word is found in UNIX words"
  (if (some #{word} CORPUS)
    true
    false))

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

;;; n must be between -26 and 26
(defn shift-letter [letter n]
  "Takes a letter and returns a new letter that is n spaces to its left or right in the circular alphabet."
  (if (> n 0)
    (circular-alphabet (+ (alphabetic-value letter) n))
    (circular-alphabet (+ (alphabetic-value letter) (+ 26 n)))))

(defn lettershift [word n]
  "Takes a word and shifts each letter n spaces."
  (apply str (map shift-letter word (repeat (count word) n))))

;;; valide-lettershift? which checks to see if the word made exists

(defn strings->regex [strings]
  (re-pattern (s/join  (interpose "|" strings))))

;;; on these its important to normalize the string to lowercase. 

;;; modify these to return a frequencies map and not just the letters
;;; in fact I could probably write each of them in terms of frequencies
;;; instead of patterns

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

;;; really this is a window. a pangram includes each letter only once!
;;; just do a freq count and make sure that all vals are one and key count
;;; is 26
(defn pangram? [string]
  "Retruns true if the string contains each letter only once."
  (antilipo? string ALPHABET))

;;; returns true if the substring of the string is a pangram.
;;; really necessary?
(defn pangram-here? [string beg end])


(defn isogram? [string]
  "Returns true if the word contains no letter more than once."
  (empty? (filter #(> % 1) (vals (frequencies string)))))

(defn isogram-sentence? [sentence]
  (empty? (filter #(> % 1) (vals (frequencies (words sentence))))))

(defn pangram-window? [string]
  "Returns true if the string contains atleast one instance of every letter."
  (and (= (count (keys (frequencies string))) 26)
       (isogram-sentence? string)))

;;; will look at all possible substrings and check each, returning the
;;; longest one.
(defn contains-pangram? [string])

;;; an isogram sentence is one where a word is only ever used once.
;;; what about different forms of the word?


;;; one where each of its letters appear n times
;;; n-isogram? 

;;; earch letter appears progressively more times. 
;;; pyramid?


;;; a scheme can be generated by using every character in the string (ignoring spaces). What other kind of kinds of schemes can be generated?
(defn generate-scheme [string]
  "Generates a scheme from a string, ignoring whitespace."
  (map s/lower-case (remove #(= % " ") (s/split string #""))))

;;; maybe scheme should be a regex?

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
          )))
;;; how to handle first letter of each line spelling out something?
;;; just give it the first letter of each line.
;;; what about each word beginning with the end of the last word
;;; to the nth place. 
;;; acrostic lines will just take a seq of text and check every beginning

;;; this runs into the same problems as acrostic and is redundant
(defn telestic? [string scheme]
  "Checks the last letters to see if it fits the scheme.")

;;; tautonyms, should tell you how many parts it is.
;;; what about pivoting on a letter if its odd?
(defn tautonym? [word]
  "Returns false if not a tautonym, and if it is then how long."
    
  )

;;; charade sentences, same as scriptio continua. disregards punct, spacing

;;; word graphs: use Loom

(defn str-reverse [string]
  (apply str (reverse string)))

;;; palindromes: words, sentences, and acronyms
(defn palindrome? [word]
  (= word (str-reverse)))

(defn palindorme-phrase? [string]
  "Ignoring whitespace, returns true if the seq of characters is a palindrome."
  (palindrome? (replace string #" " "")))

(defn reversal? [word]
  "A reversal is a word that is another word (that is not itself) spelled backwards."
  (and (word? (str-reverse word))
       (not (palindrome? word))))

;;; ladders: given a word will return three + n words which are made from
;;; subsequent one letter changes to the words (allow adding a character?)
;;; might not be a ladder anymore.

;;; generate all possible anagrams for a word, making sure each is valid.
;;; anagrams

;;; consectutive identitcal letters

;;; cadence: two identical letters seperated by the same number of letters
;;; find the longest instance of one.

;;; anchor???
(defn anchor [word]
  "Turns a word into its anchorized form: a pair of beginning and ending letter."
  [(first word) (last word)])

;;; tautonyms and palindromes embedded in words. 

;;;check for rhymes in 

;;takes an acronym and creates all permutations of those letters.
(defn permutations [acronym]
  )

(defn vowelize [consonants]
  "takes a string of consonants which it turns into a regex and looks up all matches in corpus."
  (let [consoseq (map str (seq consonants))
        regex (re-pattern
               (apply str (interleave (repeat (count consoseq)
                                              "([AOEUIYaoeuiy]*)?") consoseq)))]
    (if (< (count consonants) MAX-LENGTH)
      (remove nil? (for [word CORPUS]
                     (if (re-matches regex word)
                       word
                       nil))))))

;;may not quite be right, and should preserve more order, so each partition should be all the ways you might divide it up, but you know: if you let the first letter stand alone then you can choose these, and if you had the next two grouped together these are your options; but another partition shows you what it's like if you group the first two together instead, then you cant group the second two.
(defn consonants->sentences [consonants-string]
  (let [all-partitions (for [i (range (count consonants-string))]
                         (partition i 1 consonants-string))
        vowels (remove empty? (for [partition all-partitions
                                    word partition]
                                (vowelize (apply str word))))]
    vowels))
