(defmacro example (&rest rest)
  "A macro which ignores its body, useful for example code."
  nil)

(defvar vectors nil) ;; will contain the word2vec vectors
(defvar word-vector-length nil)
(defun load-word-vectors (filename) 
  "Given a text based output file from word2vec, load the vectors
into a hash table indexed by the symbol for each word."
  (let ((tbl (make-hash-table :test 'eql :size 20000))
		(lines (with-current-buffer (find-file-noselect filename) (prog1 (cdr (buffer-all-lines)) (kill-buffer)))))
	(loop while (car lines) 
		  do
		  (let* ((line-parts (split-string (pop lines) " "))
				 (key (intern (car line-parts)))
				 (vec (car (read-from-string (concat "[" (join (cdr line-parts)) "]")))))
			(puthash key vec tbl)))
	(setf vectors tbl)
	(setf word-vector-length (length (gethash (car (keyshash tbl)) tbl)))))

;; Load the word2vec vectors into memory and get their length
;;(load-word-vectors "/scratch/vectors.txt")
(load-word-vectors "./resources/swtnl-vectors.txt")


(defun cos-dist (v1 v2)
  "Given two equal-length vectors, return a number between 0 and
1.  0 indicates the vectors are identical while 1 indicates they
are as different as possible."
  (let ((dot 0)
		(m1 0)
		(m2 0))
	(cond ((and v1 v2) 
		   (loop for i from 0 below (length v1) do
				 (let ((e1 (aref v1 i))
					   (e2 (aref v2 i)))
				   (setq dot (+ dot (* e1 e2)))
				   (setq m1 (+ m1 (* e1 e1)))
				   (setq m2 (+ m2 (* e2 e2)))))
		   (- 1 (/ dot (* (sqrt m1) (sqrt m2)))))
		  (:otherwise 0))))

(defun word-dist (w1 w2)
  "Given two words as Emacs Lisp symbols, return the cos-dist between the two."
  (cos-dist (gethash w1 vectors)
			 (gethash w2 vectors)))

;; A structure for holding information about sentences.
(cl-defstruct sentence-data sentence source start end vector)

(defun sentence-data-length (s)
  "Return the lenght, in characters, of a sentence."
  (- (sentence-data-end s)
	 (sentence-data-start s)))

(defun text->words (txt)
  "Given a string of text, extract just the words, without punctuation."
  (let ((words nil)) 
	(with-temp-buffer 
	  (insert txt)
	  (goto-char (point-min))
	  (forward-word);; goto the first word's end
	  (let ((bnds (bounds-of-thing-at-point 'word))
			(words (list)))
		(loop while (and bnds
						 (not (>= (cdr bnds) (point-max)))) 
			  
			  do 
			  (push (buffer-substring-no-properties (car bnds) (cdr bnds)) words)
			  (forward-thing 'word)
			  (setq bnds (bounds-of-thing-at-point 'word))
			  finally 
			  (if bnds (push (buffer-substring-no-properties (car bnds) (cdr bnds)) words)))
		(reverse words)))))

(defun sentence->vector (sntc)
  "Convert a sentence to a vector by summing up its word vectors."
  (let ((words (text->words sntc))
		(vec (make-vector word-vector-length 0.0)))
	(loop for word in words do 
		  (let ((wvec (gethash (intern word) vectors)))
			(if wvec (loop for i from 0 below 200 do 
						   (aset vec i (+ (aref vec i) (aref wvec i)))))))
	vec))

(example 
 (sentence->vector "Hello world"))

(defun collect-sentence-data (file-name)
  "Given a file name, extract all the sentences from the file and
calculate their vectors.  Return the result as a list."
  (with-current-buffer (find-file-noselect file-name)
	(save-excursion 	  
	  (goto-char (point-min))
	  (forward-thing 'sentence);; goto the first word's end
	  (let ((bnds (bounds-of-thing-at-point 'sentence))
			(sentences (list)))
		(loop while (and bnds
						 (not (>= (cdr bnds) (point-max)))) 
			  
			  do 
			  (push (let*
						((start (car bnds))
						 (end (cdr bnds))
						 (sentence (buffer-substring-no-properties start end))
						 (sentence-vector (sentence->vector sentence)))
					  (make-sentence-data :sentence sentence
									 :source file-name
									 :start start
									 :end end
									 :vector sentence-vector)) sentences)
			  (forward-thing 'sentence)
			  (setq bnds (bounds-of-thing-at-point 'sentence))
			  finally 
			  (if bnds (push 
						(let* ((start (car bnds))
							   (end (cdr bnds))
							   (sentence (buffer-substring-no-properties start end))
							   (sentence-vector (sentence->vector sentence)))
						  (make-sentence-data :sentence sentence
											  :source file-name
											  :start start
											  :end end
											  :vector sentence-vector)) 
						sentences)))
		(reverse sentences)))))

(defun dump-file-words (infile outfile)
  (let ((out-buffer (find-file-noselect outfile))) 
	(flet ((output (&rest args)
				   (with-current-buffer out-buffer 
					 (apply #'insert args))))
	  (with-current-buffer out-buffer
		(kill-region (point-min) (point-max)))
	  (with-current-buffer (find-file-noselect infile)
		(save-excursion 
		  (goto-char (point-min))
		  (forward-word) ;; goto the first word's end
		  (let ((bnds (bounds-of-thing-at-point 'word)))
			(loop while (and bnds
							 (not (>= (cdr bnds) (point-max)))) 
				  
				  do 
				  (output (downcase (buffer-substring-no-properties (car bnds) (cdr bnds))))
				  (output " ")
				  (forward-thing 'word)
				  (setq bnds (bounds-of-thing-at-point 'word))
				  finally 
				  (if bnds (downcase (output (buffer-substring-no-properties (car bnds) (cdr bnds)))))))))
	  (with-current-buffer out-buffer (save-buffer)))))


;; Define a circular buffer to hold excluded sentences.  We'll use an
;; array to minimize consing.
(cl-defstruct circular-buffer array index)

(defun circular-buffer-insert (ca val)
  "Insert a value into the circular buffer."
  (let ((i (circular-buffer-index ca))
		(array (circular-buffer-array ca))) 
	(aset array i val)
	(setf (circular-buffer-index ca)
		  (mod (+ i 1) (length array)))))

(defun in-array (obj array test)
  "Returns T when OBJ is in the array ARRAY by TEST"
  (let ((result nil))
	(loop for item across array until (progn (setq result (funcall test item obj)) result))
	result))

(defun in-circular-buffer (obj cb test)
  "Same as IN-ARRAY but for a circular buffer."
  (in-array obj (circular-buffer-array cb) test))

(defun nearest-sentence (s from-set exclude)
  "Given a sentence S and a set FROM-SET of sentences, find the best match."
  (let ((svec (sentence-data-vector s))
		(best nil)
		(best-score 2.0))
	(loop for ss in from-set do
		  (let ((d (cos-dist svec (sentence-data-vector ss))))
			(if (and (< d best-score)
					 (not (in-circular-buffer ss exclude #'eq)))
				(progn 
				  (setq best ss)
				  (setq best-score d)))))
	(if (not best) (warn "Couldn't find match for sentence %s:" (sentence-data-sentence s)))
	(or best s)))

(defun get-file-contents (file)
  "Extract the file contents of FILE."
  (with-current-buffer (find-file-noselect file)
	(buffer-substring-no-properties (point-min) (point-max))))

(defun perform-substitution (template-file source-file output-file-name)
  "Perform a substitution of all sentences from TEMPLATE-FILE
with their closest matches from SOURCE-FILE and store the result
in OUTPUT-FILE-NAME."
  (let ((template-data (collect-sentence-data template-file))
		(source-data (collect-sentence-data source-file))
		(template-txt (get-file-contents template-file))
		;; we exclude from use the last ten used sentences.
		(exlude (make-circular-buffer :array (make-vector 10 :no-match) :index 0)))
	(with-current-buffer (find-file-noselect output-file-name)
	  (delete-region (point-min) (point-max))
	  (insert template-txt)
	  (goto-char (point-min))
	  (let ((current-offset 0)) 
	  	(loop for s in template-data do 
	  		  (let ((best-match (nearest-sentence s source-data exlude)))
				(circular-buffer-insert exlude best-match)
				(goto-char (+ current-offset (sentence-data-end s)))
	  			(delete-region (+ current-offset (sentence-data-start s))
	  						   (+ current-offset (sentence-data-end s)))
	  			(insert (sentence-data-sentence best-match))
	  			(setq current-offset 
	  				  (+ current-offset (- (sentence-data-length best-match)
	  									   (sentence-data-length s)))))))
	  (save-buffer))))

(defun perform-substitution->html (template-file source-file output-file-name)
  "Perform a substitution of all sentences from TEMPLATE-FILE
with their closest matches from SOURCE-FILE and store the result
in OUTPUT-FILE-NAME.  This version generates an HTML file with
tool-tips showing the source sentence for each sentence, thus
revealing that the almost random results."
  (let ((template-data (collect-sentence-data template-file))
		(source-data (collect-sentence-data source-file))
		(template-txt (get-file-contents template-file))
		(exlude (make-circular-buffer :array (make-vector 10 :no-match) :index 0)))
	(with-current-buffer (find-file-noselect output-file-name)
	  (delete-region (point-min) (point-max))
	  (insert template-txt)
	  (goto-char (point-min))
	  (let ((current-offset 0)) 
	  	(loop for s in template-data do 
	  		  (let ((best-match (nearest-sentence s source-data exlude)))
				(circular-buffer-insert exlude best-match)
				(goto-char (+ current-offset (sentence-data-end s)))
	  			(delete-region (+ current-offset (sentence-data-start s))
	  						   (+ current-offset (sentence-data-end s)))
	  			(let ((to-insert (format "<span title='%S'>%s</span>" 
										 (replace-regexp-in-string "[[:cntrl:]]+" " " (sentence-data-sentence s)) 
										 (sentence-data-sentence best-match)))) 
				  (insert to-insert)
				 (setq current-offset 
					   (+ current-offset (- (length to-insert)
											(sentence-data-length s))))))))
	  (goto-char (point-min)) 
	  (insert "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"><title></title></head><body>")
	  (goto-char (point-max))
	  (insert "</body></html>")
	  (save-buffer))))

(defun matrix (template-file source-file)
  "A utility function for comparing sentence sets."
  (let ((s1 (collect-sentence-data template-file))
		(s2 (collect-sentence-data source-file)))
	(loop for ss1 in s1 do
		  (loop for ss2 in s2 do
				(insertf "%s\t" (cos-dist (sentence-data-vector ss1) (sentence-data-vector ss2))))
		  (insertf "\n"))))

(example 
 ;; A very simple example where two almost identical stories are used.
 ;; The first, upon substitution, becomes the second.
 (perform-substitution "./resources/template.txt" "./resources/source.txt" "/tmp/test-out.txt")

 ;; A short sample of The Night Land and Swann's Way is used for testing
 (perform-substitution "./resources/tnl-sample.txt" "./resources/swannsway-sample.txt" "/tmp/test-out.txt")

 ;; Generate a novel.
 (perform-substitution "./resources/tnl.txt" "./resources/swannsway.txt" "./output/swanns-way-through-the-night-land.txt")

 ;; Generate a novel.
 (perform-substitution->html "./resources/tnl.txt" "./resources/bible.txt" "./output/kjv-night-land.txt")
 
 (perform-substitution->html "./resources/tnl.txt" "./resources/swannsway.txt" "./output/swanns-way-through-the-night-land.html")

 (dump-file-words "./resources/tnl.txt" "./resources/tnl-words.txt")
 (dump-file-words "./resources/swannsway.txt" "./resources/swanns-way-words.txt")

 (perform-substitution "./resources/tnl.txt" "./resources/swannsway.txt" "./output/swanns-way-through-the-night-land-custom-vectors-exclude-repeats.txt")
 (perform-substitution->html "./resources/tnl.txt" "./resources/swannsway.txt" "./output/swanns-way-through-the-night-land-custom-vectors-exclude-repeats.html")
 (perform-substitution->html "./resources/tnl.txt" "./resources/good-bad.txt" "./output/good-bad-the-night-land.html")

 (gethash (intern "it's") vectors)
 ;; An example of extracting sentence data.
 (collect-sentence-data "./resources/template.txt"))


