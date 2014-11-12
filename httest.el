(defmacro example (&rest rest)
  "A macro which ignores its body, useful for example code."
  nil)

(defvar vectors nil) ;; will contain the word2vec vectors
(defun load-word-vectors (filename) 
  "Given a text based output file from word2vec, load the vectors
into a hash table indexed by the symbol for each word."
  (let ((tbl (make-hash-table :test 'eql :size 20000))
		(lines (with-current-buffer (find-file-noselect filename) (prog1 (cdr (buffer-all-lines)) (kill-buffer)))))
	(loop while (car lines) 
		  do
		  (let* ((line (pop lines))
				 (res (read-from-string line))
				 (key (car res))
				 (vec (car (read-from-string (concat "[" (substring line (cdr res)) "]")))))
			(puthash key vec tbl)))
	(setf vectors tbl)))

;; Load the word2vec vectors into memory and get their length
(load-word-vectors "/scratch/vectors.txt")
(defvar word-vector-length (length (gethash 'and vectors)))

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
						 (svec (sentence->vector sentence)))
					  (make-sentence-data :sentence sentence
									 :source file-name
									 :start start
									 :end end
									 :vector svec)) sentences)
			  (forward-thing 'sentence)
			  (setq bnds (bounds-of-thing-at-point 'sentence))
			  finally 
			  (if bnds (push 
						(let* ((start (car bnds))
							   (end (cdr bnds))
							   (sentence (buffer-substring-no-properties start end))
							   (svec (sentence->vector sentence)))
						  (make-sentence-data :sentence sentence
											  :source file-name
											  :start start
											  :end end
											  :vector svec)) 
						sentences)))
		(reverse sentences)))))

(defun nearest-sentence (s from-set)
  "Given a sentence S and a set FROM-SET of sentences, find the best match."
  (let ((svec (sentence-data-vector s))
		(best nil)
		(best-score 2.0))
	(loop for ss in from-set do
		  (let ((d (cos-dist svec (sentence-data-vector ss))))
			(if (< d best-score)
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
		(template-txt (get-file-contents template-file)))
	(with-current-buffer (find-file-noselect output-file-name)
	  (delete-region (point-min) (point-max))
	  (insert template-txt)
	  (goto-char (point-min))
	  (let ((current-offset 0)) 
	  	(loop for s in template-data do 
	  		  (let ((best-match (nearest-sentence s source-data)))
				(goto-char (+ current-offset (sentence-data-end s)))
	  			(delete-region (+ current-offset (sentence-data-start s))
	  						   (+ current-offset (sentence-data-end s)))
	  			(insert (sentence-data-sentence best-match))
	  			(setq current-offset 
	  				  (+ current-offset (- (sentence-data-length best-match)
	  									   (sentence-data-length s)))))))
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
 
 ;; An example of extracting sentence data.
 (collect-sentence-data "./resources/template.txt"))







