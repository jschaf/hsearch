;;; hoogle.el --- Elisp functions to search Hoogle

;; Author:  Joe Schafer <joe@jschaf.com>
;; Maintainer:  Joe Schafer <joe@jschaf.com>
;; Created: 10 Dec 2011
;; Version:  0.1
;; Keywords:  languages, haskell, hoogle


;;; Commentary:
;; 

(require 'url)
(require 'url-util)
(require 'eieio)

;;; Code:

(defvar hoogle-base-url
  "http://www.haskell.org/hoogle/?hoogle="
  "The base URL to use to search.
Concatenating the query to this string should be a valid
search URL.")

(defvar hoogle-display-buffer "*hoogle*"
  "The buffer in which to display hoogle results.")

(defun hoogle-build-search-url (query)
  "Return a valid hoogle URL for QUERY."
  (concat hoogle-base-url
          (url-hexify-string query)))

;;;###autoload
(defun hoogle-search (query)
  "Search Hoogle for QUERY and return results as alist."
  (interactive)
  ;; TODO: offer way to use a local version.
  (url-retrieve (hoogle-build-search-url query)
                #'hoogle-callback-display-results))

(defclass hoogle-results ()
  ((url :initarg :url)
   (answers :initarg :answers
            :initform '()))
  "A class for the overall hoogle results.")

(defmethod add-answer ((obj hoogle-results) answer)
  "Add a hoogle match to HOOGLE-RESULTS."
  (object-add-to-list obj :answers answer 'append))

(defclass hoogle-answer ()
  ((category :initarg :category)
   (name :initarg :name)
   (signature :initarg :signature)
   (locations :initarg :locations
              :initform '())
   (doc :initarg :doc
        :initform ""))
  "A class for an individual hoogle match.")

(defmethod add-location ((obj hoogle-answer) location)
  "Add a location to HOOGLE-ANSWER.locations."
  (object-add-to-list obj :locations location 'append))

(defvar hoogle-a-tag-regexp
  ">\\(.*?\\)</a>"
  "Extract text from between an <a> tag into the 1st group.")

(defvar hoogle-span-tag-regexp
  "<span>\\(\\([\r\n]\\|.\\)*?\\)</span>"
  "Extract text from between a <span> tag into the 1st group.")

(defsubst hoogle-search-bound (str)
  "Return the location of the next STR or max point.
Doesn't signal an error."
  (or (save-excursion (search-forward str nil 'noerror))
      (point-max)))

(defun hoogle-parse-html ()
  "Parse the current buffer and return a class `hoogle-results'."
  (goto-char (point-min))
  (let (answer
        str
        max-point
        current-point
        location
        (results (make-instance 'hoogle-results)))
    ;; Each entry is a div.ans
    (while (search-forward "class='ans'" nil 'noerror)
      (setq answer (make-instance 'hoogle-answer))

      ;; Get category from the first a.dull
      (search-forward "class='dull'")
      (re-search-forward hoogle-a-tag-regexp)
      ;; empty is function, data is data, class is class, module is
      ;; module
      (setq str (hoogle-strip-tags (match-string 1)))
      (oset answer :category (if (string= "" str) "function" str))
      
      ;; Get name from a.a
      (search-forward "class='a'")
      (re-search-forward hoogle-a-tag-regexp)
      ;; Remove <b> used for substring matching (e.g. map in
      ;; mapAccumL) and convert html entities (e.g. &lt;*&gt; to <*>)
      (setq str (hoogle-strip-tags (match-string 1)))
      (oset answer :name (hoogle-decode-html-entities str))

      ;; Get type signature from second a.dull
      (search-forward "class='dull'")
      (re-search-forward hoogle-a-tag-regexp)
      ;; By stripping tags, we throw away some information.  Namely,
      ;; how hoogle matches signatures that use different type
      ;; variables.  We could add text-properties to the specific
      ;; classes so we don't have redo the logic.
      (setq str (hoogle-strip-tags (match-string 1)))
      (oset answer :signature (hoogle-decode-html-entities str))
      
      ;; Get defined locations from div.from.
      (search-forward "class='from'")
      (setq max-point (hoogle-search-bound "</div>"))
      (loop do
            (search-forward "class='p1'" max-point)
            (re-search-forward hoogle-a-tag-regexp)
            (setq location (list (match-string 1)))

            
            (if (search-forward "class='p2'" max-point 'noerror)
                (progn (re-search-forward hoogle-a-tag-regexp)
                       (nconc location (list (match-string 1)))))

            (add-location answer location)
            while
            (save-excursion (search-forward "class='p" max-point 'noerror)))
      (goto-char max-point)

      ;; Get optional documentation string.  Maybe spread across
      ;; multiple lines.  If it exists, assume it's always wrapped in
      ;; a span tag.
      (setq current-point (point))
      (setq max-point (hoogle-search-bound "class='ans'"))
      (if (search-forward "class='doc" max-point 'noerror)
          (progn (re-search-forward hoogle-span-tag-regexp)
                 (setq str (hoogle-strip-tags (match-string 1)))
                 (oset answer :doc (hoogle-decode-html-entities str)))
        ;; If there is no doc, we need to go back because the max
        ;; bound for doc was "class='ans'".  Since search-forward
        ;; moved us to the end of the match, we won't see it next
        ;; iteration unless we move back.
        (goto-char current-point))

      (add-answer results answer))
    results))

(defsubst hoogle-strip-tags (str)
  "Remove tags from STR."
  (replace-regexp-in-string "</?[^<>]+>" "" str))

(defun hoogle-decode-html-entities (str)
  "Replace HTML entities in STR with their decoded value.

Only replaces those entities common to Haskell, i.e. ones seen in
type signatures.

I feel like this function is already somewhere inside Emacs."
  (let* ((entity-pairs '(("&amp;" . "&")
                         ("&lt;" . "<")
                         ("&gt;" . ">")
                         ("&quot;" . "\"")))
         (encoded-entities (mapcar 'car entity-pairs))
         (decoded-entities (mapcar 'cdr entity-pairs))
         (entity-regexp (regexp-opt encoded-entities)))
    (replace-regexp-in-string
     entity-regexp
     (lambda (match-text)
       (loop for key in encoded-entities
             for value in decoded-entities
             if (string= match-text key) return value))
     str)))

(defun hoogle-callback-display-results (status &optional cbargs)
  "The callback function for `url-retrieve'.
Argument STATUS result of `url-retrieve'.
Optional argument CBARGS optional callback args passed from `url-retrieve'."
  (when status
    ;; FIXME: error handling
    nil)
  (when cbargs
    ;; I don't think we need any additional args
    nil)
  (let ((results (hoogle-parse-html)))
    (hoogle-font-lock-results results)
    (with-current-buffer (get-buffer-create hoogle-display-buffer)
      (erase-buffer)
      (hoogle-render-results results)))
  (pop-to-buffer hoogle-display-buffer))

(defun hoogle-render-results (results)
  "Render RESULTS in the current buffer."
  (let ((answers (oref results :answers)))
    (loop for answer in answers
          do
          (hoogle-render-answer answer))))

(defun hoogle-font-lock-results (results)
  "Font lock RESULTS."
  (loop for answer in (oref results :answers)
        do (hoogle-font-lock-answer answer)))

(defsubst hoogle-propertize-string (str &rest properties)
  "Modify STR to include PROPERTIES."
  (set-text-properties 0 (length str) properties str))

(defun hoogle-font-lock-answer (answer)
  "Font lock all fields within ANSWER."
  (let* ((pairs '((:category . font-lock-comment-face)
                  (:name . font-lock-function-name-face)
                  (:signature . font-lock-type-face)
                  ;; :locations
                  (:doc . font-lock-doc-face)))
         (font-locker (lambda (pair)
                        (let* ((field (car pair))
                               (face (cdr pair))
                               (old (slot-value answer field)))
                          (set-slot-value answer field
                                          (propertize old 'face face))))))
    (mapc font-locker pairs)))

(defun hoogle-render-answer (answer)
  "Render ANSWER in the current buffer."
  (let ((category (oref answer :category))
        (name (oref answer :name))
        (signature (oref answer :signature))
        (locations (oref answer :locations))
        (doc (oref answer :doc)))
    (hoogle-render-category category)
    (hoogle-render-name name)
    (hoogle-render-signature signature)
    (hoogle-render-locations locations)
    (hoogle-render-doc doc)))

(defun hoogle-render-category (str)
  "Render the category STR in the current buffer."
  (unless (string= "function" str)
    (insert str " ")))

(defun hoogle-render-name (str)
  "Render the name STR in the current buffer."
  (insert str))

(defun hoogle-render-signature (str)
  "Render the signature STR in the current buffer."
  (insert str)
  (insert "\n"))

(defun hoogle-render-locations (locations)
  "Render LOCATIONS in the current buffer."
  (insert
   (mapconcat (lambda (location)
                (mapconcat #'concat location "."))
              locations ", "))
  (insert "\n"))

(defun hoogle-render-doc (str)
  "Render the doc STR in the current buffer."
  (insert str)
  (insert "\n\n\n"))

;; Enable lexical binding.  Shouldn't affect Emacsen without lexbind
;; support.

;; Local Variables:
;; lexical-binding: t
;; End:

(provide 'hoogle)

;;; hoogle.el ends here
