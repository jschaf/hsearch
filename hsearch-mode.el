;;; hsearch-mode.el --- a major mode to search Haskell

;; Author:  Joe Schafer <joe@jschaf.com>
;; Maintainer:  Joe Schafer <joe@jschaf.com>
;; Created: 10 Dec 2011
;; Version:  0.1
;; Keywords:  languages, haskell, hoogle, hayoo

;;; Installation:
;;
;; Place hsearch-mode.el on your `load-path' by adding this to your
;; `user-init-file', usually ~/.emacs or ~/.emacs.d/init.el
;;
;; (add-to-list 'load-path "~/YOUR_LOAD_PATH")
;;
;; Load the code:
;;
;; (autoload 'hsearch "hsearch"
;;           "Function to search for Haskell." nil)

;;; Commentary:
;;
;; The most recent code is at http://github.com/jschaf/hsearch
;;
;; `hsearch-query' is the overall class, containing all meta-info
;; including the query and url as well as a list of all results.  This
;; class implements `hsearch-renderable', an interface that provides
;; `render'.
;;
;; `hsearch-result' is class for each indiviual result returned from
;; the query.

;;; Todo:
;;
;; * Headers with query and url
;; 
;; * Use local version of Hoogle if available.
;; 
;; * Parse links in Hoogle output and linkify *hsearch* buffer.
;;
;; * Hoogle pagination
;;
;; * Fill paragraphs in long doc strings and break up multiple
;;   locations

(require 'button)
(require 'eieio)
(require 'url)
(require 'url-util)

;;; Code:


;;; hsearch-mode

(defgroup hsearch nil
  "A major mode for searching Haskell."
  :prefix "hsearch-"
  :group 'languages)

(defcustom hsearch-mode-hook nil
  "Hook to run when starting hsearch mode."
  :type 'hook
  :group 'hsearch)

(defcustom hsearch-prompt "λ-search: "
  "Minibuffer prompt."
  :type 'string
  :group 'hsearch)

(defcustom hsearch-search-rank-list
  '(("hoogle" . hoogle-search)
    ("hayoo" . hayoo-search))
  "An alist of search engines and search function by preference.")

(defface hsearch-category
  '((t :inherit font-lock-comment-face))
  "A face for the category of an `hsearch-result' class."
  :group 'hsearch)

(defface hsearch-name
  '((t :inherit font-lock-function-name-face))
  "A face for the name of an `hsearch-result' class."
  :group 'hsearch)

(defface hsearch-type
  '((t :inherit font-lock-type-face))
  "A face for the type signature of an `hsearch-result' class."
  :group 'hsearch)

(defface hsearch-module-base
  '((t :inherit (list variable-pitch font-lock-comment-face)))
  "A face for the module base of an `hsearch-result' class."
  :group 'hsearch)

(defface hsearch-module-name
  '((t :inherit (list variable-pitch font-lock-builtin-face)))
  "A face for the module name of an `hsearch-result' class."
  :group 'hsearch)

(defface hsearch-doc
  '((t :inherit (list variable-pitch font-lock-doc-face)))
  "A face for the doc of an `hsearch-result' class."
  :group 'hsearch)

(defvar hsearch-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-buffer-map)

    ;; (define-key map [mouse-2] 'hsearch-follow-mouse)
    ;; (define-key map "\C-c\C-b" 'hsearch-go-back)
    ;; (define-key map "\C-c\C-f" 'hsearch-go-forward)
    ;; (define-key map "\C-c\C-c" 'hsearch-follow-symbol)
    ;; ;; Documentation only, since we use minor-mode-overriding-map-alist.
    ;; (define-key map "\r" 'hsearch-follow)
    map)
  "Keymap for `hsearch-mode'.")

(defun hsearch-mode ()
  "Major mode for searching Haskell.

Commands:
\\{hsearch-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map hsearch-mode-map)
  (setq mode-name "λ-search")
  (setq major-mode 'hsearch-mode)

  (view-mode)

  (set (make-local-variable 'word-wrap) t)
  (set (make-local-variable 'view-no-disable-on-exit) t)
  ;; With Emacs 22 `view-exit-action' could delete the selected window
  ;; disregarding whether the help buffer was shown in that window at
  ;; all.  Since `view-exit-action' is called with the help buffer as
  ;; argument it seems more appropriate to have it work on the buffer
  ;; only and leave it to `view-mode-exit' to delete any associated
  ;; window(s).
  (setq view-exit-action
	(lambda (buffer)
	  ;; Use `with-current-buffer' to make sure that `bury-buffer'
	  ;; also removes BUFFER from the selected window.
	  (with-current-buffer buffer
	    (bury-buffer))))

  ;; (set (make-local-variable 'revert-buffer-function)
  ;;      'help-mode-revert-buffer)

  (run-mode-hooks 'hsearch-mode-hook))

(defun hsearch-read-prompt ()
  "Read from prompt and return (query . search-function)."
  (let* (raw-query
         (pref-search-pair (car hsearch-search-rank-list))
         (pref-search-str (car pref-search-pair)))
    
    (setq raw-query (read-from-minibuffer hsearch-prompt
                                      (concat pref-search-str ":")))
    (hsearch-parse-prompt raw-query)))

(defun hsearch-parse-prompt (raw-query)
  "Parse RAW-QUERY and return (query . search-function)."
  (let* (query
         search-engine-str
         search-engine-func
         (pref-search-pair (car hsearch-search-rank-list))
         (pref-search-str (car pref-search-pair)))

    (setq search-engine-str
          (if (and (string-match "^\\([a-z]+:\\)?\\(.*\\)" raw-query)
                   (match-string 1 raw-query))
              ;; Remove colon
              (substring (match-string 1 raw-query) 0 -1)
            pref-search-str))

    (unless (setq search-engine-func
                  (cdr (assoc search-engine-str hsearch-search-rank-list)))
      (error "Hsearch error: `%s' does not match a search engine"
             search-engine-str))

    (setq query (match-string 2 raw-query))
    (when (string= "" query)
      (error "Hsearch error: query was empty"))

    (cons query search-engine-func)))

;;;###autoload
(defun hsearch (&optional raw-query)
  "Prompt for a query and display the results.

If RAW-QUERY is non-nil, use it as the query instead of
prompting."
  (interactive)
  (let* ((search-pair (if raw-query
                          (hsearch-parse-prompt raw-query)
                        (hsearch-read-prompt)))
         (search-str (car search-pair))
         (search-func (cdr search-pair)))
    (funcall search-func search-str)))

(defun hsearch-buffer ()
  "Initialize and return the *hsearch* buffer."
  (let ((buf (get-buffer hsearch-display-buffer)))
    (if buf
        buf
      (setq buf (generate-new-buffer hsearch-display-buffer))
      (with-current-buffer buf
        (hsearch-mode)))
    buf))

;;; Rendering

(defvar hsearch-display-buffer "*hsearch*"
  "The buffer in which to display query results.")

(defclass hsearch-renderable () (())
  "Interface to provide `render'."
  :abstract t)
(defmethod render ((obj hsearch-renderable))
  (error "Called abstract method: Must define `render'"))

(defclass hsearch-query (hsearch-renderable)
  ((query :initarg :query)
   (url :initarg :url)
   (results :initarg :results
            :initform '()))
  "A class for the overall results.")

(defmethod add-result ((obj hsearch-query) result)
  "Add a result HSEARCH-QUERY."
  (object-add-to-list obj :results result 'append))

(defmethod render ((obj hsearch-query))
  "Render RESULTS in the current buffer."
  (with-current-buffer (hsearch-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (goto-char (point-min))
      (loop for result in (oref obj :results)
            do
            (insert (render result))
            (insert "\n"))))
  (pop-to-buffer hsearch-display-buffer))

(defclass hsearch-result (hsearch-renderable)
  ((category :initarg :category)
   (name :initarg :name)
   (signature :initarg :signature)
   (locations :initarg :locations
              :initform '())
   (doc :initarg :doc
        :initform ""))
  "A class for an individual result.")

(defmethod add-location ((obj hsearch-result) location)
  "Add a location to HSEARCH-RESULT.locations."
  (object-add-to-list obj :locations location 'append))

(defsubst hsearch-propertize-string (str &rest properties)
  "Replace all properties of STR with PROPERTIES."
  (set-text-properties 0 (length str) properties str)
  str)

(defun hsearch-fontify-string (str face)
  "Modify STRs font-lock-face property to FACE and return STR."
  (hsearch-propertize-string str 'font-lock-face face))

(defsubst hsearch-empty-or-string (str format-str)
  "If STR then return FORMAT-STR, else return the empty string."
  (if (string= "" str)
      ""
    (format format-str str)))

(defmethod render ((obj hsearch-result))
  "Render fields within HSEARCH-RESULT."
  (with-slots (category name signature locations doc)
      obj
    ;; This must be in the right order
    (let* ((category-str (hsearch-empty-or-string (render category) "%s "))
           (name-str (render name))
           (signature-str (hsearch-empty-or-string (render signature) " %s"))
           (doc-str (hsearch-empty-or-string (render doc) "%s\n")))
      (concat category-str
              name-str
              signature-str
              "\n"
              ;; Locations is a list of location objects
              (mapconcat (lambda (loc) (render loc)) locations
                         (hsearch-fontify-string ", " 'hsearch-doc))
              "\n"
              doc-str))))

(defclass hsearch-result-category (hsearch-renderable)
  ((category :initarg :category
             :initform ""))
  "The category of a hsearch-result.")

(defmethod render ((obj hsearch-result-category))
  "Render the category for `hsearch-result'."
  (with-slots (category) obj
    (hsearch-fontify-string category 'hsearch-category)
    ;; Don't specify functions, that's the default
    (unless (string= "function" category)
      category)))

(defclass hsearch-result-name (hsearch-renderable)
  ((name :initarg :name)
   (doc-link :initarg :doc-link
             :initform ""))
  "The name of a hsearch-result.")

(defmethod render ((obj hsearch-result-name))
  "Render the name for an HSEARCH-RESULT."
  (with-slots (name doc-link) obj
    (hsearch-propertize-string
     name
     'font-lock-face 'hsearch-name
     'mouse-face 'highlight
     'help-echo (if (string= doc-link "")
                    nil
                  (format "LINK: %s" doc-link)))))

(defclass hsearch-result-signature (hsearch-renderable)
  ((signature :initarg :signature))
  "The type signature for a `hsearch-result'.
SIGNATURE should include the double-colon, e.g ':: a -> b'")

(defmethod render ((obj hsearch-result-signature))
  "Render HSEARCH-RESULT-SIGNATURE in the current buffer."
  (with-slots (signature) obj
    (hsearch-fontify-string signature 'hsearch-type)))

(defclass hsearch-result-location ()
  ((module-base :initarg :module-base)
   (module-base-link :initarg :module-base-link)
   (module-name :initarg :module-name
                :initform "")
   (module-name-link :initarg :module-name-link))
  "A location that provides the query.")

(defmethod render ((obj hsearch-result-location))
  "Render a location for `hsearch-result'."
  (with-slots (module-base module-base-link
               module-name module-name-link)
      obj
    (hsearch-fontify-string module-base 'hsearch-module-base)
    (hsearch-fontify-string module-name 'hsearch-module-name)
    (concat module-base
            ;; Looks strange if this space is monospaced between two
            ;; variable spaced strings
            (hsearch-fontify-string " " 'hsearch-module-name)
            module-name)))

(defclass hsearch-result-doc ()
  ((doc :initarg :doc))
  "The documentation for the result.")

(defmethod render ((obj hsearch-result-doc))
  "Render HSEARCH-RESULT-DOC in the current buffer."
  (with-slots (doc) obj
    (hsearch-fontify-string doc 'hsearch-doc)))


;;; Utilites

(defun hsearch-fill-text-string (str)
  "Fill STR with `fill-region' and return STR."
  (with-temp-buffer
    ;; Space is normalized in HTML to one space, so there are no
    ;; double spaces to end a sentence.
    (let ((sentence-end-double-space nil)
          (fill-column 78))
      (insert str)
      (fill-region (point-min) (point-max))
      (buffer-substring (point-min) (point-max)))))

(defsubst hsearch-strip-tags (str)
  "Remove all HTML tags from STR."
  (replace-regexp-in-string "</?[^<>]+>" "" str))

(defun hsearch-decode-html-entities (str)
  "Replace HTML entities in STR with their literal value.

Only replaces those entities common to Haskell, i.e. ones seen in
type signatures.

I feel like this function is already somewhere inside Emacs."
  (let* ((entity-pairs '(("&amp;" . "&")
                         ("&lt;" . "<")
                         ("&gt;" . ">")
                         ("&quot;" . "\"")))
         (encoded-entities (mapcar 'car entity-pairs))
         (entity-regexp (regexp-opt encoded-entities)))
    (replace-regexp-in-string
     entity-regexp
     (lambda (match-text) (cdr (assoc match-text entity-pairs)))
     str)))

(defun hsearch-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)


;;; Hoogle support

(defvar hoogle-base-url
  "http://www.haskell.org/hoogle/?hoogle="
  "The base URL to use to search.
Concatenating the query to this string should be a valid
search URL.")

(defun hoogle-build-search-url (query)
  "Return a valid hoogle URL for QUERY."
  (concat hoogle-base-url
          (url-hexify-string query)))

;;;###autoload
(defun hoogle-search (query)
  "Search Hoogle for QUERY and return results as alist."
  (interactive)
  (let ((url (hoogle-build-search-url query)))
    (url-retrieve url
                  #'hoogle-callback-display-results
                  (list query url))))

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

(defun hoogle-callback-display-results (status &rest cbargs)
  "The callback function for `url-retrieve'.
STATUS is result of `url-retrieve'.
CBARGS are the callback args passed from `url-retrieve'."
  (when status
    ;; FIXME: error handling
    nil)
  (let ((query (make-instance 'hsearch-query)))
   (when cbargs
     (assert (and (listp cbargs) (eq (length cbargs) 2)))
     (oset query :query (car cbargs))
     (oset query :url (cadr cbargs)))
   (setq query (hoogle-parse-html query))
   (render query)))

(defun hoogle-parse-html (query)
  "Add results from the parsed HTML to the QUERY object.
QUERY is an `hsearch-query' class."
  (goto-char (point-min))
  (let (result
        str
        max-point
        current-point
        location)
    ;; Each entry is a div.ans
    (while (search-forward "class='ans'" nil 'noerror)
      (setq result (make-instance 'hsearch-result))

      ;; Get category from the first a.dull
      (search-forward "class='dull'")
      (re-search-forward hoogle-a-tag-regexp)
      ;; empty is function, data is data, class is class, module is
      ;; module
      (setq str (hsearch-strip-tags (match-string 1)))
      (setq str (hsearch-decode-html-entities (hsearch-chomp str)))
      (oset result :category (hsearch-result-category "" :category str))
      
      ;; Get name from a.a
      (search-forward "class='a'")
      (re-search-forward hoogle-a-tag-regexp)
      ;; Remove <b> used for substring matching (e.g. map in
      ;; mapAccumL) and convert html entities (e.g. &lt;*&gt; to <*>)
      (setq str (hsearch-strip-tags (match-string 1)))
      (setq str (hsearch-decode-html-entities str))
      (oset result :name (hsearch-result-name "" :name str))

      ;; Get type signature from second a.dull
      (search-forward "class='dull'")
      (re-search-forward hoogle-a-tag-regexp)
      ;; By stripping tags, we throw away some information.  Namely,
      ;; how hoogle matches signatures that use different type
      ;; variables.  We could add text-properties to the specific
      ;; classes so we don't have redo the logic.
      (setq str (hsearch-strip-tags (match-string 1)))
      (setq str (hsearch-decode-html-entities str))
      (setq str (hsearch-chomp str))
      (oset result :signature (hsearch-result-signature "" :signature str))
      
      ;; Get defined locations from div.from.
      (search-forward "class='from'")
      (setq max-point (hoogle-search-bound "</div>"))
      (loop do
            (search-forward "class='p1'" max-point)
            (re-search-forward hoogle-a-tag-regexp)
            (setq str (match-string 1))
            (setq location
                  (hsearch-result-location "" :module-base str))

            
            (if (search-forward "class='p2'" max-point 'noerror)
                (progn (re-search-forward hoogle-a-tag-regexp)
                       (oset location :module-name (match-string 1))))

            (add-location result location)
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
                 (setq str (hsearch-strip-tags (match-string 1)))
                 (setq str (hsearch-decode-html-entities str))
                 (oset result :doc (hsearch-result-doc "" :doc str)))
        ;; If there is no doc, we need to go back because the max
        ;; bound for doc was "class='ans'".  Since search-forward
        ;; moved us to the end of the match, we won't see it next
        ;; iteration unless we move back.
        (oset result :doc (hsearch-result-doc "" :doc ""))
        (goto-char current-point))

      (add-result query result))
    query))


;;; Hayoo support

(defun hayoo-search (query)
  "Search Hayoo for QUERY and display the results."
  nil)


;; Enable lexical binding.  Shouldn't affect Emacsen without lexbind
;; support.
;;
;; Local Variables:
;; lexical-binding: t
;; End:

(provide 'hsearch-mode)

;;; hsearch-mode.el ends here
