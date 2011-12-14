;;; hsearch-mode.el --- Elisp functions to search Haskell

;; Author:  Joe Schafer <joe@jschaf.com>
;; Maintainer:  Joe Schafer <joe@jschaf.com>
;; Created: 10 Dec 2011
;; Version:  0.1
;; Keywords:  languages, haskell, hoogle, hayoo

;;; Commentary:
;; 

(require 'eieio)

;;; Code:
(setq lexical-binding t)

(defclass hsearch-results ()
  ((query :initarg :query)
   (url :initarg :url)
   (answers :initarg :answers
            :initform '()))
  "A class for the overall results.")

(defmethod add-answer ((obj hsearch-results) answer)
  "Add an answer (i.e. a match) to HSEARCH-RESULTS."
  (object-add-to-list obj :answers answer 'append))

(defclass hsearch-answer ()
  ((category :initarg :category)
   (name :initarg :name)
   (signature :initarg :signature)
   (locations :initarg :locations
              :initform '())
   (doc :initarg :doc
        :initform ""))
  "A class for an individual answer.")

(defmethod add-location ((obj hsearch-answer) location)
  "Add a location to HSEARCH-ANSWER.locations."
  (object-add-to-list obj :locations location 'append))

;; Enable lexical binding.  Shouldn't affect Emacsen without lexbind
;; support.
;;
;; Local Variables:
;; lexical-binding: t
;; End:

(provide 'hsearch-mode)

;;; hsearch.el ends here
