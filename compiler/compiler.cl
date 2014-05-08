;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiler.cl: a full beet-to-lisp compiler
;;
;; Copyright (c) 2014 Colin J. Fuller
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *tokens* '(keyword id eof comma num other string))

(defparameter *beet-keywords* '(def include package))

(defun token (tk v)
  (list tk v)
)

(defun token-is-a-p (token typesym)
  "See if the specified token has the specified type."
  (eql (car token) typesym)
)

;; parsing predicates

(defun whitespace-char-p (character)
  "See if the character is whitespace."
  (member character '(#\Tab #\Linefeed #\Return #\Page #\Space #\Newline)))

(defun comment-start-p (character)
  "See if the character starts a comment."
  (eql character #\#))

(defun initial-identifier-char-p (character)
  (and character
       (or (alpha-char-p character) (eql #\_ character))))

(defun identifier-char-p (character)
  (and character
       (or (initial-identifier-char-p character)
           (digit-char-p character)
           (member character '(#\$ #\@ #\! #\? #\. #\+ #\* #\= #\/ #\- #\& #\^
                               #\< #\> #\~ #\:)))))

(defun escape-char-p (character)
  (eql character #\\ ))

(defun quote-delimiter-p (character)
  (eql character #\"))

(defun comma-p (character)
  (eql character #\,))

(defun number-id-helper (id has-seen-decimal)
  (if (eql (length id) 0)
      t
    (let ((c (char id 0)))
      (cond ((digit-char-p c)
             (number-id-helper (subseq id 1) has-seen-decimal))
            ((eql c #\.)
             (if has-seen-decimal
                 nil
               (number-id-helper (subseq id 1) t)))
            (t
             nil)))))


(defun number-id-p (id)
  (if (eql (length id) 0)
      nil
    (number-id-helper id nil)))

(defun keyword-id-p (id)
  (member (intern (string-upcase id)) *beet-keywords*))

;; building and eating functions

(defun eof-p (stream)
  (not (listen stream)))

;; safe peek -- returns nil rather than error if at eof
(defun safe-peek-char (type stream)
  (if (eof-p stream)
      nil
      (peek-char type stream)))

(defun eat-char (stream)
  (read-char-no-hang stream))

(defun eat-comment (stream)
  (read-line stream))

(defun eat-whitespace (stream)
  (let ((c (safe-peek-char nil stream)))
    (when (and c (whitespace-char-p c))
      (eat-char stream)
      (eat-whitespace stream))))

(defun build-identifier (input-stream output-stream)
  (if output-stream
      (let ((c (safe-peek-char nil input-stream)))
        (if (identifier-char-p c)
            (progn
              (format output-stream "~C" (eat-char input-stream))
              (build-identifier input-stream output-stream))
          (get-output-stream-string output-stream)))

    (let ((s (make-string-output-stream)))
      (build-identifier input-stream s))))

(defun build-token (input-stream)
  (if (identifier-char-p (safe-peek-char nil input-stream))
      (let ((id (build-identifier input-stream nil)))
        (cond
         ((number-id-p id)
          (token 'num (read-from-string id)))
         ((keyword-id-p id)
          (token 'keyword id))
         (t
          (token 'id id))))
    (token 'other (eat-char input-stream))))

(defun build-quote-token (input-stream output-stream)
  (if output-stream
      (let ((c (safe-peek-char nil input-stream)))
        (if (escape-char-p c)
            (progn
              (eat-char input-stream) ;; throw away the escape
              ;; unconditionally write it to output
              (format output-stream "~C" (eat-char input-stream))
              (build-quote-token input-stream output-stream))
          (if (quote-delimiter-p c)
              ;; delimiter- we're done and should eat the delimiter,
              ;; returning string token
              (progn
                (eat-char input-stream)
                (token 'string (get-output-stream-string output-stream)))
            ;; not a delimiter- write the current token and recurse
            (progn
              (format output-stream "~C" (eat-char input-stream))
              (build-quote-token input-stream output-stream)))))
    ;; no output stream - first call from outside
    (let ((s (make-string-output-stream)))
      (eat-char input-stream) ;; first char == quote opener
      (build-quote-token input-stream s))))

(defun get-token (stream)
  "Get the next token from the stream."

  (when (eof-p stream)
    (return-from get-token (token 'eof nil)))

  (eat-whitespace stream)

  (when (eof-p stream)
    (return-from get-token (token 'eof nil)))

  (let ((c (safe-peek-char nil stream)))
    (cond
      ((comment-start-p c)
       (eat-comment c)
       (get-token stream)
       )
      ((comma-p c)
       (eat-char stream)
       (token 'comma #\,))
      ((quote-delimiter-p c)
       (build-quote-token stream nil))
      (c ;; make sure c is a character
       (build-token stream))
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun token-stream (char-stream head tail)
  (if tail
      (let ((next (get-token char-stream)))
        (setf (cdr tail) (list next))
        (if (token-is-a-p next 'eof)
            head
            (token-stream char-stream head (cdr tail))))
      (let ((next (list (get-token char-stream))))
        (token-stream char-stream next next))))

(defun next-token (stream)
  (let ((first (car stream))
        (rest (cdr stream)))
    (setf (car stream) (car rest))
    (setf (cdr stream) (cdr rest))
    first))

(defun peek-token (stream)
  (car stream))

(defun parse-comma-expr (first curr-token stream)
  ;; curr-token is a comma
  `(:type comma-grouped
          :first ,first
          :rest ,(parse (next-token stream) stream)))

(defun parse-identifier-expr (curr-token stream)
  (let ((id `(:type variable :name ,(cadr curr-token)))
        (next (peek-token stream)))
    (cond
      ; bare identifier -> function call
      ((or (token-is-a-p next 'id)
          (token-is-a-p next 'num)
          (token-is-a-p next 'string)
          (token-is-a-p next 'block))
       (let ((args (parse (next-token stream) stream)))
         ;; TODO: method . parsing
         `(:type functioncall :name ,id :args ,args)))

      ; parenthesized arguments -> function call
      ((eql (cadr next) #\( )
       (let ((args (parse-paren-expr (next-token stream) stream)))
         `(:type functioncall :name ,id :args ,args)))

      ; comma -> comma expression
      ((token-is-a-p next 'comma)
       (parse-comma-expr id (next-token stream) stream))

      ; just an regular old expression
      (t
       id))))

(defun parse-number-expr (curr-token stream)
  (let ((next (peek-token stream))
        (num `(:type number :value ,(cadr curr-token))))
    (if (token-is-a-p next 'comma)
        (parse-comma-expr num (next-token stream) stream)
        num)))

(defun parse-string-expr (curr-token stream)
  (let ((next (peek-token stream))
        (str `(:type string :value ,(cadr curr-token))))
    (if (token-is-a-p next 'comma)
        (parse-comma-expr str (next-token stream) stream)
        str)))

(defun parse-keyword-expr (curr-token stream)
  (case (intern (string-upcase (cadr curr-token)))
    ((def)
     (parse-kw-def curr-token stream))
    ((include)
     (parse-include curr-token stream))
    ((package)
     (parse-package curr-token stream))
    (otherwise
     '(:type :error :message "Unrecognized keyword."))))

(defun parse-kw-def (curr-token stream)
  (let ((name `(:type variable :name ,(cadr (next-token stream))))
        (args (parse-paren-expr (next-token stream) stream))
        (body (parse-block-expr (next-token stream) stream)))
    `(:type function-definition :name ,name :args ,args :body ,body)))

(defun parse-include (curr-token stream)
  (let ((path (parse (next-token stream) stream)))
    `(:type include :source ,path)))

(defun parse-package (curr-token stream)
  `(:type package-decl :pkg ,(parse (next-token stream) stream)))

(defun parse-other (curr-token stream)
  (cond
    ((eql (cadr curr-token) #\( )
     (parse-paren-expr curr-token stream))
    ((eql (cadr curr-token) #\{ )
     (parse-block-expr curr-token stream))
    ((eql (cadr curr-token) #\;)
     (parse (next-token stream) stream))))

(defun parse-paren-expr (curr-token stream)
  (if (eq (cadr (peek-token stream)) #\) )
      (progn
        (next-token stream) ; eat close paren
        nil)
      (let ((node (parse (next-token stream) stream))
            (next (next-token stream)))
        (if (eql (cadr next) #\) )
            (if (eql (peek-token stream) #\, )
                (parse-comma-expr node (next-token stream) stream)
                node)
            '(:type :error :message "Expected ) after current node.")))))

(defun parse-block-helper (bl curr-token stream)
  (let ((next (next-token stream)))
    (cond ((token-is-a-p next 'eof)
           '(:type :error :message "Expected } before EOF."))
      ((eql (cadr next) #\} )
           `(:type block :body ,(parse-all nil bl)))
      (t
       (parse-block-helper (append bl (list next)) next stream)))))

(defun parse-block-expr (curr-token stream)
  (parse-block-helper () curr-token stream))

(defun parse (curr-token stream)
    (cond
      ((token-is-a-p curr-token 'id)
       (parse-identifier-expr curr-token stream))
      ((token-is-a-p curr-token 'num)
       (parse-number-expr curr-token stream))
      ((token-is-a-p curr-token 'string)
       (parse-string-expr curr-token stream))
      ((token-is-a-p curr-token 'keyword)
       (parse-keyword-expr curr-token stream))
      ((token-is-a-p curr-token 'other)
       (parse-other curr-token stream))
      ((token-is-a-p curr-token 'eof)
       nil)
      (t nil)))

(defun parse-all (char-stream ts)
  (if ts
      (if (= (length ts) 1)
          (parse (next-token ts) ts)
          (append (list (parse (next-token ts) ts)) (parse-all char-stream ts)))
      (let ((ts (token-stream char-stream nil nil)))
        (parse-all char-stream ts))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *beet-mangle-prefix* "__beet_slaw_") ; if you mangle a beet, it becomes a delicious slaw!

(defparameter *beet-loadpath* '((:relative ".") (:relative "lib")))
;;TODO: env variable or something here

(defun name-mangle (name)
  (concatenate 'string *beet-mangle-prefix* name))

(defun maybe-mangle (name)
  (if (eql (char name 0) #\~)
      ;; don't mangle if the name starts with ~; instead, strip the ~
      (subseq name 1)
      ;; otherwise, mangle
      (name-mangle name)))

(defun packageify-name (name pkg)
  (when pkg
      (if (eql (char (getf name :name) 0) #\~)
          (setf (getf name :name) (concatenate 'string "~" pkg ":" (subseq (getf name :name) 1)))
          (setf (getf name :name) (concatenate 'string pkg ":" (getf name :name)))))
  name)

(defun node-type-is (node type)
  (eq (getf node :type) type))

(defun compile-comma-expr (node)
  (if (node-type-is (getf node :rest) 'comma-grouped)
      (cons (compile-node (getf node :first)) (compile-node (getf node :rest)))
      (cons (compile-node (getf node :first))
            (cons (compile-node (getf node :rest)) nil))))

(defun compile-number-expr (node)
  (getf node :value))

(defun compile-string-expr (node)
  (getf node :value))

(defun compile-variable-expr (node)
  (intern (maybe-mangle (getf node :name))))

(defun block-assignment-helper (nodes)
  (unless nodes
    (return-from block-assignment-helper nil))
  (let ((node (car nodes)))
    (if (and (eq (getf node :type) 'functioncall)
             (equal (getf (getf node :name) :name) "="))
        ;; It's an assignment.  We need to set up a let and then nest the
        ;; remainder inside.  But first parse out the argument names.
        (let ((lhs (compile-variable-expr (getf (getf node :args) :first))) ;;TODO: error if it's not a name
              (rhs (getf (getf node :args) :rest)))
          `((let ((,lhs ,(compile-node rhs)))
             ,@(block-assignment-helper (cdr nodes)))))
        ;; Not an assignment; concatenate this to the result of compiling the remainder
        (cons (compile-node node) (block-assignment-helper (cdr nodes))))))

(defun compile-block-expr (node)
  (block-assignment-helper (getf node :body)))


(defun parse-include-def (curr-token stream)
  (let ((package (parse (next-toen stream) stream)))
    `(:type inline-code :from :include :source ,package)))


(defun compile-code-inline (node))

(defun func-def-helper (name args body)
  (if args
      `(defun ,name (,@args) ,@body)
      `(defun ,name () ,@body)))

(defun compile-function-definition (node &optional pkg)
  (if (node-type-is (getf node :args) 'comma-grouped)
      (func-def-helper (compile-variable-expr (packageify-name (getf node :name) pkg))
                       (compile-comma-expr (getf node :args))
                       (compile-block-expr (getf node :body)))
      (if (getf node :args)
          (func-def-helper (compile-variable-expr (packageify-name (getf node :name) pkg))
                           (cons (compile-node (getf node :args)) nil)
                           (compile-block-expr (getf node :body)))
          (func-def-helper (compile-variable-expr (packageify-name (getf node :name) pkg))
                           nil
                           (compile-block-expr (getf node :body))))))

(defun compile-function-call (node)
  (if (node-type-is (getf node :args) 'comma-grouped)
      `(,(compile-node (getf node :name)) ,@(compile-node (getf node :args)))
      (if (getf node :args)
          `(,(compile-node (getf node :name)) ,(compile-node (getf node :args)))
          `(,(compile-node (getf node :name))))))

(defun compile-node (node &optional pkg)
  (cond
    ((node-type-is node 'comma-grouped)
     (compile-comma-expr node))
    ((node-type-is node 'block)
     (compile-block-expr node))
    ((node-type-is node 'number)
     (compile-number-expr node))
    ((node-type-is node 'string)
     (compile-string-expr node))
    ((node-type-is node 'variable)
     (compile-variable-expr node))
    ((node-type-is node 'function-definition)
     (compile-function-definition node pkg))
    ((node-type-is node 'functioncall)
     (compile-function-call node))
    ((node-type-is node 'error)
     (princ (getf node :message))
     nil)))

(defun get-package-name (node)
  (getf (getf node :pkg) :name))

(defun include-source (node)
  (getf (getf node :source) :value))

(defun find-include (source)
  (dolist (d *beet-loadpath*)
    ;;TODO: subdirectory search
    (let ((pn (make-pathname :directory d :name source)))
      (when (probe-file pn)
        (return-from find-include pn))))
  nil)

(defun compile-remaining (remaining-nodes &optional pkg)
  (if remaining-nodes
      (cond
        ((node-type-is (car remaining-nodes) 'package-decl)
         (compile-remaining (cdr remaining-nodes) (get-package-name (car remaining-nodes))))
        ((node-type-is (car remaining-nodes) 'include)
         (append (compile-include (include-source (car remaining-nodes)))
                 (compile-remaining (cdr remaining-nodes) pkg)))
        (t
         (cons (compile-node (car remaining-nodes) pkg)
               (compile-remaining (cdr remaining-nodes) pkg))))
      nil))

(defun compile-include (source)
  (let ((sourcefile (find-include source)))
    (if sourcefile
        (with-open-file (include-file sourcefile)
          (let ((nodes (parse-all include-file nil)))
            (compile-remaining nodes)))
          nil))) ;; TODO: actually signal an error here if it can't find the include

(defun compile-all (input-stream output-stream)
    (let ((nodes (parse-all input-stream nil)))
      (dolist (f (compile-remaining nodes))
        (print f output-stream))))

(defun compile-file (filename output-stream)
  (with-open-file (f filename)
    (compile-all f output-stream)))

(defun compile-stdin (output-stream)
  (compile-all *standard-input* output-stream))

(defun compile-stdin-to-stdout ()
  (compile-all *standard-input* *standard-output*))

(when *args*
  (compile-stdin-to-stdout))
