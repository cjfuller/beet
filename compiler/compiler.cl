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

(defparameter *tokens* '(keyword id eof comma num other string sig-space))

(defparameter *beet-keywords* '(def include package macro))

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
  (member character '(#\Tab #\Page #\Space)))

(defun newline-p (character)
  (member character '(#\Newline #\Linefeed #\Return)))

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
    (if (and c (whitespace-char-p c))
      (progn
        (eat-char stream)
        (eat-whitespace stream))
      (when (and c (newline-p c))
        (eat-char stream)
        (token 'sig-space (build-significant-whitespace-token stream))))))

(defun build-significant-whitespace-token (stream)
  (if (whitespace-char-p (safe-peek-char nil stream))
      (concatenate 'string (list (eat-char stream)) (build-significant-whitespace-token stream))
      nil))

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

  (let ((ws-token (eat-whitespace stream)))
    (when ws-token
      (return-from get-token ws-token)))

  (when (eof-p stream)
    (return-from get-token (token 'eof nil)))

  (let ((c (safe-peek-char nil stream)))
    (cond
      ((comment-start-p c)
       (eat-comment stream)
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
  (if (or (eql (cadr (peek-token stream)) #\} )
           (eql (cadr (peek-token stream)) #\) ))
       `(:type comma-grouped
               :first ,first
               :rest nil)
       `(:type comma-grouped
               :first ,first
               :rest ,(parse (next-token stream) stream))))

(defun parse-identifier-expr (curr-token stream)
  (let ((id `(:type variable :name ,(cadr curr-token)))
        (next (peek-token stream)))
    (cond
      ; bare identifier or parens -> function call
      ((or (token-is-a-p next 'id)
           (token-is-a-p next 'num)
           (token-is-a-p next 'string)
           (eql (cadr next) #\{ )
           (eql (cadr next) #\( ))
       (let ((args (if (eql (cadr next) #\{ )
                       nil
                       (parse (next-token stream) stream t)))
             (assoc-block (if (eql (cadr (peek-token stream)) #\{ )
                              (parse (next-token stream) stream)
                              nil)))
         ;; TODO: method . parsing
         (let ((form `(:type functioncall :name ,id :args ,args :bl ,assoc-block)))
           (if (token-is-a-p (peek-token stream) 'comma)
               (parse-comma-expr form (next-token stream) stream)
               form))))

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
    ((macro)
     (parse-macro curr-token stream))
    (otherwise
     '(:type error :message "Unrecognized keyword."))))

(defun parse-kw-def (curr-token stream)
  (let ((name `(:type variable :name ,(cadr (next-token stream))))
        (args (parse (next-token stream) stream))
        (body (parse (next-token stream) stream)))
    `(:type function-definition :name ,name :args ,args :body ,body)))

(defun parse-macro (curr-token stream)
  ;;TODO
)

(defun parse-include (curr-token stream)
  (let ((path (parse (next-token stream) stream)))
    `(:type include :source ,path)))

(defun parse-package (curr-token stream)
  `(:type package-decl :pkg ,(parse (next-token stream) stream)))

(defun parse-other (curr-token stream &optional bound)
  (cond
    ((eql (cadr curr-token) #\( )
     (parse-paren-expr curr-token stream bound))
    ((eql (cadr curr-token) #\{ )
     (parse-block-expr curr-token stream))
    ((eql (cadr curr-token) #\;)
     (parse (next-token stream) stream))))


(defun parse-paren-expr (curr-token stream &optional bound)
  (if (eq (cadr (peek-token stream)) #\) )
      (progn
        (next-token stream) ; eat close paren
        nil)
      (let ((node `(:type group :contents ,(parse (next-token stream) stream)))
            (next (next-token stream)))
        (if (eql (cadr next) #\) )
            (if (and (not bound) (eql (cadr (peek-token stream)) #\, ))
                (parse-comma-expr node (next-token stream) stream)
                node)
            `(:type error :message ,(concatenate 'string "Expected ) after current node.  "
                                    "Got " (prin1-to-string next)
                                    "  Current node: " (prin1-to-string node)))))))

(defun parse-block-helper (bl n curr-token stream)
  (let ((next (next-token stream)))
    (cond ((token-is-a-p next 'eof)
           '(:type error :message "Expected } before EOF."))
      ((eql (cadr next) #\} )
       (if (= n 0)
           `(:type block :body ,(parse-all nil bl t))
           (parse-block-helper (append bl (list next)) (1- n) next stream)))
      ((eql (cadr next) #\{ )
       (parse-block-helper (append bl (list next)) (1+ n) next stream))
      (t
       (parse-block-helper (append bl (list next)) n next stream)))))

(defun parse-block-expr (curr-token stream)
  (parse-block-helper () 0 curr-token stream))

(defun parse (curr-token stream &optional bound)
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
       (parse-other curr-token stream bound))
      ((token-is-a-p curr-token 'eof)
       nil)
      ((token-is-a-p curr-token 'sig-space)
       (parse (next-token stream) stream))
      (t nil)))

(defun parse-all (raw-char-stream raw-ts &optional skip-preprocess)
  (let ((char-stream (if skip-preprocess
                         raw-char-stream
                         (preprocess-character-stream raw-char-stream)))
        (ts (if skip-preprocess
                raw-ts
                (preprocess-token-stream raw-ts))))
    (if ts
        (if (or (= (length ts) 0)
                (not (peek-token ts)))
            nil
            (append (list (parse (next-token ts) ts)) (parse-all char-stream ts t)))
        (let ((ts (token-stream char-stream nil nil)))
          (parse-all char-stream ts)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preprocessor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The preprocessor is a pluggable system for processing beet code files.  It
;; operates in three phases:
;; 1.  Filtering of the character stream for the file directly.
;; 2.  Filtering of the token stream.
;; (3.  Macro expansion.) -- Under development/consideration.  I'm envisioning
;; this as somewhat different from lisp macros, and I'll need to think about
;; this more.  My current vision is as the inverse of the current block model
;; -- essentially code without a binding, where you declare any variables that
;; you *do* want to be bound to the macro scope.  I want to add flexibility in
;; argument parsing, which is a little trickier.
;;
;; A function that operates as part of the preprocessor should do one of three things:
;; 1.  Take a character stream and return a processed character stream.
;; 2.  Take a token stream and return a processed token stream.
;; 3.  Take a token and return a processed token.
;;
;; Plugins that do one of these three things can be activated using (register-pp-charstream fct),
;; (register-pp-tokenstream fct), or (register-pp-token fct).

(defparameter *beet-pp-charstream-plugins* nil)
(defparameter *beet-pp-tokenstream-plugins* nil)
(defparameter *beet-pp-token-plugins* nil)

(defun register-pp-charstream (fct)
  (setf *beet-pp-charstream-plugins*
        (append *beet-pp-charstream-plugins* (cons fct nil))))

(defun register-pp-tokenstream (fct)
  (setf *beet-pp-tokenstream-plugins*
        (append *beet-pp-tokenstream-plugins* (cons fct nil))))

(defun register-pp-token (fct)
  (setf *beet-pp-token-plugins*
        (append *beet-pp-token-plugins* (cons fct nil))))

(defun apply-token-plugin (ts tpl)
  (let ((output nil))
    (dolist (tk ts)
      (setf output (append output (list (funcall tpl tk)))))
    output))

(defun token-tokenstream-plugin (ts)
  (dolist (tpl *beet-pp-token-plugins*)
    (setf ts (apply-token-plugin ts tpl)))
  ts)

(register-pp-tokenstream (function token-tokenstream-plugin))

(defun preprocess-token-stream (ts)
  (dolist (pl *beet-pp-tokenstream-plugins*)
    (setf ts (funcall pl ts)))
  ts)

(defun preprocess-character-stream (cs &optional cs-out)
  (if (eof-p cs)
      cs ;; should be cs-out
      cs))

;; built-in preprocessor plugins

; aliasing plugin
(defun alias-plugin (ts &optional alias-table output-stream)
;; Syntax: alias <existing-name> as <new-name>
;; Defines the pseudo-function "alias", which causes the subsequent id token to
;; be added to an alias table with the value of the subsequent .  Any id tokens that are in the alias table will
;; be substituted for an id token with the aliased name.
  (if alias-table
      (if (and ts
               (car ts))
          (let ((tk (next-token ts)))
            (if (and (token-is-a-p tk 'id)
                     (equal (cadr tk) "alias"))
                (let* ((old-name (next-token ts))
                       (as (next-token ts)) ;; TODO: error if this isn't "as"
                       (new-name  (intern (cadr (next-token ts))))) ;; TODO: error if the names aren't type "id"
                  (setf (gethash new-name alias-table) old-name)
                  (alias-plugin ts alias-table output-stream))
                (if (and (token-is-a-p tk 'id)
                         (gethash (intern (cadr tk)) alias-table))
                    (alias-plugin ts alias-table
                                  (append output-stream
                                          (list (gethash (intern (cadr tk))
                                                         alias-table))))
                    (alias-plugin ts alias-table (append output-stream (list tk))))))
          output-stream)
      (alias-plugin ts (make-hash-table) output-stream)))

(register-pp-tokenstream (function alias-plugin))

; significant whitespace plugin
(defun whitespace-plugin (ts &optional (unmatched-paren-count 0) (last-indent-level (list 0)) output-stream)
  (if (and ts
           (car ts))
      (let ((tk (next-token ts)))
        (if (token-is-a-p tk 'sig-space)
            (progn
              (if (and (> (length (cadr tk))
                          (car last-indent-level))
                       (= unmatched-paren-count 0))
                  (whitespace-plugin ts unmatched-paren-count (cons (length (cadr tk)) last-indent-level)
                                     (append output-stream
                                             (list (token 'other #\{ ))))
                  (if (and (< (length (cadr tk))
                              (car last-indent-level))
                           (= unmatched-paren-count 0))
                      ;; If we're decreasing indent level, push this token back
                      ;; onto the stream, pop the indent pevel, and insert a }.
                      ;; This allows us to handle multiple decreasing levels at
                      ;; once at the cost of extraneous semicolons.
                      (whitespace-plugin (cons tk ts) unmatched-paren-count (cdr last-indent-level)
                                         (append output-stream
                                                 (list (token 'other #\} ))))
                      (if (= unmatched-paren-count 0)
                          (whitespace-plugin ts unmatched-paren-count
                                             last-indent-level
                                             (append output-stream
                                                     (list (token 'other #\;))))
                          (whitespace-plugin ts unmatched-paren-count
                                             last-indent-level
                                             output-stream)))))
            (if (and
                 (token-is-a-p tk 'other)
                 (eql (cadr tk) #\( ))
                (whitespace-plugin ts (1+ unmatched-paren-count)
                                   last-indent-level
                                   (append output-stream (list tk)))
                (if (and
                     (token-is-a-p tk 'other)
                     (eql (cadr tk) #\) ))
                    (whitespace-plugin ts (1- unmatched-paren-count)
                                       last-indent-level
                                       (append output-stream (list tk)))
                    (whitespace-plugin ts unmatched-paren-count
                                       last-indent-level
                                       (append output-stream (list tk)))))))
      output-stream))

(register-pp-tokenstream (function whitespace-plugin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(defparameter *beet-mangle-prefix* "__beet_slaw_") ; if you mangle a beet, it becomes a delicious slaw!

(defparameter *beet-mangle-prefix* "__")

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
  (let ((mangled (maybe-mangle (getf node :name))))
    (if (eql (char mangled 0) #\:)
        (read-from-string mangled)
        (intern mangled))))

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
  (let ((body (block-assignment-helper (getf node :body))))
    (if (and (listp (car body))
             (equal (car (car body)) (intern (name-mangle "->"))))
        (let ((unbound (cddr (car body))))
          `(lambda ,unbound ,@(cdr body)))
        `(lambda nil ,@body))))

(defun parse-include-def (curr-token stream)
  (let ((package (parse (next-token stream) stream)))
    `(:type inline-code :from :include :source ,package)))


(defun compile-code-inline (node))

(defun compile-group-expr (node)
  (compile-node (getf node :contents)))

(defun func-def-helper (name args body)
  `(defun ,name (,(intern (name-mangle "_bl")) ,@(func-args-helper args)) (funcall ,body)))

(defun is-glob-arg (sym)
  (eql (char (symbol-name sym) (length *beet-mangle-prefix*)) #\*))

(defun deglob-arg (sym)
  (let ((strname (symbol-name sym)))
    (intern (concatenate 'string
                         (subseq strname 0 (length *beet-mangle-prefix*))
                         (subseq strname (1+ (length *beet-mangle-prefix*)))))))

(defun func-args-helper (args)
  (if args
      (if (is-glob-arg (car args))
          (cons '&rest (cons (deglob-arg (car args)) (func-args-helper (cdr args))))
          (cons (car args) (func-args-helper (cdr args))))
      nil))

(defun compile-function-definition (node &optional pkg)
  (let ((args (compile-node (getf node :args))))
    (if (or (node-type-is (getf node :args) 'comma-grouped)
            (and (node-type-is (getf node :args) 'group)
                 (node-type-is (getf (getf node :args) :contents) 'comma-grouped)))
        (func-def-helper (compile-variable-expr (packageify-name (getf node :name) pkg))
                         args
                         (compile-block-expr (getf node :body)))
        (if (getf node :args)
            (func-def-helper (compile-variable-expr (packageify-name (getf node :name) pkg))
                             (cons args nil)
                             (compile-block-expr (getf node :body)))
            (func-def-helper (compile-variable-expr (packageify-name (getf node :name) pkg))
                             nil
                             (compile-block-expr (getf node :body)))))))

(defun is-beety-name (name)
  (not (eql (char name 0) #\~)))

(defun compile-beety-function-call (node)
    (if (or (node-type-is (getf node :args) 'comma-grouped)
            (and (node-type-is (getf node :args) 'group)
                 (node-type-is (getf (getf node :args) :contents) 'comma-grouped)))
        `(,(compile-node (getf node :name))
           ,(compile-node (getf node :bl))
           ,@(compile-node (getf node :args)))
        (if (getf node :args)
            `(,(compile-node (getf node :name))
               ,(compile-node (getf node :bl))
               ,(compile-node (getf node :args)))
            `(,(compile-node (getf node :name))
               ,(compile-node (getf node :bl))))))

(defun compile-native-function-call (node)
;; don't give it a block if it doesn't expect one!!
  (if (or (node-type-is (getf node :args) 'comma-grouped)
          (and (node-type-is (getf node :args) 'group)
               (node-type-is (getf (getf node :args) :contents) 'comma-grouped)))
      `(,(compile-node (getf node :name))
         ,@(compile-node (getf node :args)))
      (if (getf node :args)
          `(,(compile-node (getf node :name))
             ,(compile-node (getf node :args)))
          `(,(compile-node (getf node :name))))))

(defun compile-function-call (node)
  (if (is-beety-name (getf (getf node :name) :name))
      (compile-beety-function-call node)
      (compile-native-function-call node)))



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
    ((node-type-is node 'group)
     (compile-group-expr node))
    ((node-type-is node 'error)
     (princ (getf node :message))
     nil)))

(defun enclose-in-toplevel-block (nodes)
  `((:type block :body ,nodes)))

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

(defun compile-helper (input-stream)
  (let ((nodes (parse-all input-stream nil)))
    (compile-remaining nodes)))

(defun compile-include (source)
  (let ((sourcefile (find-include source)))
    (if sourcefile
        (with-open-file (include-file sourcefile)
          (compile-helper include-file))
          nil))) ;; TODO: actually signal an error here if it can't find the include

(defun compile-all (input-stream output-stream &optional skip-prepend)
  ;; prepend the kernel
  (unless skip-prepend
    (with-open-file (kernel "kernel.beet")
      (compile-all kernel output-stream t)))
  (dolist (f (compile-helper input-stream))
    (print f output-stream)))

(defun compile-file (filename output-stream)
  (with-open-file (f filename)
    (compile-all f output-stream)))

(defun compile-stdin (output-stream)
  (compile-all *standard-input* output-stream))

(defun compile-stdin-to-stdout ()
  (compile-all *standard-input* *standard-output*))

(when *args*
  (compile-stdin-to-stdout))
