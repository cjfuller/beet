(load "parser.cl")

(defparameter *beet-mangle-prefix* "__beet_slaw_") ; if you mangle a beet, it becomes a delicious slaw!

(defun name-mangle (name)
  (concatenate 'string *beet-mangle-prefix* name))

(defun maybe-mangle (name)
  (if (eql (char name 0) #\~)
      ;; don't mangle if the name starts with ~; instead, strip the ~
      (subseq name 1)
      ;; otherwise, mangle
      (name-mangle name)))

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

(defun compile-block-expr (node)
  (map 'list #'compile-node (getf node :body)))

(defun func-def-helper (name args body)
  `(defun ,name (,@args) ,@body))

(defun compile-function-definition (node)
  (if (node-type-is (getf node :args) 'comma-grouped)
      (func-def-helper (compile-variable-expr (getf node :name))
                       (compile-comma-expr (getf node :args))
                       (compile-block-expr (getf node :body)))
      (func-def-helper (compile-variable-expr (getf node :name))
                       (cons (compile-node (getf node :args)) nil)
                       (compile-block-expr (getf node :body)))))

(defun compile-function-call (node)
  (if (node-type-is (getf node :args) 'comma-grouped)
      `(,(compile-node (getf node :name)) ,@(compile-node (getf node :args)))
      (if (getf node :args)
          `(,(compile-node (getf node :name)) ,(compile-node (getf node :args)))
          `(,(compile-node (getf node :name))))))

(defun compile-node (node)
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
     (compile-function-definition node))
    ((node-type-is node 'functioncall)
     (compile-function-call node))
    ((node-type-is node 'error)
     (princ (getf node :message))
     nil)))

(defun compile-remaining (remaining-nodes)
  (if remaining-nodes
      (cons (compile-node (car remaining-nodes)) (compile-remaining (cdr remaining-nodes)))
      nil))


(defun compile-all (input-stream output-stream)
    (let ((nodes (parse-all input-stream nil)))
      (dolist (f (compile-remaining nodes))
        (print f output-stream))))

(compile-all *standard-input* *standard-output*)
