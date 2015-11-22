(defvar *state* nil "List of conditions for the current state")

(defvar *ops* nil "List of available actions")

(defstruct op "An operation/action"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun gps (*state* goals *ops*)
  "General Problem Solver"
  (if (every #'achieve goals) 'solved))

(defun achieve (goal)
  (or (member goal *state*)
      (some #'apply-op
          (find-all goal *ops* :test #'appropriate-p))))

(defun appropriate-p (goal op)
  (member goal (op-add-list op)))

(defun apply-op (op)
  (when (every #'achieve (op-preconds op))
    (print (list 'executing (op-action op)))
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))
    t))

(defun find-all (item sequence &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence
          :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
          :test (complement test) keyword-args)))
