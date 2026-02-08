;; Expected output for: let add = (a, b) => a + b
;;
;; This is the target WAT output for the simple integer addition function.
;; The compiler should produce something similar to this.

(module
  ;; Standard types
  (type $string (array (mut i32)))
  (type $unit (struct))

  ;; Function: add(a, b) => a + b
  ;; Parameters are i31ref (boxed integers)
  ;; Returns i31ref
  (func $add (export "add")
    (param (ref i31))
    (param (ref i31))
    (result (ref i31))
    ;; Unwrap both i31refs to i32, add, wrap result back to i31ref
    (ref.i31
      (i32.add
        (i31.get_s (local.get 0))
        (i31.get_s (local.get 1)))))
)
