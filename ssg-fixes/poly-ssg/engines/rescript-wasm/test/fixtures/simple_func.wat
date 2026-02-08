;; Expected output for: let max = (a, b) => if a > b then a else b
;;
;; This demonstrates conditional compilation with pattern matching on
;; integer comparison results.

(module
  ;; Standard types
  (type $string (array (mut i32)))
  (type $unit (struct))

  ;; Function: max(a, b) => if a > b then a else b
  (func $max (export "max")
    (param (ref i31))
    (param (ref i31))
    (result (ref i31))
    ;; Compare a > b, result is 1 (true) or 0 (false)
    (if (result (ref i31))
      (i32.gt_s
        (i31.get_s (local.get 0))
        (i31.get_s (local.get 1)))
      (then
        (local.get 0))
      (else
        (local.get 1))))
)
