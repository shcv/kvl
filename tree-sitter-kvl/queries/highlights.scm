; KVL highlight queries

; Comments (/= ...)
(comment) @comment

; Headers (#= kvl 1.0 ...)
(header) @keyword.directive

; Keys
(pair
  key: (key) @variable)

; Separators
(pair
  separator: (separator) @punctuation.delimiter)

; Boolean values
(pair
  value: (value) @constant.builtin
  (#match? @constant.builtin "^(true|false)$"))

; Number values (integers and floats)
(pair
  value: (value) @number
  (#match? @number "^-?[0-9]+(\\.[0-9]+)?$"))

; String values (fallback - must come after more specific patterns)
(pair
  value: (value) @string
  (#not-match? @string "^(true|false)$")
  (#not-match? @string "^-?[0-9]+(\\.[0-9]+)?$"))

; List markers (-, +, *)
(list_item
  marker: (list_marker) @punctuation.special)

; List item values
(list_item
  value: (value) @string)

; Continuation lines (multiline values)
(continuation) @string
