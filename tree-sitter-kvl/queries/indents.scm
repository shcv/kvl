; KVL indent queries

; Indent after a pair with no value (object parent)
(pair
  separator: (separator)
  !value) @indent.begin

; Dedent on blank lines or when returning to lower indent level
(continuation) @indent.auto
