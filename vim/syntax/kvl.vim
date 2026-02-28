if exists('b:current_syntax')
  finish
endif

syn case match

" Header line (first line only): #= kvl 1.0 ...
syn match kvlHeader /\%^#.\+$/ display

" Comments: /= ...
syn match kvlComment /^\s*\/=.*$/ display contains=@Spell

" List markers: - + * at start of line (possibly indented), followed by space
syn match kvlListMarker /^\s*\zs[-+*]\ze[ \t]/ display

" Escape sequences: backslash before separator characters (= : >)
syn match kvlEscape /\\[=:>]/ display

" Boolean values at end of line, after a separator
syn match kvlBoolean /\(=\|:\|->\|:=\)\s*\zs\(true\|false\)\ze\s*$/ display

" Number values at end of line, after a separator
syn match kvlNumber /\(=\|:\|->\|:=\)\s*\zs-\?\d\+\(\.\d\+\)\?\ze\s*$/ display

" Separators: match the operator between key and value
" Order: longer matches first
syn match kvlSeparator /\s\+:=\s*/ display
syn match kvlSeparator /\s\+->\s*/ display
syn match kvlSeparator /\s\+=\s*/ display
" Colon separator (avoid matching inside values like URLs)
syn match kvlSeparator /^\s*[^/ \t]\S*\s*\zs:\ze\s/ display

" Empty-key = at start of line for categorical structure
syn match kvlSeparator /^\s*\zs=\ze\s/ display

" Keys: text before the separator
" Default separator (=)
syn match kvlKey /^\s*\zs[^/ \t#].\{-}\ze\s\+=/ display contains=kvlEscape
" Colon separator
syn match kvlKey /^\s*\zs[^/ \t#]\S*\ze\s*:/ display contains=kvlEscape
" Arrow separator
syn match kvlKey /^\s*\zs[^/ \t#].\{-}\ze\s\+->/ display contains=kvlEscape
" Assign separator
syn match kvlKey /^\s*\zs[^/ \t#].\{-}\ze\s\+:=/ display contains=kvlEscape

let b:current_syntax = 'kvl'

hi def link kvlHeader PreProc
hi def link kvlComment Comment
hi def link kvlKey Identifier
hi def link kvlSeparator Operator
hi def link kvlBoolean Boolean
hi def link kvlNumber Number
hi def link kvlListMarker Special
hi def link kvlEscape SpecialChar
