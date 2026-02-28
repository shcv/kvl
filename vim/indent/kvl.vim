if exists('b:did_indent')
  finish
endif
let b:did_indent = 1

setlocal indentexpr=KvlIndent()
setlocal indentkeys=o,O,<CR>
setlocal autoindent

let b:undo_indent = 'setlocal indentexpr< indentkeys< autoindent<'

if exists('*KvlIndent')
  finish
endif

function! KvlIndent() abort
  let lnum = prevnonblank(v:lnum - 1)

  " First line or no previous non-blank line
  if lnum == 0
    return 0
  endif

  let prev_line = getline(lnum)
  let prev_indent = indent(lnum)

  " Previous line ends with separator and optional whitespace (empty value = nested block)
  " Match: key = <EOL>  or  key: <EOL>  or  key -> <EOL>  or  key := <EOL>
  if prev_line =~# '\(=\|:\|:=\|->\)\s*$'
    return prev_indent + shiftwidth()
  endif

  return prev_indent
endfunction
