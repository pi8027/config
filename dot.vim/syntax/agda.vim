" File: ~/.vim/syntax/agda.vim

if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

" Basics
syn match agdaModule    "\<module\>"
syn match agdaStructure "\<\(data\|where\|record\|with\|forall\|open import\|open\|hiding\|private\|renaming\|to\|postulate\|abstract\)\>"
syn match agdaFunction  "\<Set\>"

" Comments
syn match   agdaLineComment      "---*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$"
syn region  agdaBlockComment     start="{-"  end="-}" contains=hsBlockComment
syn region  agdaPragma         start="{-#" end="#-}"
syn match   agdaInfix           "\<\(infix\|infixl\|infixr\)\>"

hi def link agdaModule          agdaStructure
hi def link agdaStructure       Structure
hi def link agdaConstructor     Constant

hi def link agdaFunction        Function
hi def link agdaInfixFunction   Operator

hi def link agdaLineComment     agdaComment
hi def link agdaBlockComment    agdaComment
hi def link agdaPragma          agdaComment
hi def link agdaComment         Comment
hi def link agdaInfix           PreProc

