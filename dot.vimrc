
"-------------------------------------------------------------------------------
" Edit
"
set ambiwidth=double
set backspace=2
set autoindent

"-------------------------------------------------------------------------------
" Search
"
set ignorecase
set smartcase
set wrapscan

"-------------------------------------------------------------------------------
" Highlight
"
if has("syntax")
	syntax on
endif
set listchars=tab:\ \ 
set list
set showmatch
set hlsearch

"-------------------------------------------------------------------------------
" Looks
"
set tabstop=4
set shiftwidth=4

set nonumber
set showcmd
set showtabline=2
set laststatus=2

let &statusline = ''
let &statusline .= '%<%f %h%m%r%w'
let &statusline .= '[%{&l:fileencoding == "" ? &encoding : &l:fileencoding}]'
let &statusline .= '%='
let &statusline .= '  %-14.(%l,%c%V%) %P'

"-------------------------------------------------------------------------------
" Bracket
"
inoremap { {}<LEFT>
inoremap [ []<LEFT>
inoremap ( ()<LEFT>
inoremap " ""<LEFT>
inoremap ' ''<LEFT>
vnoremap { "zdi^V{<C-R>z}<ESC>
vnoremap [ "zdi^V[<C-R>z]<ESC>
vnoremap ( "zdi^V(<C-R>z)<ESC>
vnoremap " "zdi^V"<C-R>z^V"<ESC>
vnoremap ' "zdi'<C-R>z'<ESC>

"-------------------------------------------------------------------------------
" Date and Time
"
if exists("*strftime")
	function! W3CDTF()
		let dt = strftime("%Y-%m-%dT%T%z")
		return printf("%s:%s",dt[0:21],dt[22:23])
	endfunction
	imap <C-D><C-W> <C-R>=W3CDTF()<CR>
endif

