
"-------------------------------------------------------------------------------
" Edit
"
set ambiwidth=double
set backspace=2
set autoindent
set nocompatible
set history=100

set fileencodings=utf-8,euc-jp,iso-2022-jp,shift_jis

"-------------------------------------------------------------------------------
" Search
"
set ignorecase
set smartcase
set wrapscan
set incsearch

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
set softtabstop=4
set noexpandtab

set nonumber
set showcmd
set showtabline=2
set laststatus=2

colorscheme evening

let &statusline = ' '
let &statusline .= '%<%f %h%m%r%w'
let &statusline .= '[%{&l:fileencoding == "" ? &encoding : &l:fileencoding}]'
let &statusline .= '%='
let &statusline .= '  %-14.(%l,%c%V%) %P '

"-------------------------------------------------------------------------------
" Autocomplete
"
cnoreabbrev ~/ <Bslash>
cnoremap <Bslash> ~/

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
" Swap and Backup Files
"
set swapfile
set directory=~/.vim/tmp
set nobackup

"-------------------------------------------------------------------------------
" Settings for each file types
"

autocmd FileType haskell setlocal tabstop=8 shiftwidth=4 softtabstop=4 expandtab

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

