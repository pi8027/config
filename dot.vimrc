
"""" Edit

set ambiwidth=double
set backspace=1
set autoindent
set nocompatible
set history=100

set fileencodings=utf-8,euc-jp,iso-2022-jp,shift_jis

"""" Search

set ignorecase
set smartcase
set wrapscan
set incsearch

"""" Highlight

syntax on
set listchars=tab:\ \ 
set list
set showmatch
set hlsearch

"""" Looks

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

set number
set showcmd
set showtabline=2
set laststatus=2

set ambiwidth=double

set wildmenu
set wildmode=longest:full,full

set guifont=M+\ 2m\ medium\ 9
set guioptions=aegirLt
set linespace=-3

colorscheme desert

let &statusline = ' '
let &statusline .= '%<%f %h%m%r%w'
let &statusline .= '[%{&l:fileencoding == "" ? &encoding : &l:fileencoding}]'
let &statusline .= '%='
let &statusline .= '  %-14.(%l,%c%V%) %P '

"""" Key Mapping

"inoremap { {}<LEFT>
"inoremap [ []<LEFT>
"inoremap ( ()<LEFT>
"inoremap " ""<LEFT>
"inoremap ' ''<LEFT>
"vnoremap { "zdi^V{<C-R>z}<ESC>
"vnoremap [ "zdi^V[<C-R>z]<ESC>
"vnoremap ( "zdi^V(<C-R>z)<ESC>
"noremap " "zdi^V"<C-R>z^V"<ESC>
"noremap ' "zdi'<C-R>z'<ESC>

inoremap <C-C> <NOP>

vnoremap <C-Y> !pbcopy<C-M><C-M>u

"""" Swap and Backup Files

set swapfile
set directory=~/tmp
set nobackup

"""" Settings for each file types

au BufNewFile,BufRead *.agda setf agda
au BufNewFile,BufRead *tmux.conf setf tmux

autocmd FileType haskell setlocal tabstop=8 shiftwidth=4 softtabstop=4 expandtab
autocmd FileType agda setlocal tabstop=8 shiftwidth=4 softtabstop=4 expandtab
autocmd FileType ruby setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab
autocmd FileType make setlocal noexpandtab

let hs_highlight_debug = 1

"""" Dvorak Key Mapping

noremap h h
noremap t j
noremap n k
noremap s l

"""" Date and Time

if exists("*strftime")
	function! W3CDTF()
		let dt = strftime("%Y-%m-%dT%T%z")
		return printf("%s:%s",dt[0:21],dt[22:23])
	endfunction
	imap <C-D><C-W> <C-R>=W3CDTF()<CR>
endif

"""" Plugins

call pathogen#runtime_append_all_bundles()

"""" Private Settings

source ~/.vimrc.private

