"-------------------------------------------------------------------------------
" Editing
"
set ambiwidth=double
set backspace=2
set autoindent

"-------------------------------------------------------------------------------
" Searching
"
set ignorecase
set smartcase
set wrapscan
set noincsearch

"-------------------------------------------------------------------------------
" Highlighting
"
if has("syntax")
	syntax on
endif
set nonumber
set listchars=tab:\ \ 
set list
set tabstop=4
set shiftwidth=4
set showcmd
set showmatch
set hlsearch
set laststatus=2
set statusline=%<%f\ %m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).']['.&ff.']'}%=%l,%c%V%8P

"-------------------------------------------------------------------------------
" Mapping
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

