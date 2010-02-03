"=============================================================================
" FILE: popd.vim
" AUTHOR: Shougo Matsushita <Shougo.Matsu@gmail.com>(Modified)
" Last Modified: 12 Jul 2009
" Usage: Just source this file.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
" Version: 1.3, for Vim 7.0
"-----------------------------------------------------------------------------
" ChangeLog: "{{{
"   1.3:
"     - Improved error message.
"
"   1.2:
"     - Supported vimshell Ver.3.2.
"
"   1.1:
"     - Use vimshell#error_line.
"
"   1.0:
"     - Initial version.
""}}}
"-----------------------------------------------------------------------------
" TODO: "{{{
"     - Nothing.
""}}}
" Bugs"{{{
"     -
""}}}
"=============================================================================

function! vimshell#internal#popd#execute(program, args, fd, other_info)
    " Pop directory.

    if empty(w:vimshell_directory_stack)
        " Error.
        call vimshell#error_line(a:fd, 'Directory stack is empty.')
        return
    endif

    let l:cnt = 0
    let l:arguments = join(a:args)
    if l:arguments =~ '^\d\+$'
        let l:pop = str2nr(l:arguments)
    elseif empty(l:arguments)
        " Default pop value.
        let l:pop = 1
    else
        " Error.
        call vimshell#error_line(a:fd, 'Arguments error .')
        return
    endif
    
    if l:pop >= len(w:vimshell_directory_stack)
        " Overflow.
        call vimshell#error_line(a:fd, printf("Not found '%d' in directory stack.", l:pop))
        return
    endif

    lcd `=w:vimshell_directory_stack[l:pop]`

    " Pop from stack.
    let w:vimshell_directory_stack = w:vimshell_directory_stack[l:pop+1:]
endfunction
