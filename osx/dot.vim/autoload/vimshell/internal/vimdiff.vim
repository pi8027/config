"=============================================================================
" FILE: vimdiff.vim
" AUTHOR: Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 12 Sep 2009
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
" Version: 1.0, for Vim 7.0
"-----------------------------------------------------------------------------
" ChangeLog: "{{{
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

function! vimshell#internal#vimdiff#execute(program, args, fd, other_info)
    " Diff file1 file2.

    if len(a:args) != 2
        " Error.
        call vimshell#error_line(a:fd, 'Usage: vimdiff file1 file2')
        return 0
    endif

    call vimshell#print_prompt()

    " Save current directiory.
    let l:cwd = getcwd()

    " Split nicely.
    if winheight(0) > &winheight
        split
    else
        vsplit
    endif

    try
        edit `=a:args[0]`
    catch /^.*/
        echohl Error | echomsg v:errmsg | echohl None
    endtry

    lcd `=l:cwd`

    vertical diffsplit `=a:args[1]`

    return 1
endfunction
