"=============================================================================
" FILE: view.vim
" AUTHOR: Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 30 Aug 2009
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
" Version: 1.5, for Vim 7.0
"-----------------------------------------------------------------------------
" ChangeLog: "{{{
"   1.5:
"     - Catch error.
"
"   1.4:
"     - Extend current directory.
"
"   1.3:
"     - Ignore directory.
"
"   1.2:
"     - Improved error.
"
"   1.1:
"     - Split nicely.
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

function! vimshell#internal#view#execute(program, args, fd, other_info)
    " View file.

    " Filename escape
    let l:arguments = join(a:args, ' ')

    if isdirectory(l:arguments)
        " Ignore.
        return 0
    endif

    call vimshell#print_prompt()

    if empty(l:arguments)
        vimshell#error_line(a:fd, 'Filename required.')
    else
        " Save current directiory.
        let l:cwd = getcwd()

        " Split nicely.
        if winheight(0) > &winheight
            split
        else
            vsplit
        endif

        try
            edit `=l:arguments`
        catch /^.*/
            echohl Error | echomsg v:errmsg | echohl None
        endtry

        lcd `=l:cwd`
        setlocal nomodifiable
    endif
endfunction
