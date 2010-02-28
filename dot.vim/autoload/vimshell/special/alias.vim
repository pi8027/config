"=============================================================================
" FILE: alias.vim
" AUTHOR: Shougo Matsushita <Shougo.Matsu@gmail.com>(Modified)
" Last Modified: 15 Jun 2010
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
" Version: 1.7, for Vim 7.0
"-----------------------------------------------------------------------------
" ChangeLog: "{{{
"   1.7:
"     - Changed as special command.
"     - Fixed parse.
"
"   1.6:
"     - Fixed parse bug.
"     - Improved error message.
"
"   1.5:
"     - Changed alias syntax.
"
"   1.4:
"     - Optimized parse.
"
"   1.3:
"     - Supported vimshell Ver.3.2.
"
"   1.2:
"     - Use vimshell#print_line.
"
"   1.1:
"     - Changed s:alias_table into b:vimshell_alias_table.
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

function! vimshell#special#alias#execute(program, args, fd, other_info)
    if empty(a:args)
        " View all aliases.
        for alias in keys(b:vimshell_alias_table)
            call vimshell#print_line(a:fd, printf('%s=%s', alias, b:vimshell_alias_table[alias]))
        endfor
    elseif join(a:args) =~ '^\h\w*$'
        if has_key(b:vimshell_alias_table, a:args[0])
            " View alias.
            call vimshell#print_line(a:fd, b:vimshell_alias_table[a:args[0]])
        endif
    else
        " Define alias.
        let l:args = join(a:args)

        " Parse command line.
        let l:alias_name = matchstr(l:args, '^\h\w*')

        " Next.
        let l:args = l:args[matchend(l:args, '^\h\w*') :]
        if l:alias_name == '' || l:args !~ '^\s*=\s*'
            call vimshell#error_line(a:fd, 'Wrong syntax: ' . join(a:args))
            return
        endif
        
        " Skip =.
        let l:expression = l:args[matchend(l:args, '^\s*=\s*') :]
        
        try
            execute printf('let b:vimshell_alias_table[%s] = %s', string(l:alias_name),  string(l:expression))
        catch
            call vimshell#error_line(a:fd, 'Wrong syntax: ' . join(a:args))
            return
        endtry
    endif
endfunction
