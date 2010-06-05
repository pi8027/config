
function ReloadSyntax()
        if filereadable("." . expand('%:') . ".vim")
                source .%.vim
        endif
endfunction

call ReloadSyntax()

map ,rs :call ReloadSyntax()<CR>

