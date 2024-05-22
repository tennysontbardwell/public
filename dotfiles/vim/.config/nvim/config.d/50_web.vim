function SettingsForJS()
    set tabstop=2
    set softtabstop=2
    set shiftwidth=2
    set commentstring=//\ %s
    set cursorcolumn
endfunction
au BufNewFile,BufRead *.{js,jsx,json} call SettingsForJS()

function SettingsForHTML()
    set tabstop=2
    set softtabstop=2
    set shiftwidth=2
    set cursorcolumn
endfunction
au BufNewFile,BufRead *.html call SettingsForHTML()
