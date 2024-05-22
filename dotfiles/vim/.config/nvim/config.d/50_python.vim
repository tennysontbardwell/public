function SetPythonOptions()
    set tabstop=4
    set softtabstop=4
    set shiftwidth=4
    set expandtab
    set autoindent
    set fileformat=unix
    set filetype=python
	call SetColLimit(80)
    " SyntasticToggleMode
    "nnoremap <silent> :exec :!clear;python %<CR>
endfunction
au BufNewFile,BufRead *.{py} call SetPythonOptions()
