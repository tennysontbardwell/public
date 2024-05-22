function SetTypescriptOptions()
    set tabstop=2
    set softtabstop=2
    set shiftwidth=2
    set expandtab
    set autoindent
    set fileformat=unix
	call SetColLimit(80)
    nnoremap <space>fp :%!prettier --parser typescript<enter>
endfunction
au BufNewFile,BufRead *.ts call SetTypescriptOptions()
