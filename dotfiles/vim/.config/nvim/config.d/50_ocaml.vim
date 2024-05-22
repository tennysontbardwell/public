" OCaml specific
" Merlin
set rtp+=/usr/local/share/ocamlmerlin/vim
" This next line is needed to add documentation and needs to be rerun as
" merlin updates
" :execute "helptags " . substitute(system('opam config var share'),'\n$','','''') .  "/merlin/vim/doc"
" let g:syntastic_ocaml_checkers = ['merlin']

function SetOCamlOptions()
    set commentstring=(*\ %s\ *)
	set tabstop=2
	set shiftwidth=2
	set expandtab
	call SetColLimit(80)
endfunction
au BufNewFile,BufRead *.{ml,mli} call SetOCamlOptions()

" ocp-indent-vim
set rtp+=~/.vim/misc/ocp-indent-vim

