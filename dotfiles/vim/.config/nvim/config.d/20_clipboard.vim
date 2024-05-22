set clipboard=unnamed " makes the system clipboard the default
if has('macunix')
    vmap <C-c> :w !pbcopy<CR><CR>
elseif has('unix')
    vmap <C-c> "+y
endif
