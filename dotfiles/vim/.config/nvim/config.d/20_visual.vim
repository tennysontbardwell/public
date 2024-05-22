syntax on
autocmd BufWinEnter * if line2byte(line("$") + 1) > 10000000 | syntax clear | endif

set nohlsearch
set number
set cursorline
hi CursorLine cterm=NONE ctermbg=234
hi CursorColumn cterm=NONE ctermbg=234

set background=dark
colorscheme PaperColor

function SetColLimit(lim)
    highlight ColorColumn ctermbg=234 guibg=#242520
    let columnLimit = a:lim + 1
    let &colorcolumn=join(range(columnLimit,columnLimit),",")
endfunction

