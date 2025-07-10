syntax on
autocmd BufWinEnter * if line2byte(line("$") + 1) > 10000000 | syntax clear | endif

set nohlsearch
set number
set cursorline
hi CursorLine cterm=NONE ctermbg=234
hi CursorColumn cterm=NONE ctermbg=234

autocmd VimEnter * call SetupColorscheme()

function! SetupColorscheme()
    let theme = system('defaults read -g AppleInterfaceStyle 2>/dev/null')
    if theme =~ 'Dark'
        set background=dark
    else
        set background=light
    endif
    colorscheme PaperColor
endfunction

function SetColLimit(lim)
    highlight ColorColumn ctermbg=234 guibg=#242520
    let columnLimit = a:lim + 1
    let &colorcolumn=join(range(columnLimit,columnLimit),",")
endfunction

