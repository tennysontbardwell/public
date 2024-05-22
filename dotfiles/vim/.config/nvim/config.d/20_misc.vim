filetype plugin indent on    " required

set mouse=a
set backspace=2 " Because backspace in insert mode wasn't working

" Unorganized
nnoremap <CR> o<Esc>0D
set nojoinspaces
set tabstop=4
set shiftwidth=4
set expandtab
nmap <s-c-up> :m -2<CR>
nmap <s-c-down> :m +1<CR>
let mapleader="\\"
set linebreak
set breakindent
noremap j gj
noremap k gk
set shortmess=a
set cmdheight=2
nnoremap <leader>m :w\|make\|redraw!\|cc<CR>
set completeopt=longest,menuone
set ignorecase
set smartcase
set splitbelow
set splitright
nnoremap <silent> <C-l> :nohlsearch<CR><C-l>
noremap ; :
noremap <C-y> 3<C-y>
noremap <C-e> 3<C-e>
inoremap zzd <esc>
noremap o A<CR>
" Highlight searchign
nnoremap <leader>s :set hlsearch!<CR>
nnoremap <leader>s <C-o>:set hlsearch!<CR>
let g:multi_cursor_exit_from_insert_mode=0
filetype on
