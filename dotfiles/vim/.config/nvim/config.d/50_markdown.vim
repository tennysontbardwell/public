function SetMarkdownOptions()
    set formatoptions-=t
    set tabstop=2
    set softtabstop=2
    set formatoptions-=t
    set shiftwidth=2
    set spell spelllang=en_us
    function MakeNewMarkdownHeader()
        let c = "#"
        let i = 0
        let line = line('.')
        let fold = foldlevel(line)
        while i < fold
            let c = c . "#"
            let i += 1
        endwhile
        call append(line, c)
        call cursor(line + 1, 100)
    endfunction
    nmap <Leader>n <ESC>:call MakeNewMarkdownHeader()<CR>jA
    nmap <Leader>o <ESC>:!open -a /Applications/Marked\ 2.app/ '%:p'<CR>
endfunction
au BufNewFile,BufRead *.md call SetMarkdownOptions()
