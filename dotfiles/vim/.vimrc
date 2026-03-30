for f in sort(split(globpath('~/.config/nvim/config.d', '**/*.vim'), '\n'))
    exe 'source' f
endfor
