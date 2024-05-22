 for f in split(glob('~/.config/nvim/*.vim'), '\n')
   exe 'source' f
endfor
