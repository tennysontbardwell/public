for f in split(glob('$HOME/.config/nvim/config.d/*.vim'), '\n')
  exe 'source' f
endfor
