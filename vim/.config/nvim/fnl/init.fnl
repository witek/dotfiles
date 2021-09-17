(module dotfiles.init
  {require {nvim aniseed.nvim
            mapping dotfiles.mapping
            }})

(set nvim.g.maplocalleader ",")

(set nvim.o.mouse "a")

(mapping.setup)

