-- Modeline theme
require('lualine').setup()

local set = vim.opt

-- Set the behavior of tab
set.tabstop = 2
set.shiftwidth = 2
set.softtabstop = 2
set.expandtab = true
set.number = true


-- set the theme
vim.cmd [[
  syntax enable
  colorscheme tokyonight
]]
