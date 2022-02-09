" ordinary neovim
luafile $HOME/.config/nvim/lua/plugins.lua
source $HOME/.config/nvim/keybindings.vim

luafile $HOME/.config/nvim/lua/lsp-settings.lua
source $HOME/.config/nvim/lsp-config.vim
luafile $HOME/.config/nvim/lua/omnisharp.lua
luafile $HOME/.config/nvim/lspsaga.lua
luafile $HOME/.config/nvim/compe-config.lua
luafile $HOME/.config/nvim/lua/lsp_install.lua

source $HOME/.config/nvim/ui.vim
luafile $HOME/.config/nvim/lua/evileline.lua

filetype plugin indent on
syntax on

set notimeout
set timeoutlen=500
set tabstop     =4
set softtabstop =4
set shiftwidth  =4
set expandtab
let g:tagbar_position = 'topleft vertical'


" Setup bracket matching
inoremap ( ()<left>
inoremap [ []<left>
inoremap { {}<left>
inoremap {<CR> {<CR>}<ESC>O
inoremap {;<CR> {<CR>};<ESC>O
inoremap <expr> ) strpart(getline('.'), col('.')-1, 1) == ")" ? "\<Right>" : ")"
inoremap <expr> } strpart(getline('.'), col('.')-1, 1) == "}" ? "\<Right>" : "}"
inoremap <expr> ] strpart(getline('.'), col('.')-1, 1) == "]" ? "\<Right>" : "]"
inoremap <expr> ' strpart(getline('.'), col('.')-1, 1) == "\'" ? "\<Right>" : "\'\'\<Left>"
inoremap <expr> " strpart(getline('.'), col('.')-1, 1) == "\"" ? "\<Right>" : "\"\"\<Left>"

