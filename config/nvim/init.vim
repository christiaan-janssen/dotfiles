
call plug#begin()
  Plug 'tpope/vim-sensible'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-surround'
  Plug 'scrooloose/nerdtree'
  Plug 'ctrlpvim/ctrlp.vim'

  Plug 'hashivim/vim-terraform'

  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  Plug 'deoplete-plugins/deoplete-go', { 'do': 'make'}
  Plug 'deoplete-plugins/deoplete-jedi'

  Plug 'jnurmine/Zenburn'

call plug#end()

set number
set splitbelow
set splitright

let g:deoplete#enable_at_startup = 1

:imap jk <esc>

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

map <F3> :NERDTreeToggle<CR>

colorscheme zenburn

