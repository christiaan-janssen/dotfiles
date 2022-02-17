
let g:mapleader = " "
let g:maplocalleader = ','

" code action
nnoremap <silent><leader>,, <cmd>lua require('lspsaga.codeaction').code_action()<CR>
vnoremap <silent><leader>,a <cmd>'<,'>lua require('lspsaga.codeaction').range_code_action()<CR>
" show hover doc
nnoremap <silent> K <cmd>lua require('lspsaga.hover').render_hover_doc()<CR>
" scroll down hover doc or scroll in definition preview
nnoremap <silent> <C-f> <cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<CR>
" scroll up hover doc
nnoremap <silent> <C-b> <cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<CR>
" show signature help
nnoremap <silent> gs <cmd>lua require('lspsaga.signaturehelp').signature_help()<CR>
" rename
nnoremap <silent>gr <cmd>lua require('lspsaga.rename').rename()<CR>
" preview definition
nnoremap <silent> gd <cmd>lua require'lspsaga.provider'.preview_definition()<CR>

" show
nnoremap <silent><leader>cd <cmd>lua require'lspsaga.diagnostic'.show_line_diagnostics()<CR>

nnoremap <silent> <leader>cd :Lspsaga show_line_diagnostics<CR>
" jump diagnostic
nnoremap <silent> [e <cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_prev()<CR>
nnoremap <silent> ]e <cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_next()<CR>


inoremap <silent><expr> <TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm('<CR>')
inoremap <silent><expr> <C-e>     compe#close('<C-e>')
inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })
inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })

" go to different screen with ctrl JKLH
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Key Bindings
:imap jk <esc>
:tnoremap jk <C-\><C-n>

nmap <F3> :TagbarToggle<CR>
nmap <F4> :NERDTreeToggle<CR>

" ### Telescope ###
" Find files using Telescope command-line sugar.
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>bb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

nnoremap <leader>ww <C-W>w
nnoremap <leader>wd <C-W>c
nnoremap <leader>ws <C-W>s
nnoremap <leader>wv <C-W>v

nnoremap <leader>fw <cmd>VimwikiIndex<cr>

" vim-test
" these "Ctrl mappings" work well when Caps Lock is mapped to Ctrl
nmap <leader> tt :TestNearest<CR>
nmap <leader> tf :TestFile<CR>
nmap <leader> ts :TestSuite<CR>
nmap <leader> tl :TestLast<CR>
nmap <leader> tv :TestVisit<CR>


" Neogit
nmap <leader> gg :Neogit<CR>
nmap <leader> gs :Neogit<CR>
