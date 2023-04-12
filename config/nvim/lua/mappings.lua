-- Functional wrapper for mapping custom keybindings
function map(mode, lhs, rhs, opts)
    local options = { noremap = true }
    if opts then
        options = vim.tbl_extend("force", options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

vim.g.mapleader = " "

map("i", "jk", "<Esc>")

map("n", "<Leader>ff", ":Telescope find_files <CR>")
map("n", "<Leader>fg", ":Telescope live_grep <CR>")
map("n", "<Leader>fb", ":Telescope buffers <CR>")
map("n", "<Leader>fh", ":Telescope help_tags <CR>")

-- Code actions
map("n", "<leader>ca", ":Lspsaga code_action <cr>")
map("n", "<leader>cd", ":Telescope diagnostics <cr>")
map("n", "<leader>ci", ":Telescope lsp_implementations <cr>")
map("n", "<leader>cd", ":Telescope lsp_definitions <cr>")
map("n", "<leader>cd", ":Telescope lsp_type_definitions <cr>")
map("n", "<leader>cf", ":Telescope quickfix <cr>")

-- lspsaga
map("n", "<leader>gf", ":Lspsaga lsp_finder <cr>")
map("n", "<leader>gr", ":Lspsaga rename <cr>")
map("n", "<leader>gd", ":Lspsaga peek_definition <cr>")
map("n", "<leader>gr", ":Lspsaga rename <cr>")

map("n", "<leader>[e", ":Lspsaga diagnostic_jump_prev <cr>")
map("n", "<leader>e]", ":Lspsaga diagnostic_jump_next <cr>")
-- misc
map("n", "<leader>mm", ":Telescope man_pages <cr>")
map("n", "<leader>mc", ":Telescope commands <cr>")
map("n", "<leader>mk", ":Telescope keymaps <cr>")

-- Search
map("n", "<leader>sb", ":Telescope current_buffer_fuzzy_find <cr>")

-- Git
map("n", "<leader>gs", ":Telescope git_status <cr>")
map("n", "<leader>gb", ":Telescope git_branches <cr>")
map("n", "<leader>gc", ":Telescope git_commits <cr>")

map("n", "<Leader>ft", ":NERDTreeToggle <CR>")

map("n", "<Leader>gg", ":LazyGit<CR>")

map("n", "<Leader>wv", ":vsplit <CR>")
map("n", "<Leader>ws", ":split <CR>")

map("n", "<F5>", ":GodotRun <CR>")

-- Better split movement
map("n", "<C-J>", "<C-W><C-J>")
map("n", "<C-K>", "<C-W><C-K>")
map("n", "<C-L>", "<C-W><C-L>")
map("n", "<C-H>", "<C-W><C-H>")

map("n", "<leader>lf", ":luafile %<CR>")
