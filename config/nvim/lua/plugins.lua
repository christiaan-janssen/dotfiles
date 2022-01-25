-- check if packer is installed (~/local/share/nvim/site/pack)
local packer_exists = pcall(vim.cmd, [[packadd packer.nvim]])

return require("packer").startup(
    function()
        use {"wbthomason/packer.nvim", opt = true}
        use {"kyazdani42/nvim-web-devicons"}
        use {"kyazdani42/nvim-tree.lua"}
        use {"lewis6991/gitsigns.nvim"}
        use {"907th/vim-auto-save"}
        use {"nvim-treesitter/nvim-treesitter"}
        use {"chriskempson/base16-vim"}
        use {"norcalli/nvim-colorizer.lua"}
        use {'Yggdroot/indentLine'}
        use {"ryanoasis/vim-devicons"}
        use {"sbdchd/neoformat"}
        use {
            "neovim/nvim-lspconfig",
            "williamboman/nvim-lsp-installer",
        }
        use {"hrsh7th/nvim-compe"}
        use {"windwp/nvim-autopairs"}
        use {"alvan/vim-closetag"}
        use {"tweekmonster/startuptime.vim"}
        use {"onsails/lspkind-nvim"}

        use {"TimUntersberger/neogit"}

        use({
            "NTBBloodbath/galaxyline.nvim",
            -- your statusline
            config = function()
                require("galaxyline.themes.eviline")
            end,
            -- some optional icons
            requires = { "kyazdani42/nvim-web-devicons", opt = true }
        })

        use {
          'nvim-telescope/telescope.nvim',
          requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}
        }

        use {"nvim-telescope/telescope-media-files.nvim"}

        use {"christoomey/vim-tmux-navigator"}

        use {'arcticicestudio/nord-vim'}
        use {'mhartington/oceanic-next'}
        use {'folke/tokyonight.nvim'}

        use {'preservim/nerdtree'}
        use {'preservim/tagbar'}
        use {'junegunn/fzf.vim'}
        use {'tmux-plugins/vim-tmux'}
        use {'dart-lang/dart-vim-plugin'}
        use {'hashivim/vim-terraform'}
        use {'tpope/vim-surround'}
        use {'glepnir/lspsaga.nvim'}

        -- Clojure
        use { 'Olical/conjure' }
        use { 'tpope/vim-dispatch' }
        use { 'clojure-vim/vim-jack-in' }
        use { 'radenling/vim-dispatch-neovim' }
        use { 'bhurlow/vim-parinfer' }

        -- Fennel
        use { 'Olical/aniseed' }

        -- Testing
        use { 'vim-test/vim-test' }
        use { 'David-Kunz/jester' }

        -- Ruby

    end
)
