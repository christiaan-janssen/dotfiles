return require('packer').startup(function()
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  use {
    'nvim-telescope/telescope.nvim',
    requires = { {'nvim-lua/plenary.nvim'} }
  }

  -- LSP
  use {
    "williamboman/nvim-lsp-installer",
    "neovim/nvim-lspconfig",
  }

  -- TODO Add TSUpdate hook
  use {
    "nvim-treesitter/nvim-treesitter"
  }

  use 'mfussenegger/nvim-dap' 
  -- Misc
  use "kdheepak/lazygit.nvim"
  use "preservim/nerdtree"

  use "glepnir/lspsaga.nvim"
  use "folke/tokyonight.nvim"
  use "ryanoasis/vim-devicons"
  use {
    'lewis6991/gitsigns.nvim',
    config = function()
      require('gitsigns').setup()
    end
  }

  use { 'christoomey/vim-tmux-navigator' }

  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'kyazdani42/nvim-web-devicons', opt = true }
  }

  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'
  use 'hrsh7th/cmp-path'
  use 'hrsh7th/cmp-cmdline'
  use 'hrsh7th/nvim-cmp'

  use 'L3MON4D3/LuaSnip'
  use 'saadparwaiz1/cmp_luasnip'
  use 'onsails/lspkind.nvim'

  use 'nvim-tree/nvim-web-devicons'
  use 'gpanders/editorconfig.nvim'

  use {
    "windwp/nvim-autopairs",
      config = function() require("nvim-autopairs").setup {} end
  }
  --use 'lommix/godot.nvim'
  use 'habamax/vim-godot'
  use 'vimwiki/vimwiki'
  use 'tools-life/taskwiki'
  use {'nvim-orgmode/orgmode', config = function()
    require('orgmode').setup{}
  end
  }
end)
