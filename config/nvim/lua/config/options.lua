-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here
return {
  {
    "folke/snacks.nvim",
    ---@type snacks.Config
    opts = {
      terminal = {
        -- your terminal configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
      },
    },
    keys = {
      {
        "<c-/>",
        function()
          Snacks.terminal()
        end,
      },
    },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = {
        "bash",
        "html",
        "javascript",
        "json",
        "lua",
        "markdown",
        "markdown_inline",
        "python",
        "query",
        "regex",
        "tsx",
        "typescript",
        "vim",
        "yaml",
        "odin",
      },
    },
  },
}
