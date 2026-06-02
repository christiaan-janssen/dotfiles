return {
  { "folke/flash.nvim", enabled = false },
  {
    "julienvincent/nvim-paredit",
    lazy = false,
    config = function()
      require("nvim-paredit").setup()
    end,
  },
}
