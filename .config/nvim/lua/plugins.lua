local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)
require("lazy").setup({
      { 'williamboman/mason.nvim',  dependencies = { 'williamboman/mason-lspconfig.nvim' } },
        { 'neovim/nvim-lspconfig', config = function() require 'plugins.lsp' end },
        {

    "hrsh7th/nvim-cmp",

    -- load cmp on InsertEnter

    event = "InsertEnter",

    -- these dependencies will only be loaded when cmp loads

    -- dependencies are always lazy-loaded unless specified otherwise

    dependencies = {

      "hrsh7th/cmp-nvim-lsp",

      "hrsh7th/cmp-buffer",

      'hrsh7th/cmp-calc',

      'hrsh7th/cmp-omni',

      'hrsh7th/cmp-nvim-lsp-signature-help',

      'hrsh7th/cmp-nvim-lsp-document-symbol',

      'hrsh7th/cmp-emoji',

      'hrsh7th/cmp-path',

      'hrsh7th/cmp-cmdline',

      'f3fora/cmp-spell',

      'hrsh7th/cmp-copilot',

      'chrisgrieser/cmp-nerdfont'

      -- 'uga-rosa/cmp-dictionary', config = function() require 'plugins.dictionary' end } #TODO

    },

    config = function()

      require 'plugins.cmp'

    end,

  },




})
