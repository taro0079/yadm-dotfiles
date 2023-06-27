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
	{
		'neoclide/coc.nvim',
		branch = 'release',
		config = function()
			require("plugins.coc")
		end

	},
	{
		"tpope/vim-fugitive"
	},

	{
		"folke/flash.nvim",

		event = "VeryLazy",
		---@type Flash.Config
		opts = {},
		keys = {
			{
				"s",
				mode = { "n", "x", "o" },
				function()
					-- default options: exact mode, multi window, all directions, with a backdrop
					require("flash").jump()
				end,
				desc = "Flash",
			},
			{
				"S",
				mode = { "n", "o", "x" },
				function()
					require("flash").treesitter()
				end,
				desc = "Flash Treesitter",
			},
			{
				"r",
				mode = "o",
				function()
					require("flash").remote()
				end,
				desc = "Remote Flash",
			},
		},
	},
	{
		'Mofiqul/dracula.nvim',
		config = function()
			vim.cmd [[colorscheme dracula]]
		end
	},
	{
		"hoob3rt/lualine.nvim",
		config = function()
			require("plugins.lualine")
		end,
	},
	{
		'lewis6991/gitsigns.nvim',
		event = "BufEnter",
		config = function()
			require("plugins.gitsign")
		end,
	},
  {
    "nvim-telescope/telescope.nvim",
    -- lazy=true,
    dependencies = { "nvim-lua/plenary.nvim" },
    config = true,
  },





})
