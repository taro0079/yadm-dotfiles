vim.opt.runtimepath:append("~/.local/share/nvim/site/pack/packer/start/nvim-treesitter/parser")
require 'nvim-treesitter.configs'.setup {
	ensure_installed = {"lua", "vim", "typescript", "javascript"},
	auto_install = false,
	indent = {
		enable = true
	},
	highlight = {
		enable =true,
	}


}
