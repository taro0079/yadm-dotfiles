local null_ls = require("null-ls")
local eslint = require("eslint")

null_ls.setup()

eslint.setup({
	bin = "eslint",
	code_actions = {
		enable = true,
		apply_on_save = {
			enable = true,
			types = { "suggestion" },
		},
		disable_rule_comment = {
			enable = true,
			location = "separate_line",
		},
	},
	deagnostics = {
		enable = true,
		report_unused_disable_directives = false,
		run_on = "type",
	},
})
