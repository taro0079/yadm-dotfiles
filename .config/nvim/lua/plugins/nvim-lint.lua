local lint = require("lint")

lint.linters_by_ft = {
    php = {"phpstan", "phpmd"},
}


local lint_autogroup = vim.api.nvim_create_augroup("lint", { clear = false }) -- clear falseにしないとnvim-lintのプロセスが終了されずにlinterのprocessがスタックしていく
vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost" },
    {
        group = lint_autogroup,
        callback = function()
            lint.try_lint()
        end
    }

)
