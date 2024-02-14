local lint = require("lint")

lint.linters_by_ft = {
    php = { 'phpcs', 'phpstan' }
}

local lint_autogroup = vim.api.nvim_create_augroup("lint", { clear = true })
vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "InsertLeave" },
    {
        group = lint_autogroup,
        callback = function()
            lint.try_lint()
        end
    }

)
