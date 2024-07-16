local lint = require("lint")
local phpmd = require('lint').linters.phpmd
phpmd.args = {
    '-',
    'json',
    './.phpmd/phpmd.xml'

}
-- lint.linters_by_ft = {
--     php = { "phpstan", "phpmd" },
-- }

-- local phpstan = require('lint').linters.phpstan
-- phpstan.args = {
--     'analyse',
--     '--error-format',
--     'json',
--     '--no-progress',
--     '--memory-limit',
--     '1G',
--     '--configuration',
--     './phpstan.dist.neon',
--     '-'
-- }

local file_exists = require('helper.file_exist')

local lint_autogroup = vim.api.nvim_create_augroup("lint", { clear = false }) -- clear falseにしないとnvim-lintのプロセスが終了されずにlinterのprocessがスタックしていく
-- vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost" },
--     {
--         group = lint_autogroup,
--         callback = function()
--             lint.try_lint()
--         end
--     }
--
-- )


vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost" }, {
    group = lint_autogroup,
    callback = function()
        local linters = {}
        if file_exists.file_exist("vendor/bin/phpstan") then
            -- lint.linters_by_ft.php = { "phpstan" }
            table.insert(linters, "phpstan")
        elseif file_exists.file_exist("vendor/bin/phpmd") then
            -- lint.linters_by_ft.php = { "phpmd" }
            table.insert(linters, "phpmd")
        end
        lint.linters_by_ft.php = linters
        lint.try_lint()
    end
})
