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


local lint_autogroup = vim.api.nvim_create_augroup("lint", { clear = false }) -- clear falseにしないとnvim-lintのプロセスが終了されずにlinterのprocessがスタックしていく
vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost" },
    {
        group = lint_autogroup,
        callback = function()
            lint.try_lint()
        end
    }

)

local function file_exists(name)
    local f = io.open(name, "r")
    if f ~= nil then
        io.close(f)
        return true
    else
        return false
    end
end

vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost" }, {
    group = lint_autogroup,
    callback = function()
        if file_exists("vendor/bin/phpstan") then
            lint.linters_by_ft.php = { "phpstan" }
        elseif file_exists("vendor/bin/phpmd") then
            lint.linters_by_ft.php = { "phpmd" }
        end
    end
})
