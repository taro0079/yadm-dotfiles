require('options')
require('plugins')
require('keys')
require('config')
require('gui-settings')
require('helper.matchit-setting')

-- vim.api.nvim_create_autocmd('BufWritePre', {
--     -- pattern = "*php",
--     -- callback = function()
--     --     vim.cmd('!php-cs-fixer fix %')
--     --     vim.cmd('e!')
--     -- end
--     pattern = "*",
--     callback = function(args)
--         require("conform").format({ async = true, bufnr = args.buf, lsp_fallback = true })
--     end
-- })

incremental_selection = {
    enable = true,
    keymaps = {
        init_selection = "<CR>",
        node_incremental = "<CR>",
        node_decremental = "<BS>",
        scope_incremental = "<TAB>",
    },
    is_supported = function()
        local mode = vim.api.nvim_get_mode().mode
        if mode == "c" then
            return false
        end
        return true
    end
}
