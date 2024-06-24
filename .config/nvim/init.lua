require('options')
require('plugins')
require('keys')
require('config')
require('gui-settings')
require('helper.matchit-setting')

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

vim.api.nvim_create_autocmd("FileType", {
    pattern = "yaml",
    callback = function()
        vim.bo.shiftwidth = 4
        vim.bo.softtabstop = 4
        vim.bo.expandtab = true
    end
})
