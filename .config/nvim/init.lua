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

vim.api.nvim_set_hl(0, 'LineNrAbove', { fg='magenta' })
vim.api.nvim_set_hl(0, 'LineNr', { fg='yellow' })
vim.api.nvim_set_hl(0, 'LineNrBelow', { fg='magenta' })
