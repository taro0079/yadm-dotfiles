require('options')
require('plugins')
require('keys')
require('config')
require('gui-settings')
require('helper.matchit-setting')

vim.api.nvim_create_autocmd('BufWritePost', {
    pattern = "*php",
    callback = function()
        vim.cmd('!php-cs-fixer fix %')
        vim.cmd('e!')

    end
})
