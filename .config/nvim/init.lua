if not vim.g.vscode then
    require('options')
    require('plugins')
    require('keys')
    require('config')
    require('gui-settings')
    require('helper.matchit-setting')
elseif vim.g.vscode then
    vim.wo.number = true
    vim.wo.relativenumber = true
    vim.o.autoindent = true
    vim.wo.wrap = true
    vim.o.hidden = true
    vim.o.smartindent = true
    vim.wo.list = true
    vim.wo.listchars = "eol:↲,tab:>.,trail:~,space:␣,nbsp:%"
    local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
    if not vim.loop.fs_stat(lazypath) then
        vim.fn.system({
            "git",
            "clone",
            "--filter=blob:none",
            "https://github.com/folke/lazy.nvim.git",
            "--branch=stable", -- latest stable release
            lazypath,
        })
    end
    vim.opt.rtp:prepend(lazypath)
    require("lazy").setup({
        {
            'easymotion/vim-easymotion'
        },
        {
            "junegunn/vim-easy-align"
        },
        {
            'tpope/vim-surround'
        },
        {
            'tpope/vim-commentary'
        },
        {
            'tpope/vim-repeat'
        },
        {
            'thinca/vim-qfhl'
        }

    })
vim.keymap.set("x", "ga", "<Plug>(EasyAlign)")
vim.keymap.set("n", "ga", "<Plug>(EasyAlign)")
end
