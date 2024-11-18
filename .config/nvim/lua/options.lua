-- vim.g.mapleader = ' '
vim.o.tags = './tags'
vim.wo.number = true
vim.wo.relativenumber = true
vim.o.smarttab = true
vim.o.cursorline = true
vim.o.pumblend = 10
vim.o.winblend = 10
if vim.fn.has("wsl") == 0 then
    vim.o.clipboard = "unnamed,unnamedplus"
end
vim.o.completeopt = "menuone,noinsert,noselect"
vim.o.expandtab = true
vim.o.tabstop = 4
vim.o.smartindent = true
-- vim.o.mouse = 'nvi'
vim.o.mouse = ''
vim.o.shiftwidth = 4
vim.o.signcolumn = "yes" -- これを設定しないとvgitでガタつく
vim.updatetime = 100
vim.opt.hlsearch = false
vim.o.incsearch = true
vim.opt.termguicolors = true
vim.opt.autoindent = true
vim.opt.smartindent = true
-- vim.o.title = true
vim.o.autoindent = true
vim.wo.wrap = true
vim.o.hidden = true
vim.wo.listchars = "eol:↲,tab:>.,trail:~,space:␣,nbsp:%"
vim.wo.list = false
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.opt.undofile = true
local function paste()
    return {
        vim.fn.split(vim.fn.getreg(""), "\n"),
        vim.fn.getregtype(""),
    }
end
-- vim.g.clipboard = {
--   name = 'OSC 52',
--   copy = {
--     ['+'] = require('vim.ui.clipboard.osc52').copy('+'),
--     ['*'] = require('vim.ui.clipboard.osc52').copy('*'),
--   },
--   paste = {
--     ['+'] = paste,
--     ['*'] = paste,
--   },
-- }
