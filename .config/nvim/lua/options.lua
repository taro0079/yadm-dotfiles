-- vim.g.maplocalleader = ','
vim.g.mapleader = ','
vim.o.tags = './tags'
vim.wo.number = true
vim.wo.relativenumber = true
vim.o.smarttab = true
vim.o.cursorline = true
vim.o.pumblend = 20
if vim.fn.has("wsl") == 0 then
  vim.o.clipboard = "unnamed,unnamedplus"
end
vim.o.completeopt = "menuone,noinsert,noselect"
vim.o.expandtab = true
vim.o.tabstop = 2
vim.o.smartindent = true
vim.o.shiftwidth = 2
vim.o.signcolumn = "yes" -- これを設定しないとvgitでガタつく
vim.updatetime = 100
vim.o.incsearch = false
vim.opt.termguicolors = true
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.o.mouse = ""
vim.o.title = true
vim.o.autoindent = true

