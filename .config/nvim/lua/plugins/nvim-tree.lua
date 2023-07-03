vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

vim.opt.termguicolors = true

require("nvim-tree").setup({
  on_attach = on_attach,
  sort_by = "case_sensitive",
  update_focused_file = {
    enable = true,
    update_cwd = true,
  },
  renderer = {
    group_empty = true,
  },
  filters = {
    dotfiles = false
  },
})
