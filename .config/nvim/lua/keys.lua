local api = vim.api
-- Telescope -- {{ 1
api.nvim_set_keymap('n', '<C-p>', "<cmd>lua require('telescope.builtin').git_files()<cr>",
    { noremap = true, silent = true })
api.nvim_set_keymap('n', '<leader>ff', "<cmd>lua require('telescope.builtin').find_files({hidden=true, no_ignore=true})<cr>",
    { noremap = true, silent = true })
api.nvim_set_keymap('n', '<leader>fq', "<cmd>lua require('telescope.builtin').live_grep({hidden=true, no_ignore=true})<cr>",
    { noremap = true, silent = true })
api.nvim_set_keymap('n', '<leader>fb', "<cmd>lua require('telescope.builtin').buffers()<cr>", {
    noremap = true,
    silent = true
})
vim.keymap.set('n', '<leader>lt', ':set list!<CR>')
-- Bookmark
vim.keymap.set('n', '<leader>bb', '<Plug>BookmarkToggle')
vim.keymap.set('n', '<leader>bx', '<Plug>BookmarkClearAll')
api.nvim_set_keymap('n', '<leader>fh', "<cmd>lua require('telescope.builtin').help_tags()<cr>",
    { noremap = true, silent = true })
api.nvim_set_keymap('n', '<leader>ps',
    "<cmd>lua require('telescope.builtin').grep_string({ search = vim.fn.input('Grep For > ')}) <cr>",
    { noremap = true, silent = false })
api.nvim_set_keymap('n', "<leader>gb", "<cmd>lua require('telescope.builtin').git_branches({theme=get_dropdown})<cr>",
    { noremap = true, silent = true })
api.nvim_set_keymap('n', "<leader>gc", "<cmd>lua require('telescope.builtin').git_commits({theme=get_dropdown})<cr>",
    { noremap = true, silent = true })
api.nvim_set_keymap('n', "<leader>gs", "<cmd>lua require('telescope.builtin').git_status({theme=get_dropdown})<cr>",
    { noremap = true, silent = true })
local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ps', function()
	builtin.grep_string({ search = vim.fn.input("Grep > "), no_ignore=true})
end)
-- skk setting
api.nvim_set_keymap('c', "<C-j>", "<Plug>(skkeleton-toggle)", { noremap = true, silent = true })
api.nvim_set_keymap('i', "<C-j>", "<Plug>(skkeleton-toggle)", { noremap = true, silent = true })

-- setting windows size
api.nvim_set_keymap('n', "<leader>+", "<cmd> vertical resize +5<cr>", { noremap = true, silent = true })
api.nvim_set_keymap('n', "<leader>-", "<cmd> vertical resize -5<cr>", { noremap = true, silent = true })

-- source init.lua
api.nvim_set_keymap('n', "<leader><cr>", "<cmd> so ~/.config/nvim/init.lua <cr>", { noremap = true, silent = true })

api.nvim_set_keymap('n', "<C-n>", ":NvimTreeToggle<cr>", { noremap = true, silent = true })

-- lsp diagnostic
api.nvim_set_keymap('n', "<leader>sd", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics() <cr>",
    { noremap = true, silent = true })

vim.keymap.set("x", "ga", "<Plug>(EasyAlign)")
vim.keymap.set("n", "ga", "<Plug>(EasyAlign)")



-- vim.cmd[[imap <silent><script><expr> <C-J> copilot#Accept("\<CR>")]]
vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle)
vim.keymap.set("n", "<leader>f", vim.lsp.buf.format)
vim.keymap.set("i", "<C-c>", "<Esc>")

vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("i", "<C-c>", "<Esc>")

-- from https://github.com/ThePrimeagen/init.lua/blob/master/lua/theprimeagen/remap.lua
-- greatest remap ever
vim.keymap.set("x", "<leader>p", [["_dP]])

-- next greatest remap ever : asbjornHaland
vim.keymap.set({"n", "v"}, "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])

vim.keymap.set({"n", "v"}, "<leader>d", [["_d]])


vim.keymap.set("n", "<C-k>", "<cmd>cnext<CR>zz")
vim.keymap.set("n", "<C-j>", "<cmd>cprev<CR>zz")
vim.keymap.set("n", "<leader>k", "<cmd>lnext<CR>zz")
vim.keymap.set("n", "<leader>j", "<cmd>lprev<CR>zz")
