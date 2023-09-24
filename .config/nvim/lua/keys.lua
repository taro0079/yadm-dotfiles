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


-- Move to previous/next
vim.keymap.set('n', '<A-,>', '<Cmd>BufferPrevious<CR>', opts)
vim.keymap.set('n', '<A-.>', '<Cmd>BufferNext<CR>', opts)
-- Re-order to previous/next
vim.keymap.set('n', '<A-<>', '<Cmd>BufferMovePrevious<CR>', opts)
vim.keymap.set('n', '<A->>', '<Cmd>BufferMoveNext<CR>', opts)
-- Goto buffer in position...
vim.keymap.set('n', '<A-1>', '<Cmd>BufferGoto 1<CR>', opts)
vim.keymap.set('n', '<A-2>', '<Cmd>BufferGoto 2<CR>', opts)
vim.keymap.set('n', '<A-3>', '<Cmd>BufferGoto 3<CR>', opts)
vim.keymap.set('n', '<A-4>', '<Cmd>BufferGoto 4<CR>', opts)
vim.keymap.set('n', '<A-5>', '<Cmd>BufferGoto 5<CR>', opts)
vim.keymap.set('n', '<A-6>', '<Cmd>BufferGoto 6<CR>', opts)
vim.keymap.set('n', '<A-7>', '<Cmd>BufferGoto 7<CR>', opts)
vim.keymap.set('n', '<A-8>', '<Cmd>BufferGoto 8<CR>', opts)
vim.keymap.set('n', '<A-9>', '<Cmd>BufferGoto 9<CR>', opts)
vim.keymap.set('n', '<A-0>', '<Cmd>BufferLast<CR>', opts)
-- Pin/unpin buffer
vim.keymap.set('n', '<A-p>', '<Cmd>BufferPin<CR>', opts)

vim.keymap.set('n', '<A-c>', '<Cmd>BufferClose<CR>', opts)
vim.keymap.set('n', '<C-b>', '<Cmd>BufferPick<CR>', opts)
-- Sort automatically by...
vim.keymap.set('n', '<Space>bb', '<Cmd>BufferOrderByBufferNumber<CR>', opts)
vim.keymap.set('n', '<Space>bd', '<Cmd>BufferOrderByDirectory<CR>', opts)
vim.keymap.set('n', '<Space>bl', '<Cmd>BufferOrderByLanguage<CR>', opts)
vim.keymap.set('n', '<Space>bw', '<Cmd>BufferOrderByWindowNumber<CR>', opts)

-- fuzzy motion
vim.keymap.set('n', '<leader>s', '<Cmd>FuzzyMotion<CR>', { noremap = true })
-- vim.cmd[[imap <silent><script><expr> <C-J> copilot#Accept("\<CR>")]]
vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle)
vim.keymap.set("n", "<leader>f", vim.lsp.buf.format)
vim.keymap.set("i", "<C-c>", "<Esc>")
