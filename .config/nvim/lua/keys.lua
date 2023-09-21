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

-- place this in one of your configuration file(s)
-- local hop = require('hop')
-- local directions = require('hop.hint').HintDirection
-- vim.keymap.set('', 'f', function()
--   hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = false })
-- end, {remap=true})
-- vim.keymap.set('', 'F', function()
--   hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = false })
-- end, {remap=true})
-- vim.keymap.set('', 't', function()
--   hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = false, hint_offset = -1 })
-- end, {remap=true})
-- vim.keymap.set('', 'T', function()
--   hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = false, hint_offset = 1 })
-- end, {remap=true})

vim.keymap.set("x", "ga", "<Plug>(EasyAlign)")
vim.keymap.set("n", "ga", "<Plug>(EasyAlign)")
-- dial
-- pounce
-- vim.keymap.set("n", "s","<cmd>Pounce<CR>", {noremap = true})
-- vim.keymap.set("n", "S","<cmd>PounceRepeat<CR>", {noremap = true})
-- vim.keymap.set("v", "s","<cmd>Pounce<CR>", {noremap = true})
-- vim.keymap.set("o", "gs","<cmd>Pounce<CR>", {noremap = true})


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

local function on_attach(bufnr)
    local api = require('nvim-tree.api')

    local function opts(desc)
        return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
    end


    -- Default mappings. Feel free to modify or remove as you wish.
    --
    -- BEGIN_DEFAULT_ON_ATTACH
    vim.keymap.set('n', '<C-]>', api.tree.change_root_to_node, opts('CD'))
    vim.keymap.set('n', '<C-e>', api.node.open.replace_tree_buffer, opts('Open: In Place'))
    vim.keymap.set('n', '<C-k>', api.node.show_info_popup, opts('Info'))
    vim.keymap.set('n', '<C-r>', api.fs.rename_sub, opts('Rename: Omit Filename'))
    vim.keymap.set('n', '<C-t>', api.node.open.tab, opts('Open: New Tab'))
    vim.keymap.set('n', '<C-v>', api.node.open.vertical, opts('Open: Vertical Split'))
    vim.keymap.set('n', '<C-x>', api.node.open.horizontal, opts('Open: Horizontal Split'))
    vim.keymap.set('n', '<BS>', api.node.navigate.parent_close, opts('Close Directory'))
    vim.keymap.set('n', '<CR>', api.node.open.edit, opts('Open'))
    vim.keymap.set('n', '<Tab>', api.node.open.preview, opts('Open Preview'))
    vim.keymap.set('n', '>', api.node.navigate.sibling.next, opts('Next Sibling'))
    vim.keymap.set('n', '<', api.node.navigate.sibling.prev, opts('Previous Sibling'))
    vim.keymap.set('n', '.', api.node.run.cmd, opts('Run Command'))
    vim.keymap.set('n', '-', api.tree.change_root_to_parent, opts('Up'))
    vim.keymap.set('n', 'a', api.fs.create, opts('Create'))
    vim.keymap.set('n', 'bmv', api.marks.bulk.move, opts('Move Bookmarked'))
    vim.keymap.set('n', 'B', api.tree.toggle_no_buffer_filter, opts('Toggle No Buffer'))
    vim.keymap.set('n', 'c', api.fs.copy.node, opts('Copy'))
    vim.keymap.set('n', 'C', api.tree.toggle_git_clean_filter, opts('Toggle Git Clean'))
    vim.keymap.set('n', '[c', api.node.navigate.git.prev, opts('Prev Git'))
    vim.keymap.set('n', ']c', api.node.navigate.git.next, opts('Next Git'))
    vim.keymap.set('n', 'd', api.fs.remove, opts('Delete'))
    vim.keymap.set('n', 'D', api.fs.trash, opts('Trash'))
    vim.keymap.set('n', 'E', api.tree.expand_all, opts('Expand All'))
    vim.keymap.set('n', 'e', api.fs.rename_basename, opts('Rename: Basename'))
    vim.keymap.set('n', ']e', api.node.navigate.diagnostics.next, opts('Next Diagnostic'))
    vim.keymap.set('n', '[e', api.node.navigate.diagnostics.prev, opts('Prev Diagnostic'))
    vim.keymap.set('n', 'F', api.live_filter.clear, opts('Clean Filter'))
    vim.keymap.set('n', 'f', api.live_filter.start, opts('Filter'))
    vim.keymap.set('n', 'g?', api.tree.toggle_help, opts('Help'))
    vim.keymap.set('n', 'gy', api.fs.copy.absolute_path, opts('Copy Absolute Path'))
    vim.keymap.set('n', 'H', api.tree.toggle_hidden_filter, opts('Toggle Dotfiles'))
    vim.keymap.set('n', 'I', api.tree.toggle_gitignore_filter, opts('Toggle Git Ignore'))
    vim.keymap.set('n', 'J', api.node.navigate.sibling.last, opts('Last Sibling'))
    vim.keymap.set('n', 'K', api.node.navigate.sibling.first, opts('First Sibling'))
    vim.keymap.set('n', 'm', api.marks.toggle, opts('Toggle Bookmark'))
    vim.keymap.set('n', 'o', api.node.open.edit, opts('Open'))
    vim.keymap.set('n', 'O', api.node.open.no_window_picker, opts('Open: No Window Picker'))
    vim.keymap.set('n', 'p', api.fs.paste, opts('Paste'))
    vim.keymap.set('n', 'P', api.node.navigate.parent, opts('Parent Directory'))
    vim.keymap.set('n', 'q', api.tree.close, opts('Close'))
    vim.keymap.set('n', 'r', api.fs.rename, opts('Rename'))
    vim.keymap.set('n', 'R', api.tree.reload, opts('Refresh'))
    vim.keymap.set('n', 's', api.node.run.system, opts('Run System'))
    vim.keymap.set('n', 'S', api.tree.search_node, opts('Search'))
    vim.keymap.set('n', 'U', api.tree.toggle_custom_filter, opts('Toggle Hidden'))
    vim.keymap.set('n', 'W', api.tree.collapse_all, opts('Collapse'))
    vim.keymap.set('n', 'x', api.fs.cut, opts('Cut'))
    vim.keymap.set('n', 'y', api.fs.copy.filename, opts('Copy Name'))
    vim.keymap.set('n', 'Y', api.fs.copy.relative_path, opts('Copy Relative Path'))
    vim.keymap.set('n', '<2-LeftMouse>', api.node.open.edit, opts('Open'))
    vim.keymap.set('n', '<2-RightMouse>', api.tree.change_root_to_node, opts('CD'))
    -- END_DEFAULT_ON_ATTACH


    -- Mappings migrated from view.mappings.list
    --
    -- You will need to insert "your code goes here" for any mappings with a custom action_cb
    vim.keymap.set('n', 'u', api.tree.change_root_to_parent, opts('Up'))
end

vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle)
