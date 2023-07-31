require('mason-nvim-dap').setup({
    ensure_installed = { 'stylua', 'jq' },
    handlers = {
        -- function(config)
        -- all sources with no handler get passed here

        -- -- Keep original functionality
        -- require('mason-nvim-dap').default_setup(config)
        -- end,
        python = function(config)
            local dap = require("dap")
            dap.adapters.python = {
                type = "executable",
                command = "/opt/homebrew/bin/python3",
                args = {
                    "-m",
                    "debugpy.adapter",
                },
            }
            require('mason-nvim-dap').default_setup(config) -- don't forget this!
        end,
        php = function(config)
            local dap = require("dap")
            dap.adapters.php = {
                type = "executable",
                command = "node",
                args = { os.getenv("HOME") .. "/.vscode-server/extensions/xdebug.php-debug-1.32.1/out/phpDebug.js" }
            }
            dap.configurations.php = {
                {
                    type = "php",
                    request = "launch",
                    name = "Listen for Xdebug",
                    port = 8008,
                }

            }
            -- require('mason-nvim-dap').default_setup(config) -- don't forget this!
        end,
    },
})
vim.keymap.set('n', '<leader>dc', '<cmd>lua require"dap".continue()<CR>')
vim.keymap.set('n', '<leader>dsv', '<cmd>lua require"dap".step_over()<CR>')
vim.keymap.set('n', '<leader>dsi', '<cmd>lua require"dap".step_into()<CR>')
vim.keymap.set('n', '<leader>dso', '<cmd>lua require"dap".step_out()<CR>')
vim.keymap.set('n', '<leader>dtb', '<cmd>lua require"dap".toggle_breakpoint()<CR>')
vim.keymap.set('n', '<leader>dsbr', '<cmd>lua require"dap".set_breakpoint(vim.fn.input("Breakpoint condition: "))<CR>')
vim.keymap.set('n', '<leader>dsbm',
    '<cmd>lua require"dap".set_breakpoint(nil, nil, vim.fn.input("Log point message: "))<CR>')
vim.keymap.set('n', '<leader>dro', '<cmd>lua require"dap".repl.open()<CR>')
vim.keymap.set('n', '<leader>drl', '<cmd>lua require"dap".repl.run_last()<CR>')

-- telescope-dap
vim.keymap.set('n', '<leader>dcc', '<cmd>lua require"telescope".extensions.dap.commands{}<CR>')
vim.keymap.set('n', '<leader>dco', '<cmd>lua require"telescope".extensions.dap.configurations{}<CR>')
vim.keymap.set('n', '<leader>dlb', '<cmd>lua require"telescope".extensions.dap.list_breakpoints{}<CR>')
vim.keymap.set('n', '<leader>dv', '<cmd>lua require"telescope".extensions.dap.variables{}<CR>')
vim.keymap.set('n', '<leader>df', '<cmd>lua require"telescope".extensions.dap.frames{}<CR>')

vim.api.nvim_set_keymap('n', '<leader>dd', ':lua require("dapui").toggle()<CR>', {})
