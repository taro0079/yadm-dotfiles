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
