local file_exist = require "helper.file_exist"
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
    -- {
    --     "folke/which-key.nvim",
    --     event = "VeryLazy",
    --     init = function()
    --         vim.o.timeout = true
    --         vim.o.timeoutlen = 300
    --     end,
    --     opts = {
    --         -- your configuration comes here
    --         -- or leave it empty to use the default settings
    --         -- refer to the configuration section below
    --     }
    -- },
    {
        "EdenEast/nightfox.nvim",
        config = function()
            require("plugins.nightfox")
            vim.cmd('colorscheme nightfox')
        end
    }, -- lazy
    {
        'VonHeikemen/lsp-zero.nvim',
        branch = 'v1.x',
        dependencies = {
            -- LSP Support
            { 'neovim/nvim-lspconfig' },
            { 'williamboman/mason.nvim' },
            { 'williamboman/mason-lspconfig.nvim' },

            -- Autocompletion
            { 'hrsh7th/nvim-cmp' },
            { 'hrsh7th/cmp-buffer' },
            { 'hrsh7th/cmp-cmdline' },
            { 'hrsh7th/cmp-path' },
            { 'saadparwaiz1/cmp_luasnip' },
            { 'hrsh7th/cmp-nvim-lsp' },
            { 'hrsh7th/cmp-nvim-lua' },

            -- Snippets
            { 'L3MON4D3/LuaSnip' },
            -- { 'rafamadriz/friendly-snippets' },
        },
        config = function()
            require('plugins.lsp')
        end
    },
    {
        'taro0079/path_to_clipboard',
        event = "BufEnter",
    },
    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        event = "BufEnter",
        config = function()
            require("plugins.tree-sitter")
        end,
    },
    {
        'tpope/vim-fugitive',
        event = "BufEnter"

    },
    {
        "iamcco/markdown-preview.nvim",
        cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
        ft = { "markdown" },
        build = function() vim.fn["mkdp#util#install"]() end,
    },
    {
        'nvim-treesitter/nvim-treesitter-context',
        config = function()
            require("plugins.treesitter-context")
        end,
        dependencies = {
            "nvim-treesitter/nvim-treesitter",

        }
    },
    -- whitespaceを消してくれるやつ
    {
        'cappyzawa/trim.nvim',
        config = function()
            require('plugins.trim')
        end,
        event = "InsertEnter",
    },
    {
        url = "https://codeberg.org/jthvai/lavender.nvim",
        branch = "stable", -- versioned tags + docs updates from main
        lazy = false,
        priority = 1000,
        config = function()
            vim.g.lavender = {
                transparent = {
                    background = true,  -- do not render the main background
                    float      = true,  -- do not render the background in floating windows
                    popup      = false, -- do not render the background in popup menus
                    sidebar    = true,  -- do not render the background in sidebars
                },
                signs = true,
            }
            -- vim.cmd("colorscheme lavender")
        end
    },

    {
        "RRethy/nvim-treesitter-endwise",
        dependencies = {
            "nvim-treesitter/nvim-treesitter",

        },
        event = "InsertEnter",
    },
    -- {
    --     'rose-pine/neovim',
    --     name = 'rose-pine',
    --     config = function()
    --         require("plugins.rose-pine")
    --     end
    -- },
    --
    {
        "lunarvim/horizon.nvim",
        -- config = function()
        --     vim.cmd [[colorscheme horizon]]
        -- end
    },
    {
        'projekt0n/github-nvim-theme',
        lazy = false,    -- make sure we load this during startup if it is your main colorscheme
        priority = 1000, -- make sure to load this before all the other start plugins
        config = function()
            require('github-theme').setup({
                -- ...
            })

            -- vim.cmd('colorscheme github_dark_tritanopia')
        end,
    },
    {
        "folke/trouble.nvim",
        dependencies = "nvim-tree/nvim-web-devicons",
        config = function()
            require("trouble").setup {

            }
        end,
        cmd = "Trouble"
    },
    {
        'nvim-lualine/lualine.nvim',
        dependencies = { 'nvim-tree/nvim-web-devicons' },
        config = function()
            require('plugins.lualine')
        end

    },
    {
        'easymotion/vim-easymotion',
        event = "BufEnter"

    },
    {
        "nvim-telescope/telescope.nvim",
        -- lazy=true,
        dependencies = { "nvim-lua/plenary.nvim" },
        config = function()
            require("plugins.telescope")
        end,
    },
    { "stevearc/dressing.nvim",  event = "VeryLazy" },

    {
        "windwp/nvim-ts-autotag",
        config = function() require "plugins.nvim-ts-autotag" end,
        event = "InsertEnter",
        ft = { "html", "javascript", "javascriptreact", "typescriptreact", "svelte", "vue", "markdown" }
    },
    {
        "numToStr/Comment.nvim",
        config = function()
            require("Comment").setup({
                pre_hook = require('ts_context_commentstring.integrations.comment_nvim').create_pre_hook(),
            })
        end,
    },
    {
        'JoosepAlviste/nvim-ts-context-commentstring',
    },
    {
        "windwp/nvim-autopairs",
        config = function()
            require("nvim-autopairs").setup()
        end,
        event = "InsertEnter",
    },
    {
        "tpope/vim-repeat",
        event = "BufEnter"
    },
    -- { "t9md/vim-quickhl",          event = "BufEnter" },
    { "junegunn/vim-easy-align", event = "BufEnter" },
    {
        "ziontee113/icon-picker.nvim",
        event = "BufEnter",
        config = function()
            require("icon-picker")
        end,
        event = "InsertEnter"
    },
    {
        'nyoom-engineering/oxocarbon.nvim',
        -- config = function()
        --     vim.opt.background = "dark" -- set this to dark or light
        --     vim.cmd("colorscheme oxocarbon")
        -- end
    },
    {
        'nvim-tree/nvim-tree.lua',
        config = function()
            require("plugins.nvim-tree")
        end,
        event = "VeryLazy"
    },

    {
        'lewis6991/gitsigns.nvim',
        event = "BufEnter",
        config = function()
            require("plugins.gitsign")
        end,
    },
    {
        "j-hui/fidget.nvim",
        tag = "legacy",
        event = "LspAttach",
        opts = {
            -- options
        },
    },
    -- snippets
    -- { "SirVer/ultisnips", },
    -- { "honza/vim-snippets", },

    -- {
    --     'junegunn/vim-emoji',
    --     event = "BufEnter",
    -- },
    {
        'folke/lsp-colors.nvim',
        event = "BufEnter",
    },
    { 'RRethy/vim-illuminate',   event = "BufEnter" },
    -- {
    --     'jose-elias-alvarez/null-ls.nvim',
    --     config = function() require 'plugins.null-ls' end,
    --     dependencies = { "nvim-lua/plenary.nvim" }
    -- },
    -- { 'rinx/cmp-skkeleton',    dependencies = { 'nvim-cmp', 'skkeleton' }, event = "InsertEnter" },
    -- {
    --     'Mofiqul/dracula.nvim',
    --     config = function()
    --         vim.cmd('colorscheme rose-pine-moon')
    --     end
    -- },
    -- {
    --     'rose-pine/neovim',
    --     as = 'rose-pine',
    --     -- config = function()
    --     --     vim.cmd('colorscheme rose-pine-dawn')
    --     -- end
    -- },

    {
        'mattn/emmet-vim',
        event = "InsertEnter"
    },
    {
        "kylechui/nvim-surround",
        version = "*", -- Use for stability; omit to use `main` branch for the latest features
        event = "VeryLazy",
        config = function()
            require("nvim-surround").setup({
                -- Configuration here, or leave empty to use defaults
            })
        end
    },
    {
        "nvim-treesitter/nvim-treesitter-textobjects",
        dependencies = "nvim-treesitter/nvim-treesitter",
    },
    -- bookmarks
    -- {
    --     'tomasky/bookmarks.nvim',
    --     event = "VimEnter",
    --     config = function()
    --         require("plugins.bookmarks")
    --     end
    -- },
    {
        'justinmk/vim-sneak',
        event = "BufEnter",
        config = function()
            vim.cmd [[
            let g:sneak#label = 1
            ]]
        end
    },
    {
        'tpope/vim-surround',
        event = "InsertEnter",
    },
    {
        'kenn7/vim-arsync',
        dependencies = {
            { 'prabirshrestha/async.vim' }
        },
        event = "VeryLazy",
    },
    {
        "mbbill/undotree"
    },
    {
        "github/copilot.vim"
    },
    {
        "theprimeagen/harpoon",
        config = function()
            require("plugins.harpoon")
        end
    },
    {
        'vim-skk/eskk.vim',
        config = function()
            vim.cmd [[let g:eskk#directory        = "~/.config/eskk"]]
            vim.cmd [[let g:eskk#dictionary       = { 'path': "~/.config/eskk/my_jisyo", 'sorted': 1, 'encoding': 'utf-8',}]]
            vim.cmd [[let g:eskk#large_dictionary = {'path': "~/.config/eskk/SKK-JISYO.L", 'sorted': 1, 'encoding': 'euc-jp',}]]
        end

    },
    -- {
    --     "craftzdog/solarized-osaka.nvim",
    --     lazy = false,
    --     config = function()
    --         vim.cmd('colorscheme solarized-osaka')
    --     end,
    --     priority = 1000,
    --     opts = {},
    -- },
    {
        'ta-tikoma/php.easy.nvim',
        ft = "php",
        config = true,
        keys = {
            { '-b',   '<CMD>PHPEasyDocBlock<CR>' },
            { '-r',   '<CMD>PHPEasyReplica<CR>' },
            { '-c',   '<CMD>PHPEasyCopy<CR>' },
            { '-d',   '<CMD>PHPEasyDelete<CR>' },
            { '-ii',  '<CMD>PHPEasyInitInterface<CR>' },
            { '-ic',  '<CMD>PHPEasyInitClass<CR>' },
            { '-iac', '<CMD>PHPEasyInitAbstractClass<CR>' },
            { '-it',  '<CMD>PHPEasyInitTrait<CR>' },
            { '-ie',  '<CMD>PHPEasyInitEnum<CR>' },
            { '-ac',  '<CMD>PHPEasyAppendConstruct<CR>' },
            -- { '-ac',  '<CMD>PHPEasyAppendConstant<CR>' },
            { '-ap',  '<CMD>PHPEasyAppendProperty<CR>' },
            { '-am',  '<CMD>PHPEasyAppendMethod<CR>' },
            { '-aa',  '<CMD>PHPEasyAppendArgument<CR>' },
        }
    },
    -- {
    --     -- lazy
    --     "sontungexpt/witch",
    --     priority = 1000,
    --     lazy = false,
    --     config = function()
    --         require("plugins.witch")
    --     end
    -- },
    {
        "stevearc/conform.nvim",
        config = function()
            require("plugins.conform")
        end,
        event = { "BufWritePre" },
        cmd = { "ConformInfo" }
    },
    {
        "mfussenegger/nvim-lint",
        event = {
            "BufReadPre",
            "BufNewFile"
        },
        config = function()
            require("plugins.nvim-lint")
        end
    },
    -- {
    --     "rshkarin/mason-nvim-lint",
    --     config = function()
    --         require("mason-nvim-lint").setup()
    --     end
    -- },
    { 'akinsho/toggleterm.nvim', version = "*",     config = function() require('plugins.toggleterm') end },

    {
        "nvim-neotest/neotest",
        lazy = true,
        dependencies = {
            "nvim-lua/plenary.nvim",
            "antoinemadec/FixCursorHold.nvim",
            "nvim-treesitter/nvim-treesitter",
            "nvim-neotest/nvim-nio",
            "olimorris/neotest-phpunit",
            "praem90/neotest-docker-phpunit.nvim"
        },
        config = function()
            local file_exists = require("helper.file_exist")
            require("neotest").setup({
                adapters = {
                    -- require("neotest-phpunit")({
                    --     phpunit_cmd = function()
                    --         -- return "symfony php bin/phpunit"
                    --         if file_exist.file_exist("bin/phpunit") then
                    --             return vim.tbl_flatten({
                    --                 "docker",
                    --                 "exec",
                    --                 "-it",
                    --                 "rpst-oms-backend_php-dev_1",
                    --                 "symfony",
                    --                 "php",
                    --                 "bin/phpunit",
                    --             })
                    --         elseif file_exist.file_exist("vendor/bin/phpunit") then
                    --             return "vendor/bin/phpunit"
                    --         end
                    --         -- "docker exec oms-dev-docker-oms-backend-1 symfony php bin/phpunit"
                    --     end,
                    -- })
                    require("neotest-docker-phpunit").setup({
                        phpunit_cmd = "/Users/taro_morita/dev/neotest-docker-phpunit/target/debug/neotest-docker-phpunit",
                        docker_phpunit = {
                            default = {
                                container = "rpst-oms-backend_php-dev_1",
                                volume = "/Users/taro_morita/dev/rpst-oms-backend/app/:/srv/app/",
                                standalone = true,
                            },
                        },
                    })
                },
            })
        end
    },
    {
        'tpope/vim-dadbod'

    },
    {
        "phpstan/vim-phpstan"
    },
    {
        "vhyrro/luarocks.nvim",
        priority = 1000,
        config = true,

    },
    {
        'mg979/vim-visual-multi'
    },
    -- {
    --     'tani/dmacro.nvim',
    --     config = function()
    --         require('dmacro').setup({
    --             dmacro_key = '<C-u>' --  you need to set the dmacro_key
    --         })
    --     end
    -- },
    {
        'tpope/vim-projectionist',
        config = function()
            vim.g.projectionist_heuristics = {
                ["*"] = {
                    ["app/src/*.php"] = {
                        alternate = "app/tests/Unit/{}Test.php",
                        type = "source"
                    },
                    ["app/src/*.php"] = {
                        alternate = "app/tests/Integration/{}Test.php",
                        type = "source"
                    },
                    ["app/tests/Unit/*Test.php"] = {
                        alternate = "app/src/{}.php",
                        type = "unittest"
                    },
                    ["app/tests/Integration/*Test.php"] = {
                        alternate = "app/src/{}.php",
                        type = "integration"
                    },
                },
            }
        end,
    },
    {
        "phpactor/phpactor",
        ft = "php",
        -- tag = "*",
        build = "composer install --no-dev -o",
    },
    {
        "vim-scripts/smartchr",
        config = function()
            vim.cmd [[autocmd FileType php inoremap <buffer> <expr> . smartchr#one_of('$', '->', '.')]]
        end
    },
    { 'echasnovski/mini.ai', version = false },
    {
        "rebelot/kanagawa.nvim",
        config = function()
            -- vim.cmd [[colorscheme kanagawa]]
        end
    },
    {
        "scottmckendry/cyberdream.nvim",
        lazy = false,
        priority = 1000,
        config = function()
            -- require("cyberdream").setup({
            --     transparent = true,
            -- })
            -- vim.cmd("colorscheme cyberdream")
        end

    },
    {
        "sainnhe/gruvbox-material",
        config = function()
            -- vim.cmd [[colorscheme gruvbox-material]]
        end

    }


})
