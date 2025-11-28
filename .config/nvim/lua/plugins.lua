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
    -- color schemas
    {
        "slugbyte/lackluster.nvim",
        lazy = false,
        priority = 1000,
        init = function()
            -- vim.cmd.colorscheme("wildcharm")
            -- vim.cmd.colorscheme("terafox") -- my favorite
            -- vim.cmd.colorscheme("lackluster-mint")
        end,
    },
    {
        "nyoom-engineering/oxocarbon.nvim",
        config = function()
            -- vim.cmd('colorscheme oxocarbon')
        end
    },
    {
        "abecodes/tabout.nvim",
        config = function()
            require('tabout').setup {
                tabkey = '<Tab>', -- key to trigger tabout, set to an empty string to disable
                backwards_tabkey = '<S-Tab>',
                act_as_tab = true,
                enable_backwards = true,
                completion = true,
                tabouts = {
                    { open = '(', close = ')' },
                    { open = '[', close = ']' },
                    { open = '{', close = '}' },
                    { open = '"', close = '"' },
                    { open = "'", close = "'" },
                    { open = '`', close = '`' },
                },
            }
        end
    },
    {
        "zenbones-theme/zenbones.nvim",
        dependencies = "rktjmp/lush.nvim",
        config = function()
            -- vim.cmd('colorscheme zenbones')
        end
    },
    {
        "EdenEast/nightfox.nvim",
        config = function()
            require("plugins.nightfox")
            -- vim.cmd('colorscheme nightfox')
        end
    }, -- lazy
    {
        'Mofiqul/dracula.nvim',
        config = function()
        end
    },
    {
        "coder/claudecode.nvim",
        config = true,
        keys = {
            { "<leader>ac", "<cmd>ClaudeCode<cr>", desc = "Claude Code" },
        }

    },
    {
        'nvim-lualine/lualine.nvim',
        dependencies = { 'nvim-tree/nvim-web-devicons' },
        config = function()
            require("plugins.lualine")
        end

    },
    {
        "rose-pine/neovim",
        name = "rose-pine",
        config = function()
            vim.cmd [[colorscheme rose-pine]]
            require("plugins.rose-pine")
        end
    },
    {
        'projekt0n/github-nvim-theme',
        config = function()
            -- vim.cmd('colorscheme github_light_tritanopia')
        end
    },
    {
        'wolverian/minimal',
        config = function()
            -- vim.cmd('colorscheme minimal')
        end

    },
    {
        "sotte/presenting.nvim",
        opts = {
            -- fill in your options here
            -- see :help Presenting.config
        },
        cmd = { "Presenting" },
    },
    {
        "lunarvim/horizon.nvim",
        -- config = function()
        --     vim.cmd [[colorscheme horizon]]
        -- end
    },
    {
        'shmerl/neogotham',
        lazy = false,    -- to make sure it's loaded on startup
        priority = 1000, -- to load before other plugins
        config = function()
            -- vim.cmd.colorscheme("neogotham")
        end
    },
    {
        "folke/tokyonight.nvim",
        lazy = false,
        priority = 1000,
        opts = {},
        config = function()
            -- require("plugins.tokyonight")
            -- vim.cmd [[colorscheme tokyonight]]
        end,
    },

    {
        "sainnhe/gruvbox-material",
        config = function()
            -- vim.cmd [[colorscheme gruvbox-material]]
        end
    },
    {
        "craftzdog/solarized-osaka.nvim",
        lazy = false,
        config = function()
            -- vim.cmd('colorscheme solarized-osaka')
        end,
        priority = 1000,
        opts = {},
    },
    {
        'vim-denops/denops.vim'

    },
    {
        'terryma/vim-expand-region'
    },
    {
        'lambdalisue/vim-gin'
    },
    {
        'VonHeikemen/lsp-zero.nvim',
        branch = 'v3.x',
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
        "xero/miasma.nvim",
        lazy = false,
        priority = 1000,
        config = function()
            -- vim.cmd("colorscheme miasma")
        end,
    },
    {
        "dgox16/oldworld.nvim",
        lazy = false,
        priority = 1000,
        config = function()
            -- vim.cmd [[colorscheme oldworld]]
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
    -- {
    --     "nvim-treesitter/nvim-treesitter-textobjects",
    --     dependencies = {
    --         "nvim-treesitter/nvim-treesitter",
    --     }
    --
    -- },
    {
        'tpope/vim-fugitive',
        event = "BufEnter"

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
        "RRethy/nvim-treesitter-endwise",
        dependencies = {
            "nvim-treesitter/nvim-treesitter",

        },
        event = "InsertEnter",
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
        'easymotion/vim-easymotion',
        event = "BufEnter"

    },
    {
        "smartpde/telescope-recent-files",
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
    {
        "folke/flash.nvim",
        event = "VeryLazy",
        ---@type Flash.Config
        opts = {},
        keys = {
            { "s",     mode = { "n", "x", "o" }, function() require("flash").jump() end,              desc = "Flash" },
            { "S",     mode = { "n", "x", "o" }, function() require("flash").treesitter() end,        desc = "Flash Treesitter" },
            { "r",     mode = "o",               function() require("flash").remote() end,            desc = "Remote Flash" },
            { "R",     mode = { "o", "x" },      function() require("flash").treesitter_search() end, desc = "Treesitter Search" },
            { "<c-s>", mode = { "c" },           function() require("flash").toggle() end,            desc = "Toggle Flash Search" },
        },
    },
    {
        'tpope/vim-surround',
        event = "InsertEnter",
    },
    -- {
    --     'kenn7/vim-arsync',
    --     dependencies = {
    --         { 'prabirshrestha/async.vim' }
    --     },
    --     event = "VeryLazy",
    -- },
    -- {
    --     "mbbill/undotree"
    -- },
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
    { 'akinsho/toggleterm.nvim', version = "*",     config = function() require('plugins.toggleterm') end },

    {
        "nvim-neotest/neotest",
        -- lazy = true,
        dependencies = {
            "nvim-lua/plenary.nvim",
            "antoinemadec/FixCursorHold.nvim",
            "nvim-treesitter/nvim-treesitter",
            "nvim-neotest/nvim-nio",
            "olimorris/neotest-phpunit",
            -- "praem90/neotest-docker-phpunit.nvim"
        },
        config = function()
            -- local file_exists = require("helper.file_exist")
            require("neotest").setup({
                adapters = {
                    require("neotest-phpunit")({
                        phpunit_cmd = function()
                            -- return "symfony php bin/phpunit"
                            -- if file_exist.file_exist("bin/phpunit") then
                            --     return vim.tbl_flatten({
                            --         "docker",
                            --         "compose",
                            --         "-f",
                            --         "../docker-compose.yml",
                            --         "run",
                            --         "--rm",
                            --         "devcontainer",
                            --         "symfony",
                            --         "php",
                            --         "bin/phpunit",
                            --     })
                            -- elseif file_exist.file_exist("vendor/bin/phpunit") then
                            return "vendor/bin/phpunit"
                            -- end
                            -- "docker exec oms-dev-docker-oms-backend-1 symfony php bin/phpunit"
                        end,
                    })
                },
            })
        end
    },
    {
        'tpope/vim-dadbod'

    },
    {
        "aerosol/dumbotron.vim",
        config = function()
            -- vim.cmd [[colorscheme dumbotron]]
        end
    },
    {
        "vhyrro/luarocks.nvim",
        priority = 1000,
        config = true,

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
        "folke/lazydev.nvim",
        ft = "lua", -- only load on lua files
        opts = {
            library = {
                -- See the configuration section for more details
                -- Load luvit types when the `vim.uv` word is found
                { path = "luvit-meta/library", words = { "vim%.uv" } },
            },
        },
    },
    -- {
    --     "nvim-neorg/neorg",
    --     lazy = false,
    --     version = "*",
    --     config = function()
    --         require('neorg').setup {
    --             load = {
    --                 ["core.defaults"] = {},
    --                 ["core.concealer"] = {},
    --                 ["core.dirman"] = {
    --                     config = {
    --                         workspaces = {
    --                             notes = "~/neorg",
    --                         },
    --                         default_workspace = "notes"
    --                     },
    --                 }
    --             }
    --         }
    --         vim.wo.foldlevel = 99
    --         vim.wo.conceallevel = 2
    --     end
    -- },
    {
        'stevearc/oil.nvim',
        ---@module 'oil'
        ---@type oil.SetupOpts
        opts = {},
        config = function()
            require('oil').setup({
                keymaps = {
                    ['--'] = "actions.parent"
                }
            })
        end,
        -- Optional dependencies
        dependencies = { { "echasnovski/mini.icons", opts = {} } },
    },
    -- {
    --     "folke/noice.nvim",
    --     event = "VeryLazy",
    --     opts = {
    --         -- add any options here
    --     },
    --     dependencies = {
    --         -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
    --         "MunifTanjim/nui.nvim",
    --         -- OPTIONAL:
    --         --   `nvim-notify` is only needed, if you want to use the notification view.
    --         --   If not available, we use `mini` as the fallback
    --         "rcarriga/nvim-notify",
    --     }
    -- }
    {
        "catppuccin/nvim",
        name = "catppuccin",
        priority = 1000,
        config = function()
            -- vim.cmd [[colorscheme catppuccin]]
        end
    },
    {
        "chentoast/marks.nvim",
        event = "VeryLazy",
        opts = {},
    }

})

-- require("myplugin.kurodake-green").setup({

-- })
-- vim.cmd.colorscheme("kurodake-green")
-- vim.cmd [[colorscheme github_dark_default]]
-- local colorschemes = { "nightfox", "rose-pine", "tokyonight", "catppuccin", "github_light_tritanopia", "neogotham", "horizon", "miasma", "kanagawa", "wildcharm", "eldritch" }
-- local ramdom_number = math.random(#colorschemes)
-- local selected_colorscheme = colorschemes[ramdom_number]
-- vim.cmd("colorscheme " .. selected_colorscheme)
