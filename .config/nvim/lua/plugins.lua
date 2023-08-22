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
    { "folke/which-key.nvim", lazy = true },
    "soramugi/auto-ctags.vim",
    {
        'norcalli/nvim-colorizer.lua',
        config = true
    },
    {
        'taro0079/path_to_clipboard'
    },
    {
        'RRethy/nvim-base16',
        config = function()
            vim.cmd([[colorscheme base16-gruvbox-dark-pale]])
        end
    },
    {
        'Matsuuu/pinkmare',
        config = function()
            -- vim.cmd([[colorscheme pinkmare]])
        end
    },
    {
        "stevearc/aerial.nvim",
        config = function()
            require("plugins.aerial")
        end
    },
    {
        "rgroli/other.nvim",
        event = "BufEnter",
        config = function()
            require("plugins.other")
        end
    },
    {
        "andymass/vim-matchup",
        config = function()
            -- may set any options here
            vim.g.matchup_matchparen_offscreen = { method = "popup" }
        end
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
        'dinhhuy258/git.nvim',
        config = function()
            require('plugins.git')
        end
    },
    {
        'NeogitOrg/neogit',
        config = function()
            require('plugins.neogit')
        end

    },
    {
        'yuki-yano/fuzzy-motion.vim',
        dependencies = "vim-denops/denops.vim"
    },
    {
        'johmsalas/text-case.nvim',
        config = true
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
        end
    },
    {
        "RRethy/nvim-treesitter-endwise",
        dependencies = {
            "nvim-treesitter/nvim-treesitter",

        }
    },
    { 'onsails/lspkind-nvim',   dependencies = "hrsh7th/nvim-cmp" },
    {
        'folke/tokyonight.nvim',
        lazy = false,
        priority = 1000,
        config = function()
            require("tokyonight").setup({
                transparent = false,
                styles = {
                    comments = { italic = true },
                    keywords = { italic = true },
                    -- sidebars = "transparent",
                    -- floats = "transparent"
                }
            })
            -- vim.cmd([[colorscheme tokyonight]])
        end
    },
    {
        "dstein64/vim-startuptime",
        -- lazy-load on a command
        cmd = "StartupTime",
    },
    {
        "github/copilot.vim",
    },
    {
        "hrsh7th/nvim-cmp",
        -- load cmp on InsertEnter
        event = "InsertEnter",
        -- these dependencies will only be loaded when cmp loads
        -- dependencies are always lazy-loaded unless specified otherwise
        dependencies = {
            "hrsh7th/cmp-nvim-lsp",
            "hrsh7th/cmp-buffer",
            'hrsh7th/cmp-calc',
            'hrsh7th/cmp-omni',
            'hrsh7th/cmp-nvim-lsp-signature-help',
            'hrsh7th/cmp-nvim-lsp-document-symbol',
            'hrsh7th/cmp-emoji',
            'hrsh7th/cmp-path',
            'hrsh7th/cmp-cmdline',
            'f3fora/cmp-spell',
            'hrsh7th/cmp-copilot',
            'chrisgrieser/cmp-nerdfont',
            'saadparwaiz1/cmp_luasnip'
            -- 'uga-rosa/cmp-dictionary', config = function() require 'plugins.dictionary' end } #TODO
        },
        config = function()
            require 'plugins.cmp'
            require 'plugins.luasnip'
        end,
    },
    {
        "folke/trouble.nvim",
        dependencies = "nvim-tree/nvim-web-devicons",
        config = function()
            require("trouble").setup {

            }
        end
    },
    {
        "numToStr/Navigator.nvim",
        config = function() require("plugins.navigator") end
    },
    {
        'romgrk/barbar.nvim',
        dependencies = 'nvim-tree/nvim-web-devicons',
        version = '^1.0.0',
        config = function()
            require("barbar").setup {
                icons = {
                    button = "x",
                    buffer_number = true,
                    preset = "powerline"
                }
            }
        end
    },

    {
        'easymotion/vim-easymotion'
    },
    {
        'koenverburg/peepsight.nvim',
        config = function()
            require("plugins.peepsight")
        end,
        dependencies = 'nvim-treesitter/nvim-treesitter'
    },

    {
        "hoob3rt/lualine.nvim",
        config = function()
            require("plugins.lualine")
        end,
    },
    {
        "nvim-telescope/telescope.nvim",
        -- lazy=true,
        dependencies = { "nvim-lua/plenary.nvim" },
        config = function()
            require("plugins.telescope")
        end,
    },
    { "stevearc/dressing.nvim", event = "VeryLazy" },

    {
        "windwp/nvim-ts-autotag",
        config = function() require "plugins.nvim-ts-autotag" end,
        event = "InsertEnter",
        ft = { "html", "javascript", "javascriptreact", "typescriptreact", "svelte", "vue", "markdown" }
    },
    {
        "akinsho/toggleterm.nvim",
        config = function()
            require("plugins.toggleterm")
        end,
    },
    {
        "numToStr/Comment.nvim",
        config = function()
            require("Comment").setup()
        end,
        event = "BufEnter"
    },
    {
        "windwp/nvim-autopairs",
        config = function()
            require("nvim-autopairs").setup()
        end,
        event = "InsertEnter",
    },
    { "tpope/vim-repeat" },
    {
        "vim-skk/skkeleton",
        dependencies = { "vim-denops/denops.vim" },
        config = function()
            require("plugins.skkeleton")
        end,
    },
    {
        "delphinus/skkeleton_indicator.nvim",
        config = function()
            require("skkeleton_indicator").setup({})
        end,
    },
    {
        "lukas-reineke/indent-blankline.nvim",
        config = function()
            require("plugins.indent-blankline")
        end,
    },
    { "haya14busa/vim-edgemotion", event = "BufEnter" },
    { "t9md/vim-quickhl",          event = "BufEnter" },
    { "junegunn/vim-easy-align",   event = "BufEnter" },
    {
        "petertriho/nvim-scrollbar",
        config = function()
            require("scrollbar").setup({})
        end,
    },
    {
        "ziontee113/icon-picker.nvim",
        event = "BufEnter",
        config = function()
            require("icon-picker")
        end,
    },
    {
        'nvim-tree/nvim-tree.lua',
        config = function()
            require("plugins.nvim-tree")
        end
    },

    -- git
    {
        'sindrets/diffview.nvim',
        dependencies = { "nvim-lua/plenary.nvim" },
    },
    {
        'lewis6991/gitsigns.nvim',
        event = "BufEnter",
        config = function()
            require("plugins.gitsign")
        end,
    },
    { "kdheepak/lazygit.nvim",    cmd = "LazyGit" },
    {
        "akinsho/git-conflict.nvim",
        config = true
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
    {
        "L3MON4D3/LuaSnip",
        -- follow latest release.
        version = "2.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
        -- install jsregexp (optional!).
        build = "make install_jsregexp"
    },
    { "honza/vim-snippets",       event = "InsertEnter" },
    -- { "previm/previm",            event = "BufEnter" }, -- markdown のプレビュープラグイン

    -- nvim-lsp
    -- { 'ray-x/lsp_signature.nvim', config = function() require "lsp_signature".setup() end },
    { 'williamboman/mason.nvim',  dependencies = { 'williamboman/mason-lspconfig.nvim' } },
    { 'junegunn/vim-emoji' },
    { 'folke/lsp-colors.nvim' },
    { 'RRethy/vim-illuminate',    event = "BufEnter" },
    {
        'jose-elias-alvarez/null-ls.nvim',
        config = function() require 'plugins.null-ls' end,
        dependencies = { "nvim-lua/plenary.nvim" }
    },
    { 'neovim/nvim-lspconfig', config = function() require 'plugins.lsp' end },
    -- { 'MunifTanjim/prettier.nvim', config = function() require 'plugins.prettier' end, event = "BufEnter" },
    { 'rinx/cmp-skkeleton',    dependencies = { 'nvim-cmp', 'skkeleton' },   event = "InsertEnter" },

    {
        "monaqa/dial.nvim", event = "BufEnter", config = function() require('plugins.dial') end
    },
    {
        'mattn/emmet-vim'
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
        'tpope/vim-fugitive',
        -- event = "InsertEnter",
    },
    {
        'tom-anders/telescope-vim-bookmarks.nvim'
    },
    -- bookmarks
    {
        'tomasky/bookmarks.nvim',
        event = "VimEnter",
        config = function()
            require("plugins.bookmarks")
        end
    },
    {
        'MattesGroeger/vim-bookmarks',
    },
    {
        'mfussenegger/nvim-dap',
        -- config = function() require('plugins.dap') end
    },
    { 'nvim-telescope/telescope-dap.nvim' },
    -- { 'mfussenegger/nvim-dap-python' },
    {
        "jay-babu/mason-nvim-dap.nvim",
        config = function() require('plugins.mason-nvim-dap') end,
        dependencies = {
            "williamboman/mason-lspconfig.nvim" }
    },
    { 'rcarriga/nvim-dap-ui',             config = function() require('plugins.nvim-dap-ui') end },
    {
        "ojroques/nvim-osc52",
        config = function()
            require("osc52").setup({})
            vim.keymap.set("n", "<leader>c", require("osc52").copy_operator, { expr = true })
            vim.keymap.set("n", "<leader>cc", "<leader>c_", { remap = true })
            vim.keymap.set("x", "<leader>c", require("osc52").copy_visual)
        end,
    },
    {
        'phaazon/hop.nvim',
        branch = 'v2', -- optional but strongly recommended
        config = function()
            -- you can configure Hop the way you like here; see :h hop-config
            require 'hop'.setup { keys = 'etovxqpdygfblzhckisuran' }
        end
    },
    {
        'tpope/vim-surround'
    },
    {
        'thinca/vim-qfhl'
    },
})
