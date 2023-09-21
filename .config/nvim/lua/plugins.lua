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
    {
        'norcalli/nvim-colorizer.lua',
        config = true,
        event = "BufEnter"
    },
    {
        'taro0079/path_to_clipboard',
        event = "BufEnter",
    },
    {
        "stevearc/aerial.nvim",
        config = function()
            require("plugins.aerial")
        end,
        event = "BufEnter"
    },
    {
        "andymass/vim-matchup",
        config = function()
            -- may set any options here
            vim.g.matchup_matchparen_offscreen = { method = "popup" }
        end,
        event = "BufEnter"
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
    { 'onsails/lspkind-nvim', dependencies = "hrsh7th/nvim-cmp" },
    -- {"quangnguyen30192/cmp-nvim-ultisnips"},
    { 'rose-pine/neovim', name = 'rose-pine',
        config = function()
            require("plugins.rose-pine")
        end
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
            -- 'hrsh7th/cmp-copilot',
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
        end,
        cmd = "Trouble"
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
    { "stevearc/dressing.nvim",    event = "VeryLazy" },

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
        event = 'BufEnter'
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
    {
        "tpope/vim-repeat",
        event = "BufEnter"
    },
    {
        "vim-skk/skkeleton",
        dependencies = { "vim-denops/denops.vim" },
        config = function()
            require("plugins.skkeleton")
        end,
        event = "BufEnter"
    },
    {
        "delphinus/skkeleton_indicator.nvim",
        config = function()
            require("skkeleton_indicator").setup({})
        end,
        event = "BufEnter"
    },
    {
        "lukas-reineke/indent-blankline.nvim",
        config = function()
            require("plugins.indent-blankline")
        end,
        event = "BufEnter"
    },
    { "t9md/vim-quickhl",          event = "BufEnter" },
    { "junegunn/vim-easy-align",   event = "BufEnter" },
    {
        "ziontee113/icon-picker.nvim",
        event = "BufEnter",
        config = function()
            require("icon-picker")
        end,
        event = "InsertEnter"
    },
    {
        'nvim-tree/nvim-tree.lua',
        config = function()
            require("plugins.nvim-tree")
        end,
        event = "VeryLazy"
    },

    -- git
    {
        'sindrets/diffview.nvim',
        dependencies = { "nvim-lua/plenary.nvim" },
        event = "BufEnter",
    },
    {
        'lewis6991/gitsigns.nvim',
        event = "BufEnter",
        config = function()
            require("plugins.gitsign")
        end,
    },
    { "kdheepak/lazygit.nvim",   cmd = "LazyGit" },
    {
        "akinsho/git-conflict.nvim",
        config = true,
        event = "BufEnter",
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
    { "honza/vim-snippets", },

    -- nvim-lsp
    { 'williamboman/mason.nvim', dependencies = { 'williamboman/mason-lspconfig.nvim' } },
    {
        'junegunn/vim-emoji',
        event = "BufEnter",
    },
    {
        'folke/lsp-colors.nvim',
        event = "BufEnter",
    },
    { 'RRethy/vim-illuminate', event = "BufEnter" },
    {
        'jose-elias-alvarez/null-ls.nvim',
        config = function() require 'plugins.null-ls' end,
        dependencies = { "nvim-lua/plenary.nvim" }
    },
    { 'neovim/nvim-lspconfig', config = function() require 'plugins.lsp' end },
    { 'rinx/cmp-skkeleton',    dependencies = { 'nvim-cmp', 'skkeleton' },   event = "InsertEnter" },

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
        'tpope/vim-fugitive',
        event = "BufEnter",
    },
    {
        'tom-anders/telescope-vim-bookmarks.nvim',
        event = "BufEnter",
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
        "ojroques/nvim-osc52",
        config = function()
            require("osc52").setup({})
            vim.keymap.set("n", "<leader>c", require("osc52").copy_operator, { expr = true })
            vim.keymap.set("n", "<leader>cc", "<leader>c_", { remap = true })
            vim.keymap.set("x", "<leader>c", require("osc52").copy_visual)
        end,
        event = "VeryLazy"
    },
    {
        'justinmk/vim-sneak',
        event = "BufEnter",
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
        "L3MON4D3/LuaSnip",
        -- follow latest release.
        version = "2.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
        -- install jsregexp (optional!).
        build = "make install_jsregexp",
        event = "BufEnter"
    },
    {
        'tpope/vim-dadbod',
        cmd = "DB"
    }
})
