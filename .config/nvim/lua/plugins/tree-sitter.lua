vim.opt.runtimepath:append("~/.local/share/nvim/site/pack/packer/start/nvim-treesitter/parser")
require 'nvim-treesitter.configs'.setup {
    endwise = {
        enable = true
    },
    -- autotag = {
    --     enable = true
    -- },
    ensure_installed = { "lua", "vim", "vimdoc", "typescript", "javascript", "php", "phpdoc", "python", "css", "html", "json", "yaml" }, -- one of "all", "maintained" (parsers with maintainers), or a list of languages
    auto_install = true,
    ignore_install = {},
    indent = {
        enable = true,
        disable = {"ruby"}
    },
    yati = { enable = true },
    highlight = {
        enable = true, -- false will disable the whole extension
        disable = { 'latex' },
        additional_vim_regex_highlighting = false,
    },
    textobjects = {
        select = {
            enable = true,
            -- Automatically jump forward to textobj, similar to targets.vim
            lookahead = true,
            keymaps = {
                -- You can use the capture groups defined in textobjects.scm
                ["af"] = "@function.outer",
                ["if"] = "@function.inner",
                ["ac"] = "@class.outer",
                -- You can optionally set descriptions to the mappings (used in the desc parameter of
                -- nvim_buf_set_keymap) which plugins like which-key display
                ["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
                -- You can also use captures from other query groups like `locals.scm`
                ["as"] = { query = "@scope", query_group = "locals", desc = "Select language scope" },
            },
            -- You can choose the select mode (default is charwise 'v')
            --
            -- Can also be a function which gets passed a table with the keys
            -- * query_string: eg '@function.inner'
            -- * method: eg 'v' or 'o'
            -- and should return the mode ('v', 'V', or '<c-v>') or a table
            -- mapping query_strings to modes.
            selection_modes = {
                ['@parameter.outer'] = 'v', -- charwise
                ['@function.outer'] = 'V',  -- linewise
                ['@class.outer'] = '<c-v>', -- blockwise
            },
            -- If you set this to `true` (default is `false`) then any textobject is
            -- extended to include preceding or succeeding whitespace. Succeeding
            -- whitespace has priority in order to act similarly to eg the built-in
            -- `ap`.
            --
            -- Can also be a function which gets passed a table with the keys
            -- * query_string: eg '@function.inner'
            -- * selection_mode: eg 'v'
            -- and should return true of false
            include_surrounding_whitespace = true,
        },
    },
}
