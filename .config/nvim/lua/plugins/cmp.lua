local cmp = require 'cmp'
local lspkind = require 'lspkind'
local luasnip = require("luasnip")
-- local cmp_ultisnips_mappings = require("cmp_nvim_ultisnips.mappings")
local has_words_before = function()
    unpack = unpack or table.unpack
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end
local cmp_kinds = {
    Text = '  ',
    Method = '  ',
    Function = '  ',
    Constructor = '  ',
    Field = '  ',
    Variable = '  ',
    Class = '  ',
    Interface = '  ',
    Module = '  ',
    Property = '  ',
    Unit = '  ',
    Value = '  ',
    Enum = '  ',
    Keyword = '  ',
    Snippet = '  ',
    Color = '  ',
    File = '  ',
    Reference = '  ',
    Folder = '  ',
    EnumMember = '  ',
    Constant = '  ',
    Struct = '  ',
    Event = '  ',
    Operator = '  ',
    TypeParameter = '  ',
}


cmp.setup {
    completion = { completeopt = 'menu,menuone,noselect' },
    preselect = cmp.PreselectMode.None,
    snippet = {
        expand = function(args)
            require 'luasnip'.lsp_expand(args.body)
        end
    },

    formatting = {
        format = function(_, vim_item)
            vim_item.kind = (cmp_kinds[vim_item.kind] or '') .. vim_item.kind
            return vim_item
        end,
    },
    mapping = cmp.mapping.preset.insert({
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({ select = false }),
        
        -- ["<Tab>"] = cmp.mapping(
        -- function(fallback)
        --     cmp_ultisnips_mappings.expand_or_jump_forwards(fallback)
        -- end,
            -- function(fallback)
            --     if cmp.visible() then
            --         cmp.select_next_item()
            --     elseif luasnip.expand_or_jumpable() then
            --         luasnip.expand_or_jump()
            --     elseif has_words_before() then
            --         cmp.complete()
            --     else
            --         fallback()
            --     end
            -- end,
            -- { "i", "s", --[[ "c" (to enable the mapping in command mode) ]] }
        -- ),
        ["<S-Tab>"] = cmp.mapping(
        -- function(fallback)
        --     cmp_ultisnips_mappings.jump_backwards(fallback)
        -- end,
            function(fallback)
                if cmp.visible() then
                    cmp.select_prev_item()
                elseif luasnip.jumpable(-1) then
                    luasnip.jump(-1)
                else
                    fallback()
                end
            end,
            { "i", "s", --[[ "c" (to enable the mapping in command mode) ]] }
        ),
    }),
    sources = {
        { name = 'buffer' },
        { name = 'nvim_lsp' },
        { name = 'path' },
        { name = 'nerdfont' },
        { name = 'spell' },
        -- { name = 'ultisnips' },
        { name = 'emoji' },
        { name = 'luasnip', keyword_length = 1 },
        -- { name = 'calc' },
        -- { name = 'copilot' },
        { name = 'nvim_lsp_signature_help' },
        { name = 'dictionary' },
        -- keyword_length = 2 }
    },
    experimental = {
        ghost_text = true
    }
}

cmp.setup.cmdline(':', {
    completion = { completeopt = 'noselect' },
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
        { name = 'path' }
    }, {
        { name = 'cmdline' }
    }
    )
})
cmp.setup.cmdline('/', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
            { name = 'nvim_lsp_document_symbol' }
        },
        {
            { name = 'buffer' }
        })
})
