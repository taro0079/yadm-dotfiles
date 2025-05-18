-- lua/plugins/lsp.lua

local lsp_zero = require('lsp-zero')

-- Snippets の読み込み
require('luasnip.loaders.from_snipmate').lazy_load({
  paths = { vim.fn.stdpath("config") .. "/snippets" }
})

-- mason と lsp のインストール
require('mason').setup({})
require('mason-lspconfig').setup({
  ensure_installed = {
    'lua_ls',
    'rust_analyzer',
    'intelephense',
    'eslint',
    'typos_lsp',
    'tsserver',
  },
  handlers = {
    lsp_zero.default_setup,
    lua_ls = function()
      require('lspconfig').lua_ls.setup({
        settings = {
          Lua = {
            diagnostics = {
              globals = { 'vim' },
            },
          },
        },
      })
    end,
  },
})

-- nvim-cmp 設定
local cmp = require('cmp')
local cmp_select = { behavior = cmp.SelectBehavior.Select }

local cmp_kinds = {
  Text = '  ', Method = '  ', Function = '  ', Constructor = '  ',
  Field = '  ', Variable = '  ', Class = '  ', Interface = '  ',
  Module = '  ', Property = '  ', Unit = '  ', Value = '  ',
  Enum = '  ', Keyword = '  ', Snippet = '  ', Color = '  ',
  File = '  ', Reference = '  ', Folder = '  ', EnumMember = '  ',
  Constant = '  ', Struct = '  ', Event = '  ', Operator = '  ',
  TypeParameter = '  ',
}

local luasnip = require('luasnip')

local cmp_mappings = cmp.mapping.preset.insert({
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.abort(),
    ['<CR>'] = cmp.mapping.confirm({ select = true }),
})

cmp.setup({
  mapping = cmp_mappings,
  formatting = {
    format = function(_, vim_item)
      vim_item.kind = (cmp_kinds[vim_item.kind] or '') .. vim_item.kind
      return vim_item
    end,
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'luasnip',   keyword_length = 1 },
    { name = 'buffer' },
    { name = 'path' },
    { name = 'nerdfont' },
    { name = 'spell' },
    { name = 'dictionary' },
    { name = 'emoji' },
  },
})

-- cmdline 用 cmp 設定
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' },
  }, {
    { name = 'cmdline' },
  }),
})

cmp.setup.cmdline('/', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'nvim_lsp_document_symbol' },
    { name = 'buffer' },
  },
})

-- lsp 設定（共通）
lsp_zero.on_attach(function(client, bufnr)
  local opts = { buffer = bufnr, remap = false }

  if client.name == 'tsserver' then
    client.server_capabilities.documentFormattingProvider = false
  end

  local keymap = vim.keymap.set
  keymap("n", "gd", vim.lsp.buf.definition, opts)
  keymap("n", "K", vim.lsp.buf.hover, opts)
  keymap("n", "<leader>vws", vim.lsp.buf.workspace_symbol, opts)
  keymap("n", "<leader>vd", vim.diagnostic.open_float, opts)
  keymap("n", "[d", vim.diagnostic.goto_next, opts)
  keymap("n", "]d", vim.diagnostic.goto_prev, opts)
  keymap("n", "<leader>vca", vim.lsp.buf.code_action, opts)
  keymap("n", "<leader>vrr", vim.lsp.buf.references, opts)
  keymap("n", "<leader>vrn", vim.lsp.buf.rename, opts)
  keymap("i", "<C-h>", vim.lsp.buf.signature_help, opts)
end)

-- diagnostic 設定
vim.diagnostic.config({
  virtual_text = true,
  signs = true,
  underline = false,
})
