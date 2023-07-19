local nvim_lsp = require('lspconfig')
local lsp_installer = require("mason")
-- local configs = require 'lspconfig.configs'

lsp_installer.setup {}

local opts = { noremap = true, silent = true }


local on_attach = function(client, bufnr)
  if client.name == "tsserver" then
    client.server_capabilities.document_formatting = false
  end

  vim.api.nvim_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev<CR>', opts)
  vim.api.nvim_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  vim.api.nvim_set_keymap('n', '<space>q', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)

  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  vim.keymap.set({ 'n' }, 'gi', '<Cmd>Telescope lsp_implementations<CR>')
  vim.keymap.set({ 'n' }, '<space>D', '<Cmd>Telescope lsp_type_definitions<CR>')
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gl', '<cmd>lua vim.diagnostic.open_float()<cr>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<C-s>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>wl',
    '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  vim.keymap.set({ 'n' }, 'gd', '<Cmd>Telescope lsp_references<CR>')
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>ff', '<cmd>lua vim.lsp.buf.format({ timeout_ms=5000, async=true })<CR>',
    opts)
  require 'illuminate'.on_attach(client)
end

local capabilities = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())
local mason_lspconfig = require('mason-lspconfig')
mason_lspconfig.setup_handlers({ function(server_name)
  nvim_lsp[server_name].setup {
    on_attach = on_attach,
    capabilities = capabilities,
    flags = {
      debounce_text_changes = 150,
    }
  }
end })
-- for _, lsp in ipairs(lsp_installer.get_installed_servers()) do
-- end
local capabilities = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())

capabilities.textDocument.completion.completionItem.snippetSupport = true

vim.diagnostic.config({
  virtual_text = false,
  -- signs = true,
  -- underline = true,
  -- update_in_insert = false,
  -- severity_sort = true,
  -- float = {
  --   focusable = true,
  --   style = "minimal",
  --   border = "rounded",
  --   source = "if_many",
  --   header = "",
  --   prefix = ""
  -- }
})


local signs = { Error = "", Warn = " ", Hint = "󱠂", Info = " " }


for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end
nvim_lsp.steep.setup {}
