require("conform").setup({
    format_after_save = function(bufnr)
        return { timeout_ms = 500, lsp_fallback = true }
    end,
    formatters_by_ft = {
        php = { "php_cs_fixer" }
    }
})
