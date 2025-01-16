local M = {}
local ns = vim.api.nvim_create_namespace("php_syntax")

function M.php_syntax_check()
    local bufnr = vim.api.nvim_get_current_buf()
    local filepath = vim.api.nvim_buf_get_name(bufnr)

    if filepath == "" then
        vim.notify("FIle must be saved to run syntax check", vim.log.levels.WARN)
        return
    end

    local cmd = "php -l " .. vim.fn.shellescape(filepath)
    vim.fn.jobstart(cmd, {
        stdout_buffered = true,
        stderr_buffered = true,
        on_stdout = function(_, data)
            if data then
                M.handle_output(bufnr, data)
            end
        end,
        on_stderr = function(_, data)
            if data then
                M.handle_output(bufnr, data)
            end
        end,
    })
end

function M.handle_output(bufnr, data)
    vim.diagnostic.reset(ns, bufnr)

    local diagnostics = {}
    for _, line in ipairs(data) do
        -- Match PHP syntax error messages
        local file, row = line:match("in (.+) on line (%d+)")
        if file and row then
            table.insert(diagnostics, {
                lnum = tonumber(row) - 1, -- Convert to 0-based indexing
                col = 0,          -- Column information is not provided by php -l
                message = line,
                severity = vim.diagnostic.severity.ERROR,
                source = "php -l",
            })
        end
    end

    -- Ensure position_encoding is set
    -- vim.lsp.util.make_position_params(vim.api.nvim_get_current_win(), 'utf-16')
    vim.diagnostic.set(ns, bufnr, diagnostics)
end

return M
