require('options')
require('plugins')
require('ui')
require('keys')
require('config')
require('gui-settings')
require('helper.matchit-setting')

incremental_selection = {
    enable = true,
    keymaps = {
        init_selection = "<CR>",
        node_incremental = "<CR>",
        node_decremental = "<BS>",
        scope_incremental = "<TAB>",
    },
    is_supported = function()
        local mode = vim.api.nvim_get_mode().mode
        if mode == "c" then
            return false
        end
        return true
    end
}

vim.api.nvim_create_autocmd("FileType", {
    pattern = "yaml",
    callback = function()
        vim.bo.shiftwidth = 4
        vim.bo.softtabstop = 4
        vim.bo.expandtab = true
    end
})
require('phpunit_runner')


function run_client()
    local cmd_prefix =
    "docker compose -f ~/dev/rpst-oms-backend/docker-compose.yml run --rm php-dev mysql --defaults-extra-file=.my.cnf -Dapp_db"
    local cmd = cmd_prefix .. " -e 'select * from mst_warehouse;'"
    local result = vim.fn.system(cmd)
    local rows = vim.split(result, "\n")
    new_rows = {}
    for _, v in ipairs(rows) do
        if v ~= "" then
            table.insert(new_rows, vim.split(v, "\t"))
        end
    end
    local new_buf = vim.api.nvim_create_buf(false, true) -- create new empty buffer
    vim.api.nvim_set_option_value("wrap", false, { win = 0 })
    vim.api.nvim_command("vsplit")
    vim.api.nvim_win_set_buf(0, new_buf)
    print(vim.inspect(new_rows))
    local tt = print_table(new_rows)


    for i, v in ipairs(tt) do
        vim.api.nvim_buf_set_lines(new_buf, i - 1, i - 1, false, { v })
    end
end

function calculate_max_length(data)
    local max_lengths = {}
    for _, row in ipairs(data) do
        for col_index, col_value in ipairs(row) do
            local length = #tostring(col_value)
            if not max_lengths[col_index] or length > max_lengths[col_index] then
                max_lengths[col_index] = length
            end
        end
    end
    return max_lengths
end

function print_table(data)
    local max_lengths = calculate_max_length(data)
    local new = {}
    for _, row in ipairs(data) do
        local cols = {}
        local row_str = ""
        for col_idx, col_val in ipairs(row) do
            local padding_val = tostring(col_val) .. string.rep(" ", max_lengths[col_idx] - #tostring(col_val))
            row_str = row_str .. padding_val .. "|"
            table.insert(cols, padding_val)
        end
        print(vim.inspect(cols))
        table.insert(new, row_str)
    end
    return new
end
