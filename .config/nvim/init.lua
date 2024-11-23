require('options')
require('plugins')
require('ui')
require('keys')
require('config')
require('gui-settings')
require('helper.matchit-setting')
require('myfunc')

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

-- yadm にファイルを追加する関数
local function add_yaml(target_file_path)
    cmd = "yadm add " .. target_file_path
    vim.fn.jobstart(cmd, {
        stdout_buffered = true,
        on_stdout = function(_, data)
            if data[0] ~= nil then
                vim.notify("Message: " .. table.concat(data, "\n"))
            end
        end,
        on_stderr = function(_, data)
            if data[0] ~= nil then
                vim.notify("Error: " .. table.concat(data, "\n"))
            end
        end,
        on_exit = function(_, code)
            if code == 0 then
                vim.notify("File added successfully")
            else
                vim.notify("Error: " .. code)
            end
        end
    })
end



function run_client()
    local cmd_prefix =
    "docker compose -f ~/dev/rpst-oms-backend/docker-compose.yml run --rm devcontainer mysql --defaults-extra-file=.my.cnf -Dapp_db"
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

function transport_to_remote(local_path, remote_path)
    local file_path = vim.fn.expand("%:p")
    if file_path:find(vim.fn.expand(local_path), 1, true) == 1 then
        vim.notify("File is inside the project directory")

        local relative_path = file_path:sub(#vim.fn.expand(local_path) + 1)
        vim.notify("Transferring file: " .. file_path .. " to remote path: " .. remote_path .. relative_path)

        -- rsync コマンドを非同期で実行
        local cmd = string.format("rsync -avz %s %s%s", file_path, remote_path, relative_path)
        vim.fn.jobstart(cmd, {
            stdout_buffered = true,
            on_stdout = function(_, data)
                if data then
                    vim.notify("Message: " .. table.concat(data, "\n"))
                end
            end,
            on_stderr = function(_, data)
                if data then
                    vim.notify("Error: " .. table.concat(data, "\n"))
                end
            end,
            on_exit = function(_, code)
                if code == 0 then
                    vim.notify("File transferred successfully")
                else
                    vim.notify("Error: " .. code)
                end
            end
        })
    end
end

-- rpst-v2のプロジェクトディレクトリ内のファイルをリモートサーバに転送する関数
function transport_v2()
    local local_project_path = "~/dev/rpst-v2/"
    local remote_project_path = "taro_morita@dev-tmorita:/var/www/rpst-v2/dev/"
    transport_to_remote(local_project_path, remote_project_path)
end

function transport_to_v2_of_rpst_api()
    local local_project_path = "~/dev/rpst-v2/"
    local remote_project_path = "taro_morita@oms-dev:/var/lib/rpst-api-docker/rpst-v2/"
    transport_to_remote(local_project_path, remote_project_path)
end

vim.api.nvim_create_autocmd("BufWritePost", {
    pattern = "*/rpst-v2/*",
    callback = function()
        transport_v2()
        transport_to_v2_of_rpst_api()
    end
})

vim.api.nvim_create_autocmd("BufWritePost", {
    pattern = "index.yaml",
    callback = function()
        CreateRedoc()
    end
})

-- yadm-auto-update-list に記載されているファイルを編集した際に yadm add する
local file_paths = require('myfunc').load_files("~/.yadm-auto-update-list")
if file_paths then
    for _, file_path in ipairs(file_paths) do
        vim.api.nvim_create_autocmd("BufWritePost", {
            pattern = file_path,
            callback = function()
                print(file_path .. " is edited.")
                add_yaml(file_path)
            end
        })
    end
end

function Phpunit()
    local php_unit = require('phpunit_runner')
    php_unit.test_runner()
end

function OmsForever()
    local current_file_path = vim.fn.expand("%")
    local cmd = string.format("ruby ~/dev/oms-create-rb/test.rb %s", current_file_path)
    local cmd_output = vim.fn.system(cmd)
    local lines = vim.split(cmd_output, "\n")

    -- コマンドの出力をバッファに出力する前にバッファを全て削除する
    vim.api.nvim_buf_set_lines(0, 0, -1, false, {})

    -- コマンドの出力をバッファに出力する
    for i, line in ipairs(lines) do
        vim.api.nvim_buf_set_lines(0, i - 1, i, false, { line })
    end
end

function CreateRedoc()
    local current_file_path = vim.fn.expand("%")
    local cmd = string.format("redocly build-docs %s", current_file_path)
    vim.fn.jobstart(cmd, {
        stdout_buffered = true,
        on_stdout = function(_, data)
            if data then
                vim.notify("Message: " .. table.concat(data, "\n"))
            end
        end,
        on_stderr = function(_, data)
            if data then
                vim.notify("Error: " .. table.concat(data, "\n"))
            end
        end,
        on_exit = function(_, code)
            if code == 0 then
                vim.notify("File transferred successfully")
            else
                vim.notify("Error: " .. code)
            end
        end
    })
end

vim.api.nvim_create_user_command("AwesomeSwitch", function()
    require('myplugin.switch').run(vim.fn.getcwd())
end, { nargs = 0 })
