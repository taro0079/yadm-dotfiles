local command = {}

local path = vim.fn.expand("~/.config/nvim/lua/command.sh")
local output_buffer = {} -- 出力を格納するバッファ

local function on_read(err, data)
    if err then
        print("error reading from pipe: " .. err)
    elseif data then
        for line in data:gmatch("[^\r\n]+") do
            table.insert(output_buffer, line)
        end
    end
end

local function on_exit(code, signal)
    print("process exited with code " .. code .. " and signal " .. signal)
    -- 新しいバッファを作成して出力を表示
    vim.cmd("vnew")                                                -- 新しいウィンドウを開く
    local bufnr = vim.api.nvim_get_current_buf()                   -- 現在のバッファ番号を取得
    vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, output_buffer) -- 出力をバッファに書き込む
    output_buffer = {}                                             -- バッファをクリア
end

local function run_shell_script(script_path)
    local stdout = vim.loop.new_pipe(false)
    local handle, pid = vim.loop.spawn("bash", {
        args = { script_path },
        stdio = { nil, stdout, nil },
    }, vim.schedule_wrap(on_exit))

    if not handle then
        print("failed to run " .. script_path)
    else
        print("running " .. script_path .. " with pid " .. pid)
        vim.loop.read_start(stdout, vim.schedule_wrap(on_read))
    end
end

function command.read()
    vim.cmd('edit ' .. path)
end

function command.run()
    run_shell_script(path)
end

return command
