local M = {}

local function runCommand(command)
    local stdout = vim.loop.new_pipe(false)
    local handle, pid

    vim.schedule(function()
        vim.cmd("vnew")
        vim.cmd("setlocal buftype=nofile")
        vim.cmd("setlocal bufhidden=hide")
        vim.cmd("setlocal nowrap")
    end)

    handle, pid = vim.loop.spawn("bash", {
        args = { "-c", command },
        stdio = { nil, stdout, nil },
    }, function(code, signal)
        vim.schedule(function()
            print("Process exited with code", code, signal)
        end)
        stdout:close()
        handle:close()
    end)

    stdout:read_start(function(err, data)
        assert(not err, err)
        if data then
            vim.schedule(function()
                local buffer = vim.api.nvim_get_current_buf()
                local lines = vim.split(data, "\n", true)
                vim.api.nvim_buf_set_lines(buffer, -1, -1, false, lines)

                local line_count = vim.api.nvim_buf_line_count(buffer)
                local win = vim.api.nvim_get_current_win()
                vim.api.nvim_win_set_cursor(win, { line_count, 0 })
            end)
        end
    end)
end

function M.runAll()
    local command = 'ssh dev-tmorita "cd /var/www/rpst-v2/test && ./tests/app/phpunit/v9/bin/test_runner.sh"'
    runCommand(command)
end

function M.runForBranch()
    local command = 'ssh dev-tmorita "cd /var/www/rpst-v2/test && ./branch_phpunit.sh"'
    runCommand(command)
end

return M
