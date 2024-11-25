local M = {}

function M.run()
    local cmd = 'php app/cli/cmd.php /sandbox/test_runner' -- TODO: fix path
    -- async process
    vim.fn.jobstart(cmd, {
        on_stdout = function(_, data, _)
            if data then
                vim.notify("Message: " .. table.concat(data, "\n"))
            end
        end,
        on_stderr = function(_, data, _)
            if data then
                vim.notify("Error: " .. table.concat(data, "\n"))
            end
        end,
        on_exit = function(_, code, _)
            if code == 0 then
                vim.notify("File transferred successfully")
            else
                vim.notify("Error: " .. code)
            end
        end
    })

end

function M.open()
    vim.cmd("e " .. "app/cli/cmd/sandbox/TestRunner.php")
end
return M
