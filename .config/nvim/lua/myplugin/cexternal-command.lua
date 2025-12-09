local M = {}

function M.register_from_external_command(command)
    local result = vim.fn.system(command)
    vim.fn.setqflist({}, 'r', { title = 'External Command Results', lines = vim.fn.split(result, '\n') })
    vim.cmd('copen')
end

return M
