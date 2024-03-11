local command = {}

local path = "~/.config/nvim/lua/command.sh"
function command.read()
    vim.cmd('edit ' .. path)
end

function command.run()
    vim.cmd('!bash ' .. path)
end

return command
