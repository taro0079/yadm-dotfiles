-- phpunitを実行するためのスクリプト
-- OMSでしか使えません
local M = {}

function M.test_runner()
    local file = vim.fn.expand('%')
    local cmd = string.format("docker compose -f ../docker-compose.yml run --rm devcontainer symfony php bin/phpunit %s", file)
    vim.api.nvim_command('vsplit | terminal ' .. cmd)
    vim.api.nvim_command('startinsert')
end

return M
