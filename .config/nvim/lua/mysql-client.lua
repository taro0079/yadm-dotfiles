local M = {}

function M.run_client()
    local cmd_prefix =
    "docker compose -f ~/dev/rpst-oms-backend/docker-compose.yml run --rm database mysql -u root -p!ChangeMe! -Dapp_db"
    local cmd = cmd_prefix .. " -e 'select * from mst_warehouse;'"
    local result = vim.fn.system(cmd)
    local split = vim.split(result, "\n")

    vim.api.nvim_out_write(result)

    -- for i, v in ipairs(split) do
    --     vim.api.nvim_out_write(v)
    -- end
end

return M
