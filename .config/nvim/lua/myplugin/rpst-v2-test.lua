local M = {}

-- basepath: 
-- rpst-v2: /var/www/rpst-v2
-- rpst-api: /var/lib/rpst-api-docker/rpst-v2
function M.run_all(basepath)
    local cmd = string.format('cd %s/test && ./tests/app/phpunit/v9/bin/test_runner.sh', basepath)
    vim.api.nvim_command(string.format('vsplit | terminal %s', cmd))
end

-- basepath: 
-- rpst-v2: /var/www/rpst-v2
-- rpst-api: /var/lib/rpst-api-docker/rpst-v2
function M.run_for_branch(basepath)
    local cmd = string.format('cd %s/test && ./branch_phpunit.sh', basepath)
    vim.api.nvim_command(string.format('vsplit | terminal %s', cmd))
end

return M
