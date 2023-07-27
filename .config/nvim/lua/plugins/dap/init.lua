require('telescope').load_extension('dap')
-- require('plugins.dap.python')
local dap = require('dap')
-- dap.adapters.php = {
--   type = "executable",
--   command = "node",
--   args = { os.getenv("HOME") .. ".vscode-server/extensions/xdebug.php-debug-1.32.1/out" }
-- }
--
-- dap.configurations.php = {
--   {
--     type = "php",
--     request = "launch",
--     name = "Listen for Xdebug",
--     port = 8008,
--     -- pathMappings = {
--     --   ["/var/www/html"] = "${workspaceFolder}"
--     -- }
--   }
-- }



