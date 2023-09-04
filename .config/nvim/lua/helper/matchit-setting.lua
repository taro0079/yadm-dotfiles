local M = {}

M.source_matchit = function ()
    vim.cmd('source $VIMRUNTIME/macros/matchit.vim')
end

return M
