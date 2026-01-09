-- lua/telescope_prs.lua
local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local conf = require("telescope.config").values
local actions = require("telescope.actions")
local action_state = require("telescope.actions.state")

local M = {}

M.my_prs = function()
    -- gh で JSON を取得
    local raw = vim.fn.system(
        { "gh", "pr", "list", "--author", "@me", "--json", "number,title,headRefName" }
    )

    -- JSON decode
    local ok, prs = pcall(vim.fn.json_decode, raw)
    if not ok or type(prs) ~= "table" then
        vim.notify("Failed to decode gh pr list output", vim.log.levels.ERROR)
        return
    end

    -- Telescope picker
    pickers.new({}, {
        prompt_title = "My PRs",
        finder = finders.new_table {
            results = prs,
            entry_maker = function(pr)
                local display = string.format("#%d  %s  (%s)", pr.number, pr.title, pr.headRefName)
                return {
                    value = pr,
                    display = display,
                    ordinal = display,
                }
            end,
        },
        sorter = conf.generic_sorter({}),
        attach_mappings = function(_, map)
            local function checkout_selected(prompt_bufnr)
                local entry = action_state.get_selected_entry()
                actions.close(prompt_bufnr)

                local pr = entry.value
                local num = pr.number

                -- gh pr checkout <number>
                vim.fn.jobstart({ "gh", "pr", "checkout", tostring(num) }, {
                    stdout_buffered = true,
                    stderr_buffered = true,
                    on_exit = function(_, code)
                        if code == 0 then
                            vim.notify("Checked out PR #" .. num .. " (" .. pr.headRefName .. ")", vim.log.levels.INFO)
                        else
                            vim.notify("Failed to checkout PR #" .. num, vim.log.levels.ERROR)
                        end
                    end,
                })
            end

            -- insert / normal 両方で <CR> で checkout
            map("i", "<CR>", checkout_selected)
            map("n", "<CR>", checkout_selected)

            return true
        end,
    }):find()
end

return M
