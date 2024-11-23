local M = {}

local file_checker = require('helper.is_file_directory')
local file_existtance = require('helper.file_exist')

-- from src file to test file
local function switch_to_test()
    local path = vim.fn.expand("%:p")
    local test_class_path = string.gsub(path, ".php", "Test.php")
    local unit_test_path = string.gsub(test_class_path, "src", "tests/Unit")
    local integration_test_path = string.gsub(test_class_path, "src", "tests/Integration")

    if file_existtance.file_exist(unit_test_path) then
        vim.cmd("e " .. unit_test_path)
    elseif file_existtance.file_exist(integration_test_path) then
        vim.cmd("e " .. integration_test_path)
    else
        print("Test file does not exist")
    end
end

-- from test file to src file
local function switch_to_src()
    local path = vim.fn.expand("%:p")
    local src_file_path = string.gsub(path, "Test.php", ".php")
    local src_path, n = string.gsub(src_file_path, "tests/Integration", "src")
    if n == 0 then
        src_path, n = string.gsub(src_file_path, "tests/Unit", "src")
    end

    if file_existtance.file_exist(src_path) then
        vim.cmd("e " .. src_path)
    else
        print("Source file does not exist")
    end
end

function M.run(path)
    if not (file_checker.run(path)) then
        print("This is not a file in " .. path)
        return
    end

    local current_file = vim.fn.expand("%:p")
    if string.find(current_file, "src") then
        switch_to_test()
    elseif string.find(current_file, "test") then
        switch_to_src()
    end
end

return M
