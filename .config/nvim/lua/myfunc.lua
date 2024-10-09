local M = {}

function M.load_files(path_of_file_list)
    -- 与えられたファイルパスにチルダが含まれる場合は、ホームディレクトリに変換する
    local is_use_relative_path = string.find(path_of_file_list, '~')
    local home_dir = os.getenv('HOME')
    if home_dir == nil then
        vim.notify('HOME is not set.')
        return
    end
    if is_use_relative_path then
        path_of_file_list = string.gsub(path_of_file_list, '~', home_dir)
    end

    local files = {}
    local file = io.open(path_of_file_list, "r")
    if file then
        for line in file:lines() do
            local is_used_relative_path = string.find(line, '~')
            vim.notify(is_used_relative_path)
            if is_used_relative_path then
                local line = string.gsub(line, '~', home_dir)
            end
            table.insert(files, line)
        end
        file:close()
    else
        vim.notify('File is not exist.')
    end
    return files
end

return M
