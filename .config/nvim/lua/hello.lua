local hello = {}

function hello.greet(name)
    print("\nHello" .. name .. "!")
end

function hello.askNameGeet()
    local name = vim.fn.input("your name ?")
    hello.greet(name)
end

function hello.testCode()
    local handle = io.popen("echo test")
    local result = handle:read("*a")
    handle:close()
    print(result)
end

function hello.yadmAdd()
    local handle = io.popen("yadm add -u")
    local result = handle:read('*a')
    handle:close()
    print(result)
end

function hello.yadmAddCurrentFile()
    local current_file = vim.api.nvim_buf_get_name(0)
    if current_file == "" then
        print("No file in current buffer !!")
        return
    end

    local command = "yadm add " .. current_file
    local handle = io.popen(command)
    local result = handle:read('*a')
    handle:close()
    print(result)
end

function hello.yadmCommitting()
    local message = vim.fn.input("yadm commit message? >")
    hello.yadmCommit(message)
end

function hello.yadmCommit(message)
    local handle = io.popen("yadm commit -m \"" .. message .. "\"")
    local result = handle:read('*a')
    handle:close()
    print(result)
end

function hello.yadmPush()
    local handle = io.popen("yadm push origin master")
    local result = handle:read('*a')
    handle:close()
    print(result)
end

function hello.yadmLog()
    local handle = io.popen("yadm log")
    local result = handle:read('*a')
    handle:close()
    print(result)
end

return hello
