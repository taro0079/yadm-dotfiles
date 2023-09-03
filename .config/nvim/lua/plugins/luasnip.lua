local ls = require("luasnip")
-- some shorthands...
local snip = ls.snippet
local node = ls.snippet_node
local r = ls.restore_node
local text = ls.text_node
local insert = ls.insert_node
local func = ls.function_node
local choice = ls.choice_node
local dynamicn = ls.dynamic_node

local date = function() return { os.date('%Y-%m-%d') } end
local function copy(args)
    return args[1]
end

ls.add_snippets(nil, {
    all = {
        snip({
            trig = "date",
            namr = "Date",
            dscr = "Date in the form of YYYY-MM-DD",
        }, {
            func(date, {}),
        }),
        snip("fn", {
            -- Simple static text.
            text("//Parameters: "),
            -- function, first parameter is the function, second the Placeholders
            -- whose text it gets as input.
            func(copy, 2),
            text({ "", "function " }),
            -- Placeholder/Insert.
            insert(1),
            text("("),
            -- Placeholder with initial text.
            insert(2, "int foo"),
            -- Linebreak
            text({ ") {", "\t" }),
            -- Last Placeholder, exit Point of the snippet.
            insert(0),
            text({ "", "}" }),
        }),
        snip("class", {
		-- Choice: Switch between two different Nodes, first parameter is its position, second a list of nodes.
		choice(1, {
			text("public "),
			text("private "),
		}),
		text("class "),
		insert(2),
		text(" "),
		choice(3, {
			text("{"),
			-- sn: Nested Snippet. Instead of a trigger, it has a position, just like insertNodes. !!! These don't expect a 0-node!!!!
			-- Inside Choices, Nodes don't need a position as the choice node is the one being jumped to.
			node(nil, {
				text("extends "),
				-- restoreNode: stores and restores nodes.
				-- pass position, store-key and nodes.
				r(1, "other_class", insert(1)),
				text(" {"),
			}),
			node(nil, {
				text("implements "),
				-- no need to define the nodes for a given key a second time.
				r(1, "other_class"),
				text(" {"),
			}),
		}),
		text({ "", "\t" }),
		insert(0),
		text({ "", "}" }),
	}),
    },
    lua = {
        snip({ trig = "lam" }, {
            text({ "function(" }), insert(1), text({ ")" }),
            text({ "", "" }),
            text({ '    ' }), insert(0),
            text({ "", "" }),
            text({ "end" }),
        })
    },
    php = {
        snip("class",{
            text({"class "}), insert(1),
            text({ "", "" }),
            text({"{"}),
            text({ "", "" }),
            text({ '    ' }), insert(0),
            text({ "", "" }),
            text({"}"}),
        }

        )
    }
})
