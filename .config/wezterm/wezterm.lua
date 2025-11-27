local wezterm = require 'wezterm';
local mac = wezterm.target_triple:find("darwin")
local linux = wezterm.target_triple:find("linux")

local mykeys = {
    { key = 'n', mods = 'OPT',  action = wezterm.action.ToggleFullScreen },
    { key = '8', mods = 'CTRL', action = wezterm.action.PaneSelect },
    { key = "v", mods = "OPT",  action = wezterm.action({ SplitHorizontal = { domain = "CurrentPaneDomain" } }) },
    { key = "s", mods = "OPT",  action = wezterm.action({ SplitVertical = { domain = "CurrentPaneDomain" } }) }
}

for i = 1, 8 do
    table.insert(mykeys, {
        key = tostring(i),
        mods = "OPT",
        action = wezterm.action { ActivateTab = i - 1 }
    })
end

if mac then
    return {
        color_scheme = "tokyonight",
        audible_bell = "Disabled",
        font = wezterm.font_with_fallback {
            { family = 'Iosevka Nerd Font',       weight = "Regular", harfbuzz_features = { 'calt=1', 'clig=1', 'liga=1' } },
            { family = 'PleckJP',                 weight = "Regular", italic = false },
            { family = 'Cica',                    weight = "Regular", italic = false },
            { family = 'CaskaydiaCove Nerd Font', weight = "Regular", italic = false },
            { family = 'HackGen Console NF',      weight = "Regular", italic = false },
            { family = 'CaskaydiaCove Nerd Font', weight = "Bold" },
            { family = 'CaskaydiaCove Nerd Font', weight = "Bold",    italic = true },

        },
        -- disable_default_key_bindings = true,
        font_size = 16,
        keys = mykeys,
        -- color_scheme = "Tokyo Night Storm",
        send_composed_key_when_right_alt_is_pressed = false,
        hide_tab_bar_if_only_one_tab = true,
        window_padding = {
            left = 0,
            right = 0,
            top = 0,
            bottom = 0
        },
        adjust_window_size_when_changing_font_size = false,
        -- front_end = "WebGpu",
        window_background_opacity = 1,
        -- macos_window_background_blur = 20,

    }
elseif linux then
    return {
        color_scheme = "tokyonight",
        enable_tab_bar = false,
        font = wezterm.font_with_fallback {
            { family = 'CaskaydiaCove Nerd Font', weight = "Regular", italic = false },
            { family = 'HackGen Console NF',      weight = "Regular", italic = true },
            { family = 'CaskaydiaCove Nerd Font', weight = "Bold" },
            { family = 'HackGen Console NF',      weight = "Bold" }

        },
        font_size = 16,
        window_padding = {
            left = 0,
            right = 0,
            top = 0,
            bottom = 0
        },
    }
end
