local lualine = require('lualine')
lualine.setup {
  options = {
    theme = "auto",
    section_separators = { left = '', right = '' },
    component_separators = '|',

  },
  sections = {
    lualine_a = {
      {
        'mode',
        icons_enabled = true,
        separator = { left = '' },
        right_padding = 2

      },

    },
    lualine_b = {
      {
        'branch',
        icon = { '' },
      },
      'diff'
    },
    lualine_z = {
      { 'location', separator = { right = '' }, left_padding = 2 },
    },
  }
}
