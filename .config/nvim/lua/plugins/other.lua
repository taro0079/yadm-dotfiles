require("other-nvim").setup({
  mappings = {
    -- builtin mappings
    "livewire",
    "angular",
    "laravel",
    "rails",
    -- custom mapping
    -- {
    --   pattern = "/app/(.*)/.*.rb$",
    --   target = "/spec/%1/%1/",
    --   transformer = "lowercase",
    --   context = "test"
    -- },
    {
      pattern = "app/controllers/(.*)/(.*).rb",
      target = "spec/requests/%1/%2_spec.rb",
      transformer = "lowercase",
      context = "controller"
    },
    {
      pattern = "/spec/(.*)/(.*)_spec.rb",
      target = {
        { target = "/db/%1/%2.rb" },
        { target = "/app/%1/%2.rb" },
        { target = "%1/%2.rb" },
      },
    },
    {
      pattern = "/spec/(.*)/(.*)/(.*)_spec.rb",
      target = {
        { target = "/db/%1/%2/%3.rb" },
        { target = "/app/%1/%2/%3.rb" },
        { target = "%1/%2/%3.rb" },
      },
    },
    {
      pattern = "/spec/(.*)/(.*)/(.*)/(.*)_spec.rb",
      target = {
        { target = "/db/%1/%2/%3/%4.rb" },
        { target = "/app/%1/%2/%3/%4.rb" },
        { target = "%1/%2/%3/%4.rb" },
      },
    },
    {
      pattern = "/spec/(.*)/(.*)/(.*)/(.*)/(.*)_spec.rb",
      target = {
        { target = "/db/%1/%2/%3/%4/%5.rb" },
        { target = "/app/%1/%2/%3/%4/%5.rb" },
        { target = "%1/%2/%3/%4/%5.rb" },
      },
    },
    {
      pattern = "/spec/requests/(.*)/(.*)_spec.rb",
      target = {
        { target = "/app/controllers/%1/%2.rb" },
      },
    },

  },
  transformers = {
    -- defining a custom transformer
    lowercase = function(inputString)
      return inputString:lower()
    end
  },
  style = {
    -- How the plugin paints its window borders
    -- Allowed values are none, single, double, rounded, solid and shadow
    border = "solid",
    -- Column seperator for the window
    seperator = "|",
    -- width of the window in percent. e.g. 0.5 is 50%, 1.0 is 100%
    width = 0.7,
    -- min height in rows.
    -- when more columns are needed this value is extended automatically
    minHeight = 2
  },
})
