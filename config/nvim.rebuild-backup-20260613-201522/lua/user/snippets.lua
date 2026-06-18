local luasnip = require("luasnip")

luasnip.setup({
  enable_autosnippets = true,
  history = true,
  updateevents = "TextChanged,TextChangedI",
})

require("luasnip.loaders.from_lua").load({
  paths = vim.fn.stdpath("config") .. "/snippets",
})
