local wk = require("which-key")

wk.setup({
  preset = "modern",
  delay = 180,
  plugins = {
    marks = true,
    registers = true,
    spelling = {
      enabled = true,
      suggestions = 20,
    },
    presets = {
      operators = true,
      motions = true,
      text_objects = true,
      windows = true,
      nav = true,
      z = true,
      g = true,
    },
  },
  win = {
    border = "rounded",
    padding = { 1, 2 },
    title = true,
    title_pos = "center",
    wo = { winblend = 0 },
  },
  layout = {
    width = { min = 24 },
    spacing = 4,
  },
  icons = {
    breadcrumb = "»",
    separator = "➜",
    group = "+",
  },
})

wk.add({
  { "<leader>b", group = "buffers", expand = function()
    return require("which-key.extras").expand.buf()
  end },
  { "<leader>c", group = "code/cwd" },
  { "<leader>l", group = "lsp" },
  { "<leader>p", group = "plugins" },
  { "<leader>w", proxy = "<c-w>", group = "windows" },
  { "<leader>z", desc = "Toggle centered editing" },
})
