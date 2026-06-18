vim.o.background = "dark"

require("gruvbox").setup({
  terminal_colors = true,
  undercurl = true,
  underline = true,
  bold = true,
  italic = {
    strings = false,
    comments = true,
    operators = false,
    folds = true,
  },
  strikethrough = true,
  invert_selection = false,
  contrast = "hard",
  transparent_mode = false,
  overrides = {
    SignColumn = { bg = "NONE" },
    StatusLine = { bg = "NONE" },
  },
})

vim.cmd.colorscheme("gruvbox")
