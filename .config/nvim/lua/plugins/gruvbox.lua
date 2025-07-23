return {
  "ellisonleao/gruvbox.nvim", -- theme
  priority = 1000,
  config = function()
    require("gruvbox").setup({
      terminal_colors = true, -- add neovim terminal colors
      undercurl = true,
      underline = true,
      bold = true,
      italic = {
        strings = true,
        emphasis = true,
        comments = true,
        operators = false,
        folds = true,
      },
      strikethrough = true,
      invert_selection = false,
      invert_signs = false,
      invert_tabline = false,
      inverse = true, -- invert background for search, diffs, statuslines and errors
      -- contrast = "soft", -- can be "hard", "soft" or empty string

      palette_overrides = {},
      overrides = {
        ["@constant.builtin"] = {link = "GruvboxPurple"},
        ["@storageclass.lifetime"] = {link = "GruvboxAqua"},
        ["@text.note"] = {link = "TODO"},
        ["@namespace.rust"] = {link = "Include"},
        ["@punctuation.bracket"] = {link = "GruvboxOrange"},
        texMathDelimZoneLI = {link = "GruvboxOrange"},
        texMathDelimZoneLD = {link = "GruvboxOrange"},
        luaParenError = {link = "luaParen"},
        luaError = {link = "NONE"},
        -- fold
        -- Folded = {fg = "#fe8019", bg = "#0E1018", italic = true},
        SignColumn = {bg = "#282828"}

      }
    })
  end
}
