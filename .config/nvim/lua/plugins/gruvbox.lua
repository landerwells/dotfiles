return {
  "ellisonleao/gruvbox.nvim", -- theme
  priority = 1000,
  config = function()
    require("gruvbox").setup({
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
