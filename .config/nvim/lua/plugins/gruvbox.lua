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
        -- Folded = {fg = "#fe8019", bg = "#0E1018", italic = true},
        -- SignColumn = {bg = "#282828"}

      }
    })
  end
}


-- return {
--   'morhetz/gruvbox',
--   priority = 1000,
--   config = function()
--     -- vim.cmd('colorscheme gruvbox')
--     -- vim.o.background = 'dark' -- or 'light'
--     vim.g.gruvbox_contrast_dark = 'hard' -- or 'soft'
--     vim.g.gruvbox_invert_selection = '0' -- disable invert selection
--     vim.g.gruvbox_italic = 1 -- enable italic comments
--   end
-- }
