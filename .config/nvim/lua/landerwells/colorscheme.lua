-- vim.cmd.colorscheme "purple"
vim.cmd.colorscheme("gruvbox")
-- vim.cmd([[
--   highlight SpellBad guisp=Red gui=undercurl
--   highlight DiagnosticUnderlineError gui=undercurl guisp=Red
--   highlight DiagnosticUnderlineWarn gui=undercurl guisp=Orange
--   highlight DiagnosticUnderlineHint gui=undercurl guisp=Blue
--   highlight DiagnosticUnderlineInfo gui=undercurl guisp=LightBlue
-- ]])
vim.api.nvim_set_hl(0, 'Question', { fg = '#8ec07c' })  -- Aqua color

-- Define the highlight groups for markdown headings
vim.api.nvim_set_hl(0, "@text.title.1.markdown", { fg = "#fb4934", bold = true }) -- Red
vim.api.nvim_set_hl(0, "markdownH2", { fg = "#fabd2f", bold = true }) -- Yellow
vim.api.nvim_set_hl(0, "markdownH3", { fg = "#b8bb26", bold = true }) -- Green
vim.api.nvim_set_hl(0, "markdownH4", { fg = "#8ec07c", bold = true }) -- Aqua
vim.api.nvim_set_hl(0, "markdownH5", { fg = "#83a598", bold = true }) -- Blue
vim.api.nvim_set_hl(0, "markdownH6", { fg = "#d3869b", bold = true }) -- Violet

