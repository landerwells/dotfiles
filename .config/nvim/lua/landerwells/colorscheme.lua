vim.cmd.colorscheme("gruvbox")

vim.cmd([[
  highlight SpellBad guisp=Red gui=undercurl
  highlight DiagnosticUnderlineError gui=undercurl guisp=Red
  highlight DiagnosticUnderlineWarn gui=undercurl guisp=Orange
  highlight DiagnosticUnderlineHint gui=undercurl guisp=Blue
  highlight DiagnosticUnderlineInfo gui=undercurl guisp=LightBlue
]])

vim.api.nvim_set_hl(0, 'Question', { fg = '#8ec07c' })  -- Aqua color
vim.api.nvim_set_hl(0, "@markup.heading.1.markdown", { fg = "#fb4934", bold = true }) -- Red
vim.api.nvim_set_hl(0, "@markup.heading.2.markdown", { fg = "#fabd2f", bold = true }) -- Yellow
vim.api.nvim_set_hl(0, "@markup.heading.3.markdown", { fg = "#b8bb26", bold = true }) -- Green
vim.api.nvim_set_hl(0, "@markup.heading.4.markdown", { fg = "#8ec07c", bold = true }) -- Aqua
vim.api.nvim_set_hl(0, "@markup.heading.5.markdown", { fg = "#83a598", bold = true }) -- Blue
vim.api.nvim_set_hl(0, "@markup.heading.6.markdown", { fg = "#d3869b", bold = true }) -- Violet
