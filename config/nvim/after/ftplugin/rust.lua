local opts = { buffer = true, silent = true }

-- Hover actions on K
vim.keymap.set("n", "K", function()
  vim.cmd.RustLsp({ 'hover', 'actions' })
end, opts)

-- Show current diagnostic on <leader>k
vim.keymap.set("n", "<leader>k", function()
  vim.cmd.RustLsp({ 'renderDiagnostic', 'current' })
end, opts)

-- Show code actions on gra
vim.keymap.set("n", "gra", function()
  vim.cmd.RustLsp('codeAction')
end, opts)
