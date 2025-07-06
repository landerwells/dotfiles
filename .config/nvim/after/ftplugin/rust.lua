-- local bufnr = vim.api.nvim_get_current_buf()
-- vim.keymap.set(
--   "<leader>", 
--   "k",  -- Override Neovim's built-in hover keymap with rustaceanvim's hover actions
--   function()
--     vim.cmd.RustLsp({'hover', 'actions'})
--   end,
--   { silent = true, buffer = bufnr }
-- )

-- ~/.config/nvim/after/ftplugin/rust.lua

local opts = { buffer = true, silent = true }

-- Hover actions on K
vim.keymap.set("n", "K", function()
  vim.cmd.RustLsp({ 'hover', 'actions' })
end, opts)

-- Show current diagnostic on <leader>k
vim.keymap.set("n", "<leader>k", function()
  vim.cmd.RustLsp({ 'renderDiagnostic', 'current' })
end, opts)
