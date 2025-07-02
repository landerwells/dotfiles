return {
  'mrcjkb/rustaceanvim',
  version = '^6', -- Recommended
  lazy = false, -- This plugin is already lazy

  -- vim.keymap.set(
  --   "n", 
  --   "K",  -- Override Neovim's built-in hover keymap with rustaceanvim's hover actions
  --   function()
  --     vim.cmd.RustLsp({'hover', 'actions'})
  --   end,
  --   { silent = true, buffer = bufnr }
  -- ),

  -- vim.keymap.set(
  --   "n", 
  --   "<leader>k",
  --   function()
  --     vim.cmd.RustLsp({ 'renderDiagnostic', 'current' })
  --   end,
  --   { silent = true, buffer = bufnr }
  -- ),
}
