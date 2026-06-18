local opts = { buffer = true, silent = true }

vim.keymap.set("n", "K", function()
  vim.cmd.RustLsp({ "hover", "actions" })
end, vim.tbl_extend("force", opts, { desc = "Rust: hover actions" }))

vim.keymap.set("n", "<leader>k", function()
  vim.cmd.RustLsp({ "renderDiagnostic", "current" })
end, vim.tbl_extend("force", opts, { desc = "Rust: render current diagnostic" }))

vim.keymap.set("n", "gra", function()
  vim.cmd.RustLsp("codeAction")
end, vim.tbl_extend("force", opts, { desc = "Rust: code action" }))
