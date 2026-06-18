vim.filetype.add({
  extension = {
    wesl = "wesl",
    wgsl = "wgsl",
  },
})

-- Highlight searches only while searching/jumping through matches.
vim.on_key(function(char)
  if vim.fn.mode() == "n" then
    vim.opt.hlsearch = vim.tbl_contains({ "<CR>", "n", "N", "*", "#", "?", "/", "z", "v" }, vim.fn.keytrans(char))
  end
end, vim.api.nvim_create_namespace("auto_hlsearch"))

vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank({ timeout = 120 })
  end,
})

vim.api.nvim_create_autocmd("VimResized", {
  callback = function()
    vim.cmd.wincmd("=")
  end,
})
