vim.bo.expandtab = true
vim.bo.shiftwidth = 4
vim.bo.softtabstop = 4
vim.bo.tabstop = 4
vim.bo.commentstring = "// %s"

local opts = { buffer = true, silent = true }
local bufnr = vim.api.nvim_get_current_buf()

local function project_root()
  return vim.fs.root(bufnr, { "build.zig", ".git" }) or vim.fn.getcwd()
end

local function run_in_terminal(command)
  vim.cmd.write()
  vim.cmd("botright 12split")
  vim.cmd("terminal cd " .. vim.fn.shellescape(project_root()) .. " && " .. command)
  vim.cmd.startinsert()
end

vim.keymap.set("n", "<leader>zb", function()
  run_in_terminal("zig build")
end, vim.tbl_extend("force", opts, { desc = "Zig: build project" }))

vim.keymap.set("n", "<leader>zt", function()
  local current_file = vim.fn.shellescape(vim.api.nvim_buf_get_name(bufnr))
  if vim.uv.fs_stat(project_root() .. "/build.zig") then
    run_in_terminal("zig build test")
  else
    run_in_terminal("zig test " .. current_file)
  end
end, vim.tbl_extend("force", opts, { desc = "Zig: run project or current-file tests" }))

vim.keymap.set("n", "<leader>zr", function()
  local current_file = vim.fn.shellescape(vim.api.nvim_buf_get_name(bufnr))
  if vim.uv.fs_stat(project_root() .. "/build.zig") then
    run_in_terminal("zig build run")
  else
    run_in_terminal("zig run " .. current_file)
  end
end, vim.tbl_extend("force", opts, { desc = "Zig: run project or current file" }))

local function format_zig(notify_if_missing)
  local formatters = vim.lsp.get_clients({
    bufnr = bufnr,
    method = vim.lsp.protocol.Methods.textDocument_formatting,
  })

  if #formatters == 0 then
    if notify_if_missing then
      vim.notify("No Zig formatter attached. Rebuild your system so zls is on PATH.", vim.log.levels.WARN)
    end
    return
  end

  vim.lsp.buf.format({ bufnr = bufnr, timeout_ms = 2000 })
end

vim.keymap.set("n", "<leader>zf", function()
  format_zig(true)
end, vim.tbl_extend("force", opts, { desc = "Zig: format buffer" }))

vim.api.nvim_create_autocmd("BufWritePre", {
  buffer = bufnr,
  group = vim.api.nvim_create_augroup("zig_format_" .. bufnr, { clear = true }),
  callback = function()
    format_zig(false)
  end,
})
