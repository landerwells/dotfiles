local map = vim.keymap.set
local builtin = require("telescope.builtin")
local util = require("user.util")

local function opts(desc)
  return { noremap = true, silent = true, desc = desc }
end

-- Navigation that keeps context centered after large jumps/searches.
map("n", "<C-u>", "<C-u>zz", opts("Half-page up and center"))
map("n", "<C-d>", "<C-d>zz", opts("Half-page down and center"))
map("n", "n", "nzz", opts("Next search result and center"))
map("n", "N", "Nzz", opts("Previous search result and center"))

-- Editing helpers.
map("v", "<leader>p", '"_dP', opts("Paste over selection without yanking it"))
map("i", "<C-BS>", "<C-w>", opts("Delete previous word"))

-- Find/search.
map("n", "<leader>f", builtin.find_files, opts("Find files"))
map("n", "<C-f>", builtin.live_grep, opts("Live grep"))
map("n", "<leader>r", builtin.oldfiles, opts("Recent files"))

-- Oil file explorer.
map("n", "<C-b>", function()
  local oil = require("oil")
  if vim.bo.filetype == "oil" then
    oil.close()
  else
    oil.open()
  end
end, opts("Toggle Oil file explorer"))

-- Plugin toggles/actions.
map("n", "<leader>z", vim.cmd.NoNeckPain, opts("Toggle centered editing"))
map("n", "<leader>u", vim.cmd.UndotreeToggle, opts("Toggle undo tree"))
map("n", "<leader>ps", function()
  vim.pack.update()
end, opts("Sync/update vim.pack plugins"))

-- Snippets + native LSP completion share <C-e>.
map("i", "<C-e>", function()
  local ok, luasnip = pcall(require, "luasnip")
  if ok and luasnip.expand_or_locally_jumpable and luasnip.expand_or_locally_jumpable() then
    luasnip.expand_or_jump()
  elseif ok and luasnip.expand_or_jumpable and luasnip.expand_or_jumpable() then
    luasnip.expand_or_jump()
  else
    vim.lsp.completion.get()
  end
end, opts("Expand snippet or trigger LSP completion"))
map({ "i", "s" }, "<C-j>", function()
  local ok, luasnip = pcall(require, "luasnip")
  if ok and luasnip.jumpable(1) then
    luasnip.jump(1)
  end
end, opts("Snippet: next field"))
map({ "i", "s" }, "<C-k>", function()
  local ok, luasnip = pcall(require, "luasnip")
  if ok and luasnip.jumpable(-1) then
    luasnip.jump(-1)
  end
end, opts("Snippet: previous field"))

-- File/project utilities.
map("n", "<leader>o", util.jump_pair, opts("Open paired/source-test file"))
map("n", "<leader>cd", function()
  vim.fn.chdir(vim.fn.expand("%:p:h"))
  vim.notify("cwd: " .. vim.fn.getcwd())
end, opts("Change cwd to current file directory"))
