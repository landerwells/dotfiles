vim.g.mapleader = " "

vim.keymap.set("n", "<C-u>", "<C-u>zz")

vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
vim.keymap.set("n", "gd", "gdzz")

vim.keymap.set("n", "<leader>=", "gg=G<C-o>")
-- prevent x from copying over Vim clipboard
vim.keymap.set('n', 'x', '"_x')

-- Quick way to open register menu
vim.keymap.set("n", "<leader>r", vim.cmd.reg)

-- indent and outdent lines in visual mode
vim.keymap.set('v', '<TAB>', '<S->>gv')
vim.keymap.set('v', '<S-TAB>', '<S-<>gv')

-- the greatest remap ever (Primeagen)
vim.keymap.set('v', '<leader>p', '"_dP')

-- keep cursor at front when appending lines below
vim.keymap.set('n', 'x', '"_x')

-- creates a new line below the cursor and goes back into normal mode
vim.keymap.set('n', '<CR>', 'o<Esc>')

vim.keymap.set("i", "<C-BS>", "<C-w>")

vim.keymap.set("n", "<leader>w", ":w<CR>")
vim.keymap.set("n", "<leader>q", ":q<CR>")
vim.keymap.set("n", "<leader>v", vim.cmd.vs)
vim.keymap.set("n", "<leader>hs", vim.cmd.split)

vim.keymap.set('n', '<C-e>', '<nop>')

vim.keymap.set('n', '<Leader>c', '<cmd>lua ToggleCopilot()<CR>', { noremap = true, silent = true })

vim.keymap.set("n", "<C-b>", function()
  require("oil").toggle_float()
end)

vim.on_key(function(char)
if vim.fn.mode() == "n" then
  vim.opt.hlsearch = vim.tbl_contains({ "<CR>", "n", "N", "*", "#", "?", "/", "z", "v" }, vim.fn.keytrans(char))
end
end, vim.api.nvim_create_namespace "auto_hlsearch")

-- Function to toggle Copilot
function ToggleCopilot()
  if vim.g.copilot_enabled == 0 then
    vim.cmd(":Copilot enable")
    vim.g.copilot_enabled = 1
    print("Copilot enabled")
  else
    vim.cmd(":Copilot disable")
    vim.g.copilot_enabled = 0
    print("Copilot disabled")
  end
end
