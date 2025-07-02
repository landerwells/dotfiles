vim.g.mapleader = " "

vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "<C-d>", "<C-d>zz")

vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
vim.keymap.set("n", "gd", "gdzz")

-- the greatest remap ever (Primeagen)
vim.keymap.set('v', '<leader>p', '"_dP')

-- creates a new line below the cursor and goes back into normal mode
vim.keymap.set('n', '<CR>', 'o<Esc>')

-- Keymap for ctrl-backspace support to work like windows.
vim.keymap.set("i", "<C-BS>", "<C-w>")

vim.keymap.set("n", "<leader>v", vim.cmd.vs)
vim.keymap.set("n", "<leader>hs", vim.cmd.split)

-- vim.keymap.set('n', '<C-e>', '<nop>')
vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { noremap=true, silent=true })


vim.keymap.set('n', '<Leader>c', '<cmd>lua ToggleCopilot()<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fD', '<cmd>lua DeleteCurrentFile()<CR>', { noremap = true, silent = true })

vim.keymap.set("n", "<C-b>", function()
  local oil = require("oil")
  if vim.bo.filetype == "oil" then
    oil.close()
  else
    oil.open()
  end
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

-- Function to delete the current file with confirmation
function DeleteCurrentFile()
  local file = vim.fn.expand('%')
  if file == '' then
    print('No file to delete')
    return
  end

  local confirm = vim.fn.input('Really delete "' .. file .. '"? (y/n): ')
  if confirm:lower() == 'y' then
    vim.cmd('bdelete!')
    os.remove(file)
    print('Deleted "' .. file .. '"')
  else
    print('Cancelled')
  end
end

-- Map the function to <leader>fD

