vim.opt.autoindent = true           -- Enable auto-indentation
vim.opt.expandtab = true            -- Converts tabs to spaces
vim.opt.smartindent = true          -- Makes indenting smart
vim.opt.smarttab = true             -- Makes tabbing smarter will realize you have 2 vs 4
vim.opt.shiftround = true           -- Round indent
vim.opt.shiftwidth = 2              -- Size of an indent
vim.opt.softtabstop = 2             -- Number of spaces tabs count for  
vim.opt.tabstop = 2                 -- Number of spaces tabs count for

vim.opt.nu = true                  -- Show line numbers
vim.opt.relativenumber = true      -- Show relative line numbers
vim.opt.wrap = false               -- Disable line wrapping
vim.opt.colorcolumn = "81"
vim.opt.signcolumn = "yes"

vim.opt.hlsearch = true
vim.opt.ignorecase = true
vim.opt.incsearch = true vim.opt.smartcase = true

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.cache/vim/undodir"
vim.opt.undofile = true

vim.opt.isfname:append("@-@")
vim.opt.backspace = "indent,eol,start"
vim.opt.conceallevel = 1
vim.opt.cursorline = true
vim.opt.numberwidth = 4
vim.opt.pumblend = 10
vim.opt.pumheight = 12
vim.opt.scrolloff = 8
vim.opt.showmode = false
vim.opt.sidescrolloff = 8
vim.opt.spell = true
vim.opt.termguicolors = true
vim.opt.updatetime = 50
vim.opt.winblend = 0

vim.pack.add({
  { src = "https://github.com/morhetz/gruvbox.git" },
  { src = "https://github.com/stevearc/oil.nvim" },
  { src = "https://github.com/tpope/vim-surround.git" },
  { src = "https://github.com/windwp/nvim-autopairs.git" },
})

require("nvim-autopairs").setup({
  map_cr = true,
})

require "oil".setup({
  view_options = {
    show_hidden = true,
  },
  keymaps = {
    ["<C-h>"] = false,
    ["<C-l>"] = false,
    ["<C-p>"] = false,
  },
})

local map = vim.keymap.set
vim.g.mapleader = " "

map("n", "-", ":Oil<CR>")
map("n", "<C-u>", "<C-u>zz")
map("n", "<C-d>", "<C-d>zz")
map("n", "n", "nzz")
map("n", "N", "Nzz")
map('v', '<leader>p', '"_dP')
map("i", "<C-BS>", "<C-w>")

-- map("n", "<C-b>", function()
--   local oil = require("oil")
--   if vim.bo.filetype == "oil" then
--     oil.close()
--   else
--     oil.open()
--   end
-- end)

-- File operations
-- vim.keymap.set('n', '<leader>fD', '<cmd>lua DeleteCurrentFile()<CR>', { noremap = true, silent = true })
map("n", "<leader>o", ":lua JumpPair()<CR>", { silent = true })
-- Change directory to the current file's directory
map("n", "<leader>cd", '<cmd>lua vim.fn.chdir(vim.fn.expand("%:p:h"))<CR>')

-- ~/.config/nvim-new/plugin/keymaps.lua
map("n", "<leader>ps", '<cmd>lua vim.pack.update()<CR>')

-- CUSTOM FUNCTIONS
function JumpPair()
  local ext = vim.fn.expand("%:e")
  local source_exts = { "cpp", "c", "frag", "server.ts", "js", "ts", "jsx", "tsx", "py", "java", "rs", "go", "css",
  "scss", "less" }
  local header_exts = { "h", "hpp", "hh", "vert", "svelte", "html", "vue", "component.ts", "component.js", "types.ts",
  "interface.ts", "d.ts", "test.py", "spec.ts", "spec.js", "test.js", "test.ts" }
  local target_exts = nil
  if vim.tbl_contains(header_exts, ext) then
    target_exts = source_exts
  elseif vim.tbl_contains(source_exts, ext) then
    target_exts = header_exts
  else
    print("Not a recognized file pair.")
    return
  end

  local base_name = vim.fn.expand("%:r")
  for _, target_ext in ipairs(target_exts) do
    local target_file = base_name .. "." .. target_ext
    if vim.fn.filereadable(target_file) == 1 then
      vim.cmd("edit " .. target_file)
      return
    end
  end

  print("Corresponding file not found.")
end

-- AUTOCOMMANDS AND UTILITIES

-- Auto hlsearch toggle
vim.on_key(function(char)
  if vim.fn.mode() == "n" then
    vim.opt.hlsearch = vim.tbl_contains({ "<CR>", "n", "N", "*", "#", "?", "/", "z", "v" }, vim.fn.keytrans(char))
  end
end, vim.api.nvim_create_namespace "auto_hlsearch")

