-- Indentation
vim.opt.autoindent = true           -- Enable auto-indentation
vim.opt.expandtab = true            -- Converts tabs to spaces
vim.opt.smartindent = true          -- Makes indenting smart
vim.opt.smarttab = true             -- Makes tabbing smarter will realize you have 2 vs 4
vim.opt.shiftround = true           -- Round indent
vim.opt.shiftwidth = 2              -- Size of an indent
vim.opt.softtabstop = 2             -- Number of spaces tabs count for  
vim.opt.tabstop = 2                 -- Number of spaces tabs count for

-- Display
vim.opt.nu = true                  -- Show line numbers
vim.opt.relativenumber = true      -- Show relative line numbers
vim.opt.wrap = false               -- Disable line wrapping
vim.opt.colorcolumn = "81"
vim.opt.signcolumn = "yes"

-- Search
vim.opt.hlsearch = true
vim.opt.ignorecase = true
vim.opt.incsearch = true
vim.opt.smartcase = true

-- Undo
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.cache/vim/undodir"
vim.opt.undofile = true

-- Misc
vim.opt.scrolloff = 8
vim.opt.sidescrolloff = 8
vim.opt.isfname:append("@-@")
vim.opt.termguicolors = true
vim.opt.updatetime = 50
vim.opt.backspace = "indent,eol,start"
vim.opt.cursorline = true
vim.opt.winblend = 0
vim.opt.pumheight = 12
vim.opt.pumblend = 10
vim.opt.showmode = false
vim.opt.numberwidth = 4
vim.opt.conceallevel = 1
vim.opt.spell = true

vim.cmd [[ highlight clear ]]
