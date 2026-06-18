local opt = vim.opt

-- Indentation
opt.autoindent = true
opt.expandtab = true
opt.smartindent = true
opt.smarttab = true
opt.shiftround = true
opt.shiftwidth = 2
opt.softtabstop = 2
opt.tabstop = 2

-- Display
opt.number = true
opt.relativenumber = true
opt.wrap = false
opt.colorcolumn = "81"
opt.signcolumn = "yes"
opt.cursorline = true
opt.numberwidth = 4
opt.termguicolors = true
opt.showmode = false
opt.scrolloff = 8
opt.sidescrolloff = 8
opt.pumblend = 10
opt.pumheight = 12
opt.winblend = 0

-- Search
opt.hlsearch = true
opt.ignorecase = true
opt.incsearch = true
opt.smartcase = true

-- Editing history
opt.swapfile = false
opt.backup = false
opt.undodir = vim.fn.stdpath("cache") .. "/undodir"
opt.undofile = true
vim.fn.mkdir(opt.undodir:get()[1], "p")

-- Misc
opt.backspace = "indent,eol,start"
opt.completeopt:append({ "menuone", "noselect", "popup" })
opt.conceallevel = 1
opt.isfname:append("@-@")
opt.spell = true
opt.updatetime = 50
