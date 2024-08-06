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
-- vim.opt.termguicolors = true
vim.opt.updatetime = 50
vim.opt.backspace = "indent,eol,start"
vim.opt.cursorline = true
vim.opt.winblend = 0
vim.opt.pumheight = 10
vim.opt.pumblend = 10
vim.opt.showmode = false
vim.opt.numberwidth = 4
vim.opt.conceallevel = 1

vim.cmd [[ highlight clear ]]
vim.g.copilot_enabled = 0

vim.on_key(function(char)
  if vim.fn.mode() == "n" then
    vim.opt.hlsearch = vim.tbl_contains({ "<CR>", "n", "N", "*", "#", "?", "/" }, vim.fn.keytrans(char))
  end
end, vim.api.nvim_create_namespace "auto_hlsearch")


-- local a = vim.api
--
-- a.nvim_create_autocmd("UIEnter", {
--   group = a.nvim_create_augroup("set_terminal_bg", {}),
--   callback = function()
--     local hl = a.nvim_get_hl(0, { name = "Normal", rgb = true })
--     local bg = hl and hl.background or nil
--
--     if not bg then
--       return
--     end
--
--     local fmt = string.format
--
--     if os.getenv("TMUX") then
--       bg = fmt('printf "\\ePtmux;\\e\\033]11;#%06x\\007\\e\\\\"', bg)
--     else
--       bg = fmt('printf "\\033]11;#%06x\\007"', bg)
--     end
--
--     os.execute(bg)
--     return true
--   end,
-- })

-- Enable true colors
vim.opt.termguicolors = true

-- Define an autocmd to reset background colors to default
vim.api.nvim_create_autocmd("UIEnter", {
  group = vim.api.nvim_create_augroup("reset_terminal_colors", {}),
  callback = function()
    -- Reset background color to default terminal color
    vim.cmd("highlight Normal guibg=NONE guifg=NONE")
    vim.cmd("highlight NonText guibg=NONE guifg=NONE")
    vim.cmd("highlight LineNr guibg=NONE guifg=NONE")
    -- Add other highlight groups as needed
  end,
})

