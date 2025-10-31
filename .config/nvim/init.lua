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
vim.opt.incsearch = true vim.opt.smartcase = true

-- Undo
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.cache/vim/undodir"
vim.opt.undofile = true

-- Misc
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
  { src = "https://github.com/vague-theme/vague.nvim.git" },
  { src = "https://github.com/shortcuts/no-neck-pain.nvim.git" },
  { src = "https://github.com/stevearc/oil.nvim" },
  { src = "https://github.com/christoomey/vim-tmux-navigator.git" },
  { src = "https://github.com/mrcjkb/rustaceanvim.git" },
  { src = "https://github.com/lukas-reineke/indent-blankline.nvim.git" },
  { src = "https://github.com/chomosuke/typst-preview.nvim" },
  { src = "https://github.com/lewis6991/gitsigns.nvim.git" },
  { src = "https://github.com/mbbill/undotree.git" },
  { src = "https://github.com/windwp/nvim-autopairs.git" },
  { src = "https://github.com/windwp/nvim-ts-autotag.git" },
  { src = "https://github.com/L3MON4D3/LuaSnip" },
  { src = 'https://github.com/neovim/nvim-lspconfig' },
  { src = "https://github.com/nvim-treesitter/nvim-treesitter", version = "main" },
  { src = "https://github.com/nvim-telescope/telescope.nvim.git" },
  { src = "https://github.com/nvim-lua/plenary.nvim.git" },
  { src = "https://github.com/HiPhish/rainbow-delimiters.nvim.git" },
  { src = "https://github.com/tpope/vim-surround.git" },
  { src = "https://github.com/mfussenegger/nvim-dap.git"},
})

require "nvim-autopairs".setup()
require "nvim-ts-autotag".setup()
require "ibl".setup()

require "telescope".setup({
  defaults = {
    layout_strategy = 'bottom_pane',
    layout_config = {
      height = 0.4, -- Adjust the height as needed (40% of the screen)
      prompt_position = 'top',
    },
    sorting_strategy = 'ascending', -- Show results from top to bottom
  },
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

require "no-neck-pain".setup({
  width = 120,
})

vim.cmd "colorscheme vague"
vim.cmd ":hi statusline guibg=NONE"
vim.cmd([[let g:tmux_navigator_no_wrap = 1]])

require "luasnip".setup({ enable_autosnippets = true })
require "luasnip.loaders.from_lua".load({ paths = "~/.config/nvim/snippets/" })

local map = vim.keymap.set
vim.g.mapleader = " "

-- Navigation
map("n", "<C-u>", "<C-u>zz")
map("n", "<C-d>", "<C-d>zz")
map("n", "n", "nzz")
map("n", "N", "Nzz")

-- Editing
map('v', '<leader>p', '"_dP')
map("i", "<C-BS>", "<C-w>")

-- LSP
map('n', 'gd', vim.lsp.buf.definition, { noremap=true, silent=true })
map("n", "<leader>k", function() vim.diagnostic.open_float() end)

-- Telescope
local builtin = require('telescope.builtin')
map('n', '<C-p>', builtin.find_files)
map('n', '<C-f>', builtin.live_grep)

-- Oil file explorer
map("n", "<C-b>", function()
  local oil = require("oil")
  if vim.bo.filetype == "oil" then
    oil.close()
  else
    oil.open()
  end
end)

-- Plugin toggles
map("n", "<leader>z", vim.cmd.NoNeckPain)
map("n", "<leader>u", vim.cmd.UndotreeToggle)

-- LuaSnip
local ls = require("luasnip")
map("i", "<C-e>", function() ls.expand_or_jump(1) end, { silent = true })
map({ "i", "s" }, "<C-J>", function() ls.jump(1) end, { silent = true })
map({ "i", "s" }, "<C-K>", function() ls.jump(-1) end, { silent = true })


-- File operations
vim.keymap.set('n', '<leader>fD', '<cmd>lua DeleteCurrentFile()<CR>', { noremap = true, silent = true })
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

local is_wayland = os.getenv("WAYLAND_DISPLAY") ~= nil
local is_ssh = os.getenv("SSH_CLIENT") ~= nil or os.getenv("SSH_CONNECTION") ~= nil

if is_ssh then
  vim.g.clipboard = 'osc52'
elseif is_wayland then
  vim.g.clipboard = {
    name = "wl-clipboard",
    copy = {
      ["+"] = "wl-copy",
      ["*"] = "wl-copy"
    },
    paste = {
      ["+"] = "wl-paste --no-newline",
      ["*"] = "wl-paste --no-newline"
    },
    cache_enabled = 0,
  }
end

-- Diagnostic configuration
vim.diagnostic.config({
  virtual_text = true,
  update_in_insert = true,
  underline = true,
  severity_sort = true,
  float = {
    focusable = true,
    style = "minimal",
    border = "rounded",
    source = true,
    header = "",
    prefix = "",
  },
  signs = {
    text = {
      [vim.diagnostic.severity.HINT] = " ",
      [vim.diagnostic.severity.INFO] = " ",
      [vim.diagnostic.severity.WARN] = " ",
      [vim.diagnostic.severity.ERROR] = " ",
    },
  },
})

vim.lsp.enable(
  {
    "clangd",
    "lua_ls",
    "rust_analyzer",
    "nixd",
  }
)

map('i', '<c-e>', function() vim.lsp.completion.get() end)

vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('my.lsp', {}),
  callback = function(args)
    local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
    if client:supports_method('textDocument/completion') then
      -- Optional: trigger autocompletion on EVERY keypress. May be slow!
      local chars = {}; for i = 32, 126 do table.insert(chars, string.char(i)) end
      client.server_capabilities.completionProvider.triggerCharacters = chars
      vim.lsp.completion.enable(true, client.id, args.buf, { autotrigger = true })
    end
  end,
})

-- map('n', '<leader>lf', vim.lsp.buf.format)
vim.cmd [[set completeopt+=menuone,noselect,popup]]

-- map({ "n", "v", "x" }, ";", ":", { desc = "Self explanatory" })
-- map({ "n", "v", "x" }, ":", ";", { desc = "Self explanatory" })
vim.keymap.set("n", "<leader>p", "<cmd>TypstPreviewToggle<CR>", { desc = "Toggle Typst Preview" })
--
