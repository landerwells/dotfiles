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
vim.opt.termguicolors = false
vim.opt.updatetime = 50
vim.opt.winblend = 0

vim.pack.add({
  { src = "https://github.com/mrcjkb/rustaceanvim.git" },
  { src = "https://github.com/L3MON4D3/LuaSnip" },
  { src = "https://github.com/Saghen/blink.cmp.git" },
  { src = "https://github.com/christoomey/vim-tmux-navigator.git" },
  { src = "https://github.com/folke/which-key.nvim.git" },
  { src = "https://github.com/lewis6991/gitsigns.nvim.git" },
  { src = "https://github.com/lukas-reineke/indent-blankline.nvim.git" },
  { src = "https://github.com/ellisonleao/gruvbox.nvim.git" },
  { src = "https://github.com/morhetz/gruvbox.git" },
  { src = "https://github.com/nvim-lua/plenary.nvim.git" },
  { src = "https://github.com/nvim-telescope/telescope.nvim.git" },
  { src = "https://github.com/shortcuts/no-neck-pain.nvim.git" },
  { src = "https://github.com/stevearc/oil.nvim" },
  { src = "https://github.com/tpope/vim-surround.git" },
  { src = "https://github.com/windwp/nvim-autopairs.git" },
  { src = 'https://github.com/neovim/nvim-lspconfig' },
})

vim.cmd("packadd nvim.undotree")
vim.keymap.set("n", "<leader>u", require("undotree").open)


require("nvim-autopairs").setup({
  map_cr = true,
})
require "ibl".setup()

require "telescope".setup({
  defaults = {
    layout_config = {
      prompt_position = 'top',
    },
    sorting_strategy = 'ascending', -- Show results from top to bottom
  },
})

vim.filetype.add({extension = {wgsl = "wgsl", zon = "zig"}})
vim.o.foldlevelstart = 99 -- do not close folds when a buffer is opened

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

-- vim.cmd "colorscheme gruvbox"
vim.cmd ":hi statusline guibg=NONE"
vim.cmd([[let g:tmux_navigator_no_wrap = 1]])

require "luasnip".setup({ enable_autosnippets = true })
require "luasnip.loaders.from_lua".load({ paths = "~/.config/nvim/snippets/" })

require("blink.cmp").setup({
  keymap = { preset = "default" },
  snippets = { preset = "luasnip" },
  completion = {
    documentation = {
      auto_show = true,
      auto_show_delay_ms = 0,
      window = {
        border = "none",
        winblend = 10,
      },
    },
    ghost_text = { enabled = false },
    menu = {
      border = "none",
      winblend = 10,
      draw = {
        columns = {
          { "label", "label_description", gap = 1 },
          { "kind" },
        },
      },
    },
  },
  signature = {
    enabled = true,
    window = {
      border = "none",
      winblend = 10,
      show_documentation = true,
    },
  },
  sources = {
    default = { "lsp", "path", "snippets", "buffer" },
  },
  appearance = {
    nerd_font_variant = "normal",
  },
  fuzzy = { implementation = "lua" },
})

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

-- LSP mappings are buffer-local in the LspAttach autocmd below.

-- Telescope
local builtin = require('telescope.builtin')
map('n', '<leader>f', builtin.find_files)
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

-- LuaSnip fallback jumps. Blink.cmp owns completion, docs, signatures, and <Tab>/<S-Tab> snippet navigation.
local ls = require("luasnip")
map({ "i", "s" }, "<C-J>", function() ls.jump(1) end, { silent = true })
map({ "i", "s" }, "<C-K>", function() ls.jump(-1) end, { silent = true })


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

local lsp_capabilities = require("blink.cmp").get_lsp_capabilities()

vim.g.rustaceanvim = {
  server = {
    capabilities = lsp_capabilities,
    default_settings = {
      ["rust-analyzer"] = {
        cargo = { allFeatures = true },
        check = { command = "clippy" },
      },
    },
  },
}

vim.lsp.config("*", {
  capabilities = lsp_capabilities,
})

vim.lsp.config("lua_ls", {
  settings = {
    Lua = {
      runtime = { version = "LuaJIT" },
      diagnostics = { globals = { "vim" } },
      workspace = {
        checkThirdParty = false,
        library = vim.api.nvim_get_runtime_file("", true),
      },
      telemetry = { enable = false },
    },
  },
})

vim.lsp.config("nixd", {
  settings = {
    nixd = {
      formatting = { command = { "alejandra" } },
    },
  },
})

local function zig_env_value(name)
  if vim.fn.executable("zig") ~= 1 then
    return nil
  end

  local output = vim.fn.systemlist({ "zig", "env" })
  if vim.v.shell_error ~= 0 then
    return nil
  end

  local pattern = "%s*%." .. name .. "%s*=%s*\"([^\"]+)\""
  for _, line in ipairs(output) do
    local value = line:match(pattern)
    if value then
      return value
    end
  end
end

vim.lsp.config("zls", {
  settings = {
    zls = {
      enable_inlay_hints = true,
      enable_snippets = true,
      semantic_tokens = "full",
      warn_style = true,
      zig_exe_path = zig_env_value("zig_exe"),
      zig_lib_path = zig_env_value("lib_dir"),
    },
  },
})

local lsp_servers = {
  clangd = "clangd",
  lua_ls = "lua-language-server",
  nixd = "nixd",
  wgsl_analyzer = "wgsl-analyzer",
  zls = "zls",
}

local enabled_lsp_servers = {}
for server, executable in pairs(lsp_servers) do
  if vim.fn.executable(executable) == 1 then
    table.insert(enabled_lsp_servers, server)
  end
end
vim.lsp.enable(enabled_lsp_servers)

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    local function lsp_map(mode, lhs, rhs, desc)
      map(mode, lhs, rhs, { buffer = args.buf, silent = true, desc = desc })
    end

    lsp_map("n", "gd", vim.lsp.buf.definition, "LSP: go to definition")
    lsp_map("n", "gD", vim.lsp.buf.declaration, "LSP: go to declaration")
    lsp_map("n", "gr", vim.lsp.buf.references, "LSP: references")
    lsp_map("n", "gI", vim.lsp.buf.implementation, "LSP: implementation")
    lsp_map("n", "gy", vim.lsp.buf.type_definition, "LSP: type definition")
    lsp_map("n", "K", vim.lsp.buf.hover, "LSP: hover documentation")
    lsp_map({ "n", "i" }, "<M-k>", vim.lsp.buf.signature_help, "LSP: signature help")
    lsp_map("n", "<leader>k", vim.diagnostic.open_float, "Diagnostics: current line")
    lsp_map("n", "<leader>la", vim.lsp.buf.code_action, "LSP: code action")
    lsp_map("n", "<leader>lr", vim.lsp.buf.rename, "LSP: rename")
    lsp_map("n", "<leader>ls", builtin.lsp_document_symbols, "LSP: document symbols")
    lsp_map("n", "<leader>lS", builtin.lsp_dynamic_workspace_symbols, "LSP: workspace symbols")
    lsp_map("n", "<leader>lf", function()
      vim.lsp.buf.format({ bufnr = args.buf, async = true })
    end, "LSP: format buffer")
    lsp_map("n", "<leader>li", function()
      vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = args.buf }), { bufnr = args.buf })
    end, "LSP: toggle inlay hints")
    lsp_map("n", "[d", function()
      vim.diagnostic.jump({ count = -1, float = true })
    end, "Diagnostics: previous")
    lsp_map("n", "]d", function()
      vim.diagnostic.jump({ count = 1, float = true })
    end, "Diagnostics: next")

    if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint) then
      vim.lsp.inlay_hint.enable(true, { bufnr = args.buf })
    end
  end,
})

vim.cmd [[set completeopt=menu,menuone,noinsert,noselect,popup]]

