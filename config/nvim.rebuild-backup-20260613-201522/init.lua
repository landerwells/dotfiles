vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

local modules = {
  "options",
  "plugins",
  "theme",
  "diagnostics",
  "clipboard",
  "treesitter",
  "lsp",
  "snippets",
  "plugin-configs",
  "autocmds",
  "keymaps",
  "which-key",
  "dashboard",
}

for _, module in ipairs(modules) do
  require("user." .. module)
end
