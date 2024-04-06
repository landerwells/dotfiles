return {
  "williamboman/mason.nvim",
  dependencies = {
    "williamboman/mason-lspconfig.nvim",
  },

  lazy = false,
  config = function()
    local mason = require("mason")

    local mason_lspconfig = require("mason-lspconfig")

    require("mason").setup({
      ui = {
        icons = {
          package_installed = "✓",
          package_pending = "➜",
          package_uninstalled = "✗"
        },
        border = "rounded",
      }
    })

    mason_lspconfig.setup({
      ensure_installed = {
        "lua_ls",
        "pyright",
        "clangd",
        "hls",
        "html",
        "cssls",
        "bashls",
        "jsonls",
      },



    })
  end,
}
