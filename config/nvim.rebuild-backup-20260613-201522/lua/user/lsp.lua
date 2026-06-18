-- LSP server binaries are Nix-managed in modules/shared/packages.nix.
-- Neovim only wires them up and auto-enables the servers that exist on PATH.

vim.g.rustaceanvim = {
  server = {
    default_settings = {
      ["rust-analyzer"] = {
        cargo = { allFeatures = true },
        check = { command = "clippy" },
      },
    },
  },
}

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

vim.lsp.config("zls", {
  settings = {
    zls = {
      enable_inlay_hints = true,
      enable_snippets = true,
      warn_style = true,
    },
  },
})

local servers = {
  clangd = "clangd",
  lua_ls = "lua-language-server",
  nixd = "nixd",
  wgsl_analyzer = "wgsl-analyzer",
  zls = "zls",
}

local enabled = {}
for server, executable in pairs(servers) do
  if vim.fn.executable(executable) == 1 then
    table.insert(enabled, server)
  end
end

vim.lsp.enable(enabled)

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    local map = function(mode, lhs, rhs, desc)
      vim.keymap.set(mode, lhs, rhs, { buffer = args.buf, silent = true, desc = desc })
    end

    map("n", "gd", vim.lsp.buf.definition, "LSP: go to definition")
    map("n", "gr", vim.lsp.buf.references, "LSP: references")
    map("n", "gI", vim.lsp.buf.implementation, "LSP: implementation")
    map("n", "K", vim.lsp.buf.hover, "LSP: hover")
    map("n", "<leader>k", vim.diagnostic.open_float, "Diagnostics: current line")
    map("n", "<leader>la", vim.lsp.buf.code_action, "LSP: code action")
    map("n", "<leader>lr", vim.lsp.buf.rename, "LSP: rename")
    map("n", "<leader>lf", function()
      vim.lsp.buf.format({ async = true })
    end, "LSP: format buffer")
    map("n", "<leader>li", function()
      vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = args.buf }), { bufnr = args.buf })
    end, "LSP: toggle inlay hints")
    map("n", "[d", function()
      vim.diagnostic.jump({ count = -1, float = true })
    end, "Diagnostics: previous")
    map("n", "]d", function()
      vim.diagnostic.jump({ count = 1, float = true })
    end, "Diagnostics: next")

    if client and client.supports_method and client:supports_method("textDocument/completion") then
      vim.lsp.completion.enable(true, client.id, args.buf, { autotrigger = true })
    end
  end,
})
