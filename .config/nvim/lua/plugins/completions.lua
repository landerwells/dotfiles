-- return {
--   'saghen/blink.cmp',
--   dependencies = { 'rafamadriz/friendly-snippets' },
--
--   version = '1.*',
--   opts = {
--     keymap = { preset = 'enter' },
--
--     appearance = {
--       nerd_font_variant = 'mono'
--     },
--
--     completion = { 
--       documentation = { auto_show = true },
--       trigger = {show_on_trigger_character = true },
--     },
--
--     sources = {
--       default = { 'lsp', 'path', 'snippets', 'buffer' },
--     },
--
--     fuzzy = { implementation = "prefer_rust_with_warning" }
--   },
--   opts_extend = { "sources.default" }
-- }

return {
  {
    "saghen/blink.cmp",
    dependencies = {
      "rafamadriz/friendly-snippets",
    },
    -- event = "InsertEnter",
    version = "*",
    config = function()
      -- vim.cmd('highlight Pmenu guibg=none')
      -- vim.cmd('highlight PmenuExtra guibg=none')
      -- vim.cmd('highlight FloatBorder guibg=none')
      -- vim.cmd('highlight NormalFloat guibg=none')

      require("blink.cmp").setup({
        snippets = { preset = "luasnip" },
        signature = { enabled = true },
        appearance = {
          use_nvim_cmp_as_default = false,
          nerd_font_variant = "normal",
        },
        sources = {
          per_filetype = {
            codecompanion = { "codecompanion" },
          },
          default = { "snippets", "lsp", "path", "buffer" },
          providers = {
            cmdline = {
              min_keyword_length = 2,
            },
          },
        },
        keymap = {
          ["<C-f>"] = {},
          -- preset = 'enter',
        },
        cmdline = {
          enabled = false,
          completion = { menu = { auto_show = true } },
          keymap = {
            ["<CR>"] = { "accept_and_enter", "fallback" },
          },
        },
        completion = {
          menu = {
            border = nil,
            scrolloff = 1,
            scrollbar = false,
            draw = {
              columns = {
                { "kind_icon" },
                { "label",      "label_description", gap = 1 },
                { "kind" },
                { "source_name" },
              },
            },
          },
          documentation = {
            window = {
              border = nil,
              scrollbar = false,
              winhighlight = 'Normal:BlinkCmpDoc,FloatBorder:BlinkCmpDocBorder,EndOfBuffer:BlinkCmpDoc',
            },
            auto_show = true,
            auto_show_delay_ms = 500,
          },
        },
      })

      require("luasnip.loaders.from_vscode").lazy_load()
    end,
  },
}
