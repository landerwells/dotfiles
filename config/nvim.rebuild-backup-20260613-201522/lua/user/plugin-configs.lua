vim.g.tmux_navigator_no_wrap = 1

require("nvim-autopairs").setup({
  map_cr = true,
})

require("ibl").setup()

require("telescope").setup({
  defaults = {
    layout_config = {
      prompt_position = "top",
    },
    sorting_strategy = "ascending",
  },
})

require("oil").setup({
  view_options = {
    show_hidden = true,
  },
  keymaps = {
    ["<C-h>"] = false,
    ["<C-l>"] = false,
    ["<C-p>"] = false,
  },
})

require("no-neck-pain").setup({
  width = 120,
})

require("gitsigns").setup()

require("nvim-surround").setup()
