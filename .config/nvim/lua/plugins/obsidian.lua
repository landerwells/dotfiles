return {
  "epwalsh/obsidian.nvim",
  version = "*",  -- recommended, use latest release instead of latest commit
  lazy = true,
  ft = "markdown",
  event = { 'BufReadPre ~/Notes/**.md' },

  dependencies = {
    -- Required.
    "nvim-lua/plenary.nvim",
  },

  opts = {

    vim.keymap.set('n', '<C-CR>', ':ObsidianFollowLink<CR>', { noremap = true, silent = true }),

    workspaces = {
      {
        name = "personal",
        path = "~/Notes",
      },
    },

    -- -- Optional, customize how markdown links are formatted.
    -- markdown_link_func = function(opts)
    --   return require("obsidian.util").markdown_link(opts)
    -- end,

    -- Either 'wiki' or 'markdown'.
    preferred_link_style = "markdown",

    -- Optional, boolean or a function that takes a filename and returns a boolean.
    -- `true` indicates that you don't want obsidian.nvim to manage frontmatter.
    disable_frontmatter = true,

    -- Optional, completion of wiki links, local markdown links, and tags using nvim-cmp.
    completion = {
      -- Set to false to disable completion.
      nvim_cmp = true,
      -- Trigger completion at 2 chars.
      min_chars = 2,
    },

    ui = {
      -- Use bullet marks for non-checkbox lists.
      bullets = { char = "•", hl_group = "ObsidianBullet" },
      external_link_icon = { char = "", hl_group = "ObsidianExtLinkIcon" },
      -- Replace the above with this if you don't have a patched font:
      -- external_link_icon = { char = "", hl_group = "ObsidianExtLinkIcon" },
      reference_text = { hl_group = "ObsidianRefText" },
      highlight_text = { hl_group = "ObsidianHighlightText" },
      tags = { hl_group = "ObsidianTag" },
      block_ids = { hl_group = "ObsidianBlockID" },
      hl_groups = {
        -- The options are passed directly to `vim.api.nvim_set_hl()`. See `:help nvim_set_hl`.
        ObsidianTodo = { bold = true, fg = "#fabd2f" },          -- Gruvbox yellow for emphasis
        ObsidianDone = { bold = true, fg = "#8ec07c" },          -- Gruvbox green for completion
        ObsidianRightArrow = { bold = true, fg = "#fabd2f" },    -- Same as ObsidianTodo for consistency
        ObsidianTilde = { bold = true, fg = "#fb4934" },         -- Gruvbox red for a strong highlight
        ObsidianImportant = { bold = true, fg = "#cc241d" },     -- Darker Gruvbox red for critical items
        ObsidianBullet = { bold = true, fg = "#8ec07c" },        -- Same as ObsidianDone for symmetry
        ObsidianRefText = { underline = true, fg = "#d65d0e" },  -- Gruvbox purple for references
        ObsidianExtLinkIcon = { fg = "#d65d0e" },                -- Same as ObsidianRefText for links
        ObsidianTag = { italic = true, fg = "#83a598" },         -- Gruvbox blue for tags
        ObsidianBlockID = { italic = true, fg = "#83a598" },     -- Same as ObsidianTag for related elements
        ObsidianHighlightText = { bg = "#3c3836" },              -- Gruvbox dark gray for highlighting
      },
    },
  },
}
