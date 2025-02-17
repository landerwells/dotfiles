return {
  {
    'iamcco/markdown-preview.nvim',
    build = 'cd app && npm install',
    -- using npm to install rather than the vim function leads to significantly faster startup time
    init = function()
      -- how to get
      -- https://github.com/iamcco/markdown-preview.nvim/issues/7
      -- :call mkdp#util#install()
      -- to run automatically on init
      vim.g.mkdp_filetypes = { 'markdown' }
    end,
    config = function()
      vim.keymap.set('n', '<leader>m', '<Plug>MarkdownPreviewToggle', { desc = 'Markdown Preview' })
    end
  },
  --   {
  --     'MeanderingProgrammer/render-markdown.nvim',
  --     dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.nvim' }, -- if you use the mini.nvim suite
  --     -- dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.icons' }, -- if you use standalone mini plugins
  --     -- dependencies = { 'nvim-treesitter/nvim-treesitter', 'nvim-tree/nvim-web-devicons' }, -- if you prefer nvim-web-devicons
  --     ---@module 'render-markdown'
  --     ---@type render.md.UserConfig
  --     opts = {},
  -- }
}
