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
}
