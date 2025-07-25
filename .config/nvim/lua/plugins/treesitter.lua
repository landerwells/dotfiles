return {
  {
    'nvim-treesitter/nvim-treesitter',
    event = { 'BufReadPre', 'BufNewFile' },
    cmd = { 'TSInstallInfo', 'TSInstall' },
    config = function()
      local status_ok, treesitter = pcall(require, 'nvim-treesitter.configs')

      if not status_ok then
        return
      end

      treesitter.setup({
        ensure_installed = { "vimdoc", "query", "cpp", "java", "ruby", "lua", "markdown", "markdown_inline", "c", "vim", "python", "elixir", "html" },
        sync_install = false,
        auto_install = true,

        ignore_install = {},

        modules = {},

        highlight = {
          enable = true,
          additional_vim_regex_highlighting = false,
        },
        context_commentstring = {
          enable = true,
          autocmd = false
        },
        indent = {
          enable = true,
          disable = { 'rust' }
        }
      })
    end

  }
}
