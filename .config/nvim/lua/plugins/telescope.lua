return {
  'nvim-telescope/telescope.nvim',
  event = 'VimEnter',
  dependencies = {
    'nvim-lua/plenary.nvim'
  },
  config = function ()
    local status_ok, telescope = pcall(require, 'telescope')

    if not status_ok then
      return
    end

    telescope.setup()
    local builtin = require('telescope.builtin')

    vim.keymap.set('n', '<C-p>', builtin.find_files, {})
    vim.keymap.set('n', '<C-f>', builtin.live_grep, {})

    require('telescope').setup{
      defaults = {
        layout_strategy = 'bottom_pane',
        layout_config = {
          height = 0.4, -- Adjust the height as needed (40% of the screen)
          prompt_position = 'top',
        },
        sorting_strategy = 'ascending', -- Show results from top to bottom
      }
    }

  end
}
