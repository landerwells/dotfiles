return {
  "nvim-lualine/lualine.nvim",
  config = function()

    local harpoon = require("harpoon")

    local function harpoon_component()
      local total_marks = harpoon:list():length()

      if total_marks == 0 then
        return ""
      end

      local current_mark = "-"


      return string.format("󱡅 %s/%d", current_mark, total_marks)
    end

    require('lualine').setup {
      options = {
        theme = 'auto', -- Doom Emacs dynamically changes themes, so 'auto' is best
        component_separators = { left = '', right = '' },
        section_separators = { left = '', right = '' },
        globalstatus = true
      },
      sections = {
        lualine_a = { 
          { 'mode', fmt = function(str) return ' ' .. str end } 
        },
        lualine_b = { 'branch', 'diff', 'diagnostics' },
        lualine_c = { { 'filename', path = 1 } },
        lualine_x = { 'encoding', 'fileformat', 'filetype' },
        lualine_y = { 'progress' },
        lualine_z = { 'location' }
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = { { 'filename', path = 1 } },
        lualine_x = { 'location' },
        lualine_y = {},
        lualine_z = {}
      }
    }

  end,
}

-- return {
--   'nvim-lualine/lualine.nvim',
--   dependencies = { 'nvim-tree/nvim-web-devicons' },
--   config = function()
--     require('lualine').setup {
--       options = { theme = 'gruvbox_dark' }
--     }
--     end
-- }
--
