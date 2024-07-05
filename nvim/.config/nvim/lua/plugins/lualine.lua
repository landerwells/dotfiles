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

    -- stylua: ignore
    local colors = {
      blue   = '#458588',
      cyan   = '#689d6a',
      black  = '#080808',
      white  = '#c6c6c6',
      red    = '#cc241d',
      violet = '#d79921',
      grey   = '#303030',
    }

    local bubbles_theme = {
      normal = {
        a = { fg = colors.black, bg = colors.violet },
        b = { fg = colors.white, bg = colors.grey },
        c = { fg = colors.white },
      },

      insert = { a = { fg = colors.black, bg = colors.blue } },
      visual = { a = { fg = colors.black, bg = colors.cyan } },
      replace = { a = { fg = colors.black, bg = colors.red } },

      inactive = {
        a = { fg = colors.white, bg = colors.black },
        b = { fg = colors.white, bg = colors.black },
        c = { fg = colors.white },
      },
    }

    require('lualine').setup {
      options = {
        theme = bubbles_theme,
        disabled_filetypes = { "oil", "" },
        component_separators = '',
        section_separators = { left = '', right = '' },
      },
      sections = {
        lualine_a = { { 'mode', separator = { left = '' }, right_padding = 2 } },
        lualine_b = {
          { "branch", icon = "" },
          "diff",
          "diagnostics",
          harpoon_component
        },lualine_c = {
          '%=', --[[ add your center compoentnts here in place of this comment ]]
        },
        lualine_x = {},
        lualine_y = { 'filetype', 'progress' },
        lualine_z = {
          { 'location', separator = { right = '' }, left_padding = 2 },
        },
      },
      inactive_sections = {
        lualine_a = { 'filename' },
        lualine_b = {},
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = { 'location' },
      },
      tabline = {},
      extensions = {},
    }  end,
}

