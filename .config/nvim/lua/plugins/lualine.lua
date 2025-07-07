return {
  "nvim-lualine/lualine.nvim",
  config = function()

    local colors = {
      bg       = '#35302f',
      yellow   = '#fabd2f',
      aqua     = '#8ec07c',
      green    = '#b8bb26',
      orange   = '#fe8019',
      red      = '#fb4934',
      white    = '#fbf1c7'
    }

    local gruvbox_theme = {
      normal = {
        -- a = { fg = colors.black, bg = colors.violet },
        -- b = { fg = colors.white, bg = colors.grey },
        c = { bg = colors.bg },
      },
    }

    -- insert = { a = { fg = colors.black, bg = colors.blue } },
    -- visual = { a = { fg = colors.black, bg = colors.cyan } },
    -- replace = { a = { fg = colors.black, bg = colors.red } },

    -- inactive = {
    --   a = { fg = colors.white, bg = colors.black },
    --   b = { fg = colors.white, bg = colors.black },
    --   c = { fg = colors.white },
    -- },

    -- local harpoon = require("harpoon")
    --
    -- local function harpoon_component()

    -- Need to get gruvbox colors in here and create a better theme

    -- local total_marks = harpoon:list():length()
    --
    -- if total_marks == 0 then
    --   return ""
    -- end
    --
    -- local current_mark = "-"


    --   return string.format("󱡅 %s/%d", current_mark, total_marks)
    -- end

    require('lualine').setup {
      options = {
        theme = 'auto',
        section_separators = { left = '', right = '' },
        component_separators = { left = '', right = '' }
      },
      sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {
          {
            function()
              local mode_map = {
                ['n'] = '󰰓',    -- Normal
                ['no'] = '󰰓',   -- Operator-pending
                ['v'] = '󰰫',    -- Visual
                ['V'] = '󰰫',    -- Visual Line
                [''] = '󰰫',  -- Visual Block
                ['i'] = '󰰄',    -- Insert
                ['R'] = '󰰄',    -- Replace
                ['c'] = '󰰓',    -- Command
                ['t'] = '󰰓',    -- Terminal
              }
              return mode_map[vim.fn.mode()] or '󰰓'
            end,
            color = function()
              local mode_color = {
                n = colors.aqua,      -- Normal
                no = colors.white,    -- Operator-pending
                v = colors.yellow,    -- Visual
                V = colors.yellow,    -- Visual Line
                [''] = colors.yellow, -- Visual Block
                i = colors.red,       -- Insert
                R = colors.red,       -- Replace
                c = colors.aqua,      -- Command
                t = colors.aqua,      -- Terminal
              }
              return { fg = mode_color[vim.fn.mode()] or colors.aqua, gui = 'bold' }
            end
          },
          'filesize',
          {
            'filename',
            color = { fg = colors.yellow }
          },
          'location',
          'diagnostics',
          {
            'selectioncount',
            color = { fg = colors.yellow }
          }
        },
        lualine_x = {
          'copilot',
          'filetype',
          'diff',
          {
            'branch',
            icon = '',
            color = { fg = colors.aqua }
          }
        },
        lualine_y = {},
        lualine_z = {}
      }
    }
  end,

}
