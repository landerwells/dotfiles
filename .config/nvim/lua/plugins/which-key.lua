return {
  "folke/which-key.nvim",

  config = function()
    local wk = require("which-key")
    wk.setup {
      delay = 1000,
    }


    wk.add({
      { "<leader>h", group = "harpoon" },
    })

    -- wk.register({
      -- { "<leader>h", group = "harpoon" },
      -- { "<leader>ha", function() harpoon:list():add() end, desc = "Add file" },
      -- { "<leader>hb", function() harpoon.ui:toggle_quick_menu(harpoon:list()) end, desc = "File 1" },
      -- { "<leader>hd", function() harpoon:list():select(1) end, desc = "File 3" },
      -- { "<leader>hf", function() harpoon:list():select(2) end, desc = "File 4" },
      -- { "<leader>hm", function() harpoon:list():select(3) end, desc = "Menu" },
      -- { "<leader>hs", function() harpoon:list():select(4) end, desc = "File 2" },
    -- })
  end
}
