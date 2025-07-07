return {
  { "ofseed/copilot-status.nvim" },

  {
    "github/copilot.vim",
    config = function()
      -- Function to toggle Copilot
      function ToggleCopilot()
        if vim.g.copilot_enabled == 0 then
          vim.cmd(":Copilot enable")
          vim.g.copilot_enabled = 1
          print("Copilot enabled")
        else
          vim.cmd(":Copilot disable")
          vim.g.copilot_enabled = 0
          print("Copilot disabled")
        end
      end

      vim.keymap.set('n', '<Leader>c', '<cmd>lua ToggleCopilot()<CR>', { noremap = true, silent = true })
      vim.g.copilot_enabled = 0
    end,
  },
}
