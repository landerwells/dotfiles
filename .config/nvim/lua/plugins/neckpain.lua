return {
  "shortcuts/no-neck-pain.nvim",

  config = function()
    local status_ok, noneckpain = pcall(require, "no-neck-pain")

    if not status_ok then
      return
    end

    vim.keymap.set("n", "<leader>z", vim.cmd.NoNeckPain)

    noneckpain.setup({
      width = 120,
    })
  end
}
