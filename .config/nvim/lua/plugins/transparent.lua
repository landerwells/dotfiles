return {
  'xiyaowong/transparent.nvim',
  config = function()
    require("transparent").setup({
      enable = true, -- Enable transparency by default
    })
  end
}

