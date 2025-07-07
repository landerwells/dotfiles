return {
  {
    'windwp/nvim-autopairs',
    event = "InsertEnter",
    config = true
  },

  {
    'windwp/nvim-ts-autotag',
    event = 'InsertEnter',

    config = function()
      require('nvim-ts-autotag').setup({})
    end
  }
}
