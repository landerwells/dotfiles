local parsers = {
  "bash",
  "c",
  "cpp",
  "json",
  "lua",
  "markdown",
  "markdown_inline",
  "nix",
  "rust",
  "toml",
  "vim",
  "vimdoc",
  "wgsl",
  "yaml",
  "zig",
}

require("nvim-treesitter").install(parsers)

vim.api.nvim_create_autocmd("FileType", {
  pattern = parsers,
  callback = function(args)
    pcall(vim.treesitter.start, args.buf)
    vim.wo.foldmethod = "expr"
    vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()"
    vim.wo.foldlevel = 99
    vim.o.foldlevelstart = 99
    vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
  end,
})
