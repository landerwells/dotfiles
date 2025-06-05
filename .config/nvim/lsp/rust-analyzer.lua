vim.lsp.config['rust-analyzer'] = {
  cmd = {'rust-analyzer'},
  filetypes = {'rs'},
  root_markers = {
    'Cargo.toml',
    'rust-project.json',
    '.git'
  }
}
