# Neovim config

Minimal Lua-first Neovim 0.12 setup using the built-in `vim.pack` package manager.

## Package/LSP approach

- Plugins are declared in `lua/user/plugins.lua` with `vim.pack.add`.
- LSP server binaries are managed by Nix in `modules/shared/packages.nix`.
- Neovim auto-enables any configured server whose executable is on `$PATH`:
  `clangd`, `lua_ls`, `nixd`, `wgsl_analyzer`, and `zls`.
- Rust is handled by `rustaceanvim` using the Nix-provided `rust-analyzer` from the Rust toolchain.

## Keymaps

| Key | Purpose |
| --- | --- |
| `<C-u>` / `<C-d>` | Half-page jump and recenter. |
| `n` / `N` | Next/previous search result and recenter. |
| `<leader>p` visual | Paste over selection without replacing the yank register. |
| `<C-BS>` insert | Delete previous word. |
| `<leader>f` | Telescope file finder. |
| `<C-f>` | Telescope live grep. |
| `<leader>r` | Recent files. |
| `<C-b>` | Toggle Oil file explorer. |
| `<leader>z` | Toggle No Neck Pain centered editing. |
| `<leader>u` | Toggle undo tree. |
| `<leader>ps` | Update/sync `vim.pack` plugins. |
| `<C-e>` insert | Expand/jump LuaSnip if possible, otherwise trigger native LSP completion. |
| `<C-j>` / `<C-k>` insert/select | Jump snippet fields forward/back. |
| `<leader>o` | Jump between source/header/test/component-style paired files. |
| `<leader>cd` | Set cwd to current file directory. |
| `gd`, `gr`, `gI`, `K` | LSP definition, references, implementation, hover. |
| `<leader>k` | Current diagnostic float (Rust buffers render rustaceanvim diagnostics). |
| `<leader>la`, `<leader>lr`, `<leader>lf`, `<leader>li` | LSP code action, rename, format, inlay-hint toggle. |
| `[d` / `]d` | Previous/next diagnostic. |

## Behavior mods

- Search highlighting turns on only while searching or jumping through matches.
- Wayland uses `wl-copy`/`wl-paste`; SSH sessions use OSC52 clipboard.
- Markdown buffers enable spellcheck, wrap, and linebreak.
- Treesitter parsers are installed via `nvim-treesitter` and highlighting/folding/indent are enabled on supported filetypes.
- The title screen is custom Lua and uses ASCII art derived from `~/Downloads/torilmudnew.jpg`.
