local is_wayland = os.getenv("WAYLAND_DISPLAY") ~= nil
local is_ssh = os.getenv("SSH_CLIENT") ~= nil or os.getenv("SSH_CONNECTION") ~= nil

if is_ssh then
  vim.g.clipboard = "osc52"
elseif is_wayland then
  vim.g.clipboard = {
    name = "wl-clipboard",
    copy = {
      ["+"] = "wl-copy",
      ["*"] = "wl-copy",
    },
    paste = {
      ["+"] = "wl-paste --no-newline",
      ["*"] = "wl-paste --no-newline",
    },
    cache_enabled = 0,
  }
end
