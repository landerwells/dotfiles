-- htop.lua
rex.session.new_window{
  layout = rex.layout.block{
    flavor  = "com.superlogical.terminal.shell",
    options = { command = { "htop" } },
  },
}
