local ls = require("luasnip")
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node
local fmta = require("luasnip.extras.fmt").fmta

return {
  s("main", fmta([[
pub fn main() !void {
    <>
}
]], { i(1) })),

  s("import", fmta([[const <> = @import("<>");]], { i(1, "std"), i(2, "std") })),

  s("std", t([[const std = @import("std");]])),

  s("print", fmta([[std.debug.print("<>", .{<>});]], { i(1), i(2) })),

  s("test", fmta([[
test "<>" {
    <>
}
]], { i(1, "description"), i(2) })),

  s("for", fmta([[
for (<>) |<>| {
    <>
}
]], { i(1), i(2, "item"), i(3) })),

  s("iferr", fmta([[
<> catch |err| {
    <>
};
]], { i(1), i(2, "return err") })),
}
