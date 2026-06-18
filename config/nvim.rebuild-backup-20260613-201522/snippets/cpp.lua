local ls = require("luasnip")
local s = ls.snippet
local i = ls.insert_node
local f = ls.function_node
local fmt = require("luasnip.extras.fmt").fmt
local fmta = require("luasnip.extras.fmt").fmta

local function header_guard()
  local filename = vim.fn.expand("%:t")
  return string.upper(filename:gsub("%.", "_"):gsub("-", "_"))
end

return {
  s({ trig = "header", desc = "C/C++ include guard" },
    fmta([[
#ifndef <>
#define <>
<>
#endif // <>
]], { f(header_guard), f(header_guard), i(1), f(header_guard) })
  ),

  s({ trig = "for", desc = "Simple indexed for-loop" },
    fmta([[
for (int i = <>; i <>; i++) {
  <>
}
]], { i(1, "0"), i(2, "< n"), i(3) })
  ),

  s({ trig = "up", desc = "std::unique_ptr type" },
    fmt([[std::unique_ptr<{}>]], { i(1) })
  ),

  s({ trig = "mu", desc = "std::make_unique call" },
    fmt([[std::make_unique<{}>({})]], { i(1), i(2) })
  ),
}
