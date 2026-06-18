local M = {}

local source_suffixes = {
  ".cpp",
  ".c",
  ".frag",
  ".server.ts",
  ".js",
  ".ts",
  ".jsx",
  ".tsx",
  ".py",
  ".java",
  ".rs",
  ".go",
  ".css",
  ".scss",
  ".less",
}

local paired_suffixes = {
  ".h",
  ".hpp",
  ".hh",
  ".vert",
  ".svelte",
  ".html",
  ".vue",
  ".component.ts",
  ".component.js",
  ".types.ts",
  ".interface.ts",
  ".d.ts",
  ".test.py",
  ".spec.ts",
  ".spec.js",
  ".test.js",
  ".test.ts",
}

local function strip_suffix(path, suffix)
  if path:sub(-#suffix) == suffix then
    return path:sub(1, #path - #suffix)
  end
end

local function find_match(path, from_suffixes, to_suffixes)
  for _, suffix in ipairs(from_suffixes) do
    local stem = strip_suffix(path, suffix)
    if stem then
      for _, target_suffix in ipairs(to_suffixes) do
        local target = stem .. target_suffix
        if vim.fn.filereadable(target) == 1 then
          return target
        end
      end
    end
  end
end

function M.jump_pair()
  local current = vim.fn.expand("%:p")
  local target = find_match(current, source_suffixes, paired_suffixes)
    or find_match(current, paired_suffixes, source_suffixes)

  if target then
    vim.cmd.edit(vim.fn.fnameescape(target))
  else
    vim.notify("Corresponding file not found", vim.log.levels.INFO)
  end
end

return M
