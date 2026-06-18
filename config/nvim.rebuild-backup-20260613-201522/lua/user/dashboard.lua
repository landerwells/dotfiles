local art = require("user.art")

local function center(lines)
  local width = vim.o.columns
  local centered = {}
  for _, line in ipairs(lines) do
    local padding = math.max(math.floor((width - vim.fn.strdisplaywidth(line)) / 2), 0)
    table.insert(centered, string.rep(" ", padding) .. line)
  end
  return centered
end

local function open_dashboard()
  if vim.fn.argc() > 0 then
    return
  end

  local buf = vim.api.nvim_get_current_buf()
  if vim.api.nvim_buf_get_name(buf) ~= "" or vim.bo[buf].buftype ~= "" then
    return
  end
  if vim.api.nvim_buf_line_count(buf) > 1 or vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1] ~= "" then
    return
  end

  local menu = {
    "",
    "[f] find files   [g] live grep   [r] recent files",
    "[c] edit config   [u] update plugins   [q] quit",
  }

  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, center(vim.list_extend(vim.deepcopy(art), menu)))
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "dashboard"
  vim.bo[buf].modifiable = false

  vim.wo.number = false
  vim.wo.relativenumber = false
  vim.wo.signcolumn = "no"
  vim.wo.colorcolumn = ""

  vim.api.nvim_buf_set_keymap(buf, "n", "q", "<cmd>quit<cr>", { silent = true, nowait = true, desc = "Quit" })
  vim.keymap.set("n", "f", function()
    vim.cmd.enew()
    require("telescope.builtin").find_files()
  end, { buffer = buf, silent = true, nowait = true, desc = "Find files" })
  vim.keymap.set("n", "g", function()
    vim.cmd.enew()
    require("telescope.builtin").live_grep()
  end, { buffer = buf, silent = true, nowait = true, desc = "Live grep" })
  vim.keymap.set("n", "r", function()
    vim.cmd.enew()
    require("telescope.builtin").oldfiles()
  end, { buffer = buf, silent = true, nowait = true, desc = "Recent files" })
  vim.keymap.set("n", "c", function()
    vim.cmd.edit(vim.fn.stdpath("config") .. "/init.lua")
  end, { buffer = buf, silent = true, nowait = true, desc = "Edit config" })
  vim.keymap.set("n", "u", function()
    vim.pack.update()
  end, { buffer = buf, silent = true, nowait = true, desc = "Update plugins" })
end

vim.api.nvim_create_autocmd("VimEnter", {
  callback = open_dashboard,
})
