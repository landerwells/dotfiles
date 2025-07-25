vim.g.mapleader = " "

vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "<C-d>", "<C-d>zz")

vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
-- vim.keymap.set("n", "gd", "gdzz")
--
-- the greatest remap ever (Primeagen)
vim.keymap.set('v', '<leader>p', '"_dP')

-- Keymap for ctrl-backspace support to work like windows.
vim.keymap.set("i", "<C-BS>", "<C-w>")

-- vim.keymap.set("n", "<leader>v", vim.cmd.vs)
-- vim.keymap.set("n", "<leader>hs", vim.cmd.split)

vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { noremap=true, silent=true })

vim.keymap.set("n", "<leader>k", function() vim.diagnostic.open_float() end, opts)

vim.on_key(function(char)
  if vim.fn.mode() == "n" then
    vim.opt.hlsearch = vim.tbl_contains({ "<CR>", "n", "N", "*", "#", "?", "/", "z", "v" }, vim.fn.keytrans(char))
  end
end, vim.api.nvim_create_namespace "auto_hlsearch")

-- Function to delete the current file with confirmation
function DeleteCurrentFile()
  local file = vim.fn.expand('%')
  if file == '' then
    print('No file to delete')
    return
  end

  local confirm = vim.fn.input('Really delete "' .. file .. '"? (y/n): ')
  if confirm:lower() == 'y' then
    vim.cmd('bdelete!')
    os.remove(file)
    print('Deleted "' .. file .. '"')
  else
    print('Cancelled')
  end
end

vim.keymap.set('n', '<leader>fD', '<cmd>lua DeleteCurrentFile()<CR>', { noremap = true, silent = true })

function JumpPair()
    local ext = vim.fn.expand("%:e")
    local source_exts = { "cpp", "c", "frag", "server.ts", "js", "ts", "jsx", "tsx", "py", "java", "rs", "go", "css",
        "scss", "less" }
    local header_exts = { "h", "hpp", "hh", "vert", "svelte", "html", "vue", "component.ts", "component.js", "types.ts",
        "interface.ts", "d.ts", "test.py", "spec.ts", "spec.js", "test.js", "test.ts" }
    local target_exts = nil
    if vim.tbl_contains(header_exts, ext) then
        target_exts = source_exts
    elseif vim.tbl_contains(source_exts, ext) then
        target_exts = header_exts
    else
        print("Not a recognized file pair.")
        return
    end

    local base_name = vim.fn.expand("%:r")
    for _, target_ext in ipairs(target_exts) do
        local target_file = base_name .. "." .. target_ext
        if vim.fn.filereadable(target_file) == 1 then
            vim.cmd("edit " .. target_file)
            return
        end
    end

    print("Corresponding file not found.")
end

-- Key mapping to jump between header and source files
vim.keymap.set("n", "<leader>o", ":lua JumpPair()<CR>", { silent = true })

local is_wayland = os.getenv("WAYLAND_DISPLAY") ~= nil

if is_wayland then
  vim.g.clipboard = {
    name = "wl-clipboard",
    copy = {
      ["+"] = "wl-copy",
      ["*"] = "wl-copy"
    },
    paste = {
      ["+"] = "wl-paste --no-newline",
      ["*"] = "wl-paste --no-newline"
    },
    cache_enabled = 0,
  }
end
