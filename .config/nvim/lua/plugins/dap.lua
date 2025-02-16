return {
  'rcarriga/nvim-dap-ui',
  dependencies = {
    "mfussenegger/nvim-dap",
    "nvim-neotest/nvim-nio"
  },
  config = function()
    local dap, dapui = require("dap"), require("dapui")


    dap.adapters.mix_task = {
      type = 'executable',
      command = '/opt/homebrew/Cellar/elixir-ls/0.26.2/libexec/debug_adapter.sh', -- debug_adapter.bat for windows
      args = {}
    }

    dap.configurations.elixir = {
      {
        type = "mix_task",
        name = "mix test",
        task = 'test',
        taskArgs = {"--trace"},
        request = "launch",
        startApps = true, -- for Phoenix projects
        projectDir = "${workspaceFolder}",
        requireFiles = {
          "test/**/test_helper.exs",
          "test/**/*_test.exs"
        }
      },
    }

    require('dapui').setup()
    dap.listeners.after.event_initialized["dapui_config"] = function()
      dapui.open()
    end
    dap.listeners.before.event_terminated["dapui_config"] = function()
      dapui.close()
    end
    dap.listeners.before.event_exited["dapui_config"] = function()
      dapui.close()
    end

    -- Need to configure better binds for Dap
    vim.keymap.set("n", "<Leader>b", ':DapToggleBreakpoint<CR>')
    vim.keymap.set("n", "<Leader>dc", ':DapContinue<CR>')
    vim.keymap.set("n", "<Leader>x", ':DapTerminate<CR>')
    vim.keymap.set("n", "<Leader>so", ':DapStepOver<CR>')

    vim.fn.sign_define('DapBreakpoint', {text = '●', texthl = 'Error', linehl = '', numhl = ''})
  end
}
