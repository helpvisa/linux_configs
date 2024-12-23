-- import .vim init
local vimrc = vim.fn.stdpath("config") .. "/vimrc.vim"
vim.cmd.source(vimrc)


-- setup nvim-tree
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
require("nvim-tree").setup();


-- setup nvimtree
require'nvim-treesitter.configs'.setup {
  -- A list of parser names, or "all" (the listed parsers MUST always be installed)
  ensure_installed = {
      "c",
      "lua",
      "vim",
      "vimdoc",
      "query",
      "markdown",
      "markdown_inline",
      "cpp",
      "bash",
      "javascript",
      "rust",
      "php",
      "html"
  },

  -- Install parsers synchronously (only applied to `ensure_installed`)
  sync_install = false,

  -- Automatically install missing parsers when entering buffer
  -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
  auto_install = true,

  -- List of parsers to ignore installing (or "all")
  ignore_install = { },

  ---- If you need to change the installation directory of the parsers (see -> Advanced Setup)
  -- parser_install_dir = "/some/path/to/store/parsers", -- Remember to run vim.opt.runtimepath:append("/some/path/to/store/parsers")!

  highlight = {
    enable = true,
    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
}


-- enable mini.nvim
require('mini.starter').setup()
require('mini.icons').setup()
-- require('mini.statusline').setup()
require('mini.fuzzy').setup()
require('mini.clue').setup()
require('mini.git').setup()
require('mini.diff').setup()
require('mini.map').setup()
require('mini.pick').setup()
require('mini.cursorword').setup()
require('mini.indentscope').setup()
require('mini.notify').setup()
require('mini.comment').setup()


-- custom remappings
local opts = { noremap = true, silent = true }
vim.api.nvim_set_keymap('n', '<Space>', '<Nop>', opts)
vim.g.mapleader = " "
vim.api.nvim_set_keymap('n', '<leader>e', ':NvimTreeToggle<CR>', opts)
-- pickers
vim.api.nvim_set_keymap('n', '<leader>,', ':Pick buffers<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>f', ':Pick files<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>g', ':Pick grep<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>G', ':Pick grep_live<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>h', ':Pick help<CR>', opts)
-- diagnostics
vim.keymap.set('n', '<leader>d', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, opts)
-- ale
vim.api.nvim_set_keymap('n', '<leader>a', ':ALEToggle<CR>', opts)
vim.api.nvim_set_keymap('n', 'gd', ':ALEGoToDefinition<CR>', opts)
vim.api.nvim_set_keymap('n', 'gtd', ':ALEGoToTypeDefinition<CR>', opts)
vim.api.nvim_set_keymap('n', 'gr', ':ALEFindReferences<CR>', opts)
vim.api.nvim_set_keymap('n', 'gk', ':ALEHover<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>ca', ':ALECodeAction<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>rn', ':ALERename<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>rf', ':ALEFileRename<CR>', opts)
