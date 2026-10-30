-- Basic Settings
vim.o.number = true          -- Show line numbers
vim.o.relativenumber = true  -- Show relative line numbers
vim.o.wrap = false           -- Don't wrap lines
vim.o.expandtab = true       -- Use spaces instead of tabs
vim.o.tabstop = 4            -- Number of spaces tabs count for
vim.o.shiftwidth = 4         -- Size of an indent
vim.o.smartindent = true     -- Insert indents automatically
vim.o.ignorecase = true      -- Ignore case in search patterns
vim.o.smartcase = true       -- Override ignorecase if search pattern contains upper case characters
vim.o.hlsearch = false       -- Don't highlight all matches on previous search pattern
vim.o.incsearch = true       -- Show search matches as you type
vim.o.hidden = true          -- Allow switching from unsaved buffer
vim.o.errorbells = false     -- Disable error bells
vim.o.swapfile = false       -- Don't use swapfile
vim.o.backup = false         -- Don't create backup files
vim.o.undodir = vim.fn.stdpath('config') .. '/undodir'  -- Set undodir
vim.o.undofile = true        -- Use an undo file
vim.o.termguicolors = true   -- True color support
vim.o.scrolloff = 8          -- Lines of context
vim.o.signcolumn = "yes"     -- Always show the signcolumn
vim.o.updatetime = 50        -- Decrease update time

vim.opt.cursorline = true -- show which line your cursor is on
vim.opt.scrolloff = 10 -- minimal number of screen lines to keep above and below the cursor|

-- Key mappings
vim.g.mapleader = " "  -- Set leader key to space

vim.api.nvim_set_keymap('n', '<Leader>y', ':%y+<CR>', { noremap = true, silent = true }) -- copy file into clipboard
vim.keymap.set('n', '<leader>p', function()
  local path = vim.fn.fnamemodify(vim.fn.expand('%:p'), ':.')
  if path == '' then
    path = '.'
  end

  vim.fn.setreg('+', path)
  vim.notify('Copied: ' .. path)
end, { desc = 'Copy current file path or cwd' })
vim.api.nvim_set_keymap('n', '<Leader>t', ':tabnew<CR>', { noremap = true, silent = true }) -- new tab 
vim.api.nvim_set_keymap('n', '<leader>e', ':Explore<CR>', { noremap = true, silent = true }) -- open file explorer
vim.api.nvim_set_keymap('t', '<Esc><Esc>', '<C-\\><C-n>', { noremap = true, silent = true }) -- escape from terminal mode to normal mode
vim.api.nvim_set_keymap('n', '<Leader><S-t>', ':terminal<CR>', { noremap = true, silent = true }) -- enter terminal mode
vim.api.nvim_set_keymap('n', '<Leader>w', ':w<CR>', { noremap = true, silent = true }) -- save files 
vim.api.nvim_set_keymap('n', '<Leader>q', ':q<CR>', { noremap = true, silent = true }) -- save files 


-- Bootstrap lazy.nvim plugin manager
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Plugin setup
require("lazy").setup({
  -- Telescope fuzzy finder
  {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.5',
    dependencies = { 'nvim-lua/plenary.nvim' }
  },
  -- Telescope fzf native for better performance
  {
    'nvim-telescope/telescope-fzf-native.nvim',
    build = 'make'
  },
  -- LSP configs for language servers
  { 'neovim/nvim-lspconfig' },
  -- Git diff signs in the gutter
  {
    'lewis6991/gitsigns.nvim',
    event = 'BufReadPre',
    config = function()
      require('gitsigns').setup({
        signs = {
          add          = { text = '┃' },
          change       = { text = '┃' },
          delete       = { text = '_' },
          topdelete    = { text = '‾' },
          changedelete = { text = '~' },
          untracked    = { text = '┆' },
        },
        on_attach = function(bufnr)
          local gs = package.loaded.gitsigns
          local function map(mode, l, r, opts)
            opts = opts or {}
            opts.buffer = bufnr
            vim.keymap.set(mode, l, r, opts)
          end
          -- Navigation between hunks
          map('n', ']c', function() gs.nav_hunk('next') end, { desc = 'Next git hunk' })
          map('n', '[c', function() gs.nav_hunk('prev') end, { desc = 'Previous git hunk' })
          -- Actions
          map('n', '<leader>hp', gs.preview_hunk, { desc = 'Preview hunk' })
          map('n', '<leader>hs', gs.stage_hunk, { desc = 'Stage hunk' })
          map('n', '<leader>hr', gs.reset_hunk, { desc = 'Reset hunk' })
          map('n', '<leader>hb', function() gs.blame_line({ full = true }) end, { desc = 'Blame line' })
        end,
      })
    end,
  },
})

-- Open a readable, full-width unified diff in a temporary tab.
local function open_git_diff(revision)
  local command
  local title

  if revision == '' then
    command = { 'git', 'diff', 'HEAD', '--no-ext-diff', '--no-color', '--find-renames' }
    title = 'Working tree diff'
  elseif revision == 'staged' then
    command = { 'git', 'diff', '--cached', '--no-ext-diff', '--no-color', '--find-renames' }
    title = 'Staged diff'
  elseif revision:find('..', 1, true) then
    command = { 'git', 'diff', revision, '--no-ext-diff', '--no-color', '--find-renames', '--stat', '--patch' }
    title = revision
  else
    command = {
      'git', 'show', revision, '--no-ext-diff', '--no-color', '--find-renames',
      '--format=fuller', '--stat', '--patch',
    }
    title = revision
  end

  local lines = vim.fn.systemlist(command)
  if vim.v.shell_error ~= 0 then
    vim.notify(table.concat(lines, '\n'), vim.log.levels.ERROR)
    return
  end

  vim.cmd('tabnew')
  local buffer = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_name(buffer, 'git://' .. title)
  vim.bo[buffer].buftype = 'nofile'
  vim.bo[buffer].bufhidden = 'wipe'
  vim.bo[buffer].swapfile = false
  vim.bo[buffer].filetype = 'diff'
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
  vim.bo[buffer].modifiable = false

  local function search(pattern, flags)
    vim.fn.search(pattern, flags)
  end

  vim.keymap.set('n', ']f', function() search('^diff --git', 'W') end,
    { buffer = buffer, desc = 'Next changed file' })
  vim.keymap.set('n', '[f', function() search('^diff --git', 'bW') end,
    { buffer = buffer, desc = 'Previous changed file' })
  vim.keymap.set('n', ']c', function() search('^@@', 'W') end,
    { buffer = buffer, desc = 'Next diff hunk' })
  vim.keymap.set('n', '[c', function() search('^@@', 'bW') end,
    { buffer = buffer, desc = 'Previous diff hunk' })
  vim.keymap.set('n', 'q', '<cmd>tabclose<CR>',
    { buffer = buffer, desc = 'Close diff' })
end

vim.api.nvim_create_user_command('Diff', function(options)
  open_git_diff(vim.trim(options.args))
end, {
  nargs = '?',
  desc = 'Open working-tree, staged, range, or commit diff',
})
vim.keymap.set('n', '<leader>gv', '<cmd>Diff<CR>', { desc = 'Open git diff' })

-- Telescope configuration
require('telescope').setup({
  defaults = {
    file_ignore_patterns = { "node_modules", ".git/" },
    layout_config = {
      horizontal = {
        preview_width = 0.55,
      },
    },
  },
})

-- Load telescope fzf extension
pcall(require('telescope').load_extension, 'fzf')

-- Telescope keymaps for fuzzy finding
local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = 'Fuzzy find files' })
vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = 'Fuzzy find text in files' })
vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Fuzzy find buffers' })
vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'Fuzzy find help' })
vim.keymap.set('n', '<leader>fr', builtin.oldfiles, { desc = 'Fuzzy find recent files' })


-- colorscheme
vim.cmd('colorscheme lunaperche')

-- LSP setup (ruff + ty)
-- vim.lsp.config('ruff', {
--   init_options = {
--     settings = {
--       lineLength = 88,
--     },
--   },
-- })
--
-- vim.lsp.config('ty', {
--   settings = {
--     ty = {},
--   },
-- })
--
-- vim.lsp.enable({ 'ruff', 'ty' })
--
-- -- Format on save with ruff
-- vim.api.nvim_create_autocmd('LspAttach', {
--   callback = function(args)
--     local client = vim.lsp.get_client_by_id(args.data.client_id)
--     if not client then return end
--
--     -- Disable hover for ruff (ty handles that better)
--     if client.name == 'ruff' then
--       client.server_capabilities.hoverProvider = false
--     end
--
--     -- Format on save via ruff
--     if client.name == 'ruff' then
--       vim.api.nvim_create_autocmd('BufWritePre', {
--         buffer = args.buf,
--         callback = function()
--           vim.lsp.buf.format({ async = false, name = 'ruff' })
--         end,
--       })
--     end
--   end,
-- })

-- LSP keymaps
vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(args)
    local buf = args.buf
    local opts = { buffer = buf }
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, opts)
    vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
    vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
    vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
    vim.keymap.set('n', '<leader>d', vim.diagnostic.open_float, opts)
  end,
})
