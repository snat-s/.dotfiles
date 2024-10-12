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
vim.api.nvim_set_keymap('n', '<Leader>t', ':tabnew<CR>', { noremap = true, silent = true }) -- new tab 
vim.api.nvim_set_keymap('n', '<leader>e', ':Explore<CR>', { noremap = true, silent = true }) -- open file explorer
vim.api.nvim_set_keymap('t', '<Leader><Esc>', '<C-\\><C-n>', { noremap = true, silent = true }) -- escape from terminal mode to normal mode
vim.api.nvim_set_keymap('n', '<Leader><S-t>', ':terminal<CR>', { noremap = true, silent = true }) -- enter terminal mode
vim.api.nvim_set_keymap('n', '<Leader>w', ':w<CR>', { noremap = true, silent = true }) -- save files 

-- packages
local lh = require('lh')
lh.setup({
    -- You can add any custom configuration here if needed
    api_key = "", -- Your DeepSeek API key
    model = "deepseek-coder",
    system_message = "You are a helpful coding assistant. Respond ONLY in code blocks.",
})

-- colorscheme
vim.cmd('colorscheme quiet')
