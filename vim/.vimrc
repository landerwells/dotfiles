set number
set relativenumber

syntax on


set autoindent
set expandtab
set smartindent
set smarttab
set shiftround
set shiftwidth=2
set softtabstop=2
set tabstop=2

" vim.opt.wrap = false               -- Disable line wrapping
" vim.opt.colorcolumn = "81"
" vim.opt.signcolumn = "yes"
" 
set hlsearch 
set ignorecase
set incsearch

set noswapfile
set undofile
set undodir="~/.cache/vim/undodir"
" 
" vim.opt.isfname:append("@-@")
" vim.opt.backspace = "indent,eol,start"
" vim.opt.conceallevel = 1
" vim.opt.cursorline = true
" vim.opt.numberwidth = 4
" vim.opt.pumblend = 10
" vim.opt.pumheight = 12
" vim.opt.scrolloff = 8
" vim.opt.showmode = false
" vim.opt.sidescrolloff = 8
" vim.opt.spell = true
" vim.opt.termguicolors = false
" vim.opt.updatetime = 50
" vim.opt.winblend = 0

" https://superuser.com/questions/632657/how-to-setup-vim-to-edit-both-makefile-and-normal-code-files-with-two-different
" enable filetype detection:
filetype on
filetype plugin on
filetype indent on " file type based indentation

autocmd FileType mail set formatoptions+=t textwidth=72 " enable wrapping in mail
autocmd FileType human set formatoptions-=t textwidth=0 " disable wrapping in txt

" for C-like  programming where comments have explicit end
" characters, if starting a new line in the middle of a comment automatically
" insert the comment leader characters:
autocmd FileType c,cpp,java set formatoptions+=ro
autocmd FileType c set omnifunc=ccomplete#Complete

" fixed indentation should be OK for XML and CSS. People have fast internet
" anyway. Indentation set to 2.
autocmd FileType html,xhtml,css,xml,xslt set shiftwidth=2 softtabstop=2

" two space indentation for some files
autocmd FileType vim,lua,nginx set shiftwidth=2 softtabstop=2

" for CSS, also have things in braces indented:
autocmd FileType css set omnifunc=csscomplete#CompleteCSS

" add completion for xHTML
autocmd FileType xhtml,html set omnifunc=htmlcomplete#CompleteTags

" add completion for XML
autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags

" in makefiles, don't expand tabs to spaces, since actual tab characters are
" needed, and have indentation at 8 chars to be sure that all indents are tabs
" (despite the mappings later):
autocmd FileType make set noexpandtab shiftwidth=8 softtabstop=0

" ensure normal tabs in assembly files
" and set to NASM syntax highlighting
autocmd FileType asm set noexpandtab shiftwidth=8 softtabstop=0 syntax=nasm
