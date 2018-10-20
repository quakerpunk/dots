" Basic Settings
set nocompatible
set backspace=indent,eol,start
" set ruler
set number
set relativenumber
set hidden
set history=100
set tabstop=4
set wrap
set ai
set linebreak
set nolist
set textwidth=0
set wrapmargin=0
set formatoptions-=t
set formatoptions+=1
set expandtab
set shiftwidth=4
set scrolloff=3
set foldcolumn=1

" Map leader to ,
let mapleader=','

" More keybindings
nnoremap ; :
nnoremap j gj
nnoremap k gk
inoremap jj <esc>
nnoremap <space> <PageDown>
nnoremap <NUL> <PageUp>
map <F1> [s
map <F2> z=
map <F3> s]
map <F4> zg
map <F5> zG


set spell
set spellsuggest=15

call plug#begin(expand('~/.vim/plugged'))
	" ## Themes
	Plug 'ayu-theme/ayu-vim'
	Plug 'vim-airline/vim-airline-themes'

	" ## Markdown
	Plug 'tpope/vim-markdown'

	" ## Other Tools
    Plug 'jistr/vim-nerdtree-tabs'
	Plug 'vim-airline/vim-airline'
    Plug 'scrooloose/nerdtree'
	Plug 'scrooloose/syntastic'
	Plug 'Raimondi/delimitmate'	" smart completion of delimiters
	Plug 'tpope/vim-fugitive'
	Plug 'reedes/vim-litecorrect'
	Plug 'reedes/vim-lexical'
    Plug 'junegunn/goyo.vim'    " Full screen writing mode
    Plug 'junegunn/limelight.vim'    " Highlights only active paragraph
call plug#end()
filetype plugin indent on

" # Plugin Settings
set laststatus=2
set foldenable		" Set folding for Markdown

" vim-airline
let g:airline_theme = 'luna'
let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#wordcount#enabled = 1
let g:airline#extensions#tagbar#enabled = 1
let g:airline_skip_empty_selctions = 1

" vim-airline
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

if !exists('g:airline_powerline_fonts')
  let g:airline#extensions#tabline#left_sep = ' '
  let g:airline#extensions#tabline#left_alt_sep = '|'
  let g:airline_left_sep          = '▶'
  let g:airline_left_alt_sep      = '»'
  let g:airline_right_sep         = '◀'
  let g:airline_right_alt_sep     = '«'
  let g:airline#extensions#branch#prefix     = '⤴' "➔, ➥, ⎇
  let g:airline#extensions#readonly#symbol   = '⊘'
  let g:airline#extensions#linecolumn#prefix = '¶'
  let g:airline#extensions#paste#symbol      = 'ρ'
  let g:airline_symbols.linenr    = '␊'
  let g:airline_symbols.branch    = '⎇'
  let g:airline_symbols.paste     = 'ρ'
  let g:airline_symbols.paste     = 'Þ'
  let g:airline_symbols.paste     = '∥'
  let g:airline_symbols.whitespace = 'Ξ'
else
  let g:airline#extensions#tabline#left_sep = ''
  let g:airline#extensions#tabline#left_alt_sep = ''

  " powerline symbols
  let g:airline_left_sep = ''
  let g:airline_left_alt_sep = ''
  let g:airline_right_sep = ''
  let g:airline_right_alt_sep = ''
  let g:airline_symbols.branch = ''
  let g:airline_symbols.readonly = ''
  let g:airline_symbols.linenr = ''
endif

" Remove existing autocommands to avoid duplication
:autocmd!

:autocmd VimEnter * :AirlineRefresh

" LiteCorrect
augroup litecorrect
    autocmd!
    autocmd FileType markdown,mkd,md call litecorrect#init()
augroup END

" Lexical
augroup lexical
    autocmd!
    autocmd FileType markdown,mkd,md call lexical#init()
    autocmd FileType textile call lexical#init()
    autocmd FileType text call lexical#init({'spell': 0})
augroup END

" Colors
syntax on
syntax enable
set background=dark
let ayucolor="mirage"
colorscheme ayu

" Tabs
nnoremap <Tab> gt
nnoremap <S-Tab> gT
nnoremap <silent> <S-t> :tabnew<CR>

" Git
noremap <Leader>ga :Gwrite<CR>
noremap <Leader>gc :Gcommit<CR>
noremap <Leader>gsh :Gpush<CR>
noremap <Leader>gll :Gpull<CR>
noremap <Leader>gs :Gstatus<CR>
noremap <Leader>gb :Gblame<CR>
noremap <Leader>gd :Gvdiff<CR>
noremap <Leader>gr :Gremove<CR>

" NERDTree configuration
let g:NERDTreeChDirMode=2
let g:NERDTreeIgnore=['\.rbc$', '\~$', '\.pyc$', '\.db$', '\.sqlite$', '__pycache__']
let g:NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$', '\.bak$', '\~$']
let g:NERDTreeShowBookmarks=1
let g:nerdtree_tabs_focus_on_files=1
let g:NERDTreeMapOpenInTabSilent = '<RightMouse>'
let g:NERDTreeWinSize = 50
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite
nnoremap <silent> <F6> :NERDTreeFind<CR>
noremap <C-n> :NERDTreeToggle<CR>

" Goyo
let g:goyo_width = 65
noremap <leader>w :Goyo
autocmd! User GoyoEnter Limelight0.7
autocmd! User GoyoLeave Limelight!
