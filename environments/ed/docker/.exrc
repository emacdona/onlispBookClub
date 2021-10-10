set nocompatible
source $VIMRUNTIME/vimrc_example.vim
source $VIMRUNTIME/mswin.vim
behave mswin

set ignorecase
set nu
set showmatch
set nowrap
set autoindent
set nosmarttab
set tabstop=3
set shiftwidth=3
set expandtab
set sidescroll=5
set listchars+=extends:>
set nobackup
set guioptions+=b
set nohlsearch
set cursorline
"set cursorcolumn
"colorscheme desert
filetype plugin indent on

let Tlist_Ctags_Cmd='/Users/emacdona/usr/local/bin/ctags'

let g:persistentBehavior=0
let g:winManagerWidth=45
let g:defaultExplorer=0

"default:
"let g:winManagerWindowLayout='FileExplorer,TagsExplorer|BufExplorer'
let g:winManagerWindowLayout='FileExplorer,TagList|BufExplorer'

noremap <C-k> <C-W>k
noremap <C-j> <C-W>j
noremap <C-h> <C-W>h
noremap <C-l> <C-W>l

"NERDTree stuff
"autocmd VimEnter * NERDTree
"autocmd VimEnter * wincmd p
let NERDTreeIgnore = ['\~$', '\.vim$', '^target$', '\.iml$', '\.ipr$', '\.iws$']

function! ToggleParedit()
   if !g:paredit_mode 
      let g:paredit_mode = 1
      echo "paredit ON"
   else
      let g:paredit_mode = 0
      echo "paredit OFF"
   endif
endfunction

nmap <silent> q :call ToggleParedit()<CR>

"see README @ https://github.com/tpope/vim-pathogen
call pathogen#infect()
"let g:solarized_termcolors=16
"colorscheme solarized
"set background=dark
"set background=light
syntax enable

map \ :NERDTreeFind
"map q :qa

"map \ 0i//j
map v :s/^\/\///j
"map q 0i#j
map V :s/^#//j
map K :map
map = ]]z.
map _ [[z.
