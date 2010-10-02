" General {
    set nocompatible
    syntax on
    filetype plugin indent on
    let mapleader=","
    set backspace=indent,eol,start
    set nobackup
    set clipboard+=unnamed
    set directory=~/.vim/swap
    set tags=tags;,lib/tags;
    set hidden
    set iskeyword+=_,$,@,%,#
    set mouse=a
    set noerrorbells
    set visualbell
    set wildmenu
    set wildmode=list:longest,full
" }

" Vim UI {
    set hlsearch
    set incsearch
    set lazyredraw
    set list
    set listchars=tab:>-,trail:-
    set nostartofline
    set number
    set numberwidth=5
    set ruler
    set scrolloff=10
    set shortmess=aOstT
    set showcmd
    set showmatch
    set sidescrolloff=10
    set statusline=%F%m%r%h%w[%L][%{&ff}]%y[%p%%][%04l,%04v]
    set showtabline=2
    set tabpagemax=15
" }

" Text Formatting/Layout {
    set expandtab
    set formatoptions=rq
    set ignorecase
    set smartcase
    set nowrap
    set shiftround
    set shiftwidth=4
    set softtabstop=4
    set tabstop=8
    set smarttab
" }

" Folding {
    set foldenable
    set foldmarker={,}
    set foldmethod=marker
    set foldlevel=100
    set foldopen=block,hor,mark,percent,quickfix,tag
    function! SimpleFoldText() " {
        return getline(v:foldstart).' '
    endfunction " }
    set foldtext=SimpleFoldText()
" }

" Plugin Settings {
    call pathogen#runtime_append_all_bundles()
    call pathogen#helptags()
    colorscheme molokai

    " SuperTab Settings {
        let g:SuperTabDefaultCompletionType = 'context'
        let g:SuperTabContextDefaultCompletionType = "<c-x><c-o>"
    " }

    " Syntastic Settings {
        let g:syntastic_enable_signs = 1
        let g:syntastic_auto_loc_list = 1
    " }
" }

" Mappings {
    " Force tabs
    cab e tabe
    tab sball
    nnoremap gf <C-W>gf

    " Navigation for tags
    map <M-Right> <C-]>
    map <M-Left> <C-T>

    " Folding
    nnoremap <space> za
" }

" Autocommands {
    autocmd BufNewFile,BufRead *.txt setfiletype text

    " Completion {
        autocmd FileType python set omnifunc=pythoncomplete#Complete
        autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
        autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
        autocmd FileType css set omnifunc=csscomplete#CompleteCSS
        autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags
        autocmd FileType php set omnifunc=phpcomplete#CompletePHP
        autocmd FileType c set omnifunc=ccomplete#Complete
    " }

    " Autoreload vimrc {
    augroup vimrcEx
        au!
        autocmd BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \    exe "normal g`\"" |
        \ endif
        autocmd BufWritePost .vimrc source $MYVIMRC
    augroup END
    " }
" }

" GUI Settings {
if has("gui_running")
    " General {
        set guifont=Menlo:h12
        set guioptions=egmrt
        set t_Co=256
        set transparency=10
        set mousehide
        let macvim_skip_cmd_opt_movement = 1
    " }
endif
" }
