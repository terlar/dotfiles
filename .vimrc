" General {
    set nocompatible
    set modelines=0
    scriptencoding utf-8
    filetype plugin indent on
    syntax on
    set mouse=a
    set noerrorbells
    set visualbell
    set backspace&
    set autowrite
    set nobackup
    set noswapfile
    set clipboard+=unnamed
    set tags=tags;
    set hidden
    set iskeyword+=_,$,@,%,#
    set history=1000
    let mapleader=','
" }

" Vim UI {
    set hlsearch
    set incsearch
    set lazyredraw
    set list
    set listchars=tab:▸\ ,trail:•
    set nostartofline
    set number
    set numberwidth=5
    set ruler
    set shortmess=aOstT
    set showcmd
    set showmode
    set showmatch
    set statusline=%<%f\ %=\:\b%n%y%m%r%w\ %l,%c%V\ %P
    set laststatus=2
    set showtabline=2
    set tabpagemax=15
    set wildmenu
    set wildmode=list:longest,full
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

" Plugins {
    call pathogen#runtime_append_all_bundles()
    call pathogen#helptags()
    colorscheme molokai

    " Syntastic {
        set statusline+=\ %#warningsmsg#
        set statusline+=%{SyntasticStatuslineFlag()}
        set statusline+=%*
        let g:syntastic_enable_signs = 1
    " }

    " Taglist {
        map <Leader>t :TlistToggle<CR>
    " }

    " Gundo {
        nnoremap <Leader>u :GundoToggle<CR>
    " }
" }

" Mappings {
    " Quickly escape input mode
    inoremap jj <Esc>

    " Buffer navigation {
        nnoremap <C-h> <C-w>h
        nnoremap <C-j> <C-w>j
        nnoremap <C-k> <C-w>k
        nnoremap <C-l> <C-w>l
        nnoremap <Leader>w <C-w>v<C-w>l
    " }

    " Shortcuts {
        " Tag navigation
        map <M-Right> <C-]>
        map <M-Left> <C-T>

        " Match brackets
        nnoremap <Tab> %
        vnoremap <Tab> %

        " Folding
        nnoremap <Space> za
        vnoremap <Space> za

        " Explorer
        map <Leader>e :Exp<CR>
        map <Leader>s :Sex<CR>

        " Change working directory to file directory
        cmap cwd lcd %:p:h

        " Clear search results
        nnoremap <Leader><Space> :noh<CR>

        " Select pasted text
        nnoremap <Leader>v V`]

        " Toggle paste
        map <Leader>P :set paste!<CR>

        " Toggle numbers
        map <Leader># :set number!<CR>
    " }

    " Tools {
        " Remove trailing whitespace
        nnoremap <Leader>W :%s/\s\+$//<CR>:let @/=''<CR>

        " Sort CSS
        nnoremap <Leader>S ?{<CR>jV/^\s*\}<CR>k:sort<CR>:noh<CR>
    " }

    " Breaking bad habits {
        inoremap  <Up>     <Nop>
        inoremap  <Down>   <Nop>
        inoremap  <Left>   <Nop>
        inoremap  <Right>  <Nop>
        noremap   <Up>     <Nop>
        noremap   <Down>   <Nop>
        noremap   <Left>   <Nop>
        noremap   <Right>  <Nop>
    " }
" }

" Autocommands {
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
if has('gui_running')
    " General {
        set guifont=Menlo:h12
        set guioptions=egmrt
        set t_Co=256
        set transparency=5
        set mousehide
        let macvim_skip_cmd_opt_movement = 1
    " }
endif
" }
