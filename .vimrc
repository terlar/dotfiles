" General {
	set nocompatible
	set nomodeline
	set nolazyredraw

	set history=1000

	set encoding=utf-8
	filetype plugin on
	filetype indent on
	syntax on

	let mapleader=','
" }

" User Interface {
	set shortmess=aOstT
	set wildmenu
	set wildmode=list:longest,full

	set listchars=tab:▸\ ,trail:•

	" Search
	set ignorecase
	set smartcase
	set hlsearch
	set incsearch

	" Matching bracets
	set showmatch
	set mat=2

	" Statusline
	set statusline=%<%f\ %=\:\b%n%y%m%r%w\ %l,%c%V\ %P
	set laststatus=2
	set showcmd
	set showmode
	set ruler

	set number
	set numberwidth=5

	set mouse=a

	" No sounds
	set noeb
	set vb
	set t_vb=
" }

" Filesystem {
	set autoread
	set hidden

	" No backup
	set nobackup
	set nowb
	set noswapfile

	set tags=tags;
" }

" Edit {
	set nostartofline
	set backspace&
	set iskeyword+=_,$,@,%,#

	" Tabs
	set shiftround
	set shiftwidth=4
	set softtabstop=4
	set tabstop=4
	set smarttab

	" Indent
	set ai
	set si

	" Wrap
	set wrap
	set lbr
" }

" Folding {
	set foldenable
	set foldmarker={,}
	set foldmethod=marker
	set foldlevel=100
	set foldopen=block,hor,mark,percent,quickfix,tag
	function! SimpleFoldText() " {
		return substitute(getline(v:foldstart), '{.*', '{...}', '')
	endfunction " }
	set foldtext=SimpleFoldText()
" }

" Plugins {
	call pathogen#runtime_append_all_bundles()
	call pathogen#helptags()
	colorscheme molokai

	" Netrw {
		let g:netrw_special_syntax = 1
		let g:netrw_list_hide='^\.[^\.]'
	" }

	" Syntastic {
		set statusline+=\ %#warningsmsg#
		set statusline+=%{SyntasticStatuslineFlag()}
		set statusline+=%*
		let g:syntastic_enable_signs = 1
	" }

	" Taglist {
		map <Leader>t :TlistToggle<CR>
		let Tlist_GainFocus_On_ToggleOpen = 1
		let Tlist_Show_One_File = 1
	" }

	" Gundo {
		nnoremap <Leader>u :GundoToggle<CR>
	" }
" }

" Mappings {
	" Quickly escape input mode
	inoremap jj <Esc>

	" Make Y yank to the end of the line
	nmap Y y$

	" Move lines {
		nmap <M-j> mz:m+<CR>
		nmap <M-k> mz:m-2<CR>
		vmap <M-j> :m'>+<CR>
		vmap <M-k> :m'<-2<CR>

		nmap <D-j> <M-j>
		nmap <D-k> <M-k>
		vmap <D-j> <M-j>
		vmap <D-k> <M-k>
	" }

	" Buffer navigation {
		nnoremap <C-h> <C-w>h
		nnoremap <C-j> <C-w>j
		nnoremap <C-k> <C-w>k
		nnoremap <C-l> <C-w>l
		nnoremap <Leader>w <C-w>v<C-w>l
	" }

	" Shortcuts {
		" Tag navigation
		map <Tab> <C-]>
		map <S-Tab> <C-T>

		" Folding
		nmap <Space> za
		vmap <Space> za

		" Explorer
		map <Leader>e :Exp<CR>
		map <Leader>s :Sex<CR>

		" Change to directory of current buffer
		map <Leader>cd :cd %:p:h<CR>

		" Clear search results
		nnoremap <Leader><Space> :noh<CR>

		" Select pasted text
		nnoremap <Leader>v V`]

		" Toggle paste
		map <Leader>P :setlocal paste!<CR>

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

	" Go to last position {
		autocmd BufReadPost *
		\ if line("'\"") > 0 && line("'\"") <= line("$") |
		\    exe "normal g`\"" |
		\ endif
	" }

	" Autoreload vimrc
	autocmd BufWritePost .vimrc source $MYVIMRC
" }

" GUI Settings {
if has('gui_running')
	" General {
		set guifont=Menlo:h12
		set guioptions=egmrt
		set guitablabel=%t
		set t_Co=256
		set mousehide
		let macvim_skip_cmd_opt_movement = 1
	" }
endif
" }
