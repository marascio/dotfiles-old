" vimrc
"
" $Id: vimrc 2942 2009-11-29 12:26:00Z louis $

colorscheme vividchalk 
set encoding=utf8
set gfn=Monaco:h13
set backspace=indent,eol,start
set nocompatible
set textwidth=72
set wrap
set number
set ruler
set shiftwidth=4
set tabstop=4
set softtabstop=4
set expandtab
set autoindent
set formatoptions=tcroqn

set statusline=%F%m%r%h%w\ (%{&ff}\ %Y\ pos=%l,%v\ %p%%\ of\ %L)
set laststatus=2

"set comments=fb:-,fb:*
"set formatlistpat=^\\s*(\\d\\+[.\\t\ ]\|[-*]\ )\\s*
"set flp=^\\(\\d\\+[.\\t\ ]\\\|[-*]\ \\\|\ \ \\)\\s*    " and  also recognizes two-space blockquoting

map fl ^gq$
map fp {gq}

syntax on

filetype on
filetype indent on
filetype plugin on

autocmd BufRead,BufNewFile *fetchmailrc* set filetype=fetchmail

autocmd BufRead,BufNewFile *.py set autoindent

autocmd BufRead,BufNewFile *.rb set autoindent
autocmd FileType ruby set tabstop=2 shiftwidth=2 sts=2

autocmd BufRead,BufNewFile */mm/*/*.h set ts=8 sw=8 sts=8 noexpandtab listchars=tab:>- list
autocmd BufRead,BufNewFile */mm/*/*.c set ts=8 sw=8 sts=8 noexpandtab listchars=tab:>- list

autocmd BufNewFile,BufRead  svn-commit.* set filetype=svn

" Align text on some centering string
vmap fc !align -c:

" Insert the current RFC 2822 compliant date time string
imap \now <C-R>=strftime("%a, %d %b %Y %H:%M:%S %z")<CR>
map  \now     "=strftime("%a, %d %b %Y %H:%M:%S %z")<CR>p

