" ---------------------------------------------------------------------------- 
" vimrc - configure VIM editor
"
" vim:ts=4:sw=4:sts=4:et:ft=vim
" ---------------------------------------------------------------------------- 

" Check for 256 colors and set colorscheme appropriatelt
if (&term =~ "-256color")
    set t_Co=256
    colorscheme xoria256-lrm
else
    colorscheme vividchalk
endif

set encoding=utf8
set backspace=indent,eol,start
set nocompatible
set textwidth=72
set wrap
set number
set ruler
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set autoindent
set formatoptions=tcroqn

set statusline=%F%m%r%h%w\ (%{&ff}\ %Y\ pos=%l,%v\ %p%%\ of\ %L)
set laststatus=2

"set comments=fb:-,fb:*
"set formatlistpat=^\\s*(\\d\\+[.\\t\ ]\|[-*]\ )\\s*
"set flp=^\\(\\d\\+[.\\t\ ]\\\|[-*]\ \\\|\ \ \\)\\s*    " and  also recognizes two-space blockquoting

syntax on

filetype on
filetype indent on
filetype plugin on

autocmd BufRead,BufNewFile *fetchmailrc* set filetype=fetchmail
autocmd BufRead,BufNewFile *.py          set autoindent
autocmd BufRead,BufNewFile *.rb          set autoindent
autocmd BufRead,BufNewFile svn-commit.*  set filetype=svn
autocmd SessionLoadPost    *             call lrm:session_autoload()

" Formating and alignment 
map fl ^gq$
map fp {gq}

" Align text on some centering string
vmap fc !align -c:

" Insert the current RFC 2822 compliant date time string
imap \now <C-R>=strftime("%a, %d %b %Y %H:%M:%S %z")<CR>
map  \now     "=strftime("%a, %d %b %Y %H:%M:%S %z")<CR>p

" Session management
set sessionoptions=blank,buffers,curdir,folds,help,localoptions,tabpages,winsize

noremap <silent> \slist   <ESC>:SessionList<CR>
noremap <silent> \ssave   <ESC>:call lrm:session_save(0)<CR>
noremap <silent> \ssaveas <ESC>:call lrm:session_save(1)<CR>

let sessionman_save_on_exit=0

function lrm:session_save(saveas)
    call lrm:wikidiary_calendar_close()

    if a:saveas == 1
        :SessionSaveAs
    else
        :SessionSave
    endif

    call lrm:wikidiary_calendar_open()
endfunction

" Use to auto-execute commands for specific sessions that are loaded. 
function lrm:session_autoload()
    call lrm:wikidiary_calendar_open()
endfunction

function lrm:wikidiary_calendar_open()
    if v:this_session !~ 'wikidiary$'
        return
    endif

    :CalendarH  " open a horizontal calendar
    :wincmd t   " go to top left window
    :0          " go to first line of buffer
endfunction

function lrm:wikidiary_calendar_close()
    if v:this_session !~ 'wikidiary$'
        return
    endif

    :wincmd b   " go to bottom right window
    :close      " close the calendar that should be there
endfunction

" Calendar Plugin
let g:calendar_weeknm = 1


" Vim Wiki
let g:vimwiki_list = [{'path': '~/wiki', 'path_html': '~/public_html/'}]

" Highlight lines > 80 characters
highlight OverLength ctermbg=52  ctermfg=166
match     OverLength '\%>80v.\+'

