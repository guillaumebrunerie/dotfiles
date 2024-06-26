# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt appendhistory sharehistory notify interactive_comments complete_in_word hist_find_no_dups hist_ignore_all_dups hist_ignore_dups
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall

bindkey '^[[1;5C' emacs-forward-word
bindkey '^[[1;5D' emacs-backward-word
bindkey '^[[3^' backward-kill-word
bindkey '^H' backward-kill-word

autoload -U select-word-style
select-word-style bash

autoload -Uz compinit
compinit -u
# End of lines added by compinstall

[ -f ~/.profile ] && source ~/.profile

# Complétion insensible à la casse
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.webp=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:';
export LS_COLORS
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

compdef '_files -W /dev/disk/by-label' mnt
compdef '_files -W /dev/disk/by-label' umnt

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git hg
zstyle ':vcs_info:git*' formats "%F{blue}(%b)%f%m%u%c "

setopt prompt_subst
[[ -n $SSH_CLIENT ]] && _HOST="${HOST%%.*} " || _HOST=
# Prompt
PROMPT='%(?.(%F{green}0%f).%B%F{red}(%?%)%f%b) %B%F{green}${_HOST}%f%B%F{blue}%~%f %F{yellow}%1(j.%U.)%(!.#.$)%u%f%b '
RPROMPT='${vcs_info_msg_0_} %F{magenta}%B[%T]%b%f'

if [[ $(uname -s) == "Linux" ]]
then
    alias ls="ls --color=auto"
else
    alias ls="ls -G"
fi
alias grep="grep --color=auto"
export LESS=' -R -j.5 '

catfunc () {
    if [[ $# == 1 ]]
    then
        source-highlight --failsafe --infer-lang -f esc --style-file=esc.style -i "$1" 2>/dev/null || /bin/cat "$1"
    else
        cat $*
    fi
}
alias cat=catfunc

rationalise-dot() {
    if [[ $LBUFFER = *. ]]; then
        LBUFFER+=./
    else
        LBUFFER+=.
    fi
}
slash-after-dots() {
    if [[ $LBUFFER = *../ ]]; then
        
    else
        LBUFFER+=/
    fi
}
zle -N rationalise-dot
zle -N slash-after-dots
bindkey . rationalise-dot
bindkey / slash-after-dots
bindkey -M isearch . self-insert
bindkey -M isearch / self-insert

if [[ $(uname -s) == "Linux" ]]
then
	emacs() {
		if [[ $TERM == linux ]]
		then
			=emacs -nw "$@"
		else
			=emacs "$@" &!
		fi
	}
fi

unsetopt automenu

reset-prompt-and-accept-line () {
    zle reset-prompt
    zle accept-line
}
zle -N reset-prompt-and-accept-line
bindkey "^M" reset-prompt-and-accept-line

__last_cmd=
preexec () {
    __last_cmd=$1
    if [[ $TERM != linux ]]
    then
        printf "\e]0;*%s\a" $__last_cmd
    fi
}
precmd () {
    local exit_status=$?
    vcs_info
    if [[ -n $__last_cmd && $TERM != linux ]]
    then
	if (( $exit_status == 0 ))
	then
	    printf "\e]0;(%s)\a" $__last_cmd
        else
	    printf "\e]0;[%s]\a" $__last_cmd
        fi
    fi
    echo -en "\a"
}
REPORTTIME=10

# Colored and paged SVN
svn() {
    if [[ "$1" == diff ]] || [[ "$1" == di ]]; then
        =svn diff "${@:2}" | ~/bin/colordiff | less -x4
    elif [[ "$1" == log ]]; then
        =svn log --verbose "${@:2}" | ~/bin/colordiff --difftype=diffu | less -x4
    elif [[ "$1" == logd ]]; then
        =svn log --diff --verbose "${@:2}" | ~/bin/colordiff | less -x4
	elif [[ "$1" == uncheckout ]]; then
		=svn up --set-depth exclude "${@:2}"
	elif [[ "$1" == unadd ]]; then
		=svn rm --keep-local "${@:2}"
    else
        =svn "$@"
    fi
}
compdef '_files' svn logd
compdef '_files' svn unadd
compdef '_files' svn uncheckout

# Tab width
tabs -4

# Load local configuration
[[ -f $HOME/.zshrc-local ]] && source $HOME/.zshrc-local

DIRSTACKSIZE=8
setopt autopushd pushdminus pushdsilent pushdtohome
alias dh='dirs -v'

alias dropbox-ignore='attr -s com.dropbox.ignored -V 1'

function dropbox-ls-ignored {
	for i in *(N) .*(N)
	do
		if [[ $(attr -ql "$i") =~ 'com.dropbox.ignored' ]] && [[ $(attr -qg com.dropbox.ignored "$i") == 1 ]]
		then
			echo "$i"
		fi
	done
}

### Emacs' vterm config

vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
	setopt PROMPT_SUBST
	PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
fi

alias resource="source ~/.zshrc"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# bun completions
[ -s "/Users/guillaumeb/.bun/_bun" ] && source "/Users/guillaumeb/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
