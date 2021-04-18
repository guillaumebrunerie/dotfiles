# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt appendhistory autocd notify interactive_comments complete_in_word HIST_IGNORE_DUPS
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
# zstyle :compinstall filename '/home/fractal/.zshrc'

bindkey '^[[1;5C' emacs-forward-word
bindkey '^[[1;5D' emacs-backward-word
bindkey '^[[3^' backward-kill-word

autoload -U select-word-style
select-word-style bash

autoload -Uz compinit
compinit -u
# End of lines added by compinstall

[ -f ~/.profile ] && source ~/.profile

# Complétion insensible à la casse
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'

compdef '_files -W /dev/disk/by-label' mnt
compdef '_files -W /dev/disk/by-label' umnt

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git hg
zstyle ':vcs_info:git*' formats "%F{blue}(%b)%f%m%u%c "

setopt prompt_subst
[[ -n $SSH_CLIENT ]] && _HOST="$HOST " || _HOST=
# Prompt
PROMPT='%(?.(%F{green}0%f).%B%F{red}(%?%)%f%b) %F{red}${_HOST}%f%B%F{blue}%~%f %F{yellow}%1(j.%U.)%(!.#.$)%u%f%b '
RPROMPT='${vcs_info_msg_0_} %F{magenta}%B[%T]%b%f'

alias ls="ls -G"
alias grep="grep --color=auto"
export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
export LESS=' -R '

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

# emacs() {
#     if [[ $TERM == linux ]]
#     then
#         /usr/bin/emacs -nw "$@"
#     else
#         /usr/bin/emacs "$@" &!
#     fi
# }

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

export PATH=~/bin:$PATH:/usr/local/bin

# Colored and paged SVN
svn() {
    if [[ "$1" == diff ]] || [[ "$1" == di ]]; then
        /usr/bin/svn diff "${@:2}" | ~/bin/colordiff | less -x4
    elif [[ "$1" == log ]]; then
        /usr/bin/svn log --verbose "${@:2}" | ~/bin/colordiff --difftype=diffu | less -x4
    elif [[ "$1" == logd ]]; then
        /usr/bin/svn log --diff --verbose "${@:2}" | ~/bin/colordiff | less -x4
    else
        /usr/bin/svn "$@"
    fi
}
compdef '_files' svn logd

# Tab width
tabs -4

# Load local configuration
. $HOME/.zshrc-local

DIRSTACKSIZE=8
setopt autopushd pushdminus pushdsilent pushdtohome
alias dh='dirs -v'
