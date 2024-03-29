# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt appendhistory autocd notify interactive_comments complete_in_word HIST_IGNORE_DUPS
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
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'

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
