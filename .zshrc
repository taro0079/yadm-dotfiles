# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi


alias e='exa -la --icons --git'
alias l=e
alias ls=e
alias ea='exa -a --icons --git'
alias la=ea
alias ee='exa -aahl --icons --git'
alias ll=ee
alias et='exa -T -L 3 -a -I "node_modules|.git|.cache" --icons'
alias lt=et
alias eta='exa -T -a -I "node_modules|.git|.cache" --color=always --icons | less -r'
alias lta=eta
alias l='clear && ls'
# vim keybind
bindkey -v

source $HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
# source $HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# GOPATH setting
export GOPATH=~/go  # GOPATHにすると決めた場所
export PATH=$GOPATH/bin:$PATH

# 補完の有効化
autoload -U compinit
compinit
export PATH="$HOME/.rbenv/bin:$PATH"
# mac
export PATH="$HOME/.cargo/bin:$PATH"
eval "$(rbenv init -)"

export PATH="$HOME/.local/bin:$PATH"

#export FZF_DEFAULT_COMMAND="find -L"
export FZF_DEFAULT_COMMAND='rg --files --hidden --glob "!.git"'
export FZF_CTRL_T_COMMAND='rg --files --hidden --glob "!.git"'
export FZF_CTRL_T_OPTS='--preview "bat --color=always --style=header,grid --line-range :100 {}"'



export DENO_INSTALL="/home/taro/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"

export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:~/.nimble/bin

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='rg --files --hidden --glob "!.git"'
export FZF_DEFAULT_OPTS='--height 40% --reverse --border'

COLOR_BLACK="\e[0;30m"
COLOR_BLUE="\e[0;34m"
COLOR_GREEN="\e[0;32m"
COLOR_CYAN="\e[0;36m"
COLOR_PINK="\e[0;35m"
COLOR_RED="\e[0;31m"
COLOR_PURPLE="\e[0;35m"
COLOR_BROWN="\e[0;33m"
COLOR_LIGHTGRAY="\e[0;37m"
COLOR_DARKGRAY="\e[1;30m"
COLOR_LIGHTBLUE="\e[1;34m"
COLOR_LIGHTGREEN="\e[1;32m"
COLOR_LIGHTCYAN="\e[1;36m"
COLOR_LIGHTRED="\e[1;31m"
COLOR_LIGHTPURPLE="\e[1;35m"
COLOR_YELLOW="\e[1;33m"
COLOR_WHITE="\e[1;37m"
COLOR_NONE="\e[0m"

# if [ -z "$TMUX" ]; then
#     export TERM=xterm-256color-italic
# else
#     export TERM=tmux-256color
# fi

fbr() {
  local branches branch
  branches=$(git branch --all | grep -v HEAD) &&
  branch=$(echo "$branches" |
           fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
  git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}



# fshow - git commit browser
fshow() {
  git log --graph --color=always \
      --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
}


fh() {
	print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/\\/\\\\/g')

}

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/moritatarou/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/moritatarou/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/moritatarou/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/moritatarou/google-cloud-sdk/completion.zsh.inc'; fi

# M1 macでsmooth_line_apiのbundle installでmysql2がこけるのを回避
if [ "$(uname)" = "Darwin" ]; then
	export LIBRARY_PATH=$LIBRARY_PATH:$(brew --prefix zstd)/lib
fi

alias dox='docker exec -it `docker ps --format "{{.Names}}" | fzf` sh'
fpath=(~/.zsh/completion $fpath)
source ~/.ghq/github.com/kwhrtsk/docker-fzf-completion/docker-fzf.zsh
eval "$(zoxide init zsh)"

# Generated for envman. Do not edit.
[ -s "$HOME/.config/envman/load.sh" ] && source "$HOME/.config/envman/load.sh"
export PATH="$HOME/zls:$PATH"
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
# export PATH="/snap/bin:$PATH"
# eval "$(pyenv init --path)" # これを追記
# eval "$(pyenv init -)"

[ -f "/home/taro/.ghcup/env" ] && source "/home/taro/.ghcup/env" # ghcup-env
export PATH="$HOME/.symfony5/bin:$PATH"

# auto suggestion settings
# bindkey '^j' autosuggest-accept
# starship
# eval "$(starship init zsh)"
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# pnpm
export PNPM_HOME="/home/taro/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end
export PATH="/usr/local/bin:$PATH" # パスが通ってなかったら実行

alias gst='git status'
alias gsw='git switch'
alias gc='git commit'
alias gpush='git push origin'
alias gpull='git pull origin'
alias gcb='git checkout -b'
alias gco='git checkout'
alias gaa='git add .'
alias ga='git add'

if [[ "$OSTYPE" == "darwin" ]]; then
  alias ctags="brew --prefix /usr/bin/ctags"
fi

# BEGIN SNIPPET: Platform.sh CLI configuration
HOME=${HOME:-'/Users/awesometaro'}
export PATH="$HOME/"'.platformsh/bin':"$PATH"
if [ -f "$HOME/"'.platformsh/shell-config.rc' ]; then . "$HOME/"'.platformsh/shell-config.rc'; fi # END SNIPPET
export PATH="/usr/local/opt/openjdk@17/bin:$PATH"
export PATH="/opt/homebrew/opt/openjdk@17/bin:$PATH"
source ~/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"
