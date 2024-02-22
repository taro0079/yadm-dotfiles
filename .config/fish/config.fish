if status is-interactive
    # Commands to run in interactive sessions can go here
    set -x PATH $PATH $HOME/.volta/bin
    fzf_key_bindings
    zoxide init fish | source
    set -x PATH $PATH $HOME/.config/composer/vendor/bin
    set -gx PATH "$HOME/.cargo/bin" $PATH;

end
function fbr
    set branches (git branch --all | grep -v HEAD)
    set branch (echo "$branches" | fzf-tmux -d (math 2 + (count $branches)) +m)
    git checkout (echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
end

function fshow
    git log --graph --color=always \
        --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" $argv | 
    fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
        --bind "ctrl-m:execute:
                  (grep -o '[a-f0-9]\\{7\\}' | head -1 |
                  xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                  {}
FZF-EOF"
end
function dox
    docker exec -it (docker ps --format "{{.Names}}" | fzf) sh
end
alias e='exa -la --icons --git'
alias ls=e

# alias for git command
alias gst='git status'
alias gsw='git switch'
alias gc='git commit'
alias gpush='git push origin'
alias gpull='git pull origin'
alias gcb='git checkout -b'
alias gco='git checkout'
alias gaa='git add .'
alias ga='git add'

