if test "$TERM" = dumb
    function fish_prompt
        echo "\$ "
    end

    function fish_right_prompt
    end
    function fish_greeting
    end
    function fish_title
    end
end
for mode in default insert visual
    fish_default_key_bindings -M $mode
end
fish_vi_key_bindings --no-erase

if status is-interactive

    # Commands to run in interactive sessions can go here
    set -x PATH $PATH $HOME/.volta/bin
    set -x PATH $PATH $HOME/.config/composer/vendor/bin
    set -x PATH $PATH $HOME/.symfony5/bin
    set -gx PATH "$HOME/.cargo/bin" $PATH
    set -x XDG_CONFIG_HOME $HOME/.config
    eval (/opt/homebrew/bin/brew shellenv)
    zoxide init fish | source
    # set PATH /opt/homebrew/bin/brew $PATH

end
function fbr
    set -l branchname (
        env FZF_DEFAULT_COMMAND='git --no-pager branch -a | grep -v HEAD | sed -e "s/^.* //g"' \
            # fzf-tmux -d --height 70% --prompt "BRANCH NAME>" \
            fzf-tmux -d --prompt "BRANCH NAME>" \
                --preview "git --no-pager log -20 --color=always {}"
    )
    if test -n "$branchname"
        git checkout (echo "$branchname"| sed "s#remotes/[^/]*/##")
    end
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

function ide
    tmux split-window -v -p 30
    tmux split-window -h -p 66
    tmux split-window -h -p 55
end

function dox
    docker exec -it (docker ps --format "{{.Names}}" | fzf) sh
end
alias e='exa -la --icons --git'
alias ls=e

# alias for git command
alias gst='git status'
alias gsw='git switch'
alias gdiff='git diff'
alias gc='git commit'
alias gcm='git commit -m'
alias gpush='git push origin'
alias gpull='git pull origin'
alias gcb='git checkout -b'
alias gco='git checkout'
alias gaa='git add .'
alias ga='git add'
alias gau='git add -u'
# fzf_key_bindings
set -gx VOLTA_HOME "$HOME/.volta"
set -gx PATH "$VOLTA_HOME/bin" $PATH
set -gx PATH "$HOME/.symfony5/bin" $PATH
