if status is-interactive
    # Commands to run in interactive sessions can go here
    set -x PATH $PATH $HOME/.volta/bin
    fzf_key_bindings
    zoxide init fish | source
    set -x PATH $PATH $HOME/.config/composer/vendor/bin

end
function fbr
    set branches (git branch --all | grep -v HEAD)
    set branch (echo "$branches" | fzf-tmux -d (math 2 + (count $branches)) +m)
    git checkout (echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
end
