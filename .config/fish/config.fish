if status is-interactive
    # Commands to run in interactive sessions can go here
    set -x PATH $PATH $HOME/.volta/bin
    fzf_key_bindings
    zoxide init fish | source

end
