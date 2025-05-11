
if status is-interactive
    # Commands to run in interactive sessions can go here

    #  ___  _ _____ _  _
    # | _ \/_\_   _| || |
    # |  _/ _ \| | | __ |
    # |_|/_/ \_\_| |_||_|

    set -gx PATH ~/.dotfiles/scripts ~/.babashka/bbin/bin ~/.nix-profile/bin $PATH


    #       _ _
    #  __ _| (_)__ _ ___ ___ ___
    # / _` | | / _` (_-</ -_|_-<
    # \__,_|_|_\__,_/__/\___/__/
    . ~/.dotfiles/shell-aliases


    #                __     _      _
    #  _ _  ___ ___ / _|___| |_ __| |_
    # | ' \/ -_) _ \  _/ -_)  _/ _| ' \
    # |_||_\___\___/_| \___|\__\__|_||_|

    neofetch
end
