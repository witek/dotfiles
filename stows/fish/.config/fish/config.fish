function fish_prompt --description 'Witek #1'
    #Save the return status of the previous command
    set -l last_pipestatus $pipestatus
    set -lx __fish_last_status $status # Export for __fish_print_pipestatus.

    if functions -q fish_is_root_user; and fish_is_root_user
        printf '%s@%s %s%s%s# ' $USER (prompt_hostname) (set -q fish_color_cwd_root
                                                         and set_color $fish_color_cwd_root
                                                         or set_color $fish_color_cwd) \
            (prompt_pwd) (set_color normal)
    else
        set -l status_color (set_color $fish_color_status)
        set -l statusb_color (set_color --bold $fish_color_status)
        set -l pipestatus_string (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)

        printf '\n--- [%s] %s%s@%s %s%s %s%s%s \n> ' (date "+%H:%M:%S") (set_color brblue) \
            $USER (prompt_hostname) (set_color $fish_color_cwd) $PWD $pipestatus_string \
            (set_color normal) '---'
    end
end


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
