# Setup fzf
# ---------
if [[ ! "$PATH" == */home/witek/.fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/home/witek/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/witek/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/home/witek/.fzf/shell/key-bindings.bash"
