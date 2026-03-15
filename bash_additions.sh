# .bashrc additional commands
# special functions
# many taken from: https://boreal.social/post/15-practical-bash-functions-i-use-in-my-bashrc
# find files quickly
fnf() {
    find . -type f -iname "*$1*" 2>/dev/null
}
# find directories quickly
fnd() {
    find . -type d -iname "*$1*" 2>/dev/null
}
# display test palette (256 colours)
colours() {
    for i in {0..255}; do
        printf '\e[48;5;%dm%3d ' "$i" "$i"
        (((i+3) % 18)) || printf '\e[0,\n'
    done
    printf '\e[0m\n'
}
# search inside man page
mans() {
    man "$1" | grep -iC 5 "$2"
}
# trash files (safe delete)
trash() {
    mkdir -p ~/.local/share/Trash/files
    for item in "$@"; do
        item=${item#./}
        item=${item%/}
        printf "[Trash Info]\nPath=$(realpath "$item")\nDeletionDate=$(date "+%Y-%m-%dT%H:%M:%S")\n" > ~/.local/share/Trash/info/"$item".trashinfo
        mv "$item" ~/.local/share/Trash/files/
        echo "Trashed $item"
    done
}
# what's on what port?
ports() {
    lsof -iTCP -sTCP:LISTEN -P -n
}
# find largest files
topsize() {
    du -hs * | sort -rh | head -10
}
# git panic button (undo)
git-undo() {
    git reset --soft HEAD~1
}
# pretty-print PATH
path() {
    echo "$PATH" | tr ":" "\n"
}
# serve current directory via HTTP w/ python stdlib
serve() {
    local port=${1:-8000}
    echo "Serving current directory on http://localhost:$port"
    python3 -m http.server "$port"
}
# extract IP addresses from file
extract-ip() {
    grep -oE "\b([0-9]{1,3}\.){3}[0-9]{1,3}\b" "$1" | sort -u
}

# less heinous LS colours bc of NTFS drives mounted with all persmissions
export LS_COLORS=$LS_COLORS:'tw=00;33:ow=01;33:'

# history
export HISTCONTROL=ignoreboth:erasedups

# git funcs
parse_git_dirty() {
  [[ $(git status --porcelain 2> /dev/null) ]] && echo "*"
}
parse_git_branch() {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/\1$(parse_git_dirty) /"
}

# distrobox funcs
distrobox_name() {
	if [ -n "$CONTAINER_ID" ]; then
                printf "%s" "\[\e[1;37m\]$CONTAINER_ID"
	else
		echo '\h'
	fi
}
distrobox_name_prompt() {
        if [ -n "$CONTAINER_ID" ]; then
                printf "%s" "@$CONTAINER_ID "
        fi
}
distrobox_name_minimal() {
    if [ -n "$CONTAINER_ID" ]; then
        printf "%s " "$CONTAINER_ID"
    fi
}

# input and autcompletion changes
bind '"\e[6~": menu-complete'
bind '"\e[5~": menu-complete-backward'
bind 'set completion-ignore-case on'

# prompt
export PS1="\[\e[1;3m\]\$(distrobox_name_minimal)\[\e[0;31m\]\w\n\[\e[0;2;3;36m\]\$(parse_git_branch)\[\e[0;0m\]\\\$ "
# title
if [ "$TERM" = "eterm-color" ]; then
    export PROMPT_COMMAND=''
else
    export PROMPT_COMMAND='printf "\033]0;%s%s in %s [%s]\007" "${USER}" "$(distrobox_name_prompt)" "${PWD/#$HOME/\~}" "${BASHPID}"'
fi

# aliases
alias lsa='ls -Fa --color=auto'
alias la='ls -Fa --color=auto'
alias l='ls -hF --color=auto'
alias ll='ls -lhF --color=auto'
alias lla='ls -lahF --color=auto'
alias e='TERM=xterm-direct emacsclient -nw'

# disable terminal pausing
stty -ixon

# function for creating mini temp directories for futzing around with 't'
# format is "t {tempfoldername}"; futz away!
t() {
    pushd $(mktemp -d /tmp/$1.XXXX)
}
