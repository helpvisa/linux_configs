echo "" | fzf \
    --multi \
    --bind "enter:replace-query+print-query,ctrl-n:preview-down,ctrl-p:preview-up,ctrl-d:preview-page-down,ctrl-u:preview-page-up" \
    --style=minimal \
    --layout=reverse --margin 3% --prompt='query definition: ' \
    --preview='dict {q}' --preview-window 'bottom,99%'
