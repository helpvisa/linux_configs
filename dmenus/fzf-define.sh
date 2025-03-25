echo "" | fzf --bind=enter:replace-query+print-query \
    --style=minimal \
    --layout=reverse --margin 3% --prompt='query definition: ' \
    --preview='dict {q}' --preview-window 'bottom,99%'
