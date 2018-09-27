# Print current git branch on the terminal
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

export PS1="\u@\h \W\[\033[32m\]\$(parse_git_branch)\[\033[00m\] $ "
if [ -f ~/.git-completion.bash ]; then
    . ~/.git-completion.bash
fi


# set your aliases
alias e="emacs"
alias e2="emacs -nw"
alias end="emacs --no-desktop"
alias ga="git add"
alias gc="git commit"
alias gs="git status"
alias gcm="git commit -m"
alias gcmb="git commit -m \"blah\""
alias gri="git rebase -i"
alias gir="grep -ir"
alias fn="find . -name"

# set git editor to emacs
export EDITOR="emacs -nw"

# Marble specific config
export MAPPING_ROOT=$HOME/data/marble-map-root/
source /opt/ros/kinetic/setup.bash
source ~/src/mBot/ros/devel/setup.bash
export MDASH_API_KEY=""
