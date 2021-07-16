# DIRECTORIES
alias ..="cd .."
alias ...="cd ../.."
alias h="cd ~"
alias back="cd -"
alias ll='ls -l'
alias lla='ls -la'

alias goto_roam="cd ~/org/roam"
alias goto_wolf="cd ~/storage/shared/Documents/cult_wolf"

# PATH SHENANIGANS
# export PATH=$PATH:
export PATH="/usr/local/bin:${PATH}"
#export PATH=$XAMPP_PHP:$PATH:./node_modules/.bin:$HOME/.composer/vendor/bin
#export PATH=$XAMPP_PHP:/usr/local/bin:$PATH:./node_modules/.bin:$HOME/.composer/vendor/bin

###############
#
# Git Aliases
#
###############
alias ga='git add'
alias gb='git branch'
alias gs='git status'
alias gpshom='git push origin master'
alias gpllom='git pull origin master'

alias dotconfig='/usr/bin/git --git-dir=$HOME/.myconf/ --work-tree=$HOME'

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

export PS1="\u@\h \W\[\033[32m\]\$(parse_git_branch)\[\033[00m\] $ "
