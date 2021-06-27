# DIRECTORIES
alias sites='cd /Users/shawnborton/Sites'
alias Sites='cd /Users/shawnborton/Sites'
alias ..="cd .."
alias ...="cd ../.."
alias h="cd ~"
alias back="cd -"
alias finder='open -a 'Finder' .'
alias ll='ls -l'
alias lla='ls -la'
alias docks='docker ps'

# FOR CHROME
alias chrome='open -a "Google Chrome"'
alias chrome-remote='/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --remote-debugging-port=9222'

# PATH SHENANIGANS
# export PATH=$PATH:
export PATH="/usr/local/bin:${PATH}"
#export PATH=$XAMPP_PHP:$PATH:./node_modules/.bin:$HOME/.composer/vendor/bin
#export PATH=$XAMPP_PHP:/usr/local/bin:$PATH:./node_modules/.bin:$HOME/.composer/vendor/bin
source "$HOME/.console/console.rc" 2>/dev/null

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

# MISCELLANEOUS
export NVM_DIR="/Users/shawnborton/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

# include .job_aliases if it exists
if [ -f $HOME/.job_aliases ]; then
	. $HOME/.job_aliases
fi

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

flushdns() {
  sudo dscacheutil -flushcache; sudo killall -HUP mDNSResponder; say DNS cache flushed
}

export PS1="\u@\h \W\[\033[32m\]\$(parse_git_branch)\[\033[00m\] $ "
# added by Anaconda3 2018.12 installer
# >>> conda init >>>
# !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$(CONDA_REPORT_ERRORS=false '/Users/shawnborton/anaconda3/bin/conda' shell.bash hook 2> /dev/null)"
# if [ $? -eq 0 ]; then
#    \eval "$__conda_setup"
# else
#    if [ -f "/Users/shawnborton/anaconda3/etc/profile.d/conda.sh" ]; then
#         . "/Users/shawnborton/anaconda3/etc/profile.d/conda.sh"
#         CONDA_CHANGEPS1=false conda activate base
#     else
#        \export PATH="/Users/shawnborton/anaconda3/bin:$PATH"
#     fi
# fi
# unset __conda_setup
# <<< conda init <<<
