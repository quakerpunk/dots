# DIRECTORIES
alias sites='cd /Users/shawnborton/Sites'
alias Sites='cd /Users/shawnborton/Sites'
alias kboxsites='cd /Users/shawnborton/Kalabox'
alias axwaysrcdir='cd docroot/sites/all/themes/axway/assets/src'
alias ..="cd .."
alias ...="cd ../.."
alias h="cd ~"
alias back="cd -"
alias finder='open -a 'Finder' .'
alias ll='ls -l'

# FOR CHROME
alias chrome='open -a "Google Chrome"'

# PHP
alias php56='/Applications/XAMPP/bin/php'
alias drucc='drush cc'
alias drucca='drush cc all'
alias drupal='$(brew --prefix homebrew/php/php71)/bin/php /usr/local/bin/drupal'
alias update-global-composer='cd ~/.composer && composer update'
alias composer-update-global='update-global-composer'

# PATH SHENANIGANS
export XAMPP_PHP=/Applications/XAMPP/bin
export DRUSH_PHP='/Applications/XAMPP/bin/php'
#export PATH=$PATH:$XAMPP_PHP:./node_modules/.bin:$HOME/.composer/vendor/bin
export PATH=$XAMPP_PHP:$(brew --prefix homebrew/php/php71)/bin:$PATH:./node_modules/.bin:$HOME/.composer/vendor/bin
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
alias kga='kbox git add'
alias kgb='kbox git branch'
alias kgs='kbox git status'

alias dotconfig='/usr/bin/git --git-dir=$HOME/.myconf/ --work-tree=$HOME'

# MISCELLANEOUS
export NVM_DIR="/Users/shawnborton/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

flushdns() {
  sudo dscacheutil -flushcache; sudo killall -HUP mDNSResponder; say DNS cache flushed
}

export PS1="\u@\h \W\[\033[32m\]\$(parse_git_branch)\[\033[00m\] $ "
