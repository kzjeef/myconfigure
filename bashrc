[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

PS1='${debian_chroot:+($debian_chroot}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
# Show current git branch
#PS1="\[\e[0;32m\]\u@\h\[\e[m\]\[\e[1;34m\] \W\[\e[m\]\[\e[1;32m\]\[\e[m\]\[\e[1;37m\]\[\033[31m\]\`ruby -e \"print (%x{git branch 2> /dev/null}.split('\n').grep(/^\*/).first || '').gsub(/^\* (.+)$/, '(\1) ')\"\`\[\033[37m\]$\[\033[00m\] "

alias ll="ls -l"
alias la="ls -a"
alias ls="ls -G"
alias u="cd .."
alias e="emacsclient"

# Git commands
alias gf="git fetch"
alias gstat="git status"
alias gdiff="git diff"
alias gadd="git add"
alias grm="git rm"
alias gca="git commit -a"
alias gco="git checkout"
alias gpo="git push origin"
alias gpom="git push origin master"
alias grom="git rebase origin/master"
alias grkw="git rebase --whitespace=fix"
alias gcaa="gca --amend"
alias grc="git rebase --continue"
alias grs="git rebase --skip"
alias gcp="git cherry-pick"
alias gri="git rebase -i"
alias gix="gitx --all"

alias ad="adb disconnect"
alias ac="adb connect"
alias adbs="adb shell"
alias alogk="adb logcat -vtime -s DirectAudio:v KaraokeService:v"
alias ap="adb push"
alias a='adb'

export PATH=$PATH:/home/jiejing/.local/bin/
#export PATH=$PATH:/Applications/Sublime\ Text.app/Contents/SharedSupport/bin

export CLICOLOR=1
export TERM=xterm-256color

export ORGANIZATION="Alibaba.com"
