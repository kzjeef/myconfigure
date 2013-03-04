[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

#PS1='${debian_chroot:+($debian_chroot}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
# Show current git branch
PS1="\h\\[\033[38m\]\u@\h\[\033[01;34m\] \w \[\033[31m\]\`ruby -e \"print (%x{git branch 2> /dev/null}.grep(/^\*/).first || '').gsub(/^\* (.+)$/, '(\1) ')\"\`\[\033[37m\]$\[\033[00m\] "
PS1="\w@\h \[\033[31m\]\`ruby -e \"print (%x{git branch 2> /dev/null}.grep(/^\*/).first || '').gsub(/^\* (.+)$/, '(\1) ')\"\`\[\033[37m\]$\[\033[00m\] "

alias ll="ls -l"
alias la="ls -a"
alias ls="ls -G"
alias u="cd .."

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
alias gcaa="gca --amend"
alias grc="git rebase --continue"
alias grs="git rebase --skip"
alias gcp="git cherry-pick"
alias gri="git rebase -i"
alias gix="gitx --all"

alias resrc="source ~/.bash_profile"

