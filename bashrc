[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

#PS1='${debian_chroot:+($debian_chroot}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
# Show current git branch
PS1="\[\e[0;32m\]\u@\h\[\e[m\]\[\e[1;34m\] \W\[\e[m\]\[\e[1;32m\]\[\e[m\]\[\e[1;37m\]\[\033[31m\]\`ruby -e \"print (%x{git branch 2> /dev/null}.split('\n').grep(/^\*/).first || '').gsub(/^\* (.+)$/, '(\1) ')\"\`\[\033[37m\]$\[\033[00m\]"

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
alias grkw="git rebase --whitespace=fix"
alias gcaa="gca --amend"
alias grc="git rebase --continue"
alias grs="git rebase --skip"
alias gcp="git cherry-pick"
alias gri="git rebase -i"
alias gix="gitx --all"

alias resrc="source ~/.bash_profile"

function wait-kmsg ()
{
        adb wait-for-device root; adb wait-for-device shell cat /proc/kmsg;
}

function usepckeyboard
{
# use Tab as Control, just "Menu" key as Caps.    
cat | xmodmap - <<EOF
keycode 66 = Control_L
clear Lock
add Control = Control_L
keycode 135 = Caps_Lock
add Lock = Caps_Lock
keycode  51 = BackSpace NoSymbol BackSpace
keycode  22 = backslash bar backslash bar
EOF
}

function usehhkb 
{
# revert all lock.  
cat | xmodmap - << EOF
keycode  22 = BackSpace NoSymbol BackSpace
keycode  51 = backslash bar backslash bar
keycode 135 = Control_L
clear Lock
add Control = Control_L
keycode 66 = Caps_Lock
add Lock = Caps_Lock
EOF
}
