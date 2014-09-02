HISTCONTROL=erasedups:ignoredups:ignorespace
HISTTIMEFORMAT=
#"%Y%m%d %H:%M:%S "
HISTIGNORE="&:ls:[bf]g:exit:jobs:lt:j:top"

# append to the history file, don't overwrite it
shopt -s histappend
PROMPT_COMMAND="history -a"

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000000
HISTFILESIZE=20000000

PS1='\[\033[01;32m\]\h\[\033[00m\]:\[\033[01;34m\] \w\[\033[00m\] \A\$ '

# some more ls aliases
alias j='jobs'
alias ls='ls -G'
alias ll='ls -alhF -G'
alias la='ls -A -G'
alias l='ls -CF -G'
alias vb='vi ~/.bashrc;. ~/.bashrc'
alias g='grep --color=auto'
alias u='cd ..'
alias p='cd -'
alias h='history'
alias ge='gedit'
alias le='less'
alias lt='ls -ltr|tail'
alias df='df -h'
alias vex='/home/build/static/projects/vex/vex'
alias acs='apt-cache search'
alias ach='apt-cache show'
alias agi='sudo apt-get install'
alias prod='prodaccess'
alias chrome='google-chrome'
alias ocaml='ledit ocaml'

# Mac specific
#alias gedit='open -a gedit'
#alias top='top -o cpu'

export OCAML_VERSION=4.01.0
#export OCAML_VERSION=4.00.1
#export OCAML_VERSION=3.12.1
export PATH=$PATH:/Developer/NVIDIA/CUDA-5.0/bin:/opt/godi/bin:/opt/godi/sbin:/Users/georgezhou/.opam/$OCAML_VERSION/bin:~/bin:$HOME/Library/Haskell/bin:/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin
export MANPATH=$MANPATH:/opt/godi/man
export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:/Developer/NVIDIA/CUDA-5.0/lib:/usr/local/cuda/lib/
#:/opt/local/lib/
#export OCAMLLIB=/opt/godi/lib/ocaml/std-lib
export OCAMLLIB=/Users/georgezhou/.opam/$OCAML_VERSION/lib/ocaml
#export OCAMLRUNPARAM=b
export G4HTTPPROXY=chinaproxy.corp.google.com:3128
export CAML_LD_LIBRARY_PATH=/Users/georgezhou/.opam/$OCAML_VERSION/lib/stublibs
#export CAML_LD_LIBRARY_PATH=/usr/local/lib/ocaml/stublibs:/Users/georgezhou/.opam/system/lib/stublibs

# added by Anaconda 1.5.1 installer
#export PATH="/Users/georgezhou/anaconda/bin:$PATH"
#export PYTHONPATH=/usr/local/lib/python2.7/site-packages/:$PYTHONPATH
export PYTHONPATH=/Applications/Xcode.app/Contents/SharedFrameworks/LLDB.framework/Resources/Python:$PYTHONPATH

##
# Your previous /Users/georgezhou/.bash_profile file was backed up as /Users/georgezhou/.bash_profile.macports-saved_2013-10-24_at_13:04:22
##

#Fink
#test -r /sw/bin/init.sh && . /sw/bin/init.sh
#export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/sw/lib/pkgconfig

# MacPorts Installer addition on 2013-10-24_at_13:04:22: adding an appropriate PATH variable for use with MacPorts.
#export PATH=$PATH:/opt/local/bin:/opt/local/sbin #:$PATH
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

#export PATH=/usr/local/bin:$PATH
export NDKROOT=/s/android-ndk
export SCALA_HOME=/Applications/scala-2.10.3/
export LDPATHS=/usr/local/cuda/lib/:$LDPATHS
#export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/usr/local/lib/pkgconfig
#export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/usr/local/Cellar
#export PATH=$PATH:/usr/local/Cellar/pkg-config/0.28/bin/
#export CPATH=$C_INCLUDE_PATH:/sw/include:/sw/include/gtksourceview-2.0
export LD_LIBRARY_PATH=.
