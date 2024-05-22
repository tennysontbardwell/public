#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

git_current_branch () {
	  local ref
	  ref=$(command git symbolic-ref --quiet HEAD 2> /dev/null)
	  local ret=$?
	  if [[ $ret != 0 ]]
	  then
		    [[ $ret == 128 ]] && return
		    ref=$(command git rev-parse --short HEAD 2> /dev/null)  || return
	  fi
	  echo ${ref#refs/heads/}
}

ggp () {
	  if [[ "$#" != 0 ]] && [[ "$#" != 1 ]]
	  then
		    git push origin "${*}"
	  else
		    [[ "$#" == 0 ]] && local b="$(git_current_branch)"
		    git push origin "${b:=$1}"
	  fi
}

ggl () {
	  if [[ "$#" != 0 ]] && [[ "$#" != 1 ]]
	  then
		    git pull origin "${*}"
	  else
		    [[ "$#" == 0 ]] && local b="$(git_current_branch)"
		    git pull origin "${b:=$1}"
	  fi
}

ggpnp () {
	  if [[ "$#" == 0 ]]
	  then
		    ggl && ggp
	  else
		    ggl "${*}" && ggp "${*}"
	  fi
}

function gst {
    git status
}

function gwip {
    git add -A; git rm $(git ls-files --deleted) 2> /dev/null; git commit --no-verify --no-gpg-sign -m "--wip-- [skip ci]"
}

function ggwip {
    gst && (gwip || :) && ggpnp
}

ggwip
