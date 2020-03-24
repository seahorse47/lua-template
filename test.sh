#!/bin/sh

if [ x$LUA == x ]
then
    LUA="lua"
    # LUA="luajit"
fi

$LUA -v

$LUA -e "package.path=package.path..[[;./src/?.lua;./tests/?.lua]]" "$@"
