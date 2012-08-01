#!/bin/sh
erlc ../src/*.erl
move ../src/*.beam .
yaws --conf ../etc/yaws.conf
