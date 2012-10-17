set erl=erl
rem set erl="C:\Program Files\erl5.7.4\bin\erl.exe"

set ebin=./ebin ./modules/mod_oauth/deps/erlang-oauth/ebin ./proto_beam/ebin ./deps/mochiweb/ebin ./deps/webzmachine/ebin 
%erl% +P 10000000 -pa %ebin% -sname zotonic -setcookie cookie -boot start_sasl -sasl errlog_type error -s zotonic
