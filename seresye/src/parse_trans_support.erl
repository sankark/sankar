-module(parse_trans_support).
-export([ex1/0]).
-compile(export_all).


-record(context, {module,
		  function,
		  arity,
                  file,
                  options}).
ex1() ->
    parse_trans_mod:transform_module(
      template1, [fun(Fs, _Os) ->
		    transform_ex1(Fs,_Os)
	    end], [{pt_pp_src,false}]).


transform_ex1(Forms, _Opts) ->
	
	{RecNames, Rec} = parse_records1(),
	F=parse_trans:do_insert_forms(above,compile_attribute(),Forms,#context{}),
	{ok,ExportRecords}=export_records(RecNames),
	NewF=parse_trans:do_insert_forms(above,Rec,F,#context{}),
	NewF2=parse_trans:do_insert_forms(above,ExportRecords,NewF,#context{}),
	{ok,Fd} = file:open("ex1.xfm", [write]),
	parse_trans_pp:pp_src(NewF2,"ex1.xfm"),
	file:close(Fd),
	TemplateForm=parse_trans_mod:rename_module(NewF2,template),
	{ok, Module, Bin} = compile:forms(TemplateForm,[binary,debug_info]),
	FileName="C:/tmp/sankar/zotonic-update/ebin/template.beam",
	{ok, File}=file:open(FileName,[write]),
	file:write(File,Bin),
	%io:format(File, "~p", [Binary]),
    file:close(File),
	code:load_binary(Module,FileName , Bin),
    io:fwrite("Forms = ~p~n", [Module]),
    Forms.

parse_records1() ->
	Files=scan_proto(),
	Rec=lists:flatten(lists:foldl(fun(H,A)-> {ok,Rec1}=parse_records(H,[]),
							   case Rec1 of
								   [] -> A;
								   _ -> [Rec1|A]
							   end
				 end, [], Files)),
	RecNames=lists:flatten(lists:foldl(fun(H,A)-> {ok,Rec2}=record_names(H,[]),
							   case Rec2 of
								   [] -> A;
								   _ -> [Rec2|A]
							   end
				 end, [], Files)),
	io:format("~p~n~p",[RecNames,Rec]),
    {RecNames, Rec}.

scan_proto()->
	{ok, Path} = application:get_env(seresye,lib_dir),
	Hrl   = filename:join([Path,"src","proto_source","*.erl"]),
    Files = filelib:wildcard(Hrl) ,
    Files.


compile_attribute()->
[{attribute,1,compile,{parse_transform,exprecs}}].

export_records(RecNames)->
	{ok,[{attribute,4,export_records,RecNames}]}.
parse_records(File, Opts) ->
    Cwd = ".",
    Dir = filename:dirname(File),
    IncludePath = [Cwd,Dir|inc_paths(Opts)],
    case epp:parse_file(File, IncludePath, pre_defs(Opts)) of
        {ok,Forms} ->
            {ok,record_attrs(Forms)};
        Error ->
            Error
    end.
pre_defs([{d,M,V}|Opts]) ->
    [{M,V}|pre_defs(Opts)];
pre_defs([{d,M}|Opts]) ->
    [M|pre_defs(Opts)];
pre_defs([_|Opts]) ->
    pre_defs(Opts);
pre_defs([]) -> [].

inc_paths(Opts) ->
    [P || {i,P} <- Opts, is_list(P)].

record_attrs(Forms) ->
    [A || A = {attribute,_,record,_D} <- Forms].

record_names(File, Opts) ->
	case parse_records(File, Opts) of 
		{ok,[]}->{ok,[]};
		{ok,Records}->{ok,[RecName || {attribute,_,record,{RecName,_D}} <- Records]}
		
	end.
	
