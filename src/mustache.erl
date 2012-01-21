%% mustache.erl
%%%
%%% @author Christopher Hall <hsw@ms2.hinet.net>
%%% @copyright 2012 Christopher Hall <hsw@ms2.hinet.net>
%%% @license BSD3 see LICENSE.text for details
%%%
%%% @doc erlang implementation of mustache

-module(mustache).
-author("Christopher Hall <hsw@ms2.hinet.net>").

%% exports
-export([render/2]).
-export([html_escape/1]).

%% records
-record(render_state, {
          context,
          section_re,
          variable_re}).


%% rendering
%% ==========

-spec render(string(), dict()) -> iolist().
%% @doc given a binary utf8 string as a template
%%      and a context dictionary render a mustache template
render(Template, Context) ->
    {ok, Section_RE} = re:compile("^(.*?)\\{\\{([#^])\\s*([^\}]+)\\s*\\}\\}\\s*(.+?)\\{\\{/\\s*\\3\\s*\\}\\}\\s*(.*)", [dotall, unicode]),
    {ok, Variable_RE} = re:compile("^(.*?)(?|\\{\\{(\\{)\\s*([^\}]+)\\s*\\}\\}\\}|\\{\\{([&!>])?\\s*([^\}]+)\\s*\\}\\})(.*)", [dotall, unicode]),
    State = #render_state{context = Context,
                          section_re = Section_RE,
                          variable_re = Variable_RE},
    process_sections(Template, Context, State).


process_sections(Template, Context, State) ->
    M = re:run(Template, State#render_state.section_re, [{capture, all, list}]),
    io:format("M = ~p~n", [M]),
    case M of
        nomatch ->
            expand_variables(Template, Context, State);

        {match, [_,  Before_Str, Tag_Char, Section_Name, Section_Str, After_Str]} ->
            io:format("SN = ~p~n", [Section_Name]),
            [ process_sections(Before_Str, Context, State),
              expand_section(Tag_Char, Section_Name, Section_Str, Context, State),
              process_sections(After_Str, Context, State) ]

    end.


expand_section("^", Name, Section_Str, Context, State) ->
    case value_of(Name, Context) of
        F when F =:= false; F =:= none; F =:= undefined; F =:= []; F =:= <<>>; F =:= 0 ->
            process_sections(Section_Str, Context, State);
        _ ->
            [""]
    end;

expand_section("#", Name, Section_Str, Context, State) ->
    io:format("X(~p) => ~p~n", [Name, Section_Str]),

    case value_of(Name, Context) of
        F when F =:= false; F =:= none; F =:= undefined; F =:= []; F =:= <<>>; F =:= 0 ->
            [""];
        [H | _T] = L when is_tuple(H) ->
            lists:map(
              fun({SubContext}) ->
                      io:format("X=~p~n", [SubContext]),
                      process_sections(Section_Str, SubContext, State)
              end, L);
        {D} ->
            process_sections(Section_Str, D, State);
        Fn when is_function(Fn) ->
            Frender = fun(S) ->
                             process_sections(S, Context, State)
                      end,
            Fn(Section_Str, Frender);
        _ ->   % just assume true
            process_sections(Section_Str, Context, State)
    end.


expand_variables(Template, Context, State) ->
    M2 = re:run(Template, State#render_state.variable_re, [{capture, all, list}]),
    io:format("M2 = ~p~n", [M2]),
    case M2 of
        nomatch ->
            [Template];
        {match, [_, Before_Str, Tag_Char, Var_Name, After_Str]} ->
            Value =
                case Tag_Char of
                    "!" ->
                        "";
                    ">" ->
                        Partial = text_helper(Var_Name, State#render_state.context),
                        process_sections(Partial, Context, State);
                    C when C =:= "{"; C =:= "&" ->
                        text_helper(Var_Name, Context);
                    _ ->
                        html_escape(text_helper(Var_Name, Context))
                end,
            [ expand_variables(Before_Str, Context, State) | [ Value | expand_variables(After_Str, Context, State) ]]

    end.


-spec value_of(string(), dict()) -> any().
%% @doc return the value of a context item
%%      missing variables produce 'undefined'
value_of(Name, Context) ->
    case dict:find(Name, Context) of
        {ok, Value} ->
            Value;
        error ->
            case dict:find(list_to_atom(Name), Context) of
                {ok, Value} ->
                    Value;
                error ->
                    undefined
            end
    end.


-spec text_helper(string(), dict()) -> any().
%% @doc helper
text_helper(Name, Context) ->
    text_of(value_of(Name, Context), Context).


-spec text_of(any(), dict()) -> string() | binary().
%% @doc convert value to string
text_of(I, _Context) when is_integer(I) ->
    io_lib:format("~b", [I]);
text_of(X, _context) when is_float(X) ->
    io_lib:format("~g", [X]);
text_of(L, _context) when is_list(L) ->
    L;
text_of(B, _context) when is_binary(B) ->
    B;
text_of(F, Context) when is_function(F) ->
    text_of(F(Context), Context);
text_of(A, _Context) when is_atom(A) ->
    atom_to_list(A).


%% HTML escape
%% ===========


quote($<) -> "&lt;";
quote($>) -> "&gt;";
quote($&) -> "&amp;";
quote($") -> "&quot;";
quote($') -> "&apos;";
quote(C) -> C.


-spec html_escape(binary() | string()) -> binary() | iolist().
%% @doc escape the key HTML/XML characters to their entity strings
html_escape(Text) when is_list(Text) ->
    lists:map(fun quote/1, Text);
html_escape(Text) when is_binary(Text) ->
    unicode:characters_to_list([ (quote(C)) || <<C/utf8>> <= Text ]).


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

rts(Template, Context) ->
    unicode:characters_to_list(render(Template, Context), unicode).


basic_test() ->
    Template = "Hello {{name}},\ndid you see {{friend}}?\n",
    Context = dict:from_list([{name, <<"Galla">>},
                               {friend, <<"Pictrix">>}]),
    Result = "Hello Galla,\ndid you see Pictrix?\n",
    ?assertEqual(Result, rts(Template, Context)),
    ok.


basic_utf8_test() ->
    Template = "English -> " ++ [22283,35486] ++ "\nToday's Weather {{weather}}\nTomorrow's Weather {{mrtq}}",
    Context = dict:from_list([{weather, unicode:characters_to_binary([20170,26085,22825,27683])},
                              {mrtq, [26126,26085,22825,27683]}]),
    Result = "English -> " ++ [22283,35486] ++
        "\nToday's Weather " ++ [20170,26085,22825,27683] ++
        "\nTomorrow's Weather " ++ [26126,26085,22825,27683],
    ?assertEqual(Result, rts(Template, Context)),
    ok.


iteration_test() ->
    Template = "{{#items}}\nHello {{name}},\ndid you see {{friend}}?\n{{/items}}\n",
    Context =
        dict:from_list([{items, [{dict:from_list([{name, <<"Galla">>},
                                                  {friend, <<"Pictrix">>}])},
                                 {dict:from_list([{name, "Paeta"},
                                                 {friend, "Antonia Thallusa"}])}]}]),
    Result =
        "Hello Galla,\ndid you see Pictrix?\n" ++
        "Hello Paeta,\ndid you see Antonia Thallusa?\n",
    ?assertEqual(Result, rts(Template, Context)),
    ok.


nested_iteration_test() ->
    Template =
        "{{#data}}\n" ++
        "Greet: {{group}}\n" ++
        "{{#items}}\nHello {{name}},\ndid you see {{friend}}?\n{{/items}}\n" ++
        "Find: {{group}}\n" ++
        "{{#items}}\nHey {{friend}},\nwhere is {{name}}?\n{{/items}}\n" ++
        "{{/data}}\n",
    Context =
        dict:from_list(
          [{data,
            [{dict:from_list(
                [{group, "1"},
                 {items, [{dict:from_list([{name, <<"Galla">>},
                                           {friend, <<"Pictrix">>}])},
                          {dict:from_list([{name, "Paeta"},
                                           {friend, "Antonia Thallusa"}])}]}]

               )},
             {dict:from_list(
                [{group, "2"},
                 {items, [{dict:from_list([{name, <<"Epicydilla">>},
                                           {friend, <<"Drusilla">>}])},
                          {dict:from_list([{name, "Claudia Helvia"},
                                           {friend, "Julia Livilla"}])}]}]
               )}
            ]}]),

    Result =
        "Greet: 1\n" ++
        "Hello Galla,\ndid you see Pictrix?\n" ++
        "Hello Paeta,\ndid you see Antonia Thallusa?\n" ++
        "Find: 1\n" ++
        "Hey Pictrix,\nwhere is Galla?\n" ++
        "Hey Antonia Thallusa,\nwhere is Paeta?\n" ++

        "Greet: 2\n" ++
        "Hello Epicydilla,\ndid you see Drusilla?\n" ++
        "Hello Claudia Helvia,\ndid you see Julia Livilla?\n" ++
        "Find: 2\n" ++
        "Hey Drusilla,\nwhere is Epicydilla?\n" ++
        "Hey Julia Livilla,\nwhere is Claudia Helvia?\n",

    ?assertEqual(Result, rts(Template, Context)),
    ok.


true_false_test() ->
    Template =
        "{{#t_flag}}\n" ++
        "t_flag yes\n" ++
        "{{/t_flag}}\n" ++
        "{{^t_flag}}\n" ++
        "t_flag no\n" ++
        "{{/t_flag}}\n" ++

        "{{#f_flag}}\n" ++
        "f_flag no\n" ++
        "{{/f_flag}}\n" ++
        "{{^f_flag}}\n" ++
        "f_flag yes\n" ++
        "{{/f_flag}}\n" ++

        "{{#empty_list}}\n" ++
        "empty_list no\n" ++
        "{{/empty_list}}\n" ++
        "{{^empty_list}}\n" ++
        "empty_list yes\n" ++
        "{{/empty_list}}\n" ++

        "{{#empty_str}}\n" ++
        "empty_str no\n" ++
        "{{/empty_str}}\n" ++
        "{{^empty_str}}\n" ++
        "empty_str yes\n" ++
        "{{/empty_str}}\n" ++

        "{{#str}}\n" ++
        "str yes\n" ++
        "{{/str}}\n" ++
        "{{^str}}\n" ++
        "str no\n" ++
        "{{/str}}\n" ++

        "{{#empty_bin}}\n" ++
        "empty_bin no\n" ++
        "{{/empty_bin}}\n" ++
        "{{^empty_bin}}\n" ++
        "empty_bin yes\n" ++
        "{{/empty_bin}}\n" ++

        "{{#bin}}\n" ++
        "bin yes\n" ++
        "{{/bin}}\n" ++
        "{{^bin}}\n" ++
        "bin no\n" ++
        "{{/bin}}\n" ++

        "{{#obj}}\n" ++
        "obj yes\n" ++
        "{{/obj}}\n" ++
        "{{^obj}}\n" ++
        "obj no\n" ++
        "{{/obj}}\n" ++

        "{{#not_defined}}\n" ++
        "not_defined no\n" ++
        "{{/not_defined}}\n" ++
        "{{^not_defined}}\n" ++
        "not_defined yes\n" ++
        "{{/not_defined}}\n",
    Context =
        dict:from_list(
          [{t_flag, true},
           {f_flag, false},
           {empty_list, []},
           {empty_str, ""},
           {empty_bin, <<>>},
           {int, 99},
           {str, "asd"},
           {obj, dict:from_list([{group, "1"}])},
           {bin, <<"asd">>}]),

    Result =
        "t_flag yes\n" ++
        "f_flag yes\n" ++
        "empty_list yes\n" ++
        "empty_str yes\n" ++
        "str yes\n" ++
        "empty_bin yes\n" ++
        "bin yes\n" ++
        "obj yes\n" ++
        "not_defined yes\n",

    ?assertEqual(Result, rts(Template, Context)),
    ok.


single_dict_test() ->
    Template =
        "{{#one}}\nHello {{name}},\ndid you see {{friend}}?\n{{/one}}\n" ++
        "{{#two}}\nHey {{friend}},\nwhere is {{name}}?\n{{/two}}\n",

    Context =
        dict:from_list([{one, {dict:from_list([{name, <<"Galla">>},
                                               {friend, <<"Pictrix">>}])}},
                        {two, {dict:from_list([{name, "Paeta"},
                                               {friend, "Antonia Thallusa"}])}}]),
    Result =
        "Hello Galla,\ndid you see Pictrix?\n" ++
        "Hey Antonia Thallusa,\nwhere is Paeta?\n",

    ?assertEqual(Result, rts(Template, Context)),
    ok.


comment_test() ->
    Template =
        "{{!ignore this}}Hello {{name}} world {{!name}}\n",

    Context =
        dict:from_list([{name, "green"},
                        {"ignore this", "purple"}]),
    Result =
        "Hello green world \n",

    ?assertEqual(Result, rts(Template, Context)),
    ok.


escape_test() ->
    Template =
        "{{{name}}} {{name}} {{&name}}\n",

    Context =
        dict:from_list([{name, "<b>green</b>"},
                        {item, "purple"}]),
    Result =
        "<b>green</b> &lt;b&gt;green&lt;/b&gt; <b>green</b>\n",

    ?assertEqual(Result, rts(Template, Context)),
    ok.


a_func(_Context) ->
    "hello there!".

function_test() ->
    Template =
        "{{f1}} {{f2}}\n",

    Context =
        dict:from_list([{f1, fun(_C) -> "Hi," end},
                        {f2, fun a_func/1}]),
    Result =
        "Hi, hello there!\n",

    ?assertEqual(Result, rts(Template, Context)),
    ok.


partial_test() ->
    Template =
        "{{#data}}\n" ++
        "{{>format}}\n" ++
        "{{/data}}\n",

    Context =
        dict:from_list([{data, [{dict:from_list([{name, <<"Galla">>}])},
                                {dict:from_list([{name, <<"Paeta">>}])},
                                {dict:from_list([{name, "Antonia Thallusa"}])}]},
                        {format, "hello {{name}}\n"}]),
    Result =
        "hello Galla\n\nhello Paeta\n\nhello Antonia Thallusa\n\n",

    ?assertEqual(Result, rts(Template, Context)),
    ok.


wrapper_test() ->
    Template =
        "{{#wrap}}\n" ++
        "{{#data}}\n" ++
        "Hello {{name}}!\n" ++
        "{{/data}}\n" ++
        "{{/wrap}}\n",

    Context =
        dict:from_list([{data, [{dict:from_list([{name, <<"Galla">>}])},
                                {dict:from_list([{name, <<"Paeta">>}])},
                                {dict:from_list([{name, "Antonia Thallusa"}])}]},
                        {wrap, fun(S, R) ->
                                       ["<green>", R(S), "</green>"]
                               end}]),
    Result =
        "<green>Hello Galla!\nHello Paeta!\nHello Antonia Thallusa!\n</green>",

    ?assertEqual(Result, rts(Template, Context)),
    ok.

-endif.
