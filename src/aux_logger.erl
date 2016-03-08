-module(aux_logger).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2]).
-export([code_change/3, terminate/2]).

-record(ctx, {writer, formatter, filter, queue = dict:new()}).

%%====================================================================
%% Callback functions
%%====================================================================

init(Args) ->
  {ok, Formatter} = aux_formatter:make_callback(Args),
  {ok, Filter} = aux_formatter:make_filter(Args),
  {ok, Writer} = aux_writer:make_callback(Args),
  {ok, #ctx{writer = Writer, formatter = Formatter, filter = Filter}}.

handle_event({Type, Gleader, {Pid, Fmt, Args}}, #ctx{filter = F} = State) ->
  {_, Severity} = TypeSeverity = type_severity(Type, Fmt),
  case F(TypeSeverity) of
    true ->
      Opts = [
        {pid, Pid},
        {gleader, Gleader},
        {type, message},
        {severity, Severity}
      ],
      NewState = display(Fmt, Args, Opts, State),
      {ok, NewState};
    false ->
      {ok, State}
  end.

handle_info({'DOWN', Ref, process, Pid, normal}, #ctx{queue = Q} = State) ->
  case dict:find(Ref, Q) of
    {ok, Pid} ->
      {ok, State#ctx{queue=dict:erase(Ref, Q)}};
    error ->
      {ok, State}
  end.

handle_call(Request, _State) ->
  {remove_handler, {unknown_request, Request}}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Args, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

display(Fmt, Args, Opts, State) ->
  #ctx{formatter = Formatter, writer = Writer, queue = Q} = State,
  {Pid, Ref} = spawn_monitor(fun() ->
    MoreOpts = [
      {message, io_lib:format(Fmt, Args)},
      {time, os:timestamp()}
    ],
    Msg = Formatter(lists:append(Opts, MoreOpts)),
    Writer(Msg)
  end),
  State#ctx{queue = dict:store(Ref, Pid, Q)}.

type_severity(_, crash_report) -> {report, error};
type_severity(_, supervisor_report) -> {report, info};
type_severity(_, supervisor) -> {report, info};
type_severity(_, progress_report) -> {report, info};
type_severity(_, progress) -> {report, info};
type_severity(_, std_info) -> {report, info};
type_severity(_, std_warning) -> {report, warning};
type_severity(_, std_error) -> {report, error};
type_severity(debug_msg, _) -> {message, debug};
type_severity(error, _) -> {message, error};
type_severity(error_report, _) -> {report, error};
type_severity(warning_msg, _) -> {message, warning};
type_severity(warning_report, _) -> {report, warning};
type_severity(info_msg, _) -> {message, info};
type_severity(info_report, _) -> {report, info};
type_severity(_, _) -> {unknown, error}.
