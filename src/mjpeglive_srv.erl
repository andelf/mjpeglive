%%%-------------------------------------------------------------------
%%% @author Wang ShuYu <andelf@gmail.com>
%%% @copyright (C) 2013, Wang ShuYu
%%% @doc
%%%
%%% @end
%%% Created :  4 Apr 2013 by Wang ShuYu <andelf@gmail.com>
%%%-------------------------------------------------------------------
-module(mjpeglive_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, add_listener/1, statistics/0, remove_listener/1, ack/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {port,
                recv_group=dict:new(),
                counter=0
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_listener(Pid) ->
    gen_server:call(?SERVER, {add_listener, Pid}).

remove_listener(Pid) ->
    gen_server:call(?SERVER, {remove_listener, Pid}).

ack(Pid) ->
    gen_server:call(?SERVER, {ack, Pid}).

statistics() ->
    gen_server:call(?SERVER, get_statistics).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    %% capturer_mjpeg support some commands, for help, refer
    %% capturer_mjpeg -h
    Program = filename:join(code:priv_dir(mjpeglive), "capturer_mjpeg"),
    Port = open_port({spawn_executable, Program}, [{args, ["-D", "/dev/video1",
                                                           "-p", "3",
                                                           "-w", "320*240"]},
                                                   stream,
                                                   binary]),
    erlang:port_command(Port, "\r"),
    {ok, #state{port=Port}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_listener, Pid}, _From, #state{recv_group=Dict} = State) ->
    Reply = ok,
    {reply, Reply, State#state{recv_group=dict:store(Pid, remote_waiting, Dict)}};
handle_call({remove_listener, Pid}, _From, #state{recv_group=Dict} = State) ->
    Reply = ok,
    {reply, Reply, State#state{recv_group=dict:erase(Pid, Dict)}};
handle_call(get_statistics, _From, State = #state{recv_group=Dict, counter=Count}) ->
    Reply = [{num_of_listener, dict:size(Dict)}, {num_of_frame, Count}],
    {reply, Reply, State};
handle_call({ack, Pid}, _From, State = #state{recv_group=Dict}) ->
    Reply = ok,
    {reply, Reply, State#state{recv_group=dict:store(Pid, remote_waiting, Dict)}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({Port, {data, Jpeg}}, State = #state{port=Port,recv_group=Dict,counter=Count}) ->
    NewDict = dict:map(fun(Pid, remote_waiting) when is_pid(Pid) ->
                               Pid ! {jpeg, Jpeg},
                               remote_sending;
                          (_Pid, remote_sending) ->
                               remote_sending
                       end, Dict),
    erlang:port_command(Port, "\r"),
    {noreply, State#state{counter=Count+1, recv_group=NewDict}};
handle_info({Port, closed}, State = #state{port=Port}) ->
    {stop, port_closed, State};
handle_info({'EXIT', Port, Reason}, State = #state{port=Port}) ->
    {stop, {port_exit, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{port=Port}) ->
    erlang:port_command(Port, "q\r"),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
