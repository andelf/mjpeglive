%%%-------------------------------------------------------------------
%%% @author Wang ShuYu <andelf@gmail.com>
%%% @copyright (C) 2013, Wang ShuYu
%%% @doc
%%%
%%% @end
%%% Created :  4 Apr 2013 by Wang ShuYu <andelf@gmail.com>
%%%-------------------------------------------------------------------
-module(mjpeglive_yaws_appmod).

%% API
-export([out/1]).
-define(BOUNDARY, "ANDELFTHISISISISISISBOOOOOOOUNDARY").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
out(_Arg) ->
    Self = self(),
    spawn(fun() ->
                  %% Create a random number
                  %%mjpeglive_srv:add_listener(self()),
                  receive _ -> ok
                  after 200 -> ok
                  end,
                  yaws_api:stream_chunk_deliver_blocking(Self, ["--", ?BOUNDARY, "\r\n"]),
                  mjpeglive_srv:add_listener(self()), % here self() is new spawned process
                  rec_loop(Self, 1000)          % can't use self() here
          end),

    {streamcontent, "multipart/x-mixed-replace;boundary=" ++ ?BOUNDARY, <<>>}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
rec_loop(YawsPid, 0) ->
    %%yaws_api:stream_chunk_deliver_blocking(YawsPid, "Hello World"),
    yaws_api:stream_chunk_end(YawsPid),
    mjpeglive_srv:remove_listener(self()),
    exit(normal);

rec_loop(YawsPid, Count) ->
    mjpeglive_srv:ack(self()),
    receive
        {jpeg, Jpeg} ->
            % Data = base64:encode(Jpeg)
            Ret = yaws_api:stream_chunk_deliver_blocking(
                    YawsPid,
                    ["Content-Type: image/jpeg\r\n",
                     "Content-Length: ", integer_to_list(byte_size(Jpeg)), "\r\n", "\r\n",
                     Jpeg,
                     "\r\n",
                     "--", ?BOUNDARY, "\r\n"]),
            case Ret of
                {error,{ypid_crash,_Reason}} ->
                    rec_loop(YawsPid, 0);
                ok ->
                    rec_loop(YawsPid, Count)
            end
    after 10000 ->
            rec_loop(YawsPid, 0)
    end.
