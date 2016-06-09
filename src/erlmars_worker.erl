%%%-------------------------------------------------------------------
%%% @author Emir Ozer <>
%%% @copyright (C) 2016, Emir Ozer
%%% @doc
%%%
%%% @end
%%% Created : 14 Feb 2016 by Emir Ozer <>
%%%-------------------------------------------------------------------
-module(erlmars_worker).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include_lib("webmachine/include/webmachine.hrl").
-compile([{parse_transform, lager_transform}]).

-record(state, {}).

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
    lager:start(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    Timer = erlang:send_after(1, self(), check),
    {ok, Timer}.

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
handle_info(_Info, OldTimer) ->
    erlang:cancel_timer(OldTimer),
    do_task(),
    Timer = erlang:send_after(15000, self(), check),
    {noreply, Timer}.

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
terminate(_Reason, _State) ->
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


do_task()->
    inets:start(),
    Response = httpc:request(get, {"http://marsweather.ingenology.com/v1/latest/", []}, [], []),
    lager:info("Received http response body: ~p", [Response]),
    Body = response_body(Response),
    Binarjson = list_to_binary(Body),
    DecodedJson = jsx:decode(Binarjson, [return_maps]),
    Report = maps:get(<<"report">>, DecodedJson),
    lager:info("conversion to erlang map completed: ~p", [Report]),

    TerrestrialDate = maps:get(<<"terrestrial_date">>, Report),
    Sol = maps:get(<<"sol">>, Report),
    Ls = maps:get(<<"ls">>, Report),
    Min_Temp = maps:get(<<"min_temp">>, Report),
    Min_Temp_Fahrenheit = maps:get(<<"min_temp_fahrenheit">>, Report),
    Max_Temp = maps:get(<<"max_temp">>, Report),
    Max_Temp_Fahrenheit = maps:get(<<"max_temp_fahrenheit">>, Report),
    Pressure = maps:get(<<"pressure">>, Report),
    Pressure_String = maps:get(<<"pressure">>, Report),
    Abs_Humidity = maps:get(<<"abs_humidity">>, Report),
    Wind_Speed = maps:get(<<"wind_speed">>, Report),
    Wind_Direction = maps:get(<<"wind_direction">>, Report),
    Atmo_Opacity = maps:get(<<"atmo_opacity">>, Report),
    Season = maps:get(<<"season">>, Report),
    Sunrise = maps:get(<<"sunrise">>, Report),
    Sunset = maps:get(<<"sunset">>, Report),

    PotentialEntry = [TerrestrialDate, Sol, Ls, Min_Temp, Min_Temp_Fahrenheit, Max_Temp, Max_Temp_Fahrenheit, 
                      Pressure,Pressure_String, Abs_Humidity, Wind_Speed, Wind_Direction, Atmo_Opacity, Season,
                     Sunrise, Sunset],
    PgUser = os:getenv("PGUSER"),
    PgPass = os:getenv("PGPASS"),
    PgDbName = os:getenv("PGDBNAME"),
    PgUrl = os:getenv("PGURL"),
    {ok, C}  = epgsql:connect(PgUrl, PgUser, PgPass, 
                              [{database, PgDbName},
                               {port, 5432},
                               {timeout, 4000}
                              ]),
    
    SelectRes = epgsql:squery(C, "SELECT * FROM mars_weather ORDER BY terrestrial_date DESC LIMIT 1;"),
    
    lager:info("latest entry in pg: ~p", [SelectRes]),
    {_, _, [X]} = SelectRes,
    LatestEntryTime = element(1, X),
    lager:info("latest entry terrestrial_date pg: ~p", [LatestEntryTime]),

    eval_latest_entry(LatestEntryTime, TerrestrialDate, C, PotentialEntry),

    ok = epgsql:close(C),
    inets:stop().
    
% unpacking the http resp
response_body({ok, { _, _, Body}}) -> Body.

build_insert_query(C, PotentialEntry)->
    Query = "INSERT INTO mars_weather (terrestrial_date, sol, ls, min_temp, min_temp_fahrenheit, max_temp, max_temp_fahrenheit, pressure, pressure_string, abs_humidity, wind_speed, wind_direction, atmo_opacity, season, sunrise, sunset) VALUES (",
    ProcessedQ = appendWithTail(Query, PotentialEntry),
    ClosingQuery =  ");",
    Final = ProcessedQ ++ ClosingQuery,
    lager:info("final statement to run: ~p", [Final]),
    InsertRes = epgsql:squery(C, Final),
    lager:info("result: ~p", [InsertRes]).

eval_latest_entry(A, A, C, PotentialEntry) ->
    lager:info("latest entry terrestrial_date is the latest API response!");
eval_latest_entry(A, B, C, PotentialEntry) ->
    lager:info("New data entry detected, inserting to pg"),
    build_insert_query(C, PotentialEntry).                    

appendWithTail(BeginningQuery ,List) when length(List) > 1 ->
    [Head | Tail] = List,
    if 
        is_atom(Head) == true ->  New = BeginningQuery ++ atom_to_list(Head) ++ ", ";
        is_binary(Head) == true ->  New = BeginningQuery ++ "'" ++ binary_to_list(Head) ++ "'" ++", ";
        is_integer(Head) == true -> New = BeginningQuery ++ "'" ++ integer_to_list(Head) ++ "'" ++ ", ";
        is_float(Head) == true ->  New = BeginningQuery ++ "'" ++float_to_list(Head,[{decimals,2}])++"'"  ++ ", ";
        true ->  New = BeginningQuery ++ "'" ++Head++ "'"++ ", "
    end,
    appendWithTail(New, Tail);

appendWithTail(BeginningQuery ,List)  when length(List) == 1 ->
    [Head | _] = List,
    New = BeginningQuery ++ "'" ++binary_to_list(Head)++"'",
    New.

fetch_mars_weather_data(C) ->
    Query = "SELECT * FROM mars_weather ORDER BY terrestrial_date DESC LIMIT 30;",
    SelectRes = epgsql:squery(C, Query).
