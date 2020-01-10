-module(erl_blake3).

%% API
-export([hash/1,
         hash_as_bytes/1,
         %% new_hasher/0,
         %% update/2,
         %% finalize/1
        ]).

%% Native lib support
-export([load/0]).
-on_load(load/0).

-spec hash(Data :: binary())-> {ok, binary()} | {error, any()}.
hash(_Data) ->
    not_loaded(?LINE).

hash_as_bytes(Hash) ->
    not_loaded(?LINE).

load() ->
    erlang:load_nif(filename:join(priv(), "libnative"), none).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv()->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end.

