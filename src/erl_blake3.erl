-module(erl_blake3).

%% API
-export([hash/1,
         as_bytes/1,
         to_hex/1,
         new/0,
         update/2,
         finalize/1
        ]).

%% Native lib support
-export([load/0]).
-on_load(load/0).

-spec hash(Data :: binary())-> {ok, reference()} | {error, any()}.
hash(_Data) ->
    not_loaded(?LINE).

-spec as_bytes(Hash :: reference())-> {ok, binary()} | {error, any()}.
as_bytes(_Hash) ->
    not_loaded(?LINE).

-spec to_hex(Hash :: reference())-> {ok, binary()} | {error, any()}.
to_hex(_Hash) ->
    not_loaded(?LINE).

-spec new()-> {ok, reference()} | {error, any()}.
new() ->
    not_loaded(?LINE).

-spec update(Hasher :: reference(), Data :: binary())-> {ok, reference()} | {error, any()}.
update(_Hasher, _Data) ->
    not_loaded(?LINE).

-spec finalize(Hasher :: reference())-> {ok, reference()} | {error, any()}.
finalize(_Hasher) ->
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

