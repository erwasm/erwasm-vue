-module(conf2_service).

-export([
  request/1
]).

contains(_Q, []) -> false;
contains(Q, [Q | _Tail]) -> true;
contains(Q, [_NoMatch | Tail]) -> contains(Q, Tail).

is_x_enabled(Param1) -> contains(<<"basex">>, Param1).

is_y_enabled(Param1) -> contains(<<"basey">>, Param1).

is_b_enabled(Param1, Param2) ->
  is_x_enabled(Param1) and contains(<<"extra_b">>, Param2).

is_c_enabled(Param1, Param2) ->
  is_y_enabled(Param1) and contains(<<"extra_c">>, Param2).

is_d_enabled(Param1, Param2) ->
  not is_x_enabled(Param1) and is_c_enabled(Param1, Param2) and contains(<<"extra_d">>, Param2).

handle([<<"details">>, Param1, Param2]) ->
  {[
    { x_allowed, is_x_enabled(Param1) },
    { y_allowed, is_y_enabled(Param1) },
    { b_enabled, is_b_enabled(Param1, Param2) },
    { c_enabled, is_c_enabled(Param1, Param2) },
    { d_enabled, is_d_enabled(Param1, Param2) }
  ]};

handle(_R) -> [404, <<"COULD KNOWN NOT">>].

request(InBytes) ->
  { ok, Data, _Rest } = jsone_decode:decode(InBytes),
  Response = handle(Data),
  { ok, RData } = jsone_encode:encode(Response),
  RData.
