-module(conf_service).

-export([
  request/1
]).

contains(_Q, []) -> false;
contains(Q, [Q | _Tail]) -> true;
contains(Q, [_NoMatch | Tail]) -> contains(Q, Tail).

is_x_enabled(Param1) -> contains(<<"basex">>, Param1).

is_y_enabled(Param1) -> contains(<<"basey">>, Param1).

is_b_enabled(Param1, Param2) ->
  case is_x_enabled(Param1) of 
    false -> false;
    true -> contains(<<"extra_b">>, Param2)
  end.

is_c_enabled(Param1, Param2) ->
  case is_y_enabled(Param1) of
    false -> false;
    true -> contains(<<"extra_c">>, Param2)
  end.

is_d_enabled(Param1, Param2) ->
   case is_x_enabled(Param1) of
     true -> false;
     false -> case is_c_enabled(Param1, Param2) of 
       true -> contains(<<"extra_d">>, Param2);
       false -> false
     end
   end.

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
