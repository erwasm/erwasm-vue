-module(price_service).

-export([
  request/1
]).


handle(<<"quote">>) ->
  { [
     { base, 5 },
     { full, 15 }
  ] };


handle(_R) -> [404, <<"COULD KNOWN NOT">>].

request(InBytes) ->
  { ok, Data, _Rest } = jsone_decode:decode(InBytes),
  Response = handle(Data),
  { ok, RData } = jsone_encode:encode(Response),
  RData.
