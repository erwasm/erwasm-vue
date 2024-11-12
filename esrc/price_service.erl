-module(price_service).

-export([
  request/1
]).


handle(Request) ->
  case Request of
    <<"quote">> -> 
      { [ 
         { <<"base">>, 5 },
         { <<"full">>, 15 }
      ] }
  end.

request(InBytes) ->
  { ok, Data, _Rest } = jsone_decode:decode(InBytes),
  Response = handle(Data),
  { ok, RData } = jsone_encode:encode(Response),
  RData.
