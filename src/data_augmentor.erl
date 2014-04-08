-module(data_augmentor).

-export([retrieveUserData/1]).

retrieveUserData(User) ->
  User = lookupUserData(User),
  ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

lookupUserData(_User) ->
  ok.
