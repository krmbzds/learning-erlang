-module(myfile).
-export([read/1, test_all/0]).

read(File) ->
  case file:read_file(File) of
    {ok, Bin} -> Bin;
    {error, enoent} -> throw(file_not_found)
  end.

% --------------------- TESTS ---------------------

test_all() ->
  read_test_success(),
  read_test_error(),
  test_all_ok.

read_test_success() ->
  true = is_binary(read("myfile.erl")),
  read_test_success_ok.

read_test_error() ->
  try
    read("void.erl"),
    error(read_test_error_failed)
  catch
    throw:file_not_found ->
      read_test_error_ok
  end.
