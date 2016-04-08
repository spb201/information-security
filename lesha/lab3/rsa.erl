-module(rsa).
-import(crypto, [mod_pow/3]).


bin_to_int(Bin) when is_binary(Bin) ->
    Bits = bit_size(Bin),
    <<Integer:Bits/integer>> = Bin,
    Integer;
bin_to_int(undefined) ->
    undefined.



trim(Bin) when is_binary(Bin) ->
        list_to_binary(trim(binary_to_list(Bin)));
trim(String) when is_list(String) ->
    F = fun(Elem) ->
        case Elem of
            0 -> true;
            Otherwise -> false
        end
    end,
    String2 = lists:dropwhile(F, String),
    lists:reverse(lists:dropwhile(F, lists:reverse
(String2))).




encrypt(Size, Key1, Key2, Bin) ->
    Size1 = Size-1,
    Y = Size1 - (bit_size(Bin) rem Size1),
    FileBlockified = [ A || <<A:Size1>> <= <<Bin/binary, 0:Y>> ],
    F = fun(Elem, Acc) ->
        Block = bin_to_int(mod_pow(Elem, Key1, Key2)),
        <<Acc/bits, Block:Size>>
    end,
    lists:foldl(F, <<>>, FileBlockified).


decrypt(Size, Key1, Key2, Bin) ->
    Size1 = Size-1,
    Y = Size - (bit_size(Bin) rem Size),
    FileBlockified = [ A || <<A:Size>> <= <<Bin/binary, 0:Y>> ],
    F = fun(Elem, Acc) ->
        Block = bin_to_int(mod_pow(Elem, Key1, Key2)),
        <<Acc/bits, Block:Size1>>
    end,
    lists:foldl(F, <<>>, FileBlockified).


main(["e", FileKey, FileFrom, FileTo]) ->
    {ok, Bin} = file:read_file(FileFrom),
    {ok, KeyBin} = file:read_file(FileKey),
    [SizeStr, Key1Str, Key2Str] = string:tokens(binary:bin_to_list(KeyBin), ":"),
    Size = list_to_integer(SizeStr),
    Key1 = list_to_integer(Key1Str),
    Key2 = list_to_integer(Key2Str),
    Encrypted = encrypt(Size, Key1, Key2, Bin),
    Y = 8 - (bit_size(Encrypted) rem 8),
    file:write_file(FileTo, <<Encrypted/bits, 0:Y>>);


main(["d", FileKey, FileFrom, FileTo]) ->
    {ok, Bin} = file:read_file(FileFrom),
    {ok, KeyBin} = file:read_file(FileKey),
    [SizeStr, Key1Str, Key2Str] = string:tokens(binary:bin_to_list(KeyBin), ":"),
    Size = list_to_integer(SizeStr),
    Key1 = list_to_integer(Key1Str),
    Key2 = list_to_integer(Key2Str),
    Decrypted = decrypt(Size, Key1, Key2, Bin),
    Y = 8 - (bit_size(Decrypted) rem 8),
    FileWhat = <<Decrypted/bits, 0:Y>>,
    file:write_file(FileTo, trim(FileWhat));


main(_) ->
    usage().


usage() ->
    io:format("usage:\n"),
    io:format("escript rsa.erl e [key size] [first key] [second key] [source file] [destination file]\n"),
    io:format("escript rsa.erl d [key size] [first key] [second key] [source file] [destination file]\n").

