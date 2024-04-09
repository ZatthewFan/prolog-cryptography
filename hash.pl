% This is an implementation of SHA-1 Hash Function
% usage: 
% initial hash values
h0(0x67452301).
h1(0xefcdab89).
h2(0x98badcfe).
h3(0x10325476).
h4(0xc3d2e1f0).

% N is the desired length
pad_with_zeros(N, List, Result) :-
    length(Zeros, N),
    maplist(=(0), Zeros),
    append(List, Zeros, Result).

pad_with_zeros_front(N, List, Result) :-
    length(Zeros, N),
    maplist(=(0), Zeros),
    append(Zeros, List, Result).

round_up_to_512_multiple(Number, Result) :-
    Result is ceiling(Number / 512) * 512.

read_file(Filename, Content) :-
    open(Filename, read, Stream),
    read_stream(Stream, Content),
    close(Stream).

read_stream(Stream, []) :-
    at_end_of_stream(Stream), !.
read_stream(Stream, [Char|Rest]) :-
    get_char(Stream, Char),
    read_stream(Stream, Rest).

binary_to_decimal([], []).
binary_to_decimal([Bin1, Bin2, Bin3, Bin4, Bin5, Bin6, Bin7, Bin8|Rest], [Decimal|Decimals]) :-
    Decimal is Bin1*128 + Bin2*64 + Bin3*32 + Bin4*16 + Bin5*8 + Bin6*4 + Bin7*2 + Bin8*1,
    binary_to_decimal(Rest, Decimals).

% converts a single decimal to binary
decimal_to_binary(0, [0]).
decimal_to_binary(1, [1]).
decimal_to_binary(N, Binary) :-
    N > 1,
    N1 is N // 2,
    R is N mod 2,
    decimal_to_binary(N1, Rest),
    append(Rest, [R], Binary).

% converts a list of decimals to binaries
decimals_to_binary([], []).
decimals_to_binary([Decimal|Decimals], [PaddedBinary|Binaries]) :-
    decimal_to_binary(Decimal, Binary),
    length(Binary, NumBits),
    PaddingLength is 8 - NumBits,
    pad_with_zeros_front(PaddingLength, Binary, PaddedBinary),
    decimals_to_binary(Decimals, Binaries).

decimal_to_ascii([], []).
decimal_to_ascii([Decimal|Decimals], [Char|Chars]) :-
    char_code(Char, Decimal),
    decimal_to_ascii(Decimals, Chars).

ascii_to_decimal([], []).
ascii_to_decimal([Char|Chars], [Code|Codes]) :-
    char_code(Char, Code),
    ascii_to_decimal(Chars, Codes).

write_ascii_to_file(Filename, Chars) :-
    open(Filename, write, Stream),
    maplist(put_char(Stream), Chars),
    close(Stream).

flatten_list([], []).
flatten_list([H|T], FlatList) :-
    flatten_list(T, Rest),
    append(H, Rest, FlatList).

% N groups
split_into_groups(_, [], []).
split_into_groups(N, List, [Chunk|Chunks]) :-
    length(Chunk, N),
    append(Chunk, Rest, List),
    split_into_groups(N, Rest, Chunks).

% preprocessing
pad_file(InputFile, Result) :-
    % read file and get length
    read_file(InputFile, Content),
    length(Content, ContentBytes),
    ContentBits is ContentBytes * 8,

    % Convert text file to binary format
    ascii_to_decimal(Content, DecimalContent),
    decimals_to_binary(DecimalContent, BinaryContent),
    flatten_list(BinaryContent, BinaryContentFlat),

    % calculate the number of padding bytes needed; -1 is compensation for the 1 bit, -64 is so that is it 64 bits away from the nearest multiple of 512
    round_up_to_512_multiple(ContentBits, ZeroPadLength),
    PaddingLengthZeros is ZeroPadLength - ContentBits - 1 - 64,
    
    % create padding with bit 1 followed by 0 until it reaches the desired padding length
    pad_with_zeros(PaddingLengthZeros, [1], ZeroPad),

    % create padding that encodes the length of InputFile's content as a 64 bit integer
    decimal_to_binary(ContentBits, ContentBitsBinary),
    length(ContentBitsBinary, LengthCBB),
    PaddingLengthCBB is 64 - LengthCBB,
    pad_with_zeros_front(PaddingLengthCBB, ContentBitsBinary, PaddedContentBitsBinary),

    % combine the padding
    append(ZeroPad, PaddedContentBitsBinary, BinaryPadding),

    % append the padding to InputFile's content
    append(BinaryContentFlat, BinaryPadding, Result).

partition_file_bits(InputFile, Result) :-
    pad_file(InputFile, Bits),
    split_into_groups(512, Bits, ChunkedResult),
    maplist(split_into_groups(32), ChunkedResult, Result).
