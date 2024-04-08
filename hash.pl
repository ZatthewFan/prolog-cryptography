% split_into_groups_of_8([], []).
% split_into_groups_of_8(List, [Chunk|Chunks]) :-
%     length(Chunk, 8),
%     append(Chunk, Rest, List),
%     split_into_groups_of_8(Rest, Chunks).

% join_groups_of_8([], []).
% join_groups_of_8([Chunk|Chunks], Result) :-
%     join_groups_of_8(Chunks, Remaining),
%     append(Chunk, Remaining, Result).

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

decimal_to_binary(0, [0]).
decimal_to_binary(1, [1]).
decimal_to_binary(N, Binary) :-
    N > 1,
    N1 is N // 2,
    R is N mod 2,
    decimal_to_binary(N1, Rest),
    append(Rest, [R], Binary).

decimal_to_ascii([], []).
decimal_to_ascii([Decimal|Decimals], [Char|Chars]) :-
    char_code(Char, Decimal),
    decimal_to_ascii(Decimals, Chars).

write_ascii_to_file(Filename, Chars) :-
    open(Filename, write, Stream),
    maplist(put_char(Stream), Chars),
    close(Stream).

pad_file_for_sha(InputFile, OutputFile, Result) :-
    % read file and get length
    read_file(InputFile, Content),
    length(Content, ContentBytes),
    ContentBits is ContentBytes * 8,

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
    binary_to_decimal(BinaryPadding, DecimalPadding),
    decimal_to_ascii(DecimalPadding, AsciiPadding),

    % append the padding to InputFile's content
    append(Content, AsciiPadding, Result),
    
    % write to OutputFile   NOTE: temporary only, should keep the BITS instead of writing to file
    write_ascii_to_file(OutputFile, Result).
