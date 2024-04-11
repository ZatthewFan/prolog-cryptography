% This is an implementation of SHA-1 Hash Function
% usage: 
%   SHA-1: sha1_checksum('<filename>', Hash).
% 
% initial hash values
h0(0x67452301).
h1(0xefcdab89).
h2(0x98badcfe).
h3(0x10325476).
h4(0xc3d2e1f0).


% <--------------------------------------------------utils-------------------------------------------------->
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

% from ChatGPT
binary_to_decimal(BinaryList, Decimal) :-
    binary_to_decimal(BinaryList, 0, Decimal).
binary_to_decimal([], Decimal, Decimal).
binary_to_decimal([Bit|Bits], Acc, Decimal) :-
    NewAcc is (Acc << 1) + Bit,
    binary_to_decimal(Bits, NewAcc, Decimal).

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

xor_words(WordA, WordB, Result) :-
    maplist(xor_bits, WordA, WordB, Result).

xor_bits(BitA, BitB, Result) :-
    Result is BitA xor BitB.

left_rotate(_, [], []).
left_rotate(0, List, List).
left_rotate(N, [H|T], Result) :-
    N > 0,
    append(T, [H], RotatedTail),
    Next is N - 1,
    left_rotate(Next, RotatedTail, Result).
% <--------------------------------------------------------------------------------------------------------->

% preprocessing
pad_file(InputFile, Result) :-
    % read file and get length
    read_file(InputFile, Content),
    length(Content, ContentBytes),
    ContentBits is ContentBytes * 8,

    % create padding that encodes the length of InputFile's content as a 64 bit integer
    % CBB means ContentBitsBinary
    decimal_to_binary(ContentBits, ContentBitsBinary),
    length(ContentBitsBinary, LengthCBB),
    PaddingLengthCBB is 64 - LengthCBB,
    pad_with_zeros_front(PaddingLengthCBB, ContentBitsBinary, PaddedContentBitsBinary),

    % Convert text file to binary format
    ascii_to_decimal(Content, DecimalContent),
    decimals_to_binary(DecimalContent, BinaryContent),
    flatten_list(BinaryContent, BinaryContentFlat),

    % calculate the number of padding bytes needed
    round_up_to_512_multiple(ContentBits, TotalLengthAfterPad),
    
    % check if there's enough room for ContentBits + 64 and if there isn't then allocate more space for the padding
    (TotalLengthAfterPad < (ContentBits + 64) -> 
        ActualTotalLengthAfterPad is TotalLengthAfterPad + 512
    ; 
        ActualTotalLengthAfterPad is TotalLengthAfterPad
    ),
    
    % structure of padding is {ContentBits} + {(1-bit) + 0-bits until 64 bits away from nearest multiple of 512} + {length of ContentBits encoded as a 64-bit int}
    PaddingLengthZeros is ActualTotalLengthAfterPad - (ContentBits + 64),
    
    % determines the padding length and creates a padding with an inital 1 followed by the necessary amount of 0s
    (PaddingLengthZeros =:= 0 ->
        ZeroPad = []
    ;
        (PaddingLengthZerosMinusOne is PaddingLengthZeros - 1,
            pad_with_zeros(PaddingLengthZerosMinusOne, [1], ZeroPad)
        )
    ),

    % combine the padding
    append(ZeroPad, PaddedContentBitsBinary, BinaryPadding),

    % append the padding to InputFile's content
    append(BinaryContentFlat, BinaryPadding, Result).

partition_file_bits(Bits, Result) :-
    % 512-bit chunks
    split_into_groups(512, Bits, ChunkedResult),
    % split each 512-bit chunks into 32-bit words
    maplist(split_into_groups(32), ChunkedResult, Result).

extend_to_80_words(Chunk, Result) :-
    extend_to_80_words(16, Chunk, Result).
extend_to_80_words(80, Chunk, Chunk).
extend_to_80_words(N, Chunk, Result) :-
    N < 80,

    IndexA is N - 3,
    IndexB is N - 8,
    IndexC is N - 14,
    IndexD is N - 16,

    nth0(IndexA, Chunk, WordA),
    nth0(IndexB, Chunk, WordB),
    nth0(IndexC, Chunk, WordC),
    nth0(IndexD, Chunk, WordD),

    xor_words(WordA, WordB, XorA),
    xor_words(XorA, WordC, XorB),
    xor_words(XorB, WordD, XorC),

    left_rotate(1, XorC, NewWord),

    append(Chunk, [NewWord], ExtendedChunk),
    Next is N + 1,
    extend_to_80_words(Next, ExtendedChunk, Result).

if_clause(Index, B, C, D, F, K) :-
    Index >= 0,
    Index =< 19,
    F is (B /\ C) \/ ((\B) /\ D),
    K is 0x5a827999.

if_clause(Index, B, C, D, F, K) :-
    Index >= 20,
    Index =< 39,
    F is B xor C xor D,
    K is 0x6ed9eba1.

if_clause(Index, B, C, D, F, K) :-
    Index >= 40,
    Index =< 59,
    F is (B /\ C) \/ (B /\ D) \/ (C /\ D),
    K is 0x8f1bbcdc.

if_clause(Index, B, C, D, F, K) :-
    Index >= 60,
    Index =< 79,
    F is B xor C xor D,
    K is 0xca62c1d6.

main_loop(80, _, _, _, _, _, _).
main_loop(Index, Word, A, B, C, D, E) :-
    if_clause(Index, B, C, D, F, K),

    decimal_to_binary(A, BinaryA),
    left_rotate(5, BinaryA, RotatedBinaryA),
    binary_to_decimal(RotatedBinaryA, DecimalRBA),
    nth0(Index, Word, WordAtIndexBinary),
    binary_to_decimal(WordAtIndexBinary, WordAtIndexDecimal),
    
    Temp is DecimalRBA + F + E + K + WordAtIndexDecimal,
    NextE is D,
    NextD is C,
    % C = B rotated to the left by 30
    decimal_to_binary(B, BinaryB),
    left_rotate(30, BinaryB, BinaryC),
    binary_to_decimal(BinaryC, NextC),
    NextB is A,
    NextA is Temp,

    NextIndex is Index + 1,
    main_loop(NextIndex, Word, NextA, NextB, NextC, NextD, NextE).

loop_chunks(0, _, _, _, _, _, _).
loop_chunks(Index, Chunks, H0, H1, H2, H3, H4) :-
    Index > 0,
    ChunkIndex is Index - 1,
    nth0(ChunkIndex, Chunks, Chunk),
    extend_to_80_words(Chunk, ExtendedChunk),

    A is H0,
    B is H1,
    C is H2,
    D is H3,
    E is H4,

    main_loop(0, ExtendedChunk, A, B, C, D, E),

    NextH0 is H0 + A,
    NextH1 is H1 + B,
    NextH2 is H2 + C,
    NextH3 is H3 + D,
    NextH4 is H4 + E,

    append(Chunks, [ExtendedChunk], ExtendedChunks),
    loop_chunks(ChunkIndex, ExtendedChunks, NextH0, NextH1, NextH2, NextH3, NextH4).

sha1_checksum(InputFile, Hash) :- 
    pad_file(InputFile, PaddedFileBits),
    partition_file_bits(PaddedFileBits, PartitionedBits),

    % loops through all the chunks of 16x 32-bit words
    length(PartitionedBits, NumChunks),
    
    % initial constants for loop
    h0(H0),
    h1(H1),
    h2(H2),
    h3(H3),
    h4(H4),

    loop_chunks(NumChunks, PartitionedBits, H0, H1, H2, H3, H4),

    HH is (H0 << 128) \/ (H1 << 96) \/ (H2 << 64) \/ (H3 << 32) \/ H4,
    format(atom(Hash), '~16r', [HH]).
