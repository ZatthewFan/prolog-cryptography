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

loop_chunks(0, Chunks, Chunks).
loop_chunks(Index, Chunks, Result) :-
    Index > 0,
    ChunkIndex is Index - 1,
    nth0(ChunkIndex, Chunks, Chunk),
    extend_to_80_words(Chunk, ExtendedChunk),
    append(Chunks, [ExtendedChunk], ExtendedChunks),
    loop_chunks(ChunkIndex, ExtendedChunks, Result).

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


% % Chunk[Index] as Word
% nth0(Index, Chunk, Word).

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

% main_loop(80, A, B, C, D, E, Result).
% main_loop(Index, X) :-
%     (Index =< 19 ->
%         F is (B /\ C) \/ (\B /\ D),
%         K is 0x5A827999,


sha1_checksum(InputFile, Checksum) :- 
    pad_file(InputFile, PaddedFileBits),
    partition_file_bits(PaddedFileBits, PartitionedBits),

    % loops through all the chunks of 16x 32-bit words
    length(PartitionedBits, NumChunks),
    loop_chunks(NumChunks, PartitionedBits, Extended80),

    A is h0(X),
    B is h1(X),
    C is h2(X),
    D is h3(X),
    E is h4(X),

    main_loop(0, A, B, C, D, E, Checksum).


