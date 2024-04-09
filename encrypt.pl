%XOR encryption algorithm
xor_toggle(InputTextCodes, KeyCodes, OutputTextCodes) :-
    xor_toggle_recurse(InputTextCodes, KeyCodes, OutputTextCodes, 0).

xor_toggle_recurse([], _, [], _).
xor_toggle_recurse([H|T], KeyCodes, [XorH|XorT], Index) :-
    length(KeyCodes, KeyLen),
    KeyIndex is Index mod KeyLen,
    nth0(KeyIndex, KeyCodes, KeyChar),
    XorH is H xor KeyChar,
    NextIndex is Index + 1,
    xor_toggle_recurse(T, KeyCodes, XorT, NextIndex).

%Example usage:
%   string_codes("test", PT), string_codes("SecretKey", K), xor_toggle(PT, K, CT), string_codes(CipherText, CT).
%   string_codes(CIPHER_TEXT, CT), string_codes("SecretKey", K), xor_toggle(CT, K, PTDecrypted), string_codes(OriginalText, PTDecrypted).
%   replace "CIPHER_TEXT" with the output of the first line

%CLI interface
% start :-
%     write("\"E.\" to encrypt, \"D.\" to decrypt: "), read(Operation), (
%         Operation = 'E' -> encrypt_flow;
%         Operation = 'D' -> decrypt_flow;
%         write('Not an option.'), nl, start).
start :-
    write('E to encrypt, D to decrypt: '), flush_output(current_output),
    read_line_to_string(user_input, Operation),
    (   Operation = "E" -> encrypt_flow;
        Operation = "D" -> decrypt_flow;
        write('Not an option.'), nl, start).

%Flow for encryption/decryption
encrypt_flow :-
    write("plaintext path: "), read(PlaintextPath),
    write("encryption key: "), read(Key),
    write("encrypted file path: "), read(OutputPath),
    encrypt_file(PlaintextPath, Key, OutputPath),
    write("Encryption successful!").

decrypt_flow :-
    write("encrypted file path: "), read(EncryptedPath),
    write("decryption key: "), read(Key),
    write("decrypted file path: "), read(OutputPath),
    decrypt_file(EncryptedPath, Key, OutputPath),
    write("Decryption successful!").

%Encrypt/decrypt file with xor_toggle
encrypt_file(InputPath, Key, OutputPath) :-
    read_file_to_string(InputPath, Plaintext, []),
    string_codes(Plaintext, PT),
    string_codes(Key, K),
    xor_toggle(PT, K, CT),
    string_codes(CipherText, CT),
    write_string_to_file(OutputPath, CipherText).

decrypt_file(InputPath, Key, OutputPath) :-
    read_file_to_string(InputPath, CipherText, []),
    string_codes(CipherText, CT),
    string_codes(Key, K),
    xor_toggle(CT, K, PTDecrypted),
    string_codes(OriginalText, PTDecrypted),
    write_string_to_file(OutputPath, OriginalText).

%Write a string to a file
write_string_to_file(FilePath, String) :-
    open(FilePath, write, Stream),
    write(Stream, String),
    close(Stream).

%Example usage:
%   ?- start.
%   Enter E to encrypt, D to decrypt: E.
%   Enter the plaintext file path: |: 'secret.txt'.
%   Enter the encryption key: |: key.
%   Enter the output file path for encrypted text: |: 'cipher.txt'.
%   ?- start.
%   Enter E to encrypt, D to decrypt: D.
%   Enter the plaintext file path: |: 'cipher.txt'.
%   Enter the encryption key: |: wrongkey.
%   Enter the output file path for encrypted text: |: 'decrypt.txt'.


%Split a list into sublists, each of equal length
init_split_into_blocks(List, BlockSize, Blocks) :-
    split_and_pad(List, BlockSize, Blocks, []).

%Reached end of input list and CurrentBlock is not empty, so need padding
pad_final_block([], BlockSize, [PaddedBlock], CurrentBlock) :-
    CurrentBlock \= [],
    !,      %cut to prevent backtracking
    reverse(CurrentBlock, ReversedCurrentBlock),        %reverse CurrentBlock because elements were added inversely
    pad_block(ReversedCurrentBlock, BlockSize, PaddedBlock).

finish_split([], _, [], []) :- !.      %base case

process_full_block(List, BlockSize, [Block|Blocks], CurrentBlock) :-
    length(CurrentBlock, Len),
    Len =:= BlockSize,  %check if current block has reached specified block size
    !,
    reverse(CurrentBlock, Block),
    split_and_pad(List, BlockSize, Blocks, []).

split_and_pad([H|T], BlockSize, Blocks, CurrentBlock) :-
    split_and_pad(T, BlockSize, Blocks, [H|CurrentBlock]). %Move current head of the list to CurrentBlock, then recurse

% Pad a given block with zeroes to reach the desired block size.
pad_block(Block, BlockSize, PaddedBlock) :-
    length(Block, Len),
    PadLength is BlockSize - Len,
    findall(48, between(1, PadLength, _), Padding),
    append(Block, Padding, PaddedBlock).

%Example usage:
%   string_codes("Hello World", CharCodes), split_into_blocks(CharCodes, 8, Blocks), maplist(string_codes, BlockStrings, Blocks).
%   string_codes("Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal.", CharCodes), split_into_blocks(CharCodes, 8, Blocks), maplist(string_codes, BlockStrings, Blocks).
%Should give:
%   BlockStrings = ["Hello Wo", "rld00000"].

%Generates an Initialization Vector of BlockSize (8 in our case)
generate_iv(BlockSize, IV) :-
    findall(Byte, (between(1, BlockSize, _), random_between(0, 255, Byte)), IV).    
    %random_between is not practical since it's not secure, but just using it for educational purposes.
%Example usage:
%   generate_iv(8, IV), string_codes(IVString, IV).

%CBC Encryption Algorithm
cbc_encrypt(_, [], _, []).
cbc_encrypt(KeyCodes, [PlainTextBlock|PlainTextBlocks], PrevCipherBlock, [CipherTextBlock|CipherTextBlocks]) :-
    xor_toggle(PrevCipherBlock, PlainTextBlock, XoredBlock),
    xor_toggle(XoredBlock, KeyCodes, CipherTextBlock),
    cbc_encrypt(KeyCodes, PlainTextBlocks, CipherTextBlock, CipherTextBlocks).

%Encrypt using CBC mode and IV generation
encrypt_file_cbc(InputPath, Key, OutputPath) :-
    read_file_to_string(InputPath, Plaintext, []),
    string_codes(Plaintext, PT),
    string_codes(Key, K),
    BlockSize = 8,
    generate_iv(BlockSize, IV),
    split_into_blocks(PT, BlockSize, Blocks),
    cbc_encrypt(K, Blocks, IV, EncryptedBlocks),
    append([IV], EncryptedBlocks, CipherTextWithIV),
    flatten(CipherTextWithIV, FlatCipherText),
    string_codes(CipherText, FlatCipherText),
    write_string_to_file(OutputPath, CipherText).

%Decrypt a list of ciphertext blocks
cbc_decrypt(_, [], _, []).
cbc_decrypt(KeyCodes, [CipherTextBlock|CipherTextBlocks], PrevCipherBlock, [PlainTextBlock|PlainTextBlocks]) :-
    xor_toggle(CipherTextBlock, KeyCodes, XoredBlock),
    xor_toggle(XoredBlock, PrevCipherBlock, PlainTextBlock),
    cbc_decrypt(KeyCodes, CipherTextBlocks, CipherTextBlock, PlainTextBlocks).

%Extract IV from first block, then decrypt the rest in CBC mode
decrypt_file_cbc(InputPath, Key, OutputPath) :-
    read_file_to_string(InputPath, CipherText, []),
    string_codes(CipherText, CT),
    BlockSize = 8,
    length(IV, BlockSize),  %IV is the first block
    append(IV, CipherTextBlocksFlat, CT),
    split_into_blocks(CipherTextBlocksFlat, BlockSize, CipherTextBlocks),
    string_codes(Key, K),
    cbc_decrypt(K, CipherTextBlocks, IV, DecryptedBlocks),
    flatten(DecryptedBlocks, FlatPlainText),
    string_codes(OriginalText, FlatPlainText),
    write_string_to_file(OutputPath, OriginalText).

%Example usage:
%   encrypt_file_cbc('plaintext.txt', 'mysecretkey', 'cipher.txt').
%   OR
%   encrypt_file_cbc('plaintext.txt', 'mysecretkey', 'cipher.txt').
%
%   decrypt_file_cbc('cipher.txt', 'mysecretkey', 'decrypted.txt').
