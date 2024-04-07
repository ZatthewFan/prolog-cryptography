%XOR encryption algorithm
xor_toggle(InputTextCodes, KeyCodes, OutputTextCodes) :-
    xor_toggle_helper(InputTextCodes, KeyCodes, OutputTextCodes, 0).

xor_toggle_helper([], _, [], _).
xor_toggle_helper([H|T], KeyCodes, [XorH|XorT], Index) :-
    length(KeyCodes, KeyLen),
    KeyIndex is Index mod KeyLen,
    nth0(KeyIndex, KeyCodes, KeyChar),
    XorH is H xor KeyChar,
    NextIndex is Index + 1,
    xor_toggle_helper(T, KeyCodes, XorT, NextIndex).

string_to_charcodes(String, CharCodes) :-
    string_codes(String, CharCodes).

charcodes_to_string(CharCodes, String) :-
    string_codes(String, CharCodes).

%Example usage:
%   string_to_charcodes("test", PT), string_to_charcodes("SecretKey", K), xor_toggle(PT, K, CT), charcodes_to_string(CT, CipherText).
%   string_to_charcodes(CIPHER_TEXT, CT), string_to_charcodes("SecretKey", K), xor_toggle(CT, K, PTDecrypted), charcodes_to_string(PTDecrypted, OriginalText).
%   replace "CIPHER_TEXT" with the output of the first line

%TODO: CHANGE
%CLI interface
start :-
    write('Enter E to encrypt, D to decrypt: '), read(Operation),
    (   Operation = 'E' -> encrypt_flow;
        Operation = 'D' -> decrypt_flow;
        write('Invalid option.'), nl, start).

% Flow for encryption.
encrypt_flow :-
    write('Enter the plaintext file path: '), read(PlaintextPath),
    write('Enter the encryption key: '), read(Key),
    write('Enter the output file path for encrypted text: '), read(OutputPath),
    encrypt_file(PlaintextPath, Key, OutputPath),
    write('File encrypted successfully.').

% Flow for decryption.
decrypt_flow :-
    write('Enter the encrypted file path: '), read(EncryptedPath),
    write('Enter the decryption key: '), read(Key),
    write('Enter the output file path for decrypted text: '), read(OutputPath),
    decrypt_file(EncryptedPath, Key, OutputPath),
    write('File decrypted successfully.').

% Encrypt a file with xor_toggle.
encrypt_file(InputPath, Key, OutputPath) :-
    read_file_to_string(InputPath, Plaintext, []),
    string_to_charcodes(Plaintext, PT),
    string_to_charcodes(Key, K),
    xor_toggle(PT, K, CT),
    charcodes_to_string(CT, CipherText),
    write_string_to_file(OutputPath, CipherText).

% Decrypt a file with xor_toggle.
decrypt_file(InputPath, Key, OutputPath) :-
    read_file_to_string(InputPath, CipherText, []),
    string_to_charcodes(CipherText, CT),
    string_to_charcodes(Key, K),
    xor_toggle(CT, K, PTDecrypted),
    charcodes_to_string(PTDecrypted, OriginalText),
    write_string_to_file(OutputPath, OriginalText).

% Helper to write a string to a file.
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


% Splits a list into sublists of a given length, padding the last sublist if necessary.
split_into_blocks(List, BlockSize, Blocks) :-
    split_into_blocks(List, BlockSize, Blocks, []).

split_into_blocks([], _, Blocks, CurrentBlock) :-
    CurrentBlock \= [],
    !,
    pad_block(CurrentBlock, PaddedBlock),
    reverse([PaddedBlock], Blocks).
split_into_blocks([], _, [], []) :- !.
split_into_blocks(List, BlockSize, [Block|Blocks], CurrentBlock) :-
    length(CurrentBlock, Len),
    Len =:= BlockSize,
    !,
    reverse(CurrentBlock, Block),
    split_into_blocks(List, BlockSize, Blocks, []).
split_into_blocks([H|T], BlockSize, Blocks, CurrentBlock) :-
    split_into_blocks(T, BlockSize, Blocks, [H|CurrentBlock]).

% Pads a block with zeroes (code for character '0' is 48) to reach the desired block size.
pad_block(Block, PaddedBlock) :-
    length(Block, Len),
    BlockSize = 8,  % Assuming block size is 8 for this example, change as needed
    PadLength is BlockSize - Len,
    findall(48, between(1, PadLength, _), Padding),
    append(Block, Padding, PaddedBlock).

%Example usage:
%   string_to_charcodes("Hello, World!", CharCodes), split_into_blocks(CharCodes, 8, Blocks), maplist(charcodes_to_string, Blocks, BlockStrings).
%Should give:
%   BlockStrings = ["Hello, W", "!dlro000"].

% Generates an Initialization Vector (IV) of a given length (BlockSize).
generate_iv(BlockSize, IV) :-
    findall(Byte, (between(1, BlockSize, _), random_between(0, 255, Byte)), IV).    
    %random_between is not practical since it's not secure, but just using it for educational purposes.

% Helper to convert the IV list to a string (for storage or display).
iv_to_string(IV, IVString) :-
    string_codes(IVString, IV).

%Example usage:
%   generate_iv(8, IV), iv_to_string(IV, IVString).

%CBC Encryption Algorithm
% CBC Encrypt a list of plaintext blocks.
cbc_encrypt(_, [], _, []).
cbc_encrypt(KeyCodes, [PlainTextBlock|PlainTextBlocks], PrevCipherBlock, [CipherTextBlock|CipherTextBlocks]) :-
    xor_toggle(PrevCipherBlock, PlainTextBlock, XoredBlock),
    xor_toggle(XoredBlock, KeyCodes, CipherTextBlock),
    cbc_encrypt(KeyCodes, PlainTextBlocks, CipherTextBlock, CipherTextBlocks).

% Modified encrypt_file to include CBC mode and IV generation.
encrypt_file_cbc(InputPath, Key, OutputPath) :-
    read_file_to_string(InputPath, Plaintext, []),
    string_to_charcodes(Plaintext, PT),
    string_to_charcodes(Key, K),
    BlockSize = 8,  % Your block size here
    generate_iv(BlockSize, IV),
    split_into_blocks(PT, BlockSize, Blocks),
    cbc_encrypt(K, Blocks, IV, EncryptedBlocks),
    append([IV], EncryptedBlocks, CipherTextWithIV),
    flatten(CipherTextWithIV, FlatCipherText),
    charcodes_to_string(FlatCipherText, CipherText),
    write_string_to_file(OutputPath, CipherText).

% CBC Decrypt a list of ciphertext blocks.
cbc_decrypt(_, [], _, []).
cbc_decrypt(KeyCodes, [CipherTextBlock|CipherTextBlocks], PrevCipherBlock, [PlainTextBlock|PlainTextBlocks]) :-
    xor_toggle(CipherTextBlock, KeyCodes, XoredBlock),
    xor_toggle(XoredBlock, PrevCipherBlock, PlainTextBlock),
    cbc_decrypt(KeyCodes, CipherTextBlocks, CipherTextBlock, PlainTextBlocks).

% Extracts the IV from the first block and then decrypts the rest in CBC mode.
decrypt_file_cbc(InputPath, Key, OutputPath) :-
    read_file_to_string(InputPath, CipherText, []),
    string_to_charcodes(CipherText, CT),
    BlockSize = 8,  % Your block size
    length(IV, BlockSize),  % Assume the IV is the first block
    append(IV, CipherTextBlocksFlat, CT),
    split_into_blocks(CipherTextBlocksFlat, BlockSize, CipherTextBlocks),
    string_to_charcodes(Key, K),
    cbc_decrypt(K, CipherTextBlocks, IV, DecryptedBlocks),
    flatten(DecryptedBlocks, FlatPlainText),
    charcodes_to_string(FlatPlainText, OriginalText),
    write_string_to_file(OutputPath, OriginalText).

%Example usage:
%   ?- encrypt_file_cbc('plaintext.txt', 'mysecretkey', 'cipher.txt').
%   ?- decrypt_file_cbc('cipher.txt', 'mysecretkey', 'decrypted.txt').
