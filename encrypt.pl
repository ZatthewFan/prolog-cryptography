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
%   start.
%   Enter E to encrypt, D to decrypt: E.
%   Enter the plaintext file path: |: 'secret.txt'.
%   Enter the encryption key: |: key.
%   Enter the output file path for encrypted text: |: 'cipher.txt'.
%   start.
%   Enter E to encrypt, D to decrypt: D.
%   Enter the plaintext file path: |: 'cipher.txt'.
%   Enter the encryption key: |: wrongkey.
%   Enter the output file path for encrypted text: |: 'decrypt.txt'.
