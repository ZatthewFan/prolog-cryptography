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
