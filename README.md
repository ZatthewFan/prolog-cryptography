# prolog-cryptography
Our implementation of some cryptographic algorithms.

To use our programs, SWI Prolog is required. Installation can be found [here](https://www.swi-prolog.org/Download.html), or downloaded through your package manager.
To start either of our programs (`encrypt.pl` or `hash.pl`), simply run the following on the command line.
```
swipl <prolog-file-to-run.pl>
```
or
```
swipl
?- [name-of-prolog-file-without-extension].
```
Individual instructions for each program can be found in its respective file.

## encrypt.pl
This file contains two different encryption algorithms: [XOR](https://en.wikipedia.org/wiki/XOR_cipher), and [CBC](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#:~:text=Cipher%20block%20chaining%20(CBC),-CBC&text=In%20CBC%20mode%2C%20each%20block,used%20in%20the%20first%20block.).

## hash.pl
This file contains a prolog implementation of [SHA-1](https://en.wikipedia.org/wiki/SHA-1).
