# PACC

PACC (PAscal C Compiler) is an work-in-progress C11-specification-aiming C compiler, which is implemented in Delphi-7-compatible Object Pascal.

The intermediate representation code architecture design is strongly inspired by [QBE](http://c9x.me/compile/) 

# Requirements

- Delphi or FreePascal as compiler for to compile PACC itself
- PasDblStrUtils.pas from https://github.com/BeRo1985/pasdblstrutils for accurate parsing of floating point number literals with 64-bit double-precision accuracy
- PasMP.pas from https://github.com/BeRo1985/pasmp for parallel processing stuff
- PUCU.pas from https://github.com/BeRo1985/pucu for unicode handling stuff
- SASMCore.pas SASMData.pas SASMDataContent.inc and SASM.inc from https://github.com/BeRo1985/sasm for assembling of x86 code

# License

zlib, otherwise see beginning comment of https://github.com/BeRo1985/pacc/blob/master/src/PACC.inc

# General guidelines for code contributors

See beginning comment of https://github.com/BeRo1985/pacc/blob/master/src/PACC.inc
