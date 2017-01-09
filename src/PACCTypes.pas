unit PACCTypes;
{$i PACC.inc}

interface

uses PasMP;

type PPACCInt8=^TPACCInt8;
     TPACCInt8=shortint;

     PPACCUInt8=^TPACCUInt8;
     TPACCUInt8=byte;

     PPACCInt16=^TPACCInt16;
     TPACCInt16=smallint;

     PPACCUInt16=^TPACCUInt16;
     TPACCUInt16=word;

     PPACCInt32=^TPACCInt32;
     TPACCInt32=longint;

     PPACCUInt32=^TPACCUInt32;
     TPACCUInt32=longword;

     PPACCInt64=^TPACCInt64;
     TPACCInt64=int64;

     PPACCUInt64=^TPACCUInt64;

     PPACCInt=^TPACCInt;
     TPACCInt=longint;

     PPACCUInt=^TPACCUInt;
     TPACCUInt=longword;

     PPACCPtrUInt=^TPACCPtrUInt;
     PPACCPtrInt=^TPACCPtrInt;

{$ifdef fpc}
 {$undef OldDelphi}
     TPACCUInt64=uint64;
     TPACCPtrUInt=PtrUInt;
     TPACCPtrInt=PtrInt;
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
     TPACCUInt64=uint64;
     TPACCPtrUInt=NativeUInt;
     TPACCPtrInt=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
  {$if CompilerVersion>=15.0}
     TPACCUInt64=uint64;
  {$else}
     TPACCUInt64=TPACCInt64;
  {$ifend}
  {$ifdef CPU64}
     TPACCPtrUInt=qword;
     TPACCPtrInt=TPACCInt64;
  {$else}
     TPACCPtrUInt=TPACCUInt32;
     TPACCPtrInt=TPACCInt32;
  {$endif}
{$endif}

     PPACCNativeUInt=^TPACCNativeUInt;
     TPACCNativeUInt=TPACCPtrUInt;

     PPACCNativeInt=^TPACCNativeInt;
     TPACCNativeInt=TPACCPtrInt;

     PPACCBoolean=^TPACCBoolean;
     TPACCBoolean=boolean;

     PPACCBool8=^TPACCBool8;
     TPACCBool8=bytebool;

     PPACCBool16=^TPACCBool16;
     TPACCBool16=wordbool;

     PPACCBool32=^TPACCBool32;
     TPACCBool32=longbool;

     PPACCFloat=^TPACCFloat;
     TPACCFloat=single;

     PPACCDouble=^TPACCDouble;
     TPACCDouble=double;

     PPACCLongDouble=^TPACCLongDouble;
     TPACCLongDouble={$ifdef HAS_TYPE_EXTENDED}extended{$else}double{$endif};

     PPACCRawByteString=^TPACCRawByteString;
     TPACCRawByteString={$if declared(RawByteString)}RawByteString{$else}AnsiString{$ifend};

implementation

end.
