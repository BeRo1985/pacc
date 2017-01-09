unit PACCGlobals;
{$i PACC.inc}

interface

uses {$if defined(Win32) or defined(Win64)}Windows,{$ifend}SysUtils,Classes,Math,PUCU,PasMP,PACCTypes,PACCRawByteStringHashMap;

const PACCVersionString='2017.01.10.00.21.0000';
      PACCCopyrightString='Copyright (C) 2009-2017, Benjamin ''BeRo'' Rosseaux';

type PPACCEncoding=^TPACCEncoding;
     TPACCEncoding=(ENCODING_NONE,
                    ENCODING_UTF8,
                    ENCODING_WCHAR,
                    ENCODING_CHAR16,
                    ENCODING_CHAR32);

     PPACCOptions=^TPACCOptions;
     TPACCOptions=record
      HandleTrigraphs:boolean;
      WarnTrigraphs:boolean;
      WarnTrigraphsMore:boolean;
      EnableWarnings:boolean;
      WarningsAreErrors:boolean;
      HandleUTF8:boolean;
     end;

     PPACCTokenSymbol=^TPACCTokenSymbol;
     TPACCTokenSymbol=record
      Name:TPUCUUTF8String;
      Index:integer;
     end;

     TPACCTokenSymbols=array of TPACCTokenSymbol;

     PPACCTypeFlag=^TPACCTypeFlag;
     TPACCTypeFlag=(
      tfUnsigned,
      tfStatic,

      // Array
      tfArray,

      // Struct
      tfStruct,

      // Function
      tfVarArgs,
      tfOldStyle,

      tfConstant,
      tfVolatile,
      tfInline,
      tfNoInline,
      tfNoReturn,
      tfRestrict,
      tfPacked
      
     );

     PPACCTypeFlags=^TPACCTypeFlags;
     TPACCTypeFlags=set of TPACCTypeFlag;

     PPACCTypeKind=^TPACCTypeKind;
     TPACCTypeKind=(
      tkVOID,
      tkBOOL,
      tkCHAR,
      tkSHORT,
      tkINT,
      tkLONG,
      tkLLONG,
      tkFLOAT,
      tkDOUBLE,
      tkLDOUBLE,
      tkARRAY,
      tkENUM,
      tkPOINTER,
      tkSTRUCT,
      tkFUNCTION,
      // used only in parser
      tkSTUB
     );

     PPACCType=^TPACCType;

     PPPACCTypes=^TPPACCTypes;
     TPPACCTypes=array of PPACCType;

     PPACCStructField=^TPACCStructField;
     TPACCStructField=record
      Name:TPUCUUTF8String;
      Type_:PPACCType;
     end;

     TPACCStructFields=array of TPACCStructField;

     TPACCType=record
     
      Previous:PPACCType;
      Next:PPACCType;

      Kind:TPACCTypeKind;
      Size:TPACCInt32;
      Alignment:TPACCInt32;

      Flags:TPACCTypeFlags;

      // Pointer/Array
      ChildType:PPACCType;

      // Array
      ArrayLength:TPACCInt64;

      // Struct
      Fields:TPACCStructFields;
      Offset:TPACCInt32;

      // Enum
      MinValue:TPACCInt;
      MaxValue:TPACCInt;

      // Bitfield
      BitOffset:TPACCInt32;
      BitSize:TPACCInt32;

      // Function
      ReturnType:PPACCType;
      Parameters:TPPACCTypes;
      CallingConvention:TPACCInt32;

     end;

     PPACCSourceLocation=^TPACCSourceLocation;
     TPACCSourceLocation=record
      Source:TPACCInt32;
      Line:TPACCInt32;
      Column:TPACCInt32;
     end;

const PACCDefaultOptions:TPACCOptions=
       (HandleTrigraphs:true;
        WarnTrigraphs:false;
        WarnTrigraphsMore:false;
        EnableWarnings:true;
        WarningsAreErrors:false;
        HandleUTF8:false;
       );

procedure DebuggerWaitEnterKey;

function SARcint(Value,Shift:TPACCInt):TPACCInt;
function SARcint8(Value,Shift:TPACCInt8):TPACCInt8;
function SARcint16(Value,Shift:TPACCInt16):TPACCInt16;
function SARcint32(Value,Shift:TPACCInt32):TPACCInt32;
function SARcint64(Value,Shift:TPACCInt64):TPACCInt64;

implementation

{$warnings off}

{$if defined(Win32) or defined(Win64)}
function IsDebuggerPresent:boolean; stdcall; external 'kernel32.dll' name 'IsDebuggerPresent';
{$endif}

procedure DebuggerWaitEnterKey;
begin
{$if defined(Win32) or defined(Win64)}
 if {$ifdef fpc}IsDebuggerPresent{$else}DebugHook<>0{$endif} then begin
  readln;
 end;
{$ifend}
end;
{$warnings on}

function SARcint(Value,Shift:TPACCInt):TPACCInt;
begin
 Shift:=Shift and ((SizeOf(TPACCInt) shl 3)-1);
 if Value<0 then begin
  result:=-((-Value) shr Shift);
 end else begin
  result:=Value shr Shift;
 end;
end;

function SARcint8(Value,Shift:TPACCInt8):TPACCInt8;
begin
 Shift:=Shift and ((SizeOf(TPACCInt8) shl 3)-1);
 if Value<0 then begin
  result:=-((-Value) shr Shift);
 end else begin
  result:=Value shr Shift;
 end;
end;

function SARcint16(Value,Shift:TPACCInt16):TPACCInt16;
begin
 Shift:=Shift and ((SizeOf(TPACCInt16) shl 3)-1);
 if Value<0 then begin
  result:=-((-Value) shr Shift);
 end else begin
  result:=Value shr Shift;
 end;
end;

function SARcint32(Value,Shift:TPACCInt32):TPACCInt32;
begin
 Shift:=Shift and ((SizeOf(TPACCInt32) shl 3)-1);
 if Value<0 then begin
  result:=-((-Value) shr Shift);
 end else begin
  result:=Value shr Shift;
 end;
end;

function SARcint64(Value,Shift:TPACCInt64):TPACCInt64;
begin
 Shift:=Shift and ((SizeOf(TPACCInt64) shl 3)-1);
 if Value<0 then begin
  result:=-((-Value) shr Shift);
 end else begin
  result:=Value shr Shift;
 end;
end;

end.


