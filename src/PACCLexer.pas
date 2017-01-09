unit PACCLexer;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCRawByteStringHashMap,PACCPreprocessor;

type PPACCLexerTokenType=^TPACCLexerTokenType;
     TPACCLexerTokenType=(TOK_NONE,

                          TOK_EOF,

                          TOK_PRAGMA,

                          TOK_PPNUM,

                          TOK_IDENT,

                          TOK_CINT,
                          TOK_CUINT,
                          TOK_CCHAR,

                          TOK_CFLOAT,
                          TOK_CDOUBLE,
                          TOK_CLDOUBLE,

                          TOK_CLLONG,
                          TOK_CULLONG,

                          TOK_MID,
                          TOK_UDIV,
                          TOK_UMOD,
                          TOK_PDIV,
                          TOK_UMULL,

                          TOK_ADDC1,
                          TOK_ADDC2,
                          TOK_SUBC1,
                          TOK_SUBC2,

                          TOK_ULT,
                          TOK_ULE,
                          TOK_UGT,
                          TOK_UGE,

                          TOK_TWOSHARPS,
                          TOK_SHARP,

                          TOK_LBRK,
                          TOK_RBRK,

                          TOK_LBRA,
                          TOK_RBRA,

                          TOK_LPAR,
                          TOK_RPAR,

                          TOK_LE,
                          TOK_LT,
                          TOK_GE,
                          TOK_GT,

                          TOK_A_SHL,
                          TOK_SHL,

                          TOK_A_SHR,
                          TOK_SHR,

                          TOK_LAND,
                          TOK_A_AND,
                          TOK_AND,

                          TOK_LOR,
                          TOK_A_OR,
                          TOK_OR,

                          TOK_INC,
                          TOK_A_ADD,
                          TOK_ADD,

                          TOK_DEC,
                          TOK_A_SUB,
                          TOK_SUB,

                          TOK_ARROW,

                          TOK_NE,

                          TOK_LNOT,

                          TOK_EQ,

                          TOK_ASSIGN,

                          TOK_A_MUL,
                          TOK_MUL,

                          TOK_A_MOD,
                          TOK_MOD,

                          TOK_A_XOR,
                          TOK_XOR,

                          TOK_A_DIV,
                          TOK_DIV,

                          TOK_COLON,
                          TOK_SEMICOLON,
                          TOK_COMMA,

                          TOK_QUEST,

                          TOK_NOT,

                          TOK_DOTS,
                          TOK_DOT,
                          TOK_ELLIPSIS,

                          TOK_CSTR,

                          // Keywords
                          TOK_ALIGNAS,
                          TOK_ALIGNOF,
                          TOK_ASM,
                          TOK_ATOMIC,
                          TOK_ATTRIBUTE,
                          TOK_AUTO,
                          TOK_BOOL,
                          TOK_BREAK,
                          TOK_CASE,
                          TOK_CHAR,
                          TOK_COMPLEX,
                          TOK_CONST,
                          TOK_CONTINUE,
                          TOK_DECLSPEC,
                          TOK_DEFAULT,
                          TOK_DO,
                          TOK_DOUBLE,
                          TOK_ELSE,
                          TOK_ENUM,
                          TOK_EXTERN,
                          TOK_FLOAT,
                          TOK_FOR,
                          TOK_GENERIC,
                          TOK_GOTO,
                          TOK_IF,
                          TOK_IMAGINARY,
                          TOK_INLINE,
                          TOK_INT,
                          TOK_LONG,
                          TOK_NORETURN,
                          TOK_REGISTER,
                          TOK_RESTRICT,
                          TOK_RETURN,
                          TOK_SHORT,
                          TOK_SIGNED1,
                          TOK_SIGNED2,
                          TOK_SIGNED3,
                          TOK_SIZEOF,
                          TOK_STATIC,
                          TOK_STATIC_ASSERT,
                          TOK_STRUCT,
                          TOK_SWITCH,
                          TOK_THREAD_LOCAL,
                          TOK_TYPEDEF,
                          TOK_TYPEOF,
                          TOK_UNION,
                          TOK_UNSIGNED,
                          TOK_VOID,
                          TOK_VOLATILE,
                          TOK_WHILE);

     PPACCLexerTokenConstant=^TPACCLexerTokenConstant;
     TPACCLexerTokenConstant=record
      StringValue:TPUCUUTF8String;
      Encoding:TPACCEncoding;
      case TPACCInt32 of
       0:(
        SignedIntegerValue:TPACCInt64;
       );
       1:(
        UnsignedIntegerValue:TPACCUInt64;
       );
       2:(
        FloatValue:single;
       );
       3:(
        DoubleValue:double;
       );
     end;

     PPACCLexerToken=^TPACCLexerToken;
     TPACCLexerToken=record
      TokenType:TPACCLexerTokenType;
      SourceLocation:TPACCSourceLocation;
      Index:TPACCInt32;
      PragmaString:TPUCUUTF8String;
      Constant:TPACCLexerTokenConstant;
     end;

     TPACCLexerTokens=array of TPACCLexerToken;

     TPACCLexer=class
      private

       LexerKeywordStringHashMap:TPACCRawByteStringHashMap;
       KeywordNames:array[TPACCLexerTokenType] of TPUCUUTF8String;

      public

       Instance:TObject;

       Preprocessor:TPACCPreprocessor;

       Tokens:TPACCLexerTokens;
       CountTokens:TPACCInt32;

       TokenIndex:TPACCInt32;

       TokenStrings:array[TPACCLexerTokenType] of TPUCUUTF8String;

       constructor Create(const AInstance:TObject);
       destructor Destroy; override;

       procedure Process;

     end;

const KeywordTokens=[TOK_ALIGNAS..TOK_WHILE];

implementation

uses PasDblStrUtils,PACCInstance;

constructor TPACCLexer.Create(const AInstance:TObject);
 procedure AddKeyword(const t:TPACCLexerTokenType;const s:TPUCUUTF8String);
 begin
  LexerKeywordStringHashMap.Add(s,pointer(TPACCPtrInt(TPACCInt32(t))));
  KeywordNames[t]:=s;
 end;
begin
 inherited Create;

 Instance:=AInstance;

 Preprocessor:=TPACCInstance(Instance).Preprocessor;

 LexerKeywordStringHashMap:=TPACCRawByteStringHashMap.Create;

 AddKeyword(TOK_ALIGNAS,'_Alignas');
 AddKeyword(TOK_ALIGNOF,'_Alignof');
 AddKeyword(TOK_ASM,'__asm__');
 AddKeyword(TOK_ATOMIC,'_Atomic');
 AddKeyword(TOK_ATTRIBUTE,'__attribute__');
 AddKeyword(TOK_AUTO,'auto');
 AddKeyword(TOK_BOOL,'_Bool');
 AddKeyword(TOK_BREAK,'break');
 AddKeyword(TOK_CASE,'case');
 AddKeyword(TOK_CHAR,'char');
 AddKeyword(TOK_COMPLEX,'_Complex');
 AddKeyword(TOK_CONST,'const');
 AddKeyword(TOK_CONTINUE,'continue');
 AddKeyword(TOK_DECLSPEC,'__declspec');
 AddKeyword(TOK_DEFAULT,'default');
 AddKeyword(TOK_DO,'do');
 AddKeyword(TOK_DOUBLE,'double');
 AddKeyword(TOK_ELSE,'else');
 AddKeyword(TOK_ENUM,'enum');
 AddKeyword(TOK_EXTERN,'extern');
 AddKeyword(TOK_FLOAT,'float');
 AddKeyword(TOK_FOR,'for');
 AddKeyword(TOK_GENERIC,'_Generic');
 AddKeyword(TOK_GOTO,'goto');
 AddKeyword(TOK_IF,'if');
 AddKeyword(TOK_IMAGINARY,'_Imaginary');
 AddKeyword(TOK_INLINE,'inline');
 AddKeyword(TOK_INT,'int');
 AddKeyword(TOK_LONG,'long');
 AddKeyword(TOK_NORETURN,'_Noreturn');
 AddKeyword(TOK_REGISTER,'register');
 AddKeyword(TOK_RESTRICT,'restrict');
 AddKeyword(TOK_RETURN,'return');
 AddKeyword(TOK_SHORT,'short');
 AddKeyword(TOK_SIGNED1,'signed');
 AddKeyword(TOK_SIGNED2,'__signed');
 AddKeyword(TOK_SIGNED3,'__signed__');
 AddKeyword(TOK_SIZEOF,'sizeof');
 AddKeyword(TOK_STATIC,'static');
 AddKeyword(TOK_STATIC_ASSERT,'_Static_assert');
 AddKeyword(TOK_STRUCT,'struct');
 AddKeyword(TOK_SWITCH,'switch');
 AddKeyword(TOK_THREAD_LOCAL,'_Thread_local');
 AddKeyword(TOK_TYPEDEF,'typedef');
 AddKeyword(TOK_TYPEOF,'typeof');
 AddKeyword(TOK_UNION,'union');
 AddKeyword(TOK_UNSIGNED,'unsigned');
 AddKeyword(TOK_VOID,'void');
 AddKeyword(TOK_VOLATILE,'volatile');
 AddKeyword(TOK_WHILE,'while');

 Tokens:=nil;
 CountTokens:=0;

 TokenIndex:=0;

end;

destructor TPACCLexer.Destroy;
begin
 SetLength(Tokens,0);
 LexerKeywordStringHashMap.Free;
 inherited Destroy;
end;

procedure TPACCLexer.Process;
var CurrentPosition,PosInfoIndex,PragmaInfoIndex:TPACCInt32;
    CurrentChar,CurrentNextChar{,TerminateChar,c}:TPUCUUTF32Char;
    WeAreAtEOF:boolean;
    TokenSourceLocation:TPACCSourceLocation;
 function RoundUpToPowerOfTwo(x:TPACCPtrUInt):TPACCPtrUInt;
 begin
  dec(x);
  x:=x or (x shr 1);
  x:=x or (x shr 2);
  x:=x or (x shr 4);
  x:=x or (x shr 8);
  x:=x or (x shr 16);
{$ifdef cpu64}
  x:=x or (x shr 32);
{$endif}
  result:=x+1;
 end;
 procedure AddError(const s:TPUCUUTF8String;const SourceLocation:PPACCSourceLocation=nil;const DoAbort:boolean=false);
 begin
  TPACCInstance(Instance).AddError(s,SourceLocation,DoAbort);
 end;
 procedure AddWarning(const s:TPUCUUTF8String;const SourceLocation:PPACCSourceLocation=nil);
 begin
  TPACCInstance(Instance).AddWarning(s,SourceLocation);
 end;
 function NextChar:TPUCUUTF32Char;
 var i:TPACCInt32;
 begin
  if CurrentPosition<=length(Preprocessor.OutputText) then begin
   WeAreAtEOF:=false;
   while (PosInfoIndex<Preprocessor.CountOutputInfos) and not ((Preprocessor.OutputInfos[PosInfoIndex].FirstCharPos<=CurrentPosition) and (Preprocessor.OutputInfos[PosInfoIndex].LastCharPos>=CurrentPosition)) do begin
    inc(PosInfoIndex);
   end;
   if PosInfoIndex<Preprocessor.CountOutputInfos then begin
    TPACCInstance(Instance).SourceLocation:=Preprocessor.OutputInfos[PosInfoIndex].SourceLocation;
    inc(TPACCInstance(Instance).SourceLocation.Column,CurrentPosition-Preprocessor.OutputInfos[PosInfoIndex].NewLineSincePos);
   end;
   while (PragmaInfoIndex<Preprocessor.CountPragmaInfos) and (Preprocessor.PragmaInfos[PragmaInfoIndex].CharPos>CurrentPosition) do begin
    inc(PragmaInfoIndex);
   end;
   if (PragmaInfoIndex<Preprocessor.CountPragmaInfos) and (Preprocessor.PragmaInfos[PragmaInfoIndex].CharPos=CurrentPosition) then begin
    i:=CountTokens;
    inc(CountTokens);
    if CountTokens>length(Tokens) then begin
     SetLength(Tokens,CountTokens*2);
    end;
    Tokens[i].TokenType:=TOK_PRAGMA;
    Tokens[i].SourceLocation:=TPACCInstance(Instance).SourceLocation;
    Tokens[i].PragmaString:=copy(Preprocessor.PragmaInfos[PragmaInfoIndex].Pragma,0,length(Preprocessor.PragmaInfos[PragmaInfoIndex].Pragma));
   end;
   CurrentChar:=PUCUUTF8CodeUnitGetCharAndIncFallback(Preprocessor.OutputText,CurrentPosition);
   if CurrentPosition<=length(Preprocessor.OutputText) then begin
    CurrentNextChar:=PUCUUTF8CodeUnitGetCharFallback(Preprocessor.OutputText,CurrentPosition);
   end else begin
    CurrentNextChar:=0;
   end;
  end else begin
   WeAreAtEOF:=true;
   CurrentChar:=0;
   CurrentNextChar:=0;
  end;
  result:=CurrentChar;
 end;
 procedure AddSimpleToken(t:TPACCLexerTokenType);
 var i:TPACCInt32;
     StringHashMapEntity:PPACCRawByteStringHashMapEntity;
     Token:PPACCLexerToken;
 begin
  i:=CountTokens;
  inc(CountTokens);
  if CountTokens>length(Tokens) then begin
   SetLength(Tokens,CountTokens*2);
  end;
  Token:=@Tokens[i];
  Token^.TokenType:=t;
  Token^.SourceLocation:=TokenSourceLocation;
  StringHashMapEntity:=TPACCInstance(Instance).TokenSymbolStringHashMap.Get(KeywordNames[t]);
  if assigned(StringHashMapEntity) then begin
   Token^.Index:=TPACCPtrInt(StringHashMapEntity^.Value);
  end else begin
   Token^.Index:=-1;
  end;
 end;
 procedure AddIdentToken(const s:TPUCUUTF8String);
 var i,Index:TPACCInt32;
     StringHashMapEntity:PPACCRawByteStringHashMapEntity;
     Token:PPACCLexerToken;
     TokenSymbol:PPACCTokenSymbol;
 begin
  StringHashMapEntity:=TPACCInstance(Instance).TokenSymbolStringHashMap.Get(s);
  if assigned(StringHashMapEntity) then begin
   Index:=TPACCPtrInt(StringHashMapEntity^.Value);
  end else begin
   i:=TPACCInstance(Instance).CountTokenSymbols;
   inc(TPACCInstance(Instance).CountTokenSymbols);
   if TPACCInstance(Instance).CountTokenSymbols>length(TPACCInstance(Instance).TokenSymbols) then begin
    SetLength(TPACCInstance(Instance).TokenSymbols,TPACCInstance(Instance).CountTokenSymbols*2);
   end;
   TokenSymbol:=@TPACCInstance(Instance).TokenSymbols[i];
   TokenSymbol^.Name:=s;
   TokenSymbol^.Index:=i+1;
   TPACCInstance(Instance).TokenSymbolStringHashMap.Add(s,pointer(TPACCPtrInt(i)));
   Index:=i;
  end;
  i:=CountTokens;
  inc(CountTokens);
  if CountTokens>length(Tokens) then begin
   SetLength(Tokens,CountTokens*2);
  end;
  Token:=@Tokens[i];
  Token^.TokenType:=TOK_IDENT;
  Token^.SourceLocation:=TokenSourceLocation;
  Token^.Index:=Index;
 end;
 function Hex2Byte(c:TPUCUUTF32Char):TPACCInt32;
 begin
  case c of
   ord('0')..ord('9'):begin
    result:=c-byte('0');
   end;
   ord('a')..ord('f'):begin
    result:=c-byte('a')+$a;
   end;
   ord('A')..ord('F'):begin
    result:=c-byte('F')+$a;
   end;
   else begin
    result:=0;
   end;
  end;
 end;
 procedure ParseString(t:TPACCLexerTokenType;Encoding:TPACCEncoding);
 var OutString:TPUCUUTF32String;
     OutStringLength,i,c,TerminateChar,BitsPerChar,BitShift:TPACCInt32;
     v:TPACCUInt64;
     Token:PPACCLexerToken;
  procedure AddChar(NewChar:ansichar);
  begin
   if OutStringLength>=length(OutString) then begin
    SetLength(OutString,(OutStringLength+1)*2);
   end;
   OutString[OutStringLength]:=byte(ansichar(NewChar));
   inc(OutStringLength);
  end;
  procedure AddChar32(NewChar:TPACCUInt32);
  begin
   if OutStringLength>=length(OutString) then begin
    SetLength(OutString,(OutStringLength+1)*2);
   end;
   OutString[OutStringLength]:=byte(ansichar(NewChar));
   inc(OutStringLength);
  end;
 begin
  OutString:=nil;
  OutStringLength:=0;
  try
   TerminateChar:=CurrentChar;
   NextChar;
   while (CurrentChar<>TerminateChar) and not (CurrentChar in [0,10]) do begin
    if CurrentChar=ord('\') then begin
     NextChar;
     case CurrentChar of
      ord('a'):begin
       AddChar(#7);
       NextChar;
      end;
      ord('b'):begin
       AddChar(#8);
       NextChar;
      end;
      ord('t'):begin
       AddChar(#9);
       NextChar;
      end;
      ord('n'):begin
       AddChar(#10);
       NextChar;
      end;
      ord('v'):begin
       AddChar(#11);
       NextChar;
      end;
      ord('f'):begin
       AddChar(#12);
       NextChar;
      end;
      ord('r'):begin
       AddChar(#13);
       NextChar;
      end;
      ord('\'):begin
       AddChar('\');
       NextChar;
      end;
      ord(''''):begin
       AddChar('''');
       NextChar;
      end;
      ord('"'):begin
       AddChar('"');
       NextChar;
      end;
      ord('?'):begin
       AddChar('?');
       NextChar;
      end;
      ord('U'):begin
       if ((CurrentPosition+9)<=length(Preprocessor.OutputText)) and
          (byte(ansichar(Preprocessor.OutputText[CurrentPosition+2])) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
          (byte(ansichar(Preprocessor.OutputText[CurrentPosition+3])) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
          (byte(ansichar(Preprocessor.OutputText[CurrentPosition+4])) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
          (byte(ansichar(Preprocessor.OutputText[CurrentPosition+5])) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
          (byte(ansichar(Preprocessor.OutputText[CurrentPosition+6])) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
          (byte(ansichar(Preprocessor.OutputText[CurrentPosition+7])) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
          (byte(ansichar(Preprocessor.OutputText[CurrentPosition+8])) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
          (byte(ansichar(Preprocessor.OutputText[CurrentPosition+9])) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) then begin
        AddChar32((Hex2Byte(byte(ansichar(Preprocessor.OutputText[CurrentPosition+2]))) shl 28) or
                  (Hex2Byte(byte(ansichar(Preprocessor.OutputText[CurrentPosition+3]))) shl 24) or
                  (Hex2Byte(byte(ansichar(Preprocessor.OutputText[CurrentPosition+4]))) shl 20) or
                  (Hex2Byte(byte(ansichar(Preprocessor.OutputText[CurrentPosition+5]))) shl 16) or
                  (Hex2Byte(byte(ansichar(Preprocessor.OutputText[CurrentPosition+6]))) shl 12) or
                  (Hex2Byte(byte(ansichar(Preprocessor.OutputText[CurrentPosition+7]))) shl 8) or
                  (Hex2Byte(byte(ansichar(Preprocessor.OutputText[CurrentPosition+8]))) shl 4) or
                  (Hex2Byte(byte(ansichar(Preprocessor.OutputText[CurrentPosition+9]))) shl 0));
        inc(CurrentPosition,8);
       end;
       NextChar;
      end;
      ord('u'):begin
       if ((CurrentPosition+5)<=length(Preprocessor.OutputText)) and
          (byte(ansichar(Preprocessor.OutputText[CurrentPosition+2])) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
          (byte(ansichar(Preprocessor.OutputText[CurrentPosition+3])) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
          (byte(ansichar(Preprocessor.OutputText[CurrentPosition+4])) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
          (byte(ansichar(Preprocessor.OutputText[CurrentPosition+5])) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) then begin
        AddChar32((Hex2Byte(byte(ansichar(Preprocessor.OutputText[CurrentPosition+2]))) shl 12) or
                  (Hex2Byte(byte(ansichar(Preprocessor.OutputText[CurrentPosition+3]))) shl 8) or
                  (Hex2Byte(byte(ansichar(Preprocessor.OutputText[CurrentPosition+4]))) shl 4) or
                  (Hex2Byte(byte(ansichar(Preprocessor.OutputText[CurrentPosition+5]))) shl 0));
        inc(CurrentPosition,4);
       end;
       NextChar;
      end;
      ord('x'),ord('X'):begin
       if ((CurrentPosition+3)<=length(Preprocessor.OutputText)) and
          (byte(ansichar(Preprocessor.OutputText[CurrentPosition+2])) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
          (byte(ansichar(Preprocessor.OutputText[CurrentPosition+3])) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) then begin
        AddChar32((Hex2Byte(byte(ansichar(Preprocessor.OutputText[CurrentPosition+2]))) shl 4) or
                  (Hex2Byte(byte(ansichar(Preprocessor.OutputText[CurrentPosition+3]))) shl 0));
        inc(CurrentPosition,2);
       end;
       NextChar;
      end;
      ord('0')..ord('7'):begin
       c:=0;
       while CurrentChar in [ord('0')..ord('7')] do begin
        c:=(c*8)+(CurrentChar-ord('0'));
        NextChar;
       end;
       AddChar32(c);
      end;
      else begin
       AddChar32(CurrentChar);
       NextChar;
      end;
     end;
    end else begin
     AddChar32(CurrentChar);
     NextChar;
    end;
   end;
   if CurrentChar=TerminateChar then begin
    NextChar;
   end;
   SetLength(OutString,OutStringLength);
   if TerminateChar=ord('''') then begin
    t:=TOK_CCHAR;
    if OutStringLength<1 then begin
     AddError('Empty character constant',nil,false);
    end else if OutStringLength>1 then begin
     AddWarning('Multi-charactor character constant',nil);
    end;
    case Encoding of
     ENCODING_UTF8:begin
      BitsPerChar:=32;
     end;
     ENCODING_WCHAR:begin
      BitsPerChar:=16;
     end;
     ENCODING_CHAR16:begin
      BitsPerChar:=16;
     end;
     ENCODING_CHAR32:begin
      BitsPerChar:=32;
     end;
     else begin
      BitsPerChar:=8;
     end;
    end;
    v:=0;
    BitShift:=0;
    for i:=0 to OutStringLength-1 do begin
     v:=v or (OutString[i] shl BitShift);
     inc(BitShift,BitsPerChar);
    end;
    i:=CountTokens;
    inc(CountTokens);
    if CountTokens>length(Tokens) then begin
     SetLength(Tokens,CountTokens*2);
    end;
    Token:=@Tokens[i];
    Token^.TokenType:=t;
    Token^.SourceLocation:=TokenSourceLocation;
    Token^.Constant.UnsignedIntegerValue:=v;
    Token^.Constant.Encoding:=Encoding;
   end else begin
    i:=CountTokens;
    inc(CountTokens);
    if CountTokens>length(Tokens) then begin
     SetLength(Tokens,CountTokens*2);
    end;
    Token:=@Tokens[i];
    Token^.TokenType:=t;
    Token^.SourceLocation:=TokenSourceLocation;
    Token^.Constant.StringValue:=PUCUUTF32ToUTF8(OutString);
    Token^.Constant.Encoding:=Encoding;
   end;
  finally
   SetLength(OutString,0);
  end;
 end;
 procedure ParseNumber(c:TPUCUUTF32Char);
 type tbn=array[0..1] of TPACCUInt32;
 var //Last:TPUCUUTF32Char;
     i,b,fb,fbi,sign,lc,uc:TPACCInt32;
     l,l1,lv,ev:TPACCInt64;
     bn:tbn;
     //s:TPUCUUTF8String;
     sv:TPasDblStrUtilsRawByteString;
     fv:double;
     t:TPACCLexerTokenType;
     Token:PPACCLexerToken;
     OK:TPasDblStrUtilsBoolean;
  procedure bnshl(var bn:tbn;shift,val:TPACCUInt32);
  begin
   bn[1]:=(bn[1] shl shift) or (bn[0] shr (32-shift));
   bn[0]:=(bn[0] shl shift) or val;
  end;
  procedure bnzero(var bn:tbn);
  begin
   bn[0]:=0;
   bn[1]:=0;
  end;
 begin
  if c=ord('.') then begin
   sv:='0.';
   while CurrentChar in [ord('0')..ord('9')] do begin
    sv:=sv+chr(CurrentChar);
    NextChar;
   end;
   if CurrentChar in [ord('e'),ord('E')] then begin
    sv:=sv+chr(CurrentChar);
    NextChar;
    if CurrentChar in [ord('+'),ord('-')] then begin
     sv:=sv+chr(CurrentChar);
     NextChar;
     while CurrentChar in [ord('0')..ord('9')] do begin
      sv:=sv+chr(CurrentChar);
      NextChar;
     end;
    end;
   end;
   OK:=false;
   fv:=ConvertStringToDouble(sv,rmNearest,@OK);
   case CurrentChar of
    ord('f'),ord('F'):begin
     NextChar;
     i:=CountTokens;
     inc(CountTokens);
     if CountTokens>length(Tokens) then begin
      SetLength(Tokens,CountTokens*2);
     end;
     Token:=@Tokens[i];
     Token^.TokenType:=TOK_CFLOAT;
     Token^.SourceLocation:=TokenSourceLocation;
     Token^.Constant.FloatValue:=fv;
    end;
    ord('l'),ord('L'):begin
     NextChar;
     i:=CountTokens;
     inc(CountTokens);
     if CountTokens>length(Tokens) then begin
      SetLength(Tokens,CountTokens*2);
     end;
     Token:=@Tokens[i];
     Token^.TokenType:=TOK_CLDOUBLE;
     Token^.SourceLocation:=TokenSourceLocation;
     Token^.Constant.DoubleValue:=fv;
    end;
    ord('d'),ord('D'):begin
     NextChar;
     i:=CountTokens;
     inc(CountTokens);
     if CountTokens>length(Tokens) then begin
      SetLength(Tokens,CountTokens*2);
     end;
     Token:=@Tokens[i];
     Token^.TokenType:=TOK_CDOUBLE;
     Token^.SourceLocation:=TokenSourceLocation;
     Token^.Constant.DoubleValue:=fv;
    end;
    else begin
     i:=CountTokens;
     inc(CountTokens);
     if CountTokens>length(Tokens) then begin
      SetLength(Tokens,CountTokens*2);
     end;
     Token:=@Tokens[i];
     Token^.TokenType:=TOK_CDOUBLE;
     Token^.SourceLocation:=TokenSourceLocation;
     Token^.Constant.DoubleValue:=fv;
    end;
   end;
   exit;
  end;
  b:=10;
  sv:='';
  if CurrentChar=ord('0') then begin
   sv:='0';
   NextChar;
   case CurrentChar of
    ord('x'),ord('X'):begin
     sv:='';
     NextChar;
     b:=16;
    end;
    ord('b'),ord('B'):begin
     sv:='';
     NextChar;
     b:=2;
    end;
   end;
  end;
  i:=0;
  while true do begin
   case CurrentChar of
    ord('a')..ord('f'):begin
     i:=CurrentChar-ord('a')+$a;
    end;
    ord('A')..ord('F'):begin
     i:=CurrentChar-ord('A')+$a;
    end;
    ord('0')..ord('9'):begin
     i:=CurrentChar-ord('0');
    end;
    else begin
     break;
    end;
   end;
   if i>=b then begin
    break;
   end;
   sv:=sv+chr(CurrentChar);
   NextChar;
  end;
  if (CurrentChar=ord('.')) or ((b=10) and (CurrentChar in [ord('e'),ord('E')])) or ((b in [2,16]) and (CurrentChar in [ord('p'),ord('P')])) then begin
   if b<>10 then begin
    case b of
     2:fbi:=1;
     16:fbi:=4;
     else fbi:=0;
    end;
    bnzero(bn);
    for i:=1 to length(sv) do begin
     case sv[i] of
      'a'..'f':begin
       lv:=ord(sv[i])-ord('a')+$a;
      end;
      'A'..'F':begin
       lv:=ord(sv[i])-ord('A')+$a;
      end;
      '0'..'9':begin
       lv:=ord(sv[i])-ord('0');
      end;
      else begin
       lv:=0;
      end;
     end;
     bnshl(bn,fbi,lv);
    end;
    fb:=0;
    if CurrentChar=ord('.') then begin
     NextChar;
     lv:=0;
     while true do begin
      case CurrentChar of
       ord('a')..ord('f'):begin
        lv:=CurrentChar-ord('a')+$a;
       end;
       ord('A')..ord('F'):begin
        lv:=CurrentChar-ord('A')+$a;
       end;
       ord('0')..ord('9'):begin
        lv:=CurrentChar-ord('0');
       end;
       else begin
        break;
       end;
      end;
      if lv>=b then begin
       break;
      end;
      bnshl(bn,fbi,lv);
      inc(fb,fbi);
      NextChar;
     end;
    end;
    if CurrentChar in [ord('p'),ord('P')] then begin
     NextChar;
    end else begin
     AddError('Exponent expected',nil,false);
    end;
    ev:=0;
    case CurrentChar of
     ord('+'):begin
      sign:=1;
      NextChar;
     end;
     ord('-'):begin
      sign:=-1;
      NextChar;
     end;
     else begin
      sign:=1;
     end;
    end;
    while CurrentChar in [ord('0')..ord('9')] do begin
     ev:=(ev*10)+CurrentChar-ord('0');
     NextChar;
    end;
    ev:=ev*sign;
    fv:=(bn[1]*4294967296.0)+bn[0];
    fv:=fv*power(2.0,ev-fb);
    case CurrentChar of
     ord('f'),ord('F'):begin
      NextChar;
      i:=CountTokens;
      inc(CountTokens);
      if CountTokens>length(Tokens) then begin
       SetLength(Tokens,CountTokens*2);
      end;
      Token:=@Tokens[i];
      Token^.TokenType:=TOK_CFLOAT;
      Token^.SourceLocation:=TokenSourceLocation;
      Token^.Constant.FloatValue:=fv;
     end;
     ord('l'),ord('L'):begin
      NextChar;
      i:=CountTokens;
      inc(CountTokens);
      if CountTokens>length(Tokens) then begin
       SetLength(Tokens,CountTokens*2);
      end;
      Token:=@Tokens[i];
      Token^.TokenType:=TOK_CLDOUBLE;
      Token^.SourceLocation:=TokenSourceLocation;
      Token^.Constant.DoubleValue:=fv;
     end;
     ord('d'),ord('D'):begin
      NextChar;
      i:=CountTokens;
      inc(CountTokens);
      if CountTokens>length(Tokens) then begin
       SetLength(Tokens,CountTokens*2);
      end;
      Token:=@Tokens[i];
      Token^.TokenType:=TOK_CDOUBLE;
      Token^.SourceLocation:=TokenSourceLocation;
      Token^.Constant.DoubleValue:=fv;
     end;
     else begin
      i:=CountTokens;
      inc(CountTokens);
      if CountTokens>length(Tokens) then begin
       SetLength(Tokens,CountTokens*2);
      end;
      Token:=@Tokens[i];
      Token^.TokenType:=TOK_CDOUBLE;
      Token^.SourceLocation:=TokenSourceLocation;
      Token^.Constant.DoubleValue:=fv;
     end;
    end;
   end else begin
    if CurrentChar=ord('.') then begin
     sv:=sv+chr(CurrentChar);
     NextChar;
    end;
    while CurrentChar in [ord('0')..ord('9')] do begin
     sv:=sv+chr(CurrentChar);
     NextChar;
    end;
    if CurrentChar in [ord('e'),ord('E')] then begin
     sv:=sv+chr(CurrentChar);
     NextChar;
     if CurrentChar in [ord('+'),ord('-')] then begin
      sv:=sv+chr(CurrentChar);
      NextChar;
      while CurrentChar in [ord('0')..ord('9')] do begin
       sv:=sv+chr(CurrentChar);
       NextChar;
      end;
     end;
    end;
    OK:=false;
    fv:=ConvertStringToDouble(sv,rmNearest,@OK);
    case CurrentChar of
     ord('f'),ord('F'):begin
      NextChar;
      i:=CountTokens;
      inc(CountTokens);
      if CountTokens>length(Tokens) then begin
       SetLength(Tokens,CountTokens*2);
      end;
      Token:=@Tokens[i];
      Token^.TokenType:=TOK_CFLOAT;
      Token^.SourceLocation:=TokenSourceLocation;
      Token^.Constant.FloatValue:=fv;
     end;
     ord('l'),ord('L'):begin
      NextChar;
      i:=CountTokens;
      inc(CountTokens);
      if CountTokens>length(Tokens) then begin
       SetLength(Tokens,CountTokens*2);
      end;
      Token:=@Tokens[i];
      Token^.TokenType:=TOK_CLDOUBLE;
      Token^.SourceLocation:=TokenSourceLocation;
      Token^.Constant.DoubleValue:=fv;
     end;
     ord('d'),ord('D'):begin
      NextChar;
      i:=CountTokens;
      inc(CountTokens);
      if CountTokens>length(Tokens) then begin
       SetLength(Tokens,CountTokens*2);
      end;
      Token:=@Tokens[i];
      Token^.TokenType:=TOK_CDOUBLE;
      Token^.SourceLocation:=TokenSourceLocation;
      Token^.Constant.DoubleValue:=fv;
     end;
     else begin
      i:=CountTokens;
      inc(CountTokens);
      if CountTokens>length(Tokens) then begin
       SetLength(Tokens,CountTokens*2);
      end;
      Token:=@Tokens[i];
      Token^.TokenType:=TOK_CDOUBLE;
      Token^.SourceLocation:=TokenSourceLocation;
      Token^.Constant.DoubleValue:=fv;
     end;
    end;
   end;
  end else begin
   l:=0;
   for i:=1 to length(sv) do begin
    case sv[i] of
     'a'..'f':begin
      lv:=ord(sv[i])-ord('a')+$a;
     end;
     'A'..'F':begin
      lv:=ord(sv[i])-ord('A')+$a;
     end;
     '0'..'9':begin
      lv:=ord(sv[i])-ord('0');
     end;
     else begin
      lv:=0;
     end;
    end;
    l1:=l;
    l:=(l*b)+lv;
    if l<l1 then begin
     AddError('Integer constant overflow',nil,false);
    end;
   end;
   if (l and $ffffffff00000000)<>0 then begin
    if (l shr 63)<>0 then begin
     t:=TOK_CULLONG;
    end else begin
     t:=TOK_CLLONG;
    end;
   end else begin
    if l>$7fffffff then begin
     t:=TOK_CUINT;
    end else begin
     t:=TOK_CINT;
    end;
   end;
   lc:=0;
   uc:=0;
   while true do begin
    case CurrentChar of
     ord('U'),ord('u'):begin
      NextChar;
      if uc>=1 then begin
       AddError('Two or more "u"''s in integer constant',nil,false);
      end;
      inc(uc);
      case t of
       TOK_CINT:begin
        t:=TOK_CUINT;
       end;
       TOK_CLLONG:begin
        t:=TOK_CULLONG;
       end;
      end;
     end;
     ord('L'),ord('l'):begin
      NextChar;
      if lc>=2 then begin
       AddError('Three or more "l"''s in integer constant',nil,false);
      end;
      inc(lc);
      if lc=2 then begin
       case t of
        TOK_CINT:begin
         t:=TOK_CLLONG;
        end;
        TOK_CUINT:begin
         t:=TOK_CULLONG;
        end;
       end;
      end;
     end;
     else begin
      break;
     end;
    end;
   end;
   i:=CountTokens;
   inc(CountTokens);
   if CountTokens>length(Tokens) then begin
    SetLength(Tokens,CountTokens*2);
   end;
   Token:=@Tokens[i];
   Token^.TokenType:=t;
   Token^.SourceLocation:=TokenSourceLocation;
   case t of
    TOK_CINT:begin
     Token^.Constant.SignedIntegerValue:=l;
    end;
    TOK_CUINT:begin
     Token^.Constant.UnsignedIntegerValue:=l;
    end;
    TOK_CLLONG:begin
     Token^.Constant.SignedIntegerValue:=l;
    end;
    TOK_CULLONG:begin
     Token^.Constant.UnsignedIntegerValue:=l;
    end;
   end;
   case CurrentChar of
    byte(ansichar('0'))..byte(ansichar('9')),byte(ansichar('a'))..byte(ansichar('z')),byte(ansichar('A'))..byte(ansichar('Z')):begin
     AddError('Invalid character in numberic constant',nil,false);
     repeat
      case CurrentChar of
       byte(ansichar('0'))..byte(ansichar('9')),byte(ansichar('a'))..byte(ansichar('z')),byte(ansichar('A'))..byte(ansichar('Z')):begin
        NextChar;
       end;
       else begin
        break;
       end;
      end;
     until false;
    end;
   end;
  end;
 end;
var lt:TPACCLexerTokenType;
    i:TPACCInt32;
    TokenSymbol:PPACCTokenSymbol;
    s:TPUCUUTF8String;
    StringHashMapEntity:PPACCRawByteStringHashMapEntity;
begin
 for lt:=low(TPACCLexerTokenType) to high(TPACCLexerTokenType) do begin
  if length(KeywordNames[lt])<>0 then begin
   s:=KeywordNames[lt];
   i:=TPACCInstance(Instance).CountTokenSymbols;
   inc(TPACCInstance(Instance).CountTokenSymbols);
   if TPACCInstance(Instance).CountTokenSymbols>length(TPACCInstance(Instance).TokenSymbols) then begin
    SetLength(TPACCInstance(Instance).TokenSymbols,TPACCInstance(Instance).CountTokenSymbols*2);
   end;
   TokenSymbol:=@TPACCInstance(Instance).TokenSymbols[i];
   TokenSymbol^.Name:=s;
   TokenSymbol^.Index:=i+1;
   TPACCInstance(Instance).TokenSymbolStringHashMap.Add(s,pointer(TPACCPtrInt(i)));
  end;
 end;
 s:='';
 TPACCInstance(Instance).SourceLocation.Source:=-1;
 TPACCInstance(Instance).SourceLocation.Line:=-1;
 TPACCInstance(Instance).SourceLocation.Column:=-1;
 PosInfoIndex:=0;
 PragmaInfoIndex:=0;
 CurrentPosition:=1;
 WeAreAtEOF:=false;
 NextChar;
 while not WeAreAtEOF do begin
  TokenSourceLocation:=TPACCInstance(Instance).SourceLocation;
  if (CurrentChar=ord('L')) and (CurrentNextChar in [ord('"'),ord('''')]) then begin
   NextChar;
   ParseString(TOK_CSTR,ENCODING_WCHAR);
  end else if (CurrentChar=ord('u')) and (CurrentNextChar in [ord('"'),ord('''')]) then begin
   NextChar;
   ParseString(TOK_CSTR,ENCODING_CHAR16);
  end else if (CurrentChar=ord('U')) and (CurrentNextChar in [ord('"'),ord('''')]) then begin
   NextChar;
   ParseString(TOK_CSTR,ENCODING_CHAR32);
  end else if (CurrentChar in [ord('A')..ord('Z'),ord('a')..ord('z'),ord('_')]) or (CurrentChar>255) then begin
   s:='';
   repeat
    case CurrentChar of
     ord('A')..ord('Z'),ord('a')..ord('z'),ord('_'),ord('0')..ord('9'):begin
      s:=s+ansichar(byte(CurrentChar));
     end;
     256..$7fffffff:begin
      s:=s+PUCUUTF32CharToUTF8(CurrentChar);
     end;
     else begin
      break;
     end;
    end;
    NextChar;
   until false;
   if (s='u8') and (CurrentChar in [ord('"'),ord('''')]) then begin
    ParseString(TOK_CSTR,ENCODING_UTF8);
   end else begin
    StringHashMapEntity:=LexerKeywordStringHashMap.Get(s,false);
    if assigned(StringHashMapEntity) then begin
     AddSimpleToken(TPACCLexerTokenType(TPACCInt32(TPACCPtrInt(TPACCPtrUInt(StringHashMapEntity^.Value)))));
    end else begin
     AddIdentToken(s);
    end;
   end;
  end else begin
   case CurrentChar of
    0:begin
     NextChar;
    end;
    9..13,32:begin
     NextChar;
    end;
    ord('#'):begin
     NextChar;
     if CurrentChar=ord('#') then begin
      NextChar;
      AddSimpleToken(TOK_TWOSHARPS);
     end else begin
      AddSimpleToken(TOK_SHARP);
     end;
    end;
    ord(''''),ord('"'):begin
     ParseString(TOK_CSTR,ENCODING_NONE);
    end;
    ord('<'):begin
     NextChar;
     case CurrentChar of
      ord(':'):begin
       NextChar;
       AddSimpleToken(TOK_LBRK);
      end;
      ord('%'):begin
       NextChar;
       AddSimpleToken(TOK_LBRA);
      end;
      ord('='):begin
       NextChar;
       AddSimpleToken(TOK_LE);
      end;
      ord('<'):begin
       NextChar;
       case CurrentChar of
        ord('='):begin
         NextChar;
         AddSimpleToken(TOK_A_SHL);
        end;
        else begin
         AddSimpleToken(TOK_SHL);
        end;
       end;
      end;
      else begin
       AddSimpleToken(TOK_LT);
      end;
     end;
    end;
    ord('>'):begin
     NextChar;
     case CurrentChar of
      ord('='):begin
       NextChar;
       AddSimpleToken(TOK_GE);
      end;
      ord('>'):begin
       NextChar;
       case CurrentChar of
        ord('='):begin
         NextChar;
         AddSimpleToken(TOK_A_SHR);
        end;
        else begin
         AddSimpleToken(TOK_SHR);
        end;
       end;
      end;
      else begin
       AddSimpleToken(TOK_GT);
      end;
     end;
    end;
    ord('&'):begin
     NextChar;
     case CurrentChar of
      ord('&'):begin
       NextChar;
       AddSimpleToken(TOK_LAND);
      end;
      ord('='):begin
       NextChar;
       AddSimpleToken(TOK_A_AND);
      end;
      else begin
       AddSimpleToken(TOK_AND);
      end;
     end;
    end;
    ord('|'):begin
     NextChar;
     case CurrentChar of
      ord('|'):begin
       NextChar;
       AddSimpleToken(TOK_LOR);
      end;
      ord('='):begin
       NextChar;
       AddSimpleToken(TOK_A_OR);
      end;
      else begin
       AddSimpleToken(TOK_OR);
      end;
     end;
    end;
    ord('+'):begin
     NextChar;
     case CurrentChar of
      ord('+'):begin
       NextChar;
       AddSimpleToken(TOK_INC);
      end;
      ord('='):begin
       NextChar;
       AddSimpleToken(TOK_A_ADD);
      end;
      else begin
       AddSimpleToken(TOK_ADD);
      end;
     end;
    end;
    ord('-'):begin
     NextChar;
     case CurrentChar of
      ord('-'):begin
       NextChar;
       AddSimpleToken(TOK_DEC);
      end;
      ord('='):begin
       NextChar;
       AddSimpleToken(TOK_A_SUB);
      end;
      ord('>'):begin
       NextChar;
       AddSimpleToken(TOK_ARROW);
      end;
      else begin
       AddSimpleToken(TOK_SUB);
      end;
     end;
    end;
    ord('!'):begin
     NextChar;
     case CurrentChar of
      ord('='):begin
       NextChar;
       AddSimpleToken(TOK_NE);
      end;
      else begin
       AddSimpleToken(TOK_LNOT);
      end;
     end;
    end;
    ord('='):begin
     NextChar;
     case CurrentChar of
      ord('='):begin
       NextChar;
       AddSimpleToken(TOK_EQ);
      end;
      else begin
       AddSimpleToken(TOK_ASSIGN);
      end;
     end;
    end;
    ord('*'):begin
     NextChar;
     case CurrentChar of
      ord('='):begin
       NextChar;
       AddSimpleToken(TOK_A_MUL);
      end;
      else begin
       AddSimpleToken(TOK_MUL);
      end;
     end;
    end;
    ord('%'):begin
     NextChar;
     case CurrentChar of
      ord('='):begin
       NextChar;
       AddSimpleToken(TOK_A_MOD);
      end;
      ord('>'):begin
       NextChar;
       AddSimpleToken(TOK_RBRA);
      end;
      ord(':'):begin
       NextChar;
       case CurrentChar of
        ord('%'):begin
         if CurrentNextChar=ord(':') then begin
          NextChar;
          NextChar;
          AddSimpleToken(TOK_TWOSHARPS);
         end;
        end;
        else begin
         AddSimpleToken(TOK_SHARP);
        end;
       end;
      end;
      else begin
       AddSimpleToken(TOK_MOD);
      end;
     end;
    end;
    ord('^'):begin
     NextChar;
     case CurrentChar of
      ord('='):begin
       NextChar;
       AddSimpleToken(TOK_A_XOR);
      end;
      else begin
       AddSimpleToken(TOK_XOR);
      end;
     end;
    end;
    ord('/'):begin
     NextChar;
     case CurrentChar of
      ord('='):begin
       NextChar;
       AddSimpleToken(TOK_A_DIV);
      end;
      else begin
       AddSimpleToken(TOK_DIV);
      end;
     end;
    end;
    ord(':'):begin
     NextChar;
     case CurrentChar of
      ord('>'):begin
       NextChar;
       AddSimpleToken(TOK_RBRK);
      end;
      else begin
       AddSimpleToken(TOK_COLON);
      end;
     end;
    end;
    ord(';'):begin
     NextChar;
     AddSimpleToken(TOK_SEMICOLON);
    end;
    ord(','):begin
     NextChar;
     AddSimpleToken(TOK_COMMA);
    end;
    ord('?'):begin
     NextChar;
     AddSimpleToken(TOK_QUEST);
    end;
    ord('~'):begin
     NextChar;
     AddSimpleToken(TOK_NOT);
    end;
    ord('('):begin
     NextChar;
     AddSimpleToken(TOK_LPAR);
    end;
    ord(')'):begin
     NextChar;
     AddSimpleToken(TOK_RPAR);
    end;
    ord('['):begin
     NextChar;
     AddSimpleToken(TOK_LBRK);
    end;
    ord(']'):begin
     NextChar;
     AddSimpleToken(TOK_RBRK);
    end;
    ord('{'):begin
     NextChar;
     AddSimpleToken(TOK_LBRA);
    end;
    ord('}'):begin
     NextChar;
     AddSimpleToken(TOK_RBRA);
    end;
    ord('.'):begin
     NextChar;
     case CurrentChar of
      ord('0')..ord('9'):begin
       ParseNumber(ord('.'));
      end;
      ord('.'):begin
       NextChar;
       case CurrentChar of
        ord('.'):begin
         NextChar;
         AddSimpleToken(TOK_ELLIPSIS);
        end;
        else begin
         AddSimpleToken(TOK_DOTS);
        end;
       end;
      end;
      else begin
       AddSimpleToken(TOK_DOT);
      end;
     end;
    end;
    ord('0')..ord('9'):begin
     ParseNumber(0);
    end;
    else begin
     AddError('Unrecognized character: '+PUCUUTF32CharToUTF8(CurrentChar),nil,true);
     NextChar;
    end;
   end;
  end;
 end;
 AddSimpleToken(TOK_EOF);
end;

end.
