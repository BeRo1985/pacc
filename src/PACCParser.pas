unit PACCParser;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PasMP,PUCU,PACCRawByteStringHashMap,PACCTypes,PACCGlobals,PACCPreprocessor,PACCLexer,
     PACCAbstractSyntaxTree;

type TPACCParser=class
      public
       Instance:TObject;
       Preprocessor:TPACCPreprocessor;
       Lexer:TPACCLexer;
       Root:TPACCAbstractSyntaxTreeNodeTranslationUnit;
       constructor Create(const AInstance:TObject);
       destructor Destroy; override;
       procedure Process;
     end;

implementation

uses PACCSort,PACCInstance, PACCTarget;

var StaticCounter:TPasMPInt32=0;
    TempVariableCounter:TPasMPInt32=0;

function CompareSwitchCases(const a,b:pointer):TPACCInt32;
begin
 result:=PPACCAbstractSyntaxTreeNodeSWITCHStatementCase(a)^.CaseBegin-PPACCAbstractSyntaxTreeNodeSWITCHStatementCase(b)^.CaseBegin;
 if result=0 then begin
  result:=PPACCAbstractSyntaxTreeNodeSWITCHStatementCase(a)^.CaseEnd-PPACCAbstractSyntaxTreeNodeSWITCHStatementCase(b)^.CaseEnd;
 end;
end;

constructor TPACCParser.Create(const AInstance:TObject);
begin
 inherited Create;
 Instance:=AInstance;
 Preprocessor:=TPACCInstance(Instance).Preprocessor;
 Lexer:=TPACCInstance(Instance).Lexer;
 Root:=TPACCAbstractSyntaxTreeNodeTranslationUnit.Create(TPACCInstance(Instance),astnkTRANSLATIONUNIT,nil,TPACCInstance(Instance).SourceLocation);
end;

destructor TPACCParser.Destroy;
begin
 Root.Free;
 inherited Destroy;
end;

procedure TPACCParser.Process;
const S_TYPEDEF=1;
      S_EXTERN=2;
      S_STATIC=4;
      S_AUTO=8;
      S_REGISTER=16;
      DECL_BODY=1;
      DECL_PARAM=2;
      DECL_PARAM_TYPEONLY=4;
      DECL_CAST=8;
type PState=^TState;
     TState=record
      TokenIndex:TPACCInt32;
      Token:PPACCLexerToken;
      PeekedToken:PPACCLexerToken;
      IsEOF:boolean;
     end;
     PCase=^TCase;
     TCase=record
      Begin_:TPACCInt;
      End_:TPACCInt;
      Label_:TPACCAbstractSyntaxTreeNodeLabel;
     end;
     TCases=record
      Ready:boolean;
      Cases:array of TCase;
      Count:TPACCInt;
      DefaultCaseLabel:TPACCAbstractSyntaxTreeNodeLabel;
     end;
const TokenEOF:TPACCLexerToken=(TokenType:TOK_EOF;
                                SourceLocation:(
                                 Source:0;
                                 Line:0;
                                 Column:0
                                );
                                Index:0;
                                PragmaString:'';
                                Constant:(
                                 StringValue:'';
                                 Encoding:ENCODING_NONE;
                                 SignedIntegerValue:0
                                );
                               );
var CurrentState:TState;
    GlobalScope:TPACCRawByteStringHashMap;
    LocalScope:TPACCRawByteStringHashMap;
    TagScope:TPACCRawByteStringHashMap;
    LabelScope:TPACCRawByteStringHashMap;
    LocalVariables:TPACCAbstractSyntaxTreeNodeList;
    Labels:TPACCAbstractSyntaxTreeNodeList;
    UnresolvedLabelUsedNodes:TPACCAbstractSyntaxTreeNodeList;
    Cases:TCases;
    CurrentFunctionType:PPACCType;
    BreakLabel:TPACCAbstractSyntaxTreeNodeLabel;
    ContinueLabel:TPACCAbstractSyntaxTreeNodeLabel;
    PragmaPack:TPACCInt32;
    PragmaPackStack:array of TPACCInt32;
    PragmaPackStackPointer:TPACCInt32;
 procedure AddError(const s:TPUCUUTF8String;const SourceLocation:PPACCSourceLocation=nil;const DoAbort:boolean=false);
 begin
  TPACCInstance(Instance).AddError(s,SourceLocation,DoAbort);
 end;
 procedure AddWarning(const s:TPUCUUTF8String;const SourceLocation:PPACCSourceLocation=nil);
 begin
  TPACCInstance(Instance).AddWarning(s,SourceLocation);
 end;
 procedure ParsePragma(const PragmaString:TPACCRawByteString;const RelevantSourceLocation:TPACCSourceLocation);
 var CurrentChar:ansichar;
     CurrentPosition:TPACCInt32;
     AtEOI:boolean;
  function NextChar:ansichar;
  begin
   if CurrentPosition<=length(PragmaString) then begin
    result:=PragmaString[CurrentPosition];
    inc(CurrentPosition);
   end else begin
    result:=#0;
    AtEOI:=true;
   end;
   CurrentChar:=result;
  end;
  procedure SkipWhiteSpace;
  begin
   while CurrentChar in [#1..#32] do begin
    NextChar;
   end;
  end;
 var PragmaCommand,Temp:TPACCRawByteString;
 begin
  AtEOI:=false;
  CurrentPosition:=1;

  NextChar;

  while not AtEOI do begin

   SkipWhiteSpace;

   if CurrentChar in ['a'..'z','A'..'Z','_'] then begin

    PragmaCommand:='';
    while CurrentChar in ['a'..'z','A'..'Z','_'] do begin
     PragmaCommand:=PragmaCommand+CurrentChar;
     NextChar;
    end;

    SkipWhiteSpace;

    if (PragmaCommand='STDC') or (PragmaCommand='stdc') then begin

     // Silence ignoring without any warning, until implemented
     break;

    end else if PragmaCommand='pack' then begin

     if CurrentChar='(' then begin
      NextChar;

      SkipWhiteSpace;

      if CurrentChar=')' then begin

       // #pragma pack()

       NextChar;
       PragmaPack:=-1;

      end else begin

       if CurrentChar in ['a'..'z','A'..'Z','_'] then begin

        // #pragma pack(push[,n])
        // #pragma pack(pop[,n])

        Temp:='';
        while CurrentChar in ['a'..'z','A'..'Z','_'] do begin
         Temp:=Temp+CurrentChar;
         NextChar;
        end;
        if Temp='push' then begin
         if length(PragmaPackStack)<=PragmaPackStackPointer then begin
          SetLength(PragmaPackStack,(PragmaPackStackPointer+1)*2);
         end;
         PragmaPackStack[PragmaPackStackPointer]:=PragmaPack;
         inc(PragmaPackStackPointer);
        end else if Temp='pop' then begin
         if PragmaPackStackPointer>0 then begin
          dec(PragmaPackStackPointer);
          PragmaPack:=PragmaPackStack[PragmaPackStackPointer];
         end else begin
          AddWarning('Pragma pack stack underflow',@RelevantSourceLocation);
         end;
        end else begin
         AddWarning('"push" or "pop" expected',@RelevantSourceLocation);
        end;

        SkipWhiteSpace;

        case CurrentChar of
         ',':begin
          NextChar;
          SkipWhiteSpace;
         end;
         ')':begin
          NextChar;
          SkipWhiteSpace;
          if CurrentChar=',' then begin
           NextChar;
           continue;
          end else begin
           AddWarning('Pragma syntax error',@RelevantSourceLocation);
           break;
          end;
          continue;
         end;
         else begin
          AddWarning('Pragma syntax error',@RelevantSourceLocation);
         end;
        end;

       end;

       if CurrentChar in ['0'..'9'] then begin
        Temp:='';
        while CurrentChar in ['0'..'9'] do begin
         Temp:=Temp+CurrentChar;
         NextChar;
        end;
        PragmaPack:=StrToIntDef(Temp,-1);
        if (PragmaPack>0) and ((PragmaPack and (PragmaPack-1))<>0) then begin
         AddError('Pragma pack be power of 2, but got '+IntToStr(PragmaPack),@RelevantSourceLocation,false);
         break;
        end else if PragmaPack<0 then begin
         AddError('Invalud pragma pack',@RelevantSourceLocation,false);
         break;
        end else if PragmaPack=0 then begin
         PragmaPack:=-1;
        end;
       end;

       if CurrentChar=')' then begin
        NextChar;
       end;
      end;

     end;

    end else begin

     AddWarning('Unknown pragma command "'+PragmaCommand+'", aborting pragma parsing, and ignoring the rest of pragma string',@RelevantSourceLocation);
     break;

    end;

    SkipWhiteSpace;

    if CurrentChar=',' then begin
     NextChar;
     continue;
    end else begin
     AddWarning('Pragma syntax error',@RelevantSourceLocation);
     break;
    end;

   end else begin
    AddWarning('Pragma syntax error',@RelevantSourceLocation);
    break;
   end;

  end;
 end;
 function NextToken:PPACCLexerToken;
 begin
  repeat
   inc(CurrentState.TokenIndex);
   if CurrentState.TokenIndex<Lexer.CountTokens then begin
    CurrentState.Token:=@Lexer.Tokens[CurrentState.TokenIndex];
    if CurrentState.Token^.TokenType=TOK_EOF then begin
     CurrentState.IsEOF:=true;
    end;
    result:=CurrentState.Token;
    TPACCInstance(Instance).SourceLocation:=result.SourceLocation;
    if result^.TokenType=TOK_PRAGMA then begin
     ParsePragma(result.PragmaString,result.SourceLocation);
     continue;
    end;
   end else begin
    CurrentState.Token:=@TokenEOF;
    CurrentState.IsEOF:=true;
    result:=nil;
   end;
   break;
  until false;
 end;
 function NextTokenType:TPACCLexerTokenType;
 begin
  NextToken;
  result:=CurrentState.Token^.TokenType;
 end;
 function PeekToken(Offset:TPACCInt32=1):PPACCLexerToken;
 begin
  repeat
   if ((CurrentState.TokenIndex+Offset)>=0) and ((CurrentState.TokenIndex+Offset)<Lexer.CountTokens) then begin
    CurrentState.PeekedToken:=@Lexer.Tokens[CurrentState.TokenIndex+Offset];
    result:=CurrentState.PeekedToken;
    if result^.TokenType=TOK_PRAGMA then begin
     inc(Offset);
     continue;
    end;
   end else begin
    CurrentState.PeekedToken:=@TokenEOF;
    result:=nil;
   end;
   break;
  until false;
 end;
 function PeekTokenType(Offset:TPACCInt32=1):TPACCLexerTokenType;
 begin
  PeekToken(Offset);
  result:=CurrentState.PeekedToken^.TokenType;
 end;
 function GetScope:TPACCRawByteStringHashMap;
 begin
  if assigned(LocalScope) then begin
   result:=LocalScope;
  end else begin
   result:=GlobalScope;
  end;
 end;
 function GetTypeDef(const Name:TPACCRawByteString):PPACCType;
 var Node:TPACCAbstractSyntaxTreeNode;
 begin
  Node:=GetScope[Name];
  if assigned(Node) and (Node.Kind=astnkTYPEDEF) then begin
   result:=TPACCAbstractSyntaxTreeNode(Node).Type_;
  end else begin
   result:=nil;
  end;
 end;
 function NewASTString(const SourceLocation:TPACCSourceLocation;const Encoding:TPACCEncoding;const Buf:TPACCRawByteString):TPACCAbstractSyntaxTreeNode;
 var Type_:PPACCType;
     Body:TPACCRawByteString;
     Temp16Bit:TPUCUUTF16String;
     Temp32Bit:TPUCUUTF32String;
 begin
  case Encoding of
   ENCODING_NONE,ENCODING_UTF8:begin
    Type_:=TPACCInstance(Instance).NewArrayType(TPACCInstance(Instance).TypeCHAR,length(Buf));
    Body:=Buf;
   end;
   ENCODING_CHAR16:begin
    Temp16Bit:=PUCUUTF8ToUTF16(Buf);
    Type_:=TPACCInstance(Instance).NewArrayType(TPACCInstance(Instance).TypeUSHORT,length(Temp16Bit));
    SetLength(Body,length(Temp16Bit)*SizeOf(TPUCUUTF16Char));
    if length(Temp16Bit)>0 then begin
     Move(Temp16Bit[1],Body[1],length(Temp16Bit)*SizeOf(TPUCUUTF16Char));
    end;
   end;
   else {ENCODING_CHAR32,ENCODING_WCHAR:}begin
    Temp32Bit:=PUCUUTF8ToUTF32(Buf);
    Type_:=TPACCInstance(Instance).NewArrayType(TPACCInstance(Instance).TypeUINT,length(Temp32Bit));
    SetLength(Body,length(Temp32Bit)*SizeOf(TPUCUUTF32Char));
    if length(Temp32Bit)>0 then begin
     Move(Temp32Bit[low(Temp32Bit)],Body[1],length(Temp32Bit)*SizeOf(TPUCUUTF32Char));
    end;
   end;
  end;
  result:=TPACCAbstractSyntaxTreeNodeStringValue.Create(TPACCInstance(Instance),astnkSTRING,Type_,SourceLocation,Body,Encoding);
 end;
 function NewLabel(const Kind:TPACCAbstractSyntaxTreeNodeKind;const RelevantSourceLocation:TPACCSourceLocation;const LabelName:TPACCRawByteString=''):TPACCAbstractSyntaxTreeNodeLabel;
 begin
  result:=TPACCAbstractSyntaxTreeNodeLabel.Create(TPACCInstance(Instance),Kind,nil,RelevantSourceLocation,LabelName);
  if assigned(Labels) then begin
   Labels.Add(result); 
  end;
 end;
 function IsType(Token:PPACCLexerToken):boolean;
 begin
  result:=false;
  if assigned(Token) then begin
   case Token^.TokenType of
    TOK_ALIGNAS,
    TOK_ATTRIBUTE,
    TOK_AUTO,
    TOK_BOOL,
    TOK_CHAR,
    TOK_COMPLEX,
    TOK_CONST,
    TOK_DOUBLE,
    TOK_ENUM,
    TOK_EXTERN,
    TOK_FLOAT,
    TOK_IMAGINARY,
    TOK_INLINE,
    TOK_INT,
    TOK_LONG,
    TOK_NORETURN,
    TOK_REGISTER,
    TOK_RESTRICT,
    TOK_SHORT,
    TOK_SIGNED1,
    TOK_SIGNED2,
    TOK_SIGNED3,
    TOK_STATIC,
    TOK_STRUCT,
    TOK_TYPEDEF,
    TOK_TYPEOF,
    TOK_UNION,
    TOK_UNSIGNED,
    TOK_VOID,
    TOK_VOLATILE:begin
     result:=true;
    end;
    TOK_IDENT:begin
     if TPACCInstance(Instance).Target.CheckCallingConvention(TPACCInstance(Instance).TokenSymbols[Token^.Index].Name)>=0 then begin
      result:=true;
     end else begin
      result:=assigned(GetTypeDef(TPACCInstance(Instance).TokenSymbols[Token^.Index].Name));
     end;
    end;
   end;
  end;
 end;
 procedure SkipParentheses;
 var Level:TPACCInt32;
 begin
  Level:=0;
  if CurrentState.Token^.TokenType=TOK_LPAR then begin
   NextToken;
   repeat
    case CurrentState.Token^.TokenType of
     TOK_EOF:begin
      AddError('Premature end of input',@CurrentState.Token^.SourceLocation,true);
     end;
     TOK_LPAR:begin
      inc(Level);
     end;
     TOK_RPAR:begin
      if Level=0 then begin
       NextToken;
       break;
      end else begin
       dec(Level);
      end;
     end;
    end;
    NextToken;
   until false;
  end;
 end;
 procedure Expect(const TokenType:TPACCLexerTokenType);
 begin
  if CurrentState.Token^.TokenType=TokenType then begin
   NextToken;
  end else begin
   AddError('"'+TPACCLexer(TPACCInstance(Instance).Lexer).TokenStrings[CurrentState.Token^.TokenType]+'" expected',@CurrentState.Token^.SourceLocation,true);
  end;
 end;
 procedure EnsureLValue(const Node:TPACCAbstractSyntaxTreeNode);
 begin
  if assigned(Node) and (afConstant in Node.Type_^.Attribute.Flags) then begin
   AddError('Expression must be a modifiable lvalue',nil,false);
  end;                                                                    
  if (not assigned(Node)) or not (Node.Kind in [astnkLVAR,astnkGVAR,astnkDEREF,astnkSTRUCT_REF]) then begin
   AddError('lvalue expected',nil,true);
  end;
 end;
 procedure EnsureIntType(const Node:TPACCAbstractSyntaxTreeNode);
 begin
  if (not assigned(Node)) or not TPACCInstance(Instance).IsIntType(Node.Type_) then begin
   AddError('Integer type expected',nil,false);
  end;
 end;
 procedure EnsureArithmeticType(const Node:TPACCAbstractSyntaxTreeNode);
 begin
  if (not assigned(Node)) or not TPACCInstance(Instance).IsArithmeticType(Node.Type_) then begin
   AddError('Arithmetic type expected',nil,false);
  end;
 end;
 procedure EnsureNotVoid(const Type_:PPACCType);
 begin
  if assigned(Type_) and (Type_^.Kind=tkVOID) then begin
   AddError('void is not allowed',nil,false);
  end;
 end;
 procedure SkipTypeQualifiers;
 begin      
  while CurrentState.Token^.TokenType in [TOK_CONST,TOK_VOLATILE,TOK_RESTRICT] do begin
   NextToken;
  end;
 end;
 function Wrap(const t:PPACCType;const Node:TPACCAbstractSyntaxTreeNode):TPACCAbstractSyntaxTreeNode;
 begin
  if TPACCInstance(Instance).SameArithmeticType(t,Node.Type_) then begin
   result:=Node;
  end else begin
   result:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkCONV,t,Node.SourceLocation,Node);
  end;
 end;
 function ValidPointerBinaryOperation(const Op:TPACCAbstractSyntaxTreeNodeKind):boolean;
 begin
  result:=Op in [astnkOP_SUB,astnkOP_LT,astnkOP_GT,astnkOP_EQ,astnkOP_NE,astnkOP_GE,astnkOP_LE];
 end;
 function BinaryOperation(const Op:TPACCAbstractSyntaxTreeNodeKind;const lhs,rhs:TPACCAbstractSyntaxTreeNode):TPACCAbstractSyntaxTreeNode;
 var r:PPACCType;
 begin
  if not (assigned(lhs) and assigned(lhs.Type_)) then begin
   result:=nil;
   AddError('Internal error 2017-01-08-04-35-0000',nil,true);
  end else if not (assigned(rhs) and assigned(rhs.Type_)) then begin
   result:=nil;
   AddError('Internal error 2017-01-08-04-36-0000',nil,true);
  end else if lhs.Type_^.Kind=tkPOINTER then begin
   if rhs.Type_^.Kind=tkPOINTER then begin
    if not ValidPointerBinaryOperation(Op) then begin
     AddError('Iinvalid pointer arithmetic operation',nil,false);
    end;
    if Op=astnkOP_SUB then begin
     // C11 6.5.6.9: Pointer subtractions have type ptrdiff_t.
     result:=TPACCAbstractSyntaxTreeNodeBinaryOperator.Create(TPACCInstance(Instance),Op,TPACCInstance(Instance).TypeLONG,lhs.SourceLocation,lhs,rhs);
    end else begin
     // C11 6.5.8.6, 6.5.9.3: Pointer comparisons have type int.
     result:=TPACCAbstractSyntaxTreeNodeBinaryOperator.Create(TPACCInstance(Instance),Op,TPACCInstance(Instance).TypeINT,lhs.SourceLocation,lhs,rhs);
    end;
   end else begin
    result:=TPACCAbstractSyntaxTreeNodeBinaryOperator.Create(TPACCInstance(Instance),Op,lhs.Type_,lhs.SourceLocation,lhs,rhs);
   end;
  end else if rhs.Type_^.Kind=tkPOINTER then begin
   result:=TPACCAbstractSyntaxTreeNodeBinaryOperator.Create(TPACCInstance(Instance),Op,rhs.Type_,rhs.SourceLocation,rhs,lhs);
  end else begin
   if not TPACCInstance(Instance).IsArithmeticType(lhs.Type_) then begin
    AddError('Internal error 2017-01-01-21-02-0000',nil,true);
   end;
   if not TPACCInstance(Instance).IsArithmeticType(rhs.Type_) then begin
    AddError('Internal error 2017-01-01-21-02-0001',nil,true);
   end;
   r:=TPACCInstance(Instance).UsualArithmeticConversion(lhs.Type_,rhs.Type_);
   result:=TPACCAbstractSyntaxTreeNodeBinaryOperator.Create(TPACCInstance(Instance),Op,r,lhs.SourceLocation,Wrap(r,lhs),Wrap(r,rhs));
  end;
 end;
 procedure EnsureAssignable(const ToType,FromType:PPACCType);
 begin
  if not (((TPACCInstance(Instance).IsArithmeticType(ToType) or (ToType^.Kind=tkPOINTER)) and
          (TPACCInstance(Instance).IsArithmeticType(FromType) or (FromType^.Kind=tkPOINTER))) or
          TPACCInstance(Instance).IsSameStruct(ToType,FromType)) then begin
   AddError('Incompatible types',nil,false);
  end;
 end;
 procedure ParseDeclaration(const Block:TPACCAbstractSyntaxTreeNodeList;const IsGlobal,IsTop:boolean); forward;
 function ParseIntegerExpression:TPACCInt64; forward;
 function ParseCastType:PPACCType; forward;
 function ParseCastExpression:TPACCAbstractSyntaxTreeNode; forward;
 function ParseExpression:TPACCAbstractSyntaxTreeNode; forward;
 function ParseCompoundStatement:TPACCAbstractSyntaxTreeNode; forward;
 function ParseAssignmentExpression:TPACCAbstractSyntaxTreeNode; forward;
 function ParseGeneric:TPACCAbstractSyntaxTreeNode;
 var Type_:PPACCType;
     ConstantExpression,DefaultExpression,TypeExpression,MatchExpression:TPACCAbstractSyntaxTreeNode;
     RelevantSourceLocation:TPACCSourceLocation;
 begin
  RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
  Expect(TOK_LPAR);
  ConstantExpression:=ParseAssignmentExpression;
  Expect(TOK_COMMA);
  DefaultExpression:=nil;
  MatchExpression:=nil;
  repeat
   case CurrentState.Token^.TokenType of
    TOK_RPAR:begin
     NextToken;
     break;
    end;
    TOK_DEFAULT:begin
     if assigned(DefaultExpression) then begin
      AddError('Default expression specified twice',@RelevantSourceLocation,false);
     end;
     NextToken;
     Expect(TOK_COLON);
     DefaultExpression:=ParseAssignmentExpression;
    end;
    else begin
     Type_:=ParseCastType;
     Expect(TOK_COLON);
     TypeExpression:=ParseAssignmentExpression;
     if (not assigned(MatchExpression)) and TPACCInstance(Instance).AreTypesCompatible(ConstantExpression.Type_,Type_) then begin
      MatchExpression:=TypeExpression;
     end;
    end;
   end;
   case CurrentState.Token^.TokenType of
    TOK_COMMA:begin
     NextToken;
    end;
    TOK_RPAR:begin
     NextToken;
     break;
    end;
    else begin
     AddError('")" or "," expected, but got "'+TPACCLexer(TPACCInstance(Instance).Lexer).TokenStrings[CurrentState.Token^.TokenType]+'"',@CurrentState.Token^.SourceLocation,true);
    end;
   end;
  until false;
  if assigned(MatchExpression) then begin
   result:=MatchExpression;
  end else if assigned(DefaultExpression) then begin
   result:=DefaultExpression;
  end else begin
   result:=nil;
   AddError('No matching generic selection',@RelevantSourceLocation,false);
  end;
 end;
 function ParseStringValue(out Encoding:TPACCEncoding):TPACCRawByteString;
 var FirstError:boolean;
 begin
  if CurrentState.Token^.TokenType=TOK_CSTR then begin
   result:=CurrentState.Token^.Constant.StringValue;
   Encoding:=CurrentState.Token^.Constant.Encoding;
   NextToken;
   FirstError:=true;
   while CurrentState.Token^.TokenType=TOK_CSTR do begin
    if (Encoding<>ENCODING_NONE) and (CurrentState.Token^.Constant.Encoding<>ENCODING_NONE) and (Encoding<>CurrentState.Token^.Constant.Encoding) then begin
     if FirstError then begin
      FirstError:=false;
      AddError('Unsupported non-standard concatenation of string literals',@CurrentState.Token^.SourceLocation,false);
     end;
    end else begin
     if Encoding=ENCODING_NONE then begin
      Encoding:=CurrentState.Token^.Constant.Encoding;
     end;
     result:=result+CurrentState.Token^.Constant.StringValue;
    end;
    NextToken;
   end;
  end else begin
   Encoding:=ENCODING_NONE;
   Expect(TOK_CSTR);
  end;
 end;
 function ParsePrimaryExpression:TPACCAbstractSyntaxTreeNode;
 var Type_:PPACCType;
     Node:TPACCAbstractSyntaxTreeNode;
     Name,StringValue:TPACCRawByteString;
     RelevantSourceLocation:TPACCSourceLocation;
     Encoding:TPACCEncoding;
 begin
  RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
  case CurrentState.Token^.TokenType of
   TOK_LPAR:begin
    NextToken;
    if CurrentState.Token^.TokenType=TOK_LBRA then begin
     NextToken;
     result:=ParseCompoundStatement;
     Expect(TOK_RPAR);
     Type_:=TPACCInstance(Instance).TypeVOID;
     if assigned(result) and (result is TPACCAbstractSyntaxTreeNodeStatements) and (TPACCAbstractSyntaxTreeNodeStatements(result).Children.Count>0) then begin
      Node:=TPACCAbstractSyntaxTreeNodeStatements(result).Children[TPACCAbstractSyntaxTreeNodeStatements(result).Children.Count-1];
      if assigned(Node) and assigned(Node.Type_) then begin
       Type_:=Node.Type_;
      end;
     end;
     result.Type_:=Type_;
    end else begin
     result:=ParseExpression;
     Expect(TOK_RPAR);
    end;
   end;
   TOK_GENERIC:begin
    NextToken;
    result:=ParseGeneric;
   end;
   TOK_IDENT:begin
    Name:=TPACCInstance(Instance).TokenSymbols[CurrentState.Token^.Index].Name;
    result:=GetScope[Name];
    NextToken;
    if assigned(result) then begin
     if result.Type_^.Kind=tkFUNCTION Then begin
      result:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration.Create(TPACCInstance(Instance),astnkFUNCDESG,result.Type_,result.SourceLocation,Name,nil,nil,nil);
     end;
    end else begin
     if CurrentState.Token^.TokenType=TOK_LPAR then begin
      AddWarning('assume returning int: '+Name+'()',@CurrentState.Token^.SourceLocation);
      result:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration.Create(TPACCInstance(Instance),astnkFUNCDESG,TPACCInstance(Instance).NewFunctionType(TPACCInstance(Instance).TypeINT,nil,true,false),CurrentState.Token^.SourceLocation,Name,nil,nil,nil);
     end else begin
      result:=nil;
      AddError('Undeclared variable: '+Name,@RelevantSourceLocation,false);
     end;
    end;
   end;
   TOK_CINT:begin
    result:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(TPACCInstance(Instance),astnkINTEGER,TPACCInstance(Instance).TypeINT,RelevantSourceLocation,CurrentState.Token^.Constant.SignedIntegerValue);
    NextToken;
   end;
   TOK_CUINT:begin
    result:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(TPACCInstance(Instance),astnkINTEGER,TPACCInstance(Instance).TypeUINT,RelevantSourceLocation,CurrentState.Token^.Constant.UnsignedIntegerValue);
    NextToken;
   end;
   TOK_CCHAR:begin
    case CurrentState.Token^.Constant.Encoding of
     ENCODING_NONE,ENCODING_UTF8:begin
      result:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(TPACCInstance(Instance),astnkINTEGER,TPACCInstance(Instance).TypeUCHAR,RelevantSourceLocation,CurrentState.Token^.Constant.UnsignedIntegerValue);
     end;
     ENCODING_CHAR16:begin
      result:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(TPACCInstance(Instance),astnkINTEGER,TPACCInstance(Instance).TypeUSHORT,RelevantSourceLocation,CurrentState.Token^.Constant.UnsignedIntegerValue);
     end;
     else begin
      result:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(TPACCInstance(Instance),astnkINTEGER,TPACCInstance(Instance).TypeUINT,RelevantSourceLocation,CurrentState.Token^.Constant.UnsignedIntegerValue);
     end;
    end;
    NextToken;
   end;
   TOK_CSTR:begin
    StringValue:=ParseStringValue(Encoding);
    result:=NewASTString(RelevantSourceLocation,Encoding,StringValue);
   end;
   TOK_CFLOAT:begin
    result:=TPACCAbstractSyntaxTreeNodeFloatValue.Create(TPACCInstance(Instance),astnkFLOAT,TPACCInstance(Instance).TypeFLOAT,RelevantSourceLocation,CurrentState.Token^.Constant.FloatValue);
    NextToken;
   end;
   TOK_CDOUBLE:begin
    result:=TPACCAbstractSyntaxTreeNodeFloatValue.Create(TPACCInstance(Instance),astnkFLOAT,TPACCInstance(Instance).TypeDOUBLE,RelevantSourceLocation,CurrentState.Token^.Constant.DoubleValue);
    NextToken;
   end;
   TOK_CLDOUBLE:begin
    result:=TPACCAbstractSyntaxTreeNodeFloatValue.Create(TPACCInstance(Instance),astnkFLOAT,TPACCInstance(Instance).TypeLDOUBLE,RelevantSourceLocation,CurrentState.Token^.Constant.DoubleValue);
    NextToken;
   end;
   TOK_CLLONG:begin
    result:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(TPACCInstance(Instance),astnkINTEGER,TPACCInstance(Instance).TypeLLONG,RelevantSourceLocation,CurrentState.Token^.Constant.SignedIntegerValue);
    NextToken;
   end;
   TOK_CULLONG:begin
    result:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(TPACCInstance(Instance),astnkINTEGER,TPACCInstance(Instance).TypeULLONG,RelevantSourceLocation,CurrentState.Token^.Constant.UnsignedIntegerValue);
    NextToken;
   end;
   else begin
    if not (CurrentState.Token^.TokenType in KeywordTokens) then begin
     AddError('Internal error 2017-01-02-03-28-0000',@RelevantSourceLocation,true);
    end;
    result:=nil;
   end;
  end;
 end;
 function ParseFunctionArguments(const Parameters:TPPACCTypes):TPACCAbstractSyntaxTreeNodeList;
 var Index:longint;
     ArgumentNode:TPACCAbstractSyntaxTreeNode;
     ParameterType:PPACCType;
 begin
  result:=TPACCAbstractSyntaxTreeNodeList.Create;
  TPACCInstance(Instance).AllocatedObjects.Add(result);
  Index:=0;
  repeat
   if CurrentState.Token^.TokenType=TOK_RPAR then begin
    NextToken;
    break;
   end else begin
    ArgumentNode:=TPACCInstance(Instance).TypeConversion(ParseAssignmentExpression);
    if Index<length(Parameters) then begin
     ParameterType:=Parameters[Index];
     inc(Index);
    end else begin
     if TPACCInstance(Instance).IsFloatType(ArgumentNode.Type_) then begin
      ParameterType:=TPACCInstance(Instance).TypeDOUBLE;
     end else if TPACCInstance(Instance).IsIntType(ArgumentNode.Type_) then begin
      ParameterType:=TPACCInstance(Instance).TypeINT;
     end else begin
      ParameterType:=ArgumentNode.Type_;
     end;
    end;
    EnsureAssignable(ParameterType,ArgumentNode.Type_);
    if ParameterType^.Kind<>ArgumentNode.Type_^.Kind then begin
     ArgumentNode:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkCONV,ParameterType,ArgumentNode.SourceLocation,ArgumentNode);
    end;
    result.Add(ArgumentNode);
    case CurrentState.Token^.TokenType of
     TOK_COMMA:begin
      NextToken;
     end;
     TOK_RPAR:begin
      NextToken;
      break;
     end;
     else begin
      AddError('")" or "," expected, but got "'+TPACCLexer(TPACCInstance(Instance).Lexer).TokenStrings[CurrentState.Token^.TokenType]+'"',@CurrentState.Token^.SourceLocation,true);
     end;
    end;
   end;
  until false;
 end;
 function ParseFunctionCall(const Node:TPACCAbstractSyntaxTreeNode):TPACCAbstractSyntaxTreeNode;
 var Desg:TPACCAbstractSyntaxTreeNode;
 begin
  if (Node.Kind=astnkADDR) and (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Kind=astnkFUNCDESG) then begin
   Desg:=TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand;
   result:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration.Create(TPACCInstance(Instance),astnkFUNCCALL,Desg.Type_^.ReturnType,Desg.SourceLocation,TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Desg).FunctionName,Desg.Type_,nil,nil);
   TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(result).Parameters:=ParseFunctionArguments(Desg.Type_^.Parameters);
  end else begin
   if (Node.Type_^.Kind=tkPOINTER) and
      (Node.Type_^.ChildTYpe^.Kind=tkFUNCTION) then begin
    result:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration.Create(TPACCInstance(Instance),astnkFUNCPTR_CALL,Node.Type_^.ChildType^.ReturnType,Node.SourceLocation,TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).FunctionName,nil,Node,nil);
    TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(result).Parameters:=ParseFunctionArguments(Node.Type_^.ChildType^.Parameters);
   end else begin
    result:=nil;
    AddError('Internal error 2017-01-02-17-56-0000',nil,true);
   end;
  end;
 end;
 function ParseStructField(const Node:TPACCAbstractSyntaxTreeNode):TPACCAbstractSyntaxTreeNode;
 var Index:longint;
     Type_,FieldType:PPACCType;
     Name:TPACCRawByteString;
 begin
  Type_:=Node.Type_;
  if (not assigned(Type_)) or (Type_^.Kind<>tkSTRUCT) then begin
   AddError('Struct or union type expected',nil,true);
  end;
  if CurrentState.Token^.TokenType=TOK_IDENT then begin
   Name:=TPACCInstance(Instance).TokenSymbols[CurrentState.Token^.Index].Name;
   NextToken;
   FieldType:=nil;
   for Index:=0 to length(Type_^.Fields)-1 do begin
    if Type_^.Fields[Index].Name=Name then begin
     FieldType:=Type_^.Fields[Index].Type_;
     break;
    end;
   end;
   if assigned(FieldType) then begin
    result:=TPACCAbstractSyntaxTreeNodeStructReference.Create(TPACCInstance(Instance),astnkSTRUCT_REF,FieldType,Node.SourceLocation,Node,Name);
   end else begin
    AddError('Struct/union has no such field with the name "'+Name+'"',nil,false);
    result:=nil;
   end;
  end else begin
   AddError('Field name expected',nil,true);
   result:=nil;
  end;
 end;
 function ParseSubscriptionExpression(const Node:TPACCAbstractSyntaxTreeNode):TPACCAbstractSyntaxTreeNode;
 var SubscriptionExpression:TPACCAbstractSyntaxTreeNode;
 begin
  SubscriptionExpression:=ParseExpression;
  if assigned(SubscriptionExpression) then begin
   Expect(TOK_RBRK);
   result:=BinaryOperation(astnkOP_ADD,TPACCInstance(Instance).TypeConversion(Node),TPACCInstance(Instance).TypeConversion(SubscriptionExpression));
   result:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkDEREF,result.Type_^.ChildType,result.SourceLocation,result);
  end else begin
   AddError('Subscription expected',nil,true);
   result:=nil;
  end;
 end;
 function ParsePostfixExpressionTail(const Node:TPACCAbstractSyntaxTreeNode):TPACCAbstractSyntaxTreeNode;
 var Type_:PPACCType;
 begin
  result:=Node;
  if assigned(result) then begin
   repeat
    case CurrentState.Token^.TokenType of
     TOK_LPAR:begin
      NextToken;
      result:=TPACCInstance(Instance).TypeConversion(result);
      Type_:=result.Type_;
      if (Type_^.Kind<>tkPOINTER) or ((not assigned(Type_^.ChildType)) or (Type_^.ChildType^.Kind<>tkFUNCTION)) then begin
       AddError('Function expected',nil,false);
      end;
      result:=ParseFunctionCall(result);
      continue;
     end;
     TOK_LBRK:begin
      NextToken;
      result:=ParseSubscriptionExpression(result);
      continue;
     end;
     TOK_DOT:begin
      NextToken;
      result:=ParseStructField(result);
      continue;
     end;
     TOK_ARROW:begin
      if result.Type_^.Kind<>tKPOINTER then begin
       AddError('Pointer type expected',nil,false);
      end;
      result:=ParseStructField(TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkDEREF,result.Type_^.ChildType,CurrentState.Token^.SourceLocation,result));
      NextToken;
      continue;
     end;
     TOK_DEC:begin
      EnsureLValue(result);
      result:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkOP_POST_DEC,result.Type_,CurrentState.Token^.SourceLocation,result);
      NextToken;
     end;
     TOK_INC:begin
      EnsureLValue(result);
      result:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkOP_POST_INC,result.Type_,CurrentState.Token^.SourceLocation,result);
      NextToken;
     end;
     else begin
      break;
     end;
    end;
   until false;
  end;
 end;
 function ParsePostfixExpression:TPACCAbstractSyntaxTreeNode;
 begin
  result:=ParsePostfixExpressionTail(ParsePrimaryExpression);
 end;
 function ParseUnaryExpression:TPACCAbstractSyntaxTreeNode;
 var Type_:PPACCType;
     RelevantSourceLocation:TPACCSourceLocation;
 begin
  RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
  case CurrentState.Token^.TokenType of
   TOK_SIZEOF:begin
    NextToken;
    if CurrentState.Token^.TokenType=TOK_LPAR then begin
     NextToken;
     Type_:=ParseCastType;
     Expect(TOK_RPAR);
    end else begin
     Type_:=ParseUnaryExpression.Type_;
    end;
    if Type_^.Kind in [tkVOID,tkFUNCTION] then begin
     // sizeof on void or function type is a GNU extension
     result:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(TPACCInstance(Instance),astnkINTEGER,TPACCInstance(Instance).TypeULONG,RelevantSourceLocation,1);
    end else if Type_^.Size>0 then begin
     result:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(TPACCInstance(Instance),astnkINTEGER,TPACCInstance(Instance).TypeULONG,RelevantSourceLocation,Type_^.Size);
    end else begin
     result:=nil;
     AddError('Internal error 2017-01-02-03-05-0000',nil,true);
    end;
   end;
   TOK_ALIGNOF:begin
    NextToken;
    Expect(TOK_LPAR);
    Type_:=ParseCastType;
    Expect(TOK_RPAR);
    result:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(TPACCInstance(Instance),astnkINTEGER,TPACCInstance(Instance).TypeINT,RelevantSourceLocation,Type_^.Alignment);
   end;
   TOK_DEC:begin
    NextToken;
    result:=TPACCInstance(Instance).TypeConversion(ParseUnaryExpression);
    EnsureLValue(result);
    result:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkOP_PRE_DEC,result.Type_,RelevantSourceLocation,result);
   end;
   TOK_INC:begin
    NextToken;
    result:=TPACCInstance(Instance).TypeConversion(ParseUnaryExpression);
    EnsureLValue(result);
    result:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkOP_PRE_INC,result.Type_,RelevantSourceLocation,result);
   end;
   TOK_LAND:begin
    NextToken;
    if CurrentState.Token^.TokenType=TOK_IDENT then begin
     result:=TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress.Create(TPACCInstance(Instance),astnkOP_LABEL_ADDR,TPACCInstance(Instance).NewPointerType(TPACCInstance(Instance).TypeVOID),RelevantSourceLocation,nil,TPACCInstance(Instance).TokenSymbols[CurrentState.Token^.Index].Name);
     NextToken;
     UnresolvedLabelUsedNodes.Add(result);
    end else begin
     result:=nil;
     AddError('Label name expected after &&, but got "'+TPACCLexer(TPACCInstance(Instance).Lexer).TokenStrings[CurrentState.Token^.TokenType]+'"',nil,true);
    end;
   end;
   TOK_AND:begin
    NextToken;
    result:=ParseCastExpression;
    if result.Kind=astnkFUNCDESG then begin
     result:=TPACCInstance(Instance).TypeConversion(result);
    end else begin
     EnsureLValue(result);
     result:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkADDR,TPACCInstance(Instance).NewPointerType(result.Type_),RelevantSourceLocation,result);
    end;
   end;
   TOK_MUL:begin
    NextToken;
    result:=TPACCInstance(Instance).TypeConversion(ParseCastExpression);
    if result.Type_^.Kind<>tkPOINTER then begin
     AddError('Pointer type expected',nil,false);
    end else if result.Type_^.Kind<>tkFUNCTION then begin
     result:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkDEREF,result.Type_^.ChildType,RelevantSourceLocation,result);
    end;
   end;
   TOK_ADD:begin
    NextToken;
    result:=ParseCastExpression;
   end;
   TOK_SUB:begin
    NextToken;
    result:=TPACCInstance(Instance).TypeConversion(ParseCastExpression);
    EnsureArithmeticType(result);
    if TPACCInstance(Instance).IsIntType(result.Type_) then begin
     result:=BinaryOperation(astnkOP_SUB,TPACCInstance(Instance).TypeConversion(TPACCAbstractSyntaxTreeNodeIntegerValue.Create(TPACCInstance(Instance),astnkINTEGER,result.Type_,RelevantSourceLocation,0)),TPACCInstance(Instance).TypeConversion(result));
    end else begin
     result:=BinaryOperation(astnkOP_SUB,TPACCAbstractSyntaxTreeNodeFloatValue.Create(TPACCInstance(Instance),astnkINTEGER,result.Type_,RelevantSourceLocation,0.0),result);
    end;
   end;
   TOK_NOT:begin
    NextToken;
    result:=TPACCInstance(Instance).TypeConversion(ParseCastExpression);
    if TPACCInstance(Instance).IsIntType(result.Type_) then begin
     result:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkOP_NOT,result.Type_,RelevantSourceLocation,result);
    end else begin
     AddError('Integer type expected',nil,false);
    end;
   end;
   TOK_LNOT:begin
    NextToken;
    result:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkOP_LOG_NOT,TPACCInstance(Instance).TypeINT,RelevantSourceLocation,TPACCInstance(Instance).TypeConversion(ParseCastExpression));
   end;
   else begin
    result:=ParsePostfixExpression;
   end;
  end;
 end;
 procedure ParseInitializerList(const List:TPACCAbstractSyntaxTreeNodeList;const Type_:PPACCType;const Offset:TPACCInt;const Designated:boolean);
  procedure AssignString(const List:TPACCAbstractSyntaxTreeNodeList;const Type_:PPACCType;const p:TPACCRawByteString;const Offset:TPACCInt);
  var Index,Len:TPACCInt;
  begin
   Len:=length(p);
   if Type_^.ArrayLength<0 then begin
    Type_^.ArrayLength:=Len+1;
    Type_^.Size:=Len+1;
   end;
   Index:=0;
   while (Index<Type_^.ArrayLength) and (Index<Len) do begin
    List.Add(TPACCAbstractSyntaxTreeNodeInitializer.Create(TPACCInstance(Instance),astnkINIT,nil,CurrentState.Token^.SourceLocation,TPACCAbstractSyntaxTreeNodeIntegerValue.Create(TPACCInstance(Instance),astnkINTEGER,TPACCInstance(Instance).TypeCHAR,CurrentState.Token^.SourceLocation,TPACCUInt8(ansichar(p[Index+1]))),Offset+Index,TPACCInstance(Instance).TypeCHAR));
    inc(Index);
   end;
   while Index<Type_^.ArrayLength do begin
    List.Add(TPACCAbstractSyntaxTreeNodeInitializer.Create(TPACCInstance(Instance),astnkINIT,nil,CurrentState.Token^.SourceLocation,TPACCAbstractSyntaxTreeNodeIntegerValue.Create(TPACCInstance(Instance),astnkINTEGER,TPACCInstance(Instance).TypeCHAR,CurrentState.Token^.SourceLocation,0),Offset+Index,TPACCInstance(Instance).TypeCHAR));
    inc(Index);
   end;
  end;
  procedure SortInitializerList(const List:TPACCAbstractSyntaxTreeNodeList);
  var Index,Count:TPACCInt;
  begin
   Index:=0;
   Count:=List.Count;
   while (Index+1)<Count do begin
    if TPACCAbstractSyntaxTreeNodeInitializer(List[Index]).InitializionOffset>TPACCAbstractSyntaxTreeNodeInitializer(List[Index+1]).InitializionOffset then begin
     List.Exchange(Index,Index+1);
     if Index>0 then begin
      dec(Index);
     end else begin
      inc(Index);
     end;
    end else begin
     inc(Index);
    end;
   end;
  end;
  procedure SkipToClosedBrace;
  var IgnoredExpression:TPACCAbstractSyntaxTreeNode;
  begin
   repeat
    case CurrentState.Token^.TokenType of
     TOK_RBRA:begin
      NextToken;
      break;
     end;
     TOK_DOT:begin
      NextToken;
      NextToken;
      Expect(TOK_ASSIGN);
     end;
    end;
    IgnoredExpression:=ParseAssignmentExpression;
    if assigned(IgnoredExpression) then begin
     AddWarning('Excessive initializer');
     if CurrentState.Token^.TokenType=TOK_COMMA then begin
      NextToken;
     end;
    end else begin
     break;
    end;
   until false;
  end;
  procedure ParseInitializerElement(const List:TPACCAbstractSyntaxTreeNodeList;const Type_:PPACCType;const Offset:TPACCInt;Designated:boolean);
  var Expression:TPACCAbstractSyntaxTreeNode;
  begin
   Expect(TOK_ASSIGN);
   if Type_^.Kind in [tkARRAY,tkSTRUCT] then begin
    ParseInitializerList(List,Type_,Offset,Designated);
   end else if CurrentState.Token^.TokenType=TOK_LBRA Then begin
    NextToken;
    ParseInitializerElement(List,Type_,Offset,true);
    Expect(TOK_RBRA);
   end else begin
    Expression:=ParseAssignmentExpression;
    EnsureAssignable(Type_,Expression.Type_);
    List.Add(TPACCAbstractSyntaxTreeNodeInitializer.Create(TPACCInstance(Instance),astnkINIT,nil,Expression.SourceLocation,Expression,Offset,Type_));
   end;
  end;
  procedure ParseArrayInitializerList(const List:TPACCAbstractSyntaxTreeNodeList;const Type_:PPACCType;const Offset:TPACCInt;const Designated:boolean);
   procedure ParseArrayInitializers(const List:TPACCAbstractSyntaxTreeNodeList;const Type_:PPACCType;const Offset:TPACCInt;Designated:boolean);
   var Index,ElementSize,ExpressionIndex:TPACCInt;
       HasBrace,Flexible,SawClosedBrace:boolean;
   begin
    HasBrace:=CurrentState.Token^.TokenType=TOK_LBRA;
    if HasBrace then begin
     NextToken;
    end;
    Flexible:=Type_^.ArrayLength<0;
    ElementSize:=Type_^.ChildType^.Size;
    SawClosedBrace:=false;
    Index:=0;
    while Flexible and (Index<Type_^.ArrayLength) do begin
     if CurrentState.Token^.TokenType=TOK_RBRA then begin
      if HasBrace then begin
       NextToken;
       SawClosedBrace:=true;
       break;
      end;
     end;
     if (CurrentState.Token^.TokenType in [TOK_DOT,TOK_LBRK]) and (HasBrace or Designated) then begin
      exit;
     end;
     if CurrentState.Token^.TokenType=TOK_LBRK then begin
      NextToken;
      ExpressionIndex:=ParseIntegerExpression;
      if (ExpressionIndex<0) or ((Type_^.ArrayLength<=ExpressionIndex) and not Flexible) then begin
       AddError('Array designator exceeds array bounds: '+IntToStr(ExpressionIndex),nil,false);
      end;
      Expect(TOK_RBRK);
      Index:=ExpressionIndex;
     end;
     ParseInitializerElement(List,Type_^.ChildType,Offset+(TPACCInt64(ElementSize)*Index),Designated);
     if CurrentState.Token^.TokenType=TOK_COMMA then begin
      NextToken;
     end;
     Designated:=false;
     inc(Index);
    end;
    if HasBrace and not SawClosedBrace then begin
     SkipToClosedBrace;
    end;
    if Type_^.ArrayLength<0 then begin
     Type_^.ArrayLength:=Index;
     Type_^.Size:=Index*TPACCInt64(ElementSize);
    end;
   end;
  begin
   ParseArrayInitializers(List,Type_,Offset,Designated);
   SortInitializerList(List);
  end;
  procedure ParseStructInitializerList(const List:TPACCAbstractSyntaxTreeNodeList;const Type_:PPACCType;const Offset:TPACCInt;const Designated:boolean);
   procedure ParseStructInitializers(const List:TPACCAbstractSyntaxTreeNodeList;const Type_:PPACCType;const Offset:TPACCInt;Designated:boolean);
   var Index,FieldIndex:TPACCInt;
       HasBrace,SawClosedBrace:boolean;
       FieldName:TPACCRawByteString;
       FieldType:PPACCType;
       Keys:TPACCStructFields;
   begin
    HasBrace:=CurrentState.Token^.TokenType=TOK_LBRA;
    if HasBrace then begin
     NextToken;
    end;
    SawClosedBrace:=false;
    Keys:=Type_^.Fields;
    try
     Index:=0;
     repeat
      if CurrentState.Token^.TokenType=TOK_RBRA then begin
       if HasBrace then begin
        NextToken;
        SawClosedBrace:=true;
        break;
       end;
      end;
      if (CurrentState.Token^.TokenType in [TOK_DOT,TOK_LBRK]) and (HasBrace or Designated) then begin
       exit;
      end;
      if CurrentState.Token^.TokenType=TOK_DOT then begin
       NextToken;
       if CurrentState.Token^.TokenType=TOK_IDENT then begin
        FieldName:=TPACCInstance(Instance).TokenSymbols[CurrentState.Token^.Index].Name;
        NextToken;
       end else begin
        FieldName:='';
        AddError('Malformed desginated initializer',nil,true);
       end;
       FieldType:=nil;
       for FieldIndex:=0 to length(Type_.Fields)-1 do begin
        if Type_.Fields[FieldIndex].Name=FieldName then begin
         FieldType:=Type_.Fields[FieldIndex].Type_;
         break;
        end;
       end;
       if not assigned(FieldType) then begin
        AddError('Field does not exist: '+FieldName,nil,false);
       end;
       Keys:=Type_^.Fields;
       Index:=0;
       while Index<length(Keys) do begin
        if Keys[Index].Name=FieldName then begin
         break;
        end;
       end;
       Designated:=true;
      end else begin
       if Index<length(Keys) then begin
        FieldName:=Keys[Index].Name;
        FieldType:=Keys[Index].Type_;
        inc(Index);
       end else begin
        break;
       end;
      end;
      ParseInitializerElement(List,Type_^.ChildType,Offset+FieldType^.Offset,Designated);
      if CurrentState.Token^.TokenType=TOK_COMMA then begin
       NextToken;
      end;
      Designated:=false;
     until not (tfStruct in Type_^.Flags);
     if HasBrace and not SawClosedBrace then begin
      SkipToClosedBrace;
     end;
    finally
     Keys:=nil;
    end;
   end;
  begin
   ParseStructInitializers(List,Type_,Offset,Designated);
   SortInitializerList(List);
  end;
 var Encoding:TPACCEncoding;
 begin
  if TPACCInstance(Instance).IsStringType(Type_) then begin
   if CurrentState.Token^.TokenType=TOK_CSTR then begin
    AssignString(List,Type_,ParseStringValue(Encoding),Offset);
    exit;
   end else if (CurrentState.Token^.TokenType=TOK_LBRA) and (PeekTokenType(1)=TOK_CSTR) then begin
    NextToken;
    AssignString(List,Type_,ParseStringValue(Encoding),Offset);
    Expect(TOK_RBRA);
    exit;
   end;
  end;
  case Type_^.Kind of
   tkARRAY:begin
    ParseArrayInitializerList(List,Type_,Offset,Designated);
   end;
   tkSTRUCT:begin
    ParseStructInitializerList(List,Type_,Offset,Designated);
   end;
   else begin
    ParseArrayInitializerList(List,TPACCInstance(Instance).NewArrayType(Type_,1),Offset,Designated);
   end;
  end;
 end;
 function ParseDeclarationInitializer(const Type_:PPACCType):TPACCAbstractSyntaxTreeNodeList;
 var Node:TPACCAbstractSyntaxTreeNode;
 begin
  result:=TPACCAbstractSyntaxTreeNodeList.Create;
  TPACCInstance(Instance).AllocatedObjects.Add(result);
  if (CurrentState.Token^.TokenType=TOK_LBRA) or TPACCInstance(Instance).IsStringType(Type_) then begin
   ParseInitializerList(result,Type_,0,false);
  end else begin
   Node:=TPACCInstance(Instance).TypeConversion(ParseAssignmentExpression);
   if TPACCInstance(Instance).IsArithmeticType(Node.Type_) and (Node.Type_^.Kind<>Type_^.Kind) then begin
    Node:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkCONV,Type_,Node.SourceLocation,Node);
   end;
   if (Node.Type_^.Kind=tkPOINTER) and (Type_^.Kind=tkPOINTER) and not TPACCInstance(Instance).AreTypesEqual(Node.Type_,Type_) then begin
    AddWarning('Initialization from incompatible pointer type',@Node.SourceLocation);
   end;
   result.Add(TPACCAbstractSyntaxTreeNodeInitializer.Create(TPACCInstance(Instance),astnkINIT,nil,Node.SourceLocation,Node,0,Type_));
  end;
 end;
 function ParseCompoundLiteral(const Type_:PPACCType):TPACCAbstractSyntaxTreeNode;
 var Name:TPACCRawByteString;
 begin
  Name:='@TEMP@'+IntToStr(TPasMPInterlocked.Increment(TempVariableCounter));
  result:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable.Create(TPACCInstance(Instance),astnkLVAR,Type_,CurrentState.Token^.SourceLocation,Name,0,'');
  TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(result).LocalVariableInitialization:=ParseDeclarationInitializer(Type_);
  Assert(assigned(LocalScope));
  LocalScope[Name]:=result;
  if assigned(LocalVariables) then begin
   TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(result).Index:=LocalVariables.Add(result);
  end else begin
   TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(result).Index:=-1;
  end;
 end;
 function ParseCastExpression:TPACCAbstractSyntaxTreeNode;
 var Type_:PPACCType;
     RelevantSourceLocation:TPACCSourceLocation;
 begin
  if (CurrentState.Token^.TokenType=TOK_LPAR) and IsType(PeekToken(1)) then begin
   RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
   NextToken;
   Type_:=ParseCastType;
   Expect(TOK_RPAR);
   if CurrentState.Token^.TokenType=TOK_LBRA then begin
    result:=ParsePostfixExpressionTail(ParseCompoundLiteral(Type_));
   end else begin
    result:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkOP_CAST,Type_,RelevantSourceLocation,ParseCastExpression);
   end;
  end else begin
   result:=ParseUnaryExpression;
  end;
 end;
 function ParseMultiplicativeExpression:TPACCAbstractSyntaxTreeNode;
 begin
  result:=ParseCastExpression;
  repeat
   case CurrentState.Token^.TokenType of
    TOK_MUL:begin
     NextToken;
     result:=BinaryOperation(astnkOP_MUL,TPACCInstance(Instance).TypeConversion(result),TPACCInstance(Instance).TypeConversion(ParseCastExpression));
    end;
    TOK_DIV:begin
     NextToken;
     result:=BinaryOperation(astnkOP_DIV,TPACCInstance(Instance).TypeConversion(result),TPACCInstance(Instance).TypeConversion(ParseCastExpression));
    end;
    TOK_MOD:begin
     NextToken;
     result:=BinaryOperation(astnkOP_MOD,TPACCInstance(Instance).TypeConversion(result),TPACCInstance(Instance).TypeConversion(ParseCastExpression));
    end;
    else begin
     break;
    end;
   end;
  until false;
 end;
 function ParseAdditiveExpression:TPACCAbstractSyntaxTreeNode;
 begin
  result:=ParseMultiplicativeExpression;
  repeat
   case CurrentState.Token^.TokenType of
    TOK_ADD:begin
     NextToken;
     result:=BinaryOperation(astnkOP_ADD,TPACCInstance(Instance).TypeConversion(result),TPACCInstance(Instance).TypeConversion(ParseMultiplicativeExpression));
    end;
    TOK_SUB:begin
     NextToken;
     result:=BinaryOperation(astnkOP_SUB,TPACCInstance(Instance).TypeConversion(result),TPACCInstance(Instance).TypeConversion(ParseMultiplicativeExpression));
    end;
    else begin
     break;
    end;
   end;
  until false;
 end;
 function ParseShiftExpression:TPACCAbstractSyntaxTreeNode;
 var Op:TPACCAbstractSyntaxTreeNodeKind;
     Right:TPACCAbstractSyntaxTreeNode;
 begin
  result:=ParseAdditiveExpression;
  Op:=astnkNONE;
  repeat
   case CurrentState.Token^.TokenType of
    TOK_SHL:begin
     NextToken;
     Op:=astnkOP_SHL;
    end;
    TOK_SHR:begin
     NextToken;
     if tfUnsigned in result.Type_^.Flags then begin
      Op:=astnkOP_SHR;
     end else begin
      Op:=astnkOP_SAR;
     end;
    end;
    else begin
     break;
    end;
   end;
   Right:=ParseAdditiveExpression;
   EnsureIntType(result);
   EnsureIntType(Right);
   result:=BinaryOperation(Op,TPACCInstance(Instance).TypeConversion(result),TPACCInstance(Instance).TypeConversion(Right));
  until false;
 end;
 function ParseRelationalExpression:TPACCAbstractSyntaxTreeNode;
 begin
  result:=ParseShiftExpression;
  repeat
   case CurrentState.Token^.TokenType of
    TOK_LT:begin
     NextToken;
     result:=BinaryOperation(astnkOP_LT,TPACCInstance(Instance).TypeConversion(result),TPACCInstance(Instance).TypeConversion(ParseShiftExpression));
    end;
    TOK_GT:begin
     NextToken;
     result:=BinaryOperation(astnkOP_GT,TPACCInstance(Instance).TypeConversion(result),TPACCInstance(Instance).TypeConversion(ParseShiftExpression));
    end;
    TOK_LE:begin
     NextToken;
     result:=BinaryOperation(astnkOP_LE,TPACCInstance(Instance).TypeConversion(result),TPACCInstance(Instance).TypeConversion(ParseShiftExpression));
    end;
    TOK_GE:begin
     NextToken;
     result:=BinaryOperation(astnkOP_GE,TPACCInstance(Instance).TypeConversion(result),TPACCInstance(Instance).TypeConversion(ParseShiftExpression));
    end;
    else begin
     break;
    end;
   end;
   result.Type_:=TPACCInstance(Instance).TypeINT;
  until false;
 end;
 function ParseEqualityExpression:TPACCAbstractSyntaxTreeNode;
 begin
  result:=ParseRelationalExpression;
  case CurrentState.Token^.TokenType of
   TOK_EQ:begin
    NextToken;
   end;
   TOK_NE:begin
    NextToken;
    result:=BinaryOperation(astnkOP_NE,TPACCInstance(Instance).TypeConversion(result),TPACCInstance(Instance).TypeConversion(ParseRelationalExpression));
   end;
  end;
 end;
 function ParseBitAndExpression:TPACCAbstractSyntaxTreeNode;
 begin
  result:=ParseEqualityExpression;
  while CurrentState.Token^.TokenType=TOK_AND do begin
   NextToken;
   result:=BinaryOperation(astnkOP_AND,TPACCInstance(Instance).TypeConversion(result),TPACCInstance(Instance).TypeConversion(ParseEqualityExpression));
  end;
 end;
 function ParseBitXorExpression:TPACCAbstractSyntaxTreeNode;
 begin
  result:=ParseBitAndExpression;
  while CurrentState.Token^.TokenType=TOK_XOR do begin
   NextToken;
   result:=BinaryOperation(astnkOP_XOR,TPACCInstance(Instance).TypeConversion(result),TPACCInstance(Instance).TypeConversion(ParseBitAndExpression));
  end;
 end;
 function ParseBitOrExpression:TPACCAbstractSyntaxTreeNode;
 begin
  result:=ParseBitXorExpression;
  while CurrentState.Token^.TokenType=TOK_OR do begin
   NextToken;
   result:=BinaryOperation(astnkOP_OR,TPACCInstance(Instance).TypeConversion(result),TPACCInstance(Instance).TypeConversion(ParseBitXorExpression));
  end;
 end;
 function ParseLogicalAndExpression:TPACCAbstractSyntaxTreeNode;
 begin
  result:=ParseBitOrExpression;
  while CurrentState.Token^.TokenType=TOK_LAND do begin
   NextToken;
   result:=TPACCAbstractSyntaxTreeNodeBinaryOperator.Create(TPACCInstance(Instance),astnkOP_LOG_AND,TPACCInstance(Instance).TypeINT,result.SourceLocation,result,ParseBitOrExpression);
  end;
 end;
 function ParseLogicalOrExpression:TPACCAbstractSyntaxTreeNode;
 begin
  result:=ParseLogicalAndExpression;
  while CurrentState.Token^.TokenType=TOK_LOR do begin
   NextToken;
   result:=TPACCAbstractSyntaxTreeNodeBinaryOperator.Create(TPACCInstance(Instance),astnkOP_LOG_OR,TPACCInstance(Instance).TypeINT,result.SourceLocation,result,ParseLogicalAndExpression);
  end;
 end;
 function ParseCommaExpression:TPACCAbstractSyntaxTreeNode; forward;
 function ParseConditionalExpression:TPACCAbstractSyntaxTreeNode; forward;
 function ParseConditionalExpressionEx(const Condition:TPACCAbstractSyntaxTreeNode):TPACCAbstractSyntaxTreeNode;
 var Then_,Else_:TPACCAbstractSyntaxTreeNode;
     t,u,r:PPACCType;
 begin
  result:=Condition;
  if CurrentState.Token^.TokenType=TOK_QUEST then begin
   NextToken;
   Then_:=TPACCInstance(Instance).TypeConversion(ParseCommaExpression);
   Expect(TOK_COLON);
   Else_:=TPACCInstance(Instance).TypeConversion(ParseConditionalExpression);
   if assigned(Then_) then begin
    t:=Then_.Type_;
   end else begin
    t:=result.Type_;
   end;
   u:=Else_.Type_;
   // C11 6.5.15p5: if both types are arithemtic type, the result type is the result of the usual arithmetic conversions.
   if TPACCInstance(Instance).IsArithmeticType(t) and TPACCInstance(Instance).IsArithmeticType(u) then begin
    r:=TPACCInstance(Instance).UsualArithmeticConversion(t,u);
    if assigned(Then_) then begin
     result:=TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator.Create(TPACCInstance(Instance),astnkTERNARY,r,result.SourceLocation,result,Wrap(r,Then_),Wrap(r,Else_));
    end else begin
     result:=TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator.Create(TPACCInstance(Instance),astnkTERNARY,r,result.SourceLocation,result,nil,Wrap(r,Else_));
    end;
   end else begin
    result:=TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator.Create(TPACCInstance(Instance),astnkTERNARY,u,result.SourceLocation,result,Then_,Else_);
   end;
  end;
 end;
 function ParseConditionalExpression:TPACCAbstractSyntaxTreeNode;
 begin
  result:=ParseConditionalExpressionEx(ParseLogicalOrExpression);
 end;
 function ParseAssignmentExpression:TPACCAbstractSyntaxTreeNode;
 var Op:TPACCAbstractSyntaxTreeNodeKind;
     Value,Right:TPACCAbstractSyntaxTreeNode;
 begin
  result:=ParseLogicalOrExpression;
  case CurrentState.Token^.TokenType of
   TOK_QUEST:begin
    result:=ParseConditionalExpressionEx(result);
   end;
   TOK_ASSIGN,TOK_A_ADD,TOK_A_SUB,TOK_A_MUL,TOK_A_DIV,TOK_A_MOD,TOK_A_AND,TOK_A_OR,TOK_A_XOR,TOK_A_SHL,TOK_A_SHR:begin
    case CurrentState.Token^.TokenType of
     TOK_A_ADD:begin
      Op:=astnkOP_A_ADD;
     end;
     TOK_A_SUB:begin
      Op:=astnkOP_A_SUB;
     end;
     TOK_A_MUL:begin
      Op:=astnkOP_A_MUL;
     end;
     TOK_A_DIV:begin
      Op:=astnkOP_A_DIV;
     end;
     TOK_A_MOD:begin
      Op:=astnkOP_A_MOD;
     end;
     TOK_A_AND:begin
      Op:=astnkOP_A_AND;
     end;
     TOK_A_OR:begin
      Op:=astnkOP_A_OR;
     end;
     TOK_A_XOR:begin
      Op:=astnkOP_A_XOR;
     end;
     TOK_A_SHL:begin
      Op:=astnkOP_A_SHL;
     end;
     TOK_A_SHR:begin
      if tfUnsigned in result.Type_^.Flags then begin
       Op:=astnkOP_A_SHR;
      end else begin
       Op:=astnkOP_A_SAR;
      end;
     end;
     else begin
      Op:=astnkOP_ASSIGN;
     end;
    end;
    NextToken;
    Value:=TPACCInstance(Instance).TypeConversion(ParseAssignmentExpression);
    EnsureLValue(result);
    if Op=astnkOP_ASSIGN then begin
     Right:=Value;
    end else begin
     Right:=BinaryOperation(Op,TPACCInstance(Instance).TypeConversion(result),Value);
    end;
    if TPACCInstance(Instance).IsArithmeticType(result.Type_) and (result.Type_.Kind<>Right.Type_.Kind) then begin
     Right:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkCONV,result.Type_,Right.SourceLocation,Right);
    end;
    result:=TPACCAbstractSyntaxTreeNodeBinaryOperator.Create(TPACCInstance(Instance),astnkOP_ASSIGN,result.Type_,result.SourceLocation,result,Right);
   end;
  end;
 end;
 function ParseCommaExpression:TPACCAbstractSyntaxTreeNode;
 var Expression:TPACCAbstractSyntaxTreeNode;
 begin
  result:=ParseAssignmentExpression;
  while CurrentState.Token^.TokenType=TOK_COMMA do begin
   NextToken;
   Expression:=ParseAssignmentExpression;
   result:=TPACCAbstractSyntaxTreeNodeBinaryOperator.Create(TPACCInstance(Instance),astnkOP_COMMA,Expression.Type_,result.SourceLocation,result,Expression);
  end;
 end;
 function ParseExpressionOptional:TPACCAbstractSyntaxTreeNode;
 begin
  result:=ParseCommaExpression;
 end;
 function ParseExpression:TPACCAbstractSyntaxTreeNode;
 begin
  result:=ParseCommaExpression;
  if not assigned(result) then begin
   AddError('Expression expected',nil,true);
  end;
 end;
 function ParseIntegerExpression:TPACCInt64;
 begin
  result:=TPACCInstance(Instance).EvaluateIntegerExpression(ParseConditionalExpression,nil);
 end;
 function ParseFunctionDefinition:pointer;
 begin
  result:=nil;
 end;
 function ParseDeclaratorSpecifier(ResultStorageClass:PPACCInt32;var Attribute:TPACCAttribute):PPACCType; forward;
 function ParseDeclarator(out Name:TPACCRawByteString;const BaseType:PPACCType;const Parameters:TPACCAbstractSyntaxTreeNodeList;Context:TPACCInt32):PPACCType;
  function ParseDeclaratorFunction(const BaseType:PPACCType;const Parameters:TPACCAbstractSyntaxTreeNodeList):PPACCType;
   function ParseDeclaratorFunctionParameterList(const ParameterVariables:TPACCAbstractSyntaxTreeNodeList;const ReturnType:PPACCType):PPACCType;
    function ParseFunctionParameter(out Name:TPACCRawByteString;Optional:boolean):PPACCType;
    var StorageClass,CallingConvention:TPACCInt32;
        BaseType:PPACCType;
        Attribute:TPACCAttribute;
    begin
     StorageClass:=0;
     Attribute:=PACCEmptyAttribute;
     BaseType:=TPACCInstance(Instance).TypeINT;
     if IsType(CurrentState.Token) then begin
      BaseType:=ParseDeclaratorSpecifier(@StorageClass,Attribute);
     end else if Optional then begin
      AddError('Type expected, but got "'+TPACCLexer(TPACCInstance(Instance).Lexer).TokenStrings[CurrentState.Token^.TokenType]+'"',nil,true);
     end;
     if Optional then begin
      result:=ParseDeclarator(Name,BaseType,nil,DECL_PARAM_TYPEONLY);
     end else begin
      result:=ParseDeclarator(Name,BaseType,nil,DECL_PARAM);
     end;
     result^.Attribute:=Attribute;
     case result^.Kind of
      tkARRAY:begin
       // C11 6.7.6.3p7: Array of T is adjusted to pointer to T in a function parameter list.
       result:=TPACCInstance(Instance).NewPointerType(result^.ChildType);
      end;
      tkFUNCTION:begin
       // C11 6.7.6.3p8: Function is adjusted to pointer to function in a function parameter list.
       result:=TPACCInstance(Instance).NewPointerType(result);
      end;
      else begin
       // We must do nothing in this case
      end;
     end;
    end;
    procedure ParseDeclaratorParameters(var ParameterTypes:TPPACCTypes;const ParameterVariables:TPACCAbstractSyntaxTreeNodeList;out Ellipsis:boolean);
    var TypeOnly:boolean;
        Count,Index:TPACCInt32;
        Name:TPACCRawByteString;
        CurrentType:PPACCType;
        RelevantSourceLocation:TPACCSourceLocation;
        Variable:TPACCAbstractSyntaxTreeNode;
    begin
     TypeOnly:=not assigned(ParameterVariables);
     Count:=length(ParameterTypes);
     repeat
      RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
      if CurrentState.Token^.TokenType=TOK_ELLIPSIS then begin
       NextToken;
       if Count=0 then begin
        AddError('At least one parameter is required before "..."',nil,false);
       end;
       Expect(TOK_RPAR);
       Ellipsis:=true;
       break;
      end else begin
       CurrentType:=ParseFunctionParameter(Name,TypeOnly);
       EnsureNotVoid(CurrentType);
       Index:=Count;
       inc(Count);
       if length(ParameterTypes)<Count then begin
        SetLength(ParameterTypes,Count*2);
       end;
       ParameterTypes[Index]:=CurrentType;
       if not TypeOnly then begin
        Variable:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable.Create(TPACCInstance(Instance),astnkLVAR,CurrentType,RelevantSourceLocation,Name,0,'');
        ParameterVariables.Add(Variable);
       end;
       case CurrentState.Token^.TokenType of
        TOK_RPAR:begin
         NextToken;
         break;
        end;
        TOK_COMMA:begin
         NextToken;
        end;
        else begin
         AddError('")" or "," expected, but got "'+TPACCLexer(TPACCInstance(Instance).Lexer).TokenStrings[CurrentState.Token^.TokenType]+'"',nil,true);
        end;
       end;
      end;
     until false;
     SetLength(ParameterTypes,Count);
    end;
    procedure ParseDeclaratorParametersOldStyle(const ParameterVariables:TPACCAbstractSyntaxTreeNodeList);
    var Name:TPACCRawByteString;
    begin
     repeat
      if CurrentState.Token^.TokenType=TOK_IDENT then begin
       Name:=TPACCInstance(Instance).TokenSymbols[CurrentState.Token^.Index].Name;
       ParameterVariables.Add(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable.Create(TPACCInstance(Instance),astnkLVAR,TPACCInstance(Instance).TypeINT,CurrentState.Token^.SourceLocation,Name,0,''));
       NextToken;
       case CurrentState.Token^.TokenType of
        TOK_RPAR:begin
         NextToken;
         break;
        end;
        TOK_COMMA:begin
         NextToken;
        end;
        else begin
         AddError('")" or "," expected, but got "'+TPACCLexer(TPACCInstance(Instance).Lexer).TokenStrings[CurrentState.Token^.TokenType]+'"',nil,true);
        end;
       end;
      end else begin
       AddError('Identifier expected, but got "'+TPACCLexer(TPACCInstance(Instance).Lexer).TokenStrings[CurrentState.Token^.TokenType]+'"',nil,true);
      end;
     until false;
    end;
   var ParameterTypes:TPPACCTypes;
       Ellipsis:boolean;
   begin
    if (CurrentState.Token^.TokenType=TOK_VOID) and (PeekTokenType(1)=TOK_RPAR) then begin
     // C11 6.7.6.3p10: A parameter list with just "void" specifies that the function has no parameters.
     NextToken;
     NextToken;
     result:=TPACCInstance(Instance).NewFunctionType(ReturnType,nil,false,false);
    end else begin
     case CurrentState.Token^.TokenType of
      TOK_RPAR:begin
       // C11 6.7.6.3p14: K&R-style un-prototyped declaration or function definition having no parameters.
       NextToken;
       result:=TPACCInstance(Instance).NewFunctionType(ReturnType,nil,true,true);
      end;
      TOK_ELLIPSIS:begin
       result:=nil;
       AddError('At least one parameter is required before "..."',nil,false);
      end;
      else begin
       if IsType(CurrentState.Token) then begin
        ParameterTypes:=nil;
        try
         Ellipsis:=false;
         ParseDeclaratorParameters(ParameterTypes,ParameterVariables,Ellipsis);
         result:=TPACCInstance(Instance).NewFunctionType(ReturnType,ParameterTypes,Ellipsis,false);
        finally
         ParameterTypes:=nil;
        end;
       end else begin
        if assigned(ParameterVariables) then begin
         ParseDeclaratorParametersOldstyle(ParameterVariables);
         ParameterTypes:=nil;
         try
          SetLength(ParameterTypes,ParameterVariables.Count);
          result:=TPACCInstance(Instance).NewFunctionType(ReturnType,ParameterTypes,false,true);
         finally
          ParameterTypes:=nil;
         end;
        end else begin
         result:=nil;
         AddError('Invalid function definition',nil,true);
        end;
       end;
      end;
     end;
    end;
   end;
  begin
   case BaseType^.Kind of
    tkFUNCTION:begin
     result:=nil;
     AddError('Function returning a function',nil,true);
    end;
    tkARRAY:begin
     result:=nil;
     AddError('Function returning an array',nil,true);
    end;
    else begin
     result:=ParseDeclaratorFunctionParameterList(Parameters,BaseType);
    end;
   end;
  end;
  function ParseDeclaratorTail(const BaseType:PPACCType;const Parameters:TPACCAbstractSyntaxTreeNodeList):PPACCType; forward;
  function ParseDeclaratorArray(const BaseType:PPACCType):PPACCType;
  var Len:TPACCInt64;
  begin
   if CurrentState.Token^.TokenType=TOK_RBRK then begin
    NextToken;
    Len:=-1;
   end else begin
    Len:=ParseIntegerExpression;
    Expect(TOK_RBRK);
   end;
   result:=ParseDeclaratorTail(BaseType,nil);
   if assigned(result) and (result^.Kind=tKFUNCTION) then begin
    AddError('Array of functions',nil,false);
   end;
   result:=TPACCInstance(Instance).NewArrayType(result,Len);
  end;
  function ParseDeclaratorTail(const BaseType:PPACCType;const Parameters:TPACCAbstractSyntaxTreeNodeList):PPACCType;
  begin
   case CurrentState.Token^.TokenType of
    TOK_LPAR:begin
     NextToken;
     result:=ParseDeclaratorFunction(BaseType,Parameters);
    end;
    TOK_LBRK:begin
     NextToken;
     result:=ParseDeclaratorArray(BaseType);
    end;
    else begin
     result:=BaseType;
    end;
   end;
  end;
 var Stub:PPACCType;
 begin
  case CurrentState.Token^.TokenType of
   TOK_LPAR:begin
    NextToken;
    if IsType(CurrentState.Token) then begin
     result:=ParseDeclaratorFunction(BaseType,Parameters);
    end else begin
     Stub:=TPACCInstance(Instance).NewStubType;
     TPACCInstance(Instance).CopyFromTypeToType(BaseType,Stub);
     result:=ParseDeclarator(Name,Stub,Parameters,Context);
     Expect(TOK_RPAR);
     TPACCInstance(Instance).CopyFromTypeToType(ParseDeclaratorTail(BaseType,Parameters),Stub);
    end;
   end;
   TOK_MUL:begin
    NextToken;
    SkipTypeQualifiers;
    result:=ParseDeclarator(Name,TPACCInstance(Instance).NewPointerType(BaseType),Parameters,Context);
   end;
   else begin
    if CurrentState.Token^.TokenType=TOK_IDENT then begin
     if Context in [DECL_CAST] then begin
      result:=nil;
      AddError('Identifier is not expected, but got "'+TPACCLexer(TPACCInstance(Instance).Lexer).TokenStrings[CurrentState.Token^.TokenType]+'"',nil,true);
     end else begin
      Name:=TPACCInstance(Instance).TokenSymbols[CurrentState.Token^.Index].Name;
      NextToken;
      result:=ParseDeclaratorTail(BaseType,Parameters);
     end;
    end else if Context in [DECL_BODY,DECL_PARAM] then begin
     result:=nil;
     AddError('Identifier, "(" or "*" are expected, but got "'+TPACCLexer(TPACCInstance(Instance).Lexer).TokenStrings[CurrentState.Token^.TokenType]+'"',nil,true);
    end else begin
     result:=ParseDeclaratorTail(BaseType,Parameters);
    end;
   end;
  end;
 end;
 function ParseAbstractDeclarator(const BaseType:PPACCType):PPACCType;
 var n:TPACCRawByteString;
 begin
  result:=ParseDeclarator(n,BaseType,nil,DECL_CAST);
 end;
 function ParseCastType:PPACCType;
 var Attribute:TPACCAttribute;
 begin
  Attribute:=PACCEmptyAttribute;
  result:=ParseAbstractDeclarator(ParseDeclaratorSpecifier(nil,Attribute));
  result^.Attribute:=Attribute;
 end;
 procedure ParseStaticAssert;
 var Value:TPACCInt64;
     Msg:TPACCRawByteString;
     Encoding:TPACCEncoding;
 begin
  Expect(TOK_LPAR);
  Value:=ParseIntegerExpression;
  Expect(TOK_COMMA);
  if CurrentState.Token^.TokenType=TOK_CSTR then begin
   Msg:=ParseStringValue(Encoding);
  end else begin
   AddError('string expected as the second argument for _Static_assert, but got "'+TPACCLexer(TPACCInstance(Instance).Lexer).TokenStrings[CurrentState.Token^.TokenType]+'"',nil,true);
   Msg:='';
  end;
  Expect(TOK_RPAR);
  Expect(TOK_SEMICOLON);
  if Value=0 then begin
   AddError('_Static_assert failure: '+IntToStr(Value)+' '+Msg,nil,true);
  end;
 end;
 procedure ParseDeclarationOrStatement(const List:TPACCAbstractSyntaxTreeNodeList); forward;
 function ParseStatement:TPACCAbstractSyntaxTreeNode; forward;
 function ParseBooleanExpression:TPACCAbstractSyntaxTreeNode;
 begin
  result:=ParseExpression;
  if TPACCInstance(Instance).IsFloatType(result.Type_) then begin
   result:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkCONV,TPACCInstance(Instance).TypeBOOL,result.SourceLocation,result);
  end;
 end;
 function ParseAssemblerStatement(const IsGlobal:boolean):TPACCAbstractSyntaxTreeNode;
  procedure ParseAssemblerOperands(const IsOutput:boolean;const Operands:TPACCAbstractSyntaxTreeNodeList);
  var Expression:TPACCAbstractSyntaxTreeNode;
      Identifier,Constraint:TPACCRawByteString;
      Operand:TPACCAbstractSyntaxTreeNodeAssemblerOperand;
      RelevantSourceLocation:TPACCSourceLocation;
      Encoding:TPACCEncoding;
  begin
   if CurrentState.Token^.TokenType<>TOK_COLON then begin
    repeat
     RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
     Identifier:='';
     if CurrentState.Token^.TokenType=TOK_LBRK then begin
      NextToken;
      if CurrentState.Token^.TokenType=TOK_IDENT then begin
       Identifier:=TPACCInstance(Instance).TokenSymbols[CurrentState.Token^.Index].Name;
       NextToken;
      end else begin
       AddError('Identifier expected',@CurrentState.Token^.SourceLocation,true);
      end;
      Expect(TOK_RBRK);
     end;
     if CurrentState.Token^.TokenType=TOK_CSTR then begin
      Constraint:=ParseStringValue(Encoding);
      Expect(TOK_LPAR);
      Expression:=ParseExpression;
      Expect(TOK_RPAR);
      if IsOutput then begin
       EnsureLValue(Expression);
      end else begin
       if pos('m',Constraint)=0 then begin
        Expression:=TPACCInstance(Instance).TypeConversion(Expression);
       end;
      end;
      Operand:=TPACCAbstractSyntaxTreeNodeAssemblerOperand.Create(TPACCInstance(Instance),astnkASSEMBLER_OPERAND,nil,RelevantSourceLocation,Identifier,Constraint,Expression);
      Operands.Add(Operand);
      if CurrentState.Token^.TokenType=TOK_COMMA then begin
       NextToken;
       continue;
      end else begin
       break;
      end;
     end else begin
      AddError('String constant expected',@CurrentState.Token^.SourceLocation,true);
     end;
    until false;
   end;
  end;
  procedure ParseClobbers(const Clobbers:TStringList);
  var RelevantSourceLocation:TPACCSourceLocation;
      Encoding:TPACCEncoding;
  begin
   if CurrentState.Token^.TokenType<>TOK_COLON then begin
    repeat
     RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
     if CurrentState.Token^.TokenType=TOK_CSTR then begin
      Clobbers.Add(ParseStringValue(Encoding));
      if CurrentState.Token^.TokenType=TOK_COMMA then begin
       NextToken;
       continue;
      end else begin
       break;
      end;
     end else begin
      AddError('String constant expected',@RelevantSourceLocation,true);
     end;
    until false;
   end;
  end;
  procedure ParseLabels(const Gotos:TPACCAbstractSyntaxTreeNodeList);
  var RelevantSourceLocation:TPACCSourceLocation;
      LabelName:TPACCRawByteString;
  begin
   if CurrentState.Token^.TokenType<>TOK_COLON then begin
    repeat
     RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
     if CurrentState.Token^.TokenType=TOK_IDENT then begin
      LabelName:=TPACCInstance(Instance).TokenSymbols[CurrentState.Token^.Index].Name;
      Gotos.Add(TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress.Create(TPACCInstance(Instance),astnkGOTO,nil,RelevantSourceLocation,nil,LabelName));
      if assigned(UnresolvedLabelUsedNodes) then begin
       UnresolvedLabelUsedNodes.Add(result);
      end else begin
       AddError('Internal error 2017-01-09-07-50-0000',nil,true);
      end;
      NextToken;
      if CurrentState.Token^.TokenType=TOK_COMMA then begin
       NextToken;
       continue;
      end else begin
       break;
      end;
     end else begin
      AddError('String constant expected',@CurrentState.Token^.SourceLocation,true);
     end;
    until false;
   end;
  end;
 var Code:TPACCRawByteString;
     RelevantSourceLocation:TPACCSourceLocation;
     IsVolatile,WithGotos:boolean;
     Encoding:TPACCEncoding;
 begin
  RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
  IsVolatile:=CurrentState.Token^.TokenType=TOK_VOLATILE;
  if IsVolatile then begin
   NextToken;
  end;
  WithGotos:=CurrentState.Token^.TokenType=TOK_GOTO;
  if WithGotos then begin
   NextToken;
  end;
  Expect(TOK_LPAR);
  if CurrentState.Token^.TokenType=TOK_CSTR then begin
   Code:=ParseStringValue(Encoding);
   result:=TPACCAbstractSyntaxTreeNodeAssembler.Create(TPACCInstance(Instance),astnkASSEMBLER,nil,RelevantSourceLocation,IsGlobal,IsVolatile,WithGotos,Code);
   TPACCAbstractSyntaxTreeNodeAssembler(result).InputOperands:=TPACCAbstractSyntaxTreeNodeList.Create;
   TPACCInstance(Instance).AllocatedObjects.Add(TPACCAbstractSyntaxTreeNodeAssembler(result).InputOperands);
   TPACCAbstractSyntaxTreeNodeAssembler(result).OutputOperands:=TPACCAbstractSyntaxTreeNodeList.Create;
   TPACCInstance(Instance).AllocatedObjects.Add(TPACCAbstractSyntaxTreeNodeAssembler(result).OutputOperands);
   TPACCAbstractSyntaxTreeNodeAssembler(result).Gotos:=TPACCAbstractSyntaxTreeNodeList.Create;
   TPACCInstance(Instance).AllocatedObjects.Add(TPACCAbstractSyntaxTreeNodeAssembler(result).Gotos);
   if CurrentState.Token^.TokenType=TOK_COLON then begin
    NextToken;
    ParseAssemblerOperands(true,TPACCAbstractSyntaxTreeNodeAssembler(result).OutputOperands);
    if CurrentState.Token^.TokenType=TOK_COLON then begin
     NextToken;
     ParseAssemblerOperands(false,TPACCAbstractSyntaxTreeNodeAssembler(result).InputOperands);
     if CurrentState.Token^.TokenType=TOK_COLON then begin
      NextToken;
      ParseClobbers(TPACCAbstractSyntaxTreeNodeAssembler(result).Clobbers);
      if WithGotos then begin
       Expect(TOK_COLON);
       ParseLabels(TPACCAbstractSyntaxTreeNodeAssembler(result).Gotos);
      end;
     end else if WithGotos then begin
      Expect(TOK_COLON);
     end;
    end else if WithGotos then begin
     Expect(TOK_COLON);
    end;
   end else if WithGotos then begin
    Expect(TOK_COLON);
   end;
   Expect(TOK_RPAR);
   if CurrentState.Token^.TokenType=TOK_SEMICOLON then begin
    Expect(TOK_SEMICOLON);
   end;
  end else begin
   result:=nil;
   AddError('Assembler code expected',@RelevantSourceLocation,false);
  end;
 end;
 function ParseIFStatement:TPACCAbstractSyntaxTreeNode;
 var ConditionNode,ThenNode,ElseNode:TPACCAbstractSyntaxTreeNode;
 begin
  Expect(TOK_LPAR);
  ConditionNode:=ParseBooleanExpression;
  Expect(TOK_RPAR);
  ThenNode:=ParseStatement;
  if CurrentState.Token^.TokenType=TOK_ELSE then begin
   NextToken;
   ElseNode:=ParseStatement;
  end else begin
   ElseNode:=nil;
  end;
  result:=TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator.Create(TPACCInstance(Instance),astnkIF,nil,ConditionNode.SourceLocation,ConditionNode,ThenNode,ElseNode);
 end;
 function ParseFORStatement:TPACCAbstractSyntaxTreeNode;
 var LoopBreakLabel,LoopContinueLabel,OldBreakLabel,OldContinueLabel:TPACCAbstractSyntaxTreeNodeLabel;
     OriginalLocalScope:TPACCRawByteStringHashMap;
     InitializationNode,ConditionNode,StepNode,BodyNode:TPACCAbstractSyntaxTreeNode;
     RelevantSourceLocation:TPACCSourceLocation;
 begin
  RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
  OriginalLocalScope:=LocalScope;
  LocalScope:=nil;
  try
   LocalScope:=TPACCRawByteStringHashMap.Create;
   LocalScope.Parent:=OriginalLocalScope;
   Expect(TOK_LPAR);
   if CurrentState.Token^.TokenType=TOK_SEMICOLON then begin
    InitializationNode:=nil;
    NextToken;
   end else begin
    InitializationNode:=TPACCAbstractSyntaxTreeNodeStatements.Create(TPACCInstance(Instance),astnkSTATEMENTS,nil,CurrentState.Token^.SourceLocation);
    ParseDeclarationOrStatement(TPACCAbstractSyntaxTreeNodeStatements(InitializationNode).Children);
   end;
   if CurrentState.Token^.TokenType=TOK_SEMICOLON then begin
    ConditionNode:=nil;
    NextToken;
   end else begin
    ConditionNode:=ParseExpressionOptional;
    if assigned(ConditionNode) and TPACCInstance(Instance).IsFloatType(ConditionNode.Type_) then begin
     ConditionNode:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkCONV,TPACCInstance(Instance).TypeBOOL,ConditionNode.SourceLocation,ConditionNode);
    end;
    Expect(TOK_SEMICOLON);
   end;
   if CurrentState.Token^.TokenType<>TOK_RPAR then begin
    StepNode:=ParseExpressionOptional;
   end else begin
    StepNode:=nil;
   end;
   Expect(TOK_RPAR);
   OldBreakLabel:=BreakLabel;
   OldContinueLabel:=ContinueLabel;
   try
    LoopBreakLabel:=NewLabel(astnkHIDDEN_LABEL,RelevantSourceLocation);
    LoopContinueLabel:=NewLabel(astnkHIDDEN_LABEL,RelevantSourceLocation);
    BreakLabel:=LoopBreakLabel;
    ContinueLabel:=LoopContinueLabel;
    BodyNode:=ParseStatement;
   finally
    BreakLabel:=OldBreakLabel;
    ContinueLabel:=OldContinueLabel;
   end;
  finally
   FreeAndNil(LocalScope);
   LocalScope:=OriginalLocalScope;
  end;
  result:=TPACCAbstractSyntaxTreeNodeFORStatement.Create(TPACCInstance(Instance),astnkFOR,nil,RelevantSourceLocation,InitializationNode,ConditionNode,StepNode,BodyNode,LoopBreakLabel,LoopContinueLabel);
 end;
 function ParseWHILEStatement:TPACCAbstractSyntaxTreeNode;
 var LoopBreakLabel,LoopContinueLabel,OldBreakLabel,OldContinueLabel:TPACCAbstractSyntaxTreeNodeLabel;
     OriginalLocalScope:TPACCRawByteStringHashMap;
     ConditionNode,BodyNode:TPACCAbstractSyntaxTreeNode;
     RelevantSourceLocation:TPACCSourceLocation;
 begin
  RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
  Expect(TOK_LPAR);
  ConditionNode:=ParseBooleanExpression;
  Expect(TOK_RPAR);
  OriginalLocalScope:=LocalScope;
  LocalScope:=nil;
  try
   LocalScope:=TPACCRawByteStringHashMap.Create;
   LocalScope.Parent:=OriginalLocalScope;
   OldBreakLabel:=BreakLabel;
   OldContinueLabel:=ContinueLabel;
   try
    LoopBreakLabel:=NewLabel(astnkHIDDEN_LABEL,RelevantSourceLocation);
    LoopContinueLabel:=NewLabel(astnkHIDDEN_LABEL,RelevantSourceLocation);
    BreakLabel:=LoopBreakLabel;
    ContinueLabel:=LoopContinueLabel;
    BodyNode:=ParseStatement;
   finally
    BreakLabel:=OldBreakLabel;
    ContinueLabel:=OldContinueLabel;
   end;
  finally
   FreeAndNil(LocalScope);
   LocalScope:=OriginalLocalScope;
  end;
  result:=TPACCAbstractSyntaxTreeNodeWHILEOrDOStatement.Create(TPACCInstance(Instance),astnkWHILE,nil,RelevantSourceLocation,ConditionNode,BodyNode,LoopBreakLabel,LoopContinueLabel);
 end;
 function ParseDOStatement:TPACCAbstractSyntaxTreeNode;
 var LoopBreakLabel,LoopContinueLabel,OldBreakLabel,OldContinueLabel:TPACCAbstractSyntaxTreeNodeLabel;
     OriginalLocalScope:TPACCRawByteStringHashMap;
     ConditionNode,BodyNode:TPACCAbstractSyntaxTreeNode;
     RelevantSourceLocation:TPACCSourceLocation;
 begin
  RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
  OriginalLocalScope:=LocalScope;
  LocalScope:=nil;
  try
   LocalScope:=TPACCRawByteStringHashMap.Create;
   LocalScope.Parent:=OriginalLocalScope;
   OldBreakLabel:=BreakLabel;
   OldContinueLabel:=ContinueLabel;
   try
    LoopBreakLabel:=NewLabel(astnkHIDDEN_LABEL,RelevantSourceLocation);
    LoopContinueLabel:=NewLabel(astnkHIDDEN_LABEL,RelevantSourceLocation);
    BreakLabel:=LoopBreakLabel;
    ContinueLabel:=LoopContinueLabel;
    BodyNode:=ParseStatement;
   finally
    BreakLabel:=OldBreakLabel;
    ContinueLabel:=OldContinueLabel;
   end;
  finally
   FreeAndNil(LocalScope);
   LocalScope:=OriginalLocalScope;
  end;
  Expect(TOK_WHILE);
  Expect(TOK_LPAR);
  ConditionNode:=ParseBooleanExpression;
  Expect(TOK_RPAR);
  Expect(TOK_SEMICOLON);
  result:=TPACCAbstractSyntaxTreeNodeWHILEOrDOStatement.Create(TPACCInstance(Instance),astnkDO,nil,RelevantSourceLocation,ConditionNode,BodyNode,LoopBreakLabel,LoopContinueLabel);
 end;
 function ParseRETURNStatement:TPACCAbstractSyntaxTreeNode;
 var RelevantSourceLocation:TPACCSourceLocation;
 begin
  RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
  if CurrentState.Token^.TokenType<>TOK_SEMICOLON then begin
   result:=ParseExpressionOptional;
  end else begin
   result:=nil;
  end;
  Expect(TOK_SEMICOLON);
  if assigned(result) then begin
   result:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkCONV,CurrentFunctionType^.ReturnType,result.SourceLocation,result);
  end;
  result:=TPACCAbstractSyntaxTreeNodeReturnStatement.Create(TPACCInstance(Instance),astnkRETURN,nil,RelevantSourceLocation,result);
 end;
 function ParseSWITCHStatement:TPACCAbstractSyntaxTreeNode;
 var Index:TPACCInt;
     ExpressionNode,BodyNode:TPACCAbstractSyntaxTreeNode;
     OldCases:TCases;
     CurrentCase:PCase;
     SwitchCases:TPACCAbstractSyntaxTreeNodeSWITCHStatementCases;
     SwitchCase:PPACCAbstractSyntaxTreeNodeSWITCHStatementCase;
     OldBreakLabel,SwitchBreakLabel:TPACCAbstractSyntaxTreeNodeLabel;
     RelevantSourceLocation:TPACCSourceLocation;
 begin
  RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
  SwitchCases:=nil;
  Expect(TOK_LPAR);
  ExpressionNode:=TPACCInstance(Instance).TypeConversion(ParseExpression);
  EnsureIntType(ExpressionNode);
  Expect(TOK_RPAR);
  OldCases:=Cases;
  OldBreakLabel:=BreakLabel;
  try
   Cases.Ready:=true;
   Cases.Cases:=nil;
   Cases.Count:=0;
   Cases.DefaultCaseLabel:=nil;
   SwitchBreakLabel:=NewLabel(astnkHIDDEN_LABEL,RelevantSourceLocation);
   BreakLabel:=SwitchBreakLabel;
   BodyNode:=ParseStatement;
   SetLength(SwitchCases,Cases.Count);
   for Index:=0 to Cases.Count-1 do begin
    SwitchCase:=@SwitchCases[Index];
    CurrentCase:=@Cases.Cases[Index];
    SwitchCase^.CaseBegin:=CurrentCase^.Begin_;
    SwitchCase^.CaseEnd:=CurrentCase^.End_;
    SwitchCase^.CaseLabel:=CurrentCase^.Label_;
   end;
   if length(SwitchCases)>1 then begin
    UntypedDirectIntroSort(@SwitchCases[0],0,length(SwitchCases)-1,sizeof(TPACCAbstractSyntaxTreeNodeSWITCHStatementCase),CompareSwitchCases);
   end;
   result:=TPACCAbstractSyntaxTreeNodeSWITCHStatement.Create(TPACCInstance(Instance),astnkSWITCH,nil,RelevantSourceLocation,ExpressionNode,BodyNode,SwitchBreakLabel,Cases.DefaultCaseLabel,SwitchCases);
  finally
   Cases:=OldCases;
   BreakLabel:=OldBreakLabel;
   SwitchCases:=nil;
  end;
 end;
 function ParseLabelTail(const CurrentLabel:TPACCAbstractSyntaxTreeNode):TPACCAbstractSyntaxTreeNode; forward;
 function ParseCASELabel:TPACCAbstractSyntaxTreeNode;
  procedure CheckDuplicates;
  var Index,Last:TPACCInt;
      x,y:PCase;
  begin
   Last:=Cases.Count-1;
   x:=@Cases.Cases[Last];
   for Index:=0 to Last-1 do begin
    y:=@Cases.Cases[Index];
    if (x^.Begin_>y^.End_) or (x^.End_<y^.Begin_) then begin
     // Do nothing
    end else begin
     if x^.Begin_=x^.End_ then begin
      AddError('Duplicate case value: '+IntToStr(x^.Begin_),nil,false);
     end else begin
      AddError('Duplicate case value range: '+IntToStr(x^.Begin_)+' .. '+IntToStr(x^.End_),nil,false);
     end;
    end;
   end;
  end;
 var Label_:TPACCAbstractSyntaxTreeNodeLabel;
     Begin_,End_,Index:TPACCInt;
     CurrentCase:PCase;
 var RelevantSourceLocation:TPACCSourceLocation;
 begin
  if Cases.Ready then begin
   result:=nil;
   RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
   Label_:=NewLabel(astnkHIDDEN_LABEL,RelevantSourceLocation);
   Begin_:=ParseIntegerExpression;
   if CurrentState.Token^.TokenType=TOK_ELLIPSIS then begin
    NextToken;
    End_:=ParseIntegerExpression;
    if Begin_>End_ then begin
     AddError('Invalid case value range: '+IntToStr(Begin_)+' .. '+IntToStr(End_),nil,false);
    end;
   end else begin
    End_:=Begin_;
   end;
   Expect(TOK_COLON);
   Index:=Cases.Count;
   inc(Cases.Count);
   if length(Cases.Cases)<Cases.Count then begin
    SetLength(Cases.Cases,Cases.Count*2);
   end;
   CurrentCase:=@Cases.Cases[Index];
   CurrentCase^.Begin_:=Begin_;
   CurrentCase^.End_:=End_;
   CurrentCase^.Label_:=Label_;
   CheckDuplicates;
   result:=ParseLabelTail(Label_);
  end else begin
   result:=nil;
   AddError('Case statement is not allowed outside switch statement',nil,true);
  end;
 end;
 function ParseDEFAULTLabel:TPACCAbstractSyntaxTreeNode;
 var RelevantSourceLocation:TPACCSourceLocation;
 begin
  if Cases.Ready then begin
   RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
   if assigned(Cases.DefaultCaseLabel) then begin
    AddError('Duplicate default case statement',nil,false);
   end;
   Cases.DefaultCaseLabel:=NewLabel(astnkHIDDEN_LABEL,RelevantSourceLocation);
   Expect(TOK_COLON);
   result:=ParseLabelTail(Cases.DefaultCaseLabel);
  end else begin
   result:=nil;
   AddError('Default case statement is not allowed outside switch statement',nil,true);
  end;
 end;
 function ParseBREAKStatement:TPACCAbstractSyntaxTreeNode;
 var RelevantSourceLocation:TPACCSourceLocation;
 begin
  RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
  if not assigned(BreakLabel) then begin
   AddError('Invalid break statement',nil,false);
  end;
  Expect(TOK_SEMICOLON);
  result:=TPACCAbstractSyntaxTreeNodeBREAKOrCONTINUEStatement.Create(TPACCInstance(Instance),astnkBREAK,nil,RelevantSourceLocation,BreakLabel);
 end;
 function ParseCONTINUEStatement:TPACCAbstractSyntaxTreeNode;
 var RelevantSourceLocation:TPACCSourceLocation;
 begin
  RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
  if not assigned(ContinueLabel) then begin
   AddError('Invalid continue statement',nil,false);
  end;
  Expect(TOK_SEMICOLON);
  result:=TPACCAbstractSyntaxTreeNodeBREAKOrCONTINUEStatement.Create(TPACCInstance(Instance),astnkCONTINUE,nil,RelevantSourceLocation,ContinueLabel);
 end;
 function ParseGOTOStatement:TPACCAbstractSyntaxTreeNode;
 var LabelName:TPACCRawByteString;
     RelevantSourceLocation:TPACCSourceLocation;
 begin
  RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
  case CurrentState.Token^.TokenType of
   TOK_MUL:begin
    NextToken;
    result:=ParseCastExpression;
    if result.Type_^.Kind<>tkPOINTER then begin
     AddError('Pointer expected for computed goto',nil,false);
    end;
    result:=TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(TPACCInstance(Instance),astnkCOMPUTED_GOTO,nil,RelevantSourceLocation,result);
   end;
   TOK_IDENT:begin
    LabelName:=TPACCInstance(Instance).TokenSymbols[CurrentState.Token^.Index].Name;
    NextToken;
    Expect(TOK_SEMICOLON);
    result:=TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress.Create(TPACCInstance(Instance),astnkGOTO,nil,RelevantSourceLocation,nil,LabelName);
    if assigned(UnresolvedLabelUsedNodes) then begin
     UnresolvedLabelUsedNodes.Add(result);
    end else begin
     AddError('Internal error 2017-01-04-02-14-0000',nil,true);
    end;
   end;
   else begin
    result:=nil;
    AddError('Identifier expected',nil,true);
   end;
  end;
 end;
 function ParseLabelTail(const CurrentLabel:TPACCAbstractSyntaxTreeNode):TPACCAbstractSyntaxTreeNode;
 var Statement:TPACCAbstractSyntaxTreeNode;
     RelevantSourceLocation:TPACCSourceLocation;
 begin
  RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
  Statement:=ParseStatement;
  result:=TPACCAbstractSyntaxTreeNodeStatements.Create(TPACCInstance(Instance),astnkSTATEMENTS,nil,RelevantSourceLocation);
  TPACCAbstractSyntaxTreeNodeStatements(result).Children.Add(CurrentLabel);
  if assigned(Statement) then begin
   TPACCAbstractSyntaxTreeNodeStatements(result).Children.Add(Statement);
  end;
 end;
 function ParseLabel:TPACCAbstractSyntaxTreeNode;
 var LabelName:TPACCRawByteString;
     RelevantSourceLocation:TPACCSourceLocation;
 begin
  RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
  LabelName:=TPACCInstance(Instance).TokenSymbols[CurrentState.Token^.Index].Name;
  if assigned(LabelScope[LabelName]) then begin
   AddError('Duplicate label: '+LabelName,nil,false);
  end;
  NextToken;
  Expect(TOK_COLON);
  result:=NewLabel(astnkLABEL,RelevantSourceLocation,LabelName);
  LabelScope[LabelName]:=result;
  result:=ParseLabelTail(result);
 end;
 function ParseStatement:TPACCAbstractSyntaxTreeNode;
 begin
  case CurrentState.Token^.TokenType of
   TOK_LBRA:begin
    NextToken;
    result:=ParseCompoundStatement;
   end;
   TOK_ASM:begin
    NextToken;
    result:=ParseAssemblerStatement(false);
   end;
   TOK_IF:begin
    NextToken;
    result:=ParseIFStatement;
   end;
   TOK_FOR:begin
    NextToken;
    result:=ParseFORStatement;
   end;
   TOK_WHILE:begin
    NextToken;
    result:=ParseWHILEStatement;
   end;
   TOK_DO:begin
    NextToken;
    result:=ParseDOStatement;
   end;
   TOK_RETURN:begin
    NextToken;
    result:=ParseRETURNStatement;
   end;
   TOK_SWITCH:begin
    NextToken;
    result:=ParseSWITCHStatement;
   end;
   TOK_CASE:begin
    NextToken;
    result:=ParseCASELabel;
   end;
   TOK_DEFAULT:begin
    NextToken;
    result:=ParseDEFAULTLabel;
   end;
   TOK_BREAK:begin
    NextToken;
    result:=ParseBREAKStatement;
   end;
   TOK_CONTINUE:begin
    NextToken;
    result:=ParseCONTINUEStatement;
   end;
   TOK_GOTO:begin
    NextToken;
    result:=ParseGOTOStatement;
   end;
   else begin
    if (CurrentState.Token^.TokenType=TOK_IDENT) and (PeekTokenType(1)=TOK_COLON) then begin
     result:=ParseLabel;
    end else begin
     result:=ParseExpressionOptional;
     Expect(TOK_SEMICOLON);
    end;
   end;
  end;
 end;
 procedure ParseDeclarationOrStatement(const List:TPACCAbstractSyntaxTreeNodeList);
 var Statement:TPACCAbstractSyntaxTreeNode;
 begin
  case CurrentState.Token^.TokenType of
   TOK_EOF:begin
    AddError('Premature end of input',nil,true);
   end;
   TOK_STATIC_ASSERT:begin
    NextToken;
    ParseStaticAssert;
   end;
   else begin
    if IsType(CurrentState.Token) then begin
     ParseDeclaration(List,false,false);
    end else begin
     Statement:=ParseStatement;
     if assigned(Statement) then begin
      List.Add(Statement);
     end;
    end;
   end;
  end;
 end;
 function ParseCompoundStatement:TPACCAbstractSyntaxTreeNode;
 var OriginalLocalScope:TPACCRawByteStringHashMap;
 begin
  result:=TPACCAbstractSyntaxTreeNodeStatements.Create(TPACCInstance(Instance),astnkSTATEMENTS,nil,CurrentState.Token^.SourceLocation);
  try
   OriginalLocalScope:=LocalScope;
   LocalScope:=nil;
   try
    LocalScope:=TPACCRawByteStringHashMap.Create;
    LocalScope.Parent:=OriginalLocalScope;
    repeat
     if CurrentState.Token^.TokenType=TOK_RBRA then begin
      NextToken;
      break;
     end else begin
      ParseDeclarationOrStatement(TPACCAbstractSyntaxTreeNodeStatements(result).Children);
     end;
    until false;
   finally
    FreeAndNil(LocalScope);
    LocalScope:=OriginalLocalScope;
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
 function ParseDeclaratorSpecifier(ResultStorageClass:PPACCInt32;var Attribute:TPACCAttribute):PPACCType;
  function ParseBitSize(const Name:TPACCRawByteString;const Type_:PPACCType):TPACCInt;
  var MaxSize:TPACCInt64;
  begin
   if TPACCInstance(Instance).IsIntType(Type_) then begin
    result:=ParseIntegerExpression;
    if Type_^.Kind=tkBOOL then begin
     MaxSize:=1;
    end else begin
     MaxSize:=Type_^.Size shl 3;
    end;
    if (result<0) or (MaxSize<result) then begin
     AddError('Invalid bitfield Size',nil,false);
    end else if (result=0) and (length(Name)>0) then begin
     AddError('Zero-width bitfield needs to be unnamed',nil,false);
    end;
   end else begin
    result:=0;
    AddError('Non-integer type can''t be a bitfield',nil,false);
   end;
  end;
  function ComputePadding(const Offset,Alignment:TPACCInt64):TPACCInt64;
  begin
   result:=Offset mod Alignment;
   if result<>0 then begin
    result:=Alignment-result;
   end;
  end;
  procedure SquashUnnamedStruct(var Fields:TPACCStructFields;var Count:TPACCInt;const Unnamed:PPACCType;const Offset:TPACCInt64);
  var UnnamedIndex,Index:TPACCInt;
  begin
   for UnnamedIndex:=0 to length(Unnamed^.Fields)-1 do begin
    Index:=Count;
    inc(Count);
    if length(Fields)<Count then begin
     SetLength(Fields,Count*2);
    end;
    Fields[Index].Name:=Unnamed^.Fields[UnnamedIndex].Name;
    Fields[Index].Type_:=TPACCInstance(Instance).CopyType(Unnamed^.Fields[UnnamedIndex].Type_);
    inc(Fields[Index].Type_.Offset,Offset);
   end;
  end;
  function UpdateStructOffsets(var Size:TPACCInt64;var Alignment:TPACCInt;const Fields:TPACCStructFields;const MaxAlignment:TPACCInt32):TPACCStructFields;
  var InputIndex,Index,Count:TPACCInt;
      Offset,BitOffset,Bit,Room:TPACCInt64;
      FieldName:TPACCRawByteString;
      FieldType:PPACCType;
   procedure FinishBitfield;
   begin
    inc(Offset,(BitOffset+7) shr 3);
    BitOffset:=0;
   end;
  begin
   result:=nil;
   Offset:=0;
   BitOffset:=0;
   Index:=0;
   Count:=0;
   try
    for InputIndex:=0 to length(Fields)-1 do begin
     FieldName:=Fields[InputIndex].Name;
     FieldType:=Fields[InputIndex].Type_;
     if length(FieldName)>0 then begin
      // C11 6.7.2.1p14: Each member is aligned to its natural boundary.
      // As a result the entire struct is aligned to the largest among its members.
      // Unnamed fields will never be accessed, so they shouldn't be taken into account
      // when calculating alignment.
      Alignment:=max(Alignment,Min(MaxAlignment,FieldType^.Alignment));
     end;
     if (length(FieldName)=0) and (FieldType^.Kind=tkSTRUCT) then begin
      // C11 6.7.2.1p13: Anonymous struct
      FinishBitfield;
      inc(Offset,ComputePadding(Offset,Min(MaxAlignment,FieldType^.Alignment)));
      SquashUnnamedStruct(result,Count,FieldType,Offset);
      inc(Offset,FieldType^.Size);
     end else if FieldType^.BitSize=0 then begin
      // C11 6.7.2.1p12: The zero-Size bit-field indicates the end of the current run of the bit-fields.
      FinishBitfield;
      inc(Offset,ComputePadding(Offset,Min(MaxAlignment,FieldType^.Alignment)));
      BitOffset:=0;
     end else begin
      if FieldType^.BitSize>0 then begin
       Bit:=FieldType^.Size shl 3;
       Room:=Bit-(((Offset shl 3)+BitOffset) mod Bit);
       if FieldType^.BitSize<=Room then begin
        FieldType^.Offset:=Offset;
        FieldType^.BitOffset:=BitOffset;
       end else begin
        FinishBitfield;
        inc(Offset,ComputePadding(Offset,Min(MaxAlignment,FieldType^.Alignment)));
        FieldType^.Offset:=Offset;
        FieldType^.BitOffset:=0;
       end;
       inc(BitOffset,FieldType^.BitSize);
      end else begin
       FinishBitfield;
       inc(Offset,ComputePadding(Offset,Min(MaxAlignment,FieldType^.Alignment)));
       FieldType^.Offset:=Offset;
       inc(Offset,FieldType^.Size);
      end;
      if length(FieldName)>0 then begin
       Index:=Count;
       inc(Count);
       if length(result)<Count then begin
        SetLength(result,Count*2);
       end;
       result[Index].Name:=FieldName;
       result[Index].Type_:=FieldType;
      end;
     end;
    end;
    FinishBitfield;
    Size:=Offset+ComputePadding(Offset,Alignment);
   finally
    SetLength(result,Count);
   end;
  end;
  function UpdateUnionOffsets(var Size:TPACCInt64;var Alignment:TPACCInt;const Fields:TPACCStructFields;const MaxAlignment:TPACCInt32):TPACCStructFields;
  var InputIndex,Index,Count:TPACCInt;
      MaxSize:TPACCInt64;
      FieldName:TPACCRawByteString;
      FieldType:PPACCType;
  begin
   result:=nil;
   MaxSize:=0;
   Index:=0;
   Count:=0;
   try
    for InputIndex:=0 to length(Fields)-1 do begin
     FieldName:=Fields[InputIndex].Name;
     FieldType:=Fields[InputIndex].Type_;
     MaxSize:=max(MaxSize,FieldType^.Size);
     Alignment:=max(Alignment,Min(MaxAlignment,FieldType^.Alignment));
     if (length(FieldName)=0) and (FieldType^.Kind=tkSTRUCT) then begin
      SquashUnnamedStruct(result,Count,FieldType,0);
     end else begin
      FieldType^.Offset:=0;
      if FieldType^.BitSize>=0 then begin
       FieldType^.BitOffset:=0;
      end;
      if length(FieldName)>0 then begin
       Index:=Count;
       inc(Count);
       if length(result)<Count then begin
        SetLength(result,Count*2);
       end;
       result[Index].Name:=FieldName;
       result[Index].Type_:=FieldType;
      end;
     end;
    end;
    Size:=MaxSize+ComputePadding(MaxSize,Alignment);
   finally
    SetLength(result,Count);
   end;
  end;
  function ParseStructUnionDefinition(const IsStruct:boolean;var StructFields:TPACCStructFields):PPACCType;
   function ParseStructFields(const IsStruct:boolean):TPACCStructFields;
   var BaseType,FieldType:PPACCType;
       Index,Count:TPACCInt;
       Name:TPACCRawByteString;
       Fields:TPACCStructFields;
       Attribute:TPACCAttribute;
   begin
    result:=nil;
    if CurrentState.Token^.TokenType=TOK_LBRA then begin
     Fields:=nil;
     Count:=0;
     try
      NextToken;
      repeat
       if CurrentState.Token^.TokenType=TOK_STATIC_ASSERT then begin
        NextToken;
        ParseStaticAssert;
       end else begin
        if IsType(CurrentState.Token) then begin
         Attribute:=PACCEmptyAttribute;
         BaseType:=ParseDeclaratorSpecifier(nil,Attribute);
         BaseType.Attribute:=Attribute;
         if Attribute.Alignment>=0 then begin
          BaseType.Alignment:=Attribute.Alignment;
         end;
         if (BaseType^.Kind=tkSTRUCT) and (CurrentState.Token^.TokenType=TOK_SEMICOLON) then begin
          Index:=Count;
          inc(Count);
          if length(Fields)<Count then begin
           SetLength(Fields,Count*2);
          end;
          Fields[Index].Name:='';
          Fields[Index].Type_:=BaseType;
         end else begin
          repeat
           Name:='';
           FieldType:=ParseDeclarator(Name,BaseType,nil,DECL_PARAM_TYPEONLY);
           EnsureNotVoid(FieldType);
           FieldType:=TPACCInstance(Instance).CopyType(FieldType);
           if CurrentState.Token^.TokenType=TOK_COLON then begin
            NextToken;
            FieldType^.BitSize:=ParseBitSize(Name,FieldType);
           end else begin
            FieldType^.BitSize:=-1;
           end;
           Index:=Count;
           inc(Count);
           if length(Fields)<Count then begin
            SetLength(Fields,Count*2);
           end;
           Fields[Index].Name:=Name;
           Fields[Index].Type_:=FieldType;
           case CurrentState.Token^.TokenType of
            TOK_COMMA:begin
             NextToken;
            end;
            TOK_RBRA:begin
             AddWarning('Missing '';'' at the end of field list');
             break;
            end;
            else begin
             Expect(TOK_SEMICOLON);
             break;
            end;
           end;
          until false;
         end;
        end else begin
         break;
        end;
       end;
      until false;
      Expect(TOK_RBRA);
     finally
      SetLength(Fields,Count);
     end;
     if (Count>0) and (Count=length(Fields)) then begin
      for Index:=0 to Count-1 do begin
       Name:=Fields[Index].Name;
       FieldType:=Fields[Index].Type_;
       if (FieldType^.Kind=tkARRAY) and (FieldType^.ArrayLength<0) then begin
        if Index<>(Count-1) then begin
         AddError('Flexible member "'+Name+'" may only appear as the last member',nil,false);
        end else if Count=1 then begin
         AddError('Flexible member "'+Name+'" with no other fields',nil,false);
        end else begin
         FieldType^.Size:=0;
         FieldType^.ArrayLength:=0;
        end;
       end;
      end;
     end;
    end;
   end;
  var Tag:TPACCRawByteString;
  begin
   if CurrentState.Token^.TokenType=TOK_IDENT then begin
    Tag:=TPACCInstance(Instance).TokenSymbols[CurrentState.Token^.Index].Name;
    if length(Tag)>0 then begin
     result:=TagScope[Tag];
     if assigned(result) then begin
      if ((result^.Kind=tkENUM) or ((tfStruct in result^.Flags)<>IsStruct)) then begin
       AddError('Declarations of "'+Tag+'" does not match',nil,true);
      end;
     end else begin
      result:=TPACCInstance(Instance).NewStructType(IsStruct);
      TagScope[Tag]:=result;
     end;
    end;
    NextToken;
   end else begin
    result:=TPACCInstance(Instance).NewStructType(IsStruct);
   end;
   StructFields:=ParseStructFields(IsStruct);
  end;
  procedure FinalizeStructUnion(const Type_:PPACCType;const StructFields:TPACCStructFields);
  var Alignment:TPACCInt;
      MaxAlignment:TPACCInt32;
      Size:TPACCInt64;
      FinalizedFields:TPACCStructFields;
  begin
   Size:=0;
   Alignment:=1;
   if afPacked in Type_^.Attribute.Flags then begin
    MaxAlignment:=1;
   end else if PragmaPack>0 then begin
    MaxAlignment:=PragmaPack;
   end else begin
    MaxAlignment:=TPACCInstance(Instance).Target.MaximumAlignment;
   end;
   if tfStruct in Type_^.Flags then begin
    FinalizedFields:=UpdateStructOffsets(Size,Alignment,StructFields,MaxAlignment);
   end else begin
    FinalizedFields:=UpdateUnionOffsets(Size,Alignment,StructFields,MaxAlignment);
   end;
   Type_^.Alignment:=Alignment;
   if length(FinalizedFields)>0 then begin
    Type_^.Fields:=FinalizedFields;
    Type_^.Size:=Size;
   end;
  end;
  function ParseEnumDefinition:PPACCType;
  var Tag,Name:TPACCRawByteString;
      Type_:PPACCType;
      Value:TPACCInt;
      ConstantValueNode:TPACCAbstractSyntaxTreeNode;
      RelevantSourceLocation:TPACCSourceLocation;
      First:boolean;
  begin
   First:=true;
   if CurrentState.Token^.TokenType=TOK_IDENT then begin
    Tag:=TPACCInstance(Instance).TokenSymbols[CurrentState.Token^.Index].Name;
    if length(Tag)>0 then begin
     Type_:=TagScope[Tag];
     if assigned(Type_) and (Type_^.Kind<>tkENUM) then begin
      AddError('Declarations of "'+Tag+'" does not match',nil,true);
     end;
     First:=Type_^.MinValue>Type_^.MaxValue;
    end;
    NextToken;
   end else begin
    Tag:='';
   end;
   if First then begin
    Type_^.MinValue:=high(TPACCInt32);
    Type_^.MaxValue:=low(TPACCInt32);
   end;
   if CurrentState.Token^.TokenType=TOK_LBRA then begin
    if length(Tag)>0 then begin
     TagScope[Tag]:=TPACCInstance(Instance).TypeENUM;
    end;
    Value:=0;
    repeat
     if CurrentState.Token^.TokenType=TOK_RBRA then begin
      NextToken;
      break;
     end;
     if CurrentState.Token^.TokenType=TOK_IDENT then begin
      RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
      Name:=TPACCInstance(Instance).TokenSymbols[CurrentState.Token^.Index].Name;
      NextToken;
      if CurrentState.Token^.TokenType=TOK_ASSIGN then begin
       NextToken;
       Value:=ParseIntegerExpression;
      end;
      ConstantValueNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(TPACCInstance(Instance),astnkINTEGER,TPACCInstance(Instance).TypeINT,RelevantSourceLocation,Value);
      GetScope[Name]:=ConstantValueNode;
      if First then begin
       First:=false;
       Type_^.MinValue:=Value;
       Type_^.MaxValue:=Value;
      end else begin
       Type_^.MinValue:=Min(Type_^.MinValue,Value);
       Type_^.MaxValue:=Min(Type_^.MaxValue,Value);
      end;
      inc(Value);
      case CurrentState.Token^.TokenType of
       TOK_COMMA:begin
        NextToken;
       end;
       TOK_RBRA:begin
        NextToken;
        break;
       end;
       else begin
        AddError('"}" or "," expected, but got "'+TPACCLexer(TPACCInstance(Instance).Lexer).TokenStrings[CurrentState.Token^.TokenType]+'"',nil,true);
       end;
      end;
     end else begin
      AddError('Identifier expected, but got "'+TPACCLexer(TPACCInstance(Instance).Lexer).TokenStrings[CurrentState.Token^.TokenType]+'"',nil,true);
     end;
    until false;
   end else begin
    if (length(Tag)=0) or not assigned(TagScope[Tag]) then begin
     AddError('enum tag "'+Tag+'" is not defined',nil,true);
    end;
   end;
   result:=TPACCInstance(Instance).TypeINT;
  end;  
  function ParseAlignAs:TPACCInt32;
  var Type_:PPACCType;
  begin
   // C11 6.7.5. Valid form of _Alignof is either _Alignas(type-name) or _Alignas(constant-expression).
   Expect(TOK_LPAR);
   if IsType(CurrentState.Token) then begin
    Type_:=ParseCastType;
    result:=Type_^.Alignment;
   end else begin
    result:=ParseIntegerExpression;
   end;
   Expect(TOK_RPAR);
  end;
  function ParseTypeOf:PPACCType;
  begin
   Expect(TOK_LPAR);
   if IsType(CurrentState.Token) then begin
    result:=ParseCastType;
   end else begin
    result:=ParseCommaExpression.Type_;
   end;
   Expect(TOK_RPAR);
  end;
 const KIND_VOID=1;
       KIND_BOOL=2;
       KIND_CHAR=3;
       KIND_INT=4;
       KIND_FLOAT=5;
       KIND_DOUBLE=6;
       SIZE_SHORT=1;
       SIZE_LONG=2;
       SIZE_LONG_LONG=3;
       SIGNED_SIGNED=1;
       SIGNED_UNSIGNED=2;
 var StorageClass,Kind,Size,Signed,Alignment,Value,TemporaryCallingConvention,CallingConvention:TPACCInt32;
     UserType,Definition:PPACCType;
     TypeFlags:TPACCTypeFlags;
     AttributeName:TPACCRawByteString;
     StructFields:TPACCStructFields;
     RelevantSourceLocation:TPACCSourceLocation;
     FromTypeDef:boolean;
  procedure Error;
  begin
   AddError('Type mismatch: '+TPACCLexer(TPACCInstance(Instance).Lexer).TokenStrings[CurrentState.Token^.TokenType],nil,true);
  end;
  procedure ErrorCheck;
  begin
   if ((Kind=KIND_BOOL) and (Size<>0) and (Signed<>0)) or
      ((Size=SIZE_SHORT) and (Kind<>0) and (Kind<>KIND_INT)) or
      ((Size=SIZE_LONG) and (Kind<>0) and (Kind<>KIND_INT) and (Kind<>KIND_DOUBLE)) or
      ((Signed<>0) and (Kind in [KIND_VOID,KIND_FLOAT,KIND_DOUBLE])) or
      (assigned(UserType) and ((Kind<>0) or (Size<>0) or (Signed<>0))) then begin
    Error;
   end;
  end;
  procedure ParseAttributeOrDeclSpec(const IsDeclSpec:boolean);
  begin
   Expect(TOK_LPAR);
   if not IsDeclSpec then begin
    Expect(TOK_LPAR);
   end;
   while CurrentState.Token^.TokenType<>TOK_RPAR do begin
    if CurrentState.Token^.TokenType=TOK_IDENT then begin
     RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
     AttributeName:=TPACCInstance(Instance).TokenSymbols[CurrentState.Token^.Index].Name;
     NextToken;
     if length(AttributeName)>0 then begin
      TemporaryCallingConvention:=TPACCInstance(Instance).Target.CheckCallingConvention(AttributeName);
      if TemporaryCallingConvention>=0 then begin
       if CallingConvention>=0 then begin
        AddError('Duplicate calling convention specifier "'+AttributeName+'"',@RelevantSourceLocation,false);
       end;
       CallingConvention:=TemporaryCallingConvention;
      end else begin
       if (AttributeName='aligned') or (AttributeName='__aligned__') then begin
        if CurrentState.Token^.TokenType=TOK_LPAR then begin
         Expect(TOK_LPAR);
         Value:=ParseIntegerExpression;
         if Value<0 then begin
          AddError('Negative alignment: '+IntToStr(Value),nil,false);
         end else begin
          // C11 6.7.5p6: alignas(0) should have no effect.
          if (Value>0) and ((Alignment<0) or (Value<Alignment)) then begin
           Alignment:=Value;
          end;
         end;
         Expect(TOK_RPAR);
        end else begin
         Value:=TPACCInstance(Instance).Target.MaximumAlignment;
         if (Value>0) and ((Alignment<0) or (Value<Alignment)) then begin
          Alignment:=Value;
         end;
        end;
       end else begin
        AddWarning('Unknown attribute specifier "'+AttributeName+'", ignoring',@RelevantSourceLocation);
        if CurrentState.Token^.TokenType=TOK_LPAR then begin
         SkipParentheses;
        end;
       end;
      end;
     end else if (AttributeName='packed') or (AttributeName='__packed__') then begin
      Include(Attribute.Flags,afPacked);
     end else if (AttributeName='noinline') or (AttributeName='__noinline__') then begin
      Include(Attribute.Flags,afNoInline);
     end else if (AttributeName='noreturn') or (AttributeName='__noreturn__') then begin
      Include(Attribute.Flags,afNoReturn);
     end else if (AttributeName='const') or (AttributeName='__const__') then begin
      Include(Attribute.Flags,afConstant);
     end else if (AttributeName='volatile') or (AttributeName='__volatile__') then begin
      Include(Attribute.Flags,afVolatile);
     end else if (AttributeName='restrict') or (AttributeName='__restrict__') then begin
      Include(Attribute.Flags,afRestrict);
     end else begin
      AddError('Attribute name expect',@CurrentState.Token^.SourceLocation,true);
     end;
    end else begin
     AddError('Attribute name expect',@CurrentState.Token^.SourceLocation,true);
    end;
    case CurrentState.Token^.TokenType of
     TOK_COMMA:begin
      NextToken;
     end;
     TOK_RPAR:begin
      break;
     end;
     else begin
      AddError('")" or "," expected',@CurrentState.Token^.SourceLocation,true);
     end;
    end;
   end;
   if not IsDeclSpec then begin
    Expect(TOK_RPAR);
   end;
   Expect(TOK_RPAR);
  end;
 begin
  result:=nil;
  StorageClass:=0;
  TypeFlags:=[];
  if IsType(CurrentState.Token) then begin
   UserType:=nil;
   Kind:=0;
   Size:=0;
   Signed:=0;
   Alignment:=-1;
   CallingConvention:=-1;
   StructFields:=nil;
   FromTypeDef:=false;
   repeat
    if CurrentState.Token^.TokenType=TOK_EOF then begin
     AddError('Premature end of input',nil,true);
    end;
    if (Kind=0) and (CurrentState.Token^.TokenType=TOK_IDENT) and not assigned(UserType) then begin
     Definition:=GetTypeDef(TPACCInstance(Instance).TokenSymbols[CurrentState.Token^.Index].Name);
     NextToken;
     if assigned(Definition) then begin
      if assigned(UserType) then begin
       Error;
      end else begin
       UserType:=Definition;
       ErrorCheck;
       FromTypeDef:=true;
      end;
     end;
    end;
    if not (CurrentState.Token^.TokenType in KeywordTokens) then begin
     break;
    end;
    case CurrentState.Token^.TokenType of
     TOK_TYPEDEF:begin
      if StorageClass<>0 then begin
       Error;
      end;
      StorageClass:=S_TYPEDEF;
      NextToken;
     end;
     TOK_EXTERN:begin
      if StorageClass<>0 then begin
       Error;
      end;
      StorageClass:=S_EXTERN;
      NextToken;
     end;
     TOK_STATIC:begin
      if StorageClass<>0 then begin
       Error;
      end;
      StorageClass:=S_STATIC;
      NextToken;
     end;
     TOK_AUTO:begin
      if StorageClass<>0 then begin
       Error;
      end;
      StorageClass:=S_AUTO;
      NextToken;
     end;
     TOK_REGISTER:begin
      if StorageClass<>0 then begin
       Error;
      end;
      StorageClass:=S_REGISTER;
      NextToken;
     end;
     TOK_CONST:begin
      NextToken;
      Include(Attribute.Flags,afConstant);
     end;
     TOK_VOLATILE:begin
      NextToken;
      Include(Attribute.Flags,afVolatile);
     end;
     TOK_RESTRICT:begin
      NextToken;
      Include(Attribute.Flags,afRestrict);
     end;
     TOK_INLINE:begin
      NextToken;
      Include(Attribute.Flags,afInline);
     end;
     TOK_NORETURN:begin
      NextToken;
      Include(Attribute.Flags,afNoReturn);
     end;
     TOK_VOID:begin
      if Kind<>0 then begin
       Error;
      end;
      Kind:=KIND_VOID;
      NextToken;
     end;
     TOK_BOOL:begin
      if Kind<>0 then begin
       Error;
      end;
      Kind:=KIND_BOOL;
      NextToken;
     end;
     TOK_CHAR:begin
      if Kind<>0 then begin
       Error;
      end;
      Kind:=KIND_CHAR;
      NextToken;
     end;
     TOK_INT:begin
      if Kind<>0 then begin
       Error;
      end;
      Kind:=KIND_INT;
      NextToken;
     end;
     TOK_FLOAT:begin
      if Kind<>0 then begin
       Error;
      end;
      Kind:=KIND_FLOAT;
      NextToken;
     end;
     TOK_DOUBLE:begin
      if Kind<>0 then begin
       Error;
      end;
      Kind:=KIND_DOUBLE;
      NextToken;
     end;
     TOK_SIGNED1,TOK_SIGNED2,TOK_SIGNED3:begin
      if Signed<>0 then begin
       Error;
      end;
      Signed:=SIGNED_SIGNED;
      NextToken;
     end;
     TOK_UNSIGNED:begin
      if Signed<>0 then begin
       Error;
      end;
      Signed:=SIGNED_UNSIGNED;
      NextToken;
     end;
     TOK_SHORT:begin
      if Size<>0 then begin
       Error;
      end;
      Size:=SIZE_SHORT;
      NextToken;
     end;
     TOK_STRUCT:begin
      if assigned(UserType) then begin
       Error;
      end;
      NextToken;
      UserType:=ParseStructUnionDefinition(true,StructFields);
     end;
     TOK_UNION:begin
      if assigned(UserType) then begin
       Error;
      end;
      NextToken;
      UserType:=ParseStructUnionDefinition(false,StructFields);
     end;
     TOK_ENUM:begin
      if assigned(UserType) then begin
       Error;
      end;
      NextToken;
      UserType:=ParseEnumDefinition;
     end;
     TOK_ALIGNAS:begin
      NextToken;
      Value:=ParseAlignAs;
      if Value<0 then begin
       AddError('Negative alignment: '+IntToStr(Value),nil,false);
      end else begin
       // C11 6.7.5p6: alignas(0) should have no effect.
       if (Value>0) and ((Alignment<0) or (Value<Alignment)) then begin
        Alignment:=Value;
       end;
      end;
     end;
     TOK_LONG:begin
      if Size<>0 then begin
       Error;
      end;
      Size:=SIZE_LONG;
      NextToken;
     end;
     TOK_TYPEOF:begin
      if assigned(UserType) then begin
       Error;
      end;
      NextToken;
      UserType:=ParseTypeOf;
      FromTypeDef:=true;
     end;
     TOK_ATTRIBUTE:begin
      NextToken;
      ParseAttributeOrDeclSpec(false);
     end;
     TOK_DECLSPEC:begin
      NextToken;
      ParseAttributeOrDeclSpec(true);
     end;
     else begin
      break;
     end;
    end;
    ErrorCheck;
   until false;
   if assigned(ResultStorageClass) then begin
    ResultStorageClass^:=StorageClass;
   end;
   if assigned(UserType) then begin
    if FromTypeDef then begin
     if TypeFlags<>[] then begin
      result:=TPACCInstance(Instance).CopyType(UserType);
      result^.Flags:=result^.Flags+TypeFlags;
      if afPacked in Attribute.Flags then begin
       Attribute.Alignment:=1;
      end;
     end else begin
      result:=UserType;
     end;
    end else begin
     result:=UserType;
     result^.Flags:=result^.Flags+TypeFlags;
     case result^.Kind of
      tkSTRUCT:begin
       FinalizeStructUnion(result,StructFields);
       if afPacked in Attribute.Flags then begin
        Attribute.Alignment:=1;
       end;
      end;
      tkENUM:begin
       if afPacked in Attribute.Flags then begin
        Exclude(Attribute.Flags,afPacked);
        if (result^.MinValue>=low(TPACCInt8)) and (result^.MaxValue<=high(TPACCInt8)) then begin
         result:=TPACCInstance(Instance).TypeCHAR;
        end else if (result^.MinValue>=low(TPACCInt16)) and (result^.MaxValue<=high(TPACCInt16)) then begin
         result:=TPACCInstance(Instance).TypeSHORT;
        end else begin
         result:=TPACCInstance(Instance).TypeINT;
        end;
       end;
      end;
      else begin
       if afPacked in Attribute.Flags then begin
        Attribute.Alignment:=1;
       end;
      end;
     end;
    end;
   end else begin
    if (Alignment>=0) and ((Alignment and (Alignment-1))<>0) then begin
     AddError('Alignment must be power of 2, but got '+IntToStr(Alignment),nil,false);
    end;
    case Kind of
     KIND_VOID:begin
      result:=TPACCInstance(Instance).TypeVOID;
     end;
     KIND_BOOL:begin
      result:=TPACCInstance(Instance).NewNumbericType(tkBOOL,false);
     end;
     KIND_CHAR:begin
      result:=TPACCInstance(Instance).NewNumbericType(tkCHAR,Signed=SIGNED_UNSIGNED);
     end;
     KIND_FLOAT:begin
      result:=TPACCInstance(Instance).NewNumbericType(tkFLOAT,false);
     end;
     KIND_DOUBLE:begin
      if Size=SIZE_LONG then begin
       result:=TPACCInstance(Instance).NewNumbericType(tkLDOUBLE,false);
      end else begin
       result:=TPACCInstance(Instance).NewNumbericType(tkDOUBLE,false);
      end;
     end;
    end;
    if not assigned(result) then begin
     case Size of
      SIZE_SHORT:begin
       result:=TPACCInstance(Instance).NewNumbericType(tkSHORT,Signed=SIGNED_UNSIGNED);
      end;
      SIZE_LONG:begin
       result:=TPACCInstance(Instance).NewNumbericType(tkLONG,Signed=SIGNED_UNSIGNED);
      end;
      SIZE_LONG_LONG:begin
       result:=TPACCInstance(Instance).NewNumbericType(tkLLONG,Signed=SIGNED_UNSIGNED);
      end;
      else begin
       result:=TPACCInstance(Instance).NewNumbericType(tkINT,Signed=SIGNED_UNSIGNED);
      end;
     end;
     if not assigned(result) then begin
      AddError('Internal error: 2016-12-31-21-58-0000',nil,true);
     end;
    end;
    if afPacked in Attribute.Flags then begin
     Attribute.Alignment:=1;
    end else if Alignment>=0 then begin
     Attribute.Alignment:=Alignment;
    end;
    result^.Flags:=result^.Flags+TypeFlags;
   end;
   if CallingConvention>=0 then begin
     if Attribute.CallingConvention<0 then begin
     Attribute.CallingConvention:=CallingConvention;
    end else begin
     AddError('Calling convention already defined',nil,false);
    end;
   end;
  end else begin
   AddError('Type name expected, but got "'+TPACCLexer(TPACCInstance(Instance).Lexer).TokenStrings[CurrentState.Token^.TokenType]+'"',nil,true);
  end;
 end;
 function ParseDeclaratorWithOptionalSpecifier(StorageClass:PPACCInt32;var Attribute:TPACCAttribute):PPACCType;
 begin
  if IsType(CurrentState.Token) then begin
   result:=ParseDeclaratorSpecifier(StorageClass,Attribute);
  end else begin
   AddWarning('Type specifier missing, assuming int');
   result:=TPACCInstance(Instance).TypeINT;
  end;
 end;
 procedure ParseDeclaration(const Block:TPACCAbstractSyntaxTreeNodeList;const IsGlobal,IsTop:boolean);
  procedure ParseOldstyleParameterTypes(const Parameters:TPACCAbstractSyntaxTreeNodeList);
  var Index,OtherIndex:TPACCInt32;
      OriginalLocalScope:TPACCRawByteStringHashMap;
      ParameterVariables:TPACCAbstractSyntaxTreeNodeList;
      Declaration,Variable,Parameter:TPACCAbstractSyntaxTreeNode;
      Found:boolean;
  begin
   ParameterVariables:=TPACCAbstractSyntaxTreeNodeList.Create;
   try
    OriginalLocalScope:=LocalScope;
    try
     LocalScope:=nil;
     repeat
      if CurrentState.Token^.TokenType=TOK_LBRA then begin
       break;
      end else if not IsType(CurrentState.Token) then begin
       AddError('K&R-style declarator expected, but got "'+TPACCLexer(TPACCInstance(Instance).Lexer).TokenStrings[CurrentState.Token^.TokenType]+'"',nil,true);
      end;
      ParseDeclaration(ParameterVariables,false,false);
     until false;
    finally
     LocalScope:=OriginalLocalScope;
    end;
    for Index:=0 to ParameterVariables.Count-1 do begin
     Declaration:=ParameterVariables[Index];
     if assigned(Declaration) and (Declaration.Kind=astnkDECL) then begin
      Variable:=TPACCAbstractSyntaxTreeNodeDeclaration(Declaration).DeclarationVariable;
      if assigned(Variable) and (Variable.Kind=astnkLVAR) then begin
       Found:=false;
       for OtherIndex:=0 to Parameters.Count-1 do begin
        Parameter:=Parameters[OtherIndex];
        if assigned(Parameter) and (Parameter.Kind=astnkLVAR) then begin
         if TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Parameter).VariableName=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Variable).VariableName then begin
          Parameter.Type_:=Variable.Type_;
          Found:=true;
          break;
         end;
        end else begin
         AddError('Internal error 2017-01-01-17-17-0000',nil,true);
        end;
       end;
       if not Found then begin
        AddError('Missing parameter: '+TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Variable).VariableName,nil,false);
       end;
      end else begin
       AddError('Internal error 2017-01-01-17-15-0000',nil,true);
      end;
     end else begin
      AddError('Internal error 2017-01-01-17-14-0000',nil,true);
     end;
    end;
   finally
    for Index:=0 to ParameterVariables.Count-1 do begin
     ParameterVariables[Index].Free;
     ParameterVariables[Index]:=nil;
    end;
    ParameterVariables.Free;
   end;
  end;
  procedure BackfillLabels;
  var Index:longint;
      Node,LabelNode:TPACCAbstractSyntaxTreeNode;
  begin
   for Index:=0 to UnresolvedLabelUsedNodes.Count-1 do begin
    Node:=UnresolvedLabelUsedNodes[Index];
    if Node is TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress then begin
     LabelNode:=LabelScope[TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).LabelName];
     if assigned(LabelNode) and (LabelNode is TPACCAbstractSyntaxTreeNodeLabel) then begin
      TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).Label_:=TPACCAbstractSyntaxTreeNodeLabel(LabelNode);
     end else begin
      if Node.Kind=astnkGOTO then begin
       AddError('Invalid GOTO label: '+TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).LabelName,@Node.SourceLocation,false);
      end else begin
       AddError('Invalid unary && label: '+TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).LabelName,@Node.SourceLocation,false);
      end;
     end;
    end else begin
     AddError('Internal error 2017-01-07-21-02-0000',@Node.SourceLocation,true);
    end;
   end;
  end;
 var StorageClass,Index:TPACCInt32;
     BaseType,CurrentType:PPACCType;
     Name:TPACCRawByteString;
     Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;
     OriginalLocalScope,OtherOriginalLocalScope:TPACCRawByteStringHashMap;
     InitializationList:TPACCAbstractSyntaxTreeNodeList;
     Parameters:TPACCAbstractSyntaxTreeNodeList;
     FunctionName,FunctionBody,Parameter:TPACCAbstractSyntaxTreeNode;
     First:boolean;
     RelevantSourceLocation:TPACCSourceLocation;
     Attribute:TPACCAttribute;
 begin
  if CurrentState.Token^.TokenType=TOK_SEMICOLON then begin
   NextToken;
  end else begin
   StorageClass:=0;
   Attribute:=PACCEmptyAttribute;
   Parameters:=nil;
   RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
   BaseType:=ParseDeclaratorWithOptionalSpecifier(@StorageClass,Attribute);
   if CurrentState.Token^.TokenType=TOK_SEMICOLON then begin
    NextToken;
   end else begin
    First:=true;
    try
     repeat
      RelevantSourceLocation:=CurrentState.Token^.SourceLocation;
      Name:='';
      if IsTop and First then begin
       Parameters:=TPACCAbstractSyntaxTreeNodeList.Create;
      end else begin
       Parameters:=nil;
      end;
      First:=false;
      CurrentType:=ParseDeclarator(Name,TPACCInstance(Instance).CopyIncompleteType(BaseType),Parameters,DECL_BODY);
      if assigned(Parameters) and
         assigned(CurrentType) and
         ((CurrentState.Token^.TokenType=TOK_LBRA) or IsType(CurrentState.Token)) then begin
       // Function definition
       TPACCInstance(Instance).AllocatedObjects.Add(Parameters);
       if tfOldStyle in CurrentType^.Flags then begin
        if (not assigned(Parameters)) or (Parameters.Count=0) then begin
         Exclude(CurrentType^.Flags,tfVarArgs);
        end;
        ParseOldstyleParameterTypes(Parameters);
        CurrentType^.Parameters:=nil;
        SetLength(CurrentType^.Parameters,Parameters.Count);
        for Index:=0 to Parameters.Count-1 do begin
         CurrentType^.Parameters[Index]:=Parameters[Index].Type_;
        end;
       end;
       if StorageClass=S_STATIC then begin
        Include(CurrentType^.Flags,tfStatic);
       end else begin
        Exclude(CurrentType^.Flags,tfStatic);
       end;
       CurrentType^.Attribute:=Attribute;
       Variable:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable.Create(TPACCInstance(Instance),astnkGVAR,CurrentType,RelevantSourceLocation,Name,0,Name);
       Assert(assigned(GlobalScope));
       GlobalScope[Name]:=Variable;
       OriginalLocalScope:=LocalScope;
       try
        Expect(TOK_LBRA);
        LabelScope:=TPACCRawByteStringHashMap.Create;
        try
         UnresolvedLabelUsedNodes:=TPACCAbstractSyntaxTreeNodeList.Create;
         try
          LocalScope:=TPACCRawByteStringHashMap.Create;
          LocalScope.Parent:=GlobalScope;
          CurrentFunctionType:=CurrentType;
          OtherOriginalLocalScope:=LocalScope;
          try
           LocalScope:=TPACCRawByteStringHashMap.Create;
           LocalScope.Parent:=OtherOriginalLocalScope;
           FunctionName:=NewASTString(CurrentState.Token^.SourceLocation,ENCODING_NONE,Name);
           LocalScope['__func__']:=FunctionName;
           LocalScope['__FUNCTION__']:=FunctionName;
           LocalVariables:=TPACCAbstractSyntaxTreeNodeList.Create;
           TPACCInstance(Instance).AllocatedObjects.Add(LocalVariables);
           Labels:=TPACCAbstractSyntaxTreeNodeList.Create;
           TPACCInstance(Instance).AllocatedObjects.Add(Labels);
           for Index:=0 to Parameters.Count-1 do begin
            Parameter:=Parameters[Index];
            if assigned(Parameter) and (Parameter.Kind=astnkLVAR) then begin
             if length(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Parameter).VariableName)>0 then begin
              LocalScope[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Parameter).VariableName]:=Parameter;
             end;
             if assigned(LocalVariables) then begin
              TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Parameter).Index:=LocalVariables.Add(Parameter);
             end else begin
              TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Parameter).Index:=-1;
             end;
            end;
           end;
           FunctionBody:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration.Create(TPACCInstance(Instance),astnkFUNC,CurrentType,RelevantSourceLocation,Name,nil,nil,nil);
           TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(FunctionBody).Parameters:=Parameters;
           TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(FunctionBody).LocalVariables:=LocalVariables;
           TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(FunctionBody).Labels:=Labels;
           TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(FunctionBody).Variable:=Variable;
           TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(FunctionBody).Body:=ParseCompoundStatement;
           Parameters:=nil;
          finally
           CurrentFunctionType:=nil;
           FreeAndNil(LocalScope);
           LocalScope:=OtherOriginalLocalScope;
           LocalVariables:=nil;
           Labels:=nil;
          end;
          Block.Add(FunctionBody);
          BackfillLabels;
         finally
          FreeAndNil(UnresolvedLabelUsedNodes);
         end;
        finally
         FreeAndNil(LabelScope);
        end;
       finally
        FreeAndNil(LocalScope);
        LocalScope:=OriginalLocalScope;
       end;
       break;
      end else begin
       // Definition
       FreeAndNil(Parameters);
       if StorageClass=S_STATIC then begin
        Include(CurrentType^.Flags,tfStatic);
       end else begin
        Exclude(CurrentType^.Flags,tfStatic);
       end;
       if StorageClass=S_TYPEDEF then begin
        GetScope[Name]:=TPACCAbstractSyntaxTreeNode.Create(TPACCInstance(Instance),astnkTYPEDEF,CurrentType,RelevantSourceLocation);
       end else if (tfStatic in CurrentType^.Flags) and not IsGlobal then begin
        EnsureNotVoid(CurrentType);
        Variable:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable.Create(TPACCInstance(Instance),astnkGVAR,CurrentType,RelevantSourceLocation,Name,0,'@STATIC@'+IntToStr(TPasMPInterlocked.Increment(StaticCounter))+'@'+Name);
        Assert(assigned(LocalScope));
        LocalScope[Name]:=Variable;
        InitializationList:=nil;
        if CurrentState.Token^.TokenType=TOK_ASSIGN then begin
         NextToken;
         OriginalLocalScope:=LocalScope;
         try
          LocalScope:=nil;
          try
           InitializationList:=ParseDeclarationInitializer(CurrentType);
          finally
           LocalScope.Free;
          end;
         finally
          LocalScope:=OriginalLocalScope;
         end;
        end;
        Root.Children.Add(TPACCAbstractSyntaxTreeNodeDeclaration.Create(TPACCInstance(Instance),astnkDECL,nil,RelevantSourceLocation,Variable,InitializationList));
       end else begin
        EnsureNotVoid(CurrentType);
        if IsGlobal then begin
         Variable:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable.Create(TPACCInstance(Instance),astnkGVAR,CurrentType,RelevantSourceLocation,Name,0,Name);
         Assert(assigned(GlobalScope));
         GlobalScope[Name]:=Variable;
        end else begin
         Variable:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable.Create(TPACCInstance(Instance),astnkLVAR,CurrentType,RelevantSourceLocation,Name,0,'');
         Assert(assigned(LocalScope));
         LocalScope[Name]:=Variable;
         if assigned(LocalVariables) then begin
          TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Variable).Index:=LocalVariables.Add(Variable);
         end else begin
          TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Variable).Index:=-1;
         end;
        end;
        if CurrentState.Token^.TokenType=TOK_ASSIGN then begin
         NextToken;
         Block.Add(TPACCAbstractSyntaxTreeNodeDeclaration.Create(TPACCInstance(Instance),astnkDECL,nil,RelevantSourceLocation,Variable,ParseDeclarationInitializer(CurrentType)));
        end else if (StorageClass<>S_EXTERN) and (CurrentType^.Kind<>tkFUNCTION) then begin
         Block.Add(TPACCAbstractSyntaxTreeNodeDeclaration.Create(TPACCInstance(Instance),astnkDECL,nil,RelevantSourceLocation,Variable,nil));
        end;
       end;
       case CurrentState.Token^.TokenType of
        TOK_SEMICOLON:begin
         NextToken;
         break;
        end;
        TOK_COMMA:begin
         NextToken;
         continue;
        end;
        else begin
         AddError(''';'' or '','' expected',nil,true);
        end;
       end;
      end;
     until false;
    finally
     Parameters:=nil;
    end;
   end;
  end;
 end;
 function ParseTranslationUnit:TPACCAbstractSyntaxTreeNodeTranslationUnit;
 begin
  result:=TPACCAbstractSyntaxTreeNodeTranslationUnit.Create(TPACCInstance(Instance),astnkTRANSLATIONUNIT,nil,CurrentState.Token^.SourceLocation);
  try
   while CurrentState.Token^.TokenType<>TOK_EOF do begin
    case CurrentState.Token^.TokenType of
     TOK_ASM:begin
      NextToken;
      result.Children.Add(ParseAssemblerStatement(true));
     end;
     else begin
      ParseDeclaration(result.Children,true,true);
     end;
    end;
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
begin
 if Lexer.CountTokens>0 then begin
  GlobalScope:=TPACCRawByteStringHashMap.Create;
  LocalScope:=nil;
  TagScope:=TPACCRawByteStringHashMap.Create;
  LabelScope:=nil;
  LocalVariables:=nil;
  Labels:=nil;
  UnresolvedLabelUsedNodes:=nil;
  Cases.Ready:=false;
  Cases.Cases:=nil;
  Cases.Count:=0;
  Cases.DefaultCaseLabel:=nil;
  CurrentFunctionType:=nil;
  BreakLabel:=nil;
  ContinueLabel:=nil;
  PragmaPack:=-1;
  PragmaPackStack:=nil;
  PragmaPackStackPointer:=0;
  try
   CurrentState.IsEOF:=false;
   CurrentState.TokenIndex:=-1;
   NextToken;
   FreeAndNil(Root);
   try
    Root:=ParseTranslationUnit;
   except
    FreeAndNil(Root);
    Root:=TPACCAbstractSyntaxTreeNodeTranslationUnit.Create(TPACCInstance(Instance),astnkTRANSLATIONUNIT,nil,TPACCInstance(Instance).SourceLocation);
    raise;
   end;
  finally
   PragmaPackStack:=nil;
   LabelScope.Free;
   TagScope.Free;
   LocalScope.Free;
   GlobalScope.Free;
   LocalVariables.Free;
   Labels.Free;
   UnresolvedLabelUsedNodes.Free;
   Finalize(Cases);
  end;
 end;
end;

end.
