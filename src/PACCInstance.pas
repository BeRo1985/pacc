unit PACCInstance;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCRawByteStringHashMap,PACCTarget,
     PACCPreprocessor,PACCLexer,PACCParser,PACCAnalyzer,PACCHighLevelOptimizer,
     PACCLinker;

type EPACCError=class(Exception)
      public
       SourceLocation:TPACCSourceLocation;
     end;

     TPACCInstance=class
      public

       Target:TPACCTarget;

       Options:TPACCOptions;

       SourceLocation:TPACCSourceLocation;

       Preprocessor:TPACCPreprocessor;
       Lexer:TPACCLexer;
       Parser:TPACCParser;
       Analyzer:TPACCAnalyzer;
       HighLevelOptimizer:TPACCHighLevelOptimizer;
       Linker:TPACCLinker;

       TokenSymbols:TPACCTokenSymbols;
       CountTokenSymbols:TPACCInt;

       TokenSymbolStringHashMap:TPACCRawByteStringHashMap;

       FirstType:PPACCType;
       LastType:PPACCType;

       TypeVOID:PPACCType;
       TypeBOOL:PPACCType;
       TypeCHAR:PPACCType;
       TypeSHORT:PPACCType;
       TypeINT:PPACCType;
       TypeLONG:PPACCType;
       TypeLLONG:PPACCType;
       TypeUCHAR:PPACCType;
       TypeUSHORT:PPACCType;
       TypeUINT:PPACCType;
       TypeULONG:PPACCType;
       TypeULLONG:PPACCType;
       TypeFLOAT:PPACCType;
       TypeDOUBLE:PPACCType;
       TypeLDOUBLE:PPACCType;
       TypeENUM:PPACCType;

       Nodes:TList;

       AllocatedObjects:TList;

       Errors:TStringList;
       Warnings:TStringList;

       HasErrors:boolean;
       HasWarnings:boolean;

       constructor Create(const ATarget:TPACCTargetClass;const AOptions:TPACCOptions);
       destructor Destroy; override;

       procedure AddError(const Msg:TPUCUUTF8String;const AtSourceLocation:PPACCSourceLocation=nil;const DoAbort:boolean=false);
       procedure AddWarning(const Msg:TPUCUUTF8String;const AtSourceLocation:PPACCSourceLocation=nil);

       function NewType:PPACCType;

       function CopyType(const FromType:PPACCType):PPACCType;
       function CopyIncompleteType(const FromType:PPACCType):PPACCType;
       function CopyFromTypeToType(const FromType,ToType:PPACCType):PPACCType;

       function NewBuiltinType(const Kind:TPACCTypeKind;const Size,Alignment:TPACCInt;const Unsigned:boolean):PPACCType;
       function NewNumbericType(const Kind:TPACCTypeKind;const Unsigned:boolean):PPACCType;
       function NewPointerType(const PointerToType:PPACCType):PPACCType;
       function NewArrayType(const ItemType:PPACCType;const ArrayLength:int64):PPACCType;
       function NewStructType(const IsStruct:boolean):PPACCType;
       function NewFunctionType(const ReturnType:PPACCType;const ParameterTypes:TPPACCTypes;const HasVarArgs,OldStyle:boolean):PPACCType;
       function NewStubType:PPACCType;

       function IsIntType(const Type_:PPACCType):boolean;
       function IsFloatType(const Type_:PPACCType):boolean;
       function IsArithmeticType(const Type_:PPACCType):boolean;
       function IsStringType(const Type_:PPACCType):boolean;

       function SameArithmeticType(const t,u:PPACCType):boolean;

       function UsualArithmeticConversion(t,u:PPACCType):PPACCType;

       function IsSameStruct(const a,b:PPACCType):boolean;

       function AreTypesCompatible(const a,b:PPACCType):boolean;

       function AreTypesEqual(const a,b:PPACCType):boolean;

     end;

implementation

constructor TPACCInstance.Create(const ATarget:TPACCTargetClass;const AOptions:TPACCOptions);
begin
 inherited Create;

 Options:=AOptions;

 Target:=ATarget.Create(self);

 SourceLocation.Source:=-1;
 SourceLocation.Line:=0;

 TokenSymbols:=nil;
 CountTokenSymbols:=0;

 TokenSymbolStringHashMap:=TPACCRawByteStringHashMap.Create;

 FirstType:=nil;
 LastType:=nil;

 Nodes:=TList.Create;

 AllocatedObjects:=TList.Create;

 Errors:=TStringList.Create;
 Warnings:=TStringList.Create;

 HasErrors:=false;
 HasWarnings:=false;

 TypeVOID:=NewBuiltinType(tkVOID,0,0,false);
 TypeBOOL:=NewBuiltinType(tkBOOL,Target.SizeOfBool,Target.AlignmentOfBool,true);
 TypeCHAR:=NewBuiltinType(tkCHAR,Target.SizeOfChar,Target.AlignmentOfChar,false);
 TypeSHORT:=NewBuiltinType(tkSHORT,Target.SizeOfShort,Target.AlignmentOfShort,false);
 TypeINT:=NewBuiltinType(tkINT,Target.SizeOfInt,Target.AlignmentOfInt,false);
 TypeLONG:=NewBuiltinType(tkLONG,Target.SizeOfLong,Target.AlignmentOfLong,false);
 TypeLLONG:=NewBuiltinType(tkLLONG,Target.SizeOfLongLong,Target.AlignmentOfLongLong,false);
 TypeUCHAR:=NewBuiltinType(tkCHAR,Target.SizeOfChar,Target.AlignmentOfChar,true);
 TypeUSHORT:=NewBuiltinType(tkSHORT,Target.SizeOfShort,Target.AlignmentOfShort,true);
 TypeUINT:=NewBuiltinType(tkINT,Target.SizeOfInt,Target.AlignmentOfInt,true);
 TypeULONG:=NewBuiltinType(tkLONG,Target.SizeOfLong,Target.AlignmentOfLong,true);
 TypeULLONG:=NewBuiltinType(tkLLONG,Target.SizeOfLongLong,Target.AlignmentOfLongLong,true);
 TypeFLOAT:=NewBuiltinType(tkFLOAT,Target.SizeOfFloat,Target.AlignmentOfFloat,false);
 TypeDOUBLE:=NewBuiltinType(tkDOUBLE,Target.SizeOfDouble,Target.AlignmentOfDouble,false);
 TypeLDOUBLE:=NewBuiltinType(tkLDOUBLE,Target.SizeOfLongDouble,Target.AlignmentOfLongDouble,false);
 TypeENUM:=NewBuiltinType(tkENUM,Target.SizeOfEnum,Target.AlignmentOfEnum,false);

 Preprocessor:=TPACCPreprocessor.Create(self);
 Lexer:=TPACCLexer.Create(self);
 Parser:=TPACCParser.Create(self);
 Analyzer:=TPACCAnalyzer.Create(self); 
 HighLevelOptimizer:=TPACCHighLevelOptimizer.Create(self);
 Linker:=TPACCLinkerClass(Target.LinkerClass).Create(self);

end;

destructor TPACCInstance.Destroy;
var CurrentType,NextType:PPACCType;
    i:TPACCInt;
begin

 SetLength(TokenSymbols,0);

 TokenSymbolStringHashMap.Free;

 Linker.Free;

 HighLevelOptimizer.Free;

 Analyzer.Free;

 Parser.Free;

 Lexer.Free;

 Preprocessor.Free;

 for i:=Nodes.Count-1 downto 0 do begin
  TObject(Nodes[i]).Free;
 end;
 Nodes.Free;

 for i:=AllocatedObjects.Count-1 downto 0 do begin
  TObject(AllocatedObjects[i]).Free;
 end;
 AllocatedObjects.Free;

 CurrentType:=FirstType;
 while assigned(CurrentType) do begin
  NextType:=CurrentType^.Next;
  Finalize(CurrentType^);
  FreeMem(CurrentType);
  CurrentType:=NextType;
 end;

 FirstType:=nil;
 LastType:=nil;

 Errors.Free;
 Warnings.Free;

 Target.Free;
 
 inherited Destroy;
end;

procedure TPACCInstance.AddError(const Msg:TPUCUUTF8String;const AtSourceLocation:PPACCSourceLocation=nil;const DoAbort:boolean=false);
var CurrentSourceLocation:PPACCSourceLocation;
    e:EPACCError;
begin
 if assigned(AtSourceLocation) then begin
  CurrentSourceLocation:=AtSourceLocation;
 end else begin
  CurrentSourceLocation:=@SourceLocation;
 end;
 if CurrentSourceLocation^.Source>=0 then begin
  Errors.Add(Preprocessor.SourceFiles[CurrentSourceLocation^.Source]+':'+IntToStr(CurrentSourceLocation^.Line+1)+':'+IntToStr(CurrentSourceLocation^.Column+1)+': error: '+Msg);
 end else begin
  Errors.Add('?:'+IntToStr(CurrentSourceLocation^.Line+1)+':'+IntToStr(CurrentSourceLocation^.Column+1)+': error: '+Msg);
 end;
 HasErrors:=true;
 if DoAbort then begin
  e:=EPACCError.Create(Msg);
  if assigned(AtSourceLocation) then begin
   e.SourceLocation:=AtSourceLocation^;
  end else begin
   e.SourceLocation:=SourceLocation;
  end;
  raise e;
 end;
end;

procedure TPACCInstance.AddWarning(const Msg:TPUCUUTF8String;const AtSourceLocation:PPACCSourceLocation=nil);
var CurrentSourceLocation:PPACCSourceLocation;
begin
 if Options.EnableWarnings then begin
  if Options.WarningsAreErrors then begin
   AddError(Msg,AtSourceLocation);
  end else begin
   if assigned(AtSourceLocation) then begin
    CurrentSourceLocation:=AtSourceLocation;
   end else begin
    CurrentSourceLocation:=@SourceLocation;
   end;
   if CurrentSourceLocation^.Source>=0 then begin
    Warnings.Add(Preprocessor.SourceFiles[CurrentSourceLocation^.Source]+':'+IntToStr(CurrentSourceLocation^.Line+1)+':'+IntToStr(CurrentSourceLocation^.Column+1)+': warning: '+Msg);
   end else begin
    Warnings.Add('?:'+IntToStr(CurrentSourceLocation^.Line+1)+':'+IntToStr(CurrentSourceLocation^.Column+1)+': warning: '+Msg);
   end;
   HasWarnings:=true;
  end;
 end;
end;

function TPACCInstance.NewType:PPACCType;
begin
 GetMem(result,SizeOf(TPACCType));
 FillChar(result^,SizeOf(TPACCType),#0);
 result^.Previous:=LastType;
 if assigned(LastType) then begin
  LastType^.Next:=result;
 end else begin
  FirstType:=result;
 end;
 LastType:=result;
 result^.CallingConvention:=-1;
end;

function TPACCInstance.CopyType(const FromType:PPACCType):PPACCType;
begin
 GetMem(result,SizeOf(TPACCType));
 FillChar(result^,SizeOf(TPACCType),#0);
 result^:=FromType^;
 result^.Fields:=copy(FromType^.Fields);
 result^.Previous:=LastType;
 result^.Next:=nil;
 if assigned(LastType) then begin
  LastType^.Next:=result;
 end else begin
  FirstType:=result;
 end;
 LastType:=result;
end;

function TPACCInstance.CopyIncompleteType(const FromType:PPACCType):PPACCType;
begin
 if not assigned(FromType) then begin
  result:=nil;
 end else if FromType^.ArrayLength<0 then begin
  result:=CopyType(FromType);
 end else begin
  result:=FromType;
 end;
end;

function TPACCInstance.CopyFromTypeToType(const FromType,ToType:PPACCType):PPACCType;
begin
 result:=ToType;
 if assigned(FromType) then begin
  result^.Kind:=FromType^.Kind;
  result^.Size:=FromType^.Size;
  result^.Alignment:=FromType^.Alignment;
  result^.Flags:=FromType^.Flags;
  result^.ChildType:=FromType^.ChildType;
  result^.ArrayLength:=FromType^.ArrayLength;
  result^.Fields:=copy(FromType^.Fields);
  result^.Offset:=FromType^.Offset;
  result^.MinValue:=FromType^.MinValue;
  result^.MaxValue:=FromType^.MaxValue;
  result^.BitOffset:=FromType^.BitOffset;
  result^.BitSize:=FromType^.BitSize;
  result^.ReturnType:=FromType^.ReturnType;
  result^.Parameters:=FromType^.Parameters;
  result^.CallingConvention:=FromType^.CallingConvention;
 end;
end;

function TPACCInstance.NewBuiltinType(const Kind:TPACCTypeKind;const Size,Alignment:TPACCInt;const Unsigned:boolean):PPACCType;
begin
 result:=NewType;
 result^.Kind:=Kind;
 if Unsigned then begin
  Include(result^.Flags,tfUnsigned);
 end else begin
  Exclude(result^.Flags,tfUnsigned);
 end;
 result^.Size:=Size;
 result^.Alignment:=Alignment;
end;

function TPACCInstance.NewNumbericType(const Kind:TPACCTypeKind;const Unsigned:boolean):PPACCType;
begin
 result:=NewType;
 result^.Kind:=Kind;
 if Unsigned then begin
  Include(result^.Flags,tfUnsigned);
 end else begin
  Exclude(result^.Flags,tfUnsigned);
 end;
 case Kind of
  tkVOID:begin
   result^.Size:=0;
   result^.Alignment:=0;
  end;
  tkBOOL:begin
   result^.Size:=Target.SizeOfBool;
   result^.Alignment:=Target.SizeOfBool;
  end;
  tkCHAR:begin
   result^.Size:=Target.SizeOfChar;
   result^.Alignment:=Target.SizeOfChar;
  end;
  tkSHORT:begin
   result^.Size:=Target.SizeOfShort;
   result^.Alignment:=Target.SizeOfShort;
  end;
  tkINT:begin
   result^.Size:=Target.SizeOfInt;
   result^.Alignment:=Target.SizeOfInt;
  end;
  tkLONG:begin
   result^.Size:=Target.SizeOfLong;
   result^.Alignment:=Target.SizeOfLong;
  end;
  tkLLONG:begin
   result^.Size:=Target.SizeOfLongLong;
   result^.Alignment:=Target.SizeOfLongLong;
  end;
  tkFLOAT:begin
   result^.Size:=Target.SizeOfFloat;
   result^.Alignment:=Target.SizeOfFloat;
  end;
  tkDOUBLE:begin
   result^.Size:=Target.SizeOfDouble;
   result^.Alignment:=Target.SizeOfDouble;
  end;
  tkLDOUBLE:begin
   result^.Size:=Target.SizeOfLongDouble;
   result^.Alignment:=Target.SizeOfLongDouble;
  end;
  else begin
   AddError('Internal error 2016-12-30-23-11-0000');
  end;
 end;
end;

function TPACCInstance.NewPointerType(const PointerToType:PPACCType):PPACCType;
begin
 result:=NewType;
 result^.Kind:=tkPOINTER;
 result^.Size:=Target.SizeOfPointer;
 result^.Alignment:=Target.SizeOfPointer;
 result^.ChildType:=PointerToType;
end;

function TPACCInstance.NewArrayType(const ItemType:PPACCType;const ArrayLength:int64):PPACCType;
var Size:int64;
begin
 if ArrayLength<0 then begin
  Size:=-1;
 end else begin
  Size:=ArrayLength*ItemType^.Size;
 end;
 result:=NewType;
 result^.Kind:=tkARRAY;
 result^.ChildType:=ItemType;
 result^.ArrayLength:=ArrayLength;
 result^.Size:=Size;
 result^.Alignment:=ItemType^.Alignment;
end;

function TPACCInstance.NewStructType(const IsStruct:boolean):PPACCType;
begin
 result:=NewType;
 result^.Kind:=tkSTRUCT;
 if IsStruct then begin
  Include(result^.Flags,tfStruct);
 end else begin
  Exclude(result^.Flags,tfStruct);
 end;
end;

function TPACCInstance.NewFunctionType(const ReturnType:PPACCType;const ParameterTypes:TPPACCTypes;const HasVarArgs,OldStyle:boolean):PPACCType;
begin
 result:=NewType;
 result^.Kind:=tkFUNCTION;
 if HasVarArgs then begin
  Include(result^.Flags,tfVarArgs);
 end else begin
  Exclude(result^.Flags,tfVarArgs);
 end;
 if OldStyle then begin
  Include(result^.Flags,tfOldStyle);
 end else begin
  Exclude(result^.Flags,tfOldStyle);
 end;
 result^.ReturnType:=ReturnType;
end;

function TPACCInstance.NewStubType:PPACCType;
begin
 result:=NewType;
 result^.Kind:=tkSTUB;
end;

function TPACCInstance.IsIntType(const Type_:PPACCType):boolean;
begin
 result:=Type_^.Kind in [tkBOOL,tkCHAR,tkSHORT,tkINT,tkLONG,tkLLONG];
end;

function TPACCInstance.IsFloatType(const Type_:PPACCType):boolean;
begin
 result:=Type_^.Kind in [tkFLOAT,tkDOUBLE,tkLDOUBLE];
end;

function TPACCInstance.IsArithmeticType(const Type_:PPACCType):boolean;
begin
 result:=Type_^.Kind in [tkBOOL,tkCHAR,tkSHORT,tkINT,tkLONG,tkLLONG,tkFLOAT,tkDOUBLE,tkLDOUBLE];
end;

function TPACCInstance.IsStringType(const Type_:PPACCType):boolean;
begin
 result:=(Type_^.Kind=tkARRAY) and assigned(Type_^.ChildType) and (Type_^.ChildType^.Kind=tkCHAR);
end;

function TPACCInstance.SameArithmeticType(const t,u:PPACCType):boolean;
begin
 result:=(t^.Kind=u^.Kind) and ((tfUnsigned in t^.Flags)=(tfUnsigned in u^.Flags));
end;

function TPACCInstance.UsualArithmeticConversion(t,u:PPACCType):PPACCType;
var z:PPACCType;
begin
 if not IsArithmeticType(t) then begin
  AddError('Internal error 2017-01-01-20-17-0000');
 end;
 if not IsArithmeticType(u) then begin
  AddError('Internal error 2017-01-01-20-17-0001');
 end;
 if t^.Kind<u^.Kind then begin
  z:=t;
  t:=u;
  u:=z;
 end;
 if IsFloatType(t) then begin
  result:=t;
 end else begin
  if not (IsIntType(t) and (t^.Size>=TypeINT^.Size)) then begin
   AddError('Internal error 2017-01-01-20-17-0002');
  end;
  if not (IsIntType(u) and (u^.Size>=TypeINT^.Size)) then begin
   AddError('Internal error 2017-01-01-20-18-0000');
  end;
  if t^.Size>u^.Size then begin
   result:=t;
  end else if t^.Size<u^.Size then begin
   AddError('Internal error 2017-01-01-20-19-0000');
  end else if (tfUnsigned in t^.Flags)=(tfUnsigned in u^.Flags) then begin
   result:=t;
  end else begin
   result:=CopyType(t);
   Include(result^.Flags,tfUnsigned);
  end;
 end;
end;

function TPACCInstance.IsSameStruct(const a,b:PPACCType):boolean;
var Index:TPACCInt;
begin
 if a^.Kind=b^.Kind then begin
  case a^.Kind of
   tkARRAY:begin
    result:=(a^.ArrayLength=b^.ArrayLength) and IsSameStruct(a^.ChildType,b^.ChildType);
   end;
   tkPOINTER:begin
    result:=IsSameStruct(a^.ChildType,b^.ChildType);
   end;
   tkSTRUCT:begin
    if ((tfStruct in a^.Flags)=(tfStruct in b^.Flags)) and
       (length(a^.Fields)=length(b^.Fields)) then begin
     result:=true;
     for Index:=0 to length(a^.Fields)-1 do begin
      if not IsSameStruct(a^.Fields[Index].Type_,b^.Fields[Index].Type_) then begin
       result:=false;
       break;
      end;
     end;
    end else begin
     result:=false;
    end;
   end;
   else begin
    result:=true;
   end;
  end;
 end else begin
  result:=false;
 end;
end;

function TPACCInstance.AreTypesCompatible(const a,b:PPACCType):boolean;
begin
 if a^.Kind=tkSTRUCT then begin
  result:=IsSameStruct(a,b);
 end else if a^.Kind=b^.Kind then begin
  if assigned(a^.ChildType) and assigned(b^.ChildType) then begin
   result:=AreTypesCompatible(a^.ChildType,b^.ChildType);
  end else if IsArithmeticType(a) and IsArithmeticType(b) then begin
   result:=SameArithmeticType(a,b);
  end else begin
   result:=true;
  end;
 end else begin
  result:=false;
 end;
end;
                                    
function TPACCInstance.AreTypesEqual(const a,b:PPACCType):boolean;
begin
 if a^.Kind=tkSTRUCT then begin
  result:=IsSameStruct(a,b);
 end else if a^.Kind=b^.Kind then begin
  if assigned(a^.ChildType) and assigned(b^.ChildType) then begin
   result:=AreTypesEqual(a^.ChildType,b^.ChildType);
  end else if IsArithmeticType(a) and IsArithmeticType(b) then begin
   result:=SameArithmeticType(a,b);
  end else if (a^.Size=b^.Size) and ((tfUnsigned in a^.Flags)=(tfUnsigned in b^.Flags)) then begin
   result:=true;
  end else begin
   result:=false;
  end;
 end else begin
  result:=false;
 end;
end;

end.
