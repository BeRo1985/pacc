unit PACCTarget_x86_32;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCAbstractSyntaxTree,PACCTarget;

const ccCDECL=0;
      ccSTDCALL=1;
      ccFASTCALL=2;

type TPACCTarget_x86_32=class(TPACCTarget)
      private

      public

       constructor Create(const AInstance:TObject); override;
       destructor Destroy; override;

       class function GetName:TPACCRawByteString; override;

       function CheckCallingConvention(const AName:TPACCRawByteString):TPACCInt32; override;

       procedure GenerateCode(const ARoot:TPACCAbstractSyntaxTreeNode;const AOutputStream:TStream); override;

       procedure AssembleCode(const AInputStream,AOutputStream:TStream;const AInputFileName:TPUCUUTF8String=''); override;

       procedure LinkCode(const AInputStreams:TList;const AInputFileNames:TStringList;const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String=''); override;

     end;

     TPACCTarget_x86_32_COFF_PE=class(TPACCTarget_x86_32)
      private

      public

       constructor Create(const AInstance:TObject); override;

       class function GetName:TPACCRawByteString; override;

       function GetDefaultOutputExtension:TPACCRawByteString; override;

     end;

     TPACCTarget_x86_32_ELF_ELF=class(TPACCTarget_x86_32)
      private

      public

       constructor Create(const AInstance:TObject); override;

       class function GetName:TPACCRawByteString; override;

       function GetDefaultOutputExtension:TPACCRawByteString; override;

     end;

implementation

uses PACCInstance,PACCPreprocessor,SASMCore,PACCLinker_COFF_PE,PACCLinker_ELF_ELF;

constructor TPACCTarget_x86_32.Create(const AInstance:TObject);
begin
 inherited Create(AInstance);

 PreprocessorCode:='#define LP32'#10+
                   '#define __LP32__'#10+
                   '#define __i386'#10+
                   '#define __i386__'#10+
                   '#define __cdecl __attribute__((cdecl))'#10+
                   '#define __stdcall __attribute__((stdcall))'#10+
                   '#define __fastcall __attribute__((fastcall))'#10+
                   '';

 SizeOfPointer:=4;
 SizeOf_PTRDIFF_T:=4;
 SizeOf_SIZE_T:=4;
 SizeOfBool:=1;
 SizeOfChar:=1;
 SizeOfShort:=2;
 SizeOfInt:=4;
 SizeOfLong:=8;
 SizeOfLongLong:=8;
 SizeOfFloat:=4;
 SizeOfDouble:=8;
 SizeOfLongDouble:=8;
 SizeOfEnum:=4;

 AlignmentOfPointer:=4;
 AlignmentOf_PTRDIFF_T:=4;
 AlignmentOf_SIZE_T:=4;
 AlignmentOfBool:=1;
 AlignmentOfChar:=1;
 AlignmentOfShort:=2;
 AlignmentOfInt:=4;
 AlignmentOfLong:=8;
 AlignmentOfLongLong:=8;
 AlignmentOfFloat:=4;
 AlignmentOfDouble:=8;
 AlignmentOfLongDouble:=8;
 AlignmentOfEnum:=4;

 MaximumAlignment:=16;

end;

destructor TPACCTarget_x86_32.Destroy;
begin
 inherited Destroy;
end;

class function TPACCTarget_x86_32.GetName:TPACCRawByteString;
begin
 result:='x86_32';
end;

function TPACCTarget_x86_32.CheckCallingConvention(const AName:TPACCRawByteString):TPACCInt32;
begin
 if (AName='cdecl') or (AName='__cdecl') or (AName='__cdecl__') then begin
  result:=ccCDECL;
 end else if (AName='stdcall') or (AName='__stdcall') or (AName='__stdcall__') then begin
  result:=ccSTDCALL;
 end else if (AName='fastcall') or (AName='__fastcall') or (AName='__fastcall__') then begin
  result:=ccFASTCALL;
 end else begin
  result:=-1;
 end;
end;

procedure TPACCTarget_x86_32.GenerateCode(const ARoot:TPACCAbstractSyntaxTreeNode;const AOutputStream:TStream);
type PCodeLevel=^TCodeLevel;
     TCodeLevel=record
      TextSectionStringList:TStringList;
      DataSectionStringList:TStringList;
      BSSSectionStringList:TStringList;
     end;
     TCodeLevels=array of TCodeLevel;
var CountCodeLevels,SectionCounter:TPACCInt32;
    CodeStringList:TStringList;
    CurrentFunction:TPACCAbstractSyntaxTreeNode;
    CodeLevels:TCodeLevels;
 procedure InitializeCodeLevels;
 begin
  CodeLevels:=nil;
 end;
 procedure FinalizeCodeLevels;
 var Index:TPACCInt32;
     CodeLevel:PCodeLevel;
 begin
  for Index:=0 to CountCodeLevels-1 do begin
   CodeLevel:=@CodeLevels[Index];
   FreeAndNil(CodeLevel^.TextSectionStringList);
   FreeAndNil(CodeLevel^.DataSectionStringList);
   FreeAndNil(CodeLevel^.BSSSectionStringList);
  end;
  SetLength(CodeLevels,0);
 end;
 function GetCodeLevel(const Depth:TPACCInt32):PCodeLevel;
 var Index,OldCountCodeLevels:TPACCInt32;
 begin
  if CountCodeLevels<=Depth then begin
   if length(CodeLevels)<=Depth then begin
    OldCountCodeLevels:=length(CodeLevels);
    SetLength(CodeLevels,(Depth+1)*2);
    FillChar(CodeLevels[OldCountCodeLevels],(length(CodeLevels)-OldCountCodeLevels)*SizeOf(TCodeLevel),#0);
   end;
   OldCountCodeLevels:=CountCodeLevels;
   CountCodeLevels:=Depth+1;
   for Index:=OldCountCodeLevels to CountCodeLevels-1 do begin
    result:=@CodeLevels[Depth];
    result^.TextSectionStringList:=TStringList.Create;
    result^.DataSectionStringList:=TStringList.Create;
    result^.BSSSectionStringList:=TStringList.Create;
   end;
  end;
  result:=@CodeLevels[Depth];
 end;
 function GetNodeLabelName(const Node:TPACCAbstractSyntaxTreeNode):TPACCRawByteString;
 begin
  result:='$node$label_'+IntToStr(TPACCPtrUInt(pointer(Node)));
 end;
 procedure EmitPrimitiveTypeData(const Type_:PPACCType;const Node:TPACCAbstractSyntaxTreeNode;const Depth:TPACCInt32);
 var f:TPACCFloat;
     d:TPACCDouble;
     i32:TPACCInt32;
     i64:TPACCInt64;
     BaseNode:TPACCAbstractSyntaxTreeNode;
     BaseType:PPACCType;
 begin
  case Type_^.Kind of
   tkFLOAT:begin
    f:=TPACCInstance(Instance).EvaluateFloatExpression(Node,tkFLOAT);
    GetCodeLevel(Depth)^.DataSectionStringList.Add('dd 0x'+IntToHex(TPACCUInt32(pointer(@f)^),8));
   end;
   tkDOUBLE:begin
    d:=TPACCInstance(Instance).EvaluateFloatExpression(Node,tkDOUBLE);
    GetCodeLevel(Depth)^.DataSectionStringList.Add('dq 0x'+IntToHex(TPACCUInt64(pointer(@d)^),16));
   end;
   tkBOOL:begin
    GetCodeLevel(Depth)^.DataSectionStringList.Add('db '+IntToStr(TPACCInstance(Instance).EvaluateIntegerExpression(Node,nil)));
   end;
   tkCHAR:begin
    GetCodeLevel(Depth)^.DataSectionStringList.Add('db '+IntToStr(TPACCInstance(Instance).EvaluateIntegerExpression(Node,nil)));
   end;
   tkSHORT:begin
    GetCodeLevel(Depth)^.DataSectionStringList.Add('dw '+IntToStr(TPACCInstance(Instance).EvaluateIntegerExpression(Node,nil)));
   end;
   tkINT:begin
    GetCodeLevel(Depth)^.DataSectionStringList.Add('dd '+IntToStr(TPACCInstance(Instance).EvaluateIntegerExpression(Node,nil)));
   end;
   tkLONG,tkLLONG:begin
    GetCodeLevel(Depth)^.DataSectionStringList.Add('dq '+IntToStr(TPACCInstance(Instance).EvaluateIntegerExpression(Node,nil)));
   end;
   tkPOINTER:begin
    if Node.Kind=astnkOP_LABEL_ADDR then begin
     GetCodeLevel(Depth)^.DataSectionStringList.Add('dd offset '+GetNodeLabelName(TPACCAbstractSyntaxTreeNodeLabel(TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).Label_)));
    end else if (Node is TPACCAbstractSyntaxTreeNodeUnaryOperator) and
                (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Kind=astnkSTRING) and
                (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_.Kind=tkARRAY) and
                (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_.ChildType^.Kind=tkCHAR) then begin
     GetCodeLevel(Depth+1)^.DataSectionStringList.Add('');
     GetCodeLevel(Depth+1)^.DataSectionStringList.Add('.align('+IntToStr(Max(4,TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_.ChildType^.Alignment))+')');
     GetCodeLevel(Depth+1)^.DataSectionStringList.Add('$_stringlabel_'+IntToStr(TPACCPtrUInt(pointer(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand)))+':');
     for i32:=1 to length(TPACCAbstractSyntaxTreeNodeStringValue(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand).Value) do begin
      GetCodeLevel(Depth+1)^.DataSectionStringList.Add('db '+IntToStr(TPACCUInt8(AnsiChar(TPACCAbstractSyntaxTreeNodeStringValue(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand).Value[i32]))));
     end;
     case TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_.ChildType^.Size of
      2:begin
       GetCodeLevel(Depth+1)^.DataSectionStringList.Add('dw 0');
      end;
      4:begin
       GetCodeLevel(Depth+1)^.DataSectionStringList.Add('dd 0');
      end;
      8:begin
       GetCodeLevel(Depth+1)^.DataSectionStringList.Add('dq 0');
      end;
      else begin
       GetCodeLevel(Depth+1)^.DataSectionStringList.Add('db 0');
      end;
     end;
     GetCodeLevel(Depth)^.DataSectionStringList.Add('dd offset $_stringlabel_'+IntToStr(TPACCPtrUInt(pointer(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand))));
    end else if Node.Kind=astnkGVAR then begin
     GetCodeLevel(Depth)^.DataSectionStringList.Add('dd offset '+GetNodeLabelName(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)));
    end else begin
     BaseNode:=nil;
     i64:=TPACCInstance(Instance).EvaluateIntegerExpression(Node,@BaseNode);
     if assigned(BaseNode) then begin
      BaseType:=BaseNode.Type_;
      if BaseNode.Kind in [astnkCONV,astnkADDR] then begin
       BaseNode:=TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand;
      end;
      if BaseNode.Kind=astnkGVAR then begin
       Assert(assigned(BaseType.ChildType));
       GetCodeLevel(Depth)^.DataSectionStringList.Add('dd offset '+GetNodeLabelName(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))+' + '+IntToStr(i64*BaseType.ChildType^.Size));
      end else begin
       TPACCInstance(Instance).AddError('Global variable expected',@BaseNode.SourceLocation,true);
      end;
     end else begin
      GetCodeLevel(Depth)^.DataSectionStringList.Add('dd '+IntToStr(TPACCUInt32(i64) and $ffffffff));
     end;
    end;
   end;
   else begin
    TPACCInstance(Instance).AddError('Internal error 2017-01-17-10-45-0000',@Node.SourceLocation,true);
   end;
  end;
 end;
 procedure EmitInitializerList(const Nodes:TPACCAbstractSyntaxTreeNodeList;Size,Offset:TPACCInt64;const Depth:TPACCInt32);
 var Index:TPACCInt32;
     Node:TPACCAbstractSyntaxTreeNodeInitializer;
     Value:TPACCAbstractSyntaxTreeNode;
     Delta,Data:TPACCInt64;
     ToType:PPACCType;
 begin
  Index:=0;
  while Index<Nodes.Count do begin
   Node:=TPACCAbstractSyntaxTreeNodeInitializer(Nodes[Index]);
   Value:=Node.InitializionValue;
   Delta:=Node.InitializionOffset-Offset;
   if Delta>0 then begin
    GetCodeLevel(Depth)^.DataSectionStringList.Add('db '+IntToStr(Delta)+' dup(0)');
   end;
   if Node.ToType^.BitSize>0 then begin
    if Node.ToType^.BitOffset=0 then begin
     Data:=TPACCInstance(Instance).EvaluateIntegerExpression(Value,nil);
     ToType:=Node.ToType;
     inc(Index);
     while Index<Nodes.Count do begin
      Node:=TPACCAbstractSyntaxTreeNodeInitializer(Nodes[Index]);
      if Node.Type_.BitSize<=0 then begin
       break;
      end else begin
       Value:=Node.InitializionValue;
       ToType:=Node.ToType;
       Data:=Data or ((TPACCInstance(Instance).EvaluateIntegerExpression(Value,nil) and ((TPACCInt64(1) shl (ToType^.BitSize))-1)) shl ToType^.BitOffset);
       inc(Index);
      end;
     end;
     case ToType^.Kind of
      tkBOOL:begin
       GetCodeLevel(Depth)^.DataSectionStringList.Add('db '+IntToStr(Data));
      end;
      tkCHAR:begin
       GetCodeLevel(Depth)^.DataSectionStringList.Add('db '+IntToStr(Data));
      end;
      tkSHORT:begin
       GetCodeLevel(Depth)^.DataSectionStringList.Add('dw '+IntToStr(Data));
      end;
      tkINT:begin
       GetCodeLevel(Depth)^.DataSectionStringList.Add('dd '+IntToStr(Data));
      end;
      tkLONG,tkLLONG:begin
       GetCodeLevel(Depth)^.DataSectionStringList.Add('dq '+IntToStr(Data));
      end;
      else begin
       TPACCInstance(Instance).AddError('Internal error 2017-01-17-11-27-0000',@Node.SourceLocation,true);
      end;
     end;
     inc(Offset,ToType^.Size);
     dec(Size,ToType^.Size);
     if Index=Nodes.Count then begin
      break;
     end;
    end else begin
     TPACCInstance(Instance).AddError('Internal error 2017-01-17-11-21-0000',@Node.SourceLocation,true);
    end;
   end else begin
    inc(Offset,Node.ToType^.Size);
    dec(Size,Node.ToType^.Size);
   end;
   if Value.Kind=astnkADDR then begin
    case TPACCAbstractSyntaxTreeNodeUnaryOperator(Value).Operand.Kind of
     astnkLVAR:begin
      GetCodeLevel(Depth+1)^.DataSectionStringList.Add('');
      GetCodeLevel(Depth+1)^.DataSectionStringList.Add('.align('+IntToStr(Max(4,TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Value).Operand).Type_.Alignment))+')');
      GetCodeLevel(Depth+1)^.DataSectionStringList.Add(GetNodeLabelName(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Value).Operand))+':');
      EmitInitializerList(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Value).Operand).LocalVariableInitialization,
                          TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Value).Operand).Type_.Size,
                           0,
                           Depth+1);
      GetCodeLevel(Depth)^.DataSectionStringList.Add('dd offset '+GetNodeLabelName(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Value).Operand)));
     end;
     astnkGVAR:begin
      GetCodeLevel(Depth)^.DataSectionStringList.Add('dd offset '+GetNodeLabelName(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Value).Operand)));
     end;
     else begin
      TPACCInstance(Instance).AddError('Internal error 2017-01-17-11-06-0000',@Node.SourceLocation,true);
     end;
    end;
   end else if (Value.Kind=astnkLVAR) and assigned(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Value).LocalVariableInitialization) then begin
    EmitInitializerList(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Value).LocalVariableInitialization,
                        TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Value).Type_.Size,
                        0,
                        Depth);
   end else begin
    EmitPrimitiveTypeData(Node.ToType,Node.InitializionValue,Depth);
   end;
   inc(Index);
  end;
  if Size>0 then begin
   GetCodeLevel(Depth)^.DataSectionStringList.Add('db '+IntToStr(Size)+' dup(0)');
  end;
 end;
 procedure EmitDeclaration(const Node:TPACCAbstractSyntaxTreeNodeDeclaration);
 const Depth=0;
 var Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;
 begin
  if assigned(Node.DeclarationVariable) then begin
   if Node.DeclarationVariable.Kind=astnkGVAR then begin
    Variable:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node.DeclarationVariable);
    if assigned(Node.DeclarationInitialization) then begin
     GetCodeLevel(Depth)^.DataSectionStringList.Add('');
     GetCodeLevel(Depth)^.DataSectionStringList.Add('.align('+IntToStr(Max(1,Variable.Type_^.Alignment))+')');
     if not ((tfStatic in Variable.Type_^.Flags) or assigned(CurrentFunction)) then begin
      GetCodeLevel(Depth)^.DataSectionStringList.Add('.public('+GetNodeLabelName(Variable)+' = "'+Variable.VariableName+'")');
     end;
     GetCodeLevel(Depth)^.DataSectionStringList.Add(GetNodeLabelName(Variable)+':');
     EmitInitializerList(Node.DeclarationInitialization,0,0,0);
    end else begin
     GetCodeLevel(Depth)^.BSSSectionStringList.Add('');
     GetCodeLevel(Depth)^.BSSSectionStringList.Add('.align('+IntToStr(Max(1,Variable.Type_^.Alignment))+')');
     if not ((tfStatic in Variable.Type_^.Flags) or assigned(CurrentFunction)) then begin
      GetCodeLevel(Depth)^.BSSSectionStringList.Add('.public('+GetNodeLabelName(Variable)+' = "'+Variable.VariableName+'")');
     end;
     GetCodeLevel(Depth)^.BSSSectionStringList.Add(GetNodeLabelName(Variable)+':');
     GetCodeLevel(Depth)^.BSSSectionStringList.Add('resb '+IntToStr(Variable.Type_^.Size));
    end;
   end else begin
    if assigned(CurrentFunction) and (Node.DeclarationVariable.Kind=astnkLVAR) then begin
     
    end else begin
     TPACCInstance(Instance).AddError('Internal error 2017-01-17-09-08-0000',@Node.SourceLocation,true);
    end;
   end;
  end else begin
   TPACCInstance(Instance).AddError('Internal error 2017-01-17-09-07-0000',@Node.SourceLocation,true);
  end;
 end;
 procedure EmitFunction(const Node:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration);
 var Index,ReturnSize,RemainingRegisterSize,Size:TPACCInt32;
     Parameter:TPACCAbstractSyntaxTreeNode;
 begin
  for Index:=0 to Node.Parameters.Count-1 do begin
   Parameter:=Node.Parameters[Index];
   if assigned(Parameter) then begin
   end;
  end;
  GetCodeLevel(0)^.TextSectionStringList.Add('');
  GetCodeLevel(0)^.TextSectionStringList.Add('.align(16)');
  if not ((tfStatic in Node.Type_^.Flags) or (afInline in Node.Type_^.Attribute.Flags)) then begin
   GetCodeLevel(0)^.TextSectionStringList.Add('.public('+GetNodeLabelName(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node.Variable))+' = "'+Node.FunctionName+'")');
  end;
  GetCodeLevel(0)^.TextSectionStringList.Add(GetNodeLabelName(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node.Variable))+':');
  GetCodeLevel(0)^.TextSectionStringList.Add('push ebp');
  GetCodeLevel(0)^.TextSectionStringList.Add('mov ebp,esp');
  // ..
  GetCodeLevel(0)^.TextSectionStringList.Add('mov esp,ebp');
  GetCodeLevel(0)^.TextSectionStringList.Add('pop ebp');
  if (Node.FunctionName='main') and (Node.Type_^.ReturnType^.Kind=tkVOID) then begin
   GetCodeLevel(0)^.TextSectionStringList.Add('xor eax,eax');
  end;
  case Node.Type_^.Attribute.CallingConvention of
   ccSTDCALL:begin
    ReturnSize:=0;
    for Index:=0 to Node.Parameters.Count-1 do begin
     Parameter:=Node.Parameters[Index];
     case Parameter.Type_^.Size of
      1,2,4:begin
       inc(ReturnSize,4);
      end;
      8:begin
       inc(ReturnSize,8);
      end;
      else begin
       inc(ReturnSize,(Parameter.Type_^.Size+3) and not TPACCInt32(3));
      end;
     end;
    end;
   end;
   ccFASTCALL:begin
    // Microsoft or GCC __fastcall convention passes the first two arguments (evaluated left to right)
    // that fit into ECX and EDX. Remaining arguments are pushed onto the stack from right to left.
    RemainingRegisterSize:=8;
    ReturnSize:=0;
    for Index:=0 to Node.Parameters.Count-1 do begin
     Parameter:=Node.Parameters[Index];
     case Parameter.Type_^.Size of
      1,2,4:begin
       if RemainingRegisterSize>=4 then begin
        dec(ReturnSize,4);
       end else begin
        inc(ReturnSize,4);
       end;
      end;
      8:begin
       if RemainingRegisterSize>=8 then begin
        dec(ReturnSize,8);
       end else begin
        inc(ReturnSize,8);
       end;
      end;
      else begin
       Size:=(Parameter.Type_^.Size+3) and not TPACCInt32(3);
       if RemainingRegisterSize>=Size then begin
        dec(ReturnSize,Size);
       end else begin
        inc(ReturnSize,Size);
       end;
      end;
     end;
    end;
   end;
   else {ccCDECL:}begin
    ReturnSize:=0; // Because it is the callee task in this case
   end;
  end;
  if ReturnSize>0 then begin
   GetCodeLevel(0)^.TextSectionStringList.Add('ret '+IntToStr(ReturnSize));
  end else begin
   GetCodeLevel(0)^.TextSectionStringList.Add('ret');
  end;
 end;
 procedure ProcessRootNode(const Node:TPACCAbstractSyntaxTreeNode);
 begin
  case Node.Kind of
   astnkDECL:begin
    EmitDeclaration(TPACCAbstractSyntaxTreeNodeDeclaration(Node));
   end;
   astnkFUNC:begin
    EmitFunction(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node));
   end;
   else begin
    TPACCInstance(Instance).AddError('Internal error 2017-01-17-09-01-0000',@Node.SourceLocation,true);
   end;
  end;
 end;
var Index,LineIndex:TPACCInt32;
    TextSectionStringList,DataSectionStringList,BSSSectionStringList:TStringList;
    CodeLevel:PCodeLevel;
begin
 CodeStringList:=TStringList.Create;
 try
  TextSectionStringList:=TStringList.Create;
  try
   DataSectionStringList:=TStringList.Create;
   try
    BSSSectionStringList:=TStringList.Create;
    try
     CurrentFunction:=nil;
     SectionCounter:=1;
     InitializeCodeLevels;
     try
      for Index:=0 to TPACCAbstractSyntaxTreeNodeStatements(ARoot).Children.Count-1 do begin
       ProcessRootNode(TPACCAbstractSyntaxTreeNodeStatements(ARoot).Children[Index]);
      end;
      for Index:=0 to CountCodeLevels-1 do begin
       CodeLevel:=@CodeLevels[Index];
       for LineIndex:=0 to CodeLevel^.TextSectionStringList.Count-1 do begin
        TextSectionStringList.Add('  '+CodeLevel^.TextSectionStringList[LineIndex]);
       end;
       for LineIndex:=0 to CodeLevel^.DataSectionStringList.Count-1 do begin
        DataSectionStringList.Add('  '+CodeLevel^.DataSectionStringList[LineIndex]);
       end;
       for LineIndex:=0 to CodeLevel^.BSSSectionStringList.Count-1 do begin
        BSSSectionStringList.Add('  '+CodeLevel^.BSSSectionStringList[LineIndex]);
       end;
      end;
     finally
      FinalizeCodeLevels;
     end;
     if self is TPACCTarget_x86_32_COFF_PE then begin
      CodeStringList.Add('.cpu(all)');
      CodeStringList.Add('');
      CodeStringList.Add('.target(coff32)');
      CodeStringList.Add('');
      CodeStringList.Add('.section(".text",'+IntToStr(IMAGE_SCN_CNT_CODE or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_EXECUTE or IMAGE_SCN_ALIGN_4096BYTES)+'){');
      if TextSectionStringList.Count=0 then begin
       CodeStringList.Add('');
       CodeStringList.Add('  nop');
       CodeStringList.Add('');
      end else begin
       CodeStringList.AddStrings(TextSectionStringList);
       CodeStringList.Add('');
      end;
      CodeStringList.Add('}');
      if DataSectionStringList.Count>0 then begin
       CodeStringList.Add('');
       CodeStringList.Add('.section(".data",'+IntToStr(IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE or IMAGE_SCN_ALIGN_4096BYTES)+'){');
       CodeStringList.AddStrings(DataSectionStringList);
       CodeStringList.Add('');
       CodeStringList.Add('}');
      end;
      if BSSSectionStringList.Count>0 then begin
       CodeStringList.Add('');
       CodeStringList.Add('.section(".bss",'+IntToStr(IMAGE_SCN_CNT_UNINITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE or IMAGE_SCN_ALIGN_4096BYTES)+'){');
       CodeStringList.AddStrings(BSSSectionStringList);
       CodeStringList.Add('');
       CodeStringList.Add('}');
      end;
     end;
    finally
     BSSSectionStringList.Free;
    end;
   finally
    DataSectionStringList.Free;
   end;
  finally
   TextSectionStringList.Free;
  end;
  CodeStringList.SaveToStream(AOutputStream);
 finally
  CodeStringList.Free;
 end;
end;

procedure TPACCTarget_x86_32.AssembleCode(const AInputStream,AOutputStream:TStream;const AInputFileName:TPUCUUTF8String='');
var Assembler_:TAssembler;
    SourceLocation:TPACCSourceLocation;
    StringList:TStringList;
    Index:TPACCInt32;
begin
 Assembler_:=TAssembler.Create;
 try
  Assembler_.Target:=ttCOFF32;
  Assembler_.ParseStream(AInputStream);
  if not Assembler_.AreErrors then begin
   Assembler_.Write(AOutputStream);
  end;
  SourceLocation.Source:=TPACCInstance(Instance).Preprocessor.GetInputSourceIndex(iskFILE,AInputFileName);
  SourceLocation.Line:=0;
  SourceLocation.Column:=0;
  if Assembler_.AreWarnings then begin
   StringList:=TStringList.Create;
   try
    StringList.Text:=Assembler_.Warnings;
    for Index:=0 to StringList.Count-1 do begin
     TPACCInstance(Instance).AddWarning(StringList[Index],@SourceLocation);
    end;
   finally
    StringList.Free;
   end;
  end;
  if Assembler_.AreErrors then begin
   StringList:=TStringList.Create;
   try
    StringList.Text:=Assembler_.Errors;
    for Index:=0 to StringList.Count-1 do begin
     TPACCInstance(Instance).AddError(StringList[Index],@SourceLocation,false);
    end;
   finally
    StringList.Free;
   end;
  end;
 finally
  Assembler_.Free;
 end;
end;

procedure TPACCTarget_x86_32.LinkCode(const AInputStreams:TList;const AInputFileNames:TStringList;const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String='');
begin
 inherited LinkCode(AInputStreams,AInputFileNames,AOutputStream,AOutputFileName);
end;

constructor TPACCTarget_x86_32_COFF_PE.Create(const AInstance:TObject);
begin
 inherited Create(AInstance);
 LinkerClass:=TPACCLinker_COFF_PE;
end;

class function TPACCTarget_x86_32_COFF_PE.GetName:TPACCRawByteString;
begin
 result:='x86_32_coff_pe';
end;

function TPACCTarget_x86_32_COFF_PE.GetDefaultOutputExtension:TPACCRawByteString;
begin
 if TPACCInstance(Instance).Options.CreateSharedLibrary then begin
  result:='.dll';
 end else begin
  result:='.exe';
 end;
end;

constructor TPACCTarget_x86_32_ELF_ELF.Create(const AInstance:TObject);
begin
 inherited Create(AInstance);
 LinkerClass:=TPACCLinker_ELF_ELF;
end;

class function TPACCTarget_x86_32_ELF_ELF.GetName:TPACCRawByteString;
begin
 result:='x86_32_elf_elf';
end;

function TPACCTarget_x86_32_ELF_ELF.GetDefaultOutputExtension:TPACCRawByteString;
begin
 if TPACCInstance(Instance).Options.CreateSharedLibrary then begin
  result:='.so';
 end else begin
  result:='';
 end;
end;

initialization
 PACCRegisterTarget(TPACCTarget_x86_32_COFF_PE);
 PACCRegisterTarget(TPACCTarget_x86_32_ELF_ELF);
end.


