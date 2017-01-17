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
var CodeStringList,TextSectionStringList,DataSectionStringList,BSSSectionStringList:TStringList;
    SectionCounter:TPACCInt32;
    CurrentFunction:TPACCAbstractSyntaxTreeNode;
 function GetVariableLabel(const Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable):TPACCRawByteString;
 begin
  result:='__varlabel_'+IntToStr(TPACCPtrUInt(pointer(Variable)));
 end;
 procedure EmitPrimitiveTypeData(const Type_:PPACCType;const Node:TPACCAbstractSyntaxTreeNode;const Depth:TPACCInt32);
 var f:TPACCFloat;
     d:TPACCDouble;
 begin
  case Type_^.Kind of
   tkFLOAT:begin
    Assert(Node is TPACCAbstractSyntaxTreeNodeFloatValue);
    f:=TPACCAbstractSyntaxTreeNodeFloatValue(Node).Value;
    DataSectionStringList.Add('dd 0x'+IntToHex(TPACCUInt32(pointer(@f)^),8));
   end;
   tkDOUBLE:begin
    Assert(Node is TPACCAbstractSyntaxTreeNodeFloatValue);
    d:=TPACCAbstractSyntaxTreeNodeFloatValue(Node).Value;
    DataSectionStringList.Add('dq 0x'+IntToHex(TPACCUInt64(pointer(@d)^),16));
   end;
   tkBOOL:begin
    DataSectionStringList.Add('db '+IntToStr(TPACCInstance(Instance).EvaluateIntegerExpression(Node,nil)));
   end;
   tkCHAR:begin
    DataSectionStringList.Add('db '+IntToStr(TPACCInstance(Instance).EvaluateIntegerExpression(Node,nil)));
   end;
   tkSHORT:begin
    DataSectionStringList.Add('dw '+IntToStr(TPACCInstance(Instance).EvaluateIntegerExpression(Node,nil)));
   end;
   tkINT:begin
    DataSectionStringList.Add('dd '+IntToStr(TPACCInstance(Instance).EvaluateIntegerExpression(Node,nil)));
   end;
   tkLONG,tkLLONG:begin
    DataSectionStringList.Add('dq '+IntToStr(TPACCInstance(Instance).EvaluateIntegerExpression(Node,nil)));
   end;
   tkPOINTER:begin
    DataSectionStringList.Add('dd '+IntToStr(TPACCInstance(Instance).EvaluateIntegerExpression(Node,nil)));
   end;
  end;
 end;
 procedure ProcessInitializerList(const Nodes:TPACCAbstractSyntaxTreeNodeList;Size,Offset:TPACCInt64;const Depth:TPACCInt32);
 var Index:TPACCInt32;
     Node:TPACCAbstractSyntaxTreeNodeInitializer;
     Value:TPACCAbstractSyntaxTreeNode;
     Delta:TPACCInt64;
 begin
  Index:=0;
  while Index<Nodes.Count do begin

   Node:=TPACCAbstractSyntaxTreeNodeInitializer(Nodes[Index]);

   Value:=Node.InitializionValue;

   Delta:=Node.InitializionOffset-Offset;
   if Delta>0 then begin
    DataSectionStringList.Add('db '+IntToStr(Delta)+' dup(0)');
   end;

   if Node.ToType^.BitSize>0 then begin

   end else begin
    inc(Offset,Node.ToType^.Size);
    dec(Size,Node.ToType^.Size);
   end;
   if Value.Kind=astnkADDR then begin

   end else if (Value.Kind=astnkLVAR) and assigned(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Value).LocalVariableInitialization) then begin
   end else begin
    EmitPrimitiveTypeData(Node.ToType,Node.InitializionValue,Depth);
   end;
   inc(Index);
  end;

  if Size>0 then begin
   DataSectionStringList.Add('db '+IntToStr(Size)+' dup(0)');
  end;
{ if Node.Kind=astnkINIT then begin
  end else begin
   TPACCInstance(Instance).AddError('Internal error 2017-01-17-09-24-0000',@Node.SourceLocation,true);
  end;{}
 end;
 procedure ProcessDeclaration(const Node:TPACCAbstractSyntaxTreeNodeDeclaration);
 var Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;
 begin
  if assigned(Node.DeclarationVariable) then begin
   if assigned(CurrentFunction) then begin
   end else begin
    if Node.DeclarationVariable.Kind=astnkGVAR then begin
     Variable:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node.DeclarationVariable);
     if assigned(Node.DeclarationInitialization) then begin
      BSSSectionStringList.Add('.align('+IntToStr(Max(1,Variable.Type_^.Alignment))+')');
      if not (tfStatic in Variable.Type_^.Flags) then begin
       DataSectionStringList.Add('.public('+GetVariableLabel(Variable)+' = "'+Variable.VariableName+'")');
      end;
      DataSectionStringList.Add(GetVariableLabel(Variable)+':');
      ProcessInitializerList(Node.DeclarationInitialization,0,0,0);
     end else begin
      BSSSectionStringList.Add('.align('+IntToStr(Max(1,Variable.Type_^.Alignment))+')');
      if not (tfStatic in Variable.Type_^.Flags) then begin
       BSSSectionStringList.Add('.public('+GetVariableLabel(Variable)+' = "'+Variable.VariableName+'")');
      end;
      BSSSectionStringList.Add(GetVariableLabel(Variable)+':');
      BSSSectionStringList.Add('resb '+IntToStr(Variable.Type_^.Size));
     end;
    end else begin
     TPACCInstance(Instance).AddError('Internal error 2017-01-17-09-08-0000',@Node.SourceLocation,true);
    end;
   end;
  end else begin
   TPACCInstance(Instance).AddError('Internal error 2017-01-17-09-07-0000',@Node.SourceLocation,true);
  end;
 end;
 procedure ProcessRootNode(const Node:TPACCAbstractSyntaxTreeNode);
 begin
  case Node.Kind of
   astnkDECL:begin
    ProcessDeclaration(TPACCAbstractSyntaxTreeNodeDeclaration(Node));
   end;
   astnkFUNC:begin
   end;
   else begin
    TPACCInstance(Instance).AddError('Internal error 2017-01-17-09-01-0000',@Node.SourceLocation,true);
   end;
  end;
 end;
var Index:TPACCInt32;
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
     for Index:=0 to TPACCAbstractSyntaxTreeNodeStatements(ARoot).Children.Count-1 do begin
      ProcessRootNode(TPACCAbstractSyntaxTreeNodeStatements(ARoot).Children[Index]);
     end;
     if self is TPACCTarget_x86_32_COFF_PE then begin
      CodeStringList.Add('.cpu(all)');
      CodeStringList.Add('.target(coff32)');
      CodeStringList.Add('.section(".text$0",'+IntToStr(IMAGE_SCN_CNT_CODE or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_EXECUTE or IMAGE_SCN_ALIGN_4096BYTES)+'){');
      if TextSectionStringList.Count=0 then begin
       CodeStringList.Add('  nop');
      end else begin
       CodeStringList.AddStrings(TextSectionStringList);
      end;
      CodeStringList.Add('}');
      if DataSectionStringList.Count>0 then begin
       CodeStringList.Add('.section(".data$0",'+IntToStr(IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE or IMAGE_SCN_ALIGN_4096BYTES)+'){');
       CodeStringList.AddStrings(DataSectionStringList);
       CodeStringList.Add('}');
      end;
      if BSSSectionStringList.Count>0 then begin
       CodeStringList.Add('.section(".bss$0",'+IntToStr(IMAGE_SCN_CNT_UNINITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE or IMAGE_SCN_ALIGN_4096BYTES)+'){');
       CodeStringList.AddStrings(BSSSectionStringList);
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


