unit PACCTarget_x86_32;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCAbstractSyntaxTree,PACCTarget,
     PACCPointerHashMap;

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

uses PACCInstance,PACCPreprocessor,SASMCore,PACCLinker_COFF_PE,PACCLinker_ELF_ELF,PACCIntermediateRepresentationCode;

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
var SectionCounter:TPACCInt32;
    CodeStringList,TextSectionStringList,DataSectionStringList,BSSSectionStringList,ExternalStringList:TStringList;
    CurrentFunction:TPACCAbstractSyntaxTreeNode;
    NodeLabelHashMap:TPACCPointerHashMap;
    NodeLabelCounter:TPACCPtrUInt;
 function GetNodeLabelName(const Node:TPACCAbstractSyntaxTreeNode):TPACCRawByteString;
 var Entity:PPACCPointerHashMapEntity;
     Index:TPACCPtrUInt;
 begin
  Entity:=NodeLabelHashMap.Get(Node,false);
  if assigned(Entity) then begin
   Index:=TPACCPtrUInt(Entity^.Value);
  end else begin
   Index:=NodeLabelCounter;
   inc(NodeLabelCounter);
   NodeLabelHashMap[Node]:=pointer(TPACCPtrUInt(Index));
  end;
  result:='__node_'+TPACCRawByteString(IntToStr(Index));
//result:='__node_0x'+TPACCRawByteString(LowerCase(IntToHex(TPACCPtrUInt(pointer(Node)),SizeOf(TPACCPtrUInt) shl 1)));
  if (Node.Kind in [astnkLVAR,astnkGVAR]) and
     (length(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node).VariableName)>0) then begin
   result:=result+'_'+TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node).VariableName;
  end;
 end;
 procedure EmitExternalDeclarations;
 var Index:TPACCInt32;
     Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;
 begin
  for Index:=0 to TPACCInstance(Instance).IntermediateRepresentationCode.ExternalDeclarations.Count-1 do begin
   Variable:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCInstance(Instance).IntermediateRepresentationCode.ExternalDeclarations[Index]);
   if Variable.Kind=astnkGVAR then begin
    ExternalStringList.Add('.external('+GetNodeLabelName(Variable)+' = "'+Variable.VariableName+'")');
   end else begin
    TPACCInstance(Instance).AddError('Internal error 2017-01-18-04-39-0001',@Variable.SourceLocation,true);
   end;
  end;
 end;
 procedure EmitDeclarations;
 var Index,DataItemIndex:TPACCInt32;
     Declaration:TPACCIntermediateRepresentationCodeDeclaration;
     DataItem:PPACCIntermediateRepresentationCodeDeclarationDataItem;
     Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;
     NeedData:boolean;
 begin
  for Index:=0 to TPACCInstance(Instance).IntermediateRepresentationCode.Declarations.Count-1 do begin
   Declaration:=TPACCInstance(Instance).IntermediateRepresentationCode.Declarations[Index];
   if assigned(Declaration.Variable) then begin
    Variable:=Declaration.Variable;
    NeedData:=false;
    if Variable.Kind=astnkGVAR then begin
     for DataItemIndex:=0 to Declaration.CountDataItems-1 do begin
      DataItem:=@Declaration.DataItems[DataItemIndex];
      if assigned(DataItem^.ValueOffsetBase) or
         ((DataItem^.Kind=pircdikUI8) and (DataItem^.ValueUI8<>0)) or
         ((DataItem^.Kind=pircdikUI16) and (DataItem^.ValueUI16<>0)) or
         ((DataItem^.Kind=pircdikUI32) and (DataItem^.ValueUI32<>0)) or
         ((DataItem^.Kind=pircdikUI64) and (DataItem^.ValueUI64<>0)) then begin
       NeedData:=true;
       break;
      end;
     end;
    end;
    if NeedData then begin
     DataSectionStringList.Add('.align('+IntToStr(Max(1,Variable.Type_^.Alignment))+')');
     DataSectionStringList.Add(GetNodeLabelName(Variable)+':');
     if (Variable.Kind=astnkGVAR) and not ((tfStatic in Variable.Type_^.Flags) or assigned(CurrentFunction)) then begin
      DataSectionStringList.Add('.public('+GetNodeLabelName(Variable)+' = "'+Variable.VariableName+'")');
     end;
     for DataItemIndex:=0 to Declaration.CountDataItems-1 do begin
      DataItem:=@Declaration.DataItems[DataItemIndex];
      if assigned(DataItem^.ValueOffsetBase) then begin
       case DataItem^.Kind of
        pircdikUI8:begin
         if DataItem^.ValueUI8=0 then begin
          if DataItem^.Count=1 then begin
           DataSectionStringList.Add('db offset '+GetNodeLabelName(DataItem^.ValueOffsetBase));
          end else begin
           DataSectionStringList.Add('db '+IntToStr(DataItem^.Count)+' dup(offset '+GetNodeLabelName(DataItem^.ValueOffsetBase)+')');
          end;
         end else begin
          if DataItem^.Count=1 then begin
           DataSectionStringList.Add('db offset '+GetNodeLabelName(DataItem^.ValueOffsetBase)+' + '+IntToStr(DataItem^.ValueUI8));
          end else begin
           DataSectionStringList.Add('db '+IntToStr(DataItem^.Count)+' dup(offset '+GetNodeLabelName(DataItem^.ValueOffsetBase)+' + '+IntToStr(DataItem^.ValueUI8)+')');
          end;
         end;
        end;
        pircdikUI16:begin
         if DataItem^.ValueUI16=0 then begin
          if DataItem^.Count=1 then begin
           DataSectionStringList.Add('dw offset '+GetNodeLabelName(DataItem^.ValueOffsetBase));
          end else begin
           DataSectionStringList.Add('dw '+IntToStr(DataItem^.Count)+' dup(offset '+GetNodeLabelName(DataItem^.ValueOffsetBase)+')');
          end;
         end else begin
          if DataItem^.Count=1 then begin
           DataSectionStringList.Add('dw offset '+GetNodeLabelName(DataItem^.ValueOffsetBase)+' + '+IntToStr(DataItem^.ValueUI16));
          end else begin
           DataSectionStringList.Add('dw '+IntToStr(DataItem^.Count)+' dup(offset '+GetNodeLabelName(DataItem^.ValueOffsetBase)+' + '+IntToStr(DataItem^.ValueUI16)+')');
          end;
         end;
        end;
        pircdikUI32:begin
         if DataItem^.ValueUI32=0 then begin
          if DataItem^.Count=1 then begin
           DataSectionStringList.Add('dd offset '+GetNodeLabelName(DataItem^.ValueOffsetBase));
          end else begin
           DataSectionStringList.Add('dd '+IntToStr(DataItem^.Count)+' dup(offset '+GetNodeLabelName(DataItem^.ValueOffsetBase)+')');
          end;
         end else begin
          if DataItem^.Count=1 then begin
           DataSectionStringList.Add('dd offset '+GetNodeLabelName(DataItem^.ValueOffsetBase)+' + '+IntToStr(DataItem^.ValueUI32));
          end else begin
           DataSectionStringList.Add('dd '+IntToStr(DataItem^.Count)+' dup(offset '+GetNodeLabelName(DataItem^.ValueOffsetBase)+' + '+IntToStr(DataItem^.ValueUI32)+')');
          end;
         end;
        end;
        pircdikUI64:begin
         if DataItem^.ValueUI64=0 then begin
          if DataItem^.Count=1 then begin
           DataSectionStringList.Add('dq offset '+GetNodeLabelName(DataItem^.ValueOffsetBase));
          end else begin
           DataSectionStringList.Add('dq '+IntToStr(DataItem^.Count)+' dup(offset '+GetNodeLabelName(DataItem^.ValueOffsetBase)+')');
          end;
         end else begin
          if DataItem^.Count=1 then begin
           DataSectionStringList.Add('dq offset '+GetNodeLabelName(DataItem^.ValueOffsetBase)+' + '+IntToStr(DataItem^.ValueUI64));
          end else begin
           DataSectionStringList.Add('dq '+IntToStr(DataItem^.Count)+' dup(offset '+GetNodeLabelName(DataItem^.ValueOffsetBase)+' + '+IntToStr(DataItem^.ValueUI64)+')');
          end;
         end;
        end;
        else begin
         TPACCInstance(Instance).AddError('Internal error 2017-02-26-03-36-0000',nil,true);
        end;
       end;
      end else begin
       case DataItem^.Kind of
        pircdikUI8:begin
         if DataItem^.Count=1 then begin
          DataSectionStringList.Add('db '+IntToStr(DataItem^.ValueUI8));
         end else begin
          DataSectionStringList.Add('db '+IntToStr(DataItem^.Count)+' dup('+IntToStr(DataItem^.ValueUI8)+')');
         end;
        end;
        pircdikUI16:begin
         if DataItem^.Count=1 then begin
          DataSectionStringList.Add('dw '+IntToStr(DataItem^.ValueUI16));
         end else begin
          DataSectionStringList.Add('dw '+IntToStr(DataItem^.Count)+' dup('+IntToStr(DataItem^.ValueUI16)+')');
         end;
        end;
        pircdikUI32:begin
         if DataItem^.Count=1 then begin
          DataSectionStringList.Add('dd '+IntToStr(DataItem^.ValueUI32));
         end else begin
          DataSectionStringList.Add('dd '+IntToStr(DataItem^.Count)+' dup('+IntToStr(DataItem^.ValueUI32)+')');
         end;
        end;
        pircdikUI64:begin
         if DataItem^.Count=1 then begin
          DataSectionStringList.Add('dq '+IntToStr(DataItem^.ValueUI64));
         end else begin
          DataSectionStringList.Add('dq '+IntToStr(DataItem^.Count)+' dup('+IntToStr(DataItem^.ValueUI64)+')');
         end;
        end;
        else begin
         TPACCInstance(Instance).AddError('Internal error 2017-02-26-03-36-0000',nil,true);
        end;
       end;
      end;
     end;
     DataSectionStringList.Add('');
    end else begin
     BSSSectionStringList.Add('.align('+IntToStr(Max(1,Variable.Type_^.Alignment))+')');
     BSSSectionStringList.Add(GetNodeLabelName(Variable)+':');
     if (Variable.Kind=astnkGVAR) and not ((tfStatic in Variable.Type_^.Flags) or assigned(CurrentFunction)) then begin
      BSSSectionStringList.Add('.public('+GetNodeLabelName(Variable)+' = "'+Variable.VariableName+'")');
     end;
     BSSSectionStringList.Add('resb '+IntToStr(Declaration.Size));
     BSSSectionStringList.Add('');
    end;
   end else begin
    TPACCInstance(Instance).AddError('Internal error 2017-02-26-03-58-0000',nil,true);
   end;
  end;
 end;
 procedure EmitFunctions;
 var Index,SubIndex,ReturnSize,RemainingRegisterSize,Size:TPACCInt32;
     CodeFunction:TPACCIntermediateRepresentationCodeFunction;
     Parameter:TPACCAbstractSyntaxTreeNode;
 begin
  for Index:=0 to TPACCInstance(Instance).IntermediateRepresentationCode.Functions.Count-1 do begin
   CodeFunction:=TPACCInstance(Instance).IntermediateRepresentationCode.Functions[Index];
   if assigned(CodeFunction) and
      assigned(CodeFunction.FunctionDeclaration) and
      (CodeFunction.FunctionDeclaration is TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration) then begin
    for SubIndex:=0 to CodeFunction.FunctionDeclaration.Parameters.Count-1 do begin
     Parameter:=CodeFunction.FunctionDeclaration.Parameters[SubIndex];
     if assigned(Parameter) then begin
     end;
    end;
    TextSectionStringList.Add('');
    TextSectionStringList.Add('.align(16)');
    if not ((tfStatic in CodeFunction.FunctionDeclaration.Type_^.Flags) or (afInline in CodeFunction.FunctionDeclaration.Type_^.Attribute.Flags)) then begin
     TextSectionStringList.Add('.public('+GetNodeLabelName(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(CodeFunction.FunctionDeclaration.Variable))+' = "'+CodeFunction.FunctionDeclaration.FunctionName+'")');
    end;
    TextSectionStringList.Add(GetNodeLabelName(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(CodeFunction.FunctionDeclaration.Variable))+':');
    TextSectionStringList.Add('push ebp');
    TextSectionStringList.Add('mov ebp, esp');
    // ..
    TextSectionStringList.Add('mov esp, ebp');
    TextSectionStringList.Add('pop ebp');
    if (CodeFunction.FunctionDeclaration.FunctionName='main') and (CodeFunction.FunctionDeclaration.Type_^.ReturnType^.Kind=tkVOID) then begin
     TextSectionStringList.Add('xor eax, eax');
    end;
    case CodeFunction.FunctionDeclaration.Type_^.Attribute.CallingConvention of
     ccSTDCALL:begin
      ReturnSize:=0;
      for SubIndex:=0 to CodeFunction.FunctionDeclaration.Parameters.Count-1 do begin
       Parameter:=CodeFunction.FunctionDeclaration.Parameters[SubIndex];
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
      for SubIndex:=0 to CodeFunction.FunctionDeclaration.Parameters.Count-1 do begin
       Parameter:=CodeFunction.FunctionDeclaration.Parameters[SubIndex];
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
     TextSectionStringList.Add('ret '+IntToStr(ReturnSize));
    end else begin
     TextSectionStringList.Add('ret');
    end;
   end;
  end;
 end;
var Index,LineIndex:TPACCInt32;
begin
 CodeStringList:=TStringList.Create;
 TextSectionStringList:=TStringList.Create;
 DataSectionStringList:=TStringList.Create;
 BSSSectionStringList:=TStringList.Create;
 ExternalStringList:=TStringList.Create;
 NodeLabelHashMap:=TPACCPointerHashMap.Create;
 NodeLabelCounter:=0;
 try
  CurrentFunction:=nil;
  SectionCounter:=1;
  EmitExternalDeclarations;
  EmitDeclarations;
  EmitFunctions;
  if self is TPACCTarget_x86_32_COFF_PE then begin
   CodeStringList.Add('.cpu(all)');
   CodeStringList.Add('');
   CodeStringList.Add('.target(coff32)');
   if ExternalStringList.Count>0 then begin
    CodeStringList.Add('');
    CodeStringList.Add('{');
    CodeStringList.AddStrings(ExternalStringList);
    CodeStringList.Add('}');
   end;
   CodeStringList.Add('');
   CodeStringList.Add('.section(".text", 0x'+TPACCRawByteString(LowerCase(IntToHex(IMAGE_SCN_CNT_CODE or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_EXECUTE or IMAGE_SCN_ALIGN_16BYTES,8)))+'){');
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
    CodeStringList.Add('.section(".data", 0x'+TPACCRawByteString(LowerCase(IntToHex(IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE or IMAGE_SCN_ALIGN_16BYTES,8)))+'){');
    CodeStringList.AddStrings(DataSectionStringList);
    CodeStringList.Add('');
    CodeStringList.Add('}');
   end;
   if BSSSectionStringList.Count>0 then begin
    CodeStringList.Add('');
    CodeStringList.Add('.section(".bss", 0x'+TPACCRawByteString(LowerCase(IntToHex(IMAGE_SCN_CNT_UNINITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE or IMAGE_SCN_ALIGN_16BYTES,8)))+'){');
    CodeStringList.AddStrings(BSSSectionStringList);
    CodeStringList.Add('');
    CodeStringList.Add('}');
   end;
  end;
  CodeStringList.SaveToStream(AOutputStream);
 finally
  NodeLabelHashMap.Free;
  ExternalStringList.Free;
  BSSSectionStringList.Free;
  DataSectionStringList.Free;
  TextSectionStringList.Free;
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

 AlignmentOfPointer:=4;
 AlignmentOf_PTRDIFF_T:=4;
 AlignmentOf_SIZE_T:=4;
 AlignmentOfBool:=1;
 AlignmentOfChar:=1;
 AlignmentOfShort:=2;
 AlignmentOfInt:=4;
 AlignmentOfLong:=4;
 AlignmentOfLongLong:=4;
 AlignmentOfFloat:=4;
 AlignmentOfDouble:=4;
 AlignmentOfLongDouble:=4;
 AlignmentOfEnum:=4;

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


