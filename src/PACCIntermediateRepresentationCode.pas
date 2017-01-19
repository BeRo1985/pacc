unit PACCIntermediateRepresentationCode;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCPointerHashMap,PACCAbstractSyntaxTree;

type PPACCIntermediateRepresentationCodeOpcode=^TPACCIntermediateRepresentationCodeOpcode;
     TPACCIntermediateRepresentationCodeOpcode=(
      pircoNOP,
      picroCALL,
      picroRET,
      picroJMP,
      picroJZ,
      picroJNZ,
      picroJEQ,
      picroJNE,
      picroJLE,
      picroJLT,
      picroJGE,
      picroJGT,
      picroSET,
      picroCONV,
      picroCOPY,
      picroLOAD,
      picroSTORE,
      picroDEREF,
      picroADDR,
      picroADD,
      picroSUB,
      picroDIV,
      picroMOD,
      picroNEG,
      picroBNOT,
      picroBAND,
      picroBOR,
      picroBXOR,
      picroLNOT,
      picroLAND,
      picroLOR,
      picroLXOR,
      picroEQ,
      picroNEQ,
      picroLE,
      picroLT,
      picroGE,
      picroGT,
      picroLAST
     );

     PPACCIntermediateRepresentationCodeInstructionVariable=^TPACCIntermediateRepresentationCodeInstructionVariable;
     TPACCIntermediateRepresentationCodeInstructionVariable=class
      private
       fInstance:TObject;
       fVariable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;
       fStaticSingleAssignmentIndex:TPACCInt64;
      public
       constructor Create(const AInstance:TObject;const AVariable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable); reintroduce;
       destructor Destroy; override;
      published
       property Instance:TObject read fInstance;
       property Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable read fVariable write fVariable;
       property StaticSingleAssignmentIndex:TPACCInt64 read fStaticSingleAssignmentIndex write fStaticSingleAssignmentIndex;
     end;

     TPACCIntermediateRepresentationCodeInstructionVariableList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCIntermediateRepresentationCodeInstructionVariable;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCIntermediateRepresentationCodeInstructionVariable);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCIntermediateRepresentationCodeInstructionVariable read GetItem write SetItem; default;
     end;

     TPACCIntermediateRepresentationCodeInstruction=class
      private
       fInstance:TObject;
       fOpcode:TPACCIntermediateRepresentationCodeOpcode;
       fJumpToInstruction:TPACCIntermediateRepresentationCodeInstruction;
       fVariables:TPACCIntermediateRepresentationCodeInstructionVariableList;
       fLabel:TPACCAbstractSyntaxTreeNodeLabel;
       fValue:TPACCAbstractSyntaxTreeNode;
       fType:PPACCType;
       fSourceLocation:TPACCSourceLocation;
      public
       constructor Create(const AInstance:TObject;const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const ASourceLocation:TPACCSourceLocation); reintroduce;
       destructor Destroy; override;
       property Type_:PPACCType read fType write fType;
      published
       property Instance:TObject read fInstance;
       property Opcode:TPACCIntermediateRepresentationCodeOpcode read fOpcode write fOpcode;
       property JumpToInstruction:TPACCIntermediateRepresentationCodeInstruction read fJumpToInstruction write fJumpToInstruction;
       property Variables:TPACCIntermediateRepresentationCodeInstructionVariableList read fVariables;
       property Label_:TPACCAbstractSyntaxTreeNodeLabel read fLabel write fLabel;
       property Value:TPACCAbstractSyntaxTreeNode read fValue write fValue;
     end;

     TPACCIntermediateRepresentationCodeInstructionList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCIntermediateRepresentationCodeInstruction;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCIntermediateRepresentationCodeInstruction);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCIntermediateRepresentationCodeInstruction read GetItem write SetItem; default;
     end;

     TPACCIntermediateRepresentationCodeBlock=class
      private
       fInstance:TObject;
       fInstructions:TPACCIntermediateRepresentationCodeInstructionList;
      public
       constructor Create(const AInstance:TObject); reintroduce;
       destructor Destroy; override;
      published
       property Instance:TObject read fInstance;
       property Instructions:TPACCIntermediateRepresentationCodeInstructionList read fInstructions write fInstructions;
     end;

procedure GenerateIntermediateRepresentationCode(const AInstance:TObject;const ARootAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNode);

implementation

uses PACCInstance;

constructor TPACCIntermediateRepresentationCodeInstructionVariable.Create(const AInstance:TObject;const AVariable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable);
begin
 inherited Create;
 fInstance:=AInstance;
 TPACCInstance(fInstance).AllocatedObjects.Add(self);
 fVariable:=AVariable;
 fStaticSingleAssignmentIndex:=-1;
end;

destructor TPACCIntermediateRepresentationCodeInstructionVariable.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCIntermediateRepresentationCodeInstructionVariableList.Create;
begin
 inherited Create;
end;

destructor TPACCIntermediateRepresentationCodeInstructionVariableList.Destroy;
begin
 inherited Destroy;
end;

function TPACCIntermediateRepresentationCodeInstructionVariableList.GetItem(const AIndex:TPACCInt):TPACCIntermediateRepresentationCodeInstructionVariable;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCIntermediateRepresentationCodeInstructionVariableList.SetItem(const AIndex:TPACCInt;const AItem:TPACCIntermediateRepresentationCodeInstructionVariable);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

constructor TPACCIntermediateRepresentationCodeInstruction.Create(const AInstance:TObject;const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const ASourceLocation:TPACCSourceLocation);
begin
 inherited Create;
 fInstance:=AInstance;
 TPACCInstance(fInstance).AllocatedObjects.Add(self);
 fOpcode:=AOpcode;
 fJumpToInstruction:=nil;
 fVariables:=TPACCIntermediateRepresentationCodeInstructionVariableList.Create;
 fLabel:=nil;
 fValue:=nil;
 fType:=nil;
end;

destructor TPACCIntermediateRepresentationCodeInstruction.Destroy;
begin
 fVariables.Free;
 inherited Destroy;
end;

constructor TPACCIntermediateRepresentationCodeInstructionList.Create;
begin
 inherited Create;
end;

destructor TPACCIntermediateRepresentationCodeInstructionList.Destroy;
begin
 inherited Destroy;
end;

function TPACCIntermediateRepresentationCodeInstructionList.GetItem(const AIndex:TPACCInt):TPACCIntermediateRepresentationCodeInstruction;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCIntermediateRepresentationCodeInstructionList.SetItem(const AIndex:TPACCInt;const AItem:TPACCIntermediateRepresentationCodeInstruction);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

constructor TPACCIntermediateRepresentationCodeBlock.Create(const AInstance:TObject);
begin
 inherited Create;
 fInstance:=AInstance;
 TPACCInstance(fInstance).AllocatedObjects.Add(self);
 fInstructions:=TPACCIntermediateRepresentationCodeInstructionList.Create;
end;

destructor TPACCIntermediateRepresentationCodeBlock.Destroy;
begin
 fInstructions.Free;
 inherited Destroy;
end;

function GenerateIntermediateRepresentationCodeForFunction(const AInstance:TObject;const AFunctionNode:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration):TPACCIntermediateRepresentationCodeBlock;
 function CreateTemporaryVariableEx(const Type_:PPACCType;ASourceLocation:PPACCSourceLocation):TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;
 begin
  if not assigned(ASourceLocation) then begin
   ASourceLocation:=@TPACCInstance(AInstance).SourceLocation;
  end;
  result:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable.Create(AInstance,astnkLVAR,Type_,ASourceLocation^,'',0);
  AFunctionNode.LocalVariables.Add(result);
 end;
 function CreateTemporaryVariable(const Type_:PPACCType;ASourceLocation:PPACCSourceLocation):TPACCIntermediateRepresentationCodeInstructionVariable;
 var Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;
 begin
  if not assigned(ASourceLocation) then begin
   ASourceLocation:=@TPACCInstance(AInstance).SourceLocation;
  end;
  Variable:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable.Create(AInstance,astnkLVAR,Type_,ASourceLocation^,'',0);
  AFunctionNode.LocalVariables.Add(Variable);
  result:=TPACCIntermediateRepresentationCodeInstructionVariable.Create(AInstance,Variable);
 end;
 function CopyVariable(const Variable:TPACCIntermediateRepresentationCodeInstructionVariable):TPACCIntermediateRepresentationCodeInstructionVariable;
 begin
  result:=TPACCIntermediateRepresentationCodeInstructionVariable.Create(AInstance,Variable.Variable);
 end;
 procedure ProcessNode(const ParentBlock:TPACCIntermediateRepresentationCodeBlock;const Node:TPACCAbstractSyntaxTreeNode;var ResultVariable:TPACCIntermediateRepresentationCodeInstructionVariable;const NeedResult:boolean);
 var Index:TPACCInt32;
     Instruction:TPACCIntermediateRepresentationCodeInstruction;
     TemporaryVariables:array[0..1] of TPACCIntermediateRepresentationCodeInstructionVariable;
     VariableNode:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;
 begin
  if assigned(Node) then begin

   case Node.Kind of

    astnkINTEGER:begin
    end;

    astnkFLOAT:begin
    end;

    astnkSTRING:begin
    end;

    astnkLVAR:begin
     ResultVariable:=TPACCIntermediateRepresentationCodeInstructionVariable.Create(AInstance,TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node));
    end;

    astnkGVAR:begin
     ResultVariable:=TPACCIntermediateRepresentationCodeInstructionVariable.Create(AInstance,TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node));
    end;

    astnkTYPEDEF:begin
    end;

    astnkASSEMBLER:begin
    end;

    astnkASSEMBLER_OPERAND:begin
    end;

    astnkFUNCCALL:begin
    end;

    astnkFUNCPTR_CALL:begin
    end;

    astnkFUNCDESG:begin
    end;

    astnkFUNC:begin
    end;

    astnkEXTERN_DECL:begin
    end;

    astnkDECL:begin
    end;

    astnkINIT:begin
    end;

    astnkCONV:begin
     ResultVariable:=CreateTemporaryVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_,@Node.SourceLocation);
     TemporaryVariables[0]:=nil;
     ProcessNode(ParentBlock,TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryVariables[0],true);
     if not assigned(TemporaryVariables[0]) then begin
      TPACCInstance(AInstance).AddError('Internal error 2017-01-19-14-59-0000',@Node.SourceLocation,true);
     end;
     Instruction:=TPACCIntermediateRepresentationCodeInstruction.Create(AInstance,picroCONV,Node.SourceLocation);
     ParentBlock.Instructions.Add(Instruction);
     Instruction.Type_:=TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_;
     Instruction.Variables.Add(ResultVariable);
     Instruction.Variables.Add(TemporaryVariables[0]);
    end;

    astnkADDR:begin
    end;

    astnkDEREF:begin
    end;

    astnkFOR:begin
    end;

    astnkDO:begin
    end;

    astnkWHILE:begin
    end;

    astnkSWITCH:begin
    end;

    astnkIF:begin
    end;

    astnkTERNARY:begin
    end;

    astnkRETURN:begin
     TemporaryVariables[0]:=nil;
     if assigned(TPACCAbstractSyntaxTreeNodeRETURNStatement(Node).ReturnValue) then begin
      ProcessNode(ParentBlock,TPACCAbstractSyntaxTreeNodeRETURNStatement(Node).ReturnValue,TemporaryVariables[0],true);
      if not assigned(TemporaryVariables[0]) then begin
       TPACCInstance(AInstance).AddError('Internal error 2017-01-19-14-59-0001',@Node.SourceLocation,true);
      end;
     end;
     Instruction:=TPACCIntermediateRepresentationCodeInstruction.Create(AInstance,picroRET,Node.SourceLocation);
     ParentBlock.Instructions.Add(Instruction);
     if assigned(ResultVariable) then begin
      Instruction.Variables.Add(TemporaryVariables[0]);
     end;
    end;

    astnkSTATEMENTS:begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeStatements(Node).Children.Count-1 do begin
      TemporaryVariables[0]:=nil;
      ProcessNode(ParentBlock,TPACCAbstractSyntaxTreeNodeStatements(Node).Children[Index],TemporaryVariables[0],false);
     end;
    end;

    astnkSTRUCT_REF:begin
    end;

    astnkGOTO:begin
    end;

    astnkCOMPUTED_GOTO:begin
    end;

    astnkLABEL:begin
    end;

    astnkHIDDEN_LABEL:begin
    end;

    astnkBREAK:begin
    end;

    astnkCONTINUE:begin
    end;

    astnkOP_COMMA:begin
    end;

    astnkOP_ARROW:begin
    end;

    astnkOP_ASSIGN:begin
    end;

    astnkOP_SIZEOF:begin
    end;

    astnkOP_CAST:begin
    end;

    astnkOP_NOT:begin
    end;

    astnkOP_NEG:begin
    end;

    astnkOP_PRE_INC:begin

     ResultVariable:=TPACCIntermediateRepresentationCodeInstructionVariable.Create(AInstance,TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node));

     TemporaryVariables[0]:=CreateTemporaryVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,@Node.SourceLocation);

     Instruction:=TPACCIntermediateRepresentationCodeInstruction.Create(AInstance,picroSET,Node.SourceLocation);
     ParentBlock.Instructions.Add(Instruction);
     Instruction.Type_:=TPACCInstance(AInstance).TypeINT;
     Instruction.fVariables.Add(TemporaryVariables[0]);
     Instruction.fValue:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(AInstance,astnkINTEGER,Instruction.Type_,Node.SourceLocation,1);

     Instruction:=TPACCIntermediateRepresentationCodeInstruction.Create(AInstance,picroADD,Node.SourceLocation);
     ParentBlock.Instructions.Add(Instruction);
     Instruction.Type_:=Node.Type_;
     Instruction.fVariables.Add(ResultVariable);
     Instruction.fVariables.Add(CopyVariable(ResultVariable));
     Instruction.fVariables.Add(CopyVariable(TemporaryVariables[0]));

    end;

    astnkOP_PRE_DEC:begin
    end;

    astnkOP_POST_INC:begin
    end;

    astnkOP_POST_DEC:begin
    end;

    astnkOP_LABEL_ADDR:begin
    end;

    astnkOP_ADD:begin
     ResultVariable:=CreateTemporaryVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,@Node.SourceLocation);
     TemporaryVariables[0]:=nil;
     TemporaryVariables[1]:=nil;
     ProcessNode(ParentBlock,TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left,TemporaryVariables[0],true);
     ProcessNode(ParentBlock,TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right,TemporaryVariables[1],true);
     if not (assigned(TemporaryVariables[0]) and assigned(TemporaryVariables[1])) then begin
      TPACCInstance(AInstance).AddError('Internal error 2017-01-19-15-13-0000',@Node.SourceLocation,true);
     end;
     Instruction:=TPACCIntermediateRepresentationCodeInstruction.Create(AInstance,picroADD,Node.SourceLocation);
     ParentBlock.Instructions.Add(Instruction);
     Instruction.Type_:=TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_;
     Instruction.Variables.Add(ResultVariable);
     Instruction.Variables.Add(TemporaryVariables[0]);
     Instruction.Variables.Add(TemporaryVariables[1]);
    end;

    astnkOP_SUB:begin
    end;

    astnkOP_MUL:begin
    end;

    astnkOP_DIV:begin
    end;

    astnkOP_MOD:begin
    end;

    astnkOP_AND:begin
    end;

    astnkOP_OR:begin
    end;

    astnkOP_XOR:begin
    end;

    astnkOP_SHL:begin
    end;

    astnkOP_SHR:begin
    end;

    astnkOP_SAR:begin
    end;

    astnkOP_LOG_AND:begin
    end;

    astnkOP_LOG_OR:begin
    end;

    astnkOP_LOG_NOT:begin
    end;

    astnkOP_EQ:begin
    end;

    astnkOP_NE:begin
    end;

    astnkOP_GT:begin
    end;

    astnkOP_LT:begin
    end;

    astnkOP_GE:begin
    end;

    astnkOP_LE:begin
    end;

    astnkOP_A_ADD:begin
    end;

    astnkOP_A_SUB:begin
    end;

    astnkOP_A_MUL:begin
    end;

    astnkOP_A_DIV:begin
    end;

    astnkOP_A_MOD:begin
    end;

    astnkOP_A_AND:begin
    end;

    astnkOP_A_OR:begin
    end;

    astnkOP_A_XOR:begin
    end;

    astnkOP_A_SHR:begin
    end;

    astnkOP_A_SHL:begin
    end;

    astnkOP_A_SAL:begin
    end;

    astnkOP_A_SAR:begin
    end;

   end;

  end;
 end;
var TemporaryVariable:TPACCIntermediateRepresentationCodeInstructionVariable;
begin
 result:=TPACCIntermediateRepresentationCodeBlock.Create(AInstance);
 TemporaryVariable:=nil;
 ProcessNode(result,AFunctionNode.Body,TemporaryVariable,false);
end;

procedure GenerateIntermediateRepresentationCode(const AInstance:TObject;const ARootAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNode);
var Index:TPACCInt32;
    RootAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNodeTranslationUnit;
    Node:TPACCAbstractSyntaxTreeNode;
begin
 if assigned(ARootAbstractSyntaxTreeNode) and
    (TPACCAbstractSyntaxTreeNode(ARootAbstractSyntaxTreeNode).Kind=astnkTRANSLATIONUNIT) and
    (ARootAbstractSyntaxTreeNode is TPACCAbstractSyntaxTreeNodeTranslationUnit) then begin
  RootAbstractSyntaxTreeNode:=TPACCAbstractSyntaxTreeNodeTranslationUnit(ARootAbstractSyntaxTreeNode);
  for Index:=0 to RootAbstractSyntaxTreeNode.Children.Count-1 do begin
   Node:=RootAbstractSyntaxTreeNode.Children[Index];
   if assigned(Node) and (Node.Kind=astnkFUNC) then begin
    GenerateIntermediateRepresentationCodeForFunction(AInstance,TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node));
   end;
  end;
 end else begin
  TPACCInstance(AInstance).AddError('Internal error 2017-01-19-11-48-0000',nil,true);
 end;
end;

end.
