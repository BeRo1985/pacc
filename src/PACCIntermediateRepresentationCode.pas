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

     TPACCIntermediateRepresentationCodeInstructionVariable=class
      private
       fVariable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;
       fStaticSingleAssignmentIndex:TPACCInt64;
      published
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
      public
       constructor Create(const AInstance:TObject); reintroduce;
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

constructor TPACCIntermediateRepresentationCodeInstruction.Create(const AInstance:TObject);
begin
 inherited Create;
 fInstance:=AInstance;
 TPACCInstance(fInstance).AllocatedObjects.Add(self);
end;

destructor TPACCIntermediateRepresentationCodeInstruction.Destroy;
begin
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
 procedure ProcessNode(const ParentBlock:TPACCIntermediateRepresentationCodeBlock;const Node:TPACCAbstractSyntaxTreeNode);
 var Index:TPACCInt32;
 begin
  if assigned(Node) then begin
   case Node.Kind of
    astnkSTATEMENTS:begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeStatements(Node).Children.Count-1 do begin
      ProcessNode(ParentBlock,TPACCAbstractSyntaxTreeNodeStatements(Node).Children[Index]);
     end;
    end;
   end;

  end;
 end;
begin
 result:=TPACCIntermediateRepresentationCodeBlock.Create(AInstance);
 ProcessNode(result,AFunctionNode.Body);
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
