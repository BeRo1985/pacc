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
      picroCOPY,
      picroLOAD,
      picroSTORE,
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
      public
       constructor Create(const AInstance:TObject); reintroduce;
       destructor Destroy; override;
      published
       property Instance:TObject read fInstance;
       property Opcode:TPACCIntermediateRepresentationCodeOpcode read fOpcode write fOpcode;
       property JumpToInstruction:TPACCIntermediateRepresentationCodeInstruction read fJumpToInstruction write fJumpToInstruction;
       property Variables:TPACCIntermediateRepresentationCodeInstructionVariableList read fVariables;
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


end.
