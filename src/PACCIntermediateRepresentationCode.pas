unit PACCIntermediateRepresentationCode;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCPointerHashMap,PACCAbstractSyntaxTree;

type PPACCIntermediateRepresentationCodeOpcode=^TPACCIntermediateRepresentationCodeOpcode;
     TPACCIntermediateRepresentationCodeOpcode=
      (
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
       picroINC,
       picroDEC,
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

     PPACCIntermediateRepresentationCodeReferenceType=^TPACCIntermediateRepresentationCodeReferenceType;
     TPACCIntermediateRepresentationCodeReferenceType=
      (
       pircrtTEMPORARY,
       pircrtCONSTANT,
       pircrtSLOT,
       pircrtTYPE,
       pircrtCALL,
       pircrtMEMORY,
       pircrtVARIABLE,
       pircrtLABEL
      );

     TPACCIntermediateRepresentationCodeReference=record
      case Type_:TPACCIntermediateRepresentationCodeReferenceType of
       pircrtTEMPORARY,
       pircrtCONSTANT,
       pircrtSLOT,
       pircrtTYPE,
       pircrtCALL,
       pircrtMEMORY:(
        Value:TPACCUInt32;
       );
       pircrtVARIABLE:(
        Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;
       );
       pircrtLABEL:(
        Label_:TPACCAbstractSyntaxTreeNodeLabel;
       );
     end;

     PPACCIntermediateRepresentationCodeClass=^TPACCIntermediateRepresentationCodeClass;
     TPACCIntermediateRepresentationCodeClass=
      (
       pirccI8,
       pirccI16,
       pirccI32,
       pirccI64,
       pirccF32,
       pirccF64
      );

     PPACCIntermediateRepresentationCodeInstruction=^TPACCIntermediateRepresentationCodeInstruction;
     TPACCIntermediateRepresentationCodeInstruction=record
      Opcode:TPACCIntermediateRepresentationCodeOpcode;
      Class_:TPACCIntermediateRepresentationCodeClass;
      To_:TPACCIntermediateRepresentationCodeReference;
      Arguments:array[0..1] of TPACCIntermediateRepresentationCodeReference;
     end;

     TPACCIntermediateRepresentationCodeInstructions=array of TPACCIntermediateRepresentationCodeInstruction;

     TPACCIntermediateRepresentationCodeBlock=class
      private
       fInstance:TObject;
      public

       Instructions:TPACCIntermediateRepresentationCodeInstructions;
       CountInstructions:TPACCINt32;

       constructor Create(const AInstance:TObject); reintroduce;
       destructor Destroy; override;

      published
       property Instance:TObject read fInstance;
     end;

procedure GenerateIntermediateRepresentationCode(const AInstance:TObject;const ARootAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNode);

implementation

uses PACCInstance;


constructor TPACCIntermediateRepresentationCodeBlock.Create(const AInstance:TObject);
begin
 inherited Create;
 fInstance:=AInstance;
 TPACCInstance(fInstance).AllocatedObjects.Add(self);
 Instructions:=nil;
end;

destructor TPACCIntermediateRepresentationCodeBlock.Destroy;
begin
 Instructions:=nil;
 inherited Destroy;
end;

function GenerateIntermediateRepresentationCodeForFunction(const AInstance:TObject;const AFunctionNode:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration):TPACCIntermediateRepresentationCodeBlock;
 procedure ProcessNode(const ParentBlock:TPACCIntermediateRepresentationCodeBlock;const Node:TPACCAbstractSyntaxTreeNode);
 var Index:TPACCInt32;
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
    end;

    astnkGVAR:begin
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
    end;

    astnkSTATEMENTS:begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeStatements(Node).Children.Count-1 do begin
      ProcessNode(ParentBlock,TPACCAbstractSyntaxTreeNodeStatements(Node).Children[Index]);
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
