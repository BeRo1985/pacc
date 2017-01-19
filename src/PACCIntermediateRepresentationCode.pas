unit PACCIntermediateRepresentationCode;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCPointerHashMap,PACCAbstractSyntaxTree;

const CountPredecessors=64;

type PPACCIntermediateRepresentationCodeOpcode=^TPACCIntermediateRepresentationCodeOpcode;
     TPACCIntermediateRepresentationCodeOpcode=
      (
       pircoNONE,

       pircoADD,
       pircoSUB,
       pircoDIV,
       pircoREM,
       pircoUDIV,
       pircoUREM,
       pircoMUL,
       pircoAND,
       pircoOR,
       pircoXOR,
       pircoSAR,
       pircoSHR,
       pircoSHL,
       pircoCI32ule,
       pircoCI32ult,
       pircoCI32sle,
       pircoCI32slt,
       pircoCI32uge,
       pircoCI32ugt,
       pircoCI32sge,
       pircoCI32sgt,
       pircoCI32eq,
       pircoCI32ne,
       pircoCI64ule,
       pircoCI64ult,
       pircoCI64sle,
       pircoCI64slt,
       pircoCI64uge,
       pircoCI64ugt,
       pircoCI64sge,
       pircoCI64sgt,
       pircoCI64eq,
       pircoCI64ne,
       pircoCF32le,
       pircoCF32lt,
       pircoCF32ge,
       pircoCF32gt,
       pircoCF32eq,
       pircoCF32ne,
       pircoCF64le,
       pircoCF64lt,
       pircoCF64ge,
       pircoCF64gt,
       pircoCF64eq,
       pircoCF64ne,

       pircoSTOREI8,
       pircoSTOREI16,
       pircoSTOREI32,
       pircoSTOREI64,
       pircoSTOREF32,
       pircoSTOREF64,

       pircoLOADUI8,
       pircoLOADSI8,
       pircoLOADUI16,
       pircoLOADSI16,
       pircoLOADUI32,
       pircoLOADSI32,
       pircoLOADUI64,
       pircoLOADSI64,
       pircoLOAD,

       pircoZEROEXTENDI8,
       pircoSIGNEXTENDI8,
       pircoZEROEXTENDI16,
       pircoSIGNEXTENDI16,
       pircoZEROEXTENDI32,
       pircoSIGNEXTENDI32,
       pircoZEROEXTENDI64,
       pircoSIGNEXTENDI64,

       pircoCONVF32TOF64,
       pircoCONVF64TOF32,
       pircoCONVF32TOI32,
       pircoCONVF32TOI64,
       pircoCONVF64TOI32,
       pircoCONVF64TOI64,
       pircoCONVI32TOF32,
       pircoCONVI32TOF64,
       pircoCONVI64TOF32,
       pircoCONVI64TOF64,

       pircoALLOC,
       pircoALLOC1,
       pircoALLOC2,
       pircoALLOC4,
       pircoALLOC8,
       pircoALLOC16,

       pircoCOPY,

       pircoPAR,
       pircoPARC,
       pircoARG,
       pircoARGC,
       pircoCALL,

       pircoNOP,
       pircoADDR,
       pircoSWAP,
       pircoSIGN,
       pircoSALLOC,
       pircoXIDIV,
       pircoXDIV,

       picroCOUNT
      );

     PPACCIntermediateRepresentationCodeReferenceType=^TPACCIntermediateRepresentationCodeReferenceType;
     TPACCIntermediateRepresentationCodeReferenceType=
      (
       pircrtNONE,
       pircrtTEMPORARY,
       pircrtCONSTANT,
       pircrtSLOT,
       pircrtTYPE,
       pircrtCALL,
       pircrtMEMORY,
       pircrtVARIABLE,
       pircrtLABEL
      );

     PPACCIntermediateRepresentationCodeReference=^TPACCIntermediateRepresentationCodeReference;
     TPACCIntermediateRepresentationCodeReference=record
      case Type_:TPACCIntermediateRepresentationCodeReferenceType of
       pircrtNONE,
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
       pirccNONE,
       pirccI8,
       pirccI16,
       pirccI32,
       pirccI64,
       pirccF32,
       pirccF64
      );

     PPACCIntermediateRepresentationCodeInstruction=^TPACCIntermediateRepresentationCodeInstruction;
     TPACCIntermediateRepresentationCodeInstruction={$ifdef HAS_ADVANCED_RECORDS}record{$else}object{$endif}
      public
       Opcode:TPACCIntermediateRepresentationCodeOpcode;
       Class_:TPACCIntermediateRepresentationCodeClass;
       To_:TPACCIntermediateRepresentationCodeReference;
       Arguments:array[0..1] of TPACCIntermediateRepresentationCodeReference;
       function Create(const AOpcode:TPACCIntermediateRepresentationCodeOpcode):TPACCIntermediateRepresentationCodeInstruction; overload;
       function Create(const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const AClass_:TPACCIntermediateRepresentationCodeClass):TPACCIntermediateRepresentationCodeInstruction; overload;
       function Create(const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const AClass_:TPACCIntermediateRepresentationCodeClass;const ATo_:TPACCIntermediateRepresentationCodeReference):TPACCIntermediateRepresentationCodeInstruction; overload;
       function Create(const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const AClass_:TPACCIntermediateRepresentationCodeClass;const ATo_,AArgument:TPACCIntermediateRepresentationCodeReference):TPACCIntermediateRepresentationCodeInstruction; overload;
       function Create(const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const AClass_:TPACCIntermediateRepresentationCodeClass;const ATo_,AArgument,AOtherArgument:TPACCIntermediateRepresentationCodeReference):TPACCIntermediateRepresentationCodeInstruction; overload;
     end;

     TPACCIntermediateRepresentationCodeInstructions=array of TPACCIntermediateRepresentationCodeInstruction;

     PPACCIntermediateRepresentationCodeJumpType=^TPACCIntermediateRepresentationCodeJumpType;
     TPACCIntermediateRepresentationCodeJumpType=
      (
       pircjtNONE,
       pircjtRET0,
       pircjtRETI8,
       pircjtRETI16,
       pircjtRETI32,
       pircjtRETI64,
       pircjtRETF32,
       pircjtRETF64,
       pircjtRETC,
       pircjtJMP,
       pircjtJNZ,
       pircjtJZ
      );

     PPACCIntermediateRepresentationCodeJump=^TPACCIntermediateRepresentationCodeJump;
     TPACCIntermediateRepresentationCodeJump=record
      Type_:TPACCIntermediateRepresentationCodeJumpType;
      Argument:TPACCIntermediateRepresentationCodeReference;
     end;

     TPACCIntermediateRepresentationCodeBlock=class;

     PPACCIntermediateRepresentationCodePhi=^TPACCIntermediateRepresentationCodePhi;
     TPACCIntermediateRepresentationCodePhi=record
      To_:TPACCIntermediateRepresentationCodeReference;
      Arguments:array[0..CountPredecessors-1] of TPACCIntermediateRepresentationCodeReference;
      Blocks:array[0..CountPredecessors-1] of TPACCIntermediateRepresentationCodeBlock;
      CountArguments:TPACCInt32;
      Class_:TPACCIntermediateRepresentationCodeClass;
      Link:PPACCIntermediateRepresentationCodePhi;
     end;

     TPACCIntermediateRepresentationCodeBlock=class
      private
       fInstance:TObject;
      public

       Phi:PPACCIntermediateRepresentationCodePhi;

       Instructions:TPACCIntermediateRepresentationCodeInstructions;
       CountInstructions:TPACCINt32;

       Jump:TPACCIntermediateRepresentationCodeJump;

       Link:TPACCIntermediateRepresentationCodeBlock;

       constructor Create(const AInstance:TObject); reintroduce;
       destructor Destroy; override;

       function AddInstruction(const AInstruction:TPACCIntermediateRepresentationCodeInstruction):TPACCInt32;

      published
       property Instance:TObject read fInstance;
     end;

procedure GenerateIntermediateRepresentationCode(const AInstance:TObject;const ARootAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNode);

implementation

uses PACCInstance;

function TPACCIntermediateRepresentationCodeInstruction.Create(const AOpcode:TPACCIntermediateRepresentationCodeOpcode):TPACCIntermediateRepresentationCodeInstruction;
begin
 Opcode:=AOpcode;
 Class_:=pirccNONE;
 To_.Type_:=pircrtNONE;
 Arguments[0].Type_:=pircrtNONE;
 Arguments[1].Type_:=pircrtNONE;
 result:=self;
end;

function TPACCIntermediateRepresentationCodeInstruction.Create(const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const AClass_:TPACCIntermediateRepresentationCodeClass):TPACCIntermediateRepresentationCodeInstruction;
begin
 Opcode:=AOpcode;
 Class_:=AClass_;
 To_.Type_:=pircrtNONE;
 Arguments[0].Type_:=pircrtNONE;
 Arguments[1].Type_:=pircrtNONE;
 result:=self;
end;

function TPACCIntermediateRepresentationCodeInstruction.Create(const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const AClass_:TPACCIntermediateRepresentationCodeClass;const ATo_:TPACCIntermediateRepresentationCodeReference):TPACCIntermediateRepresentationCodeInstruction;
begin
 Opcode:=AOpcode;
 Class_:=AClass_;
 To_:=ATo_;
 Arguments[0].Type_:=pircrtNONE;
 Arguments[1].Type_:=pircrtNONE;
 result:=self;
end;

function TPACCIntermediateRepresentationCodeInstruction.Create(const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const AClass_:TPACCIntermediateRepresentationCodeClass;const ATo_,AArgument:TPACCIntermediateRepresentationCodeReference):TPACCIntermediateRepresentationCodeInstruction;
begin
 Opcode:=AOpcode;
 Class_:=AClass_;
 To_:=ATo_;
 Arguments[0]:=AArgument;
 Arguments[1].Type_:=pircrtNONE;
 result:=self;
end;

function TPACCIntermediateRepresentationCodeInstruction.Create(const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const AClass_:TPACCIntermediateRepresentationCodeClass;const ATo_,AArgument,AOtherArgument:TPACCIntermediateRepresentationCodeReference):TPACCIntermediateRepresentationCodeInstruction;
begin
 Opcode:=AOpcode;
 Class_:=AClass_;
 To_:=ATo_;
 Arguments[0]:=AArgument;
 Arguments[1]:=AOtherArgument;
 result:=self;
end;

constructor TPACCIntermediateRepresentationCodeBlock.Create(const AInstance:TObject);
begin
 inherited Create;

 fInstance:=AInstance;
 TPACCInstance(fInstance).AllocatedObjects.Add(self);

 Instructions:=nil;
 CountInstructions:=0;

 Jump.Type_:=pircjtNONE;

 Link:=nil;
 
 Phi:=nil;

end;

destructor TPACCIntermediateRepresentationCodeBlock.Destroy;
begin

 Instructions:=nil;

 if assigned(Phi) then begin
  Finalize(Phi^);
  FreeMem(Phi);
  Phi:=nil;
 end;

 inherited Destroy;
end;

function TPACCIntermediateRepresentationCodeBlock.AddInstruction(const AInstruction:TPACCIntermediateRepresentationCodeInstruction):TPACCInt32;
begin
 result:=CountInstructions;
 inc(CountInstructions);
 if length(Instructions)<CountInstructions then begin
  SetLength(Instructions,CountInstructions*2);
 end;
 Instructions[result]:=AInstruction;
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
