unit PACCIntermediateRepresentationCode;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCPointerHashMap,PACCAbstractSyntaxTree;

// A intermediate representation instruction set for 32-bit and 64-bit targets (sorry not for 8-bit and 16-bit targets, at least not yet,
// at least that would be a task of somebody else then, to write a corresponding patch for it and submit it, because I myself as
// primary author and creator of PACC only want to focus on the 32-bit and 64-bit targets.)

// A temporary slot will be transformed in the end either to a stack slot or to a CPU register after the last IR processing pass
// before the backend code generation process

// There are two kinds of temporary slots:
//   1. Integer temporary slots (which includes pointer stuff)
//   2. Floating point temporary slots

// A integer temporary slot of type INT is on 32-bit targets 32-bit wide, and  on 64-bit targets 64-bit wide, but where only the lowest
// 32-bits are used

// A integer temporary slot of type LONG are on 32-bit targets virtual-mapped to two 32-bit integer temporaries of type INT, and
// on 64-bit targets 64-bit wide

// A floating point temporary slot of type FLOAT is 32-bit wide (always)

// A floating point temporary slot of type DOUBLE are 64-bit wide (always)

// And there is no support for Intel's 80-bit floating point format, because it do exist primary only on Intel x86 processors and x87
// coprocessors, so therefore they are non-portable and have no support here in the PACC compiler architecture.

type PPACCIntermediateRepresentationCodeOpcode=^TPACCIntermediateRepresentationCodeOpcode;
     TPACCIntermediateRepresentationCodeOpcode=
      (
       pircoNONE,

       pircoNOP,

       pircoASM,

       pircoMOVI, // Move from int to int
       pircoMOVL, // Move from long to long
       pircoMOVF, // Move from float to float
       pircoMODD, // Move frim double to double

       picroCITF, // Convert int to float
       picroCLTF, // Convert long to float
       picroCITD, // Convert int to double
       picroCLTD, // Convert long to double

       picroTFTI, // Truncate float to int
       picroTFTL, // Truncate float to long
       picroTDTI, // Truncate double to int
       picroTDTL, // Truncate double to long

       pircoZECI, // Zero extend from char to int
       pircoZESI, // Zero extend from short to int
       pircoZEIL, // Zero extend from int to long

       pircoSECI, // Sign extend from char to int
       pircoSESI, // Sign extend from short to int
       pircoSEIL, // Sign extend from int to long

       pircoLDUCI, // Load from unsigned char to int
       pircoLDUSI, // Load from unsigned short to int
       pircoLDSCI, // Load from signed char to int
       pircoLDSSI, // Load from signed short to int
       pircoLDII, // Load from int to int
       pircoLDUCL, // Load from unsigned char to long
       pircoLDUSL, // Load from unsigned short to long
       pircoLDUIL, // Load from unsigned int to long
       pircoLDSCL, // Load from signed char to long
       pircoLDSSL, // Load from signed short to long
       pircoLDSIL, // Load from signed int to long
       pircoLDLL, // Load from long to long

       picroSTIC, // Store from int to char
       picroSTIS, // Store from int to short
       picroSTII, // Store from int to int
       picroSTLC, // Store from long to char
       picroSTLS, // Store from long to short
       picroSTLI, // Store from long to int
       picroSTLL, // Store from long to long

       pircoNEGI, // Negation int
       pircoADDI, // Addition int
       pircoSUBI, // Subtraction int
       pircoSMULI, // Singed multiplication int
       pircoSDIVI, // Signed division int
       pircoSMODI, // Signed modulo int
       pircoUMULI, // Unsinged multiplication int
       pircoUDIVI, // Unsigned division int
       pircoUMODI, // Unsigned modulo int
       pircoNOTI, // Bitwise-Not int
       pircoANDI, // Bitwise-And int
       pircoORI, // Bitwise-Or int
       pircoXORI, // Bitwise-Xor int
       pircoSHLI, // Sign-neutral bitshift left int
       pircoSHRI, // Unsigned bitshift right int
       pircoSARI, // Signed bitshift right int

       pircoNEGL, // Negation long
       pircoADDL, // Addition long
       pircoSUBL, // Subtraction long
       pircoSMULL, // Singed multiplication long
       pircoSDIVL, // Signed division long
       pircoSMODL, // Signed modulo long
       pircoUMULL, // Unsinged multiplication long
       pircoUDIVL, // Unsigned division long
       pircoUMODL, // Unsigned modulo long
       pircoNOTL, // Bitwise-Not long
       pircoANDL, // Bitwise-And long
       pircoORL, // Bitwise-Or long
       pircoXORL, // Bitwise-Xor long
       pircoSHLL, // Sign-neutral bitshift left long
       pircoSHRL, // Unsigned bitshift right long int
       pircoSARL, // Signed bitshift right long

       picroNEGF, // Negation float
       pircoADDF, // Addition float
       pircoSUBF, // Subtraction float
       pircoMULF, // Multiplication float
       pircoDIVF, // Division float

       picroNEGD, // Negation double
       pircoADDD, // Addition double
       pircoSUBD, // Subtraction double
       pircoMULD, // Multiplication double
       pircoDIVD, // Division double

       pircoCSLEI, // Signed less than or equal int
       pircoCSLTI, // Signed less than int
       pircoCSGEI, // Signed greater than or equal int
       pircoCSGTI, // Signed greater than int
       pircoCULEI, // Unsigned less than or equal int
       pircoCULTI, // Unsigned less than int
       pircoCUGEI, // Unsigned greater than or equal int
       pircoCUGTI, // Unsigned greater than int
       pircoCEQI, // Equal int
       pircoCNEI, // Not equal int
       pircoCZI, // Zero int
       pircoCNZI, // Not zero int

       pircoCSLEL, // Signed less than or equal long
       pircoCSLTL, // Signed less than long
       pircoCSGEL, // Signed greater than or equal long
       pircoCSGTL, // Signed greater than long
       pircoCULEL, // Unsigned less than or equal long
       pircoCULTL, // Unsigned less than long
       pircoCUGEL, // Unsigned greater than or equal long
       pircoCUGTL, // Unsigned greater than long
       pircoCEQL, // Equal long
       pircoCNEL, // Not equal long
       pircoCZL, // Zero long
       pircoCNZL, // Not zero long

       pircoCLEF, // Less than or equal float
       pircoCLTF, // Less than float
       pircoCGEF, // Greater than or equal float
       pircoCGTF, // Greater than float
       pircoCEQF, // Equal float
       pircoCNEF, // Not equal float
       pircoCOF, // Ordered float
       pircoCNOF, // Not ordered float

       pircoCLED, // Less than or equal double
       pircoCLTD, // Less than double
       pircoCGED, // Greater than or equal double
       pircoCGTD, // Greater than double
       pircoCEQD, // Equal double
       pircoCNED, // Not equal double
       pircoCOD, // Ordered double
       pircoCNOD, // Not ordered double

       pircoALLOCI, // Allocate int(typically 4)
       pircoALLOCL, // Allocate long (typically 8)
       pircoALLOCIS, // Allocate int-SIMD-friendly (typically 16)
       pircoALLOCLS, // Allocate long-SIMD-friendly (typically 32)

       pircoALLOCF, // Allocate float (typically 4)
       pircoALLOCD, // Allocate double (typically 8)
       pircoALLOCFS, // Allocate float-SIMD-friendly (typically 16)
       pircoALLOCDS, // Allocate double-SIMD-friendly (typically 32)

       pircoPARI, // Get function parameter int
       pircoPARL, // Get function parameter long
       pircoPARF, // Get function parameter float
       pircoPARD, // Get function parameter double
       pircoARGI, // Set function call argument int
       pircoARGL, // Set function call argument int
       pircoARGF, // Set function call argument float
       pircoARGD, // Set function call argument double
       pircoCALL,

       picroCOUNT
      );

     PPACCIntermediateRepresentationCodeJumpKind=^TPACCIntermediateRepresentationCodeJumpKind;
     TPACCIntermediateRepresentationCodeJumpKind=
      (
       pircjkNONE,
       pircjkRET,  // Return
       pircjkRETI, // Return int
       pircjkRETL, // Return long
       pircjkRETF, // Return float
       pircjkRETD, // Return doube
       pircjkJMP,  // Jump
       pircjkJF,   // Jump if false
       pircjkJT,   // Jump if true
       pircjkCOUNT
      );

     PPACCIntermediateRepresentationCodeType=^TPACCIntermediateRepresentationCodeType;
     TPACCIntermediateRepresentationCodeType=
      (
       pirccTYPE,
       pirccINT,
       pirccLONG,
       pirccFLOAT,
       pirccDOUBLE
      );

     PPACCIntermediateRepresentationCodeReferenceKind=^TPACCIntermediateRepresentationCodeReferenceKind;
     TPACCIntermediateRepresentationCodeReferenceKind=
      (
       pircrkNONE,
       pircrkCONSTANT,
       pircrkTEMPORARY,
       pircrkVARIABLE,
       pircrkLABEL,
       pircrkREGISTER,
       pircrkSTACKSLOT,
       pircrkCOUNT
      );

     PPACCIntermediateRepresentationCodeReference=^TPACCIntermediateRepresentationCodeReference;
     TPACCIntermediateRepresentationCodeReference=record
      Type_:PPACCType;
      case Kind:TPACCIntermediateRepresentationCodeReferenceKind of
       pircrkNONE:(
       );
       pircrkCONSTANT:(
        case boolean of
         false:(
          ValueInteger:TPACCInt64;
         );
         true:(
          ValueFloat:TPACCDouble;
         );
       );
       pircrkTEMPORARY:(
        TemporaryIndex:TPACCInt32;
       );
       pircrkVARIABLE:(
        Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;
       );
       pircrkLABEL:(
        Label_:TPACCAbstractSyntaxTreeNodeLabel;
       );
       pircrkREGISTER:(
        RegisterIndex:TPACCInt32;
       );
       pircrkSTACKSLOT:(
        StackSlotIndex:TPACCInt32;
       );
     end;

     PPACCIntermediateRepresentationCodeTemporary=^TPACCIntermediateRepresentationCodeTemporary;
     TPACCIntermediateRepresentationCodeTemporary=record
      Type_:PPACCType;
     end;

     TPACCIntermediateRepresentationCodeTemporaries=array of TPACCIntermediateRepresentationCodeTemporary;

     PPACCIntermediateRepresentationCodeInstruction=^TPACCIntermediateRepresentationCodeInstruction;
     TPACCIntermediateRepresentationCodeInstruction={$ifdef HAS_ADVANCED_RECORDS}record{$else}object{$endif}
      public
       Opcode:TPACCIntermediateRepresentationCodeOpcode;
       To_:TPACCIntermediateRepresentationCodeReference;
       Arguments:array[0..1] of TPACCIntermediateRepresentationCodeReference;
     end;

     TPACCIntermediateRepresentationCodeInstructions=array of TPACCIntermediateRepresentationCodeInstruction;

     PPACCIntermediateRepresentationCodeJump=^TPACCIntermediateRepresentationCodeJump;
     TPACCIntermediateRepresentationCodeJump=record
      Kind:TPACCIntermediateRepresentationCodeJumpKind;
      Argument:TPACCIntermediateRepresentationCodeReference;
     end;

     PPACCIntermediateRepresentationCodeBlock=^TPACCIntermediateRepresentationCodeBlock;
     TPACCIntermediateRepresentationCodeBlock=class;

     TPACCIntermediateRepresentationCodeBlocks=array of TPACCIntermediateRepresentationCodeBlock;

     PPACCIntermediateRepresentationCodePhi=^TPACCIntermediateRepresentationCodePhi;
     TPACCIntermediateRepresentationCodePhi=record
      To_:TPACCIntermediateRepresentationCodeReference;
      Arguments:array of TPACCIntermediateRepresentationCodeReference;
      Blocks:array of TPACCIntermediateRepresentationCodeBlock;
      CountArguments:TPACCInt32;
      Type_:PPACCType;
      Link:PPACCIntermediateRepresentationCodePhi;
     end;

     TPACCIntermediateRepresentationCodeBitSet={$ifdef HAVE_ADVANCED_RECORDS}record{$else}object{$endif}
      private
       fBitmap:array of TPACCUInt32;
       fBitmapSize:TPACCInt32;
       function GetBit(const AIndex:TPACCInt32):boolean;
       procedure SetBit(const AIndex:TPACCInt32;const ABit:boolean);
      public
       procedure Clear;
       property BitmapSize:TPACCInt32 read fBitmapSize;
       property Bits[const AIndex:TPACCInt32]:boolean read GetBit write SetBit; default;
     end;

     TPACCIntermediateRepresentationCodeBlock=class
      private
       fInstance:TObject;
      public

       Index:TPACCInt32;

       Label_:TPACCAbstractSyntaxTreeNodeLabel;

       Phi:PPACCIntermediateRepresentationCodePhi;

       Instructions:TPACCIntermediateRepresentationCodeInstructions;
       CountInstructions:TPACCInt32;

       Jump:TPACCIntermediateRepresentationCodeJump;

       Successors:array[0..1] of TPACCIntermediateRepresentationCodeBlock;

       Link:TPACCIntermediateRepresentationCodeBlock;

       ID:TPACCInt32;
       Visit:TPACCInt32;

       InverseDominance:TPACCIntermediateRepresentationCodeBlock;
       Dominance:TPACCIntermediateRepresentationCodeBlock;
       DominanceLink:TPACCIntermediateRepresentationCodeBlock;

       Fronts:TPACCIntermediateRepresentationCodeBlocks;
       CountFronts:TPACCInt32;

       Predecessors:TPACCIntermediateRepresentationCodeBlocks;
       CountPredecessors:TPACCInt32;

       In_:TPACCIntermediateRepresentationCodeBitSet;
       Out_:TPACCIntermediateRepresentationCodeBitSet;
       Gen_:TPACCIntermediateRepresentationCodeBitSet;

       CountLive:array[0..1] of TPACCInt32;
       Loop:TPACCInt32;

       constructor Create(const AInstance:TObject); reintroduce;
       destructor Destroy; override;

       function AddInstruction(const AInstruction:TPACCIntermediateRepresentationCodeInstruction):TPACCInt32;

      published
       property Instance:TObject read fInstance;
     end;

     TPACCIntermediateRepresentationCodeBlockList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCIntermediateRepresentationCodeBlock;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCIntermediateRepresentationCodeBlock);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCIntermediateRepresentationCodeBlock read GetItem write SetItem; default;
     end;

     TPACCIntermediateRepresentationCodeFunction=class
      private

       fInstance:TObject;

      public

       FunctionDeclaration:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration;

       FunctionName:TPACCRawByteString;

       Blocks:TPACCIntermediateRepresentationCodeBlockList;

       BlockLabelHashMap:TPACCPointerHashMap;

       StartBlock:TPACCIntermediateRepresentationCodeBlock;

       Temporaries:TPACCIntermediateRepresentationCodeTemporaries;
       CountTemporaries:TPACCInt32;

       TemporaryReferenceCounter:TPACCUInt32;

       VariableTemporaryReferenceHashMap:TPACCPointerHashMap;

       constructor Create(const AInstance:TObject); reintroduce;
       destructor Destroy; override;

       function RequestTemporary(const Type_:PPACCType):TPACCInt32;
       function CreateTemporaryReference(const Type_:PPACCType):TPACCIntermediateRepresentationCodeReference;

      published
       property Instance:TObject read fInstance;
     end;

procedure GenerateIntermediateRepresentationCode(const AInstance:TObject;const ARootAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNode);

function TypeToClass(const AInstance:TObject;const Type_:PPACCType):TPACCIntermediateRepresentationCodeClass;

implementation

uses PACCInstance;

function TypeToClass(const AInstance:TObject;const Type_:PPACCType):TPACCIntermediateRepresentationCodeClass;
begin
 case Type_^.Kind of
  tkBOOL,
  tkCHAR,
  tkSHORT,
  tkINT,
  tkENUM:begin
   result:=pirccWORD;
  end;
  tkLONG,
  tkLLONG:begin
   result:=pirccLONG;
  end;
  tkFLOAT:begin
   result:=pirccSINGLE;
  end;
  tkDOUBLE,
  tkLDOUBLE:begin
   result:=pirccDOUBLE;
  end;
  tkPOINTER:begin
   if TPACCInstance(AInstance).Target.SizeOfPointer=TPACCInstance(AInstance).Target.SizeOfInt then begin
    result:=pirccWORD;
   end else if TPACCInstance(AInstance).Target.SizeOfPointer=TPACCInstance(AInstance).Target.SizeOfLong then begin
    result:=pirccLONG;
   end else begin
    result:=pirccNONE;
    TPACCInstance(AInstance).AddError('Internal error 2017-01-22-10-33-0000',nil,true);
   end;
  end;
  else begin
   result:=pirccNONE;
   TPACCInstance(AInstance).AddError('Internal error 2017-01-22-11-29-0000',nil,true);
  end;
 end;
end;

function TPACCIntermediateRepresentationCodeBitSet.GetBit(const AIndex:TPACCInt32):boolean;
begin
 result:=((AIndex>=0) and (AIndex<(fBitmapSize shl 3))) and
         ((fBitmap[AIndex shr 3] and (TPACCUInt32(1) shl (AIndex and 31)))<>0);
end;

procedure TPACCIntermediateRepresentationCodeBitSet.SetBit(const AIndex:TPACCInt32;const ABit:boolean);
var OldSize,Index:TPACCInt32;
begin
 if AIndex>=0 then begin
  if (fBitmapSize shl 3)<=AIndex then begin
   fBitmapSize:=(AIndex+31) shr 3;
   OldSize:=length(fBitmap);
   if OldSize<fBitmapSize then begin
    SetLength(fBitmap,fBitmapSize*2);
    FillChar(fBitmap[OldSize],(length(fBitmap)-OldSize)*SizeOf(TPACCUInt32),#0);
   end;
  end;
  if ABit then begin
   fBitmap[AIndex shr 3]:=fBitmap[AIndex shr 3] or (TPACCUInt32(1) shl (AIndex and 31));
  end else begin
   fBitmap[AIndex shr 3]:=fBitmap[AIndex shr 3] and not (TPACCUInt32(1) shl (AIndex and 31));
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeBitSet.Clear;
begin
 fBitmap:=nil;
 fBitmapSize:=0;
end;

constructor TPACCIntermediateRepresentationCodeBlock.Create(const AInstance:TObject);
begin
 inherited Create;

 fInstance:=AInstance;
 TPACCInstance(fInstance).AllocatedObjects.Add(self);

 Index:=0;

 Label_:=nil;

 Phi:=nil;

 Instructions:=nil;
 CountInstructions:=0;

 Jump.Kind:=pircjkNONE;

 Successors[0]:=nil;
 Successors[1]:=nil;

 Link:=nil;

 ID:=0;
 Visit:=0;

 InverseDominance:=nil;
 Dominance:=nil;
 DominanceLink:=nil;

 Fronts:=nil;
 CountFronts:=0;

 Predecessors:=nil;
 CountPredecessors:=0;

 In_.Clear;
 Out_.Clear;
 Gen_.Clear;

 CountLive[0]:=0;
 CountLive[1]:=0;
 Loop:=0;

end;

destructor TPACCIntermediateRepresentationCodeBlock.Destroy;
begin

 if assigned(Phi) then begin
  Finalize(Phi^);
  FreeMem(Phi);
  Phi:=nil;
 end;

 Instructions:=nil;

 Fronts:=nil;
 CountFronts:=0;

 Predecessors:=nil;
 CountPredecessors:=0;

 In_.Clear;
 Out_.Clear;
 Gen_.Clear;

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

constructor TPACCIntermediateRepresentationCodeBlockList.Create;
begin
 inherited Create;
end;

destructor TPACCIntermediateRepresentationCodeBlockList.Destroy;
begin
 inherited Destroy;
end;

function TPACCIntermediateRepresentationCodeBlockList.GetItem(const AIndex:TPACCInt):TPACCIntermediateRepresentationCodeBlock;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCIntermediateRepresentationCodeBlockList.SetItem(const AIndex:TPACCInt;const AItem:TPACCIntermediateRepresentationCodeBlock);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

constructor TPACCIntermediateRepresentationCodeFunction.Create(const AInstance:TObject);
begin
 inherited Create;

 fInstance:=AInstance;
 TPACCInstance(fInstance).AllocatedObjects.Add(self);

 Blocks:=TPACCIntermediateRepresentationCodeBlockList.Create;

 BlockLabelHashMap:=TPACCPointerHashMap.Create;

 StartBlock:=nil;

 Temporaries:=nil;
 CountTemporaries:=0;

 TemporaryReferenceCounter:=0;

 VariableTemporaryReferenceHashMap:=TPACCPointerHashMap.Create;

end;

destructor TPACCIntermediateRepresentationCodeFunction.Destroy;
begin
 Blocks.Free;
 BlockLabelHashMap.Free;
 VariableTemporaryReferenceHashMap.Free;
 Temporaries:=nil;
 inherited Destroy;
end;

function TPACCIntermediateRepresentationCodeFunction.RequestTemporary(const Type_:PPACCType):TPACCInt32;
var Temporary:PPACCIntermediateRepresentationCodeTemporary;
begin
 result:=CountTemporaries;
 inc(CountTemporaries);
 if length(Temporaries)<CountTemporaries then begin
  SetLength(Temporaries,CountTemporaries*2);
 end;
 Temporary:=@Temporaries[result];
 Temporary^.Type_:=Type_;
end;

function TPACCIntermediateRepresentationCodeFunction.CreateTemporaryReference(const Type_:PPACCType):TPACCIntermediateRepresentationCodeReference;
begin
 result.Kind:=pircrkTEMPORARY;
 result.Type_:=TPACCInstance(fInstance).GetPromotionType(Type_);
 result.TemporaryIndex:=RequestTemporary(result.Type_);
end;

function GenerateIntermediateRepresentationCodeForFunction(const AInstance:TObject;const AFunctionNode:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration):TPACCIntermediateRepresentationCodeFunction;
type PValueKind=^TValueKind;
     TValueKind=
      (
       vkNONE,
       vkLVALUE,
       vkRVALUE
      );
var CurrentBlock:TPACCIntermediateRepresentationCodeBlock;
    BlockLink,PhiLink:PPACCIntermediateRepresentationCodeBlock;
    CodeInstance:TPACCIntermediateRepresentationCodeFunction;
    NeedNewBlock:boolean;
 function GetVariableReference(Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable):TPACCIntermediateRepresentationCodeReference;
 begin
  result.Kind:=pircrkVARIABLE;
  result.Type_:=Variable.Type_;
  result.Variable:=Variable;
 end;
 function CreateTemporaryVariableReference(const Type_:PPACCType):TPACCIntermediateRepresentationCodeReference;
 var Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;
 begin
  Variable:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable.Create(AInstance,astnkLVAR,Type_,TPACCInstance(AInstance).SourceLocation,'',0);
  Variable.Index:=AFunctionNode.LocalVariables.Add(Variable);
  result.Kind:=pircrkVARIABLE;
  result.Type_:=Variable.Type_;
  result.Variable:=Variable;
 end;
 function NewHiddenLabel:TPACCAbstractSyntaxTreeNodeLabel;
 begin
  result:=TPACCAbstractSyntaxTreeNodeLabel.Create(AInstance,astnkHIDDEN_LABEL,nil,TPACCInstance(AInstance).SourceLocation,'');
 end;
 procedure CloseBlock;
 begin
  BlockLink:=@CurrentBlock.Link;
  NeedNewBlock:=true;
 end;
 function FindBlock(const Label_:TPACCAbstractSyntaxTreeNodeLabel):TPACCIntermediateRepresentationCodeBlock;
 begin
  result:=CodeInstance.BlockLabelHashMap[Label_];
  if not assigned(result) then begin
   result:=TPACCIntermediateRepresentationCodeBlock.Create(AInstance);
   result.Index:=CodeInstance.Blocks.Add(result);
   result.Label_:=Label_;
   CodeInstance.BlockLabelHashMap[Label_]:=result;
  end;
 end;
 procedure EmitLabel(const Label_:TPACCAbstractSyntaxTreeNodeLabel);
 var Block:TPACCIntermediateRepresentationCodeBlock;
 begin
  Block:=FindBlock(Label_);
  if assigned(CurrentBlock) and (CurrentBlock.Jump.Kind=pircjkNONE) then begin
   CloseBlock;
   CurrentBlock.Jump.Kind:=pircjkJMP;
   CurrentBlock.Successors[0]:=Block;
  end;
  if Block.Jump.Kind<>pircjkNONE then begin
   TPACCInstance(AInstance).AddError('Internal error 2017-01-20-14-42-0000',nil,true);
  end;
  BlockLink^:=Block;
  CurrentBlock:=Block;
  PhiLink:=@CurrentBlock.Phi;
  NeedNewBlock:=false;
 end;
 procedure EmitJump(const Label_:TPACCAbstractSyntaxTreeNodeLabel);
 var Block:TPACCIntermediateRepresentationCodeBlock;
 begin
  Block:=FindBlock(Label_);
  CurrentBlock.Jump.Kind:=pircjkJMP;
  CurrentBlock.Successors[0]:=Block;
  CloseBlock;
 end;
 procedure CreateNewBlockIfNeeded;
 begin
  if NeedNewBlock then begin
   EmitLabel(NewHiddenLabel);
  end;
 end;
 procedure EmitInstruction(const AInstruction:TPACCIntermediateRepresentationCodeInstruction); overload;
 begin
  CreateNewBlockIfNeeded;
  CurrentBlock.AddInstruction(AInstruction);
 end;
 procedure EmitInstruction0(const AOpcode:TPACCIntermediateRepresentationCodeOpcode); overload;
 var Instruction:TPACCIntermediateRepresentationCodeInstruction;
 begin
  Instruction.Opcode:=AOpcode;
  Instruction.To_.Kind:=pircrkNONE;
  Instruction.Arguments[0].Kind:=pircrkNONE;
  Instruction.Arguments[1].Kind:=pircrkNONE;
  EmitInstruction(Instruction);
 end;
 procedure EmitInstruction1(const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const ATo_:TPACCIntermediateRepresentationCodeReference); overload;
 var Instruction:TPACCIntermediateRepresentationCodeInstruction;
 begin
  Instruction.Opcode:=AOpcode;
  Instruction.To_:=ATo_;
  Instruction.Arguments[0].Kind:=pircrkNONE;
  Instruction.Arguments[1].Kind:=pircrkNONE;
  EmitInstruction(Instruction);
 end;
 procedure EmitInstruction(const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const ATo_,AArgument:TPACCIntermediateRepresentationCodeReference); overload;
 var Instruction:TPACCIntermediateRepresentationCodeInstruction;
 begin
  Instruction.Opcode:=AOpcode;
  Instruction.To_:=ATo_;
  Instruction.Arguments[0]:=AArgument;
  Instruction.Arguments[1].Kind:=pircrkNONE;
  EmitInstruction(Instruction);
 end;
 procedure EmitInstruction(const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const ATo_,AArgument,AOtherArgument:TPACCIntermediateRepresentationCodeReference); overload;
 var Instruction:TPACCIntermediateRepresentationCodeInstruction;
 begin
  Instruction.Opcode:=AOpcode;
  Instruction.To_:=ATo_;
  Instruction.Arguments[0]:=AArgument;
  Instruction.Arguments[1]:=AOtherArgument;
  EmitInstruction(Instruction);
 end;
 procedure ProcessNode(const Node:TPACCAbstractSyntaxTreeNode;const OutputResultReference:PPACCIntermediateRepresentationCodeReference;const ValueKind:TValueKind);
 var Index:TPACCInt32;
     Opcode:TPACCIntermediateRepresentationCodeOpcode;
     ReferenceA,ReferenceB,ReferenceC:PPACCIntermediateRepresentationCodeReference;
  function AllocateReference(var Reference:PPACCIntermediateRepresentationCodeReference):PPACCIntermediateRepresentationCodeReference;
  begin
   if not assigned(Reference) then begin
    GetMem(Reference,SizeOf(TPACCIntermediateRepresentationCodeReference));
    FillChar(Reference^,SizeOf(TPACCIntermediateRepresentationCodeReference),#0);
   end;
   result:=Reference;
   result^.Kind:=pircrkNONE;
  end;
  procedure FreeReference(var Reference:PPACCIntermediateRepresentationCodeReference);
  begin
   if assigned(Reference) then begin
    Finalize(Reference^);
    FreeMem(Reference,SizeOf(TPACCIntermediateRepresentationCodeReference));
    Reference:=nil;
   end;
  end;
  procedure CheckResultReference(const Type_:PPACCType);
  begin
   if not (assigned(OutputResultReference) and
           (OutputResultReference^.Kind<>pircrkNONE) and
           TPACCInstance(AInstance).AreTypesEqual(OutputResultReference^.Type_,Type_)) then begin
    TPACCInstance(AInstance).AddError('Internal error 2017-01-21-14-40-0000',nil,true);
   end;
  end;
  procedure ProcessLValueNode(const Node:TPACCAbstractSyntaxTreeNode;const OutputResultReference:PPACCIntermediateRepresentationCodeReference);
  begin
   case Node.KInd of
    astnkLVAR,
    astnkGVAR:begin
     if assigned(OutputResultReference) and (OutputResultReference^.Kind=pircrkNONE) then begin
      OutputResultReference^:=GetVariableReference(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node));
     end else begin
      TPACCInstance(AInstance).AddError('Internal error 2017-01-21-14-04-0000',nil,true);
     end;
    end;
    astnkDEREF:begin
    end;
    astnkSTRUCT_REF:begin
    end;
    else begin
     TPACCInstance(AInstance).AddError('Internal error 2017-01-21-18-39-0000',nil,true);
    end;
   end;
  end;
 begin
  if assigned(Node) then begin

   ReferenceA:=nil;
   ReferenceB:=nil;
   ReferenceC:=nil;

// writeln(TypInfo.GetEnumName(TypeInfo(TPACCAbstractSyntaxTreeNodeKind),TPACCInt32(Node.Kind)));

   case Node.Kind of

    astnkINTEGER:begin
{    if assigned(OutputResultReference) and (ValueKind=vkRVALUE) then begin
      AllocateReference(ReferenceA);
      try
       OutputResultReference^:=CodeInstance.CreateTemporaryReference(Node.Type_);
       ReferenceA^.Kind:=pircrkCONSTANT;
       ReferenceA^.Type_:=Node.Type_;
       ReferenceA^.ValueInteger:=TPACCAbstractSyntaxTreeNodeIntegerValue(Node).Value;
       EmitInstruction(pircoCOPY,OutputResultReference^,ReferenceA^);
      finally
       FreeReference(ReferenceA);
      end;
     end else begin
      TPACCInstance(AInstance).AddError('Internal error 2017-01-21-14-50-0000',nil,true);
     end;}
    end;

    astnkFLOAT:begin
{    if assigned(OutputResultReference) and (ValueKind=vkRVALUE) then begin
      AllocateReference(ReferenceA);
      try
       OutputResultReference^:=CodeInstance.CreateTemporaryReference(Node.Type_);
       ReferenceA^.Kind:=pircrkCONSTANT;
       ReferenceA^.Type_:=Node.Type_;
       ReferenceA^.ValueFloat:=TPACCAbstractSyntaxTreeNodeFloatValue(Node).Value;
       EmitInstruction(pircoCOPY,OutputResultReference^,ReferenceA^);
      finally
       FreeReference(ReferenceA);
      end;
     end else begin
      TPACCInstance(AInstance).AddError('Internal error 2017-01-21-14-55-0000',nil,true);
     end;}
    end;

    astnkSTRING:begin
     TPACCInstance(AInstance).AddError('Internal error 2017-01-21-14-56-0000',nil,true);
    end;

    astnkLVAR,
    astnkGVAR:begin
{    if assigned(OutputResultReference) and (OutputResultReference^.Kind=pircrkNONE) then begin
      case ValueKind of
       vkLVALUE:begin
        OutputResultReference^:=GetVariableReference(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node));
       end;
       vkRVALUE:begin
        AllocateReference(ReferenceA);
        try
         OutputResultReference^:=CodeInstance.CreateTemporaryReference(Node.Type_);
         ReferenceA^:=GetVariableReference(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node));
         EmitInstruction(pircoLOAD,OutputResultReference^,ReferenceA^);
        finally
         FreeReference(ReferenceA);
        end;
       end;
       else begin
        TPACCInstance(AInstance).AddError('Internal error 2017-01-22-09-38-0000',nil,true);
       end;
      end;
     end else begin
      TPACCInstance(AInstance).AddError('Internal error 2017-01-21-14-04-0000',nil,true);
     end;}
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
{    if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) and
        assigned(OutputResultReference) then begin
      AllocateReference(ReferenceA);
      try
       ReferenceA^.Kind:=pircrkNONE;
       ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,ReferenceA,ValueKind);
       if ReferenceA^.Kind=pircrkNONE then begin
        TPACCInstance(AInstance).AddError('Internal error 2017-01-21-13-58-0000',nil,true);
       end else begin
        case Node.Type_^.Kind of
         tkBOOL,
         tkCHAR,
         tkSHORT,
         tkINT:begin
          case ReferenceA^.Type_^.Kind of
           tkBOOL,
           tkCHAR:begin
            OutputResultReference^:=CodeInstance.CreateTemporaryReference(Node.Type_);
            if (tfUnsigned in Node.Type_^.Flags) or (tfUnsigned in ReferenceA^.Type_^.Flags) then begin
             Opcode:=pircoZEROEXTEND8TO32;
            end else begin
             Opcode:=pircoSIGNEXTEND8TO32;
            end;
            EmitInstruction(Opcode,OutputResultReference^,ReferenceA^);
           end;
           tkSHORT:begin
            OutputResultReference^:=CodeInstance.CreateTemporaryReference(Node.Type_);
            if not ((tfUnsigned in Node.Type_^.Flags) or (tfUnsigned in ReferenceA^.Type_^.Flags)) then begin
             Opcode:=pircoZEROEXTEND16TO32;
            end else begin
             Opcode:=pircoSIGNEXTEND16TO32;
            end;
            EmitInstruction(Opcode,OutputResultReference^,ReferenceA^);
           end;
           tkINT:begin
            OutputResultReference^:=CodeInstance.CreateTemporaryReference(Node.Type_);
            EmitInstruction(pircoCOPY,OutputResultReference^,ReferenceA^);
           end;
          end;
         end;
        end;
       end;
      finally
       FreeReference(ReferenceA);
      end;
     end else begin
      TPACCInstance(AInstance).AddError('Internal error 2017-01-21-12-45-0000',nil,true);
     end;}
    end;

    astnkADDR:begin
     {if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) and
        assigned(OutputResultReference) then begin
      AllocateReference(ReferenceA);
      try
       ReferenceA^.Kind:=pircrkNONE;
       ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,ReferenceA,vkLVALUE);
       if ReferenceA^.Kind=pircrkNONE then begin
        TPACCInstance(AInstance).AddError('Internal error 2017-01-22-09-33-0000',nil,true);
       end else begin
        Opcode:=pircoADDR;
        PrepareResultReference(Node.Type_);
        EmitInstruction(Opcode,OutputResultReference^,ReferenceA^);
        CheckResultReference(Node.Type_);
       end;
      finally
       FreeReference(ReferenceA);
      end;
     end else begin
      TPACCInstance(AInstance).AddError('Internal error 2017-01-22-09-33-0002',nil,true);
     end;}
    end;

    astnkDEREF:begin
{    if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) and
        assigned(OutputResultReference) then begin
      AllocateReference(ReferenceA);
      try
       ReferenceA^.Kind:=pircrkNONE;
       ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,ReferenceA,vkLVALUE);
       if ReferenceA^.Kind=pircrkNONE then begin
        TPACCInstance(AInstance).AddError('Internal error 2017-01-21-15-11-0000',nil,true);
       end else begin
        if Node.Type_.Kind=tkPOINTER then begin
         PrepareResultDereference(Node.Type_^.ChildType);
         EmitInstruction(pircoDEREF,OutputResultReference^,ReferenceA^);
         CheckResultReference(Node.Type_^.ChildType);
        end else begin
         TPACCInstance(AInstance).AddError('Internal error 2017-01-21-18-06-0000',nil,true);
        end;
       end;
      finally
       FreeReference(ReferenceA);
      end;
     end else begin
      TPACCInstance(AInstance).AddError('Internal error 2017-01-21-15-11-0002',nil,true);
     end;}
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
     AllocateReference(ReferenceA);
     try
      ReferenceA^.Kind:=pircrkNONE;
      if assigned(TPACCAbstractSyntaxTreeNodeRETURNStatement(Node).ReturnValue) then begin
       ProcessNode(TPACCAbstractSyntaxTreeNodeRETURNStatement(Node).ReturnValue,ReferenceA,vkRVALUE);
      end;
      if CurrentBlock.Jump.Kind=pircjkNONE then begin
       CurrentBlock.Jump.Kind:=pircjkRET;
       CurrentBlock.Jump.Argument:=ReferenceA^;
       CloseBlock;
      end else begin
       TPACCInstance(AInstance).AddError('Internal error 2017-01-21-13-54-0000',nil,true);
      end;
     finally
      FreeReference(ReferenceA);
     end;
    end;

    astnkSTATEMENTS:begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeStatements(Node).Children.Count-1 do begin
      ProcessNode(TPACCAbstractSyntaxTreeNodeStatements(Node).Children[Index],nil,vkNONE);
     end;
    end;

    astnkSTRUCT_REF:begin
    end;

    astnkGOTO:begin
     EmitJump(TPACCAbstractSyntaxTreeNodeLabel(TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).Label_));
    end;

    astnkCOMPUTED_GOTO:begin
    end;

    astnkLABEL:begin
     EmitLabel(TPACCAbstractSyntaxTreeNodeLabel(Node));
    end;

    astnkHIDDEN_LABEL:begin
     EmitLabel(TPACCAbstractSyntaxTreeNodeLabel(Node));
    end;

    astnkBREAK:begin
     EmitJump(TPACCAbstractSyntaxTreeNodeLabel(TPACCAbstractSyntaxTreeNodeBREAKOrCONTINUEStatement(Node).Label_));
    end;

    astnkCONTINUE:begin
     EmitJump(TPACCAbstractSyntaxTreeNodeLabel(TPACCAbstractSyntaxTreeNodeBREAKOrCONTINUEStatement(Node).Label_));
    end;

    astnkOP_COMMA:begin
     if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
        assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
      AllocateReference(ReferenceA);
      try
       ReferenceA^.Kind:=pircrkNONE;
       ProcessNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left,ReferenceA,ValueKind);
       ProcessNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right,OutputResultReference,ValueKind);
      finally
       FreeReference(ReferenceA);
      end;
     end else begin
      TPACCInstance(AInstance).AddError('Internal error 2017-01-21-15-08-0000',nil,true);
     end;
    end;

    astnkOP_ARROW:begin
    end;

    astnkOP_ASSIGN:begin
    end;

    astnkOP_CAST:begin
    end;

    astnkOP_NOT:begin
    end;

    astnkOP_NEG:begin
    end;

    astnkOP_PRE_INC,
    astnkOP_PRE_DEC,
    astnkOP_POST_INC,
    astnkOP_POST_DEC:begin
{    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
        assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) and
        assigned(OutputResultReference) then begin
      AllocateReference(ReferenceA);
      AllocateReference(ReferenceB);
      try
       ReferenceA^.Kind:=pircrkNONE;
       ReferenceB^.Kind:=pircrkCONSTANT;
       ReferenceB^.ValueInteger:=1;
       ProcessNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left,ReferenceA,vkLVALUE);
       if ReferenceA^.Kind=pircrkNONE then begin
        TPACCInstance(AInstance).AddError('Internal error 2017-01-21-15-01-0000',nil,true);
       end else begin
        case Node.Kind of
         astnkOP_PRE_INC,astnkOP_POST_INC:begin
          Opcode:=pircoADD;
         end;
         astnkOP_PRE_DEC,astnkOP_POST_DEC:begin
          Opcode:=pircoSUB;
         end;
         else begin
          Opcode:=pircoNONE;
          TPACCInstance(AInstance).AddError('Internal error 2017-01-21-15-01-0001',nil,true);
         end;
        end;
        if Node.Kind in [astnkOP_PRE_INC,astnkOP_PRE_DEC] then begin
         EmitInstruction(Opcode,ReferenceA^,ReferenceA^,ReferenceB^);
         PrepareResultReference(Node.Type_);
         EmitInstruction(pircoCOPY,OutputResultReference^,ReferenceA^);
         CheckResultReference(Node.Type_);
        end else begin
         PrepareResultReference(Node.Type_);
         EmitInstruction(pircoCOPY,OutputResultReference^,ReferenceA^);
         CheckResultReference(Node.Type_);
         EmitInstruction(Opcode,ReferenceA^,ReferenceA^,ReferenceB^);
        end;
       end;
      finally
       FreeReference(ReferenceA);
       FreeReference(ReferenceB);
      end;
     end else begin
      TPACCInstance(AInstance).AddError('Internal error 2017-01-21-15-01-0002',nil,true);
     end;}
    end;

    astnkOP_LABEL_ADDR:begin
    end;

    astnkOP_ADD,
    astnkOP_SUB,
    astnkOP_MUL,
    astnkOP_DIV,
    astnkOP_MOD,
    astnkOP_AND,
    astnkOP_OR,
    astnkOP_XOR,
    astnkOP_SHL,
    astnkOP_SHR,
    astnkOP_SAR:begin
{    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
        assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) and
        assigned(OutputResultReference) then begin
      AllocateReference(ReferenceA);
      AllocateReference(ReferenceB);
      try
       ReferenceA^.Kind:=pircrkNONE;
       ReferenceB^.Kind:=pircrkNONE;
       ProcessNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left,ReferenceA,vkRVALUE);
       ProcessNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right,ReferenceB,vkRVALUE);
       if (ReferenceA^.Kind=pircrkNONE) or
          (ReferenceB^.Kind=pircrkNONE) then begin
        TPACCInstance(AInstance).AddError('Internal error 2017-01-21-14-01-0000',nil,true);
       end else begin
        case Node.Kind of
         astnkOP_ADD:begin
          Opcode:=pircoADD;
         end;
         astnkOP_SUB:begin
          Opcode:=pircoSUB;
         end;
         astnkOP_MUL:begin
          Opcode:=pircoMUL;
         end;
         astnkOP_DIV:begin
          Opcode:=pircoDIV;
         end;
         astnkOP_MOD:begin
          Opcode:=pircoMOD;
         end;
         astnkOP_AND:begin
          Opcode:=pircoAND;
         end;
         astnkOP_OR:begin
          Opcode:=pircoOR;
         end;
         astnkOP_XOR:begin
          Opcode:=pircoXOR;
         end;
         astnkOP_SHL:begin
          Opcode:=pircoSHL;
         end;
         astnkOP_SHR:begin
          Opcode:=pircoSHR;
         end;
         astnkOP_SAR:begin
          Opcode:=pircoSAR;
         end;
         else begin
          Opcode:=pircoNONE;
          TPACCInstance(AInstance).AddError('Internal error 2017-01-21-14-44-0001',nil,true);
         end;
        end;
        PrepareResultReference(Node.Type_);
        EmitInstruction(Opcode,OutputResultReference^,ReferenceA^,ReferenceB^);
        CheckResultReference(Node.Type_);
       end;
      finally
       FreeReference(ReferenceA);
       FreeReference(ReferenceB);
      end;
     end else begin
      TPACCInstance(AInstance).AddError('Internal error 2017-01-21-14-01-0001',nil,true);
     end;}
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
{var Index,ParameterIndex:longint;
    LocalVariable:TPACCAbstractSyntaxTreeNodeLocaLGlobalVariable;
    Reference:TPACCIntermediateRepresentationCodeReference;
    ReferenceValue:TPACCIntermediateRepresentationCodeReference;}
begin

 result:=TPACCIntermediateRepresentationCodeFunction.Create(AInstance);

 result.FunctionDeclaration:=AFunctionNode;

 result.FunctionName:=AFunctionNode.FunctionName;

 CodeInstance:=result;

 CurrentBlock:=nil;
 BlockLink:=@CodeInstance.StartBlock;
 PhiLink:=nil;

 NeedNewBlock:=true;

 EmitLabel(NewHiddenLabel);

{for Index:=0 to AFunctionNode.LocalVariables.Count-1 do begin

  LocalVariable:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(AFunctionNode.LocalVariables[Index]);
  if assigned(LocalVariable) then begin

   Reference:=GetVariableReference(LocalVariable);

   ParameterIndex:=AFunctionNode.Parameters.IndexOf(LocalVariable);
   if ParameterIndex>=0 then begin

   end;

  end;

 end;}

 ProcessNode(AFunctionNode.Body,nil,vkNONE);

 if CurrentBlock.Jump.Kind=pircjkNONE then begin
  CurrentBlock.Jump.Kind:=pircjkRET;
 end;

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
   if assigned(Node) then begin
    case Node.Kind of
     astnkDECL:begin
     end;
     astnkFUNC:begin
      GenerateIntermediateRepresentationCodeForFunction(AInstance,TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node));
     end;
    end;
   end;
  end;
 end else begin
  TPACCInstance(AInstance).AddError('Internal error 2017-01-19-11-48-0000',nil,true);
 end;
end;

end.
