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

const PACCIntermediateRepresentationCodeINTTypeKinds=[tkBOOL,tkCHAR,tkSHORT,tkINT];
      PACCIntermediateRepresentationCodeLONGTypeKinds=[tkLONG,tkLLONG];
      PACCIntermediateRepresentationCodeFLOATTypeKinds=[tkFLOAT];
      PACCIntermediateRepresentationCodeDOUBLETypeKinds=[tkDOUBLE,tkLDOUBLE];

type PPACCIntermediateRepresentationCodeOpcode=^TPACCIntermediateRepresentationCodeOpcode;
     TPACCIntermediateRepresentationCodeOpcode=
      (
       pircoNONE,

       pircoNOP,

       pircoASM,

       pircoMOVI, // Move from int to int
       pircoMOVL, // Move from long to long
       pircoMOVF, // Move from float to float
       pircoMODD, // Move from double to double

       pircoSWAPI, // Swap between int and int
       pircoSWAPL, // Swap between long and long
       pircoSWAPF, // Swap between float and float
       pircoSWAPD, // Swap between double and double

       picroCITF, // Convert int to float
       picroCLTF, // Convert long to float
       picroCITD, // Convert int to double
       picroCLTD, // Convert long to double

       picroCITL, // Convert int to long         (CDQ instruction (doubleword eax => quadcore eax:edx) on x86)
       picroCLTO, // Convert long to double-long (CQO instruction (  quadword rax => octocore rax:rdx) on x86)

       picroTFTI, // Truncate float to int
       picroTFTL, // Truncate float to long
       picroTDTI, // Truncate double to int
       picroTDTL, // Truncate double to long

       pircoCASTFI, // Bitwise cast from float to int
       pircoCASTIF, // Bitwise cast from int to float
       pircoCASTDL, // Bitwise cast from double to long
       pircoCASTLD, // Bitwise cast from long to double

       pircoADDROFI, // Address-of to int (on 32-bit targets)
       pircoADDROFL, // Address-of to long (on 64-bit targets)

       pircoZECI, // Zero extend from char to int
       pircoZESI, // Zero extend from short to int
       pircoZECL, // Zero extend from char to long
       pircoZESL, // Zero extend from short to long
       pircoZEIL, // Zero extend from int to long

       pircoSECI, // Sign extend from char to int
       pircoSESI, // Sign extend from short to int
       pircoSECL, // Sign extend from char to long
       pircoSESL, // Sign extend from short to long
       pircoSEIL, // Sign extend from int to long

       pircoTRLI, // Truncate from long to int

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
       pirctNONE,
       pirctINT,
       pirctLONG,
       pirctFLOAT,
       pirctDOUBLE
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
      Index:TPACCInt32;
      Type_:TPACCIntermediateRepresentationCodeType;
     end;

     TPACCIntermediateRepresentationCodeTemporaries=array of TPACCIntermediateRepresentationCodeTemporary;

     PPACCIntermediateRepresentationCodeOperandFlag=^TPACCIntermediateRepresentationCodeOperandFlag;
     TPACCIntermediateRepresentationCodeOperandFlag=
      (
       pircofMEMORY
     );

     PPACCIntermediateRepresentationCodeOperandFlags=^TPACCIntermediateRepresentationCodeOperandFlags;
     TPACCIntermediateRepresentationCodeOperandFlags=set of TPACCIntermediateRepresentationCodeOperandFlag;

     PPACCIntermediateRepresentationCodeOperandKind=^TPACCIntermediateRepresentationCodeOperandKind;
     TPACCIntermediateRepresentationCodeOperandKind=
      (
       pircokNONE,
       pircokTEMPORARY,
       pircokINTEGER,
       pircokFLOAT,
       pircokVARIABLE,
       pircokLABEL,
       pircokCOUNT
     );

     PPACCIntermediateRepresentationCodeOperand=^TPACCIntermediateRepresentationCodeOperand;
     TPACCIntermediateRepresentationCodeOperand=record
      Flags:TPACCIntermediateRepresentationCodeOperandFlags;
      case Kind:TPACCIntermediateRepresentationCodeOperandKind of
       pircokNONE:(
       );
       pircokTEMPORARY:(
        Temporary:TPACCInt32;
       );
       pircokINTEGER:(
        IntegerValue:TPACCInt64;
       );
       pircokFLOAT:(
        FloatValue:TPACCDouble;
       );
       pircokVARIABLE:(
        Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;
       );
       pircokLABEL:(
        Label_:TPACCAbstractSyntaxTreeNodeLabel;
       );
       pircokCOUNT:(
       );
     end;

     TPACCIntermediateRepresentationCodeOperands=array of TPACCIntermediateRepresentationCodeOperand;

     PPACCIntermediateRepresentationCodeInstruction=^TPACCIntermediateRepresentationCodeInstruction;
     TPACCIntermediateRepresentationCodeInstruction={$ifdef HAS_ADVANCED_RECORDS}record{$else}object{$endif}
      public
       Opcode:TPACCIntermediateRepresentationCodeOpcode;
       Operands:TPACCIntermediateRepresentationCodeOperands;
     end;

     TPACCIntermediateRepresentationCodeInstructions=array of TPACCIntermediateRepresentationCodeInstruction;

     PPACCIntermediateRepresentationCodeJump=^TPACCIntermediateRepresentationCodeJump;
     TPACCIntermediateRepresentationCodeJump=record
      Kind:TPACCIntermediateRepresentationCodeJumpKind;
      Operand:TPACCIntermediateRepresentationCodeOperand;
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
      Type_:TPACCIntermediateRepresentationCodeType;
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

       function CreateTemporary(const Type_:TPACCIntermediateRepresentationCodeType):TPACCInt32;

      published
       property Instance:TObject read fInstance;
     end;

     TPACCIntermediateRepresentationCodeFunctionList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCIntermediateRepresentationCodeFunction;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCIntermediateRepresentationCodeFunction);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCIntermediateRepresentationCodeFunction read GetItem write SetItem; default;
     end;

     TPACCIntermediateRepresentationCode=class
      private

       fInstance:TObject;

       fFunctions:TPACCIntermediateRepresentationCodeFunctionList;

      public

       constructor Create(const AInstance:TObject); reintroduce;
       destructor Destroy; override;

      published

       property Instance:TObject read fInstance;

       property Functions:TPACCIntermediateRepresentationCodeFunctionList read fFunctions;

     end;

procedure GenerateIntermediateRepresentationCode(const AInstance:TObject;const ARootAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNode);

implementation

uses PACCInstance;

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

function TPACCIntermediateRepresentationCodeFunction.CreateTemporary(const Type_:TPACCIntermediateRepresentationCodeType):TPACCInt32;
var Temporary:PPACCIntermediateRepresentationCodeTemporary;
begin
 result:=CountTemporaries;
 inc(CountTemporaries);
 if length(Temporaries)<CountTemporaries then begin
  SetLength(Temporaries,CountTemporaries*2);
 end;
 Temporary:=@Temporaries[result];
 Temporary^.Index:=result;
 Temporary^.Type_:=Type_;
end;

procedure GenerateIntermediateRepresentationCode(const AInstance:TObject;const ARootAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNode);
 function EmitFunction(const AFunctionNode:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration):TPACCIntermediateRepresentationCodeFunction;
 type PValueKind=^TValueKind;
      TValueKind=
       (
        vkNONE,
        vkLVALUE,
        vkRVALUE
       );
 var CurrentBlock:TPACCIntermediateRepresentationCodeBlock;
     BlockLink,PhiLink:PPACCIntermediateRepresentationCodeBlock;
     Function_:TPACCIntermediateRepresentationCodeFunction;
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
   result:=Function_.BlockLabelHashMap[Label_];
   if not assigned(result) then begin
    result:=TPACCIntermediateRepresentationCodeBlock.Create(AInstance);
    result.Index:=Function_.Blocks.Add(result);
    result.Label_:=Label_;
    Function_.BlockLabelHashMap[Label_]:=result;
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
  function CreateTemporary(const Type_:TPACCIntermediateRepresentationCodeType):TPACCInt32;
  begin
   result:=Function_.CreateTemporary(Type_);
  end;
  function CreateTemporaryOperand(const Temporary:TPACCInt32):TPACCIntermediateRepresentationCodeOperand;
  begin
   result.Kind:=pircokTEMPORARY;
   result.Temporary:=Temporary;
  end;
  procedure EmitInstruction(const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const AOperands:array of TPACCIntermediateRepresentationCodeOperand); overload;
  var Index:TPACCInt32;
      Instruction:TPACCIntermediateRepresentationCodeInstruction;
  begin
   Instruction.Opcode:=AOpcode;
   Instruction.Operands:=nil;
   if length(AOperands)>0 then begin
    SetLength(Instruction.Operands,length(AOperands));
    for Index:=0 to length(AOperands)-1 do begin
     Instruction.Operands[Index]:=AOperands[Index];
    end;
   end;
   CreateNewBlockIfNeeded;
   CurrentBlock.AddInstruction(Instruction);
  end;
  procedure ProcessNode(const Node:TPACCAbstractSyntaxTreeNode;var OutputTemporary:TPACCInt32;const ValueKind:TValueKind);
  var Index,TemporaryA,TemporaryB,TemporaryC:TPACCInt32;
      Opcode:TPACCIntermediateRepresentationCodeOpcode;
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

 // writeln(TypInfo.GetEnumName(TypeInfo(TPACCAbstractSyntaxTreeNodeKind),TPACCInt32(Node.Kind)));

    case Node.Kind of

     astnkINTEGER:begin
 {    if assigned(OutputResultReference) and (ValueKind=vkRVALUE) then begin
       AllocateReference(ReferenceA);
       try
        OutputResultReference^:=Function_.CreateTemporaryReference(Node.Type_);
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
        OutputResultReference^:=Function_.CreateTemporaryReference(Node.Type_);
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
          OutputResultReference^:=Function_.CreateTemporaryReference(Node.Type_);
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
      if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) and
         assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_) and
         assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_) then begin
       if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_=TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_) or
          ((TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Kind=TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind) and
           ((TPACCInstance(AInstance).IsIntType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_) and TPACCInstance(AInstance).IsIntType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_)) or
            (TPACCInstance(AInstance).IsFloatType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_) and TPACCInstance(AInstance).IsFloatType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_)) or
            ((TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Kind=tkPOINTER) and (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind=tkPOINTER)))) then begin
        ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,OutputTemporary,ValueKind);
       end else begin
        case TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Kind of
         tkBOOL,tkCHAR,tkSHORT,tkINT:begin
          case TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind of
           tkBOOL:begin
            TemporaryA:=-1;
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,ValueKind);
            OutputTemporary:=CreateTemporary(pirctINT);
            if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
               (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
             EmitInstruction(pircoZECI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
            end else begin
             EmitInstruction(pircoSECI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
            end;
           end;
           tkCHAR:begin
            TemporaryA:=-1;
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,ValueKind);
            OutputTemporary:=CreateTemporary(pirctINT);
            if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
               (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
             EmitInstruction(pircoZECI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
            end else begin
             EmitInstruction(pircoSECI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
            end;
           end;
           tkSHORT:begin
            TemporaryA:=-1;
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,ValueKind);
            OutputTemporary:=CreateTemporary(pirctINT);
            if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
               (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
             EmitInstruction(pircoZESI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
            end else begin
             EmitInstruction(pircoSESI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
            end;
           end;
           tkINT:begin
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,OutputTemporary,ValueKind);
           end;
           tkLONG:begin
            TemporaryA:=-1;
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,ValueKind);
            OutputTemporary:=CreateTemporary(pirctINT);
            EmitInstruction(pircoTRLI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
           end;
           tkLLONG:begin
            TemporaryA:=-1;
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,ValueKind);
            OutputTemporary:=CreateTemporary(pirctINT);
            EmitInstruction(pircoTRLI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
           end;
           tkFLOAT:begin
            TemporaryA:=-1;
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,ValueKind);
            OutputTemporary:=CreateTemporary(pirctINT);
            EmitInstruction(picroTFTI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
           end;
           tkDOUBLE:begin
            TemporaryA:=-1;
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,ValueKind);
            OutputTemporary:=CreateTemporary(pirctINT);
            EmitInstruction(picroTDTI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
           end;
           tkLDOUBLE:begin
            TemporaryA:=-1;
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,ValueKind);
            OutputTemporary:=CreateTemporary(pirctINT);
            EmitInstruction(picroTDTI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
           end;
           tkPOINTER:begin
            if TPACCInstance(AInstance).Target.SizeOfPointer=TPACCInstance(AInstance).Target.SizeOfLong then begin
             TemporaryA:=-1;
             ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,ValueKind);
             OutputTemporary:=CreateTemporary(pirctINT);
             EmitInstruction(pircoTRLI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
            end else begin
             ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,OutputTemporary,ValueKind);
            end;
           end;
           else begin
            TPACCInstance(AInstance).AddError('Internal error 2017-01-22-15-12-0000',@Node.SourceLocation,true);
           end;
          end;
         end;
         tkLONG,tkLLONG:begin
          case TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind of
           tkBOOL:begin
            TemporaryA:=-1;
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,ValueKind);
            OutputTemporary:=CreateTemporary(pirctLONG);
            if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
               (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
             EmitInstruction(pircoZECL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
            end else begin
             EmitInstruction(pircoSECL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
            end;
           end;
           tkCHAR:begin
            TemporaryA:=-1;
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,ValueKind);
            OutputTemporary:=CreateTemporary(pirctLONG);
            if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
               (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
             EmitInstruction(pircoZECL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
            end else begin
             EmitInstruction(pircoSECL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
            end;
           end;
           tkSHORT:begin
            TemporaryA:=-1;
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,ValueKind);
            OutputTemporary:=CreateTemporary(pirctLONG);
            if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
               (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
             EmitInstruction(pircoZESL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
            end else begin
             EmitInstruction(pircoSESL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
            end;
           end;
           tkINT:begin
            TemporaryA:=-1;
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,ValueKind);
            OutputTemporary:=CreateTemporary(pirctLONG);
            if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
               (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
             EmitInstruction(pircoZEIL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
            end else begin
             EmitInstruction(pircoSEIL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
            end;
           end;
           tkLONG:begin
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,OutputTemporary,ValueKind);
           end;
           tkLLONG:begin
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,OutputTemporary,ValueKind);
           end;
           tkFLOAT:begin
            TemporaryA:=-1;
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,ValueKind);
            OutputTemporary:=CreateTemporary(pirctLONG);
            EmitInstruction(picroTFTL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
           end;
           tkDOUBLE:begin
            TemporaryA:=-1;
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,ValueKind);
            OutputTemporary:=CreateTemporary(pirctLONG);
            EmitInstruction(picroTDTL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
           end;
           tkLDOUBLE:begin
            TemporaryA:=-1;
            ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,ValueKind);
            OutputTemporary:=CreateTemporary(pirctLONG);
            EmitInstruction(picroTDTL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
           end;
           tkPOINTER:begin
            if TPACCInstance(AInstance).Target.SizeOfPointer=TPACCInstance(AInstance).Target.SizeOfLong then begin
             ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,OutputTemporary,ValueKind);
            end else begin
             TemporaryA:=-1;
             ProcessNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,ValueKind);
             OutputTemporary:=CreateTemporary(pirctLONG);
             if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
                (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
              EmitInstruction(pircoZEIL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
             end else begin
              EmitInstruction(pircoSEIL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)]);
             end;
            end;
           end;
           else begin
            TPACCInstance(AInstance).AddError('Internal error 2017-01-22-15-12-0000',@Node.SourceLocation,true);
           end;
          end;
         end;
         tkFLOAT:begin
         end;
         tkDOUBLE,tkLDOUBLE:begin
         end;
         tkPOINTER:begin
         end;
         else begin
          TPACCInstance(AInstance).AddError('Internal error 2017-01-22-15-19-0000',@Node.SourceLocation,true);
         end;
        end;
       end;
      end else begin
       TPACCInstance(AInstance).AddError('Internal error 2017-01-22-14-53-0000',@Node.SourceLocation,true);
      end;
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
             OutputResultReference^:=Function_.CreateTemporaryReference(Node.Type_);
             if (tfUnsigned in Node.Type_^.Flags) or (tfUnsigned in ReferenceA^.Type_^.Flags) then begin
              Opcode:=pircoZEROEXTEND8TO32;
             end else begin
              Opcode:=pircoSIGNEXTEND8TO32;
             end;
             EmitInstruction(Opcode,OutputResultReference^,ReferenceA^);
            end;
            tkSHORT:begin
             OutputResultReference^:=Function_.CreateTemporaryReference(Node.Type_);
             if not ((tfUnsigned in Node.Type_^.Flags) or (tfUnsigned in ReferenceA^.Type_^.Flags)) then begin
              Opcode:=pircoZEROEXTEND16TO32;
             end else begin
              Opcode:=pircoSIGNEXTEND16TO32;
             end;
             EmitInstruction(Opcode,OutputResultReference^,ReferenceA^);
            end;
            tkINT:begin
             OutputResultReference^:=Function_.CreateTemporaryReference(Node.Type_);
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
      if assigned(AFunctionNode.Type_) and
         assigned(AFunctionNode.Type_^.ReturnType) then begin
       TemporaryA:=-1;
       if assigned(TPACCAbstractSyntaxTreeNodeRETURNStatement(Node).ReturnValue) then begin
        ProcessNode(TPACCAbstractSyntaxTreeNodeRETURNStatement(Node).ReturnValue,TemporaryA,vkRVALUE);
        if TemporaryA<0 then begin
         TPACCInstance(AInstance).AddError('Internal error 2017-01-22-14-51-0000',@Node.SourceLocation,true);
        end;
       end else begin
        TPACCInstance(AInstance).AddError('Internal error 2017-01-22-14-48-0000',@Node.SourceLocation,true);
       end;
       if CurrentBlock.Jump.Kind=pircjkNONE then begin
        case AFunctionNode.Type_^.ReturnType^.Kind of
         tkBOOL,tkCHAR,tkSHORT,tkINT:begin
          CurrentBlock.Jump.Kind:=pircjkRETI;
         end;
         tkLONG,tkLLONG:begin
          CurrentBlock.Jump.Kind:=pircjkRETL;
         end;
         tkFLOAT:begin
          CurrentBlock.Jump.Kind:=pircjkRETF;
         end;
         tkDOUBLE,tkLDOUBLE:begin
          CurrentBlock.Jump.Kind:=pircjkRETD;
         end;
         tkPOINTER:begin
          if TPACCInstance(AInstance).Target.SizeOfPointer=TPACCInstance(AInstance).Target.SizeOfLong then begin
           CurrentBlock.Jump.Kind:=pircjkRETL;
          end else begin
           CurrentBlock.Jump.Kind:=pircjkRETI;
          end;
         end;
         else begin
          TPACCInstance(AInstance).AddError('Internal error 2017-01-22-14-44-0000',@Node.SourceLocation,true);
         end;
        end;
        if TemporaryA>=0 then begin
         CurrentBlock.Jump.Operand.Kind:=pircokTEMPORARY;
         CurrentBlock.Jump.Operand.Temporary:=TemporaryA;
        end else begin
         case AFunctionNode.Type_^.ReturnType^.Kind of
          tkBOOL,tkCHAR,tkSHORT,tkINT,tkLONG,tkLLONG,tkPOINTER:begin
           CurrentBlock.Jump.Operand.Kind:=pircokINTEGER;
           CurrentBlock.Jump.Operand.IntegerValue:=0;
          end;
          tkFLOAT,tkDOUBLE,tkLDOUBLE:begin
           CurrentBlock.Jump.Operand.Kind:=pircokFLOAT;
           CurrentBlock.Jump.Operand.FloatValue:=0;
          end;
          else begin
           TPACCInstance(AInstance).AddError('Internal error 2017-01-22-14-50-0000',@Node.SourceLocation,true);
          end;
         end;
        end;
        CloseBlock;
       end else begin
        TPACCInstance(AInstance).AddError('Internal error 2017-01-21-13-54-0000',@Node.SourceLocation,true);
       end;
      end else begin
       if CurrentBlock.Jump.Kind=pircjkNONE then begin
        CurrentBlock.Jump.Kind:=pircjkRET;
        CloseBlock;
       end else begin
        TPACCInstance(AInstance).AddError('Internal error 2017-01-21-13-54-0000',@Node.SourceLocation,true);
       end;
      end;
     end;

     astnkSTATEMENTS:begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeStatements(Node).Children.Count-1 do begin
       TemporaryA:=-1;
       ProcessNode(TPACCAbstractSyntaxTreeNodeStatements(Node).Children[Index],TemporaryA,vkNONE);
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
       TemporaryA:=-1;
       ProcessNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left,TemporaryA,ValueKind);
       ProcessNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right,OutputTemporary,ValueKind);
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
 var IgnoredTemporary:TPACCInt32;
 begin

  result:=TPACCIntermediateRepresentationCodeFunction.Create(AInstance);

  result.FunctionDeclaration:=AFunctionNode;

  result.FunctionName:=AFunctionNode.FunctionName;

  Function_:=result;

  TPACCInstance(AInstance).IntermediateRepresentationCode.Functions.Add(Function_);

  CurrentBlock:=nil;
  BlockLink:=@Function_.StartBlock;
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

  ProcessNode(AFunctionNode.Body,IgnoredTemporary,vkNONE);

  if CurrentBlock.Jump.Kind=pircjkNONE then begin
   CurrentBlock.Jump.Kind:=pircjkRET;
  end;

 end;
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
     astnkEXTERN_DECL:begin
     end;
     astnkDECL:begin
     end;
     astnkFUNC:begin
      EmitFunction(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node));
     end;
     else begin
      TPACCInstance(AInstance).AddError('Internal error 2017-01-22-14-05-0000',@Node.SourceLocation,true);
     end;
    end;
   end;
  end;
 end else begin
  TPACCInstance(AInstance).AddError('Internal error 2017-01-19-11-48-0000',nil,true);
 end;
end;

constructor TPACCIntermediateRepresentationCodeFunctionList.Create;
begin
 inherited Create;
end;

destructor TPACCIntermediateRepresentationCodeFunctionList.Destroy;
begin
 inherited Destroy;
end;

function TPACCIntermediateRepresentationCodeFunctionList.GetItem(const AIndex:TPACCInt):TPACCIntermediateRepresentationCodeFunction;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCIntermediateRepresentationCodeFunctionList.SetItem(const AIndex:TPACCInt;const AItem:TPACCIntermediateRepresentationCodeFunction);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

constructor TPACCIntermediateRepresentationCode.Create(const AInstance:TObject);
begin

 inherited Create;

 fInstance:=AInstance;

 fFunctions:=TPACCIntermediateRepresentationCodeFunctionList.Create;

end;

destructor TPACCIntermediateRepresentationCode.Destroy;
begin
 fFunctions.Free;
 inherited Destroy;
end;

end.
