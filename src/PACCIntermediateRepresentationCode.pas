unit PACCIntermediateRepresentationCode;
{$i PACC.inc}

interface

uses TypInfo,SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCPointerHashMap,PACCAbstractSyntaxTree;

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

const PACCIntermediateRepresentationCodeINTTypeKinds=[tkBOOL,tkCHAR,tkSHORT,tkINT,tkENUM];
      PACCIntermediateRepresentationCodeLONGTypeKinds=[tkLONG,tkLLONG];
      PACCIntermediateRepresentationCodeFLOATTypeKinds=[tkFLOAT];
      PACCIntermediateRepresentationCodeDOUBLETypeKinds=[tkDOUBLE,tkLDOUBLE];

type PPACCIntermediateRepresentationCodeOpcode=^TPACCIntermediateRepresentationCodeOpcode;
     TPACCIntermediateRepresentationCodeOpcode=
      (
       pircoNONE,

       pircoNOP,

       pircoASM,

       pircoSETI, // Set int
       pircoSETL, // Set long
       pircoSETF, // Set float
       pircoSETD, // Set double

       pircoMOVI, // Move from int to int
       pircoMOVL, // Move from long to long
       pircoMOVF, // Move from float to float
       pircoMOVD, // Move from double to double

       pircoSWAPI, // Swap between int and int
       pircoSWAPL, // Swap between long and long
       pircoSWAPF, // Swap between float and float
       pircoSWAPD, // Swap between double and double

       pircoCITF, // Convert int to float
       pircoCLTF, // Convert long to float
       pircoCITD, // Convert int to double
       pircoCLTD, // Convert long to double
       pircoCFTD, // Convert float to double
       pircoCDTF, // Convert double to float

       pircoCITL, // Convert int to long         (CDQ instruction (doubleword eax => quadcore eax:edx) on x86)
       pircoCLTO, // Convert long to double-long (CQO instruction (  quadword rax => octocore rax:rdx) on x86)

       pircoTFTI, // Truncate float to int
       pircoTFTL, // Truncate float to long
       pircoTDTI, // Truncate double to int
       pircoTDTL, // Truncate double to long

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
       pircoLDUII, // Load from unsigned int to int
       pircoLDSCI, // Load from signed char to int
       pircoLDSSI, // Load from signed short to int
       pircoLDSII, // Load from signed int to int
       pircoLDUCL, // Load from unsigned char to long
       pircoLDUSL, // Load from unsigned short to long
       pircoLDUIL, // Load from unsigned int to long
       pircoLDULL, // Load from unsigned long to long
       pircoLDSCL, // Load from signed char to long
       pircoLDSSL, // Load from signed short to long
       pircoLDSIL, // Load from signed int to long
       pircoLDSLL, // Load from signed long to long
       pircoLDF, // Load from float to float
       pircoLDD, // Load from double to double

       pircoSTIC, // Store from int to char
       pircoSTIS, // Store from int to short
       pircoSTII, // Store from int to int
       pircoSTLC, // Store from long to char
       pircoSTLS, // Store from long to short
       pircoSTLI, // Store from long to int
       pircoSTLL, // Store from long to long
       pircoSTF,  // Store from float to float
       pircoSTD,  // Store from double to double

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

       pircoNEGF, // Negation float
       pircoADDF, // Addition float
       pircoSUBF, // Subtraction float
       pircoMULF, // Multiplication float
       pircoDIVF, // Division float

       pircoNEGD, // Negation double
       pircoADDD, // Addition double
       pircoSUBD, // Subtraction double
       pircoMULD, // Multiplication double
       pircoDIVD, // Division double

       pircoCMPSLEI, // Signed less than or equal int
       pircoCMPSLTI, // Signed less than int
       pircoCMPSGEI, // Signed greater than or equal int
       pircoCMPSGTI, // Signed greater than int
       pircoCMPULEI, // Unsigned less than or equal int
       pircoCMPULTI, // Unsigned less than int
       pircoCMPUGEI, // Unsigned greater than or equal int
       pircoCMPUGTI, // Unsigned greater than int
       pircoCMPEQI, // Equal int
       pircoCMPNEI, // Not equal int
       pircoCMPZI, // Zero int
       pircoCMPNZI, // Not zero int

       pircoCMPSLEL, // Signed less than or equal long
       pircoCMPSLTL, // Signed less than long
       pircoCMPSGEL, // Signed greater than or equal long
       pircoCMPSGTL, // Signed greater than long
       pircoCMPULEL, // Unsigned less than or equal long
       pircoCMPULTL, // Unsigned less than long
       pircoCMPUGEL, // Unsigned greater than or equal long
       pircoCMPUGTL, // Unsigned greater than long
       pircoCMPEQL, // Equal long
       pircoCMPNEL, // Not equal long
       pircoCMPZL, // Zero long
       pircoCMPNZL, // Not zero long

       pircoCMPLEF, // Less than or equal float
       pircoCMPLTF, // Less than float
       pircoCMPGEF, // Greater than or equal float
       pircoCMPGTF, // Greater than float
       pircoCMPEQF, // Equal float
       pircoCMPNEF, // Not equal float
       pircoCMPOF, // Ordered float
       pircoCMPNOF, // Not ordered float

       pircoCMPLED, // Less than or equal double
       pircoCMPLTD, // Less than double
       pircoCMPGED, // Greater than or equal double
       pircoCMPGTD, // Greater than double
       pircoCMPEQD, // Equal double
       pircoCMPNED, // Not equal double
       pircoCMPOD, // Ordered double
       pircoCMPNOD, // Not ordered double

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

       pircoCOUNT
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
       pircjkJMPA, // Jump to address
       pircjkJZI,   // Jump if zero int
       pircjkJNZI,  // Jump if not zero int
       pircjkJZL,   // Jump if zero long
       pircjkJNZL,  // Jump if not zero long
       pircjkJZF,   // Jump if zero float
       pircjkJNZF,  // Jump if not zero float
       pircjkJZD,   // Jump if zero double
       pircjkJNZD,  // Jump if not zero double
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

     PPPACCIntermediateRepresentationCodePhi=^PPACCIntermediateRepresentationCodePhi;
     PPACCIntermediateRepresentationCodePhi=^TPACCIntermediateRepresentationCodePhi;
     TPACCIntermediateRepresentationCodePhi=record
      To_:TPACCIntermediateRepresentationCodeOperand;
      Operands:array of TPACCIntermediateRepresentationCodeOperand;
      Blocks:array of TPACCIntermediateRepresentationCodeBlock;
      CountOperands:TPACCInt32;
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

     PPACCIntermediateRepresentationCodeValueKind=^TPACCIntermediateRepresentationCodeValueKind;
     TPACCIntermediateRepresentationCodeValueKind=
      (
       pircvkNONE,
       pircvkLVALUE,
       pircvkRVALUE
      );

     TPACCIntermediateRepresentationCodeUnaryOpHook=procedure(var OutputTemporary:TPACCInt32;const InputTemporary:TPACCInt32;const OpNode:TPACCAbstractSyntaxTreeNode) of object;

     TPACCIntermediateRepresentationCodeBinaryOpHook=procedure(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const OpNode:TPACCAbstractSyntaxTreeNode) of object;

     TPACCIntermediateRepresentationCodeFunction=class
      private

       fInstance:TObject;

       CurrentBlock:TPACCIntermediateRepresentationCodeBlock;
       BlockLink:PPACCIntermediateRepresentationCodeBlock;
       PhiLink:PPPACCIntermediateRepresentationCodePhi;
       NeedNewBlock:boolean;

       AssignOpLValueTemporary:TPACCInt32;

       function NewHiddenLabel:TPACCAbstractSyntaxTreeNodeLabel;
       procedure CloseBlock;
       function FindBlock(const Label_:TPACCAbstractSyntaxTreeNodeLabel):TPACCIntermediateRepresentationCodeBlock;
       procedure EmitLabel(const Label_:TPACCAbstractSyntaxTreeNodeLabel);
       procedure EmitJump(const Label_:TPACCAbstractSyntaxTreeNodeLabel);
       procedure EmitPhi(const Type_:TPACCIntermediateRepresentationCodeType;
                         const To_:TPACCIntermediateRepresentationCodeOperand;
                         const Operands:array of TPACCIntermediateRepresentationCodeOperand;
                         const Blocks:array of TPACCIntermediateRepresentationCodeBlock;
                         const SourceLocation:TPACCSourceLocation);
       procedure CreateNewBlockIfNeeded;
       function CreateTemporary(const Type_:TPACCIntermediateRepresentationCodeType):TPACCInt32;
       function CreateTemporaryOperand(const Temporary:TPACCInt32):TPACCIntermediateRepresentationCodeOperand;
       function CreateIntegerValueOperand(const Value:TPACCInt64):TPACCIntermediateRepresentationCodeOperand;
       function CreateFloatValueOperand(const Value:TPACCDouble):TPACCIntermediateRepresentationCodeOperand;
       function CreateVariableOperand(const Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable):TPACCIntermediateRepresentationCodeOperand;
       function CreateLabelOperand(const Label_:TPACCAbstractSyntaxTreeNodeLabel):TPACCIntermediateRepresentationCodeOperand;
       function SetOperandFlags(const Operand:TPACCIntermediateRepresentationCodeOperand;const IncludeFlags,ExcludeFlags:TPACCIntermediateRepresentationCodeOperandFlags):TPACCIntermediateRepresentationCodeOperand;
       procedure EmitInstruction(const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const AOperands:array of TPACCIntermediateRepresentationCodeOperand;const SourceLocation:TPACCSourceLocation); overload;
       procedure EmitLoad(var OutputTemporary:TPACCInt32;const InputLValueTemporary:TPACCInt32;const Type_:PPACCType);
       procedure EmitStore(const DestinationLValueTemporary,InputValueTemporary:TPACCInt32;const Type_:PPACCType);
       procedure EmitUnaryOpINC(var OutputTemporary:TPACCInt32;const InputTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitUnaryOpDEC(var OutputTemporary:TPACCInt32;const InputTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitBinaryOpADD(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitBinaryOpSUB(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitBinaryOpMUL(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitBinaryOpDIV(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitBinaryOpMOD(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitBinaryOpAND(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitBinaryOpOR(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitBinaryOpXOR(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitBinaryOpSHL(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitBinaryOpSHR(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitBinaryOpSAR(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitBinaryOpLE(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitBinaryOpLT(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitBinaryOpGE(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitBinaryOpGT(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitBinaryOpEQ(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitBinaryOpNE(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitNOT(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
       procedure EmitNEG(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
       procedure EmitLoadUnaryOpStore(const LValueNode,Node:TPACCAbstractSyntaxTreeNode;const UnaryOpHook:TPACCIntermediateRepresentationCodeUnaryOpHook;var OutputTemporary:TPACCInt32;const PostOp:boolean);
       procedure EmitBinaryOp(const Node:TPACCAbstractSyntaxTreeNode;const BinaryOpHook:TPACCIntermediateRepresentationCodeBinaryOpHook;var OutputTemporary:TPACCInt32);
       procedure EmitAssignmentBinaryOp(const Node:TPACCAbstractSyntaxTreeNode;const BinaryOpHook:TPACCIntermediateRepresentationCodeBinaryOpHook;var OutputTemporary:TPACCInt32);
       procedure EmitAssign(const Node:TPACCAbstractSyntaxTreeNodeBinaryOperator;var OutputTemporary:TPACCInt32);
       procedure EmitAssignOp(const Node:TPACCAbstractSyntaxTreeNodeBinaryOperator;var OutputTemporary:TPACCInt32);
       procedure EmitAssignSrc(const Node:TPACCAbstractSyntaxTreeNode;var OutputTemporary:TPACCInt32;const ValueKind:TPACCIntermediateRepresentationCodeValueKind);
       procedure EmitIntegerValue(const Node:TPACCAbstractSyntaxTreeNodeIntegerValue;var OutputTemporary:TPACCInt32);
       procedure EmitFloatValue(const Node:TPACCAbstractSyntaxTreeNodeFloatValue;var OutputTemporary:TPACCInt32);
       procedure EmitStoreToVariable(const Node:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;const InputTemporary:TPACCInt32);
       procedure EmitVariable(const Node:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;var OutputTemporary:TPACCInt32;const InputTemporaries:array of TPACCInt32;const ValueKind:TPACCIntermediateRepresentationCodeValueKind);
       procedure EmitComputedJump(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator);
       procedure EmitLogicalANDOR(const Node:TPACCAbstractSyntaxTreeNodeBinaryOperator;var OutputTemporary:TPACCInt32;const IsAND:boolean);
       procedure EmitLogicalNOT(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
       procedure EmitCONV(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32;const ValueKind:TPACCIntermediateRepresentationCodeValueKind);
       procedure EmitADDR(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32;const ValueKind:TPACCIntermediateRepresentationCodeValueKind);
       procedure EmitDEREF(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32;const ValueKind:TPACCIntermediateRepresentationCodeValueKind);
       procedure EmitRETURN(const Node:TPACCAbstractSyntaxTreeNodeRETURNStatement);
       procedure EmitCOMMA(const Node:TPACCAbstractSyntaxTreeNodeBinaryOperator;var OutputTemporary:TPACCInt32;const ValueKind:TPACCIntermediateRepresentationCodeValueKind);
       procedure EmitExpression(const Node:TPACCAbstractSyntaxTreeNode;var OutputTemporary:TPACCInt32;const InputTemporaries:array of TPACCInt32;const ValueKind:TPACCIntermediateRepresentationCodeValueKind);
       procedure EmitStatement(const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitStatements(const Node:TPACCAbstractSyntaxTreeNodeStatements);
       procedure EmitFunction(const AFunctionNode:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration);

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

function TPACCIntermediateRepresentationCodeFunction.NewHiddenLabel:TPACCAbstractSyntaxTreeNodeLabel;
begin
 result:=TPACCAbstractSyntaxTreeNodeLabel.Create(fInstance,astnkHIDDEN_LABEL,nil,TPACCInstance(fInstance).SourceLocation,'');
end;

procedure TPACCIntermediateRepresentationCodeFunction.CloseBlock;
begin
 BlockLink:=@CurrentBlock.Link;
 NeedNewBlock:=true;
end;

function TPACCIntermediateRepresentationCodeFunction.FindBlock(const Label_:TPACCAbstractSyntaxTreeNodeLabel):TPACCIntermediateRepresentationCodeBlock;
begin
 result:=BlockLabelHashMap[Label_];
 if not assigned(result) then begin
  result:=TPACCIntermediateRepresentationCodeBlock.Create(fInstance);
  result.Index:=Blocks.Add(result);
  result.Label_:=Label_;
  BlockLabelHashMap[Label_]:=result;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitLabel(const Label_:TPACCAbstractSyntaxTreeNodeLabel);
var Block:TPACCIntermediateRepresentationCodeBlock;
begin
 Block:=FindBlock(Label_);
 if assigned(CurrentBlock) and (CurrentBlock.Jump.Kind=pircjkNONE) then begin
  CloseBlock;
  CurrentBlock.Jump.Kind:=pircjkJMP;
  CurrentBlock.Successors[0]:=Block;
 end;
 if Block.Jump.Kind<>pircjkNONE then begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-20-14-42-0000',nil,true);
 end;
 BlockLink^:=Block;
 CurrentBlock:=Block;
 PhiLink:=@CurrentBlock.Phi;
 NeedNewBlock:=false;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitJump(const Label_:TPACCAbstractSyntaxTreeNodeLabel);
var Block:TPACCIntermediateRepresentationCodeBlock;
begin
 Block:=FindBlock(Label_);
 CurrentBlock.Jump.Kind:=pircjkJMP;
 CurrentBlock.Successors[0]:=Block;
 CloseBlock;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitPhi(const Type_:TPACCIntermediateRepresentationCodeType;
                                                              const To_:TPACCIntermediateRepresentationCodeOperand;
                                                              const Operands:array of TPACCIntermediateRepresentationCodeOperand;
                                                              const Blocks:array of TPACCIntermediateRepresentationCodeBlock;
                                                              const SourceLocation:TPACCSourceLocation);
var Index:TPACCInt32;
    Phi:PPACCIntermediateRepresentationCodePhi;
begin
 if assigned(CurrentBlock) then begin
  if assigned(CurrentBlock.Phi) then begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-24-10-59-0001',nil,true);
  end else if length(Operands)=0 then begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-24-11-01-0000',nil,true);
  end else if length(Operands)<>length(Blocks) then begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-24-11-01-0001',nil,true);
  end else begin
   GetMem(Phi,SizeOf(TPACCIntermediateRepresentationCodePhi));
   FillChar(Phi^,SizeOf(TPACCIntermediateRepresentationCodePhi),#0);
   CurrentBlock.Phi:=Phi;
   Phi.Type_:=Type_;
   Phi.To_:=To_;
   Phi.CountOperands:=length(Operands);
   SetLength(Phi.Operands,Phi.CountOperands);
   SetLength(Phi.Blocks,Phi.CountOperands);
   for Index:=0 to Phi.CountOperands-1 do begin
    Phi.Operands[Index]:=Operands[Index];
    Phi.Blocks[Index]:=Blocks[Index];
   end;
   PhiLink^:=Phi;
   PhiLink:=@Phi^.Link;
  end;
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-10-59-0000',nil,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.CreateNewBlockIfNeeded;
begin
 if NeedNewBlock then begin
  EmitLabel(NewHiddenLabel);
 end;
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

function TPACCIntermediateRepresentationCodeFunction.CreateTemporaryOperand(const Temporary:TPACCInt32):TPACCIntermediateRepresentationCodeOperand;
begin
 result.Flags:=[];
 result.Kind:=pircokTEMPORARY;
 result.Temporary:=Temporary;
end;

function TPACCIntermediateRepresentationCodeFunction.CreateIntegerValueOperand(const Value:TPACCInt64):TPACCIntermediateRepresentationCodeOperand;
begin
 result.Flags:=[];
 result.Kind:=pircokINTEGER;
 result.IntegerValue:=Value;
end;

function TPACCIntermediateRepresentationCodeFunction.CreateFloatValueOperand(const Value:TPACCDouble):TPACCIntermediateRepresentationCodeOperand;
begin
 result.Flags:=[];
 result.Kind:=pircokFLOAT;
 result.FloatValue:=Value;
end;

function TPACCIntermediateRepresentationCodeFunction.CreateVariableOperand(const Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable):TPACCIntermediateRepresentationCodeOperand;
begin
 result.Flags:=[];
 result.Kind:=pircokVARIABLE;
 result.Variable:=Variable;
end;

function TPACCIntermediateRepresentationCodeFunction.CreateLabelOperand(const Label_:TPACCAbstractSyntaxTreeNodeLabel):TPACCIntermediateRepresentationCodeOperand;
begin
 result.Flags:=[];
 result.Kind:=pircokLABEL;
 result.Label_:=Label_;
end;

function TPACCIntermediateRepresentationCodeFunction.SetOperandFlags(const Operand:TPACCIntermediateRepresentationCodeOperand;const IncludeFlags,ExcludeFlags:TPACCIntermediateRepresentationCodeOperandFlags):TPACCIntermediateRepresentationCodeOperand;
begin
 result:=Operand;
 result.Flags:=(result.Flags-ExcludeFlags)+IncludeFlags;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitInstruction(const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const AOperands:array of TPACCIntermediateRepresentationCodeOperand;const SourceLocation:TPACCSourceLocation);
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

procedure TPACCIntermediateRepresentationCodeFunction.EmitLoad(var OutputTemporary:TPACCInt32;const InputLValueTemporary:TPACCInt32;const Type_:PPACCType);
begin
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitStore(const DestinationLValueTemporary,InputValueTemporary:TPACCInt32;const Type_:PPACCType);
begin
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitUnaryOpINC(var OutputTemporary:TPACCInt32;const InputTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoADDI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(1)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoADDL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(1)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoADDF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputTemporary),CreateFloatValueOperand(1)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoADDD,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputTemporary),CreateFloatValueOperand(1)],Node.SourceLocation);
 end else if Node.Type_^.Kind=tkPOINTER then begin
  if assigned(Node.Type_^.ChildType) then begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoADDI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(Node.Type_^.ChildType.Size)],Node.SourceLocation);
   end else begin
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoADDL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(Node.Type_^.ChildType.Size)],Node.SourceLocation);
   end;
  end else begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoADDI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(1)],Node.SourceLocation);
   end else begin
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoADDL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(1)],Node.SourceLocation);
   end;
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitUnaryOpDEC(var OutputTemporary:TPACCInt32;const InputTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoSUBI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(1)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoSUBL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(1)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoSUBF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputTemporary),CreateFloatValueOperand(1)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoSUBD,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputTemporary),CreateFloatValueOperand(1)],Node.SourceLocation);
 end else if Node.Type_^.Kind=tkPOINTER then begin
  if assigned(Node.Type_^.ChildType) then begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoSUBI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(Node.Type_^.ChildType.Size)],Node.SourceLocation);
   end else begin
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoSUBL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(Node.Type_^.ChildType.Size)],Node.SourceLocation);
   end;
  end else begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoSUBI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(1)],Node.SourceLocation);
   end else begin
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoSUBL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(1)],Node.SourceLocation);
   end;
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpADD(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
var IntermediateResultTemporary:TPACCInt32;
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoADDI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoADDL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoADDF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoADDD,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind=tkPOINTER then begin
  if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Kind=tkPOINTER) and
     assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType) and
     TPACCInstance(fInstance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    IntermediateResultTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoUMULI,[CreateTemporaryOperand(IntermediateResultTemporary),CreateTemporaryOperand(InputRightTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType^.Size)],Node.SourceLocation);
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoADDI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(IntermediateResultTemporary)],Node.SourceLocation);
   end else begin
    IntermediateResultTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoUMULL,[CreateTemporaryOperand(IntermediateResultTemporary),CreateTemporaryOperand(InputRightTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType^.Size)],Node.SourceLocation);
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoADDL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(IntermediateResultTemporary)],Node.SourceLocation);
   end;
  end else if TPACCInstance(fInstance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) and
              (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Kind=tkPOINTER) and
              assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.ChildType) then begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    IntermediateResultTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoUMULI,[CreateTemporaryOperand(IntermediateResultTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.ChildType^.Size)],Node.SourceLocation);
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoADDI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(IntermediateResultTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
   end else begin
    IntermediateResultTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoUMULL,[CreateTemporaryOperand(IntermediateResultTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.ChildType^.Size)],Node.SourceLocation);
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoADDL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(IntermediateResultTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
   end;
  end else begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoADDI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
   end else begin
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoADDL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
   end;
  end;
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-22-15-52-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpSUB(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
var IntermediateResultTemporary:TPACCInt32;
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Kind=tkPOINTER) and
     (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Kind=tkPOINTER) and
     (assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType) or
      assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.ChildType)) then begin
   IntermediateResultTemporary:=CreateTemporary(pirctINT);
   EmitInstruction(pircoSUBI,[CreateTemporaryOperand(IntermediateResultTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
   OutputTemporary:=CreateTemporary(pirctINT);
   if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType) then begin
    EmitInstruction(pircoSDIVI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(IntermediateResultTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType^.Size)],Node.SourceLocation);
   end else begin
    EmitInstruction(pircoSDIVI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(IntermediateResultTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.ChildType^.Size)],Node.SourceLocation);
   end;
  end else begin
   OutputTemporary:=CreateTemporary(pirctINT);
   EmitInstruction(pircoSUBI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Kind=tkPOINTER) and
     (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Kind=tkPOINTER) and
     (assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType) or
      assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.ChildType)) then begin
   IntermediateResultTemporary:=CreateTemporary(pirctLONG);
   EmitInstruction(pircoSUBL,[CreateTemporaryOperand(IntermediateResultTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
   OutputTemporary:=CreateTemporary(pirctLONG);
   if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType) then begin
    EmitInstruction(pircoSDIVL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(IntermediateResultTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType^.Size)],Node.SourceLocation);
   end else begin
    EmitInstruction(pircoSDIVL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(IntermediateResultTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.ChildType^.Size)],Node.SourceLocation);
   end;
  end else begin
   OutputTemporary:=CreateTemporary(pirctLONG);
   EmitInstruction(pircoSUBL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoSUBF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoSUBD,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind=tkPOINTER then begin
  if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
   OutputTemporary:=CreateTemporary(pirctINT);
   EmitInstruction(pircoADDI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   OutputTemporary:=CreateTemporary(pirctLONG);
   EmitInstruction(pircoADDL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-00-58-0000',@Node.SourceLocation,true);
 end;
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Kind=tkPOINTER) and
     (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Kind=tkPOINTER) and
     (assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType) or
      assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.ChildType)) then begin
   IntermediateResultTemporary:=CreateTemporary(pirctINT);
   EmitInstruction(pircoSUBI,[CreateTemporaryOperand(IntermediateResultTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
   OutputTemporary:=CreateTemporary(pirctINT);
   if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType) then begin
    EmitInstruction(pircoSDIVI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(IntermediateResultTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType^.Size)],Node.SourceLocation);
   end else begin
    EmitInstruction(pircoSDIVI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(IntermediateResultTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.ChildType^.Size)],Node.SourceLocation);
   end;
  end else begin
   OutputTemporary:=CreateTemporary(pirctINT);
   EmitInstruction(pircoSUBI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Kind=tkPOINTER) and
     (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Kind=tkPOINTER) and
     (assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType) or
      assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.ChildType)) then begin
   IntermediateResultTemporary:=CreateTemporary(pirctLONG);
   EmitInstruction(pircoSUBL,[CreateTemporaryOperand(IntermediateResultTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
   OutputTemporary:=CreateTemporary(pirctLONG);
   if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType) then begin
    EmitInstruction(pircoSDIVL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(IntermediateResultTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType^.Size)],Node.SourceLocation);
   end else begin
    EmitInstruction(pircoSDIVL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(IntermediateResultTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.ChildType^.Size)],Node.SourceLocation);
   end;
  end else begin
   OutputTemporary:=CreateTemporary(pirctLONG);
   EmitInstruction(pircoSUBL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoSUBF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoSUBD,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind=tkPOINTER then begin
  if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
   OutputTemporary:=CreateTemporary(pirctINT);
   EmitInstruction(pircoADDI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   OutputTemporary:=CreateTemporary(pirctLONG);
   EmitInstruction(pircoADDL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-00-58-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpMUL(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoUMULI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoSMULI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoUMULL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoSMULL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoMULF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoMULD,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-01-03-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpDIV(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoUDIVI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoSDIVI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoUDIVL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoSDIVL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoDIVF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoDIVD,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-01-03-0002',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpMOD(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoUMODI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoSMODI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoUMODL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoSMODL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-01-21-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpAND(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoANDI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoANDL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-01-23-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpOR(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoORI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoORL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-01-23-0002',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpXOR(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoXORI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoXORL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-01-24-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpSHL(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoSHLI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoSHLL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-01-24-0002',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpSHR(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoSHRI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoSHRL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-01-24-0004',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpSAR(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoSARI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoSARL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-01-25-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpLE(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
    ((Node.Type_^.Kind=tkPOINTER) and
     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoCMPULEI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoCMPSLEI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoCMPULEL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoCMPSLEL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoCMPLEF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoCMPLED,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-12-05-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpLT(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
    ((Node.Type_^.Kind=tkPOINTER) and
     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoCMPULTI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoCMPSLTI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoCMPULTL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoCMPSLTL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoCMPLTF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoCMPLTD,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-12-05-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpGE(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
    ((Node.Type_^.Kind=tkPOINTER) and
     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoCMPUGEI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoCMPSGEI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoCMPUGEL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoCMPSGEL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoCMPGEF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoCMPGED,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-12-05-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpGT(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
    ((Node.Type_^.Kind=tkPOINTER) and
     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoCMPUGTI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoCMPSGTI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoCMPUGTL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoCMPSGTL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoCMPGTF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoCMPGTD,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-12-05-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpEQ(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
    ((Node.Type_^.Kind=tkPOINTER) and
     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoCMPEQI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoCMPEQL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoCMPEQF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoCMPEQD,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-12-05-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpNE(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
    ((Node.Type_^.Kind=tkPOINTER) and
     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
  OutputTemporary:=CreateTemporary(pirctINT);
 EmitInstruction(pircoCMPNEI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoCMPNEL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoCMPNEF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoCMPNED,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-12-05-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitNOT(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
var ValueTemporary:TPACCInt32;
begin
 ValueTemporary:=-1;
 EmitExpression(Node.Operand,ValueTemporary,[],pircvkRVALUE);
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoNOTI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(ValueTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoNOTL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(ValueTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-18-03-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitNEG(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
var ValueTemporary:TPACCInt32;
begin
 ValueTemporary:=-1;
 EmitExpression(Node.Operand,ValueTemporary,[],pircvkRVALUE);
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoNEGI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(ValueTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoNEGL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(ValueTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoNEGF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(ValueTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoNEGD,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(ValueTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-18-03-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitLoadUnaryOpStore(const LValueNode,Node:TPACCAbstractSyntaxTreeNode;const UnaryOpHook:TPACCIntermediateRepresentationCodeUnaryOpHook;var OutputTemporary:TPACCInt32;const PostOp:boolean);
var TemporaryA,TemporaryB,TemporaryC,LValueTemporary:TPACCInt32;
begin
 OutputTemporary:=-1;
 TemporaryA:=-1;
 TemporaryB:=-1;
 TemporaryC:=-1;
 LValueTemporary:=-1;
 if LValueNode.Kind in [astnkLVAR,astnkGVAR] then begin
  if PostOp then begin
   EmitExpression(LValueNode,TemporaryA,[],pircvkRVALUE);
   if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoMOVI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
   end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoMOVL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
   end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
    OutputTemporary:=CreateTemporary(pirctFLOAT);
    EmitInstruction(pircoMOVF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
   end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
    OutputTemporary:=CreateTemporary(pirctDOUBLE);
    EmitInstruction(pircoMOVD,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
   end else if Node.Type_^.Kind=tkPOINTER then begin
    if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
     OutputTemporary:=CreateTemporary(pirctINT);
     EmitInstruction(pircoMOVI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
    end else begin
     OutputTemporary:=CreateTemporary(pirctLONG);
     EmitInstruction(pircoMOVL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
    end;
   end;
   UnaryOpHook(TemporaryB,TemporaryA,Node);
   EmitStoreToVariable(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(LValueNode),TemporaryB);
  end else begin
   EmitExpression(LValueNode,TemporaryA,[],pircvkRVALUE);
   UnaryOpHook(OutputTemporary,TemporaryA,Node);
   EmitStoreToVariable(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(LValueNode),OutputTemporary);
  end;
 end else begin
  if PostOp then begin
   EmitExpression(LValueNode,LValueTemporary,[],pircvkLVALUE);
   EmitLoad(TemporaryA,LValueTemporary,LValueNode.Type_);
   if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoMOVI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
   end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoMOVL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
   end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
    OutputTemporary:=CreateTemporary(pirctFLOAT);
    EmitInstruction(pircoMOVF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
   end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
    OutputTemporary:=CreateTemporary(pirctDOUBLE);
    EmitInstruction(pircoMOVD,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
   end else if Node.Type_^.Kind=tkPOINTER then begin
    if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
     OutputTemporary:=CreateTemporary(pirctINT);
     EmitInstruction(pircoMOVI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
    end else begin
     OutputTemporary:=CreateTemporary(pirctLONG);
     EmitInstruction(pircoMOVL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
    end;
   end;
   UnaryOpHook(TemporaryB,TemporaryA,Node);
   EmitStore(LValueTemporary,TemporaryB,LValueNode.Type_);
  end else begin
   EmitExpression(LValueNode,LValueTemporary,[],pircvkLVALUE);
   EmitLoad(TemporaryA,LValueTemporary,LValueNode.Type_);
   UnaryOpHook(OutputTemporary,TemporaryA,Node);
   EmitStore(LValueTemporary,OutputTemporary,LValueNode.Type_);
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOp(const Node:TPACCAbstractSyntaxTreeNode;const BinaryOpHook:TPACCIntermediateRepresentationCodeBinaryOpHook;var OutputTemporary:TPACCInt32);
var TemporaryA,TemporaryB:TPACCInt32;
begin
 if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
    assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
  OutputTemporary:=-1;
  TemporaryA:=-1;
  TemporaryB:=-1;
  EmitExpression(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left,TemporaryA,[],pircvkRVALUE);
  EmitExpression(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right,TemporaryB,[],pircvkRVALUE);
  BinaryOpHook(OutputTemporary,TemporaryA,TemporaryB,Node);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-22-15-41-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitAssignmentBinaryOp(const Node:TPACCAbstractSyntaxTreeNode;const BinaryOpHook:TPACCIntermediateRepresentationCodeBinaryOpHook;var OutputTemporary:TPACCInt32);
var TemporaryA,TemporaryB,TemporaryC:TPACCInt32;
begin
 if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
    assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
  OutputTemporary:=-1;
  TemporaryA:=-1;
  TemporaryB:=-1;
  TemporaryC:=-1;
  EmitExpression(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left,TemporaryA,[],pircvkLVALUE);
  EmitLoad(TemporaryB,TemporaryA,TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_);
  EmitExpression(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right,TemporaryC,[],pircvkRVALUE);
  BinaryOpHook(OutputTemporary,TemporaryB,TemporaryC,Node);
  EmitStore(TemporaryA,OutputTemporary,TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-14-04-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitAssign(const Node:TPACCAbstractSyntaxTreeNodeBinaryOperator;var OutputTemporary:TPACCInt32);
var AssignLValueTemporary:TPACCInt32;
begin
 OutputTemporary:=-1;
 if Node.Left.Kind in [astnkLVAR,astnkGVAR] then begin
  EmitExpression(Node.Right,OutputTemporary,[],pircvkRVALUE);
  EmitStoreToVariable(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node.Left),OutputTemporary);
 end else begin
  AssignLValueTemporary:=-1;
  EmitExpression(Node.Left,AssignLValueTemporary,[],pircvkLVALUE);
  EmitExpression(Node.Right,OutputTemporary,[],pircvkRVALUE);
  EmitStore(AssignLValueTemporary,OutputTemporary,Node.Left.Type_);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitAssignOp(const Node:TPACCAbstractSyntaxTreeNodeBinaryOperator;var OutputTemporary:TPACCInt32);
var LastAssignOpLValueTemporary:TPACCInt32;
begin
 LastAssignOpLValueTemporary:=AssignOpLValueTemporary;
 try
  AssignOpLValueTemporary:=-1;
  OutputTemporary:=-1;
  EmitExpression(Node.Left,AssignOpLValueTemporary,[],pircvkLVALUE);
  EmitExpression(Node.Right,OutputTemporary,[],pircvkRVALUE);
  EmitStore(AssignOpLValueTemporary,OutputTemporary,Node.Left.Type_);
 finally
  AssignOpLValueTemporary:=LastAssignOpLValueTemporary;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitAssignSrc(const Node:TPACCAbstractSyntaxTreeNode;var OutputTemporary:TPACCInt32;const ValueKind:TPACCIntermediateRepresentationCodeValueKind);
begin
 if AssignOpLValueTemporary<0 then begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-15-41-0000',@Node.SourceLocation,true);
 end else begin
  case ValueKind of
   pircvkLVALUE:begin
    OutputTemporary:=AssignOpLValueTemporary;
   end;
   pircvkRVALUE:begin
    EmitLoad(OutputTemporary,AssignOpLValueTemporary,Node.Type_);
   end;
   else begin
    TPACCInstance(fInstance).AddError('Internal error 2017-01-23-15-40-0000',@Node.SourceLocation,true);
   end;
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitIntegerValue(const Node:TPACCAbstractSyntaxTreeNodeIntegerValue;var OutputTemporary:TPACCInt32);
begin
 if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
    ((Node.Type_^.Kind=tkPOINTER) and
     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoSETI,[CreateTemporaryOperand(OutputTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeIntegerValue(Node).Value)],Node.SourceLocation);
 end else if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoSETL,[CreateTemporaryOperand(OutputTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeIntegerValue(Node).Value)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-22-16-01-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitFloatValue(const Node:TPACCAbstractSyntaxTreeNodeFloatValue;var OutputTemporary:TPACCInt32);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoSETF,[CreateTemporaryOperand(OutputTemporary),CreateFloatValueOperand(TPACCAbstractSyntaxTreeNodeFloatValue(Node).Value)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoSETD,[CreateTemporaryOperand(OutputTemporary),CreateFloatValueOperand(TPACCAbstractSyntaxTreeNodeFloatValue(Node).Value)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-22-16-04-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitStoreToVariable(const Node:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;const InputTemporary:TPACCInt32);
begin
 case Node.Type_^.Kind of
  tkBOOL,tkCHAR:begin
   EmitInstruction(pircoSTIC,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateTemporaryOperand(InputTemporary)],Node.SourceLocation);
  end;
  tkSHORT:begin
   EmitInstruction(pircoSTIS,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateTemporaryOperand(InputTemporary)],Node.SourceLocation);
  end;
  tkINT,tkENUM:begin
   EmitInstruction(pircoSTII,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateTemporaryOperand(InputTemporary)],Node.SourceLocation);
  end;
  tkLONG,tkLLONG:begin
   EmitInstruction(pircoSTLL,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateTemporaryOperand(InputTemporary)],Node.SourceLocation);
  end;
  tkFLOAT:begin
   EmitInstruction(pircoSTF,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateTemporaryOperand(InputTemporary)],Node.SourceLocation);
  end;
  tkDOUBLE,tkLDOUBLE:begin
   EmitInstruction(pircoSTD,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateTemporaryOperand(InputTemporary)],Node.SourceLocation);
  end;
  tkPOINTER:begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    EmitInstruction(pircoSTII,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateTemporaryOperand(InputTemporary)],Node.SourceLocation);
   end else begin
    EmitInstruction(pircoSTLL,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateTemporaryOperand(InputTemporary)],Node.SourceLocation);
   end;
  end;
  else begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-22-16-36-0000',@Node.SourceLocation,true);
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitVariable(const Node:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;var OutputTemporary:TPACCInt32;const InputTemporaries:array of TPACCInt32;const ValueKind:TPACCIntermediateRepresentationCodeValueKind);
begin
 case ValueKind of
  pircvkLVALUE:begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoADDROFI,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
   end else begin
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoADDROFL,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
   end;
  end;
  pircvkRVALUE:begin
   case Node.Type_^.Kind of
    tkBOOL,tkCHAR:begin
     OutputTemporary:=CreateTemporary(pirctINT);
     if tfUnsigned in Node.Type_^.Flags then begin
      EmitInstruction(pircoLDUCI,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
     end else begin
      EmitInstruction(pircoLDSCI,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
     end;
    end;
    tkSHORT:begin
     OutputTemporary:=CreateTemporary(pirctINT);
     if tfUnsigned in Node.Type_^.Flags then begin
      EmitInstruction(pircoLDUSI,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
     end else begin
      EmitInstruction(pircoLDSSI,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
     end;
    end;
    tkINT,tkENUM:begin
     OutputTemporary:=CreateTemporary(pirctINT);
     if tfUnsigned in Node.Type_^.Flags then begin
      EmitInstruction(pircoLDUII,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
     end else begin
      EmitInstruction(pircoLDSII,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
     end;
    end;
    tkLONG,tkLLONG:begin
     OutputTemporary:=CreateTemporary(pirctLONG);
     if tfUnsigned in Node.Type_^.Flags then begin
      EmitInstruction(pircoLDULL,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
     end else begin
      EmitInstruction(pircoLDSLL,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
     end;
    end;
    tkFLOAT:begin
     OutputTemporary:=CreateTemporary(pirctFLOAT);
     EmitInstruction(pircoLDF,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
    end;
    tkDOUBLE,tkLDOUBLE:begin
     OutputTemporary:=CreateTemporary(pirctDOUBLE);
     EmitInstruction(pircoLDD,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
    end;
    tkPOINTER:begin
     if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
      OutputTemporary:=CreateTemporary(pirctINT);
      if tfUnsigned in Node.Type_^.Flags then begin
       EmitInstruction(pircoLDUII,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
      end else begin
       EmitInstruction(pircoLDSII,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
      end;
     end else begin
      OutputTemporary:=CreateTemporary(pirctLONG);
      if tfUnsigned in Node.Type_^.Flags then begin
       EmitInstruction(pircoLDULL,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
      end else begin
       EmitInstruction(pircoLDSLL,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
      end;
     end;
    end;
    else begin
     TPACCInstance(fInstance).AddError('Internal error 2017-01-22-16-15-0000',@Node.SourceLocation,true);
    end;
   end;
  end;
  else begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-22-16-36-0001',@Node.SourceLocation,true);
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitComputedJump(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator);
var ComputedTargetTemporary:TPACCInt32;
begin
 ComputedTargetTemporary:=-1;
 EmitExpression(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,ComputedTargetTemporary,[],pircvkRVALUE);
 CurrentBlock.Jump.Kind:=pircjkJMPA;
 CurrentBlock.Jump.Operand:=CreateTemporaryOperand(ComputedTargetTemporary);
 CloseBlock;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitLogicalANDOR(const Node:TPACCAbstractSyntaxTreeNodeBinaryOperator;var OutputTemporary:TPACCInt32;const IsAND:boolean);
var l0,l1,l2,l3:TPACCAbstractSyntaxTreeNodeLabel;
    b0,b1,b2,b3:TPACCIntermediateRepresentationCodeBlock;
    LeftTemporary,RightTemporary:TPACCInt32;
begin

 l0:=NewHiddenLabel;
 l1:=NewHiddenLabel;
 l2:=NewHiddenLabel;
 l3:=NewHiddenLabel;
 b0:=FindBlock(l0);
 b1:=FindBlock(l1);
 b2:=FindBlock(l2);
 b3:=FindBlock(l3);

 LeftTemporary:=-1;
 EmitExpression(Node.Left,LeftTemporary,[],pircvkRVALUE);
 if (Node.Left.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
    ((Node.Left.Type_^.Kind=tkPOINTER) and
     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
  CurrentBlock.Jump.Kind:=pircjkJNZI;
 end else if (Node.Left.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Left.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  CurrentBlock.Jump.Kind:=pircjkJNZL;
 end else if Node.Left.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  CurrentBlock.Jump.Kind:=pircjkJNZF;
 end else if Node.Left.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  CurrentBlock.Jump.Kind:=pircjkJNZD;
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-10-28-0000',@Node.SourceLocation,true);
 end;
 CurrentBlock.Jump.Operand:=CreateTemporaryOperand(LeftTemporary);
 if IsAND then begin
  CurrentBlock.Successors[0]:=b0;
  CurrentBlock.Successors[1]:=b2;
 end else begin
  CurrentBlock.Successors[0]:=b1;
  CurrentBlock.Successors[1]:=b0;
 end;
 CloseBlock;

 EmitLabel(l0);
 RightTemporary:=-1;
 EmitExpression(Node.Right,RightTemporary,[],pircvkRVALUE);
 if (Node.Right.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
    ((Node.Right.Type_^.Kind=tkPOINTER) and
     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
  CurrentBlock.Jump.Kind:=pircjkJNZI;
 end else if (Node.Right.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Right.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  CurrentBlock.Jump.Kind:=pircjkJNZL;
 end else if Node.Right.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  CurrentBlock.Jump.Kind:=pircjkJNZF;
 end else if Node.Right.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  CurrentBlock.Jump.Kind:=pircjkJNZD;
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-10-29-0000',@Node.SourceLocation,true);
 end;
 CurrentBlock.Jump.Operand:=CreateTemporaryOperand(RightTemporary);
 CurrentBlock.Successors[0]:=b1;
 CurrentBlock.Successors[1]:=b2;
 CloseBlock;

 EmitLabel(l1);
 EmitJump(l3);

 EmitLabel(l2);
 EmitJump(l3);

 EmitLabel(l3);
 OutputTemporary:=CreateTemporary(pirctINT);
 EmitPhi(pirctINT,
         CreateTemporaryOperand(OutputTemporary),
         [CreateIntegerValueOperand(1),CreateIntegerValueOperand(0)],
         [b0,b1],
         Node.SourceLocation);

end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitLogicalNOT(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
var l0,l1,l2:TPACCAbstractSyntaxTreeNodeLabel;
    b0,b1,b2:TPACCIntermediateRepresentationCodeBlock;
    OperandTemporary:TPACCInt32;
begin

 l0:=NewHiddenLabel;
 l1:=NewHiddenLabel;
 l2:=NewHiddenLabel;
 b0:=FindBlock(l0);
 b1:=FindBlock(l1);
 b2:=FindBlock(l2);

 OperandTemporary:=-1;
 EmitExpression(Node.Operand,OperandTemporary,[],pircvkRVALUE);
 if (Node.Operand.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
    ((Node.Operand.Type_^.Kind=tkPOINTER) and
     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
  CurrentBlock.Jump.Kind:=pircjkJNZI;
 end else if (Node.Operand.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Operand.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  CurrentBlock.Jump.Kind:=pircjkJNZL;
 end else if Node.Operand.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  CurrentBlock.Jump.Kind:=pircjkJNZF;
 end else if Node.Operand.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  CurrentBlock.Jump.Kind:=pircjkJNZD;
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-10-48-0000',@Node.SourceLocation,true);
 end;
 CurrentBlock.Jump.Operand:=CreateTemporaryOperand(OperandTemporary);
 CurrentBlock.Successors[0]:=b0;
 CurrentBlock.Successors[1]:=b1;
 CloseBlock;

 EmitLabel(l0);
 EmitJump(l2);

 EmitLabel(l1);
 EmitJump(l2);

 EmitLabel(l2);
 OutputTemporary:=CreateTemporary(pirctINT);
 EmitPhi(pirctINT,
         CreateTemporaryOperand(OutputTemporary),
         [CreateIntegerValueOperand(1),CreateIntegerValueOperand(0)],
         [b0,b1],
         Node.SourceLocation);

end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitCONV(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32;const ValueKind:TPACCIntermediateRepresentationCodeValueKind);
var TemporaryA,TemporaryB:TPACCInt32;
begin
 if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) and
    assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_) and
    assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_) then begin
  if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_=TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_) or
     ((TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Kind=TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind) and
      ((TPACCInstance(fInstance).IsIntType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_) and TPACCInstance(fInstance).IsIntType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_)) or
       (TPACCInstance(fInstance).IsFloatType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_) and TPACCInstance(fInstance).IsFloatType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_)) or
       ((TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Kind=tkPOINTER) and (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind=tkPOINTER)))) then begin
   EmitExpression(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,OutputTemporary,[],ValueKind);
  end else begin
   TemporaryA:=-1;
   EmitExpression(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA,[],ValueKind);
   if TemporaryA<0 then begin
    TPACCInstance(fInstance).AddError('Internal error 2017-01-22-15-21-0000',@Node.SourceLocation,true);
   end else begin
    if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
       ((TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Kind=tkPOINTER) and
        (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
     case TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind of
      tkBOOL:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZECI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSECI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkCHAR:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZECI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSECI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkSHORT:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZESI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSESI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkINT,tkENUM:begin
       OutputTemporary:=TemporaryA;
      end;
      tkLONG:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       EmitInstruction(pircoTRLI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkLLONG:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       EmitInstruction(pircoTRLI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkFLOAT:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       EmitInstruction(pircoTFTI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkDOUBLE:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       EmitInstruction(pircoTDTI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkLDOUBLE:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       EmitInstruction(pircoTDTI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkPOINTER:begin
       if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong then begin
        OutputTemporary:=CreateTemporary(pirctINT);
        EmitInstruction(pircoTRLI,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        OutputTemporary:=TemporaryA;
       end;
      end;
      else begin
       TPACCInstance(fInstance).AddError('Internal error 2017-01-22-15-12-0000',@Node.SourceLocation,true);
      end;
     end;
    end else if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Kind in [tkLONG,tkLONG]) or
                ((TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Kind=tkPOINTER) and
                 (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
     case TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind of
      tkBOOL:begin
       OutputTemporary:=CreateTemporary(pirctLONG);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZECL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSECL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkCHAR:begin
       OutputTemporary:=CreateTemporary(pirctLONG);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZECL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSECL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkSHORT:begin
       OutputTemporary:=CreateTemporary(pirctLONG);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZESL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSESL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkINT,tkENUM:begin
       OutputTemporary:=CreateTemporary(pirctLONG);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZEIL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSEIL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkLONG:begin
       OutputTemporary:=TemporaryA;
      end;
      tkLLONG:begin
       OutputTemporary:=TemporaryA;
      end;
      tkFLOAT:begin
       OutputTemporary:=CreateTemporary(pirctLONG);
       EmitInstruction(pircoTFTL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkDOUBLE:begin
       OutputTemporary:=CreateTemporary(pirctLONG);
       EmitInstruction(pircoTDTL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkLDOUBLE:begin
       OutputTemporary:=CreateTemporary(pirctLONG);
       EmitInstruction(pircoTDTL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkPOINTER:begin
       if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong then begin
        OutputTemporary:=TemporaryA;
       end else begin
        OutputTemporary:=CreateTemporary(pirctLONG);
        if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
           (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
         EmitInstruction(pircoZEIL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
        end else begin
         EmitInstruction(pircoSEIL,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
        end;
       end;
      end;
      else begin
       TPACCInstance(fInstance).AddError('Internal error 2017-01-22-15-12-0000',@Node.SourceLocation,true);
      end;
     end;
    end else begin
     case TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Kind of
      tkFLOAT:begin
       if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
           ((TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind=tkPOINTER) and
            (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
        if tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags then begin
         TemporaryB:=CreateTemporary(pirctLONG);
         EmitInstruction(pircoZEIL,[CreateTemporaryOperand(TemporaryB),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
         OutputTemporary:=CreateTemporary(pirctFLOAT);
         EmitInstruction(pircoCLTF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryB)],Node.SourceLocation);
        end else begin
         OutputTemporary:=CreateTemporary(pirctFLOAT);
         EmitInstruction(pircoCITF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
        end;
       end else if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
                   ((TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind=tkPOINTER) and
                     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
        OutputTemporary:=CreateTemporary(pirctFLOAT);
        EmitInstruction(pircoCLTF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else if TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
        OutputTemporary:=CreateTemporary(pirctFLOAT);
        EmitInstruction(pircoCDTF,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        TPACCInstance(fInstance).AddError('Internal error 2017-01-22-15-32-0000',@Node.SourceLocation,true);
       end;
      end;
      tkDOUBLE,tkLDOUBLE:begin
       if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
           ((TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind=tkPOINTER) and
            (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
        if tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags then begin
         TemporaryB:=CreateTemporary(pirctLONG);
         EmitInstruction(pircoZEIL,[CreateTemporaryOperand(TemporaryB),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
         OutputTemporary:=CreateTemporary(pirctDOUBLE);
         EmitInstruction(pircoCLTD,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryB)],Node.SourceLocation);
        end else begin
         OutputTemporary:=CreateTemporary(pirctDOUBLE);
         EmitInstruction(pircoCITD,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
        end;
       end else if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
                   ((TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind=tkPOINTER) and
                     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
        OutputTemporary:=CreateTemporary(pirctDOUBLE);
        EmitInstruction(pircoCLTD,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else if TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
        OutputTemporary:=CreateTemporary(pirctDOUBLE);
        EmitInstruction(pircoCFTD,[CreateTemporaryOperand(OutputTemporary),CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        TPACCInstance(fInstance).AddError('Internal error 2017-01-22-15-32-0000',@Node.SourceLocation,true);
       end;
      end;
      else begin
       TPACCInstance(fInstance).AddError('Internal error 2017-01-22-15-19-0000',@Node.SourceLocation,true);
      end;
     end;
    end;
   end;
  end;
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-22-14-53-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitADDR(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32;const ValueKind:TPACCIntermediateRepresentationCodeValueKind);
begin
 if ValueKind=pircvkRVALUE then begin
  if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
   case TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Kind of
    astnkLVAR,astnkGVAR:begin 
     if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
      OutputTemporary:=CreateTemporary(pirctINT);
      EmitInstruction(pircoADDROFI,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand))],TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.SourceLocation);
     end else begin
      OutputTemporary:=CreateTemporary(pirctLONG);
      EmitInstruction(pircoADDROFL,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand))],TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.SourceLocation);
     end;
    end;
    astnkSTRUCT_REF:begin
     EmitExpression(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,OutputTemporary,[],pircvkLVALUE);
    end;
    astnkDEREF:begin
     EmitExpression(TPACCAbstractSyntaxTreeNodeUnaryOperator(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand).Operand,OutputTemporary,[],pircvkLVALUE);
    end;
    astnkFUNCDESG:begin
     if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
      OutputTemporary:=CreateTemporary(pirctINT);
      EmitInstruction(pircoADDROFI,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Variable).Operand))],TPACCAbstractSyntaxTreeNodeUnaryOperator(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Variable).Operand.SourceLocation);
     end else begin
      OutputTemporary:=CreateTemporary(pirctLONG);
      EmitInstruction(pircoADDROFL,[CreateTemporaryOperand(OutputTemporary),CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Variable).Operand))],TPACCAbstractSyntaxTreeNodeUnaryOperator(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Variable).Operand.SourceLocation);
     end;
    end;
    else begin
     TPACCInstance(fInstance).AddError('Internal error 2017-01-22-17-17-0000',nil,true);
    end;
   end;
  end else begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-22-09-33-0002',nil,true);
  end;
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-16-13-0000',nil,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitDEREF(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32;const ValueKind:TPACCIntermediateRepresentationCodeValueKind);
var LValueTemporary:TPACCInt32;
begin
 if ValueKind=pircvkLVALUE then begin
  LValueTemporary:=-1;
  OutputTemporary:=-1;
  EmitExpression(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,LValueTemporary,[],pircvkLVALUE);
  EmitLoad(OutputTemporary,LValueTemporary,Node.Type_);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-16-15-0000',nil,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitRETURN(const Node:TPACCAbstractSyntaxTreeNodeRETURNStatement);
var TemporaryA:TPACCInt32;
begin
 if assigned(FunctionDeclaration.Type_) and
    assigned(FunctionDeclaration.Type_^.ReturnType) then begin
  TemporaryA:=-1;
  if assigned(TPACCAbstractSyntaxTreeNodeRETURNStatement(Node).ReturnValue) then begin
   EmitExpression(TPACCAbstractSyntaxTreeNodeRETURNStatement(Node).ReturnValue,TemporaryA,[],pircvkRVALUE);
   if TemporaryA<0 then begin
    TPACCInstance(fInstance).AddError('Internal error 2017-01-22-14-51-0000',@Node.SourceLocation,true);
   end;
  end else begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-22-14-48-0000',@Node.SourceLocation,true);
  end;
  if CurrentBlock.Jump.Kind=pircjkNONE then begin
   case FunctionDeclaration.Type_^.ReturnType^.Kind of
    tkBOOL,tkCHAR,tkSHORT,tkINT,tkENUM:begin
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
     if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong then begin
      CurrentBlock.Jump.Kind:=pircjkRETL;
     end else begin
      CurrentBlock.Jump.Kind:=pircjkRETI;
     end;
    end;
    else begin
     TPACCInstance(fInstance).AddError('Internal error 2017-01-22-14-44-0000',@Node.SourceLocation,true);
    end;
   end;
   if TemporaryA>=0 then begin
    CurrentBlock.Jump.Operand.Kind:=pircokTEMPORARY;
    CurrentBlock.Jump.Operand.Temporary:=TemporaryA;
   end else begin
    case FunctionDeclaration.Type_^.ReturnType^.Kind of
     tkBOOL,tkCHAR,tkSHORT,tkINT,tkENUM,tkLONG,tkLLONG,tkPOINTER:begin
      CurrentBlock.Jump.Operand.Kind:=pircokINTEGER;
      CurrentBlock.Jump.Operand.IntegerValue:=0;
     end;
     tkFLOAT,tkDOUBLE,tkLDOUBLE:begin
      CurrentBlock.Jump.Operand.Kind:=pircokFLOAT;
      CurrentBlock.Jump.Operand.FloatValue:=0;
     end;
     else begin
      TPACCInstance(fInstance).AddError('Internal error 2017-01-22-14-50-0000',@Node.SourceLocation,true);
     end;
    end;
   end;
   CloseBlock;
  end else begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-21-13-54-0000',@Node.SourceLocation,true);
  end;
 end else begin
  if CurrentBlock.Jump.Kind=pircjkNONE then begin
   CurrentBlock.Jump.Kind:=pircjkRET;
   CloseBlock;
  end else begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-21-13-54-0000',@Node.SourceLocation,true);
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitCOMMA(const Node:TPACCAbstractSyntaxTreeNodeBinaryOperator;var OutputTemporary:TPACCInt32;const ValueKind:TPACCIntermediateRepresentationCodeValueKind);
var TemporaryA:TPACCInt32;
begin
 if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
    assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
  TemporaryA:=-1;
  EmitExpression(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left,TemporaryA,[],ValueKind);
  EmitExpression(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right,OutputTemporary,[],ValueKind);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-21-15-08-0000',nil,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitExpression(const Node:TPACCAbstractSyntaxTreeNode;var OutputTemporary:TPACCInt32;const InputTemporaries:array of TPACCInt32;const ValueKind:TPACCIntermediateRepresentationCodeValueKind);
 procedure EnsureRValue;
 begin
  if ValueKind<>pircvkRVALUE then begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-24-09-41-0000',nil,true);
  end;
 end;
begin
 if assigned(Node) then begin

//writeln(TypInfo.GetEnumName(TypeInfo(TPACCAbstractSyntaxTreeNodeKind),TPACCInt32(Node.Kind)));

  case Node.Kind of

   astnkINTEGER:begin
    EmitIntegerValue(TPACCAbstractSyntaxTreeNodeIntegerValue(Node),OutputTemporary);
   end;

   astnkFLOAT:begin
    EmitFloatValue(TPACCAbstractSyntaxTreeNodeFloatValue(Node),OutputTemporary);
   end;

   astnkSTRING:begin
    TPACCInstance(fInstance).AddError('Internal error 2017-01-21-14-56-0000',nil,true);
   end;

   astnkLVAR,
   astnkGVAR:begin
    EmitVariable(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node),OutputTemporary,InputTemporaries,ValueKind);
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
    EmitCONV(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node),OutputTemporary,ValueKind);
   end;

   astnkADDR:begin
    EmitADDR(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node),OutputTemporary,ValueKind);
   end;

   astnkDEREF:begin
    EmitDEREF(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node),OutputTemporary,ValueKind);
   end;

   astnkTERNARY:begin
   end;

   astnkSTRUCT_REF:begin
   end;

   astnkOP_COMMA:begin
    EmitCOMMA(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node),OutputTemporary,ValueKind);
   end;

   astnkOP_ASSIGN:begin
    EmitAssign(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node),OutputTemporary);
   end;

   astnkOP_ASSIGN_OP:begin
    EmitAssignOp(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node),OutputTemporary);
   end;

   astnkOP_ASSIGN_SRC:begin
    EmitAssignSrc(Node,OutputTemporary,ValueKind);
   end;

   astnkOP_CAST:begin
   end;

   astnkOP_NOT:begin
    EnsureRValue;
    EmitNOT(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node),OutputTemporary);
   end;

   astnkOP_NEG:begin
    EnsureRValue;
    EmitNEG(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node),OutputTemporary);
   end;

   astnkOP_PRE_INC:begin
    EnsureRValue;
    if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     EmitLoadUnaryOpStore(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,Node,EmitUnaryOpINC,OutputTemporary,false);
    end else begin
     TPACCInstance(fInstance).AddError('Internal error 2017-01-21-15-01-0002',nil,true);
    end;
   end;

   astnkOP_PRE_DEC:begin
    EnsureRValue;
    if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     EmitLoadUnaryOpStore(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,Node,EmitUnaryOpDEC,OutputTemporary,false);
    end else begin
     TPACCInstance(fInstance).AddError('Internal error 2017-01-23-02-21-0000',nil,true);
    end;
   end;

   astnkOP_POST_INC:begin
    EnsureRValue;
    if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     EmitLoadUnaryOpStore(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,Node,EmitUnaryOpINC,OutputTemporary,true);
    end else begin
     TPACCInstance(fInstance).AddError('Internal error 2017-01-23-02-22-0000',nil,true);
    end;
   end;

   astnkOP_POST_DEC:begin
    EnsureRValue;
    if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     EmitLoadUnaryOpStore(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,Node,EmitUnaryOpDEC,OutputTemporary,true);
    end else begin
     TPACCInstance(fInstance).AddError('Internal error 2017-01-23-02-22-0001',nil,true);
    end;
   end;

   astnkOP_LABEL_ADDR:begin
    if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
     OutputTemporary:=CreateTemporary(pirctINT);
     EmitInstruction(pircoADDROFI,[CreateTemporaryOperand(OutputTemporary),CreateLabelOperand(TPACCAbstractSyntaxTreeNodeLabel(TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).Label_))],Node.SourceLocation);
    end else begin
     OutputTemporary:=CreateTemporary(pirctLONG);
     EmitInstruction(pircoADDROFL,[CreateTemporaryOperand(OutputTemporary),CreateLabelOperand(TPACCAbstractSyntaxTreeNodeLabel(TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).Label_))],Node.SourceLocation);
    end;
   end;

   astnkRETURN:begin
    EmitRETURN(TPACCAbstractSyntaxTreeNodeRETURNStatement(Node));
   end;

   astnkGOTO:begin
    EmitJump(TPACCAbstractSyntaxTreeNodeLabel(TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).Label_));
   end;

   astnkCOMPUTED_GOTO:begin
    EmitComputedJump(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node));
   end;

   astnkLABEL:begin
    EmitLabel(TPACCAbstractSyntaxTreeNodeLabel(Node));
   end;

   astnkHIDDEN_LABEL:begin
    EmitLabel(TPACCAbstractSyntaxTreeNodeLabel(Node));
   end;

   astnkOP_ADD:begin
    EnsureRValue;
    EmitBinaryOp(Node,EmitBinaryOpADD,OutputTemporary);
   end;

   astnkOP_SUB:begin
    EnsureRValue;
    EmitBinaryOp(Node,EmitBinaryOpSUB,OutputTemporary);
   end;

   astnkOP_MUL:begin
    EnsureRValue;
    EmitBinaryOp(Node,EmitBinaryOpMUL,OutputTemporary);
   end;

   astnkOP_DIV:begin
    EnsureRValue;
    EmitBinaryOp(Node,EmitBinaryOpDIV,OutputTemporary);
   end;

   astnkOP_MOD:begin
    EnsureRValue;
    EmitBinaryOp(Node,EmitBinaryOpMOD,OutputTemporary);
   end;

   astnkOP_AND:begin
    EnsureRValue;
    EmitBinaryOp(Node,EmitBinaryOpAND,OutputTemporary);
   end;

   astnkOP_OR:begin
    EnsureRValue;
    EmitBinaryOp(Node,EmitBinaryOpOR,OutputTemporary);
   end;

   astnkOP_XOR:begin
    EnsureRValue;
    EmitBinaryOp(Node,EmitBinaryOpXOR,OutputTemporary);
   end;

   astnkOP_SHL:begin
    EnsureRValue;
    EmitBinaryOp(Node,EmitBinaryOpSHL,OutputTemporary);
   end;

   astnkOP_SHR:begin
    EnsureRValue;
    EmitBinaryOp(Node,EmitBinaryOpSHR,OutputTemporary);
   end;

   astnkOP_SAR:begin
    EnsureRValue;
    EmitBinaryOp(Node,EmitBinaryOpSAR,OutputTemporary);
   end;

   astnkOP_LOG_AND:begin
    EnsureRValue;
    EmitLogicalANDOR(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node),OutputTemporary,true);
   end;

   astnkOP_LOG_OR:begin
    EnsureRValue;
    EmitLogicalANDOR(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node),OutputTemporary,false);
   end;

   astnkOP_LOG_NOT:begin
    EnsureRValue;
    EmitLogicalNOT(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node),OutputTemporary);
   end;

   astnkOP_EQ:begin
    EnsureRValue;
    EmitBinaryOp(Node,EmitBinaryOpEQ,OutputTemporary);
   end;

   astnkOP_NE:begin
    EnsureRValue;
    EmitBinaryOp(Node,EmitBinaryOpNE,OutputTemporary);
   end;

   astnkOP_GT:begin
    EnsureRValue;
    EmitBinaryOp(Node,EmitBinaryOpGT,OutputTemporary);
   end;

   astnkOP_LT:begin
    EnsureRValue;
    EmitBinaryOp(Node,EmitBinaryOpLT,OutputTemporary);
   end;

   astnkOP_GE:begin
    EnsureRValue;
    EmitBinaryOp(Node,EmitBinaryOpGE,OutputTemporary);
   end;

   astnkOP_LE:begin
    EnsureRValue;
    EmitBinaryOp(Node,EmitBinaryOpLE,OutputTemporary);
   end;

  end;

 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitStatement(const Node:TPACCAbstractSyntaxTreeNode);
var Index,TemporaryA:TPACCInt32;
begin
 if assigned(Node) then begin
  case Node.Kind of
   astnkSTATEMENTS:begin
    EmitStatements(TPACCAbstractSyntaxTreeNodeStatements(Node));
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
   astnkBREAK:begin
    EmitJump(TPACCAbstractSyntaxTreeNodeLabel(TPACCAbstractSyntaxTreeNodeBREAKOrCONTINUEStatement(Node).Label_));
   end;
   astnkCONTINUE:begin
    EmitJump(TPACCAbstractSyntaxTreeNodeLabel(TPACCAbstractSyntaxTreeNodeBREAKOrCONTINUEStatement(Node).Label_));
   end;
   else begin
    EmitExpression(Node,TemporaryA,[],pircvkRVALUE);
   end;
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitStatements(const Node:TPACCAbstractSyntaxTreeNodeStatements);
var Index:TPACCInt32;
begin
 for Index:=0 to TPACCAbstractSyntaxTreeNodeStatements(Node).Children.Count-1 do begin
  EmitStatement(TPACCAbstractSyntaxTreeNodeStatements(Node).Children[Index]);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitFunction(const AFunctionNode:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration);
begin

 FunctionDeclaration:=AFunctionNode;

 FunctionName:=AFunctionNode.FunctionName;

 TPACCInstance(fInstance).IntermediateRepresentationCode.Functions.Add(self);

 CurrentBlock:=nil;
 BlockLink:=@StartBlock;
 PhiLink:=nil;

 NeedNewBlock:=true;

 AssignOpLValueTemporary:=-1;

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

 EmitStatements(TPACCAbstractSyntaxTreeNodeStatements(AFunctionNode.Body));

 if CurrentBlock.Jump.Kind=pircjkNONE then begin
  CurrentBlock.Jump.Kind:=pircjkRET;
 end;

end;

procedure GenerateIntermediateRepresentationCode(const AInstance:TObject;const ARootAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNode);
var Index:TPACCInt32;
    RootAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNodeTranslationUnit;
    Node:TPACCAbstractSyntaxTreeNode;
    Function_:TPACCIntermediateRepresentationCodeFunction;
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
      Function_:=TPACCIntermediateRepresentationCodeFunction.Create(AInstance);
      Function_.EmitFunction(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node));
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
