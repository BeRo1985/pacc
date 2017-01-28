unit PACCIntermediateRepresentationCode;
{$i PACC.inc}

interface

uses TypInfo,SysUtils,Classes,Math,PUCU,PasMP,PACCTypes,PACCGlobals,PACCPointerHashMap,PACCAbstractSyntaxTree;

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

       pircoMEMCPYI, // Memory copy int
       pircoMEMCPYL, // Memory copy long

       pircoZEROMEMI, // Zero memory int
       pircoZEROMEML, // Zero memory long

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

       pircoALLOC, // Allocate with size and alignment as operands

       pircoLVAR, // Local variable with size and alignment as operands

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
       pircjkJMPT, // Jump table
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
       pirctTOP,
       pirctINT,
       pirctLONG,
       pirctFLOAT,
       pirctDOUBLE
      );

     PPACCIntermediateRepresentationCodeUseKind=^TPACCIntermediateRepresentationCodeUseKind;
     TPACCIntermediateRepresentationCodeUseKind=
      (
       pircukNONE,
       pircukPHI,
       pircukINS,
       pircukJMP
      );

     PPPACCIntermediateRepresentationCodePhi=^PPACCIntermediateRepresentationCodePhi;
     PPACCIntermediateRepresentationCodePhi=^TPACCIntermediateRepresentationCodePhi;
     TPACCIntermediateRepresentationCodePhi=class;

     TPACCIntermediateRepresentationCodeInstruction=class;

     PPACCIntermediateRepresentationCodeUseBy=^TPACCIntermediateRepresentationCodeUseBy;
     TPACCIntermediateRepresentationCodeUseBy=record
      case TPACCIntermediateRepresentationCodeUseKind of
       pircukPHI:(
        Phi:TPACCIntermediateRepresentationCodePhi;
       );
       pircukINS:(
        Instruction:TPACCIntermediateRepresentationCodeInstruction;
       );
       pircukJMP:(
       );
     end;

     PPACCIntermediateRepresentationCodeUse=^TPACCIntermediateRepresentationCodeUse;
     TPACCIntermediateRepresentationCodeUse=class
      public
       Kind:TPACCIntermediateRepresentationCodeUseKind;
       BlockID:TPACCInt32;
       By:TPACCIntermediateRepresentationCodeUseBy;
     end;

     TPACCIntermediateRepresentationCodeUseList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCIntermediateRepresentationCodeUse;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCIntermediateRepresentationCodeUse);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCIntermediateRepresentationCodeUse read GetItem write SetItem; default;
     end;

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
       pircokCALL,
       pircokINTEGER,
       pircokFLOAT,
       pircokVARIABLE,
       pircokLABEL,
       pircokFUNCTION,
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
       pircokFUNCTION:(
        Function_:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration;
       );
       pircokCOUNT:(
       );
     end;

     TPACCIntermediateRepresentationCodeOperands=array of TPACCIntermediateRepresentationCodeOperand;

     PPACCIntermediateRepresentationCodeAliasKind=^TPACCIntermediateRepresentationCodeAliasKind;
     TPACCIntermediateRepresentationCodeAliasKind=
      (
       pircakBOTTOM,
       pircakSTACKLOCAL,
       pircakCONSTANT,
       pircakSTACKESCAPE,
       pircakSYMBOL,
       pircakUNKNOWN
      );

     PPACCIntermediateRepresentationCodeAlias=^TPACCIntermediateRepresentationCodeAlias;
     TPACCIntermediateRepresentationCodeAlias=class
      public
       Kind:TPACCIntermediateRepresentationCodeAliasKind;
       Base:TPACCIntermediateRepresentationCodeOperand;
       Label_:TPACCAbstractSyntaxTreeNodeLabel;
       Offset:TPACCInt64;
       constructor Create;
       destructor Destroy; override;
     end;

     PPACCIntermediateRepresentationCodeTemporaryKind=^TPACCIntermediateRepresentationCodeTemporaryKind;
     TPACCIntermediateRepresentationCodeTemporaryKind=
      (
       pirctkNONE,
       pirctkNORMAL,
       pirctkVARIABLE,
       pirctkLINK,
       pirctkCOUNT
      );

     PPACCIntermediateRepresentationCodeTemporary=^TPACCIntermediateRepresentationCodeTemporary;
     TPACCIntermediateRepresentationCodeTemporary=class
      public
       Kind:TPACCIntermediateRepresentationCodeTemporaryKind;
       Index:TPACCInt32;
       Link:TPACCIntermediateRepresentationCodeTemporary;
       Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;
       Name:TPACCRawByteString;
       Type_:TPACCIntermediateRepresentationCodeType;
       Uses_:TPACCIntermediateRepresentationCodeUseList;
       CountDefinitions:TPACCUInt32;
       Cost:TPACCUInt32;
       Slot:TPACCInt32;
       Phi:TPACCInt32;
       Visit:TPACCInt32;
       Alias:TPACCIntermediateRepresentationCodeAlias;
       MappedTo:array[0..1] of TPACCInt32;
       constructor Create;
       destructor Destroy; override;
     end;

     TPACCIntermediateRepresentationCodeTemporaryList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCIntermediateRepresentationCodeTemporary;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCIntermediateRepresentationCodeTemporary);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCIntermediateRepresentationCodeTemporary read GetItem write SetItem; default;
     end;

     PPACCIntermediateRepresentationCodeInstruction=^TPACCIntermediateRepresentationCodeInstruction;

     TPACCIntermediateRepresentationCodeInstruction=class
      public
       Opcode:TPACCIntermediateRepresentationCodeOpcode;
       Type_:TPACCIntermediateRepresentationCodeType;
       To_:TPACCIntermediateRepresentationCodeOperand;
       Operands:TPACCIntermediateRepresentationCodeOperands;
       SourceLocation:TPACCSourceLocation;
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

     PPACCIntermediateRepresentationCodeJump=^TPACCIntermediateRepresentationCodeJump;
     TPACCIntermediateRepresentationCodeJump=record
      Kind:TPACCIntermediateRepresentationCodeJumpKind;
      Operand:TPACCIntermediateRepresentationCodeOperand;
     end;

     PPACCIntermediateRepresentationCodeBlock=^TPACCIntermediateRepresentationCodeBlock;
     TPACCIntermediateRepresentationCodeBlock=class;

     TPACCIntermediateRepresentationCodeBlocks=array of TPACCIntermediateRepresentationCodeBlock;

     TPACCIntermediateRepresentationCodePhi=class
      public
       To_:TPACCIntermediateRepresentationCodeOperand;
       Operands:array of TPACCIntermediateRepresentationCodeOperand;
       Blocks:array of TPACCIntermediateRepresentationCodeBlock;
       CountOperands:TPACCInt32;
       Type_:TPACCIntermediateRepresentationCodeType;
       Link:TPACCIntermediateRepresentationCodePhi;
       constructor Create; reintroduce;
       destructor Destroy; override;
     end;

     TPACCIntermediateRepresentationCodeBitSet={$ifdef HAVE_ADVANCED_RECORDS}record{$else}object{$endif}
      private
       fBitmap:array of TPACCUInt32;
       fBitmapSize:TPACCInt32;
       procedure SetBitmapSize(const ABitmapSize:TPACCInt32);
       function GetBit(const AIndex:TPACCInt32):boolean;
       procedure SetBit(const AIndex:TPACCInt32;const ABit:boolean);
      public
       procedure Clear;
       procedure ClearBits;
       procedure Assign(const From:TPACCIntermediateRepresentationCodeBitSet);
       procedure Union(const With_:TPACCIntermediateRepresentationCodeBitSet);
       procedure Intersection(const With_:TPACCIntermediateRepresentationCodeBitSet);
       procedure Subtraction(const With_:TPACCIntermediateRepresentationCodeBitSet);
       function EqualsTo(const With_:TPACCIntermediateRepresentationCodeBitSet):boolean;
       function Count:TPACCInt32;
       function IterateToNextBit(var BitIndex:TPACCInt32):boolean;
       property BitmapSize:TPACCInt32 read fBitmapSize write SetBitmapSize;
       property Bits[const AIndex:TPACCInt32]:boolean read GetBit write SetBit; default;
     end;

     TPACCIntermediateRepresentationCodeBlockList=class;
     
     TPACCIntermediateRepresentationCodeBlock=class
      private
       fInstance:TObject;
      public

       Index:TPACCInt32;

       Label_:TPACCAbstractSyntaxTreeNodeLabel;

       Phi:TPACCIntermediateRepresentationCodePhi;

       Instructions:TPACCIntermediateRepresentationCodeInstructionList;

       Jump:TPACCIntermediateRepresentationCodeJump;

       Successors:TPACCIntermediateRepresentationCodeBlockList;

       Link:TPACCIntermediateRepresentationCodeBlock;

       ID:TPACCInt32;
       Visit:TPACCInt32;

       InterDominance:TPACCIntermediateRepresentationCodeBlock;
       Dominance:TPACCIntermediateRepresentationCodeBlock;
       DominanceLink:TPACCIntermediateRepresentationCodeBlock;

       Frontiers:TPACCIntermediateRepresentationCodeBlockList;

       Predecessors:TPACCIntermediateRepresentationCodeBlockList;

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

     TPACCIntermediateRepresentationCodeUnaryOpHook=procedure(var OutputTemporary:TPACCInt32;const InputTemporary:TPACCInt32;const OpNode:TPACCAbstractSyntaxTreeNode) of object;

     TPACCIntermediateRepresentationCodeBinaryOpHook=procedure(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const OpNode:TPACCAbstractSyntaxTreeNode) of object;

     TPACCIntermediateRepresentationCodeFunction=class
      private

       fInstance:TObject;

       CurrentBlock:TPACCIntermediateRepresentationCodeBlock;
       BlockLink:PPACCIntermediateRepresentationCodeBlock;
       PhiLink:PPACCIntermediateRepresentationCodePhi;
       NeedNewBlock:boolean;

       AssignOpLValueTemporary:TPACCInt32;

       function DataTypeToCodeType(const Type_:PPACCType):TPACCIntermediateRepresentationCodeType;
       function NewHiddenLabel:TPACCAbstractSyntaxTreeNodeLabel;
       procedure CloseBlock;
       function FindBlock(const Label_:TPACCAbstractSyntaxTreeNodeLabel):TPACCIntermediateRepresentationCodeBlock;
       procedure EmitLabel(const Label_:TPACCAbstractSyntaxTreeNodeLabel);
       procedure EmitJump(const Label_:TPACCAbstractSyntaxTreeNodeLabel);
       procedure EmitJumpTable(const Operand:TPACCIntermediateRepresentationCodeOperand;
                               const Blocks:array of TPACCIntermediateRepresentationCodeBlock);
       procedure EmitPhi(const Type_:TPACCIntermediateRepresentationCodeType;
                         const To_:TPACCIntermediateRepresentationCodeOperand;
                         const Operands:array of TPACCIntermediateRepresentationCodeOperand;
                         const Blocks:array of TPACCIntermediateRepresentationCodeBlock;
                         const SourceLocation:TPACCSourceLocation);
       procedure CreateNewBlockIfNeeded;
       function CreateTemporary(const Type_:TPACCIntermediateRepresentationCodeType):TPACCInt32;
       function CreateVariableTemporary(const Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable):TPACCInt32;
       function CreateLinkTemporary(const ToTemporaryIndex:TPACCInt32):TPACCInt32;
       function CreateTemporaryOperand(const Temporary:TPACCInt32):TPACCIntermediateRepresentationCodeOperand;
       function CreateIntegerValueOperand(const Value:TPACCInt64):TPACCIntermediateRepresentationCodeOperand;
       function CreateFloatValueOperand(const Value:TPACCDouble):TPACCIntermediateRepresentationCodeOperand;
       function CreateVariableOperand(const Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable):TPACCIntermediateRepresentationCodeOperand;
       function CreateLabelOperand(const Label_:TPACCAbstractSyntaxTreeNodeLabel):TPACCIntermediateRepresentationCodeOperand;
       function CreateFunctionOperand(const TheFunction:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration):TPACCIntermediateRepresentationCodeOperand;
       function SetOperandFlags(const Operand:TPACCIntermediateRepresentationCodeOperand;const IncludeFlags,ExcludeFlags:TPACCIntermediateRepresentationCodeOperandFlags):TPACCIntermediateRepresentationCodeOperand;
       procedure EmitInstruction(const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const AType_:TPACCIntermediateRepresentationCodeType;const ATo_:TPACCIntermediateRepresentationCodeOperand;const AOperands:array of TPACCIntermediateRepresentationCodeOperand;const SourceLocation:TPACCSourceLocation); overload;
       procedure EmitLoad(var OutputTemporary:TPACCInt32;const InputLValueTemporary:TPACCInt32;const Type_:PPACCType;const SourceLocation:TPACCSourceLocation);
       procedure EmitStore(const DestinationLValueTemporary,InputValueTemporary:TPACCInt32;const Type_:PPACCType;const SourceLocation:TPACCSourceLocation);
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
       procedure EmitAssignSrc(const Node:TPACCAbstractSyntaxTreeNode;var OutputTemporary:TPACCInt32);
       procedure EmitIntegerValue(const Node:TPACCAbstractSyntaxTreeNodeIntegerValue;var OutputTemporary:TPACCInt32);
       procedure EmitFloatValue(const Node:TPACCAbstractSyntaxTreeNodeFloatValue;var OutputTemporary:TPACCInt32);
       procedure EmitStoreIntegerValueToVariable(const Node:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;const InputValue:TPACCInt64);
       procedure EmitStoreFloatValueToVariable(const Node:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;const InputValue:TPACCDouble);
       procedure EmitStoreToVariable(const Node:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;const InputTemporary:TPACCInt32);
       procedure EnsureLVarInit(const Node:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;const SourceLocation:TPACCSourceLocation);
       procedure EmitVariable(const Node:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;var OutputTemporary:TPACCInt32);
       procedure EmitComputedJump(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator);
       procedure EmitLogicalANDOR(const Node:TPACCAbstractSyntaxTreeNodeBinaryOperator;var OutputTemporary:TPACCInt32;const IsAND:boolean);
       procedure EmitLogicalNOT(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
       procedure EmitCONV(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
       procedure EmitCAST(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
       procedure EmitFUNCDESG(const Node:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration;var OutputTemporary:TPACCInt32);
       procedure EmitADDR(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
       procedure EmitDEREF(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
       procedure EmitRETURN(const Node:TPACCAbstractSyntaxTreeNodeRETURNStatement);
       procedure EmitTERNARY(const Node:TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator;var OutputTemporary:TPACCInt32);
       procedure EmitSTRUCT_REF(const Node:TPACCAbstractSyntaxTreeNodeStructReference;var OutputTemporary:TPACCInt32);
       procedure EmitCOMMA(const Node:TPACCAbstractSyntaxTreeNodeBinaryOperator;var OutputTemporary:TPACCInt32);
       procedure EmitInitializerList(const Node:TPACCAbstractSyntaxTreeNode;const Nodes:TPACCAbstractSyntaxTreeNodeList;const VariableTemporary:TPACCInt32;const Size,Offset:TPACCInt64);
       procedure EmitDECL(const Node:TPACCAbstractSyntaxTreeNodeDeclaration);
       procedure EmitLValueVariable(const Node:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;var OutputTemporary:TPACCInt32);
       procedure EmitLValueSTRUCT_REF(const Node:TPACCAbstractSyntaxTreeNodeStructReference;var OutputTemporary:TPACCInt32);
       procedure EmitLValueAssignSrc(const Node:TPACCAbstractSyntaxTreeNode;var OutputTemporary:TPACCInt32);
       procedure EmitLValueDEREF(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
       procedure EmitLValue(const Node:TPACCAbstractSyntaxTreeNode;var OutputTemporary:TPACCInt32);
       procedure EmitFunctionCall(const Node:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration;var OutputTemporary:TPACCInt32);
       procedure EmitExpression(const Node:TPACCAbstractSyntaxTreeNode;var OutputTemporary:TPACCInt32);
       procedure EmitFORStatement(const Node:TPACCAbstractSyntaxTreeNodeFORStatement);
       procedure EmitWHILEOrDOStatement(const Node:TPACCAbstractSyntaxTreeNodeWHILEOrDOStatement;const IsWHILE:boolean);
       procedure EmitSWITCHStatement(const Node:TPACCAbstractSyntaxTreeNodeSWITCHStatement);
       procedure EmitIFStatement(const Node:TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator);
       procedure EmitStatement(const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitStatements(const Node:TPACCAbstractSyntaxTreeNodeStatements);
       procedure DeleteBlock(const Block:TPACCIntermediateRepresentationCodeBlock);
       procedure FillRPO;
       procedure FillPredecessors;
       procedure FillUse;
       procedure LiveOn(var BitSet:TPACCIntermediateRepresentationCodeBitSet;const Block,Successor:TPACCIntermediateRepresentationCodeBlock);
       function ReturnRegisters(const Operand:TPACCIntermediateRepresentationCodeOperand;var CountLiveInt,CountLiveFloat:TPACCInt32):TPACCIntermediateRepresentationCodeBitSet;
       function ArgumentRegisters(const Operand:TPACCIntermediateRepresentationCodeOperand;var CountLiveInt,CountLiveFloat:TPACCInt32):TPACCIntermediateRepresentationCodeBitSet;
       function IntegerRegisterToSave(const Operand:TPACCIntermediateRepresentationCodeOperand):TPACCInt32;
       function FloatRegisterToSave(const Operand:TPACCIntermediateRepresentationCodeOperand):TPACCInt32;
       procedure FillLive;
       function CompareSDominance(Block,OtherBlock:TPACCIntermediateRepresentationCodeBlock):boolean;
       function CompareDominance(Block,OtherBlock:TPACCIntermediateRepresentationCodeBlock):boolean;
       function CodeTypeMerge(var ResultType_:TPACCIntermediateRepresentationCodeType;const Type_:TPACCIntermediateRepresentationCodeType):boolean;
       procedure SSA;
       procedure PostProcess;
       procedure EmitFunction(const AFunctionNode:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration);

      public

       FunctionDeclaration:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration;

       FunctionName:TPACCRawByteString;

       Blocks:TPACCIntermediateRepresentationCodeBlockList;

       BlockLabelHashMap:TPACCPointerHashMap;

       StartBlock:TPACCIntermediateRepresentationCodeBlock;

       CountBlocks:TPACCInt32;

       RPO:TPACCIntermediateRepresentationCodeBlocks;

       Temporaries:TPACCIntermediateRepresentationCodeTemporaryList;

       TemporaryReferenceCounter:TPACCUInt32;

       VariableTemporaryHashMap:TPACCPointerHashMap;

       constructor Create(const AInstance:TObject); reintroduce;
       destructor Destroy; override;

       procedure DumpTo(const AStringList:TStringList);

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

     PPACCIntermediateRepresentationCodeDeclarationDataItemKind=^TPACCIntermediateRepresentationCodeDeclarationDataItemKind;
     TPACCIntermediateRepresentationCodeDeclarationDataItemKind=
      (
       pircdikNONE,
       pircdikUI8,
       pircdikUI16,
       pircdikUI32,
       pircdikUI64,
       pircdikCOUNT
      );

     PPACCIntermediateRepresentationCodeDeclarationDataItem=^TPACCIntermediateRepresentationCodeDeclarationDataItem;
     TPACCIntermediateRepresentationCodeDeclarationDataItem=record
      ValueOffsetBase:TPACCAbstractSyntaxTreeNode;
      Count:TPACCUInt64;
      case Kind:TPACCIntermediateRepresentationCodeDeclarationDataItemKind of
       pircdikNONE:(
       );
       pircdikUI8:(
        ValueUI8:TPACCUInt8;
       );
       pircdikUI16:(
        ValueUI16:TPACCUInt16;
       );
       pircdikUI32:(
        ValueUI32:TPACCUInt32;
       );
       pircdikUI64:(
        ValueUI64:TPACCUInt64;
       );
     end;

     TPACCIntermediateRepresentationCodeDeclarationDataItems=array of TPACCIntermediateRepresentationCodeDeclarationDataItem;

     TPACCIntermediateRepresentationCodeDeclaration=class
      private

       fInstance:TObject;

       fDeclaration:TPACCAbstractSyntaxTreeNodeDeclaration;

       fLabel:TPACCAbstractSyntaxTreeNodeLabel;

       fVariable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;

       fSize:TPACCInt64;

       fAlignment:TPACCInt32;

       function NewHiddenLabel:TPACCAbstractSyntaxTreeNodeLabel;

       procedure EmitUI8(const Value:TPACCUInt8;const ValueOffsetBase:TPACCAbstractSyntaxTreeNode;const Count:TPACCInt32);
       procedure EmitUI16(const Value:TPACCUInt16;const ValueOffsetBase:TPACCAbstractSyntaxTreeNode;const Count:TPACCInt32);
       procedure EmitUI32(const Value:TPACCUInt32;const ValueOffsetBase:TPACCAbstractSyntaxTreeNode;const Count:TPACCInt32);
       procedure EmitUI64(const Value:TPACCUInt64;const ValueOffsetBase:TPACCAbstractSyntaxTreeNode;const Count:TPACCInt32);
       procedure EmitStringData(const Node:TPACCAbstractSyntaxTreeNodeStringValue);
       procedure EmitPrimitiveTypeData(const Type_:PPACCType;const Node:TPACCAbstractSyntaxTreeNode);
       procedure EmitInitializerList(const Nodes:TPACCAbstractSyntaxTreeNodeList;Size,Offset:TPACCInt64);
       procedure Finish;

      public

       DataItems:TPACCIntermediateRepresentationCodeDeclarationDataItems;
       CountDataItems:TPACCInt32;

       constructor Create(const AInstance:TObject); reintroduce;
       destructor Destroy; override;

       procedure EmitDeclaration(const Node:TPACCAbstractSyntaxTreeNodeDeclaration);

      published

       property Instance:TObject read fInstance;

       property Declaration:TPACCAbstractSyntaxTreeNodeDeclaration read fDeclaration;

       property Label_:TPACCAbstractSyntaxTreeNodeLabel read fLabel write fLabel;

       property Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable read fVariable write fVariable;

       property Size:TPACCInt64 read fSize write fSize;

       property Alignment:TPACCInt32 read fAlignment write fAlignment;

     end;

     TPACCIntermediateRepresentationCodeDeclarationList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCIntermediateRepresentationCodeDeclaration;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCIntermediateRepresentationCodeDeclaration);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCIntermediateRepresentationCodeDeclaration read GetItem write SetItem; default;
     end;

     TPACCIntermediateRepresentationCode=class
      private

       fInstance:TObject;

       fFunctions:TPACCIntermediateRepresentationCodeFunctionList;

       fDeclarations:TPACCIntermediateRepresentationCodeDeclarationList;

       fExternalDeclarations:TPACCAbstractSyntaxTreeNodeList;

      public

       constructor Create(const AInstance:TObject); reintroduce;
       destructor Destroy; override;

      published

       property Instance:TObject read fInstance;

       property Functions:TPACCIntermediateRepresentationCodeFunctionList read fFunctions;

       property Declarations:TPACCIntermediateRepresentationCodeDeclarationList read fDeclarations;

       property ExternalDeclarations:TPACCAbstractSyntaxTreeNodeList read fExternalDeclarations;

     end;

const EmptyOperand:TPACCIntermediateRepresentationCodeOperand=
       (
        Flags:[];
        Kind:pircokNONE;
       );

      CodeTypeBaseClass:array[TPACCIntermediateRepresentationCodeType] of TPACCInt32=(0,0,0,0,1,1);

      CodeTypeBaseWidth:array[TPACCIntermediateRepresentationCodeType] of TPACCInt32=(0,0,0,1,0,1);

      CodeTypeChars:array[TPACCIntermediateRepresentationCodeType] of AnsiChar=('n','t','i','l','f','d');

var OpcodeNames:array[TPACCIntermediateRepresentationCodeOpcode] of TPACCRawByteString;

    JumpKindNames:array[TPACCIntermediateRepresentationCodeJumpKind] of TPACCRawByteString;
    
procedure GenerateIntermediateRepresentationCode(const AInstance:TObject;const ARootAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNode);

implementation

uses PACCInstance,PACCSort,PasDblStrUtils;

constructor TPACCIntermediateRepresentationCodeUseList.Create;
begin
 inherited Create;
end;

destructor TPACCIntermediateRepresentationCodeUseList.Destroy;
begin
 inherited Destroy;
end;

function TPACCIntermediateRepresentationCodeUseList.GetItem(const AIndex:TPACCInt):TPACCIntermediateRepresentationCodeUse;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCIntermediateRepresentationCodeUseList.SetItem(const AIndex:TPACCInt;const AItem:TPACCIntermediateRepresentationCodeUse);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

constructor TPACCIntermediateRepresentationCodeAlias.Create;
begin
 inherited Create;
 Kind:=pircakUNKNOWN;
 Base.Kind:=pircokNONE;
 Label_:=nil;
 Offset:=0;
end;

destructor TPACCIntermediateRepresentationCodeAlias.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCIntermediateRepresentationCodeTemporary.Create;
begin
 inherited Create;
 Kind:=pirctkNONE;
 Index:=0;
 Link:=nil;
 Variable:=nil;
 Name:='';
 Type_:=pirctNONE;
 Uses_:=TPACCIntermediateRepresentationCodeUseList.Create;
 CountDefinitions:=0;
 Cost:=0;
 Slot:=0;
 Phi:=0;
 Visit:=0;
 MappedTo[0]:=-1;
 MappedTo[1]:=-1;
 Alias:=TPACCIntermediateRepresentationCodeAlias.Create;
end;

destructor TPACCIntermediateRepresentationCodeTemporary.Destroy;
begin
 FreeAndNil(Uses_);
 FreeAndNil(Alias);
 inherited Destroy;
end;

constructor TPACCIntermediateRepresentationCodeTemporaryList.Create;
begin
 inherited Create;
end;

destructor TPACCIntermediateRepresentationCodeTemporaryList.Destroy;
begin
 inherited Destroy;
end;

function TPACCIntermediateRepresentationCodeTemporaryList.GetItem(const AIndex:TPACCInt):TPACCIntermediateRepresentationCodeTemporary;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCIntermediateRepresentationCodeTemporaryList.SetItem(const AIndex:TPACCInt;const AItem:TPACCIntermediateRepresentationCodeTemporary);
begin
 inherited Items[AIndex]:=pointer(AItem);
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

constructor TPACCIntermediateRepresentationCodePhi.Create;
begin
 inherited Create;
 To_.Kind:=pircokNONE;
 Operands:=nil;
 Blocks:=nil;
 CountOperands:=0;
 Type_:=pirctNONE;
 Link:=nil;
end;

destructor TPACCIntermediateRepresentationCodePhi.Destroy;
begin
 Operands:=nil;
 Blocks:=nil;
 inherited Destroy;
end;

procedure TPACCIntermediateRepresentationCodeBitSet.SetBitmapSize(const ABitmapSize:TPACCInt32);
var OldSize:TPACCInt32;
begin
 fBitmapSize:=(ABitmapSize+31) shr 5;
 OldSize:=length(fBitmap);
 if OldSize<fBitmapSize then begin
  SetLength(fBitmap,fBitmapSize*2);
  FillChar(fBitmap[OldSize],(length(fBitmap)-OldSize)*SizeOf(TPACCUInt32),#0);
 end;
end;

function TPACCIntermediateRepresentationCodeBitSet.GetBit(const AIndex:TPACCInt32):boolean;
begin
 result:=((AIndex>=0) and (AIndex<(fBitmapSize shl 5))) and
         ((fBitmap[AIndex shr 5] and (TPACCUInt32(1) shl (AIndex and 31)))<>0);
end;

procedure TPACCIntermediateRepresentationCodeBitSet.SetBit(const AIndex:TPACCInt32;const ABit:boolean);
var OldSize,Index:TPACCInt32;
begin
 if AIndex>=0 then begin
  if (fBitmapSize shl 5)<=AIndex then begin
   fBitmapSize:=(AIndex+31) shr 5;
   OldSize:=length(fBitmap);
   if OldSize<fBitmapSize then begin
    SetLength(fBitmap,fBitmapSize*2);
    FillChar(fBitmap[OldSize],(length(fBitmap)-OldSize)*SizeOf(TPACCUInt32),#0);
   end;
  end;
  if ABit then begin
   fBitmap[AIndex shr 5]:=fBitmap[AIndex shr 5] or (TPACCUInt32(1) shl (AIndex and 31));
  end else begin
   fBitmap[AIndex shr 5]:=fBitmap[AIndex shr 5] and not (TPACCUInt32(1) shl (AIndex and 31));
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeBitSet.Clear;
begin
 fBitmap:=nil;
 fBitmapSize:=0;
end;

procedure TPACCIntermediateRepresentationCodeBitSet.ClearBits;
begin
 if length(fBitmap)>0 then begin
  FillChar(fBitmap[0],length(fBitmap)*SizeOf(TPACCUInt32),#0);
 end;
end;

procedure TPACCIntermediateRepresentationCodeBitSet.Assign(const From:TPACCIntermediateRepresentationCodeBitSet);
begin
 fBitmap:=copy(From.fBitmap);
 fBitmapSize:=From.fBitmapSize;
end;

procedure TPACCIntermediateRepresentationCodeBitSet.Union(const With_:TPACCIntermediateRepresentationCodeBitSet);
var Index:TPACCInt32;
begin
 if (fBitmapSize=With_.fBitmapSize) and (length(fBitmap)=length(With_.fBitmap)) then begin
  for Index:=0 to length(fBitmap)-1 do begin
   fBitmap[Index]:=fBitmap[Index] or With_.fBitmap[Index];
  end;
 end else begin
  raise Exception.Create('TPACCIntermediateRepresentationCodeBitSet.Union');
 end;
end;

procedure TPACCIntermediateRepresentationCodeBitSet.Intersection(const With_:TPACCIntermediateRepresentationCodeBitSet);
var Index:TPACCInt32;
begin
 if (fBitmapSize=With_.fBitmapSize) and (length(fBitmap)=length(With_.fBitmap)) then begin
  for Index:=0 to length(fBitmap)-1 do begin
   fBitmap[Index]:=fBitmap[Index] and With_.fBitmap[Index];
  end;
 end else begin
  raise Exception.Create('TPACCIntermediateRepresentationCodeBitSet.Intersection');
 end;
end;

procedure TPACCIntermediateRepresentationCodeBitSet.Subtraction(const With_:TPACCIntermediateRepresentationCodeBitSet);
var Index:TPACCInt32;
begin
 if (fBitmapSize=With_.fBitmapSize) and (length(fBitmap)=length(With_.fBitmap)) then begin
  for Index:=0 to length(fBitmap)-1 do begin
   fBitmap[Index]:=fBitmap[Index] and not With_.fBitmap[Index];
  end;
 end else begin
  raise Exception.Create('TPACCIntermediateRepresentationCodeBitSet.Subtraction');
 end;
end;

function TPACCIntermediateRepresentationCodeBitSet.EqualsTo(const With_:TPACCIntermediateRepresentationCodeBitSet):boolean;
var Index:TPACCInt32;
begin
 result:=(fBitmapSize=With_.fBitmapSize) and (length(fBitmap)=length(With_.fBitmap));
 if result then begin
  for Index:=0 to length(fBitmap)-1 do begin
   if fBitmap[Index]<>With_.fBitmap[Index] then begin
    result:=false;
    exit;
   end;
  end;
 end;
end;

function TPACCIntermediateRepresentationCodeBitSet.Count:TPACCInt32;
var Index:TPACCInt32;
begin
 result:=0;
 for Index:=0 to length(fBitmap)-1 do begin
  inc(result,TPasMPMath.PopulationCount32(fBitmap[Index]));
 end;
end;

function TPACCIntermediateRepresentationCodeBitSet.IterateToNextBit(var BitIndex:TPACCInt32):boolean;
var BitmapBitIndex,BitmapIndex:TPACCInt32;
    BitmapValue:TPACCUInt32;
begin
 result:=false;
 if BitIndex>=-1 then begin
  inc(BitIndex);
  BitmapBitIndex:=BitIndex and 31;
  BitmapIndex:=BitIndex shr 5;
  if (BitmapIndex>=0) and (BitmapIndex<fBitmapSize) then begin
   BitmapValue:=fBitmap[BitmapIndex] and not ((TPACCUInt32(1) shl (BitmapBitIndex and 31))-1);
   while BitmapValue=0 do begin
    inc(BitmapIndex);
    if BitmapIndex<fBitmapSize then begin
     BitmapValue:=fBitmap[BitmapIndex];
    end else begin
     exit;
    end;
   end;
   BitIndex:=(BitmapIndex shl 5) or TPasMPMath.FindFirstSetBit32(BitmapValue);
   result:=true;
  end;
 end;
end;

constructor TPACCIntermediateRepresentationCodeBlock.Create(const AInstance:TObject);
begin
 inherited Create;

 fInstance:=AInstance;
 TPACCInstance(fInstance).AllocatedObjects.Add(self);

 Index:=0;

 Label_:=nil;

 Phi:=nil;

 Instructions:=TPACCIntermediateRepresentationCodeInstructionList.Create;

 Jump.Kind:=pircjkNONE;

 Successors:=TPACCIntermediateRepresentationCodeBlockList.Create;

 Link:=nil;

 ID:=0;
 Visit:=0;

 InterDominance:=nil;
 Dominance:=nil;
 DominanceLink:=nil;

 Frontiers:=TPACCIntermediateRepresentationCodeBlockList.Create;

 Predecessors:=TPACCIntermediateRepresentationCodeBlockList.Create;

 In_.Clear;
 Out_.Clear;
 Gen_.Clear;

 CountLive[0]:=0;
 CountLive[1]:=0;
 Loop:=0;

end;

destructor TPACCIntermediateRepresentationCodeBlock.Destroy;
begin

 FreeAndNil(Instructions);

 FreeAndNil(Successors);

 FreeAndNil(Frontiers);

 FreeAndNil(Predecessors);

 In_.Clear;
 Out_.Clear;
 Gen_.Clear;

 inherited Destroy;
end;

function TPACCIntermediateRepresentationCodeBlock.AddInstruction(const AInstruction:TPACCIntermediateRepresentationCodeInstruction):TPACCInt32;
begin
 result:=Instructions.Add(AInstruction);
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

 CurrentBlock:=nil;

 Blocks:=TPACCIntermediateRepresentationCodeBlockList.Create;

 BlockLabelHashMap:=TPACCPointerHashMap.Create;

 StartBlock:=nil;

 CountBlocks:=0;

 RPO:=nil;

 Temporaries:=TPACCIntermediateRepresentationCodeTemporaryList.Create;

 TemporaryReferenceCounter:=0;

 VariableTemporaryHashMap:=TPACCPointerHashMap.Create;

end;

destructor TPACCIntermediateRepresentationCodeFunction.Destroy;
begin
 Blocks.Free;
 BlockLabelHashMap.Free;
 VariableTemporaryHashMap.Free;
 Temporaries.Free;
 RPO:=nil;
 inherited Destroy;
end;

function TPACCIntermediateRepresentationCodeFunction.DataTypeToCodeType(const Type_:PPACCType):TPACCIntermediateRepresentationCodeType;
begin
 if (Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
    ((Type_^.Kind=tkPOINTER) and
     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
  result:=pirctINT;
 end else if (Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  result:=pirctLONG;
 end else if Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  result:=pirctFLOAT;
 end else if Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  result:=pirctDOUBLE;
 end else begin
  result:=pirctNONE;
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-13-59-0000',nil,true);
 end;
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
  inc(CountBlocks);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitLabel(const Label_:TPACCAbstractSyntaxTreeNodeLabel);
var Block:TPACCIntermediateRepresentationCodeBlock;
begin
 Block:=FindBlock(Label_);
 if assigned(CurrentBlock) and (CurrentBlock.Jump.Kind=pircjkNONE) then begin
  CloseBlock;
  CurrentBlock.Jump.Kind:=pircjkJMP;
  CurrentBlock.Successors.Clear;
  CurrentBlock.Successors.Add(Block);
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
  CurrentBlock.Successors.Clear;
  CurrentBlock.Successors.Add(Block);
 CloseBlock;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitJumpTable(const Operand:TPACCIntermediateRepresentationCodeOperand;
                                                                    const Blocks:array of TPACCIntermediateRepresentationCodeBlock);
var Index:TPACCInt32;
begin
 CurrentBlock.Jump.Kind:=pircjkJMPT;
 CurrentBlock.Jump.Operand:=Operand;
 CurrentBlock.Successors.Clear;
 for Index:=0 to length(Blocks)-1 do begin
  CurrentBlock.Successors.Add(Blocks[Index]);
 end;
 CloseBlock;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitPhi(const Type_:TPACCIntermediateRepresentationCodeType;
                                                              const To_:TPACCIntermediateRepresentationCodeOperand;
                                                              const Operands:array of TPACCIntermediateRepresentationCodeOperand;
                                                              const Blocks:array of TPACCIntermediateRepresentationCodeBlock;
                                                              const SourceLocation:TPACCSourceLocation);
var Index:TPACCInt32;
    Phi:TPACCIntermediateRepresentationCodePhi;
begin
 if assigned(CurrentBlock) then begin
  if assigned(CurrentBlock.Phi) then begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-24-10-59-0001',nil,true);
  end else if length(Operands)=0 then begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-24-11-01-0000',nil,true);
  end else if length(Operands)<>length(Blocks) then begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-24-11-01-0001',nil,true);
  end else begin
   Phi:=TPACCIntermediateRepresentationCodePhi.Create;
   TPACCInstance(fInstance).AllocatedObjects.Add(Phi);
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
   PhiLink:=@Phi.Link;
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
var Temporary:TPACCIntermediateRepresentationCodeTemporary;
begin
 Temporary:=TPACCIntermediateRepresentationCodeTemporary.Create;
 TPACCInstance(fInstance).AllocatedObjects.Add(Temporary);
 result:=Temporaries.Add(Temporary);
 Temporary.Kind:=pirctkNORMAL;
 Temporary.Index:=result;
 Temporary.Link:=nil;
 Temporary.Type_:=Type_;
end;

function TPACCIntermediateRepresentationCodeFunction.CreateVariableTemporary(const Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable):TPACCInt32;
var Temporary:TPACCIntermediateRepresentationCodeTemporary;
begin
 Temporary:=VariableTemporaryHashMap[Variable];
 if assigned(Temporary) then begin
  result:=Temporary.Index;
 end else begin
  Temporary:=TPACCIntermediateRepresentationCodeTemporary.Create;
  TPACCInstance(fInstance).AllocatedObjects.Add(Temporary);
  result:=Temporaries.Add(Temporary);
  Temporary.Kind:=pirctkVARIABLE;
  Temporary.Index:=result;
  Temporary.Type_:=pirctTOP;
  Temporary.Variable:=Variable;
  VariableTemporaryHashMap[Variable]:=Temporary;
 end;
end;

function TPACCIntermediateRepresentationCodeFunction.CreateLinkTemporary(const ToTemporaryIndex:TPACCInt32):TPACCInt32;
var Temporary,ToTemporary:TPACCIntermediateRepresentationCodeTemporary;
begin
 ToTemporary:=Temporaries[ToTemporaryIndex];
 Temporary:=TPACCIntermediateRepresentationCodeTemporary.Create;
 TPACCInstance(fInstance).AllocatedObjects.Add(Temporary);
 result:=Temporaries.Add(Temporary);
 Temporary.Kind:=pirctkLINK;
 Temporary.Index:=result;
 Temporary.Link:=ToTemporary;
 Temporary.Type_:=ToTemporary.Type_;
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
 if Variable.Kind=astnkLVAR then begin
  // Local variable, so convert into a temporary
  result:=CreateTemporaryOperand(CreateVariableTemporary(Variable));
 end else begin
  // Global variable
  result.Flags:=[];
  result.Kind:=pircokVARIABLE;
  result.Variable:=Variable;
 end;
end;

function TPACCIntermediateRepresentationCodeFunction.CreateLabelOperand(const Label_:TPACCAbstractSyntaxTreeNodeLabel):TPACCIntermediateRepresentationCodeOperand;
begin
 result.Flags:=[];
 result.Kind:=pircokLABEL;
 result.Label_:=Label_;
end;

function TPACCIntermediateRepresentationCodeFunction.CreateFunctionOperand(const TheFunction:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration):TPACCIntermediateRepresentationCodeOperand;
begin
 result.Flags:=[];
 result.Kind:=pircokFUNCTION;
 result.Function_:=TheFunction;
end;

function TPACCIntermediateRepresentationCodeFunction.SetOperandFlags(const Operand:TPACCIntermediateRepresentationCodeOperand;const IncludeFlags,ExcludeFlags:TPACCIntermediateRepresentationCodeOperandFlags):TPACCIntermediateRepresentationCodeOperand;
begin
 result:=Operand;
 result.Flags:=(result.Flags-ExcludeFlags)+IncludeFlags;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitInstruction(const AOpcode:TPACCIntermediateRepresentationCodeOpcode;const AType_:TPACCIntermediateRepresentationCodeType;const ATo_:TPACCIntermediateRepresentationCodeOperand;const AOperands:array of TPACCIntermediateRepresentationCodeOperand;const SourceLocation:TPACCSourceLocation);
var Index:TPACCInt32;
    Instruction:TPACCIntermediateRepresentationCodeInstruction;
begin
 Instruction:=TPACCIntermediateRepresentationCodeInstruction.Create;
 TPACCInstance(fInstance).AllocatedObjects.Add(Instruction);
 Instruction.Opcode:=AOpcode;
 Instruction.Type_:=AType_;
 Instruction.To_:=ATo_;
 Instruction.Operands:=nil;
 if length(AOperands)>0 then begin
  SetLength(Instruction.Operands,length(AOperands));
  for Index:=0 to length(AOperands)-1 do begin
   Instruction.Operands[Index]:=AOperands[Index];
  end;
 end;
 Instruction.SourceLocation:=SourceLocation;
 CreateNewBlockIfNeeded;
 CurrentBlock.AddInstruction(Instruction);
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitLoad(var OutputTemporary:TPACCInt32;const InputLValueTemporary:TPACCInt32;const Type_:PPACCType;const SourceLocation:TPACCSourceLocation);
var ValueTemporary,ShiftedValueTemporary:TPACCInt32;
begin
 case Type_^.Kind of
  tkBOOL,tkCHAR:begin
   OutputTemporary:=CreateTemporary(pirctINT);
   if tfUnsigned in Type_^.Flags then begin
    EmitInstruction(pircoLDUCI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLValueTemporary)],SourceLocation);
   end else begin
    EmitInstruction(pircoLDSCI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLValueTemporary)],SourceLocation);
   end;
  end;
  tkSHORT:begin
   OutputTemporary:=CreateTemporary(pirctINT);
   if tfUnsigned in Type_^.Flags then begin
    EmitInstruction(pircoLDUSI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLValueTemporary)],SourceLocation);
   end else begin
    EmitInstruction(pircoLDSSI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLValueTemporary)],SourceLocation);
   end;
  end;
  tkINT,tkENUM:begin
   OutputTemporary:=CreateTemporary(pirctINT);
   if tfUnsigned in Type_^.Flags then begin
    EmitInstruction(pircoLDUII,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLValueTemporary)],SourceLocation);
   end else begin
    EmitInstruction(pircoLDSII,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLValueTemporary)],SourceLocation);
   end;
  end;
  tkLONG,tkLLONG:begin
   OutputTemporary:=CreateTemporary(pirctLONG);
   if tfUnsigned in Type_^.Flags then begin
    EmitInstruction(pircoLDULL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLValueTemporary)],SourceLocation);
   end else begin
    EmitInstruction(pircoLDSLL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLValueTemporary)],SourceLocation);
   end;
  end;
  tkFLOAT:begin
   OutputTemporary:=CreateTemporary(pirctFLOAT);
   EmitInstruction(pircoLDF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLValueTemporary)],SourceLocation);
  end;
  tkDOUBLE,tkLDOUBLE:begin
   OutputTemporary:=CreateTemporary(pirctDOUBLE);
   EmitInstruction(pircoLDD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLValueTemporary)],SourceLocation);
  end;
  tkPOINTER:begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    OutputTemporary:=CreateTemporary(pirctINT);
    if tfUnsigned in Type_^.Flags then begin
     EmitInstruction(pircoLDUII,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLValueTemporary)],SourceLocation);
    end else begin
     EmitInstruction(pircoLDSII,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLValueTemporary)],SourceLocation);
    end;
   end else begin
    OutputTemporary:=CreateTemporary(pirctLONG);
    if tfUnsigned in Type_^.Flags then begin
     EmitInstruction(pircoLDULL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLValueTemporary)],SourceLocation);
    end else begin
     EmitInstruction(pircoLDSLL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLValueTemporary)],SourceLocation);
    end;
   end;
  end;
  else begin
   OutputTemporary:=-1;
   TPACCInstance(fInstance).AddError('Internal error 2017-01-24-17-28-0000',@SourceLocation,true);
  end;
 end;
 if (Type_^.BitSize>0) and
    (Type_^.Kind in (PACCIntermediateRepresentationCodeINTTypeKinds+PACCIntermediateRepresentationCodeLONGTypeKinds)) then begin
  ValueTemporary:=OutputTemporary;
  if Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
   ShiftedValueTemporary:=CreateTemporary(pirctINT);
   OutputTemporary:=CreateTemporary(pirctINT);
   EmitInstruction(pircoSHRI,pirctINT,CreateTemporaryOperand(ShiftedValueTemporary),[CreateTemporaryOperand(ValueTemporary),CreateIntegerValueOperand(Type_^.BitOffset)],SourceLocation);
   EmitInstruction(pircoANDI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(ShiftedValueTemporary),CreateIntegerValueOperand((TPACCInt64(1) shl Type_^.BitSize)-1)],SourceLocation);
  end else begin
   ShiftedValueTemporary:=CreateTemporary(pirctLONG);
   OutputTemporary:=CreateTemporary(pirctLONG);
   EmitInstruction(pircoSHRL,pirctLONG,CreateTemporaryOperand(ShiftedValueTemporary),[CreateTemporaryOperand(ValueTemporary),CreateIntegerValueOperand(Type_^.BitOffset)],SourceLocation);
   EmitInstruction(pircoANDL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(ShiftedValueTemporary),CreateIntegerValueOperand((TPACCInt64(1) shl Type_^.BitSize)-1)],SourceLocation);
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitStore(const DestinationLValueTemporary,InputValueTemporary:TPACCInt32;const Type_:PPACCType;const SourceLocation:TPACCSourceLocation);
var ValueTemporary,MaskedValueTemporary,MaskedInputValueTemporary,ShiftedInputValueTemporary,CombinedValueTemporary:TPACCInt32;
begin
 if (Type_^.BitSize>0) and
    (Type_^.Kind in (PACCIntermediateRepresentationCodeINTTypeKinds+PACCIntermediateRepresentationCodeLONGTypeKinds)) then begin
  case Type_^.Kind of
   tkBOOL,tkCHAR:begin
    ValueTemporary:=CreateTemporary(pirctINT);
    if tfUnsigned in Type_^.Flags then begin
     EmitInstruction(pircoLDUCI,pirctINT,CreateTemporaryOperand(ValueTemporary),[CreateTemporaryOperand(DestinationLValueTemporary)],SourceLocation);
    end else begin
     EmitInstruction(pircoLDSCI,pirctINT,CreateTemporaryOperand(ValueTemporary),[CreateTemporaryOperand(DestinationLValueTemporary)],SourceLocation);
    end;
   end;
   tkSHORT:begin
    ValueTemporary:=CreateTemporary(pirctINT);
    if tfUnsigned in Type_^.Flags then begin
     EmitInstruction(pircoLDUSI,pirctINT,CreateTemporaryOperand(ValueTemporary),[CreateTemporaryOperand(DestinationLValueTemporary)],SourceLocation);
    end else begin
     EmitInstruction(pircoLDSSI,pirctINT,CreateTemporaryOperand(ValueTemporary),[CreateTemporaryOperand(DestinationLValueTemporary)],SourceLocation);
    end;
   end;
   tkINT,tkENUM:begin
    ValueTemporary:=CreateTemporary(pirctINT);
    if tfUnsigned in Type_^.Flags then begin
     EmitInstruction(pircoLDUII,pirctINT,CreateTemporaryOperand(ValueTemporary),[CreateTemporaryOperand(DestinationLValueTemporary)],SourceLocation);
    end else begin
     EmitInstruction(pircoLDSII,pirctINT,CreateTemporaryOperand(ValueTemporary),[CreateTemporaryOperand(DestinationLValueTemporary)],SourceLocation);
    end;
   end;
   tkLONG,tkLLONG:begin
    ValueTemporary:=CreateTemporary(pirctLONG);
    if tfUnsigned in Type_^.Flags then begin
     EmitInstruction(pircoLDULL,pirctLONG,CreateTemporaryOperand(ValueTemporary),[CreateTemporaryOperand(DestinationLValueTemporary)],SourceLocation);
    end else begin
     EmitInstruction(pircoLDSLL,pirctLONG,CreateTemporaryOperand(ValueTemporary),[CreateTemporaryOperand(DestinationLValueTemporary)],SourceLocation);
    end;
   end;
   else begin
    ValueTemporary:=-1;
    TPACCInstance(fInstance).AddError('Internal error 2017-01-24-17-41-0000',@SourceLocation,true);
   end;
  end;
  if Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
   MaskedValueTemporary:=CreateTemporary(pirctINT);
   MaskedInputValueTemporary:=CreateTemporary(pirctINT);
   ShiftedInputValueTemporary:=CreateTemporary(pirctINT);
   CombinedValueTemporary:=CreateTemporary(pirctINT);
   EmitInstruction(pircoANDI,pirctINT,CreateTemporaryOperand(MaskedValueTemporary),[CreateTemporaryOperand(ValueTemporary),CreateIntegerValueOperand(not (((TPACCInt64(1) shl Type_^.BitSize)-1) shl Type_^.BitOffset))],SourceLocation);
   EmitInstruction(pircoANDI,pirctINT,CreateTemporaryOperand(MaskedInputValueTemporary),[CreateTemporaryOperand(InputValueTemporary),CreateIntegerValueOperand((TPACCInt64(1) shl Type_^.BitSize)-1)],SourceLocation);
   EmitInstruction(pircoSHLI,pirctINT,CreateTemporaryOperand(ShiftedInputValueTemporary),[CreateTemporaryOperand(MaskedInputValueTemporary),CreateIntegerValueOperand(Type_^.BitOffset)],SourceLocation);
   EmitInstruction(pircoORI,pirctINT,CreateTemporaryOperand(CombinedValueTemporary),[CreateTemporaryOperand(MaskedValueTemporary),CreateTemporaryOperand(ShiftedInputValueTemporary)],SourceLocation);
  end else begin
   MaskedValueTemporary:=CreateTemporary(pirctLONG);
   MaskedInputValueTemporary:=CreateTemporary(pirctLONG);
   ShiftedInputValueTemporary:=CreateTemporary(pirctLONG);
   CombinedValueTemporary:=CreateTemporary(pirctLONG);
   EmitInstruction(pircoANDL,pirctINT,CreateTemporaryOperand(MaskedValueTemporary),[CreateTemporaryOperand(ValueTemporary),CreateIntegerValueOperand(not (((TPACCInt64(1) shl Type_^.BitSize)-1) shl Type_^.BitOffset))],SourceLocation);
   EmitInstruction(pircoANDL,pirctINT,CreateTemporaryOperand(MaskedInputValueTemporary),[CreateTemporaryOperand(InputValueTemporary),CreateIntegerValueOperand((TPACCInt64(1) shl Type_^.BitSize)-1)],SourceLocation);
   EmitInstruction(pircoSHLL,pirctINT,CreateTemporaryOperand(ShiftedInputValueTemporary),[CreateTemporaryOperand(MaskedInputValueTemporary),CreateIntegerValueOperand(Type_^.BitOffset)],SourceLocation);
   EmitInstruction(pircoORL,pirctINT,CreateTemporaryOperand(CombinedValueTemporary),[CreateTemporaryOperand(MaskedValueTemporary),CreateTemporaryOperand(ShiftedInputValueTemporary)],SourceLocation);
  end;
  ValueTemporary:=CombinedValueTemporary;
 end else begin
  ValueTemporary:=InputValueTemporary;
 end;
 case Type_^.Kind of
  tkBOOL,tkCHAR:begin
   EmitInstruction(pircoSTIC,pirctNONE,EmptyOperand,[CreateTemporaryOperand(DestinationLValueTemporary),CreateTemporaryOperand(ValueTemporary)],SourceLocation);
  end;
  tkSHORT:begin
   EmitInstruction(pircoSTIS,pirctNONE,EmptyOperand,[CreateTemporaryOperand(DestinationLValueTemporary),CreateTemporaryOperand(ValueTemporary)],SourceLocation);
  end;
  tkINT,tkENUM:begin
   EmitInstruction(pircoSTII,pirctNONE,EmptyOperand,[CreateTemporaryOperand(DestinationLValueTemporary),CreateTemporaryOperand(ValueTemporary)],SourceLocation);
  end;
  tkLONG,tkLLONG:begin
   EmitInstruction(pircoSTLL,pirctNONE,EmptyOperand,[CreateTemporaryOperand(DestinationLValueTemporary),CreateTemporaryOperand(ValueTemporary)],SourceLocation);
  end;
  tkFLOAT:begin
   EmitInstruction(pircoSTF,pirctNONE,EmptyOperand,[CreateTemporaryOperand(DestinationLValueTemporary),CreateTemporaryOperand(ValueTemporary)],SourceLocation);
  end;
  tkDOUBLE,tkLDOUBLE:begin
   EmitInstruction(pircoSTD,pirctNONE,EmptyOperand,[CreateTemporaryOperand(DestinationLValueTemporary),CreateTemporaryOperand(ValueTemporary)],SourceLocation);
  end;
  tkPOINTER:begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    EmitInstruction(pircoSTII,pirctNONE,EmptyOperand,[CreateTemporaryOperand(DestinationLValueTemporary),CreateTemporaryOperand(ValueTemporary)],SourceLocation);
   end else begin
    EmitInstruction(pircoSTLL,pirctNONE,EmptyOperand,[CreateTemporaryOperand(DestinationLValueTemporary),CreateTemporaryOperand(ValueTemporary)],SourceLocation);
   end;
  end;
  else begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-24-17-40-0000',@SourceLocation,true);
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitUnaryOpINC(var OutputTemporary:TPACCInt32;const InputTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoADDI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(1)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoADDL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(1)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoADDF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputTemporary),CreateFloatValueOperand(1)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoADDD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputTemporary),CreateFloatValueOperand(1)],Node.SourceLocation);
 end else if Node.Type_^.Kind=tkPOINTER then begin
  if assigned(Node.Type_^.ChildType) then begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoADDI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(Node.Type_^.ChildType.Size)],Node.SourceLocation);
   end else begin
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoADDL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(Node.Type_^.ChildType.Size)],Node.SourceLocation);
   end;
  end else begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoADDI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(1)],Node.SourceLocation);
   end else begin
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoADDL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(1)],Node.SourceLocation);
   end;
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitUnaryOpDEC(var OutputTemporary:TPACCInt32;const InputTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoSUBI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(1)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoSUBL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(1)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoSUBF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputTemporary),CreateFloatValueOperand(1)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoSUBD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputTemporary),CreateFloatValueOperand(1)],Node.SourceLocation);
 end else if Node.Type_^.Kind=tkPOINTER then begin
  if assigned(Node.Type_^.ChildType) then begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoSUBI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(Node.Type_^.ChildType.Size)],Node.SourceLocation);
   end else begin
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoSUBL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(Node.Type_^.ChildType.Size)],Node.SourceLocation);
   end;
  end else begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoSUBI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(1)],Node.SourceLocation);
   end else begin
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoSUBL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputTemporary),CreateIntegerValueOperand(1)],Node.SourceLocation);
   end;
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpADD(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
var IntermediateResultTemporary:TPACCInt32;
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoADDI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoADDL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoADDF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoADDD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind=tkPOINTER then begin
  if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Kind=tkPOINTER) and
     assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType) and
     TPACCInstance(fInstance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    IntermediateResultTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoUMULI,pirctINT,CreateTemporaryOperand(IntermediateResultTemporary),[CreateTemporaryOperand(InputRightTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType^.Size)],Node.SourceLocation);
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoADDI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(IntermediateResultTemporary)],Node.SourceLocation);
   end else begin
    IntermediateResultTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoUMULL,pirctLONG,CreateTemporaryOperand(IntermediateResultTemporary),[CreateTemporaryOperand(InputRightTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType^.Size)],Node.SourceLocation);
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoADDL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(IntermediateResultTemporary)],Node.SourceLocation);
   end;
  end else if TPACCInstance(fInstance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) and
              (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Kind=tkPOINTER) and
              assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.ChildType) then begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    IntermediateResultTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoUMULI,pirctINT,CreateTemporaryOperand(IntermediateResultTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.ChildType^.Size)],Node.SourceLocation);
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoADDI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(IntermediateResultTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
   end else begin
    IntermediateResultTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoUMULL,pirctLONG,CreateTemporaryOperand(IntermediateResultTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.ChildType^.Size)],Node.SourceLocation);
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoADDL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(IntermediateResultTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
   end;
  end else begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoADDI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
   end else begin
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoADDL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
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
   EmitInstruction(pircoSUBI,pirctINT,CreateTemporaryOperand(IntermediateResultTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
   OutputTemporary:=CreateTemporary(pirctINT);
   if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType) then begin
    EmitInstruction(pircoSDIVI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(IntermediateResultTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType^.Size)],Node.SourceLocation);
   end else begin
    EmitInstruction(pircoSDIVI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(IntermediateResultTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.ChildType^.Size)],Node.SourceLocation);
   end;
  end else begin
   OutputTemporary:=CreateTemporary(pirctINT);
   EmitInstruction(pircoSUBI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Kind=tkPOINTER) and
     (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Kind=tkPOINTER) and
     (assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType) or
      assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.ChildType)) then begin
   IntermediateResultTemporary:=CreateTemporary(pirctLONG);
   EmitInstruction(pircoSUBL,pirctLONG,CreateTemporaryOperand(IntermediateResultTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
   OutputTemporary:=CreateTemporary(pirctLONG);
   if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType) then begin
    EmitInstruction(pircoSDIVL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(IntermediateResultTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.ChildType^.Size)],Node.SourceLocation);
   end else begin
    EmitInstruction(pircoSDIVL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(IntermediateResultTemporary),CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.ChildType^.Size)],Node.SourceLocation);
   end;
  end else begin
   OutputTemporary:=CreateTemporary(pirctLONG);
   EmitInstruction(pircoSUBL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoSUBF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoSUBD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind=tkPOINTER then begin
  if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
   OutputTemporary:=CreateTemporary(pirctINT);
   EmitInstruction(pircoSUBI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   OutputTemporary:=CreateTemporary(pirctLONG);
   EmitInstruction(pircoSUBL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
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
   EmitInstruction(pircoUMULI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoSMULI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoUMULL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoSMULL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoMULF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoMULD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
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
   EmitInstruction(pircoUDIVI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoSDIVI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoUDIVL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoSDIVL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoDIVF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoDIVD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
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
   EmitInstruction(pircoUMODI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoSMODI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoUMODL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoSMODL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-01-21-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpAND(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoANDI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoANDL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-01-23-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpOR(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoORI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoORL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-01-23-0002',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpXOR(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoXORI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoXORL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-01-24-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpSHL(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoSHLI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoSHLL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-01-24-0002',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpSHR(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoSHRI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoSHRL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-01-24-0004',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitBinaryOpSAR(var OutputTemporary:TPACCInt32;const InputLeftTemporary,InputRightTemporary:TPACCInt32;const Node:TPACCAbstractSyntaxTreeNode);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoSARI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoSARL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
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
   EmitInstruction(pircoCMPULEI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoCMPSLEI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoCMPULEL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoCMPSLEL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoCMPLEF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoCMPLED,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
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
   EmitInstruction(pircoCMPULTI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoCMPSLTI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoCMPULTL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoCMPSLTL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoCMPLTF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoCMPLTD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
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
   EmitInstruction(pircoCMPUGEI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoCMPSGEI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoCMPUGEL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoCMPSGEL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoCMPGEF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoCMPGED,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
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
   EmitInstruction(pircoCMPUGTI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoCMPSGTI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  if (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_^.Flags) or
     (tfUnsigned in TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_^.Flags) then begin
   EmitInstruction(pircoCMPUGTL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end else begin
   EmitInstruction(pircoCMPSGTL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
  end;
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoCMPGTF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoCMPGTD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
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
  EmitInstruction(pircoCMPEQI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoCMPEQL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoCMPEQF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoCMPEQD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
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
 EmitInstruction(pircoCMPNEI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoCMPNEL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoCMPNEF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoCMPNED,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(InputLeftTemporary),CreateTemporaryOperand(InputRightTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-12-05-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitNOT(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
var ValueTemporary:TPACCInt32;
begin
 ValueTemporary:=-1;
 EmitExpression(Node.Operand,ValueTemporary);
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoNOTI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(ValueTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoNOTL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(ValueTemporary)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-18-03-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitNEG(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
var ValueTemporary:TPACCInt32;
begin
 ValueTemporary:=-1;
 EmitExpression(Node.Operand,ValueTemporary);
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoNEGI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(ValueTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoNEGL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(ValueTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoNEGF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(ValueTemporary)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoNEGD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(ValueTemporary)],Node.SourceLocation);
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
  EnsureLVarInit(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(LValueNode),LValueNode.SourceLocation);
  if PostOp then begin
   EmitExpression(LValueNode,TemporaryA);
   if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoMOVI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
   end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoMOVL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
   end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
    OutputTemporary:=CreateTemporary(pirctFLOAT);
    EmitInstruction(pircoMOVF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
   end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
    OutputTemporary:=CreateTemporary(pirctDOUBLE);
    EmitInstruction(pircoMOVD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
   end else if Node.Type_^.Kind=tkPOINTER then begin
    if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
     OutputTemporary:=CreateTemporary(pirctINT);
     EmitInstruction(pircoMOVI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
    end else begin
     OutputTemporary:=CreateTemporary(pirctLONG);
     EmitInstruction(pircoMOVL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
    end;
   end;
   UnaryOpHook(TemporaryB,TemporaryA,Node);
   EmitStoreToVariable(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(LValueNode),TemporaryB);
  end else begin
   EmitExpression(LValueNode,TemporaryA);
   UnaryOpHook(OutputTemporary,TemporaryA,Node);
   EmitStoreToVariable(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(LValueNode),OutputTemporary);
  end;
 end else begin
  if PostOp then begin
   EmitLValue(LValueNode,LValueTemporary);
   EmitLoad(TemporaryA,LValueTemporary,LValueNode.Type_,Node.SourceLocation);
   if Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds then begin
    OutputTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoMOVI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
   end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds then begin
    OutputTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoMOVL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
   end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
    OutputTemporary:=CreateTemporary(pirctFLOAT);
    EmitInstruction(pircoMOVF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
   end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
    OutputTemporary:=CreateTemporary(pirctDOUBLE);
    EmitInstruction(pircoMOVD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
   end else if Node.Type_^.Kind=tkPOINTER then begin
    if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
     OutputTemporary:=CreateTemporary(pirctINT);
     EmitInstruction(pircoMOVI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
    end else begin
     OutputTemporary:=CreateTemporary(pirctLONG);
     EmitInstruction(pircoMOVL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
    end;
   end;
   UnaryOpHook(TemporaryB,TemporaryA,Node);
   EmitStore(LValueTemporary,TemporaryB,LValueNode.Type_,Node.SourceLocation);
  end else begin
   EmitLValue(LValueNode,LValueTemporary);
   EmitLoad(TemporaryA,LValueTemporary,LValueNode.Type_,Node.SourceLocation);
   UnaryOpHook(OutputTemporary,TemporaryA,Node);
   EmitStore(LValueTemporary,OutputTemporary,LValueNode.Type_,Node.SourceLocation);
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
  EmitExpression(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left,TemporaryA);
  EmitExpression(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right,TemporaryB);
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
  EmitLValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left,TemporaryA);
  EmitLoad(TemporaryB,TemporaryA,TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_,Node.SourceLocation);
  EmitExpression(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right,TemporaryC);
  BinaryOpHook(OutputTemporary,TemporaryB,TemporaryC,Node);
  EmitStore(TemporaryA,OutputTemporary,TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_,Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-14-04-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitAssign(const Node:TPACCAbstractSyntaxTreeNodeBinaryOperator;var OutputTemporary:TPACCInt32);
var AssignLValueTemporary,VariableTemporary:TPACCInt32;
begin
 OutputTemporary:=-1;
 if Node.Left.Kind in [astnkLVAR,astnkGVAR] then begin
  EnsureLVarInit(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node.Left),Node.Left.SourceLocation);
  if Node.Right.Kind in [astnkLVAR,astnkGVAR] then begin
   EnsureLVarInit(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node.Right),Node.Right.SourceLocation);
  end;
  if Node.Left.Type_^.Kind in [tkARRAY,tkSTRUCT] then begin
   VariableTemporary:=0;
   EmitLValue(Node.Left,AssignLValueTemporary);
   if Node.Right.Kind=astnkOP_ASSIGN then begin
    EmitExpression(Node.Right,VariableTemporary);
    EmitLValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node.Right).Right,VariableTemporary);
   end else begin
    EmitLValue(Node.Right,VariableTemporary);
   end;
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    EmitInstruction(pircoMEMCPYI,pirctNONE,EmptyOperand,[CreateTemporaryOperand(AssignLValueTemporary),CreateTemporaryOperand(VariableTemporary),CreateIntegerValueOperand(Node.Left.Type_^.Size)],Node.SourceLocation);
   end else begin
    EmitInstruction(pircoMEMCPYL,pirctNONE,EmptyOperand,[CreateTemporaryOperand(AssignLValueTemporary),CreateTemporaryOperand(VariableTemporary),CreateIntegerValueOperand(Node.Left.Type_^.Size)],Node.SourceLocation);
   end;
  end else begin
   if (Node.Left.Kind in [astnkLVAR,astnkGVAR]) and
      (Node.Left.Type_^.Kind in (PACCIntermediateRepresentationCodeINTTypeKinds+
                                 PACCIntermediateRepresentationCodeLONGTypeKinds+
                                 PACCIntermediateRepresentationCodeFLOATTypeKinds+
                                 PACCIntermediateRepresentationCodeDOUBLETypeKinds)) and
      (Node.Right.Kind=astnkINTEGER) then begin
    EmitStoreIntegerValueToVariable(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node.Left),TPACCAbstractSyntaxTreeNodeIntegerValue(Node.Right).Value);
   end else if (Node.Left.Kind in [astnkLVAR,astnkGVAR]) and
               (Node.Left.Type_^.Kind in (PACCIntermediateRepresentationCodeFLOATTypeKinds+
                                          PACCIntermediateRepresentationCodeDOUBLETypeKinds)) and
               (Node.Right.Kind=astnkFLOAT) then begin
    EmitStoreFloatValueToVariable(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node.Left),TPACCAbstractSyntaxTreeNodeFloatValue(Node.Right).Value);
   end else begin
    EmitExpression(Node.Right,OutputTemporary);
    EmitStoreToVariable(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node.Left),OutputTemporary);
   end;
  end;
 end else begin
  AssignLValueTemporary:=-1;
  EmitLValue(Node.Left,AssignLValueTemporary);
  EmitExpression(Node.Right,OutputTemporary);
  EmitStore(AssignLValueTemporary,OutputTemporary,Node.Left.Type_,Node.SourceLocation);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitAssignOp(const Node:TPACCAbstractSyntaxTreeNodeBinaryOperator;var OutputTemporary:TPACCInt32);
var LastAssignOpLValueTemporary:TPACCInt32;
begin
 LastAssignOpLValueTemporary:=AssignOpLValueTemporary;
 try
  AssignOpLValueTemporary:=-1;
  OutputTemporary:=-1;
  if Node.Left.Kind in [astnkLVAR,astnkGVAR] then begin
   EnsureLVarInit(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node.Left),Node.Left.SourceLocation);
   EmitExpression(Node.Right,OutputTemporary);
   EmitStoreToVariable(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node.Left),OutputTemporary);
  end else begin
   EmitLValue(Node.Left,AssignOpLValueTemporary);
   EmitExpression(Node.Right,OutputTemporary);
   EmitStore(AssignOpLValueTemporary,OutputTemporary,Node.Left.Type_,Node.SourceLocation);
  end;
 finally
  AssignOpLValueTemporary:=LastAssignOpLValueTemporary;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitAssignSrc(const Node:TPACCAbstractSyntaxTreeNode;var OutputTemporary:TPACCInt32);
begin
 if AssignOpLValueTemporary<0 then begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-23-15-41-0000',@Node.SourceLocation,true);
 end else begin
  EmitLoad(OutputTemporary,AssignOpLValueTemporary,Node.Type_,Node.SourceLocation);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitIntegerValue(const Node:TPACCAbstractSyntaxTreeNodeIntegerValue;var OutputTemporary:TPACCInt32);
begin
 if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
    ((Node.Type_^.Kind=tkPOINTER) and
     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoSETI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeIntegerValue(Node).Value)],Node.SourceLocation);
 end else if (Node.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoSETL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateIntegerValueOperand(TPACCAbstractSyntaxTreeNodeIntegerValue(Node).Value)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-22-16-01-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitFloatValue(const Node:TPACCAbstractSyntaxTreeNodeFloatValue;var OutputTemporary:TPACCInt32);
begin
 if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctFLOAT);
  EmitInstruction(pircoSETF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateFloatValueOperand(TPACCAbstractSyntaxTreeNodeFloatValue(Node).Value)],Node.SourceLocation);
 end else if Node.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  OutputTemporary:=CreateTemporary(pirctDOUBLE);
  EmitInstruction(pircoSETD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateFloatValueOperand(TPACCAbstractSyntaxTreeNodeFloatValue(Node).Value)],Node.SourceLocation);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-22-16-04-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitStoreIntegerValueToVariable(const Node:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;const InputValue:TPACCInt64);
begin
 case Node.Type_^.Kind of
  tkBOOL,tkCHAR:begin
   EmitInstruction(pircoSTIC,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateIntegerValueOperand(InputValue)],Node.SourceLocation);
  end;
  tkSHORT:begin
   EmitInstruction(pircoSTIS,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateIntegerValueOperand(InputValue)],Node.SourceLocation);
  end;
  tkINT,tkENUM:begin
   EmitInstruction(pircoSTII,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateIntegerValueOperand(InputValue)],Node.SourceLocation);
  end;
  tkLONG,tkLLONG:begin
   EmitInstruction(pircoSTLL,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateIntegerValueOperand(InputValue)],Node.SourceLocation);
  end;
  tkFLOAT:begin
   EmitInstruction(pircoSTF,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateFloatValueOperand(InputValue)],Node.SourceLocation);
  end;
  tkDOUBLE,tkLDOUBLE:begin
   EmitInstruction(pircoSTD,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateFloatValueOperand(InputValue)],Node.SourceLocation);
  end;
  tkPOINTER:begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    EmitInstruction(pircoSTII,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateIntegerValueOperand(InputValue)],Node.SourceLocation);
   end else begin
    EmitInstruction(pircoSTLL,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateIntegerValueOperand(InputValue)],Node.SourceLocation);
   end;
  end;
  else begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-22-16-36-0000',@Node.SourceLocation,true);
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitStoreFloatValueToVariable(const Node:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;const InputValue:TPACCDouble);
begin
 case Node.Type_^.Kind of
  tkFLOAT:begin
   EmitInstruction(pircoSTF,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateFloatValueOperand(InputValue)],Node.SourceLocation);
  end;
  tkDOUBLE,tkLDOUBLE:begin
   EmitInstruction(pircoSTD,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateFloatValueOperand(InputValue)],Node.SourceLocation);
  end;
  else begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-22-16-36-0000',@Node.SourceLocation,true);
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitStoreToVariable(const Node:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;const InputTemporary:TPACCInt32);
begin
 case Node.Type_^.Kind of
  tkBOOL,tkCHAR:begin
   EmitInstruction(pircoSTIC,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateTemporaryOperand(InputTemporary)],Node.SourceLocation);
  end;
  tkSHORT:begin
   EmitInstruction(pircoSTIS,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateTemporaryOperand(InputTemporary)],Node.SourceLocation);
  end;
  tkINT,tkENUM:begin
   EmitInstruction(pircoSTII,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateTemporaryOperand(InputTemporary)],Node.SourceLocation);
  end;
  tkLONG,tkLLONG:begin
   EmitInstruction(pircoSTLL,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateTemporaryOperand(InputTemporary)],Node.SourceLocation);
  end;
  tkFLOAT:begin
   EmitInstruction(pircoSTF,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateTemporaryOperand(InputTemporary)],Node.SourceLocation);
  end;
  tkDOUBLE,tkLDOUBLE:begin
   EmitInstruction(pircoSTD,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateTemporaryOperand(InputTemporary)],Node.SourceLocation);
  end;
  tkPOINTER:begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    EmitInstruction(pircoSTII,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateTemporaryOperand(InputTemporary)],Node.SourceLocation);
   end else begin
    EmitInstruction(pircoSTLL,pirctNONE,EmptyOperand,[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)),CreateTemporaryOperand(InputTemporary)],Node.SourceLocation);
   end;
  end;
  else begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-22-16-36-0000',@Node.SourceLocation,true);
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EnsureLVarInit(const Node:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;const SourceLocation:TPACCSourceLocation);
var VariableTemporary:TPACCInt32;
    LocalVariableInitialization:TPACCAbstractSyntaxTreeNodeList;
begin
 if assigned(Node.LocalVariableInitialization) then begin
  LocalVariableInitialization:=Node.LocalVariableInitialization;
  Node.LocalVariableInitialization:=nil;
  EmitLValue(Node,VariableTemporary);
  EmitInitializerList(Node,LocalVariableInitialization,VariableTemporary,Node.Type_^.Size,0);
 end;
end;                                                                               

procedure TPACCIntermediateRepresentationCodeFunction.EmitVariable(const Node:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;var OutputTemporary:TPACCInt32);
var LValueTemporary,SrcTemporary:TPACCInt32;
begin
 EnsureLVarInit(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node),Node.SourceLocation);
 case Node.Type_^.Kind of
  tkBOOL,tkCHAR:begin
   OutputTemporary:=CreateTemporary(pirctINT);
   if tfUnsigned in Node.Type_^.Flags then begin
    EmitInstruction(pircoLDUCI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
   end else begin
    EmitInstruction(pircoLDSCI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
   end;
  end;
  tkSHORT:begin
   OutputTemporary:=CreateTemporary(pirctINT);
   if tfUnsigned in Node.Type_^.Flags then begin
    EmitInstruction(pircoLDUSI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
   end else begin
    EmitInstruction(pircoLDSSI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
   end;
  end;
  tkINT,tkENUM:begin
   OutputTemporary:=CreateTemporary(pirctINT);
   if tfUnsigned in Node.Type_^.Flags then begin
    EmitInstruction(pircoLDUII,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
   end else begin
    EmitInstruction(pircoLDSII,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
   end;
  end;
  tkLONG,tkLLONG:begin
   OutputTemporary:=CreateTemporary(pirctLONG);
   if tfUnsigned in Node.Type_^.Flags then begin
    EmitInstruction(pircoLDULL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
   end else begin
    EmitInstruction(pircoLDSLL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
   end;
  end;
  tkFLOAT:begin
   OutputTemporary:=CreateTemporary(pirctFLOAT);
   EmitInstruction(pircoLDF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
  end;
  tkDOUBLE,tkLDOUBLE:begin
   OutputTemporary:=CreateTemporary(pirctDOUBLE);
   EmitInstruction(pircoLDD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
  end;
  tkPOINTER:begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    OutputTemporary:=CreateTemporary(pirctINT);
    if tfUnsigned in Node.Type_^.Flags then begin
     EmitInstruction(pircoLDUII,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
    end else begin
     EmitInstruction(pircoLDSII,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
    end;
   end else begin
    OutputTemporary:=CreateTemporary(pirctLONG);
    if tfUnsigned in Node.Type_^.Flags then begin
     EmitInstruction(pircoLDULL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
    end else begin
     EmitInstruction(pircoLDSLL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
    end;
   end;
  end;
  else begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-22-16-15-0000',@Node.SourceLocation,true);
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitComputedJump(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator);
var ComputedTargetTemporary:TPACCInt32;
begin
 ComputedTargetTemporary:=-1;
 EmitExpression(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,ComputedTargetTemporary);
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
 EmitExpression(Node.Left,LeftTemporary);
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
 CurrentBlock.Successors.Clear;
 if IsAND then begin
  CurrentBlock.Successors.Add(b0);
  CurrentBlock.Successors.Add(b2);
 end else begin
  CurrentBlock.Successors.Add(b1);
  CurrentBlock.Successors.Add(b0);
 end;
 CloseBlock;

 EmitLabel(l0);
 RightTemporary:=-1;
 EmitExpression(Node.Right,RightTemporary);
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
 CurrentBlock.Successors.Clear;
 CurrentBlock.Successors.Add(b1);
 CurrentBlock.Successors.Add(b2);
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
 EmitExpression(Node.Operand,OperandTemporary);
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
 CurrentBlock.Successors.Clear;
 CurrentBlock.Successors.Add(b0);
 CurrentBlock.Successors.Add(b1);
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

procedure TPACCIntermediateRepresentationCodeFunction.EmitCONV(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
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
   EmitExpression(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,OutputTemporary);
  end else begin
   TemporaryA:=-1;
   EmitExpression(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA);
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
        EmitInstruction(pircoZECI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSECI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkCHAR:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZECI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSECI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkSHORT:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZESI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSESI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkINT,tkENUM:begin
       OutputTemporary:=TemporaryA;
      end;
      tkLONG:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       EmitInstruction(pircoTRLI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkLLONG:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       EmitInstruction(pircoTRLI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkFLOAT:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       EmitInstruction(pircoTFTI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkDOUBLE:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       EmitInstruction(pircoTDTI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkLDOUBLE:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       EmitInstruction(pircoTDTI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkPOINTER:begin
       if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong then begin
        OutputTemporary:=CreateTemporary(pirctINT);
        EmitInstruction(pircoTRLI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
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
        EmitInstruction(pircoZECL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSECL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkCHAR:begin
       OutputTemporary:=CreateTemporary(pirctLONG);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZECL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSECL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkSHORT:begin
       OutputTemporary:=CreateTemporary(pirctLONG);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZESL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSESL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkINT,tkENUM:begin
       OutputTemporary:=CreateTemporary(pirctLONG);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZEIL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSEIL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
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
       EmitInstruction(pircoTFTL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkDOUBLE:begin
       OutputTemporary:=CreateTemporary(pirctLONG);
       EmitInstruction(pircoTDTL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkLDOUBLE:begin
       OutputTemporary:=CreateTemporary(pirctLONG);
       EmitInstruction(pircoTDTL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkPOINTER:begin
       if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong then begin
        OutputTemporary:=TemporaryA;
       end else begin
        OutputTemporary:=CreateTemporary(pirctLONG);
        if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
           (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
         EmitInstruction(pircoZEIL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
        end else begin
         EmitInstruction(pircoSEIL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
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
         EmitInstruction(pircoZEIL,pirctLONG,CreateTemporaryOperand(TemporaryB),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
         OutputTemporary:=CreateTemporary(pirctFLOAT);
         EmitInstruction(pircoCLTF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryB)],Node.SourceLocation);
        end else begin
         OutputTemporary:=CreateTemporary(pirctFLOAT);
         EmitInstruction(pircoCITF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
        end;
       end else if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
                   ((TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind=tkPOINTER) and
                     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
        OutputTemporary:=CreateTemporary(pirctFLOAT);
        EmitInstruction(pircoCLTF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else if TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
        OutputTemporary:=CreateTemporary(pirctFLOAT);
        EmitInstruction(pircoCDTF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
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
         EmitInstruction(pircoZEIL,pirctLONG,CreateTemporaryOperand(TemporaryB),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
         OutputTemporary:=CreateTemporary(pirctDOUBLE);
         EmitInstruction(pircoCLTD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryB)],Node.SourceLocation);
        end else begin
         OutputTemporary:=CreateTemporary(pirctDOUBLE);
         EmitInstruction(pircoCITD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
        end;
       end else if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
                   ((TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind=tkPOINTER) and
                     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
        OutputTemporary:=CreateTemporary(pirctDOUBLE);
        EmitInstruction(pircoCLTD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else if TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
        OutputTemporary:=CreateTemporary(pirctDOUBLE);
        EmitInstruction(pircoCFTD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
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

procedure TPACCIntermediateRepresentationCodeFunction.EmitCAST(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
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
   EmitExpression(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,OutputTemporary);
  end else begin
   TemporaryA:=-1;
   EmitExpression(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,TemporaryA);
   if TemporaryA<0 then begin
    TPACCInstance(fInstance).AddError('Internal error 2017-01-24-15-21-0000',@Node.SourceLocation,true);
   end else begin
    if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
       ((TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Kind=tkPOINTER) and
        (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
     case TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind of
      tkBOOL:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZECI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSECI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkCHAR:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZECI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSECI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkSHORT:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZESI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSESI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkINT,tkENUM:begin
       OutputTemporary:=TemporaryA;
      end;
      tkLONG:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       EmitInstruction(pircoTRLI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkLLONG:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       EmitInstruction(pircoTRLI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkFLOAT:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       EmitInstruction(pircoTFTI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkDOUBLE:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       EmitInstruction(pircoTDTI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkLDOUBLE:begin
       OutputTemporary:=CreateTemporary(pirctINT);
       EmitInstruction(pircoTDTI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkPOINTER:begin
       if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong then begin
        OutputTemporary:=CreateTemporary(pirctINT);
        EmitInstruction(pircoTRLI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        OutputTemporary:=TemporaryA;
       end;
      end;
      else begin
       TPACCInstance(fInstance).AddError('Internal error 2017-01-24-15-12-0000',@Node.SourceLocation,true);
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
        EmitInstruction(pircoZECL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSECL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkCHAR:begin
       OutputTemporary:=CreateTemporary(pirctLONG);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZECL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSECL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkSHORT:begin
       OutputTemporary:=CreateTemporary(pirctLONG);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZESL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSESL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end;
      end;
      tkINT,tkENUM:begin
       OutputTemporary:=CreateTemporary(pirctLONG);
       if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
          (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
        EmitInstruction(pircoZEIL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        EmitInstruction(pircoSEIL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
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
       EmitInstruction(pircoTFTL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkDOUBLE:begin
       OutputTemporary:=CreateTemporary(pirctLONG);
       EmitInstruction(pircoTDTL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkLDOUBLE:begin
       OutputTemporary:=CreateTemporary(pirctLONG);
       EmitInstruction(pircoTDTL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
      end;
      tkPOINTER:begin
       if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong then begin
        OutputTemporary:=TemporaryA;
       end else begin
        OutputTemporary:=CreateTemporary(pirctLONG);
        if (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_^.Flags) or
           (tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags) then begin
         EmitInstruction(pircoZEIL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
        end else begin
         EmitInstruction(pircoSEIL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
        end;
       end;
      end;
      else begin
       TPACCInstance(fInstance).AddError('Internal error 2017-01-24-15-12-0000',@Node.SourceLocation,true);
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
         EmitInstruction(pircoZEIL,pirctLONG,CreateTemporaryOperand(TemporaryB),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
         OutputTemporary:=CreateTemporary(pirctFLOAT);
         EmitInstruction(pircoCLTF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryB)],Node.SourceLocation);
        end else begin
         OutputTemporary:=CreateTemporary(pirctFLOAT);
         EmitInstruction(pircoCITF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
        end;
       end else if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
                   ((TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind=tkPOINTER) and
                     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
        OutputTemporary:=CreateTemporary(pirctFLOAT);
        EmitInstruction(pircoCLTF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else if TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
        OutputTemporary:=CreateTemporary(pirctFLOAT);
        EmitInstruction(pircoCDTF,pirctFLOAT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        TPACCInstance(fInstance).AddError('Internal error 2017-01-24-15-32-0000',@Node.SourceLocation,true);
       end;
      end;
      tkDOUBLE,tkLDOUBLE:begin
       if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
           ((TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind=tkPOINTER) and
            (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
        if tfUnsigned in TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Flags then begin
         TemporaryB:=CreateTemporary(pirctLONG);
         EmitInstruction(pircoZEIL,pirctLONG,CreateTemporaryOperand(TemporaryB),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
         OutputTemporary:=CreateTemporary(pirctDOUBLE);
         EmitInstruction(pircoCLTD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryB)],Node.SourceLocation);
        end else begin
         OutputTemporary:=CreateTemporary(pirctDOUBLE);
         EmitInstruction(pircoCITD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
        end;
       end else if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
                   ((TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind=tkPOINTER) and
                     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
        OutputTemporary:=CreateTemporary(pirctDOUBLE);
        EmitInstruction(pircoCLTD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else if TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
        OutputTemporary:=CreateTemporary(pirctDOUBLE);
        EmitInstruction(pircoCFTD,pirctDOUBLE,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(TemporaryA)],Node.SourceLocation);
       end else begin
        TPACCInstance(fInstance).AddError('Internal error 2017-01-24-15-32-0000',@Node.SourceLocation,true);
       end;
      end;
      else begin
       TPACCInstance(fInstance).AddError('Internal error 2017-01-24-15-19-0000',@Node.SourceLocation,true);
      end;
     end;
    end;
   end;
  end;
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-14-53-0000',@Node.SourceLocation,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitFUNCDESG(const Node:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration;var OutputTemporary:TPACCInt32);
begin
 if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Variable) then begin
  if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
   OutputTemporary:=CreateTemporary(pirctINT);
   EmitInstruction(pircoADDROFI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Variable))],Node.SourceLocation);
  end else begin
   OutputTemporary:=CreateTemporary(pirctLONG);
   EmitInstruction(pircoADDROFL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Variable))],Node.SourceLocation);
  end;
 end else begin
  if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
   OutputTemporary:=CreateTemporary(pirctINT);
   EmitInstruction(pircoADDROFI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateFunctionOperand(Node)],Node.SourceLocation);
  end else begin
   OutputTemporary:=CreateTemporary(pirctLONG);
   EmitInstruction(pircoADDROFL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateFunctionOperand(Node)],Node.SourceLocation);
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitADDR(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
begin
 if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
  case TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Kind of
   astnkLVAR,astnkGVAR:begin 
    EnsureLVarInit(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node.Operand),Node.SourceLocation);
    if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
     OutputTemporary:=CreateTemporary(pirctINT);
     EmitInstruction(pircoADDROFI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand))],TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.SourceLocation);
    end else begin
     OutputTemporary:=CreateTemporary(pirctLONG);
     EmitInstruction(pircoADDROFL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand))],TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.SourceLocation);
    end;
   end;
   astnkSTRUCT_REF:begin
    EmitLValue(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,OutputTemporary);
   end;
   astnkDEREF:begin
    EmitLValue(TPACCAbstractSyntaxTreeNodeUnaryOperator(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand).Operand,OutputTemporary);
   end;
   astnkFUNCDESG:begin
    EmitFUNCDESG(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand),OutputTemporary);
   end;
   else begin
    TPACCInstance(fInstance).AddError('Internal error 2017-01-22-17-17-0000',nil,true);
   end;
  end;
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-22-09-33-0002',nil,true);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitDEREF(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
var LValueTemporary:TPACCInt32;
begin
 LValueTemporary:=-1;
 OutputTemporary:=-1;
 if TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Kind=astnkADDR then begin
  EmitExpression(TPACCAbstractSyntaxTreeNodeUnaryOperator(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand).Operand,OutputTemporary);
 end else begin
  EmitLValue(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,LValueTemporary);
  EmitLoad(OutputTemporary,LValueTemporary,Node.Type_,Node.SourceLocation);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitRETURN(const Node:TPACCAbstractSyntaxTreeNodeRETURNStatement);
var TemporaryA:TPACCInt32;
begin
 if assigned(FunctionDeclaration.Type_) and
    assigned(FunctionDeclaration.Type_^.ReturnType) then begin
  TemporaryA:=-1;
  if assigned(TPACCAbstractSyntaxTreeNodeRETURNStatement(Node).ReturnValue) then begin
   EmitExpression(TPACCAbstractSyntaxTreeNodeRETURNStatement(Node).ReturnValue,TemporaryA);
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

procedure TPACCIntermediateRepresentationCodeFunction.EmitTERNARY(const Node:TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator;var OutputTemporary:TPACCInt32);
var l0,l1,l2:TPACCAbstractSyntaxTreeNodeLabel;
    b0,b1,b2:TPACCIntermediateRepresentationCodeBlock;
    ConditionTemporary,TrueTemporary,FalseTemporary:TPACCInt32;
    Type_:TPACCIntermediateRepresentationCodeType;
begin

 l0:=NewHiddenLabel;
 l1:=NewHiddenLabel;
 l2:=NewHiddenLabel;
 b0:=FindBlock(l0);
 b1:=FindBlock(l1);
 b2:=FindBlock(l2);

 ConditionTemporary:=-1;
 EmitExpression(Node.Condition,ConditionTemporary);
 if (Node.Condition.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
    ((Node.Condition.Type_^.Kind=tkPOINTER) and
     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
  CurrentBlock.Jump.Kind:=pircjkJNZI;
 end else if (Node.Condition.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Condition.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  CurrentBlock.Jump.Kind:=pircjkJNZL;
 end else if Node.Condition.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  CurrentBlock.Jump.Kind:=pircjkJNZF;
 end else if Node.Condition.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  CurrentBlock.Jump.Kind:=pircjkJNZD;
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-13-52-0000',@Node.SourceLocation,true);
 end;
 CurrentBlock.Jump.Operand:=CreateTemporaryOperand(ConditionTemporary);
 CurrentBlock.Successors.Clear;
 CurrentBlock.Successors.Add(b0);
 CurrentBlock.Successors.Add(b1);
 CloseBlock;

 EmitLabel(l0);
 TrueTemporary:=-1;
 if assigned(Node.Then_) then begin
  EmitExpression(Node.Then_,TrueTemporary);
 end else begin
  ConditionTemporary:=TrueTemporary;
 end;
 EmitJump(l2);

 EmitLabel(l1);
 EmitExpression(Node.Else_,FalseTemporary);
 EmitJump(l2);

 EmitLabel(l2);

 Type_:=DataTypeToCodeType(Node.Type_);
 OutputTemporary:=CreateTemporary(Type_);
 EmitPhi(Type_,
         CreateTemporaryOperand(OutputTemporary),
         [CreateTemporaryOperand(TrueTemporary),
          CreateTemporaryOperand(FalseTemporary)],
         [b0,
          b1],
         Node.SourceLocation);

end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitSTRUCT_REF(const Node:TPACCAbstractSyntaxTreeNodeStructReference;var OutputTemporary:TPACCInt32);
var StructTemporary:TPACCInt32;
begin
 OutputTemporary:=-1;
 StructTemporary:=-1;
 EmitLValueSTRUCT_REF(Node,StructTemporary);
 EmitLoad(OutputTemporary,StructTemporary,Node.Type_,Node.SourceLocation);
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitCOMMA(const Node:TPACCAbstractSyntaxTreeNodeBinaryOperator;var OutputTemporary:TPACCInt32);
var TemporaryA:TPACCInt32;
begin
 if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
    assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
  TemporaryA:=-1;
  EmitExpression(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left,TemporaryA);
  EmitExpression(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right,OutputTemporary);
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-21-15-08-0000',nil,true);
 end;
end;

function TPACCIntermediateRepresentationCodeFunctionEmitInitializerListCompareInitializers(a,b:pointer):TPACCInt32;
begin
 result:=TPACCAbstractSyntaxTreeNodeInitializer(a).InitializionOffset-TPACCAbstractSyntaxTreeNodeInitializer(b).InitializionOffset;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitInitializerList(const Node:TPACCAbstractSyntaxTreeNode;const Nodes:TPACCAbstractSyntaxTreeNodeList;const VariableTemporary:TPACCInt32;const Size,Offset:TPACCInt64);
var Index,TargetTemporary,ValueTemporary:TPACCInt32;
    SubNode:TPACCAbstractSyntaxTreeNodeInitializer;
    TempNodes:TPACCAbstractSyntaxTreeNodeList;
    LastEnd:TPACCInt64;
begin
 TempNodes:=TPACCAbstractSyntaxTreeNodeList.Create;
 try
  for Index:=0 to Nodes.Count-1 do begin
   SubNode:=TPACCAbstractSyntaxTreeNodeInitializer(Nodes[Index]);
   TempNodes.Add(SubNode);
  end;
  TempNodes.Sort(TPACCIntermediateRepresentationCodeFunctionEmitInitializerListCompareInitializers);
  TargetTemporary:=-1;
  LastEnd:=0;
  for Index:=0 to TempNodes.Count-1 do begin
   SubNode:=TPACCAbstractSyntaxTreeNodeInitializer(TempNodes[Index]);
   if LastEnd<SubNode.InitializionOffset then begin
    if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
     TargetTemporary:=CreateTemporary(pirctINT);
     EmitInstruction(pircoADDI,pirctINT,CreateTemporaryOperand(TargetTemporary),[CreateTemporaryOperand(VariableTemporary),CreateIntegerValueOperand(Offset+SubNode.InitializionOffset)],SubNode.SourceLocation);
     EmitInstruction(pircoZEROMEMI,pirctNONE,EmptyOperand,[CreateTemporaryOperand(TargetTemporary),CreateIntegerValueOperand(SubNode.Type_.Size)],SubNode.SourceLocation);
    end else begin
     TargetTemporary:=CreateTemporary(pirctLONG);
     EmitInstruction(pircoADDL,pirctLONG,CreateTemporaryOperand(TargetTemporary),[CreateTemporaryOperand(VariableTemporary),CreateIntegerValueOperand(Offset+SubNode.InitializionOffset)],SubNode.SourceLocation);
     EmitInstruction(pircoZEROMEML,pirctNONE,EmptyOperand,[CreateTemporaryOperand(TargetTemporary),CreateIntegerValueOperand(SubNode.Type_.Size)],SubNode.SourceLocation);
    end;
   end else begin
    ValueTemporary:=-1;
    if SubNode.Kind=astnkLVAR then begin
     EmitInitializerList(Node,
                         TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(SubNode).LocalVariableInitialization,
                         VariableTemporary,
                         TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(SubNode).Type_^.Size,
                         Offset+SubNode.InitializionOffset);
    end else begin
     EmitExpression(SubNode.InitializionValue,ValueTemporary);
     if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
      TargetTemporary:=CreateTemporary(pirctINT);
      EmitInstruction(pircoADDI,pirctINT,CreateTemporaryOperand(TargetTemporary),[CreateTemporaryOperand(VariableTemporary),CreateIntegerValueOperand(Offset+SubNode.InitializionOffset)],SubNode.SourceLocation);
     end else begin
      TargetTemporary:=CreateTemporary(pirctLONG);
      EmitInstruction(pircoADDL,pirctLONG,CreateTemporaryOperand(TargetTemporary),[CreateTemporaryOperand(VariableTemporary),CreateIntegerValueOperand(Offset+SubNode.InitializionOffset)],SubNode.SourceLocation);
     end;
     EmitStore(TargetTemporary,ValueTemporary,SubNode.ToType,Node.SourceLocation);
    end;
   end;
   LastEnd:=SubNode.InitializionOffset+SubNode.ToType^.Size;
  end;
  if LastEnd<Size then begin
   if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
    TargetTemporary:=CreateTemporary(pirctINT);
    EmitInstruction(pircoADDI,pirctINT,CreateTemporaryOperand(TargetTemporary),[CreateTemporaryOperand(VariableTemporary),CreateIntegerValueOperand(Offset+LastEnd)],Node.SourceLocation);
    EmitInstruction(pircoZEROMEMI,pirctNONE,EmptyOperand,[CreateTemporaryOperand(TargetTemporary),CreateIntegerValueOperand(Size-LastEnd)],Node.SourceLocation);
   end else begin
    TargetTemporary:=CreateTemporary(pirctLONG);
    EmitInstruction(pircoADDL,pirctLONG,CreateTemporaryOperand(TargetTemporary),[CreateTemporaryOperand(VariableTemporary),CreateIntegerValueOperand(Offset+LastEnd)],Node.SourceLocation);
    EmitInstruction(pircoZEROMEML,pirctNONE,EmptyOperand,[CreateTemporaryOperand(TargetTemporary),CreateIntegerValueOperand(Size-LastEnd)],Node.SourceLocation);
   end;
  end;
 finally
  TempNodes.Free;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitDECL(const Node:TPACCAbstractSyntaxTreeNodeDeclaration);
var Index,VariableTemporary,ValueTemporary:TPACCInt32;
begin
 if assigned(Node.DeclarationInitialization) then begin
  VariableTemporary:=-1;
  ValueTemporary:=-1;
  if (Node.DeclarationInitialization.Count=1) and
     (TPACCInstance(fInstance).IsArithmeticType(Node.DeclarationVariable.Type_) or
      (Node.DeclarationVariable.Type_^.Kind=tkPOINTER)) then begin
   if Node.DeclarationVariable.Kind in [astnkLVAR,astnkGVAR] then begin
    EnsureLVarInit(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node.DeclarationVariable),Node.SourceLocation);
    if assigned(TPACCAbstractSyntaxTreeNodeInitializer(Node.DeclarationInitialization[0]).InitializionValue) and
       (TPACCAbstractSyntaxTreeNodeInitializer(Node.DeclarationInitialization[0]).InitializionValue.Kind=astnkINTEGER) then begin
     EmitStoreIntegerValueToVariable(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node.DeclarationVariable),TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeInitializer(Node.DeclarationInitialization[0]).InitializionValue).Value);
    end else if assigned(TPACCAbstractSyntaxTreeNodeInitializer(Node.DeclarationInitialization[0]).InitializionValue) and
                (TPACCAbstractSyntaxTreeNodeInitializer(Node.DeclarationInitialization[0]).InitializionValue.Kind=astnkFLOAT) then begin
     EmitStoreFloatValueToVariable(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node.DeclarationVariable),TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeInitializer(Node.DeclarationInitialization[0]).InitializionValue).Value);
    end else begin
     EmitExpression(TPACCAbstractSyntaxTreeNodeInitializer(Node.DeclarationInitialization[0]).InitializionValue,ValueTemporary);
     EmitStoreToVariable(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node.DeclarationVariable),ValueTemporary);
    end;
   end else begin
    EmitLValue(Node.DeclarationVariable,VariableTemporary);
    EmitExpression(TPACCAbstractSyntaxTreeNodeInitializer(Node.DeclarationInitialization[0]).InitializionValue,ValueTemporary);
    EmitStore(VariableTemporary,ValueTemporary,Node.DeclarationVariable.Type_,Node.SourceLocation);
   end;
  end else begin
   EmitLValue(Node.DeclarationVariable,VariableTemporary);
   EmitInitializerList(Node,Node.DeclarationInitialization,VariableTemporary,Node.DeclarationVariable.Type_^.Size,0);
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitLValueVariable(const Node:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;var OutputTemporary:TPACCInt32);
var LValueTemporary:TPACCInt32;
begin
 EnsureLVarInit(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node),Node.SourceLocation);
 if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
  OutputTemporary:=CreateTemporary(pirctINT);
  EmitInstruction(pircoADDROFI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
 end else begin
  OutputTemporary:=CreateTemporary(pirctLONG);
  EmitInstruction(pircoADDROFL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateVariableOperand(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node))],Node.SourceLocation);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitLValueSTRUCT_REF(const Node:TPACCAbstractSyntaxTreeNodeStructReference;var OutputTemporary:TPACCInt32);
var StructTemporary:TPACCInt32;
begin
 OutputTemporary:=-1;
 if Node.Type_^.Offset>0 then begin
  StructTemporary:=-1;
  EmitLValue(Node.Struct,StructTemporary);
  if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
   OutputTemporary:=CreateTemporary(pirctINT);
   EmitInstruction(pircoADDI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(StructTemporary),CreateIntegerValueOperand(Node.Type_^.Offset)],Node.SourceLocation);
  end else begin
   OutputTemporary:=CreateTemporary(pirctLONG);
   EmitInstruction(pircoADDL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateTemporaryOperand(StructTemporary),CreateIntegerValueOperand(Node.Type_^.Offset)],Node.SourceLocation);
  end;
 end else begin
  EmitLValue(Node.Struct,OutputTemporary);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitLValueAssignSrc(const Node:TPACCAbstractSyntaxTreeNode;var OutputTemporary:TPACCInt32);
begin
 if AssignOpLValueTemporary<0 then begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-20-20-0000',@Node.SourceLocation,true);
 end else begin
  OutputTemporary:=AssignOpLValueTemporary;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitLValueDEREF(const Node:TPACCAbstractSyntaxTreeNodeUnaryOperator;var OutputTemporary:TPACCInt32);
begin
 case Node.Operand.Kind of
  astnkADDR:begin
   EmitLValue(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node.Operand).Operand,OutputTemporary);
  end;
  else begin
   EmitExpression(Node.Operand,OutputTemporary);
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitLValue(const Node:TPACCAbstractSyntaxTreeNode;var OutputTemporary:TPACCInt32);
begin
 case Node.Kind of
  astnkLVAR,astnkGVAR:begin
   EmitLValueVariable(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node),OutputTemporary);
  end;
  astnkSTRUCT_REF:begin
   EmitLValueSTRUCT_REF(TPACCAbstractSyntaxTreeNodeStructReference(Node),OutputTemporary);
  end;
  astnkOP_ASSIGN_SRC:begin
   EmitLValueAssignSrc(Node,OutputTemporary);
  end;
  astnkDEREF:begin
   EmitLValueDEREF(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node),OutputTemporary);
  end;
  astnkOP_ASSIGN:begin
   EmitExpression(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node),OutputTemporary);
   EmitLValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left,OutputTemporary);
  end;
  else begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-24-20-17-0000',@Node.SourceLocation,true);
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitFunctionCall(const Node:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration;var OutputTemporary:TPACCInt32);
var Index:TPACCInt32;
    FunctionPointerTemporary,ArgumentTemporary:TPACCInt32;
    FunctionCallOperands:array of TPACCIntermediateRepresentationCodeOperand;
    OutputTemporaryOperand:TPACCIntermediateRepresentationCodeOperand;
    CodeType:TPACCIntermediateRepresentationCodeType;
begin
 FunctionPointerTemporary:=1;
 if Node.Kind=astnkFUNCPTR_CALL then begin
  if assigned(Node.FunctionPointer) then begin
   EmitExpression(Node.FunctionPointer,FunctionPointerTemporary);
  end else begin
   TPACCInstance(fInstance).AddError('Internal error 2017-01-24-21-15-0000',@Node.SourceLocation,true);
  end;
 end;
 if assigned(Node.Type_^.ReturnType) and (Node.Type_^.ReturnType^.Kind<>tkVOID) then begin
  CodeType:=DataTypeToCodeType(Node.Type_^.ReturnType);
  OutputTemporary:=CreateTemporary(CodeType);
 end else begin
  CodeType:=pirctNONE;
  OutputTemporary:=-1;
 end;
 FunctionCallOperands:=nil;
 try
  if assigned(Node.Arguments) and (Node.Arguments.Count>0) then begin
   SetLength(FunctionCallOperands,Node.Arguments.Count+1);
   for Index:=0 to Node.Arguments.Count-1 do begin
    ArgumentTemporary:=-1;
    EmitExpression(Node.Arguments[Index],ArgumentTemporary);
    FunctionCallOperands[Index+1]:=CreateTemporaryOperand(ArgumentTemporary);
   end;
  end else  begin
   SetLength(FunctionCallOperands,1);
  end;
  if FunctionPointerTemporary>=0 then begin
   FunctionCallOperands[0]:=CreateTemporaryOperand(FunctionPointerTemporary);
  end else begin
   FunctionCallOperands[0]:=CreateFunctionOperand(Node);
  end;
  if OutputTemporary>=0 then begin
   OutputTemporaryOperand:=CreateTemporaryOperand(OutputTemporary);
  end else begin
   OutputTemporaryOperand.Kind:=pircokNONE;
  end;
  EmitInstruction(pircoCALL,CodeType,OutputTemporaryOperand,FunctionCallOperands,Node.SourceLocation);
 finally
  FunctionCallOperands:=nil;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitExpression(const Node:TPACCAbstractSyntaxTreeNode;var OutputTemporary:TPACCInt32);
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
    TPACCInstance(fInstance).AddError('Internal error 2017-01-21-14-56-0000',@Node.SourceLocation,true);
   end;

   astnkLVAR,
   astnkGVAR:begin
    EmitVariable(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node),OutputTemporary);
   end;

   astnkASSEMBLER:begin
   end;

   astnkFUNCCALL:begin
    EmitFunctionCall(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node),OutputTemporary);
   end;

   astnkFUNCPTR_CALL:begin
    EmitFunctionCall(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node),OutputTemporary);
   end;

   astnkFUNCDESG:begin
    EmitFUNCDESG(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node),OutputTemporary);
   end;

   astnkDECL:begin
    EmitDECL(TPACCAbstractSyntaxTreeNodeDeclaration(Node));
   end;

   astnkCONV:begin
    EmitCONV(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node),OutputTemporary);
   end;

   astnkADDR:begin
    EmitADDR(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node),OutputTemporary);
   end;

   astnkDEREF:begin
    EmitDEREF(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node),OutputTemporary);
   end;

   astnkTERNARY:begin
    EmitTERNARY(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node),OutputTemporary);
   end;

   astnkSTRUCT_REF:begin
    EmitSTRUCT_REF(TPACCAbstractSyntaxTreeNodeStructReference(Node),OutputTemporary);
   end;

   astnkOP_COMMA:begin
    EmitCOMMA(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node),OutputTemporary);
   end;

   astnkOP_ASSIGN:begin
    EmitAssign(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node),OutputTemporary);
   end;

   astnkOP_ASSIGN_OP:begin
    EmitAssignOp(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node),OutputTemporary);
   end;

   astnkOP_ASSIGN_SRC:begin
    EmitAssignSrc(Node,OutputTemporary);
   end;

   astnkOP_CAST:begin
    EmitCAST(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node),OutputTemporary);
   end;

   astnkOP_NOT:begin
    EmitNOT(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node),OutputTemporary);
   end;

   astnkOP_NEG:begin
    EmitNEG(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node),OutputTemporary);
   end;

   astnkOP_PRE_INC:begin
    if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     EmitLoadUnaryOpStore(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,Node,EmitUnaryOpINC,OutputTemporary,false);
    end else begin
     TPACCInstance(fInstance).AddError('Internal error 2017-01-21-15-01-0002',nil,true);
    end;
   end;

   astnkOP_PRE_DEC:begin
    if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     EmitLoadUnaryOpStore(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,Node,EmitUnaryOpDEC,OutputTemporary,false);
    end else begin
     TPACCInstance(fInstance).AddError('Internal error 2017-01-23-02-21-0000',nil,true);
    end;
   end;

   astnkOP_POST_INC:begin
    if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     EmitLoadUnaryOpStore(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,Node,EmitUnaryOpINC,OutputTemporary,true);
    end else begin
     TPACCInstance(fInstance).AddError('Internal error 2017-01-23-02-22-0000',nil,true);
    end;
   end;

   astnkOP_POST_DEC:begin
    if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     EmitLoadUnaryOpStore(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,Node,EmitUnaryOpDEC,OutputTemporary,true);
    end else begin
     TPACCInstance(fInstance).AddError('Internal error 2017-01-23-02-22-0001',nil,true);
    end;
   end;

   astnkOP_LABEL_ADDR:begin
    if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
     OutputTemporary:=CreateTemporary(pirctINT);
     EmitInstruction(pircoADDROFI,pirctINT,CreateTemporaryOperand(OutputTemporary),[CreateLabelOperand(TPACCAbstractSyntaxTreeNodeLabel(TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).Label_))],Node.SourceLocation);
    end else begin
     OutputTemporary:=CreateTemporary(pirctLONG);
     EmitInstruction(pircoADDROFL,pirctLONG,CreateTemporaryOperand(OutputTemporary),[CreateLabelOperand(TPACCAbstractSyntaxTreeNodeLabel(TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).Label_))],Node.SourceLocation);
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
    EmitBinaryOp(Node,EmitBinaryOpADD,OutputTemporary);
   end;

   astnkOP_SUB:begin
    EmitBinaryOp(Node,EmitBinaryOpSUB,OutputTemporary);
   end;

   astnkOP_MUL:begin
    EmitBinaryOp(Node,EmitBinaryOpMUL,OutputTemporary);
   end;

   astnkOP_DIV:begin
    EmitBinaryOp(Node,EmitBinaryOpDIV,OutputTemporary);
   end;

   astnkOP_MOD:begin
    EmitBinaryOp(Node,EmitBinaryOpMOD,OutputTemporary);
   end;

   astnkOP_AND:begin
    EmitBinaryOp(Node,EmitBinaryOpAND,OutputTemporary);
   end;

   astnkOP_OR:begin
    EmitBinaryOp(Node,EmitBinaryOpOR,OutputTemporary);
   end;

   astnkOP_XOR:begin
    EmitBinaryOp(Node,EmitBinaryOpXOR,OutputTemporary);
   end;

   astnkOP_SHL:begin
    EmitBinaryOp(Node,EmitBinaryOpSHL,OutputTemporary);
   end;

   astnkOP_SHR:begin
    EmitBinaryOp(Node,EmitBinaryOpSHR,OutputTemporary);
   end;

   astnkOP_SAR:begin
    EmitBinaryOp(Node,EmitBinaryOpSAR,OutputTemporary);
   end;

   astnkOP_LOG_AND:begin
    EmitLogicalANDOR(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node),OutputTemporary,true);
   end;

   astnkOP_LOG_OR:begin
    EmitLogicalANDOR(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node),OutputTemporary,false);
   end;

   astnkOP_LOG_NOT:begin
    EmitLogicalNOT(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node),OutputTemporary);
   end;

   astnkOP_EQ:begin
    EmitBinaryOp(Node,EmitBinaryOpEQ,OutputTemporary);
   end;

   astnkOP_NE:begin
    EmitBinaryOp(Node,EmitBinaryOpNE,OutputTemporary);
   end;

   astnkOP_GT:begin
    EmitBinaryOp(Node,EmitBinaryOpGT,OutputTemporary);
   end;

   astnkOP_LT:begin
    EmitBinaryOp(Node,EmitBinaryOpLT,OutputTemporary);
   end;

   astnkOP_GE:begin
    EmitBinaryOp(Node,EmitBinaryOpGE,OutputTemporary);
   end;

   astnkOP_LE:begin
    EmitBinaryOp(Node,EmitBinaryOpLE,OutputTemporary);
   end;

   else begin
    TPACCInstance(fInstance).AddError('Internal error 2017-01-24-13-48-0000',@Node.SourceLocation,true);
   end;

  end;

 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitFORStatement(const Node:TPACCAbstractSyntaxTreeNodeFORStatement);
var ConditionLabel,BodyLabel,ContinueLabel,BreakLabel:TPACCAbstractSyntaxTreeNodeLabel;
    ConditionBlock,BodyBlock,ContinueBlock,BreakBlock:TPACCIntermediateRepresentationCodeBlock;
    IgnoredTemporary,ConditionTemporary:TPACCInt32;
begin

 IgnoredTemporary:=-1;
 EmitExpression(Node.Initialization_,IgnoredTemporary);

 ConditionLabel:=NewHiddenLabel;
 BodyLabel:=NewHiddenLabel;
 ContinueLabel:=TPACCAbstractSyntaxTreeNodeLabel(Node.ContinueLabel);
 BreakLabel:=TPACCAbstractSyntaxTreeNodeLabel(Node.BreakLabel);
 ConditionBlock:=FindBlock(ConditionLabel);
 BodyBlock:=FindBlock(BodyLabel);
 ContinueBlock:=FindBlock(ContinueLabel);
 BreakBlock:=FindBlock(BreakLabel);

 EmitLabel(ConditionLabel);
 ConditionTemporary:=-1;
 EmitExpression(Node.Condition,ConditionTemporary);
 if (Node.Condition.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
    ((Node.Condition.Type_^.Kind=tkPOINTER) and
     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
  CurrentBlock.Jump.Kind:=pircjkJNZI;
 end else if (Node.Condition.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Condition.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  CurrentBlock.Jump.Kind:=pircjkJNZL;
 end else if Node.Condition.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  CurrentBlock.Jump.Kind:=pircjkJNZF;
 end else if Node.Condition.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  CurrentBlock.Jump.Kind:=pircjkJNZD;
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-14-39-0000',@Node.SourceLocation,true);
 end;
 CurrentBlock.Jump.Operand:=CreateTemporaryOperand(ConditionTemporary);
 CurrentBlock.Successors.Clear;
 CurrentBlock.Successors.Add(BodyBlock);
 CurrentBlock.Successors.Add(BreakBlock);
 CloseBlock;

 EmitLabel(BodyLabel);
 EmitStatement(Node.Body);
 EmitJump(ContinueLabel);

 EmitLabel(ContinueLabel);
 EmitExpression(Node.Step,IgnoredTemporary);
 EmitJump(ConditionLabel);

 EmitLabel(BreakLabel);

end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitWHILEOrDOStatement(const Node:TPACCAbstractSyntaxTreeNodeWHILEOrDOStatement;const IsWHILE:boolean);
var BodyLabel,ContinueLabel,BreakLabel:TPACCAbstractSyntaxTreeNodeLabel;
    BodyBlock,ContinueBlock,BreakBlock:TPACCIntermediateRepresentationCodeBlock;
    ConditionTemporary:TPACCInt32;
begin

 BodyLabel:=NewHiddenLabel;
 ContinueLabel:=TPACCAbstractSyntaxTreeNodeLabel(Node.ContinueLabel);
 BreakLabel:=TPACCAbstractSyntaxTreeNodeLabel(Node.BreakLabel);
 BodyBlock:=FindBlock(BodyLabel);
 ContinueBlock:=FindBlock(ContinueLabel);
 BreakBlock:=FindBlock(BreakLabel);

 if IsWHILE then begin
  EmitJump(ContinueLabel);
 end else begin
  EmitJump(BodyLabel);
 end;

 EmitLabel(BodyLabel);
 EmitStatement(Node.Body);

 EmitLabel(ContinueLabel);
 ConditionTemporary:=-1;
 EmitExpression(Node.Condition,ConditionTemporary);
 if (Node.Condition.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
    ((Node.Condition.Type_^.Kind=tkPOINTER) and
     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
  CurrentBlock.Jump.Kind:=pircjkJNZI;
 end else if (Node.Condition.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Condition.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  CurrentBlock.Jump.Kind:=pircjkJNZL;
 end else if Node.Condition.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  CurrentBlock.Jump.Kind:=pircjkJNZF;
 end else if Node.Condition.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  CurrentBlock.Jump.Kind:=pircjkJNZD;
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-14-39-0000',@Node.SourceLocation,true);
 end;
 CurrentBlock.Jump.Operand:=CreateTemporaryOperand(ConditionTemporary);
 CurrentBlock.Successors.Clear;
 CurrentBlock.Successors.Add(BodyBlock);
 CurrentBlock.Successors.Add(BreakBlock);
 CloseBlock;

 EmitLabel(BreakLabel);

end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitSWITCHStatement(const Node:TPACCAbstractSyntaxTreeNodeSWITCHStatement);
 procedure EmitJumpIfOp(const OpSI,OpUI,OpSL,OpUL:TPACCIntermediateRepresentationCodeOpcode;const ValueOperand:TPACCIntermediateRepresentationCodeOperand;const ComparsionValue:TPACCInt64;const TrueBlock,FalseBlock:TPACCIntermediateRepresentationCodeBlock);
 var ConditionTemporary:TPACCInt32;
     Opcode:TPACCIntermediateRepresentationCodeOpcode;
     JumpKind:TPACCIntermediateRepresentationCodeJumpKind;
     CodeType:TPACCIntermediateRepresentationCodeType;
 begin
  ConditionTemporary:=-1;
  case DataTypeToCodeType(Node.Value.Type_) of
   pirctINT:begin
    if tfUnsigned in Node.Value.Type_^.Flags then begin
     Opcode:=OpUI;
    end else begin
     Opcode:=OpSI;
    end;
    JumpKind:=pircjkJNZI;
    CodeType:=pirctINT;
   end;
   pirctLONG:begin
    if tfUnsigned in Node.Value.Type_^.Flags then begin
     Opcode:=OpUL;
    end else begin
     Opcode:=OpSL;
    end;
    JumpKind:=pircjkJNZL;
    CodeType:=pirctLONG;
   end;
   else begin
    Opcode:=pircoNONE;
    JumpKind:=pircjkNONE;
    CodeType:=pirctNONE;
    TPACCInstance(fInstance).AddError('Internal error 2017-01-24-15-56-0000',@Node.SourceLocation,true);
   end;
  end;
  EmitInstruction(Opcode,CodeType,CreateTemporaryOperand(ConditionTemporary),[ValueOperand,CreateIntegerValueOperand(ComparsionValue)],Node.SourceLocation);
  CurrentBlock.Jump.Kind:=JumpKind;
  CurrentBlock.Jump.Operand:=CreateTemporaryOperand(ConditionTemporary);
  CurrentBlock.Successors.Clear;
  CurrentBlock.Successors.Add(TrueBlock);
  CurrentBlock.Successors.Add(FalseBlock);
  CloseBlock;
 end;
var Index,SubIndex,ValueTemporary,OffsetedValueTemporary,JumpTableOffsetValueTemporary:TPACCInt32;
    SwitchBegin,SwitchEnd:TPACCInt64;
    StatementCase:PPACCAbstractSyntaxTreeNodeSWITCHStatementCase;
    SkipBodyLabel,BodyLabel,JumpTableLabel,DefaultOrSkipLabel,NextCheckLabel,NextCaseLabel:TPACCAbstractSyntaxTreeNodeLabel;
    SkipBodyBlock,BodyBlock,JumpTableBlock,DefaultOrSkipBlock,NextCheckBlock,NextCaseBlock:TPACCIntermediateRepresentationCodeBlock;
    JumpTableLabelBlocks:array of TPACCIntermediateRepresentationCodeBlock;
begin

 JumpTableLabelBlocks:=nil;
 try

  ValueTemporary:=-1;
  EmitExpression(Node.Value,ValueTemporary);

  SkipBodyLabel:=NewHiddenLabel;
  BodyLabel:=NewHiddenLabel;
  SkipBodyBlock:=FindBlock(SkipBodyLabel);
  BodyBlock:=FindBlock(BodyLabel);

  SwitchBegin:=High(TPACCInt64);
  SwitchEnd:=Low(TPACCInt64);

  for Index:=0 to length(Node.Cases)-1 do begin
   StatementCase:=@Node.Cases[Index];
   if Index=0 then begin
    SwitchBegin:=StatementCase^.CaseBegin;
    SwitchEnd:=StatementCase^.CaseEnd;
   end else begin
    SwitchBegin:=Min(SwitchBegin,StatementCase^.CaseBegin);
    SwitchEnd:=Max(SwitchEnd,StatementCase^.CaseEnd);
   end;
  end;

  if assigned(Node.DefaultCaseLabel) then begin
   DefaultOrSkipLabel:=TPACCAbstractSyntaxTreeNodeLabel(Node.DefaultCaseLabel);
   DefaultOrSkipBlock:=FindBlock(DefaultOrSkipLabel);
  end else begin
   DefaultOrSkipLabel:=SkipBodyLabel;
   DefaultOrSkipBlock:=SkipBodyBlock;
  end;

  if (length(Node.Cases)>=4) and ((SwitchEnd-SwitchBegin)<=1024) then begin

   JumpTableLabel:=NewHiddenLabel;
   JumpTableBlock:=FindBlock(JumpTableLabel);

   NextCheckLabel:=NewHiddenLabel;
   NextCheckBlock:=FindBlock(NextCheckLabel);

   EmitJumpIfOp(pircoCMPSLTI,pircoCMPULTI,pircoCMPSLTL,pircoCMPULTL,CreateTemporaryOperand(ValueTemporary),SwitchBegin,DefaultOrSkipBlock,NextCheckBlock);

   EmitLabel(NextCheckLabel);
   EmitJumpIfOp(pircoCMPSGTI,pircoCMPUGTI,pircoCMPSGTL,pircoCMPUGTL,CreateTemporaryOperand(ValueTemporary),SwitchEnd,DefaultOrSkipBlock,JumpTableBlock);

   // Jump table approach
   SetLength(JumpTableLabelBlocks,SwitchEnd-SwitchBegin);

   if assigned(Node.DefaultCaseLabel) then begin
    for Index:=0 to length(JumpTableLabelBlocks)-1 do begin
     JumpTableLabelBlocks[Index]:=DefaultOrSkipBlock;
    end;
   end;

   for Index:=0 to length(Node.Cases)-1 do begin
    StatementCase:=@Node.Cases[Index];
    for SubIndex:=StatementCase^.CaseBegin-SwitchBegin to (StatementCase^.CaseEnd-SwitchBegin)-1 do begin
     JumpTableLabelBlocks[SubIndex]:=FindBlock(TPACCAbstractSyntaxTreeNodeLabel(StatementCase^.CaseLabel));
    end;
   end;

   case DataTypeToCodeType(Node.Value.Type_) of
    pirctINT:begin
     OffsetedValueTemporary:=CreateTemporary(pirctINT);
     EmitInstruction(pircoSUBI,pirctINT,CreateTemporaryOperand(OffsetedValueTemporary),[CreateTemporaryOperand(ValueTemporary),CreateIntegerValueOperand(SwitchBegin)],Node.SourceLocation);
     JumpTableOffsetValueTemporary:=OffsetedValueTemporary;
    end;
    pirctLONG:begin
     OffsetedValueTemporary:=CreateTemporary(pirctLONG);
     EmitInstruction(pircoSUBL,pirctLONG,CreateTemporaryOperand(OffsetedValueTemporary),[CreateTemporaryOperand(ValueTemporary),CreateIntegerValueOperand(SwitchBegin)],Node.SourceLocation);
     JumpTableOffsetValueTemporary:=CreateTemporary(pirctINT);
     EmitInstruction(pircoTRLI,pirctINT,CreateTemporaryOperand(JumpTableOffsetValueTemporary),[CreateIntegerValueOperand(OffsetedValueTemporary)],Node.SourceLocation);
    end;
    else begin
     JumpTableOffsetValueTemporary:=-1;
     TPACCInstance(fInstance).AddError('Internal error 2017-01-24-15-48-0000',@Node.SourceLocation,true);
    end;
   end;

   EmitLabel(JumpTableLabel);
   EmitJumpTable(CreateTemporaryOperand(JumpTableOffsetValueTemporary),JumpTableLabelBlocks);

  end else begin

   // Multiple-branch-jumps approach

   for Index:=0 to length(Node.Cases)-1 do begin
    StatementCase:=@Node.Cases[Index];
    if StatementCase^.CaseBegin=StatementCase^.CaseEnd then begin
     NextCaseLabel:=NewHiddenLabel;
     NextCaseBlock:=FindBlock(NextCaseLabel);
     EmitJumpIfOp(pircoCMPEQI,pircoCMPEQI,pircoCMPEQL,pircoCMPEQL,CreateTemporaryOperand(ValueTemporary),StatementCase^.CaseBegin,FindBlock(TPACCAbstractSyntaxTreeNodeLabel(StatementCase^.CaseLabel)),NextCaseBlock);
     EmitLabel(NextCaseLabel);
    end else begin
     NextCaseLabel:=NewHiddenLabel;
     NextCaseBlock:=FindBlock(NextCaseLabel);
     NextCheckLabel:=NewHiddenLabel;
     NextCheckBlock:=FindBlock(NextCheckLabel);
     EmitJumpIfOp(pircoCMPSLTI,pircoCMPULTI,pircoCMPSLTL,pircoCMPULTL,CreateTemporaryOperand(ValueTemporary),StatementCase^.CaseBegin,NextCaseBlock,NextCheckBlock);
     EmitLabel(NextCheckLabel);
     EmitJumpIfOp(pircoCMPSGTI,pircoCMPUGTI,pircoCMPSGTL,pircoCMPUGTL,CreateTemporaryOperand(ValueTemporary),StatementCase^.CaseBegin,NextCaseBlock,FindBlock(TPACCAbstractSyntaxTreeNodeLabel(StatementCase^.CaseLabel)));
     EmitLabel(NextCaseLabel);
    end;
    for SubIndex:=StatementCase^.CaseBegin-SwitchBegin to (StatementCase^.CaseEnd-SwitchBegin)-1 do begin
     JumpTableLabelBlocks[SubIndex]:=FindBlock(TPACCAbstractSyntaxTreeNodeLabel(StatementCase^.CaseLabel));
    end;
   end;

   EmitJump(DefaultOrSkipLabel);

  end;

  EmitLabel(BodyLabel);
  EmitStatement(Node.Body);
  EmitJump(SkipBodyLabel);
  EmitLabel(SkipBodyLabel);

 finally
  JumpTableLabelBlocks:=nil;
 end;

end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitIFStatement(const Node:TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator);
var l0,l1,l2:TPACCAbstractSyntaxTreeNodeLabel;
    b0,b1,b2:TPACCIntermediateRepresentationCodeBlock;
    ConditionTemporary:TPACCInt32;
begin

 l0:=NewHiddenLabel;
 l1:=NewHiddenLabel;
 l2:=NewHiddenLabel;
 b0:=FindBlock(l0);
 b1:=FindBlock(l1);
 b2:=FindBlock(l2);

 ConditionTemporary:=-1;
 EmitExpression(Node.Condition,ConditionTemporary);
 if (Node.Condition.Type_^.Kind in PACCIntermediateRepresentationCodeINTTypeKinds) or
    ((Node.Condition.Type_^.Kind=tkPOINTER) and
     (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt)) then begin
  CurrentBlock.Jump.Kind:=pircjkJNZI;
 end else if (Node.Condition.Type_^.Kind in PACCIntermediateRepresentationCodeLONGTypeKinds) or
             ((Node.Condition.Type_^.Kind=tkPOINTER) and
              (TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfLong)) then begin
  CurrentBlock.Jump.Kind:=pircjkJNZL;
 end else if Node.Condition.Type_^.Kind in PACCIntermediateRepresentationCodeFLOATTypeKinds then begin
  CurrentBlock.Jump.Kind:=pircjkJNZF;
 end else if Node.Condition.Type_^.Kind in PACCIntermediateRepresentationCodeDOUBLETypeKinds then begin
  CurrentBlock.Jump.Kind:=pircjkJNZD;
 end else begin
  TPACCInstance(fInstance).AddError('Internal error 2017-01-24-13-52-0000',@Node.SourceLocation,true);
 end;
 CurrentBlock.Jump.Operand:=CreateTemporaryOperand(ConditionTemporary);
 CurrentBlock.Successors.Clear;
 CurrentBlock.Successors.Add(b0);
 CurrentBlock.Successors.Add(b1);
 CloseBlock;

 EmitLabel(l0);
 EmitStatement(Node.Then_);
 EmitJump(l2);

 EmitLabel(l1);
 EmitStatement(Node.Else_);
 EmitJump(l2);

 EmitLabel(l2);

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
    EmitFORStatement(TPACCAbstractSyntaxTreeNodeFORStatement(Node));
   end;
   astnkDO:begin
    EmitWHILEOrDOStatement(TPACCAbstractSyntaxTreeNodeWHILEOrDOStatement(Node),false);
   end;
   astnkWHILE:begin
    EmitWHILEOrDOStatement(TPACCAbstractSyntaxTreeNodeWHILEOrDOStatement(Node),true);
   end;
   astnkSWITCH:begin
    EmitSWITCHStatement(TPACCAbstractSyntaxTreeNodeSWITCHStatement(Node));
   end;
   astnkIF:begin
    EmitIFStatement(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node));
   end;
   astnkBREAK:begin
    EmitJump(TPACCAbstractSyntaxTreeNodeLabel(TPACCAbstractSyntaxTreeNodeBREAKOrCONTINUEStatement(Node).Label_));
   end;
   astnkCONTINUE:begin
    EmitJump(TPACCAbstractSyntaxTreeNodeLabel(TPACCAbstractSyntaxTreeNodeBREAKOrCONTINUEStatement(Node).Label_));
   end;
   else begin
    EmitExpression(Node,TemporaryA);
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

procedure TPACCIntermediateRepresentationCodeFunction.DeleteBlock(const Block:TPACCIntermediateRepresentationCodeBlock);
var Index,SubIndex,SubSubIndex:TPACCInt32;
    Successor:TPACCIntermediateRepresentationCodeBlock;
    Phi:TPACCIntermediateRepresentationCodePhi;
    AlreadySeenHashMap:TPACCPointerHashMap;
    Successors:TPACCIntermediateRepresentationCodeBlockList;
begin
 Successors:=TPACCIntermediateRepresentationCodeBlockList.Create;
 try
  for Index:=0 to Block.Successors.Count-1 do begin
   Successors.Add(Block.Successors[Index]);
  end;
  AlreadySeenHashMap:=TPACCPointerHashMap.Create;
  try
   for Index:=0 to Successors.Count-1 do begin
    Successor:=Successors[Index];
    if assigned(Successor) and not assigned(AlreadySeenHashMap[Successor]) then begin
     AlreadySeenHashMap[Successor]:=Successor;
     Phi:=Successor.Phi;
     while assigned(Phi) do begin
      for SubIndex:=Phi.CountOperands-1 downto 0 do begin
       if Phi.Blocks[SubIndex]=Block then begin
        for SubSubIndex:=SubIndex+1 to Phi.CountOperands-1 do begin
         Phi.Operands[SubSubIndex-1]:=Phi.Operands[SubSubIndex];
         Phi.Blocks[SubSubIndex-1]:=Phi.Blocks[SubSubIndex];
        end;
        dec(Phi.CountOperands);
        SetLength(Phi.Operands,Phi.CountOperands);
        SetLength(Phi.Blocks,Phi.CountOperands);
       end;
      end;
      Phi:=Phi.Link;
     end;
     if Successor.Predecessors.Count>0 then begin
      for SubIndex:=Successor.Predecessors.Count-1 downto 0 do begin
       if Successor.Predecessors[SubIndex]=Block then begin
        Successor.Predecessors.Delete(SubIndex);
       end;
      end;
     end;
    end;
   end;
  finally
   AlreadySeenHashMap.Free;
  end;
 finally
  Successors.Free;
 end;
end;

function CompareTPACCIntermediateRepresentationCodeFunctionFillRPORPORecSuccessors(a,b:pointer):TPACCInt32;
begin
 if assigned(a) and assigned(b) then begin
  result:=TPACCIntermediateRepresentationCodeBlock(a).Loop-TPACCIntermediateRepresentationCodeBlock(b).Loop;
 end else if assigned(a) then begin
  result:=-1;
 end else if assigned(b) then begin
  result:=1;
 end else begin
  result:=0;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.FillRPO;
 function RPORec(const Block:TPACCIntermediateRepresentationCodeBlock;const x:TPACCInt32):TPACCInt32;
 var Index:TPACCInt32;
     Successors:TPACCIntermediateRepresentationCodeBlockList;
 begin
  result:=x;
  if assigned(Block) and (Block.ID<0) then begin
   Block.ID:=1;
   case Block.Successors.Count of
    1:begin
     if assigned(Block.Successors[0]) then begin
      result:=RPORec(Block.Successors[0],result);
     end;
    end;
    2:begin
     if assigned(Block.Successors[0]) and assigned(Block.Successors[1]) then begin
      if Block.Successors[0].Loop>Block.Successors[1].Loop then begin
       result:=RPORec(Block.Successors[1],result);
       result:=RPORec(Block.Successors[0],result);
      end else begin
       result:=RPORec(Block.Successors[0],result);
       result:=RPORec(Block.Successors[1],result);
      end;
     end else if assigned(Block.Successors[0]) then begin
      result:=RPORec(Block.Successors[0],result);
     end else if assigned(Block.Successors[1]) then begin
      result:=RPORec(Block.Successors[1],result);
     end;
    end;
    4..$7fffffff:begin
     Successors:=TPACCIntermediateRepresentationCodeBlockList.Create;
     try
      for Index:=0 to Block.Successors.Count-1 do begin
       Successors.Add(Block.Successors[Index]);
      end;
      Successors.Sort(CompareTPACCIntermediateRepresentationCodeFunctionFillRPORPORecSuccessors);
      for Index:=0 to Successors.Count-1 do begin
       if assigned(Successors[Index]) then begin
        result:=RPORec(Successors[Index],result);
       end;
      end;
     finally
      Successors.Free;
     end;
    end;
   end;
   Block.ID:=result;
   Assert(result>=0);
   dec(result);
  end;
 end;
var Count,CountRPO:TPACCInt32;
    Block:TPACCIntermediateRepresentationCodeBlock;
    Phi:PPACCIntermediateRepresentationCodeBlock;
begin
 Block:=StartBlock;
 while assigned(Block) do begin
  Block.ID:=-1;
  Block:=Block.Link;
 end;
 Count:=1+RPORec(StartBlock,CountBlocks-1);
 dec(CountBlocks,Count);
 Phi:=@StartBlock;
 CountRPO:=0;
 try
  repeat
   Block:=Phi^;
   if assigned(Block) then begin
    if Block.ID<0 then begin
     DeleteBlock(Block);
     Phi^:=Block.Link;
    end else begin
     dec(Block.ID,Count);
     CountRPO:=Max(CountRPO,Block.ID+1);
     if length(RPO)<=Block.ID then begin
      SetLength(RPO,(Block.ID+1)*2);
     end;
     RPO[Block.ID]:=Block;
     Phi:=@Block.link;
    end;
   end else begin
    break;
   end;
  until false;
 finally
  SetLength(RPO,CountRPO);
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.FillPredecessors;
var Index:TPACCInt32;
    Block,Successor:TPACCIntermediateRepresentationCodeBlock;
    AlreadySeenHashMap:TPACCPointerHashMap;
begin

 Block:=StartBlock;
 while assigned(Block) do begin
  Block.Predecessors.Clear;
  Block.Visit:=0;
  Block:=Block.Link;
 end;

 Block:=StartBlock;
 while assigned(Block) do begin
  AlreadySeenHashMap:=TPACCPointerHashMap.Create;
  try
   for Index:=0 to Block.Successors.Count-1 do begin
    Successor:=Block.Successors[Index];
    if assigned(Successor) and not assigned(AlreadySeenHashMap[Successor]) then begin
     AlreadySeenHashMap[Successor]:=Successor;
     Successor.Predecessors.Add(nil);
    end;
   end;
  finally
   AlreadySeenHashMap.Free;
  end;
  Block:=Block.Link;
 end;

 Block:=StartBlock;
 while assigned(Block) do begin
  AlreadySeenHashMap:=TPACCPointerHashMap.Create;
  try
   for Index:=0 to Block.Successors.Count-1 do begin
    Successor:=Block.Successors[Index];
    if assigned(Successor) and not assigned(AlreadySeenHashMap[Successor]) then begin
     AlreadySeenHashMap[Successor]:=Successor;
     Successor.Predecessors[Successor.Visit]:=Block;
     inc(Successor.Visit);
    end;
   end;
  finally
   AlreadySeenHashMap.Free;
  end;
  Block:=Block.Link;
 end;

end;

procedure TPACCIntermediateRepresentationCodeFunction.FillUse;
var Index,SubIndex:TPACCInt32;
    Block:TPACCIntermediateRepresentationCodeBlock;
    Phi:TPACCIntermediateRepresentationCodePhi;
    Instruction:TPACCIntermediateRepresentationCodeInstruction;
    Temporary:TPACCIntermediateRepresentationCodeTemporary;
    Use:TPACCIntermediateRepresentationCodeUse;
begin

 for Index:=0 to Temporaries.Count-1 do begin
  Temporary:=Temporaries[Index];
  Temporary.CountDefinitions:=0;
  Temporary.Uses_.Clear;
  Temporary.Phi:=0;
  Temporary.Type_:=pirctNONE;
 end;

 Block:=StartBlock;
 while assigned(Block) do begin

  Phi:=Block.Phi;
  while assigned(Phi) do begin
   if Phi.To_.Kind=pircokTEMPORARY then begin
    Temporary:=Temporaries[Phi.To_.Temporary];
    inc(Temporary.CountDefinitions);
    Temporary.Type_:=Phi.Type_;
    Temporary.Phi:=Phi.To_.Temporary;
    for Index:=0 to Phi.CountOperands-1 do begin
     if Phi.Operands[Index].Kind=pircokTEMPORARY then begin
      Temporary:=Temporaries[Phi.Operands[Index].Temporary];
      Use:=TPACCIntermediateRepresentationCodeUse.Create;
      TPACCInstance(fInstance).AllocatedObjects.Add(Use);
      Temporary.Uses_.Add(Use);
      Use.Kind:=pircukPHI;
      Use.BlockID:=Block.ID;
      Use.By.Phi:=Phi;
      if Temporary.Phi=0 then begin
       Temporary.Phi:=Phi.To_.Temporary;
      end;
     end;
    end;
   end else begin
    TPACCInstance(fInstance).AddError('Internal error 2017-01-25-13-53-0000',nil,true);
   end;
   Phi:=Phi.Link;
  end;

  for Index:=0 to Block.Instructions.Count-1 do begin

   Instruction:=Block.Instructions[Index];
   if Instruction.To_.Kind<>pircokNONE then begin
    if Instruction.To_.Kind=pircokTEMPORARY then begin
     Temporary:=Temporaries[Instruction.To_.Temporary];
     if Instruction.Type_<>pirctNONE then begin
      Temporary.Type_:=Instruction.Type_;
     end;
     inc(Temporary.CountDefinitions);
//   writeln(Temporary.CountDefinitions,' ',Instruction.To_.Temporary,' ',TPACCPtrUInt(Temporary.Variable));
    end else begin
     TPACCInstance(fInstance).AddError('Internal error 2017-01-25-14-01-0000',nil,true);
    end;
   end;

   for SubIndex:=0 to length(Instruction.Operands)-1 do begin
    if Instruction.Operands[SubIndex].Kind=pircokTEMPORARY then begin
     Temporary:=Temporaries[Instruction.Operands[SubIndex].Temporary];
     Use:=TPACCIntermediateRepresentationCodeUse.Create;
     TPACCInstance(fInstance).AllocatedObjects.Add(Use);
     Temporary.Uses_.Add(Use);
     Use.Kind:=pircukINS;
     Use.BlockID:=Block.ID;
     Use.By.Instruction:=Instruction;
    end;
   end;

   if Block.Jump.Operand.Kind=pircokTEMPORARY then begin
    Temporary:=Temporaries[Block.Jump.Operand.Temporary];
    Use:=TPACCIntermediateRepresentationCodeUse.Create;
    TPACCInstance(fInstance).AllocatedObjects.Add(Use);
    Temporary.Uses_.Add(Use);
    Use.Kind:=pircukJMP;
    Use.BlockID:=Block.ID;
   end;

  end;

  Block:=Block.Link;
 end;

end;

procedure TPACCIntermediateRepresentationCodeFunction.LiveOn(var BitSet:TPACCIntermediateRepresentationCodeBitSet;const Block,Successor:TPACCIntermediateRepresentationCodeBlock);
var Index:TPACCInt32;
    Phi:TPACCIntermediateRepresentationCodePhi;
begin

 BitSet.Assign(Successor.In_);

 Phi:=Successor.Phi;
 while assigned(Phi) do begin
  if Phi.To_.Kind=pircokTEMPORARY then begin
   BitSet.SetBit(Phi.To_.Temporary,false);
  end;
  Phi:=Phi.Link;
 end;

 Phi:=Successor.Phi;
 while assigned(Phi) do begin
  for Index:=0 to Phi.CountOperands-1 do begin
   if (Phi.Blocks[Index]=Block) and (Phi.Operands[Index].Kind=pircokTEMPORARY) then begin
    BitSet.SetBit(Phi.Operands[Index].Temporary,true);
    Block.Gen_.SetBit(Phi.Operands[Index].Temporary,true);
   end;
  end;
  Phi:=Phi.Link;
 end;

end;

function TPACCIntermediateRepresentationCodeFunction.ReturnRegisters(const Operand:TPACCIntermediateRepresentationCodeOperand;var CountLiveInt,CountLiveFloat:TPACCInt32):TPACCIntermediateRepresentationCodeBitSet;
begin
 result.Clear;
end;

function TPACCIntermediateRepresentationCodeFunction.ArgumentRegisters(const Operand:TPACCIntermediateRepresentationCodeOperand;var CountLiveInt,CountLiveFloat:TPACCInt32):TPACCIntermediateRepresentationCodeBitSet;
begin
 result.Clear;
end;

function TPACCIntermediateRepresentationCodeFunction.IntegerRegisterToSave(const Operand:TPACCIntermediateRepresentationCodeOperand):TPACCInt32;
begin
 result:=0;
end;

function TPACCIntermediateRepresentationCodeFunction.FloatRegisterToSave(const Operand:TPACCIntermediateRepresentationCodeOperand):TPACCInt32;
begin
 result:=0;
end;

procedure TPACCIntermediateRepresentationCodeFunction.FillLive;
var Phis:array of TPACCInt32;
    CountLive,TemporaryCountLive:array[0..1] of TPACCInt32;
 function PhiTmp(const t:TPACCInt32):TPACCInt32;
 begin
  result:=Temporaries[t].Phi;
  if result=0 then begin
   result:=t;
  end;
 end;
 procedure PhiFix(const t1:TPACCInt32);
 var t,t2:TPACCInt32;
 begin
  t:=PhiTmp(t1);
  t2:=Phis[t];
  if (t2<>0) and (t1<>t2) then begin
   if t<>t1 then begin
    Temporaries[t1].Phi:=t1;
    t:=t1;
   end else begin
    Temporaries[t2].Phi:=t2;
    Phis[t2]:=t2;
   end;
  end;
  Phis[t]:=t1;
 end;
 procedure BSet(const Operand:TPACCIntermediateRepresentationCodeOperand;b:TPACCIntermediateRepresentationCodeBlock);
 begin
  if Operand.Kind=pircokTEMPORARY then begin
   b.Gen_.SetBit(Operand.Temporary,true);
   PhiFix(Operand.Temporary);
   if not b.In_.GetBit(Operand.Temporary) then begin
    inc(CountLive[CodeTypeBaseClass[Temporaries[Operand.Temporary].Type_]]);
    b.In_.SetBit(Operand.Temporary,true);
   end;
  end;
 end;
var Index,SubIndex,OtherIndex,TemporaryIndex:TPACCInt32;
    Block,Successor:TPACCIntermediateRepresentationCodeBlock;
    Instruction:TPACCIntermediateRepresentationCodeInstruction;
    TemporaryBitSet0,TemporaryBitSet1:TPACCIntermediateRepresentationCodeBitSet;
    Changed:boolean;
begin

 Phis:=nil;
 try

  TemporaryBitSet0.BitmapSize:=Temporaries.Count;
  TemporaryBitSet1.BitmapSize:=Temporaries.Count;
  TemporaryBitSet0.ClearBits;
  TemporaryBitSet1.ClearBits;

  Block:=StartBlock;
  while assigned(Block) do begin
   Block.In_.BitmapSize:=Temporaries.Count;
   Block.Out_.BitmapSize:=Temporaries.Count;
   Block.Gen_.BitmapSize:=Temporaries.Count;
   Block.In_.ClearBits;
   Block.Out_.ClearBits;
   Block.Gen_.ClearBits;
   Block:=Block.Link;
  end;

  SetLength(Phis,Temporaries.Count);

  Changed:=true;
  repeat

   for Index:=CountBlocks-1 downto 0 do begin
    Block:=RPO[Index];
    TemporaryBitSet0.Assign(Block.Out_);
    for SubIndex:=0 to Block.Successors.Count-1 do begin
     Successor:=Block.Successors[SubIndex];
     if assigned(Successor) then begin
      LiveOn(TemporaryBitSet1,Block,Successor);
      Block.Out_.Union(TemporaryBitSet1);
     end;
    end;
   end;

   Changed:=Changed or not Block.Out_.EqualsTo(TemporaryBitSet0);

   FillChar(Phis[0],length(Phis)*SizeOf(TPACCInt32),#0);
   FillChar(CountLive[0],length(CountLive)*SizeOf(TPACCInt32),#0);
   Block.In_.Assign(Block.Out_);
   Index:=-1;
   while Block.In_.IterateToNextBit(Index) do begin
    PhiFix(Index);
    inc(CountLive[CodeTypeBaseClass[Temporaries[Index].Type_]]);
   end;

   if Block.Jump.Operand.Kind=pircokCALL then begin
    if (Block.In_.Count=0) and (CountLive[0]=0) and (CountLive[1]=0) then begin
     Block.In_.Union(ReturnRegisters(Block.Jump.Operand,CountLive[0],CountLive[1]));
    end else begin
     TPACCInstance(fInstance).AddError('Internal error 2017-01-25-16-45-0000',nil,true);
    end;
   end else begin
    BSet(Block.Jump.Operand,Block);
   end;

   Block.CountLive[0]:=CountLive[0];
   Block.CountLive[1]:=CountLive[1];

   for Index:=Block.Instructions.Count-1 downto 0 do begin
    Instruction:=Block.Instructions[Index];
    if (Instruction.Opcode=pircoCALL) and (length(Instruction.Operands)>0) and (Instruction.Operands[0].Kind=pircokCALL) then begin
     TemporaryCountLive[0]:=0;
     TemporaryCountLive[1]:=0;
     Block.In_.Subtraction(ReturnRegisters(Block.Jump.Operand,TemporaryCountLive[0],TemporaryCountLive[1]));
     for OtherIndex:=0 to 1 do begin
      dec(CountLive[OtherIndex],TemporaryCountLive[OtherIndex]);
     end;
     if (CountLive[0]+IntegerRegisterToSave(Block.Jump.Operand))>Block.CountLive[0] then begin
      Block.CountLive[0]:=CountLive[0]+IntegerRegisterToSave(Block.Jump.Operand);
     end;
     if (CountLive[1]+FloatRegisterToSave(Block.Jump.Operand))>Block.CountLive[1] then begin
      Block.CountLive[1]:=CountLive[1]+FloatRegisterToSave(Block.Jump.Operand);
     end;
     Block.In_.Union(ArgumentRegisters(Block.Jump.Operand,TemporaryCountLive[0],TemporaryCountLive[1]));
     for OtherIndex:=0 to 1 do begin
      inc(CountLive[OtherIndex],TemporaryCountLive[OtherIndex]);
     end;
    end;
    if Instruction.To_.Kind<>pircokNONE then begin
     if Instruction.To_.Kind=pircokTEMPORARY then begin
      TemporaryIndex:=Instruction.To_.Temporary;
      if Block.In_.GetBit(TemporaryIndex) then begin
       dec(CountLive[CodeTypeBaseClass[Temporaries[TemporaryIndex].Type_]]);
      end;
      Block.Gen_.SetBit(TemporaryIndex,true);
      Block.In_.SetBit(TemporaryIndex,false);
      Phis[PhiTmp(TemporaryIndex)]:=0;
     end else begin
      TPACCInstance(fInstance).AddError('Internal error 2017-01-25-17-34-0000',nil,true);
     end;
    end;
    for OtherIndex:=0 to length(Instruction.Operands)-1 do begin
     case Instruction.Operands[OtherIndex].Kind of
      pircokNONE:begin
      end;
      else begin
       BSet(Instruction.Operands[OtherIndex],Block);
      end;
     end;
    end;
    for OtherIndex:=0 to 1 do begin
     Block.CountLive[OtherIndex]:=max(Block.CountLive[OtherIndex],CountLive[OtherIndex]);
    end;
   end;

   if Changed then begin
    Changed:=false;
    continue;
   end else begin
    break;
   end;
  until false;

 finally
  TemporaryBitSet0.Clear;
  TemporaryBitSet1.Clear;
  Phis:=nil;
 end;

end;

function TPACCIntermediateRepresentationCodeFunction.CompareSDominance(Block,OtherBlock:TPACCIntermediateRepresentationCodeBlock):boolean;
begin
 if assigned(Block) and assigned(OtherBlock) then begin
  if Block=OtherBlock then begin
   result:=false;
  end else begin
   while Block.ID<OtherBlock.ID do begin
    OtherBlock:=OtherBlock.InterDominance;
   end;
   result:=Block=OtherBlock;
  end;
 end else begin
  result:=false;
  TPACCInstance(fInstance).AddError('Internal error 2017-01-25-15-31-0000',nil,true);
 end;
end;

function TPACCIntermediateRepresentationCodeFunction.CompareDominance(Block,OtherBlock:TPACCIntermediateRepresentationCodeBlock):boolean;
begin
 result:=(Block=OtherBlock) or CompareSDominance(Block,OtherBlock);
end;

function TPACCIntermediateRepresentationCodeFunction.CodeTypeMerge(var ResultType_:TPACCIntermediateRepresentationCodeType;const Type_:TPACCIntermediateRepresentationCodeType):boolean;
begin
 if ResultType_ in [pirctNONE,pirctTOP] then begin
  ResultType_:=Type_;
  result:=false;
 end else if ((ResultType_=pirctINT) and (Type_=pirctLONG)) or ((ResultType_=pirctLONG) and (Type_=pirctINT)) then begin
  ResultType_:=pirctINT;
  result:=false;
 end else begin
  result:=ResultType_<>Type_;
 end;
end;

procedure TPACCIntermediateRepresentationCodeFunction.SSA;
 procedure FillDominators;
  function FindInterDominance(Block,OtherBlock:TPACCIntermediateRepresentationCodeBlock):TPACCIntermediateRepresentationCodeBlock;
  var TemporaryBlock:TPACCIntermediateRepresentationCodeBlock;
  begin
   if assigned(Block) then begin
    while Block<>OtherBlock do begin
     if Block.ID<OtherBlock.ID then begin
      TemporaryBlock:=Block;
      Block:=OtherBlock;
      OtherBlock:=TemporaryBlock;
     end;
     while Block.ID>OtherBlock.ID do begin
      Block:=Block.InterDominance;
      if not assigned(Block) then begin
       TPACCInstance(fInstance).AddError('Internal error 2017-01-25-15-06-0000',nil,true);
      end;
     end;
    end;
    result:=Block;
   end else begin
    result:=OtherBlock;
   end;
  end;
 var Index,SubIndex:TPACCInt32;
     Block,DominanceBlock:TPACCIntermediateRepresentationCodeBlock;
     Changed:boolean;
 begin
  Block:=StartBlock;
  while assigned(Block) do begin
   Block.InterDominance:=nil;
   Block.Dominance:=nil;
   Block.DominanceLink:=nil;
   Block:=Block.Link;
  end;
  repeat
   Changed:=false;
   for Index:=1 to CountBlocks-1 do begin
    Block:=RPO[Index];
    DominanceBlock:=nil;
    for SubIndex:=0 to Block.Predecessors.Count-1 do begin
     if assigned(Block.Predecessors[SubIndex].InterDominance) or (Block.Predecessors[SubIndex]=StartBlock) then begin
      DominanceBlock:=FindInterDominance(DominanceBlock,Block.Predecessors[SubIndex]);
     end;
    end;
    if Block.InterDominance<>DominanceBlock then begin
     Block.InterDominance:=DominanceBlock;
     Changed:=true;
    end;
   end;
  until not Changed;
  Block:=StartBlock;
  while assigned(Block) do begin
   DominanceBlock:=Block.InterDominance;
   if assigned(DominanceBlock) then begin
    if DominanceBlock=Block then begin
     TPACCInstance(fInstance).AddError('Internal error 2017-01-25-15-14-0000',nil,true);
    end else begin
     Block.DominanceLink:=DominanceBlock.Dominance;
     DominanceBlock.Dominance:=Block;
    end;
   end;
   Block:=Block.Link;
  end;
 end;
 procedure FillDominanceFrontier;
  procedure AddFrontier(const Block,OtherBlock:TPACCIntermediateRepresentationCodeBlock);
  var Index:TPACCInt32;
  begin
   for Index:=0 to Block.Frontiers.Count-1 do begin
    if Block.Frontiers[Index]=OtherBlock then begin
     exit;
    end;
   end;
   Block.Frontiers.Add(OtherBlock);
  end;
 var Index:TPACCInt32;
     Block,OtherBlock,Successor:TPACCIntermediateRepresentationCodeBlock;
 begin
  Block:=StartBlock;
  while assigned(Block) do begin
   for Index:=0 to Block.Successors.Count-1 do begin
    Successor:=Block.Successors[Index];
    if assigned(Successor) then begin
     OtherBlock:=Block;
     while not CompareSDominance(OtherBlock,Successor) do begin
      AddFrontier(OtherBlock,Successor);
      OtherBlock:=OtherBlock.InterDominance;
     end;
    end;
   end;
   Block:=Block.Link;
  end;
 end;
 procedure FillMissingPhiInstructions;
 var TemporaryIndex,InstructionIndex,InstructionOperandIndex,Index:TPACCInt32;
     TemporaryBitSet,DefinedBlockBitSet:TPACCIntermediateRepresentationCodeBitSet;
     BlockStack:TPACCIntermediateRepresentationCodeBlockList;
     Block,Frontier:TPACCIntermediateRepresentationCodeBlock;
     Temporary:TPACCIntermediateRepresentationCodeTemporary;
     Operand:TPACCIntermediateRepresentationCodeOperand;
     Instruction:TPACCIntermediateRepresentationCodeInstruction;
     CodeType:TPACCIntermediateRepresentationCodeType;
     Phi:TPACCIntermediateRepresentationCodePhi;
 begin

  TemporaryBitSet.BitmapSize:=CountBlocks;
  DefinedBlockBitSet.BitmapSize:=CountBlocks;
  TemporaryBitSet.ClearBits;
  DefinedBlockBitSet.ClearBits;
  BlockStack:=TPACCIntermediateRepresentationCodeBlockList.Create;
  try

   for TemporaryIndex:=0 to Temporaries.Count-1 do begin

    Temporary:=Temporaries[TemporaryIndex];

    Temporary.Visit:=0;

    if Temporary.Phi=0 then begin

     TemporaryBitSet.ClearBits;
     CodeType:=pirctNONE;

     Block:=StartBlock;
     while assigned(Block) do begin
      Block.Visit:=0;
      Operand:=EmptyOperand;
      for InstructionIndex:=0 to Block.Instructions.Count-1 do begin
       Instruction:=Block.Instructions[InstructionIndex];
       if Operand.Kind<>pircokNONE then begin
        for InstructionOperandIndex:=0 to length(Instruction.Operands)-1 do begin
         if (Instruction.Operands[InstructionOperandIndex].Kind=pircokTEMPORARY) and
            (Instruction.Operands[InstructionOperandIndex].Temporary=TemporaryIndex) then begin
          Instruction.Operands[InstructionOperandIndex]:=Operand;
         end;
        end;
       end;
       if (Instruction.To_.Kind=pircokTEMPORARY) and (Instruction.To_.Temporary=TemporaryIndex) then begin
        if Block.Out_.GetBit(TemporaryIndex) then begin
         if not TemporaryBitSet.GetBit(Block.ID) then begin
          TemporaryBitSet.SetBit(Block.ID,true);
          BlockStack.Add(Block);
         end;
         if CodeTypeMerge(CodeType,Instruction.Type_) then begin
          TPACCInstance(fInstance).AddError('Internal error 2017-01-26-11-40-0000',nil,true);
         end;
        end else begin
         if Temporary.CountDefinitions=1 then begin
          Operand:=CreateTemporaryOperand(TemporaryIndex);
         end else begin
          Operand:=CreateTemporaryOperand(CreateLinkTemporary(TemporaryIndex));
         end;
         Instruction.To_:=Operand;
        end;
       end;
      end;
      if (Operand.Kind<>pircokNONE) and (Block.Jump.Operand.Kind=pircokTEMPORARY) and (Block.Jump.Operand.Temporary=TemporaryIndex) then begin
       Block.Jump.Operand:=Operand;
      end;
      Block:=Block.Link;
     end;

     DefinedBlockBitSet.Assign(TemporaryBitSet);

     while BlockStack.Count>0 do begin
      Block:=BlockStack[BlockStack.Count-1];
      BlockStack.Delete(BlockStack.Count-1);
      Temporary.Visit:=TemporaryIndex;
      TemporaryBitSet.SetBit(Block.ID,false);
      for Index:=0 to Block.Frontiers.Count-1 do begin
       Frontier:=Block.Frontiers[Index];
       if Frontier.Visit=0 then begin
        inc(Frontier.Visit);
        if Frontier.In_.GetBit(TemporaryIndex) then begin
         Phi:=TPACCIntermediateRepresentationCodePhi.Create;
         TPACCInstance(fInstance).AllocatedObjects.Add(Phi);
         Phi.Type_:=CodeType;
         Phi.To_:=CreateTemporaryOperand(TemporaryIndex);
         Phi.Link:=Frontier.Phi;
         Frontier.Phi:=Phi;
         if not (DefinedBlockBitSet.GetBit(Frontier.ID) or TemporaryBitSet.GetBit(Frontier.ID)) then begin
          TemporaryBitSet.SetBit(Frontier.ID,true);
          BlockStack.Add(Frontier);
         end;
        end;
       end else begin
        inc(Frontier.Visit);
       end;
      end;
     end;

    end;

   end;


  finally
   TemporaryBitSet.Clear;
   DefinedBlockBitSet.Clear;
   BlockStack.Free;
  end;

 end;
 procedure Rename;
 type PStackItem=^TStackItem;
      TStackItem=record
       Up:PStackItem;
       Operand:TPACCIntermediateRepresentationCodeOperand;
       Block:TPACCIntermediateRepresentationCodeBlock;
      end;
      TStackItems=array of PStackItem;
 var StackItems:TStackItems;
     LastStackItem:PStackItem;
     AllocatedStackItems:TList;
  procedure ProcessBlock(const Block:TPACCIntermediateRepresentationCodeBlock);
   function NewStackItem(const Operand:TPACCIntermediateRepresentationCodeOperand;const Up:PStackItem):PStackItem;
   begin
    result:=LastStackItem;
    if assigned(result) then begin
     LastStackItem:=result^.Up;
    end else begin
     GetMem(result,SizeOf(TStackItem));
     FillChar(result^,SizeOf(TStackItem),#0);
     AllocatedStackItems.Add(result);
    end;
    result^.Up:=Up;
    result^.Operand:=Operand;
    result^.Block:=Block;
   end;
   procedure FreeStackItem(const StackItem:PStackItem);
   begin
    StackItem^.Up:=LastStackItem;
    LastStackItem:=StackItem;
   end;
   function GetStack(const TemporaryIndex:TPACCInt32):TPACCIntermediateRepresentationCodeOperand;
   var StackItem,OldStackItem:PStackItem;
   begin
    StackItem:=StackItems[TemporaryIndex];
    while assigned(StackItem) and not CompareDominance(StackItem^.Block,Block) do begin
     OldStackItem:=StackItem;
     StackItem:=StackItem^.Up;
     FreeStackItem(OldStackItem);
    end;
    StackItems[TemporaryIndex]:=StackItem;
   end;
   procedure ProcessOperand(var Operand:TPACCIntermediateRepresentationCodeOperand);
   var TemporaryIndex,LinkTemporaryIndex:TPACCInt32;
   begin
    if (Operand.Kind=pircokTEMPORARY) and (Temporaries[Operand.Temporary].Visit<>0) then begin
     TemporaryIndex:=Operand.Temporary;
     LinkTemporaryIndex:=CreateLinkTemporary(TemporaryIndex);
     Temporaries[LinkTemporaryIndex].Visit:=TemporaryIndex;
     StackItems[TemporaryIndex]:=NewStackItem(Operand,StackItems[TemporaryIndex]);
     Operand.Temporary:=LinkTemporaryIndex;
    end;
   end;
  var InstructionIndex,InstructionOperandIndex,TemporaryIndex,SuccessorIndex,Index:TPACCInt32;
      Phi:TPACCIntermediateRepresentationCodePhi;
      Instruction:TPACCIntermediateRepresentationCodeInstruction;
      Temporary:TPACCIntermediateRepresentationCodeTemporary;
      Successor:TPACCIntermediateRepresentationCodeBlock;
      Successors:TPACCIntermediateRepresentationCodeBlockList;
      AlreadySeenHashMap:TPACCPointerHashMap;
  begin

   Phi:=Block.Phi;
   while assigned(Phi) do begin
    ProcessOperand(Phi.To_);
    Phi:=Phi.Link;
   end;

   for InstructionIndex:=0 to Block.Instructions.Count-1 do begin
    Instruction:=Block.Instructions[InstructionIndex];
    for InstructionOperandIndex:=0 to length(Instruction.Operands)-1 do begin
     if Instruction.Operands[InstructionOperandIndex].Kind=pircokTEMPORARY then begin
      TemporaryIndex:=Instruction.Operands[InstructionOperandIndex].Temporary;
      Temporary:=Temporaries[TemporaryIndex];
      if Temporary.Visit<>0 then begin
       Instruction.Operands[InstructionOperandIndex]:=GetStack(TemporaryIndex);
      end;
     end;
     ProcessOperand(Instruction.To_);
    end;
   end;

   if Block.Jump.Operand.Kind=pircokTEMPORARY then begin
    TemporaryIndex:=Block.Jump.Operand.Temporary;
    Temporary:=Temporaries[TemporaryIndex];
    if Temporary.Visit<>0 then begin
     Block.Jump.Operand:=GetStack(TemporaryIndex);
    end;
   end;

   Successors:=TPACCIntermediateRepresentationCodeBlockList.Create;
   try
    AlreadySeenHashMap:=TPACCPointerHashMap.Create;
    try
     for SuccessorIndex:=0 to Block.Successors.Count-1 do begin
      Successor:=Block.Successors[SuccessorIndex];
      if not assigned(AlreadySeenHashMap[Successor]) then begin
       AlreadySeenHashMap[Successor]:=Successor;
       Successors.Add(Successor);
      end;
     end;
    finally
     AlreadySeenHashMap.Free;
    end;
    for SuccessorIndex:=0 to Successors.Count-1 do begin
     Successor:=Successors[SuccessorIndex];
     Phi:=Successor.Phi;
     while assigned(Phi) do begin
      if Phi.To_.Kind=pircokTEMPORARY then begin
       TemporaryIndex:=Phi.To_.Temporary;
       Temporary:=Temporaries[TemporaryIndex];
       if TemporaryIndex=Temporary.Visit then begin
        Index:=Phi.CountOperands;
        inc(Phi.CountOperands);
        if length(Phi.Operands)<Phi.CountOperands then begin
         SetLength(Phi.Operands,Phi.CountOperands*2);
        end;
        if length(Phi.Blocks)<Phi.CountOperands then begin
         SetLength(Phi.Blocks,Phi.CountOperands*2);
        end;
        Phi.Operands[Index]:=GetStack(TemporaryIndex);
        Phi.Blocks[Index]:=Block;
       end;
      end else begin
       TPACCInstance(fInstance).AddError('Internal error 2017-01-27-23-38-0000',nil,true);
      end;
      Phi:=Phi.Link;
     end;
    end;
   finally
    Successors.Free;
   end;

   Successor:=Block.Dominance;
   while assigned(Successor) do begin
    ProcessBlock(Successor);
    Successor:=Successor.DominanceLink;
   end;

  end;
 var Index:TPACCInt32;
 begin
  StackItems:=nil;
  try
   SetLength(StackItems,Temporaries.Count);
   if length(StackItems)>0 then begin
    FillChar(StackItems[0],length(StackItems)*SizeOf(PStackItem),#0);
   end;
   LastStackItem:=nil;
   AllocatedStackItems:=TList.Create;
   try
    ProcessBlock(StartBlock);
   finally
    for Index:=0 to AllocatedStackItems.Count-1 do begin
     FreeMem(AllocatedStackItems[Index]);
    end;
    AllocatedStackItems.Free;
   end;
  finally
   StackItems:=nil;
  end;
 end;
begin
 FillDominators;
 FillDominanceFrontier;
 FillLive;
 FillMissingPhiInstructions;
 Rename;
end;

procedure TPACCIntermediateRepresentationCodeFunction.PostProcess;
begin
 FillRPO;
 FillPredecessors;
 FillUse;
 SSA;
end;

procedure TPACCIntermediateRepresentationCodeFunction.EmitFunction(const AFunctionNode:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration);
var Index,Size,Alignment:TPACCInt32;
    Variable:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;
    StringList:TStringList;
    Type_:PPACCType;
begin

 FunctionDeclaration:=AFunctionNode;

 FunctionName:=AFunctionNode.FunctionName;

 CurrentBlock:=nil;
 BlockLink:=@StartBlock;
 PhiLink:=nil;

 NeedNewBlock:=true;

 AssignOpLValueTemporary:=-1;

 EmitLabel(NewHiddenLabel);

 if assigned(FunctionDeclaration.Parameters) then begin
  for Index:=0 to FunctionDeclaration.Parameters.Count-1 do begin
   Variable:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(FunctionDeclaration.Parameters[Index]);
   if assigned(Variable) then begin
    CreateVariableTemporary(Variable);
   end;
  end;
 end;

 if assigned(FunctionDeclaration.LocalVariables) then begin
  for Index:=0 to FunctionDeclaration.LocalVariables.Count-1 do begin
   Variable:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(FunctionDeclaration.LocalVariables[Index]);
   if assigned(Variable) then begin
    Type_:=Variable.Type_;
    if Type_.Kind in (PACCIntermediateRepresentationCodeINTTypeKinds+
                      PACCIntermediateRepresentationCodeLONGTypeKinds+
                      PACCIntermediateRepresentationCodeFLOATTypeKinds+
                      PACCIntermediateRepresentationCodeDOUBLETypeKinds+
                      [tkPOINTER]) then begin
     if not Variable.InitializedBeforeUse then begin
      EmitStoreIntegerValueToVariable(Variable,0);
     end;
    end else begin
     Alignment:=Variable.Type_^.Alignment;
     Size:=1;
     case Type_^.Kind of
      tkARRAY:begin
       while assigned(Type_) and (Type_^.Kind=tkARRAY) do begin
        Size:=Size*Type_^.ArrayLength;
        Type_:=Type_^.ChildType;
       end;
       if Type_^.Kind=tkSTRUCT then begin
        Type_:=Variable.Type_;
        Size:=Type_^.Size;
        Type_:=TPACCInstance(fInstance).TypeCHAR;
       end else begin
        Size:=Size*Type_^.Size;
       end;
      end;
      tkSTRUCT:begin
       Size:=Type_^.Size;
       Type_:=TPACCInstance(fInstance).TypeCHAR;
      end;
     end;
     EmitInstruction(pircoALLOC,DataTypeToCodeType(Type_),CreateTemporaryOperand(CreateVariableTemporary(Variable)),[CreateIntegerValueOperand(Size),CreateIntegerValueOperand(Alignment)],AFunctionNode.SourceLocation);
    end;
   end;
  end;
 end;

 EmitStatements(TPACCAbstractSyntaxTreeNodeStatements(AFunctionNode.Body));

 if CurrentBlock.Jump.Kind=pircjkNONE then begin
  CurrentBlock.Jump.Kind:=pircjkRET;
 end;

 PostProcess;

 StringList:=TStringList.Create;
 try
  DumpTo(StringList);
  writeln(StringList.Text);
 finally
  StringList.Free;
 end;

end;

procedure TPACCIntermediateRepresentationCodeFunction.DumpTo(const AStringList:TStringList);
const AlignmentOpcode=20;
 procedure ProcessBlock(const Block:TPACCIntermediateRepresentationCodeBlock);
 var InstructionIndex,OperandIndex,SuccessorIndex:TPACCInt32;
     Instruction:TPACCIntermediateRepresentationCodeInstruction;
     Phi:TPACCIntermediateRepresentationCodePhi;
     Line:TPACCRawByteString;
     Successor:TPACCIntermediateRepresentationCodeBlock;
  procedure ProcessOperand(const Operand:TPACCIntermediateRepresentationCodeOperand);
  begin
   case Operand.Kind of
    pircokNONE:begin
    end;
    pircokTEMPORARY:begin
     Line:=Line+'tmp('+IntToStr(Operand.Temporary)+')';
    end;
    pircokCALL:begin
     Assert(false);
    end;
    pircokINTEGER:begin
     Line:=Line+'integer('+IntToStr(Operand.IntegerValue)+')';
    end;
    pircokFLOAT:begin
     Line:=Line+'float('+ConvertDoubleToString(Operand.FloatValue,omStandard)+')';
    end;
    pircokVARIABLE:begin
     Line:=Line+'variable('+Operand.Variable.VariableName+')';
    end;
    pircokLABEL:begin
     Line:=Line+'label('+LowerCase(IntToHex(TPACCPtrUInt(Operand.Label_),SizeOf(TPACCPtrUInt) shl 1))+')';
    end;
    pircokFUNCTION:begin
     Assert(false);
    end;
    else begin
//       TPACCInstance(fInstance).AddError('Internal error 2017-01-28-00-38-0000',@Instruction.SourceLocation,true);
    end;
   end
  end;
 begin
  if assigned(Block) then begin

   AStringList.Add('  @'+LowerCase(IntToHex(TPACCPtrUInt(Block),SizeOf(TPACCPtrUInt) shl 1))+':');

   Phi:=Block.Phi;
   if assigned(Phi) then begin
    Line:='    ';
    case Phi.To_.Kind of
     pircokNONE:begin
     end;
     pircokTEMPORARY:begin
      Line:=Line+'tmp('+IntToStr(Phi.To_.Temporary)+') ='+CodeTypeChars[Phi.Type_]+' ';
     end;
     else begin
      TPACCInstance(fInstance).AddError('Internal error 2017-01-28-01-01-0000',nil,true);
     end;
    end;
    while length(Line)<AlignmentOpcode do begin
     Line:=Line+' ';
    end;
    Line:=Line+'phi';
    for OperandIndex:=0 to Phi.CountOperands-1 do begin
     if OperandIndex=0 then begin
      Line:=Line+' ';
     end else begin
      Line:=Line+', ';
     end;
     Line:=Line+'@'+LowerCase(IntToHex(TPACCPtrUInt(Phi.Blocks[OperandIndex]),SizeOf(TPACCPtrUInt) shl 1))+' ';
     ProcessOperand(Phi.Operands[OperandIndex]);
    end;
    AStringList.Add(Line);
   end;

   for InstructionIndex:=0 to Block.Instructions.Count-1 do begin
    Instruction:=Block.Instructions[InstructionIndex];
    Line:='    ';
    case Instruction.To_.Kind of
     pircokNONE:begin
     end;
     pircokTEMPORARY:begin
      Line:=Line+'tmp('+IntToStr(Instruction.To_.Temporary)+') ='+CodeTypeChars[Instruction.Type_]+' ';
     end;
     else begin
      TPACCInstance(fInstance).AddError('Internal error 2017-01-28-00-38-0000',@Instruction.SourceLocation,true);
     end;
    end;

    while length(Line)<AlignmentOpcode do begin
     Line:=Line+' ';
    end;

    Line:=Line+OpcodeNames[Instruction.Opcode];

    for OperandIndex:=0 to length(Instruction.Operands)-1 do begin
     if OperandIndex=0 then begin
      Line:=Line+' ';
     end else begin
      Line:=Line+', ';
     end;
     ProcessOperand(Instruction.Operands[OperandIndex]);
    end;

    AStringList.Add(Line);

   end;

   Line:='';
   while length(Line)<AlignmentOpcode do begin
    Line:=Line+' ';
   end;
   Line:=Line+JumpKindNames[Block.Jump.Kind];
   if Block.Jump.Operand.Kind<>pircokNONE then begin
    Line:=Line+' ';
    ProcessOperand(Block.Jump.Operand);
   end;
   if Block.Successors.Count>0 then begin
    Line:=Line+' => [';
    for SuccessorIndex:=0 to Block.Successors.Count-1 do begin
     Successor:=Block.Successors[SuccessorIndex];
     Line:=Line+'@'+LowerCase(IntToHex(TPACCPtrUInt(Successor),SizeOf(TPACCPtrUInt) shl 1));
     if (SuccessorIndex+1)<Block.Successors.Count then begin
      Line:=Line+', ';
     end;
    end;
    Line:=Line+']';
   end;
   AStringList.Add(Line);

  end;
 end;
var Index:TPACCInt32;
    Line:TPACCRawByteString;
    Parameter:TPACCAbstractSyntaxTreeNodeLocalGlobalVariable;
    Temporary:TPACCIntermediateRepresentationCodeTemporary;
    Block:TPACCIntermediateRepresentationCodeBlock;
begin                                         

 if assigned(FunctionDeclaration.Type_) and not ((tfStatic in FunctionDeclaration.Type_^.Flags) or (afInline in FunctionDeclaration.Type_^.Attribute.Flags)) then begin
  Line:='export ';
 end else begin
  Line:='';
 end;
 Line:=Line+'function ';
 if assigned(FunctionDeclaration.Type_) and
    assigned(FunctionDeclaration.Type_^.ReturnType) and
    (FunctionDeclaration.Type_^.ReturnType^.Kind<>tkVOID) then begin
  Line:=Line+CodeTypeChars[DataTypeToCodeType(FunctionDeclaration.Type_^.ReturnType)]+' ';
 end;
 Line:=Line+'$'+FunctionName+'(';
 if assigned(FunctionDeclaration.Parameters) then begin
  for Index:=0 to FunctionDeclaration.Parameters.Count-1 do begin
   Parameter:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(FunctionDeclaration.Parameters[Index]);
   if assigned(Parameter) then begin
    Temporary:=VariableTemporaryHashMap[Parameter];
    if assigned(Temporary) then begin
     if Index>0 then begin
      Line:=Line+', ';
     end;
     Line:=Line+CodeTypeChars[DataTypeToCodeType(Parameter.Type_)]+' tmp('+IntToStr(Temporary.Index)+')';
    end;
   end;
  end;
 end;
 Line:=Line+'){';
 AStringList.Add(Line);

 Block:=StartBlock;
 while assigned(Block) do begin
  ProcessBlock(Block);
  Block:=Block.Link;
 end;

 AStringList.Add('}');
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

constructor TPACCIntermediateRepresentationCodeDeclaration.Create(const AInstance:TObject);
begin

 inherited Create;

 fInstance:=AInstance;

 TPACCInstance(fInstance).AllocatedObjects.Add(self);

 fDeclaration:=nil;

 fLabel:=nil;

 fVariable:=nil;

 fSize:=0;

 fAlignment:=1;

 DataItems:=nil;
 CountDataItems:=0;

end;

destructor TPACCIntermediateRepresentationCodeDeclaration.Destroy;
begin
 SetLength(DataItems,0);
 inherited Destroy;
end;

function TPACCIntermediateRepresentationCodeDeclaration.NewHiddenLabel:TPACCAbstractSyntaxTreeNodeLabel;
begin
 result:=TPACCAbstractSyntaxTreeNodeLabel.Create(fInstance,astnkHIDDEN_LABEL,nil,TPACCInstance(fInstance).SourceLocation,'');
end;

procedure TPACCIntermediateRepresentationCodeDeclaration.EmitUI8(const Value:TPACCUInt8;const ValueOffsetBase:TPACCAbstractSyntaxTreeNode;const Count:TPACCInt32);
var Index:TPACCInt32;
    DataItem:PPACCIntermediateRepresentationCodeDeclarationDataItem;
begin
 Index:=CountDataItems;
 inc(CountDataItems);
 if length(DataItems)<CountDataItems then begin
  SetLength(DataItems,CountDataItems*2);
 end;
 DataItem:=@DataItems[Index];
 DataItem^.Kind:=pircdikUI8;
 DataItem^.ValueUI8:=Value;
 DataItem^.ValueOffsetBase:=ValueOffsetBase;
 DataItem^.Count:=Count;
 fSize:=fSize+(TPACCInt64(Count)*SizeOf(TPACCUInt8));
end;

procedure TPACCIntermediateRepresentationCodeDeclaration.EmitUI16(const Value:TPACCUInt16;const ValueOffsetBase:TPACCAbstractSyntaxTreeNode;const Count:TPACCInt32);
var Index:TPACCInt32;
    DataItem:PPACCIntermediateRepresentationCodeDeclarationDataItem;
begin
 Index:=CountDataItems;
 inc(CountDataItems);
 if length(DataItems)<CountDataItems then begin
  SetLength(DataItems,CountDataItems*2);
 end;
 DataItem:=@DataItems[Index];
 DataItem^.Kind:=pircdikUI16;
 DataItem^.ValueUI16:=Value;
 DataItem^.ValueOffsetBase:=ValueOffsetBase;
 DataItem^.Count:=Count;
 fSize:=fSize+(TPACCInt64(Count)*SizeOf(TPACCUInt16));
end;

procedure TPACCIntermediateRepresentationCodeDeclaration.EmitUI32(const Value:TPACCUInt32;const ValueOffsetBase:TPACCAbstractSyntaxTreeNode;const Count:TPACCInt32);
var Index:TPACCInt32;
    DataItem:PPACCIntermediateRepresentationCodeDeclarationDataItem;
begin
 Index:=CountDataItems;
 inc(CountDataItems);
 if length(DataItems)<CountDataItems then begin
  SetLength(DataItems,CountDataItems*2);
 end;
 DataItem:=@DataItems[Index];
 DataItem^.Kind:=pircdikUI32;
 DataItem^.ValueUI32:=Value;
 DataItem^.ValueOffsetBase:=ValueOffsetBase;
 DataItem^.Count:=Count;
 fSize:=fSize+(TPACCInt64(Count)*SizeOf(TPACCUInt32));
end;

procedure TPACCIntermediateRepresentationCodeDeclaration.EmitUI64(const Value:TPACCUInt64;const ValueOffsetBase:TPACCAbstractSyntaxTreeNode;const Count:TPACCInt32);
var Index:TPACCInt32;
    DataItem:PPACCIntermediateRepresentationCodeDeclarationDataItem;
begin
 Index:=CountDataItems;
 inc(CountDataItems);
 if length(DataItems)<CountDataItems then begin
  SetLength(DataItems,CountDataItems*2);
 end;
 DataItem:=@DataItems[Index];
 DataItem^.Kind:=pircdikUI64;
 DataItem^.ValueUI64:=Value;
 DataItem^.ValueOffsetBase:=ValueOffsetBase;
 DataItem^.Count:=Count;
 fSize:=fSize+(TPACCInt64(Count)*SizeOf(TPACCUInt64));
end;

procedure TPACCIntermediateRepresentationCodeDeclaration.EmitStringData(const Node:TPACCAbstractSyntaxTreeNodeStringValue);
var Index:TPACCInt32;
begin
 for Index:=1 to length(Node.Value) do begin
  EmitUI8(TPACCUInt8(AnsiChar(Node.Value[Index])),nil,1);
 end;
 case Node.Type_.ChildType^.Size of
  2:begin
   EmitUI16(0,nil,1);
  end;
  4:begin
   EmitUI32(0,nil,1);
  end;
  8:begin
   EmitUI64(0,nil,1);
  end;
  else begin
   EmitUI8(0,nil,1);
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeDeclaration.EmitPrimitiveTypeData(const Type_:PPACCType;const Node:TPACCAbstractSyntaxTreeNode);
var f:TPACCFloat;
    d:TPACCDouble;
    i32:TPACCInt32;
    i64:TPACCInt64;
    BaseNode:TPACCAbstractSyntaxTreeNode;
    BaseType:PPACCType;
    StringDeclaration:TPACCIntermediateRepresentationCodeDeclaration;
begin
 case Type_^.Kind of
  tkFLOAT:begin
   f:=TPACCInstance(Instance).EvaluateFloatExpression(Node,tkFLOAT);
   EmitUI32(TPACCUInt32(pointer(@f)^),nil,1);
  end;
  tkDOUBLE:begin
   d:=TPACCInstance(Instance).EvaluateFloatExpression(Node,tkDOUBLE);
   EmitUI64(TPACCUInt64(pointer(@d)^),nil,1);
  end;
  tkBOOL:begin
   EmitUI8(TPACCInstance(Instance).EvaluateIntegerExpression(Node,nil),nil,1);
  end;
  tkCHAR:begin
   EmitUI8(TPACCInstance(Instance).EvaluateIntegerExpression(Node,nil),nil,1);
  end;
  tkSHORT:begin
   EmitUI16(TPACCInstance(Instance).EvaluateIntegerExpression(Node,nil),nil,1);
  end;
  tkINT:begin
   EmitUI32(TPACCInstance(Instance).EvaluateIntegerExpression(Node,nil),nil,1);
  end;
  tkLONG,tkLLONG:begin
   EmitUI64(TPACCInstance(Instance).EvaluateIntegerExpression(Node,nil),nil,1);
  end;
  tkPOINTER:begin
   if Node.Kind=astnkOP_LABEL_ADDR then begin
    if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
     EmitUI32(0,TPACCAbstractSyntaxTreeNodeLabel(TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).Label_),1);
    end else begin
     EmitUI64(0,TPACCAbstractSyntaxTreeNodeLabel(TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).Label_),1);
    end;
   end else if (Node is TPACCAbstractSyntaxTreeNodeUnaryOperator) and
               (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Kind=astnkSTRING) and
               (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_.Kind=tkARRAY) and
               (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_.ChildType^.Kind=tkCHAR) then begin
    StringDeclaration:=TPACCIntermediateRepresentationCodeDeclaration.Create(fInstance);
    TPACCInstance(fInstance).IntermediateRepresentationCode.Declarations.Add(StringDeclaration);
    StringDeclaration.Variable:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable.Create(TPACCInstance(Instance),astnkLVAR,TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_,Node.SourceLocation,'_$TEMPSTRING$'+IntToStr(TPACCPtrUInt(StringDeclaration)),0);
    StringDeclaration.Alignment:=Max(4,TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_.Alignment);
    StringDeclaration.EmitStringData(TPACCAbstractSyntaxTreeNodeStringValue(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand));
    StringDeclaration.Finish;
    if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
     EmitUI32(0,TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(StringDeclaration.Variable),1);
    end else begin
     EmitUI64(0,TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(StringDeclaration.Variable),1);
    end;
   end else if Node.Kind=astnkGVAR then begin
    if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
     EmitUI32(0,TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node),1);
    end else begin
     EmitUI64(0,TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node),1);
    end;
   end else begin
    BaseNode:=nil;
    i64:=TPACCInstance(Instance).EvaluateIntegerExpression(Node,@BaseNode);
    if assigned(BaseNode) then begin
     BaseType:=BaseNode.Type_;
     if BaseNode.Kind in [astnkCONV,astnkADDR] then begin
      BaseNode:=TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand;
     end;
     if BaseNode.Kind=astnkGVAR then begin
      Assert(assigned(BaseType.ChildType));
      if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
       EmitUI32(i64*BaseType.ChildType^.Size,TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node),1);
      end else begin
       EmitUI64(i64*BaseType.ChildType^.Size,TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node),1);
      end;
     end else begin
      TPACCInstance(Instance).AddError('Global variable expected',@BaseNode.SourceLocation,true);
     end;
    end else begin
     if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
      EmitUI32(TPACCUInt32(i64),nil,1);
     end else begin
      EmitUI64(TPACCUInt64(i64),nil,1);
     end;
    end;
   end;
  end;
  else begin
   TPACCInstance(Instance).AddError('Internal error 2017-01-25-00-21-0000',@Node.SourceLocation,true);
  end;
 end;
end;

procedure TPACCIntermediateRepresentationCodeDeclaration.EmitInitializerList(const Nodes:TPACCAbstractSyntaxTreeNodeList;Size,Offset:TPACCInt64);
var Index:TPACCInt32;
    Node:TPACCAbstractSyntaxTreeNodeInitializer;
    Value:TPACCAbstractSyntaxTreeNode;
    Delta,Data:TPACCInt64;
    ToType:PPACCType;
    SubDeclaration:TPACCIntermediateRepresentationCodeDeclaration;
begin
 Index:=0;
 while Index<Nodes.Count do begin
  Node:=TPACCAbstractSyntaxTreeNodeInitializer(Nodes[Index]);
  Value:=Node.InitializionValue;
  Delta:=Node.InitializionOffset-Offset;
  if Delta>0 then begin
   EmitUI8(0,nil,Delta);
  end;
  if Node.ToType^.BitSize>0 then begin
   if Node.ToType^.BitOffset=0 then begin
    Data:=TPACCInstance(Instance).EvaluateIntegerExpression(Value,nil);
    ToType:=Node.ToType;
    inc(Index);
    while Index<Nodes.Count do begin
     Node:=TPACCAbstractSyntaxTreeNodeInitializer(Nodes[Index]);
     if Node.Type_.BitSize<=0 then begin
      break;
     end else begin
      Value:=Node.InitializionValue;
      ToType:=Node.ToType;
      Data:=Data or ((TPACCInstance(Instance).EvaluateIntegerExpression(Value,nil) and ((TPACCInt64(1) shl ToType^.BitSize)-1)) shl ToType^.BitOffset);
      inc(Index);
     end;
    end;
    case ToType^.Kind of
     tkBOOL:begin
      EmitUI8(Data,nil,1);
     end;
     tkCHAR:begin
      EmitUI8(Data,nil,1);
     end;
     tkSHORT:begin
      EmitUI16(Data,nil,1);
     end;
     tkINT:begin
      EmitUI32(Data,nil,1);
     end;
     tkLONG,tkLLONG:begin
      EmitUI64(Data,nil,1);
     end;
     else begin
      TPACCInstance(Instance).AddError('Internal error 2017-01-17-11-27-0000',@Node.SourceLocation,true);
     end;
    end;
    inc(Offset,ToType^.Size);
    dec(Size,ToType^.Size);
    if Index=Nodes.Count then begin
     break;
    end;
   end else begin
    TPACCInstance(Instance).AddError('Internal error 2017-01-17-11-21-0000',@Node.SourceLocation,true);
   end;
  end else begin
   inc(Offset,Node.ToType^.Size);
   dec(Size,Node.ToType^.Size);
  end;
  if Value.Kind=astnkADDR then begin
   case TPACCAbstractSyntaxTreeNodeUnaryOperator(Value).Operand.Kind of
    astnkLVAR:begin
     SubDeclaration:=TPACCIntermediateRepresentationCodeDeclaration.Create(fInstance);
     TPACCInstance(fInstance).IntermediateRepresentationCode.Declarations.Add(SubDeclaration);
     SubDeclaration.Variable:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Value).Operand);
     SubDeclaration.Alignment:=Max(1,SubDeclaration.Variable.Type_^.Alignment);
     SubDeclaration.EmitInitializerList(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Value).Operand).LocalVariableInitialization,
                                        TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Value).Operand).Type_.Size,
                                        0);
     SubDeclaration.Finish;
     if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
      EmitUI32(0,TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Value).Operand),1);
     end else begin
      EmitUI64(0,TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Value).Operand),1);
     end;
    end;
    astnkGVAR:begin
     if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
      EmitUI32(0,TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Value).Operand),1);
     end else begin
      EmitUI64(0,TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeUnaryOperator(Value).Operand),1);
     end;
    end;
    astnkFUNCDESG:begin
     if TPACCInstance(fInstance).Target.SizeOfPointer=TPACCInstance(fInstance).Target.SizeOfInt then begin
      EmitUI32(0,TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(TPACCAbstractSyntaxTreeNodeUnaryOperator(Value).Operand).Variable),1);
     end else begin
      EmitUI64(0,TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(TPACCAbstractSyntaxTreeNodeUnaryOperator(Value).Operand).Variable),1);
     end;
    end;
    else begin
     TPACCInstance(Instance).AddError('Internal error 2017-01-25-00-41-0000',@Node.SourceLocation,true);
    end;
   end;
  end else if (Value.Kind=astnkLVAR) and assigned(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Value).LocalVariableInitialization) then begin
   EmitInitializerList(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Value).LocalVariableInitialization,
                       TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Value).Type_.Size,
                       0);
  end else begin
   EmitPrimitiveTypeData(Node.ToType,Node.InitializionValue);
  end;
  inc(Index);
 end;
 if Size>0 then begin
  EmitUI8(0,nil,Size);
 end;
end;

procedure TPACCIntermediateRepresentationCodeDeclaration.EmitDeclaration(const Node:TPACCAbstractSyntaxTreeNodeDeclaration);
begin
 fDeclaration:=Node;
 EmitInitializerList(Node.DeclarationInitialization,
                     Node.DeclarationVariable.Type_^.Size,
                     0);
end;

procedure TPACCIntermediateRepresentationCodeDeclaration.Finish;
begin
 SetLength(DataItems,CountDataItems);
end;

constructor TPACCIntermediateRepresentationCodeDeclarationList.Create;
begin
 inherited Create;
end;

destructor TPACCIntermediateRepresentationCodeDeclarationList.Destroy;
begin
 inherited Destroy;
end;

function TPACCIntermediateRepresentationCodeDeclarationList.GetItem(const AIndex:TPACCInt):TPACCIntermediateRepresentationCodeDeclaration;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCIntermediateRepresentationCodeDeclarationList.SetItem(const AIndex:TPACCInt;const AItem:TPACCIntermediateRepresentationCodeDeclaration);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

constructor TPACCIntermediateRepresentationCode.Create(const AInstance:TObject);
begin

 inherited Create;

 fInstance:=AInstance;

 fFunctions:=TPACCIntermediateRepresentationCodeFunctionList.Create;

 fDeclarations:=TPACCIntermediateRepresentationCodeDeclarationList.Create;

 fExternalDeclarations:=TPACCAbstractSyntaxTreeNodeList.Create;

end;

destructor TPACCIntermediateRepresentationCode.Destroy;
begin
 fFunctions.Free;
 fDeclarations.Free;
 fExternalDeclarations.Free;
 inherited Destroy;
end;

procedure GenerateIntermediateRepresentationCode(const AInstance:TObject;const ARootAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNode);
var Index:TPACCInt32;
    RootAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNodeTranslationUnit;
    Node:TPACCAbstractSyntaxTreeNode;
    Function_:TPACCIntermediateRepresentationCodeFunction;
    Declaration:TPACCIntermediateRepresentationCodeDeclaration;
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
      if TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationVariable.Kind=astnkGVAR then begin
       TPACCInstance(AInstance).IntermediateRepresentationCode.ExternalDeclarations.Add(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationVariable));
      end else begin
       TPACCInstance(AInstance).AddError('Internal error 2017-01-24-21-39-0000',@Node.SourceLocation,true);
      end;
     end;
     astnkDECL:begin
      Declaration:=TPACCIntermediateRepresentationCodeDeclaration.Create(AInstance);
      TPACCInstance(AInstance).IntermediateRepresentationCode.Declarations.Add(Declaration);
      Declaration.Variable:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationVariable);
      Declaration.Alignment:=Max(1,Declaration.Variable.Type_^.Alignment);
      Declaration.EmitDeclaration(TPACCAbstractSyntaxTreeNodeDeclaration(Node));
      Declaration.Finish;
     end;
     astnkFUNC:begin
      Function_:=TPACCIntermediateRepresentationCodeFunction.Create(AInstance);
      TPACCInstance(AInstance).IntermediateRepresentationCode.Functions.Add(Function_);
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

procedure InitializeOpcodeNames;
begin
 FillChar(OpcodeNames,SizeOf(OpcodeNames),#0);
 OpcodeNames[pircoNONE]:='none';
 OpcodeNames[pircoNOP]:='nop';
 OpcodeNames[pircoASM]:='asm';
 OpcodeNames[pircoMEMCPYI]:='memcpyi';
 OpcodeNames[pircoMEMCPYL]:='memcpyl';
 OpcodeNames[pircoZEROMEMI]:='zeromemi';
 OpcodeNames[pircoZEROMEML]:='zeromeml';
 OpcodeNames[pircoSETI]:='seti';
 OpcodeNames[pircoSETL]:='setl';
 OpcodeNames[pircoSETF]:='setf';
 OpcodeNames[pircoSETD]:='setd';
 OpcodeNames[pircoMOVI]:='movi';
 OpcodeNames[pircoMOVL]:='movl';
 OpcodeNames[pircoMOVF]:='movf';
 OpcodeNames[pircoMOVD]:='movd';
 OpcodeNames[pircoSWAPI]:='swapi';
 OpcodeNames[pircoSWAPL]:='swapl';
 OpcodeNames[pircoSWAPF]:='swapf';
 OpcodeNames[pircoSWAPD]:='swapd';
 OpcodeNames[pircoCITF]:='citf';
 OpcodeNames[pircoCLTF]:='cltf';
 OpcodeNames[pircoCITD]:='citd';
 OpcodeNames[pircoCLTD]:='cltd';
 OpcodeNames[pircoCFTD]:='cftd';
 OpcodeNames[pircoCDTF]:='cdtf';
 OpcodeNames[pircoCITL]:='citl';
 OpcodeNames[pircoCLTO]:='clto';
 OpcodeNames[pircoTFTI]:='tfti';
 OpcodeNames[pircoTFTL]:='tftl';
 OpcodeNames[pircoTDTI]:='tdti';
 OpcodeNames[pircoTDTL]:='tdtl';
 OpcodeNames[pircoCASTFI]:='castfi';
 OpcodeNames[pircoCASTIF]:='castif';
 OpcodeNames[pircoCASTDL]:='castdl';
 OpcodeNames[pircoCASTLD]:='castld';
 OpcodeNames[pircoADDROFI]:='addrofi';
 OpcodeNames[pircoADDROFL]:='addrofl';
 OpcodeNames[pircoZECI]:='zeci';
 OpcodeNames[pircoZESI]:='zesi';
 OpcodeNames[pircoZECL]:='zecl';
 OpcodeNames[pircoZESL]:='zesl';
 OpcodeNames[pircoZEIL]:='zeil';
 OpcodeNames[pircoSECI]:='seci';
 OpcodeNames[pircoSESI]:='sesi';
 OpcodeNames[pircoSECL]:='secl';
 OpcodeNames[pircoSESL]:='sesl';
 OpcodeNames[pircoSEIL]:='seil';
 OpcodeNames[pircoTRLI]:='trli';
 OpcodeNames[pircoLDUCI]:='lduci';
 OpcodeNames[pircoLDUSI]:='ldusi';
 OpcodeNames[pircoLDUII]:='lduii';
 OpcodeNames[pircoLDSCI]:='ldsci';
 OpcodeNames[pircoLDSSI]:='ldssi';
 OpcodeNames[pircoLDSII]:='ldsii';
 OpcodeNames[pircoLDUCL]:='lducl';
 OpcodeNames[pircoLDUSL]:='ldusl';
 OpcodeNames[pircoLDUIL]:='lduil';
 OpcodeNames[pircoLDULL]:='ldull';
 OpcodeNames[pircoLDSCL]:='ldscl';
 OpcodeNames[pircoLDSSL]:='ldssl';
 OpcodeNames[pircoLDSIL]:='ldsil';
 OpcodeNames[pircoLDSLL]:='ldsll';
 OpcodeNames[pircoLDF]:='ldf';
 OpcodeNames[pircoLDD]:='ldd';
 OpcodeNames[pircoSTIC]:='stic';
 OpcodeNames[pircoSTIS]:='stis';
 OpcodeNames[pircoSTII]:='stii';
 OpcodeNames[pircoSTLC]:='stlc';
 OpcodeNames[pircoSTLS]:='stls';
 OpcodeNames[pircoSTLI]:='stli';
 OpcodeNames[pircoSTLL]:='stll';
 OpcodeNames[pircoSTF]:='stf';
 OpcodeNames[pircoSTD]:='std';
 OpcodeNames[pircoNEGI]:='negi';
 OpcodeNames[pircoADDI]:='addi';
 OpcodeNames[pircoSUBI]:='subi';
 OpcodeNames[pircoSMULI]:='smuli';
 OpcodeNames[pircoSDIVI]:='sdivi';
 OpcodeNames[pircoSMODI]:='smodi';
 OpcodeNames[pircoUMULI]:='umuli';
 OpcodeNames[pircoUDIVI]:='udivi';
 OpcodeNames[pircoUMODI]:='umodi';
 OpcodeNames[pircoNOTI]:='noti';
 OpcodeNames[pircoANDI]:='andi';
 OpcodeNames[pircoORI]:='ori';
 OpcodeNames[pircoXORI]:='xori';
 OpcodeNames[pircoSHLI]:='shli';
 OpcodeNames[pircoSHRI]:='shri';
 OpcodeNames[pircoSARI]:='sari';
 OpcodeNames[pircoNEGL]:='negl';
 OpcodeNames[pircoADDL]:='addl';
 OpcodeNames[pircoSUBL]:='subl';
 OpcodeNames[pircoSMULL]:='smull';
 OpcodeNames[pircoSDIVL]:='sdivl';
 OpcodeNames[pircoSMODL]:='smodl';
 OpcodeNames[pircoUMULL]:='umull';
 OpcodeNames[pircoUDIVL]:='udivl';
 OpcodeNames[pircoUMODL]:='umodl';
 OpcodeNames[pircoNOTL]:='notl';
 OpcodeNames[pircoANDL]:='andl';
 OpcodeNames[pircoORL]:='orl';
 OpcodeNames[pircoXORL]:='xorl';
 OpcodeNames[pircoSHLL]:='shll';
 OpcodeNames[pircoSHRL]:='shrl';
 OpcodeNames[pircoSARL]:='sarl';
 OpcodeNames[pircoNEGF]:='negf';
 OpcodeNames[pircoADDF]:='addf';
 OpcodeNames[pircoSUBF]:='subf';
 OpcodeNames[pircoMULF]:='mulf';
 OpcodeNames[pircoDIVF]:='divf';
 OpcodeNames[pircoNEGD]:='negd';
 OpcodeNames[pircoADDD]:='addd';
 OpcodeNames[pircoSUBD]:='subd';
 OpcodeNames[pircoMULD]:='muld';
 OpcodeNames[pircoDIVD]:='divd';
 OpcodeNames[pircoCMPSLEI]:='cmpslei';
 OpcodeNames[pircoCMPSLTI]:='cmpslti';
 OpcodeNames[pircoCMPSGEI]:='cmpsgei';
 OpcodeNames[pircoCMPSGTI]:='cmpsgti';
 OpcodeNames[pircoCMPULEI]:='cmpulei';
 OpcodeNames[pircoCMPULTI]:='cmpulti';
 OpcodeNames[pircoCMPUGEI]:='cmpugei';
 OpcodeNames[pircoCMPUGTI]:='cmpugti';
 OpcodeNames[pircoCMPEQI]:='cmpeqi';
 OpcodeNames[pircoCMPNEI]:='cmpnei';
 OpcodeNames[pircoCMPZI]:='cmpzi';
 OpcodeNames[pircoCMPNZI]:='cmpnzi';
 OpcodeNames[pircoCMPSLEL]:='cmpslel';
 OpcodeNames[pircoCMPSLTL]:='cmpsltl';
 OpcodeNames[pircoCMPSGEL]:='cmpsgel';
 OpcodeNames[pircoCMPSGTL]:='cmpsgtl';
 OpcodeNames[pircoCMPULEL]:='cmpulel';
 OpcodeNames[pircoCMPULTL]:='cmpultl';
 OpcodeNames[pircoCMPUGEL]:='cmpugel';
 OpcodeNames[pircoCMPUGTL]:='cmpugtl';
 OpcodeNames[pircoCMPEQL]:='cmpeql';
 OpcodeNames[pircoCMPNEL]:='cmpnel';
 OpcodeNames[pircoCMPZL]:='cmpzl';
 OpcodeNames[pircoCMPNZL]:='cmpnzl';
 OpcodeNames[pircoCMPLEF]:='cmplef';
 OpcodeNames[pircoCMPLTF]:='cmpltf';
 OpcodeNames[pircoCMPGEF]:='cmpgef';
 OpcodeNames[pircoCMPGTF]:='cmpgtf';
 OpcodeNames[pircoCMPEQF]:='cmpeqf';
 OpcodeNames[pircoCMPNEF]:='cmpnef';
 OpcodeNames[pircoCMPOF]:='cmpof';
 OpcodeNames[pircoCMPNOF]:='cmpnof';
 OpcodeNames[pircoCMPLED]:='cmpled';
 OpcodeNames[pircoCMPLTD]:='cmpltd';
 OpcodeNames[pircoCMPGED]:='cmpged';
 OpcodeNames[pircoCMPGTD]:='cmpgtd';
 OpcodeNames[pircoCMPEQD]:='cmpeqd';
 OpcodeNames[pircoCMPNED]:='cmpned';
 OpcodeNames[pircoCMPOD]:='cmpod';
 OpcodeNames[pircoCMPNOD]:='cmpnod';
 OpcodeNames[pircoALLOC]:='alloc';
 OpcodeNames[pircoLVAR]:='lvar';
 OpcodeNames[pircoPARI]:='pari';
 OpcodeNames[pircoPARL]:='parl';
 OpcodeNames[pircoPARF]:='parf';
 OpcodeNames[pircoPARD]:='pard';
 OpcodeNames[pircoARGI]:='argi';
 OpcodeNames[pircoARGL]:='argl';
 OpcodeNames[pircoARGF]:='argf';
 OpcodeNames[pircoARGD]:='argd';
 OpcodeNames[pircoCALL]:='call';
 OpcodeNames[pircoCOUNT]:='count';
end;

procedure InitializeJumpKindNames;
begin
 FillChar(JumpKindNames,SizeOf(JumpKindNames),#0);
 JumpKindNames[pircjkNONE]:='none';
 JumpKindNames[pircjkRET]:='ret';
 JumpKindNames[pircjkRETI]:='reti';
 JumpKindNames[pircjkRETL]:='retl';
 JumpKindNames[pircjkRETF]:='retf';
 JumpKindNames[pircjkRETD]:='retd';
 JumpKindNames[pircjkJMP]:='jmp';
 JumpKindNames[pircjkJMPA]:='jmpa';
 JumpKindNames[pircjkJMPT]:='jmpt';
 JumpKindNames[pircjkJZI]:='jzi';
 JumpKindNames[pircjkJNZI]:='jnzi';
 JumpKindNames[pircjkJZL]:='jzl';
 JumpKindNames[pircjkJNZL]:='jnzl';
 JumpKindNames[pircjkJZF]:='jzf';
 JumpKindNames[pircjkJNZF]:='jnzf';
 JumpKindNames[pircjkJZD]:='jzd';
 JumpKindNames[pircjkJNZD]:='jnzd';
 JumpKindNames[pircjkCOUNT]:='count';
end;

initialization
 InitializeOpcodeNames;
 InitializeJumpKindNames;
end.
