unit PACCAbstractSyntaxTree;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals;

type PPACCAbstractSyntaxTreeNodeKind=^TPACCAbstractSyntaxTreeNodeKind;
     TPACCAbstractSyntaxTreeNodeKind=(
      astnkNONE,
      astnkTRANSLATIONUNIT,
      astnkINTEGER,
      astnkFLOAT,
      astnkSTRING,
      astnkLVAR,
      astnkGVAR,
      astnkTYPEDEF,
      astnkASSEMBLER,
      astnkASSEMBLER_OPERAND,
      astnkFUNCCALL,
      astnkFUNCPTR_CALL,
      astnkFUNCDESG,
      astnkFUNC,
      astnkEXTERN_DECL,
      astnkDECL,
      astnkINIT,
      astnkCONV,
      astnkADDR,
      astnkDEREF,
      astnkFOR,
      astnkDO,
      astnkWHILE,
      astnkSWITCH,
      astnkIF,
      astnkTERNARY,
      astnkRETURN,
      astnkSTATEMENTS,
      astnkSTRUCT_REF,
      astnkGOTO,
      astnkCOMPUTED_GOTO,
      astnkLABEL,
      astnkHIDDEN_LABEL,
      astnkBREAK,
      astnkCONTINUE,
      astnkOP_COMMA,
      astnkOP_ARROW,
      astnkOP_ASSIGN,
      astnkOP_SIZEOF,
      astnkOP_CAST,
      astnkOP_NOT,
      astnkOP_NEG,
      astnkOP_PRE_INC,
      astnkOP_PRE_DEC,
      astnkOP_POST_INC,
      astnkOP_POST_DEC,
      astnkOP_LABEL_ADDR,
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
      astnkOP_SAR,
      astnkOP_LOG_AND,
      astnkOP_LOG_OR,
      astnkOP_LOG_NOT,
      astnkOP_EQ,
      astnkOP_NE,
      astnkOP_GT,
      astnkOP_LT,
      astnkOP_GE,
      astnkOP_LE,
      astnkOP_A_ADD,
      astnkOP_A_SUB,
      astnkOP_A_MUL,
      astnkOP_A_DIV,
      astnkOP_A_MOD,
      astnkOP_A_AND,
      astnkOP_A_OR,
      astnkOP_A_XOR,
      astnkOP_A_SHR,
      astnkOP_A_SHL,
      astnkOP_A_SAL,
      astnkOP_A_SAR
     );

     TPACCAbstractSyntaxTreeNode=class;

     TPACCAbstractSyntaxTreeNodeList=class(TList)
      private
       function GetNode(const Index:TPACCInt):TPACCAbstractSyntaxTreeNode;
       procedure SetNode(const Index:TPACCInt;Node:TPACCAbstractSyntaxTreeNode);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const Index:TPACCInt]:TPACCAbstractSyntaxTreeNode read GetNode write SetNode;
       property Nodes[const Index:TPACCInt]:TPACCAbstractSyntaxTreeNode read GetNode write SetNode; default;
     end;

     PPACCAbstractSyntaxTreeNode=^TPACCAbstractSyntaxTreeNode;

     TPACCAbstractSyntaxTreeNode=class
      public
       Instance:TObject;
       Kind:TPACCAbstractSyntaxTreeNodeKind;
       Type_:PPACCType;
       SourceLocation:TPACCSourceLocation;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation); reintroduce; virtual;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeTranslationUnit=class(TPACCAbstractSyntaxTreeNode)
      public
       Children:TPACCAbstractSyntaxTreeNodeList;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation); override;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeLiteral=class(TPACCAbstractSyntaxTreeNode)
      public
       Value:TPACCRawByteString;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AValue:TPACCRawByteString); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeAssemblerOperand=class(TPACCAbstractSyntaxTreeNode)
      public
       Identifier:TPACCRawByteString;
       Constraint:TPACCRawByteString;
       Expression:TPACCAbstractSyntaxTreeNode;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AIdentifier,AConstraint:TPACCRawByteString;const AExpression:TPACCAbstractSyntaxTreeNode); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeAssembler=class(TPACCAbstractSyntaxTreeNode)
      public
       Global:boolean;
       Volatile:boolean;
       WithGotos:boolean;
       Code:TPACCRawByteString;
       InputOperands:TPACCAbstractSyntaxTreeNodeList;
       OutputOperands:TPACCAbstractSyntaxTreeNodeList;
       Clobbers:TStringList;
       Gotos:TPACCAbstractSyntaxTreeNodeList;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AGlobal,AVolatile,AWithGotos:boolean;const ACode:TPACCRawByteString); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeIntegerValue=class(TPACCAbstractSyntaxTreeNode)
      public
       Value:TPACCInt64;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AValue:TPACCInt64); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeFloatValue=class(TPACCAbstractSyntaxTreeNode)
      public
       Value:TPACCLongDouble;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AValue:TPACCLongDouble); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeStringValue=class(TPACCAbstractSyntaxTreeNode)
      public
       Value:TPACCRawByteString;
       Encoding:TPACCEncoding;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AValue:TPACCRawByteString;const AEncoding:TPACCEncoding); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeLocalGlobalVariable=class(TPACCAbstractSyntaxTreeNode)
      public
       VariableName:TPACCRawByteString;
       Index:TPACCInt;
       LocalOffset:TPACCInt;
       LocalVariableInitialization:TPACCAbstractSyntaxTreeNodeList;
       GlobalLabelName:TPACCRawByteString;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AVariableName:TPACCRawByteString;const ALocalOffset:TPACCInt;const AGlobalLabelName:TPACCRawByteString); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeBinaryOperator=class(TPACCAbstractSyntaxTreeNode)
      public
       Left:TPACCAbstractSyntaxTreeNode;
       Right:TPACCAbstractSyntaxTreeNode;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const ALeft,ARight:TPACCAbstractSyntaxTreeNode); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeUnaryOperator=class(TPACCAbstractSyntaxTreeNode)
      public
       Operand:TPACCAbstractSyntaxTreeNode;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AOperand:TPACCAbstractSyntaxTreeNode); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration=class(TPACCAbstractSyntaxTreeNode)
      public
       FunctionName:TPACCRawByteString;
       Variable:TPACCAbstractSyntaxTreeNode;
       Arguments:TPACCAbstractSyntaxTreeNodeList;
       FunctionType:PPACCType;
       FunctionPointer:TPACCAbstractSyntaxTreeNode;
       Parameters:TPACCAbstractSyntaxTreeNodeList;
       LocalVariables:TPACCAbstractSyntaxTreeNodeList;
       Labels:TPACCAbstractSyntaxTreeNodeList;
       Body:TPACCAbstractSyntaxTreeNode;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AFunctionName:TPACCRawByteString;const AFunctionType:PPACCType;const AFunctionPointer,ABody:TPACCAbstractSyntaxTreeNode); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeDeclaration=class(TPACCAbstractSyntaxTreeNode)
      public
       DeclarationVariable:TPACCAbstractSyntaxTreeNode;
       DeclarationInitialization:TPACCAbstractSyntaxTreeNodeList;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const ADeclarationVariable:TPACCAbstractSyntaxTreeNode;const ADeclarationInitialization:TPACCAbstractSyntaxTreeNodeList); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeInitializer=class(TPACCAbstractSyntaxTreeNode)
      public
       InitializionValue:TPACCAbstractSyntaxTreeNode;
       InitializionOffset:TPACCInt64;
       ToType:PPACCType;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AInitializionValue:TPACCAbstractSyntaxTreeNode;const AInitializionOffset:TPACCInt64;const AToType:PPACCType); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeFORStatement=class(TPACCAbstractSyntaxTreeNode)
      public
       Initialization_:TPACCAbstractSyntaxTreeNode;
       Condition:TPACCAbstractSyntaxTreeNode;
       Step:TPACCAbstractSyntaxTreeNode;
       Body:TPACCAbstractSyntaxTreeNode;
       BreakLabel:TPACCAbstractSyntaxTreeNode;
       ContinueLabel:TPACCAbstractSyntaxTreeNode;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AInitialization_,ACondition,AStep,ABody,ABreakLabel,AContinueLabel:TPACCAbstractSyntaxTreeNode); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeWHILEOrDOStatement=class(TPACCAbstractSyntaxTreeNode)
      public
       Condition:TPACCAbstractSyntaxTreeNode;
       Body:TPACCAbstractSyntaxTreeNode;
       BreakLabel:TPACCAbstractSyntaxTreeNode;
       ContinueLabel:TPACCAbstractSyntaxTreeNode;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const ACondition,ABody,ABreakLabel,AContinueLabel:TPACCAbstractSyntaxTreeNode); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator=class(TPACCAbstractSyntaxTreeNode)
      public
       Condition:TPACCAbstractSyntaxTreeNode;
       Then_:TPACCAbstractSyntaxTreeNode;
       Else_:TPACCAbstractSyntaxTreeNode;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const ACondition,AThen_,AElse_:TPACCAbstractSyntaxTreeNode); reintroduce;
       destructor Destroy; override;
     end;

     PPACCAbstractSyntaxTreeNodeSWITCHStatementCase=^TPACCAbstractSyntaxTreeNodeSWITCHStatementCase;
     TPACCAbstractSyntaxTreeNodeSWITCHStatementCase=record
      CaseBegin:TPACCInt64;
      CaseEnd:TPACCInt64;
      CaseLabel:TPACCAbstractSyntaxTreeNode;
     end;

     TPACCAbstractSyntaxTreeNodeSWITCHStatementCases=array of TPACCAbstractSyntaxTreeNodeSWITCHStatementCase;

     TPACCAbstractSyntaxTreeNodeSWITCHStatement=class(TPACCAbstractSyntaxTreeNode)
      public
       Value:TPACCAbstractSyntaxTreeNode;
       Body:TPACCAbstractSyntaxTreeNode;
       SwitchBreakLabel:TPACCAbstractSyntaxTreeNode;
       DefaultCaseLabel:TPACCAbstractSyntaxTreeNode;
       Cases:TPACCAbstractSyntaxTreeNodeSWITCHStatementCases;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AValue,ABody,ASwitchBreakLabel,ADefaultCaseLabel:TPACCAbstractSyntaxTreeNode;const ACases:TPACCAbstractSyntaxTreeNodeSWITCHStatementCases); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeLabel=class(TPACCAbstractSyntaxTreeNode)
      public
       LabelName:TPACCRawByteString;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const ALabelName:TPACCRawByteString); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress=class(TPACCAbstractSyntaxTreeNode)
      public
       Label_:TPACCAbstractSyntaxTreeNode;
       LabelName:TPACCRawByteString;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const ALabel_:TPACCAbstractSyntaxTreeNode;const ALabelName:TPACCRawByteString); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeBREAKOrCONTINUEStatement=class(TPACCAbstractSyntaxTreeNode)
      public
       Label_:TPACCAbstractSyntaxTreeNode;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const ALabel_:TPACCAbstractSyntaxTreeNode); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeReturnStatement=class(TPACCAbstractSyntaxTreeNode)
      public
       ReturnValue:TPACCAbstractSyntaxTreeNode;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AReturnValue:TPACCAbstractSyntaxTreeNode); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeStatements=class(TPACCAbstractSyntaxTreeNode)
      public
       Children:TPACCAbstractSyntaxTreeNodeList;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation); reintroduce;
       destructor Destroy; override;
     end;

     TPACCAbstractSyntaxTreeNodeStructReference=class(TPACCAbstractSyntaxTreeNode)
      public
       Struct:TPACCAbstractSyntaxTreeNode;
       FieldName:TPACCRawByteString;
       constructor Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AStruct:TPACCAbstractSyntaxTreeNode;const AFieldName:TPACCRawByteString); reintroduce;
       destructor Destroy; override;
     end;

implementation

uses PACCInstance;

constructor TPACCAbstractSyntaxTreeNodeList.Create;
begin
 inherited Create;
end;

destructor TPACCAbstractSyntaxTreeNodeList.Destroy;
begin
 inherited Destroy;
end;

function TPACCAbstractSyntaxTreeNodeList.GetNode(const Index:TPACCInt):TPACCAbstractSyntaxTreeNode;
begin
 result:=pointer(inherited Items[Index]);
end;

procedure TPACCAbstractSyntaxTreeNodeList.SetNode(const Index:TPACCInt;Node:TPACCAbstractSyntaxTreeNode);
begin
 inherited Items[Index]:=pointer(Node);
end;

constructor TPACCAbstractSyntaxTreeNode.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation);
begin
 inherited Create;
 Instance:=AInstance;
 Kind:=AKind;
 Type_:=AType;
 SourceLocation:=ASourceLocation;
 if assigned(Instance) then begin
  TPACCInstance(Instance).Nodes.Add(self);
 end;
end;

destructor TPACCAbstractSyntaxTreeNode.Destroy;
begin
 if assigned(Instance) then begin
  TPACCInstance(Instance).Nodes.Remove(self);
 end;
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeTranslationUnit.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 Children:=TPACCAbstractSyntaxTreeNodeList.Create;
end;

destructor TPACCAbstractSyntaxTreeNodeTranslationUnit.Destroy;
begin
 Children.Free;
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeLiteral.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AValue:TPACCRawByteString);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 Value:=AValue;
end;

destructor TPACCAbstractSyntaxTreeNodeLiteral.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeAssemblerOperand.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AIdentifier,AConstraint:TPACCRawByteString;const AExpression:TPACCAbstractSyntaxTreeNode);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 Identifier:=AIdentifier;
 Constraint:=AConstraint;
 Expression:=AExpression;
end;

destructor TPACCAbstractSyntaxTreeNodeAssemblerOperand.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeAssembler.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AGlobal,AVolatile,AWithGotos:boolean;const ACode:TPACCRawByteString);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 Global:=AGlobal;
 Volatile:=AVolatile;
 WithGotos:=AWithGotos;
 Code:=ACode;
 InputOperands:=nil;
 OutputOperands:=nil;
 Clobbers:=TStringList.Create;
 Gotos:=nil;
end;

destructor TPACCAbstractSyntaxTreeNodeAssembler.Destroy;
begin
 Clobbers.Free;
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeIntegerValue.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AValue:TPACCInt64);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 Value:=AValue;
end;

destructor TPACCAbstractSyntaxTreeNodeIntegerValue.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeFloatValue.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AValue:TPACCLongDouble);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 Value:=AValue;
end;

destructor TPACCAbstractSyntaxTreeNodeFloatValue.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeStringValue.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AValue:TPACCRawByteString;const AEncoding:TPACCEncoding);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 Value:=AValue;
 Encoding:=AEncoding;
end;

destructor TPACCAbstractSyntaxTreeNodeStringValue.Destroy;
begin
 Value:='';
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeLocalGlobalVariable.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AVariableName:TPACCRawByteString;const ALocalOffset:TPACCInt;const AGlobalLabelName:TPACCRawByteString);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 VariableName:=AVariableName;
 LocalVariableInitialization:=nil;
 GlobalLabelName:=AGlobalLabelName;
end;

destructor TPACCAbstractSyntaxTreeNodeLocalGlobalVariable.Destroy;
begin
 VariableName:='';
 GlobalLabelName:='';
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeBinaryOperator.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const ALeft,ARight:TPACCAbstractSyntaxTreeNode);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 Left:=ALeft;
 Right:=ARight;
end;

destructor TPACCAbstractSyntaxTreeNodeBinaryOperator.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeUnaryOperator.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AOperand:TPACCAbstractSyntaxTreeNode);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 Operand:=AOperand;
end;

destructor TPACCAbstractSyntaxTreeNodeUnaryOperator.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AFunctionName:TPACCRawByteString;const AFunctionType:PPACCType;const AFunctionPointer,ABody:TPACCAbstractSyntaxTreeNode);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 FunctionName:=AFunctionName;
 Arguments:=nil;
 FunctionType:=AFunctionType;
 FunctionPointer:=AFunctionPointer;
 Parameters:=nil;
 LocalVariables:=nil;
 Labels:=nil;
 Body:=ABody;
end;

destructor TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration.Destroy;
begin
 FunctionName:='';
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeDeclaration.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const ADeclarationVariable:TPACCAbstractSyntaxTreeNode;const ADeclarationInitialization:TPACCAbstractSyntaxTreeNodeList);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 DeclarationVariable:=ADeclarationVariable;
 DeclarationInitialization:=ADeclarationInitialization;
end;

destructor TPACCAbstractSyntaxTreeNodeDeclaration.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeInitializer.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AInitializionValue:TPACCAbstractSyntaxTreeNode;const AInitializionOffset:TPACCInt64;const AToType:PPACCType);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 InitializionValue:=AInitializionValue;
 InitializionOffset:=AInitializionOffset;
 ToType:=AToType;
end;

destructor TPACCAbstractSyntaxTreeNodeInitializer.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeFORStatement.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AInitialization_,ACondition,AStep,ABody,ABreakLabel,AContinueLabel:TPACCAbstractSyntaxTreeNode);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 Initialization_:=AInitialization_;
 Condition:=ACondition;
 Step:=AStep;
 Body:=ABody;
 BreakLabel:=ABreakLabel;
 ContinueLabel:=AContinueLabel;
end;

destructor TPACCAbstractSyntaxTreeNodeFORStatement.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeWHILEOrDOStatement.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const ACondition,ABody,ABreakLabel,AContinueLabel:TPACCAbstractSyntaxTreeNode);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 Condition:=ACondition;
 Body:=ABody;
 BreakLabel:=ABreakLabel;
 ContinueLabel:=AContinueLabel;
end;

destructor TPACCAbstractSyntaxTreeNodeWHILEOrDOStatement.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const ACondition,AThen_,AElse_:TPACCAbstractSyntaxTreeNode);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 Condition:=ACondition;
 Then_:=AThen_;
 Else_:=AElse_;
end;

destructor TPACCAbstractSyntaxTreeNodeIfStatementOrTernaryOperator.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeSWITCHStatement.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AValue,ABody,ASwitchBreakLabel,ADefaultCaseLabel:TPACCAbstractSyntaxTreeNode;const ACases:TPACCAbstractSyntaxTreeNodeSWITCHStatementCases);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 Value:=AValue;
 Body:=ABody;
 SwitchBreakLabel:=ASwitchBreakLabel;
 DefaultCaseLabel:=ADefaultCaseLabel;
 Cases:=ACases;
end;

destructor TPACCAbstractSyntaxTreeNodeSWITCHStatement.Destroy;
begin
 Cases:=nil;
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeLabel.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const ALabelName:TPACCRawByteString);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 LabelName:=ALabelName;
end;

destructor TPACCAbstractSyntaxTreeNodeLabel.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const ALabel_:TPACCAbstractSyntaxTreeNode;const ALabelName:TPACCRawByteString);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 Label_:=ALabel_;
 LabelName:=ALabelName;
end;

destructor TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeBREAKOrCONTINUEStatement.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const ALabel_:TPACCAbstractSyntaxTreeNode);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 Label_:=ALabel_;
end;

destructor TPACCAbstractSyntaxTreeNodeBREAKOrCONTINUEStatement.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeReturnStatement.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AReturnValue:TPACCAbstractSyntaxTreeNode);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 ReturnValue:=AReturnValue;
end;

destructor TPACCAbstractSyntaxTreeNodeReturnStatement.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeStatements.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 Children:=TPACCAbstractSyntaxTreeNodeList.Create;
end;

destructor TPACCAbstractSyntaxTreeNodeStatements.Destroy;
begin
 Children.Free;
 inherited Destroy;
end;

constructor TPACCAbstractSyntaxTreeNodeStructReference.Create(const AInstance:TObject;const AKind:TPACCAbstractSyntaxTreeNodeKind;const AType:PPACCType;const ASourceLocation:TPACCSourceLocation;const AStruct:TPACCAbstractSyntaxTreeNode;const AFieldName:TPACCRawByteString);
begin
 inherited Create(AInstance,AKind,AType,ASourceLocation);
 Struct:=AStruct;
 FieldName:=AFieldName;
end;

destructor TPACCAbstractSyntaxTreeNodeStructReference.Destroy;
begin
 inherited Destroy;
end;

end.
