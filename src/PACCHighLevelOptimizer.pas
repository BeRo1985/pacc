unit PACCHighLevelOptimizer;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PasMP,PUCU,PACCRawByteStringHashMap,PACCTypes,PACCGlobals,PACCAbstractSyntaxTree;

type TPACCHighLevelOptimizer=class
      public

       Instance:TObject;

       constructor Create(const AInstance:TObject);
       destructor Destroy; override;

       function SafeForToDiscard(const Node:TPACCAbstractSyntaxTreeNode):boolean;

       function OptimizeNode(var Node:TPACCAbstractSyntaxTreeNode):boolean;

       procedure Process;

     end;

implementation

uses PACCInstance,PACCParser;

constructor TPACCHighLevelOptimizer.Create(const AInstance:TObject);
begin
 inherited Create;
 Instance:=AInstance;
end;

destructor TPACCHighLevelOptimizer.Destroy;
begin
 inherited Destroy;
end;

function TPACCHighLevelOptimizer.SafeForToDiscard(const Node:TPACCAbstractSyntaxTreeNode):boolean;
var Index:TPACCInt;
    SubNode:TPACCAbstractSyntaxTreeNode;
begin
 result:=true;
 if assigned(Node) then begin
  case Node.Kind of
   astnkNONE:begin
   end;
   astnkTRANSLATIONUNIT:begin
    for Index:=0 to TPACCAbstractSyntaxTreeNodeTranslationUnit(Node).Children.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeTranslationUnit(Node).Children[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
   end;
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
    for Index:=0 to TPACCAbstractSyntaxTreeNodeAssembler(Node).InputOperands.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeAssembler(Node).InputOperands[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    for Index:=0 to TPACCAbstractSyntaxTreeNodeAssembler(Node).OutputOperands.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeAssembler(Node).OutputOperands[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    for Index:=0 to TPACCAbstractSyntaxTreeNodeAssembler(Node).Gotos.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeAssembler(Node).Gotos[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
   end;
   astnkASSEMBLER_OPERAND:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeAssemblerOperand(Node).Expression) then begin
     result:=false;
    end;
   end;
   astnkFUNCCALL:begin
    for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).FunctionPointer) then begin
     result:=false;
    end;
    for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Body) then begin
     result:=false;
    end;
   end;
   astnkFUNCPTR_CALL:begin
    for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).FunctionPointer) then begin
     result:=false;
    end;
    for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Body) then begin
     result:=false;
    end;
   end;
   astnkFUNCDESG:begin
    for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).FunctionPointer) then begin
     result:=false;
    end;
    for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Body) then begin
     result:=false;
    end;
   end;
   astnkFUNC:begin
    for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).FunctionPointer) then begin
     result:=false;
    end;
    for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Body) then begin
     result:=false;
    end;
   end;
   astnkDECL:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationVariable) then begin
     result:=false;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationInitialization) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationInitialization.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationInitialization[Index];
      if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
       result:=false;
      end;
     end;
    end;
   end;
   astnkINIT:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeInitializer(Node).InitializionValue) then begin
     result:=false;
    end;
   end;
   astnkCONV:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=false;
    end;
   end;
   astnkADDR:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=false;
    end;
   end;
   astnkDEREF:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=false;
    end;
   end;
   astnkFOR:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Initialization_) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Condition) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Step) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Body) then begin
     result:=false;
    end;
   end;
   astnkDO:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Condition) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Body) then begin
     result:=false;
    end;
   end;
   astnkWHILE:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Condition) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Body) then begin
     result:=false;
    end;
   end;
   astnkSWITCH:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeSWITCHStatement(Node).Value) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeSWITCHStatement(Node).Body) then begin
     result:=false;
    end;
   end;
   astnkIF:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Condition) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Then_) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Else_) then begin
     result:=false;
    end;
   end;
   astnkTERNARY:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Condition) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Then_) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Else_) then begin
     result:=false;
    end;
   end;
   astnkRETURN:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeReturnStatement(Node).ReturnValue) then begin
     result:=false;
    end;
   end;
   astnkSTATEMENTS:begin
    for Index:=0 to TPACCAbstractSyntaxTreeNodeStatements(Node).Children.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeStatements(Node).Children[Index];
     if assigned(SubNode) and not SafeForToDiscard(SubNode) then begin
      result:=false;
     end;
    end;
   end;
   astnkSTRUCT_REF:begin
   end;
   astnkGOTO:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).Label_) then begin
     result:=false;
    end;
   end;
   astnkCOMPUTED_GOTO:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=false;
    end;
   end;
   astnkLABEL:begin
    result:=false;
   end;
   astnkHIDDEN_LABEL:begin
   end;
   astnkBREAK:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBREAKOrContinueStatement(Node).Label_) then begin
     result:=false;
    end;
   end;
   astnkCONTINUE:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBREAKOrContinueStatement(Node).Label_) then begin
     result:=false;
    end;
   end;
   astnkOP_COMMA:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_ASSIGN:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_ASSIGN_OP:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_ASSIGN_SRC:begin
   end;
   astnkOP_CAST:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=false;
    end;
   end;
   astnkOP_NOT:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=false;
    end;
   end;
   astnkOP_NEG:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=false;
    end;
   end;
   astnkOP_PRE_INC:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=false;
    end;
   end;
   astnkOP_PRE_DEC:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=false;
    end;
   end;
   astnkOP_POST_INC:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=false;
    end;
   end;
   astnkOP_POST_DEC:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=false;
    end;
   end;
   astnkOP_LABEL_ADDR:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).Label_) then begin
     result:=false;
    end;
   end;
   astnkOP_ADD:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_SUB:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_MUL:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_DIV:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_MOD:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_AND:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_OR:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_XOR:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_SHL:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_SHR:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_SAR:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_LOG_AND:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_LOG_OR:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_LOG_NOT:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=false;
    end;
   end;
   astnkOP_EQ:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_NE:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_GT:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_LT:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_GE:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
   astnkOP_LE:begin
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=false;
    end;
    if not SafeForToDiscard(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=false;
    end;
   end;
  end;
 end;
end;

function TPACCHighLevelOptimizer.OptimizeNode(var Node:TPACCAbstractSyntaxTreeNode):boolean;
const BoolToInt:array[boolean] of TPACCInt=(0,1);
var Index:TPACCInt;
    SubNode,NewNode:TPACCAbstractSyntaxTreeNode;
begin
 result:=false;
 if assigned(Node) then begin
  case Node.Kind of
   astnkNONE:begin
   end;
   astnkTRANSLATIONUNIT:begin
    for Index:=0 to TPACCAbstractSyntaxTreeNodeTranslationUnit(Node).Children.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeTranslationUnit(Node).Children[Index];
     if assigned(SubNode) then begin
      try
       if OptimizeNode(SubNode) then begin
        result:=true;
       end;
      finally
       TPACCAbstractSyntaxTreeNodeTranslationUnit(Node).Children[Index]:=SubNode;
      end;
     end;
    end;
   end;
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
    if assigned(TPACCAbstractSyntaxTreeNodeAssembler(Node).InputOperands) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeAssembler(Node).InputOperands.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeAssembler(Node).InputOperands[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeAssembler(Node).InputOperands[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeAssembler(Node).OutputOperands) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeAssembler(Node).OutputOperands.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeAssembler(Node).OutputOperands[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeAssembler(Node).OutputOperands[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeAssembler(Node).Gotos) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeAssembler(Node).Gotos.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeAssembler(Node).Gotos[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeAssembler(Node).Gotos[Index]:=SubNode;
       end;
      end;
     end;
    end;
   end;
   astnkASSEMBLER_OPERAND:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeAssemblerOperand(Node).Expression) then begin
     result:=true;
    end;
   end;
   astnkFUNCCALL:begin
    if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).FunctionPointer) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Body) then begin
     result:=true;
    end;
   end;
   astnkFUNCPTR_CALL:begin
    if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).FunctionPointer) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Body) then begin
     result:=true;
    end;
   end;
   astnkFUNCDESG:begin
    if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).FunctionPointer) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Body) then begin
     result:=true;
    end;
   end;
   astnkFUNC:begin
    if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).FunctionPointer) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels[Index]:=SubNode;
       end;
      end;
     end;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Body) then begin
     result:=true;
    end;
   end;
   astnkDECL:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationVariable) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationInitialization) then begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationInitialization.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationInitialization[Index];
      if assigned(SubNode) then begin
       try
        if OptimizeNode(SubNode) then begin
         result:=true;
        end;
       finally
        TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationInitialization[Index]:=SubNode;
       end;
      end;
     end;
    end;
   end;
   astnkINIT:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeInitializer(Node).InitializionValue) then begin
     result:=true;
    end;
   end;
   astnkCONV:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=true;
    end;
    if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Kind=astnkINTEGER) and
       TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_) and
       TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_) then begin
     NewNode:=TPACCAbstractSyntaxTreeNodeFloatValue.Create(Instance,
                                                           astnkFLOAT,
                                                           TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_,
                                                           TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).SourceLocation,
                                                           TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand).Value);
     Node.Free;
     Node:=NewNode;
    end;
   end;
   astnkADDR:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=true;
    end;
   end;
   astnkDEREF:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=true;
    end;
   end;
   astnkFOR:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Initialization_) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Condition) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Step) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Body) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Condition) then begin
     NewNode:=Node;
     if (TPACCAbstractSyntaxTreeNodeFORStatement(Node).Condition.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Condition.Type_) then begin
      if TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Condition).Value=0 then begin
       if SafeForToDiscard(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Body) then begin
        NewNode:=TPACCAbstractSyntaxTreeNodeFORStatement(Node).Initialization_;
        if not assigned(NewNode) then begin
         NewNode:=TPACCAbstractSyntaxTreeNodeStatements.Create(Instance,astnkSTATEMENTS,nil,Node.SourceLocation);
        end;
       end;
      end;
     end;
     if NewNode<>Node then begin
      Node.Free;
      Node:=NewNode;
      result:=true;
     end;
    end;
   end;
   astnkDO:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Condition) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Body) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Condition) then begin
     NewNode:=Node;
     if (TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Condition.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Condition.Type_) then begin
      if TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Condition).Value=0 then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Body;
       if not assigned(NewNode) then begin
        NewNode:=TPACCAbstractSyntaxTreeNodeStatements.Create(Instance,astnkSTATEMENTS,nil,Node.SourceLocation);
       end;
      end;
     end;
     if NewNode<>Node then begin
      Node.Free;
      Node:=NewNode;
      result:=true;
     end;
    end;
   end;
   astnkWHILE:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Condition) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Body) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Condition) then begin
     NewNode:=Node;
     if (TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Condition.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Condition.Type_) then begin
      if TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Condition).Value=0 then begin
       if SafeForToDiscard(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Body) then begin
        NewNode:=TPACCAbstractSyntaxTreeNodeStatements.Create(Instance,astnkSTATEMENTS,nil,Node.SourceLocation);
       end;
      end;
     end;
     if NewNode<>Node then begin
      Node.Free;
      Node:=NewNode;
      result:=true;
     end;
    end;
   end;
   astnkSWITCH:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeSWITCHStatement(Node).Value) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeSWITCHStatement(Node).Body) then begin
     result:=true;
    end;
   end;
   astnkIF:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Condition) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Then_) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Else_) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Condition) then begin
     NewNode:=Node;
     if (TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Condition.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Condition.Type_) then begin
      if TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Condition).Value<>0 then begin
       if SafeForToDiscard(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Else_) then begin
        NewNode:=TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Then_;
       end;
      end else begin
       if SafeForToDiscard(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Then_) then begin
        NewNode:=TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Else_;
       end;
      end;
     end;
     if NewNode<>Node then begin
      if not assigned(NewNode) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeStatements.Create(Instance,astnkSTATEMENTS,nil,Node.SourceLocation);
      end;
      Node.Free;
      Node:=NewNode;
      result:=true;
     end;
    end;
   end;
   astnkTERNARY:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Condition) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Then_) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Else_) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Condition) then begin
     NewNode:=Node;
     if (TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Condition.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Condition.Type_) then begin
      if TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Condition).Value<>0 then begin
       if SafeForToDiscard(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Else_) then begin
        if assigned(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Then_) then begin
         NewNode:=TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Then_;
        end else begin
         NewNode:=TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Condition;
        end;
       end;
      end else begin
       if SafeForToDiscard(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Then_) then begin
        NewNode:=TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Else_;
       end;
      end;
     end;
     if (NewNode<>Node) and assigned(NewNode) then begin
      Node.Free;
      Node:=NewNode;
      result:=true;
     end;
    end;
   end;
   astnkRETURN:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeReturnStatement(Node).ReturnValue) then begin
     result:=true;
    end;
   end;
   astnkSTATEMENTS:begin
    for Index:=0 to TPACCAbstractSyntaxTreeNodeStatements(Node).Children.Count-1 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeStatements(Node).Children[Index];
     if assigned(SubNode) then begin
      try
       if OptimizeNode(SubNode) then begin
        result:=true;
       end;
      finally
       TPACCAbstractSyntaxTreeNodeStatements(Node).Children[Index]:=SubNode;
      end;
     end;
    end;
    for Index:=TPACCAbstractSyntaxTreeNodeStatements(Node).Children.Count-1 downto 0 do begin
     SubNode:=TPACCAbstractSyntaxTreeNodeStatements(Node).Children[Index];
     if (not assigned(SubNode)) or
        ((SubNode.Kind=astnkSTATEMENTS) and
         (TPACCAbstractSyntaxTreeNodeStatements(SubNode).Children.Count=0)) then begin
      TPACCAbstractSyntaxTreeNodeStatements(Node).Children.Delete(Index);
      result:=true;
     end;
    end;
   end;
   astnkSTRUCT_REF:begin
   end;
   astnkGOTO:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).Label_) then begin
     result:=true;
    end;
   end;
   astnkCOMPUTED_GOTO:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=true;
    end;
   end;
   astnkLABEL:begin
   end;
   astnkHIDDEN_LABEL:begin
   end;
   astnkBREAK:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBREAKOrContinueStatement(Node).Label_) then begin
     result:=true;
    end;
   end;
   astnkCONTINUE:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBREAKOrContinueStatement(Node).Label_) then begin
     result:=true;
    end;
   end;
   astnkOP_COMMA:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
   end;
   astnkOP_ASSIGN:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
   end;
   astnkOP_ASSIGN_OP:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
   end;
   astnkOP_ASSIGN_SRC:begin
   end;
   astnkOP_CAST:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=true;
    end;
   end;
   astnkOP_NOT:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_) then begin
      NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                              astnkINTEGER,
                                                              TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_,
                                                              TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).SourceLocation,
                                                              not TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand).Value);
      Node.Free;
      Node:=NewNode;
      result:=true;
     end;
    end;
   end;
   astnkOP_NEG:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_) then begin
      NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                              astnkINTEGER,
                                                              TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_,
                                                              TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).SourceLocation,
                                                              -TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand).Value);
      Node.Free;
      Node:=NewNode;
      result:=true;
     end else if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Kind=astnkFLOAT) and
                 TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_) and
                 TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_) then begin
      NewNode:=TPACCAbstractSyntaxTreeNodeFloatValue.Create(Instance,
                                                            astnkFLOAT,
                                                            TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_,
                                                            TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).SourceLocation,
                                                            -TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand).Value);
      Node.Free;
      Node:=NewNode;
      result:=true;
     end;
    end;
   end;
   astnkOP_PRE_INC:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=true;
    end;
   end;
   astnkOP_PRE_DEC:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=true;
    end;
   end;
   astnkOP_POST_INC:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=true;
    end;
   end;
   astnkOP_POST_DEC:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=true;
    end;
   end;
   astnkOP_LABEL_ADDR:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).Label_) then begin
     result:=true;
    end;
   end;
   astnkOP_ADD:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
       assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value+TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeFloatValue.Create(Instance,
                                                             astnkFLOAT,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                             TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value+TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkFLOAT) and
                 TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) and
                 TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeFloatValue.Create(Instance,
                                                             astnkFLOAT,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                             TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value+TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeFloatValue.Create(Instance,
                                                             astnkFLOAT,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                             TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value+TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end;
    end;
   end;
   astnkOP_SUB:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
       assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value-TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeFloatValue.Create(Instance,
                                                             astnkFLOAT,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                             TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value-TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkFLOAT) and
                 TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) and
                 TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeFloatValue.Create(Instance,
                                                             astnkFLOAT,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                             TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value-TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeFloatValue.Create(Instance,
                                                             astnkFLOAT,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                             TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value-TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end;
    end;
   end;
   astnkOP_MUL:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
       assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value*TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeFloatValue.Create(Instance,
                                                             astnkFLOAT,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                             TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value*TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkFLOAT) and
                 TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) and
                 TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeFloatValue.Create(Instance,
                                                             astnkFLOAT,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                             TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value*TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeFloatValue.Create(Instance,
                                                             astnkFLOAT,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                             TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value*TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end;
    end;
   end;
   astnkOP_DIV:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
       assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value div TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeFloatValue.Create(Instance,
                                                             astnkFLOAT,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                             TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value/TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkFLOAT) and
                 TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) and
                 TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeFloatValue.Create(Instance,
                                                             astnkFLOAT,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                             TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value/TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeFloatValue.Create(Instance,
                                                             astnkFLOAT,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                             TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                             TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value/TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end;
    end;
   end;
   astnkOP_MOD:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
       assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value mod TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end;
    end;
   end;
   astnkOP_AND:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
       assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value and TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end;
    end;
   end;
   astnkOP_OR:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
       assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value or TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end;
    end;
   end;
   astnkOP_XOR:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
       assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value xor TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end;
    end;
   end;
   astnkOP_SHL:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
       assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value shl TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end;
    end;
   end;
   astnkOP_SHR:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
       assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value shr TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end;
    end;
   end;
   astnkOP_SAR:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
       assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               SARCInt64(TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value,TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value));
       Node.Free;
       Node:=NewNode;
      end;
     end;
    end;
   end;
   astnkOP_LOG_AND:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
   end;
   astnkOP_LOG_OR:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
   end;
   astnkOP_LOG_NOT:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand) then begin
     if (TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand.Type_) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_) then begin
      if TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand).Value<>0 then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).SourceLocation,
                                                               1);
      end else begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).SourceLocation,
                                                               0);
      end;
      Node.Free;
      Node:=NewNode;
      result:=true;
     end;
    end;
   end;
   astnkOP_EQ:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
       assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value=TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
                  TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value=TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkFLOAT) and
                 TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) and
                 TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value=TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value=TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end;
    end;
   end;
   astnkOP_NE:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
       assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value<>TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
                  TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value<>TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkFLOAT) and
                 TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) and
                 TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value<>TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value<>TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end;
    end;
   end;
   astnkOP_GT:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
       assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value>TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
                  TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value>TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkFLOAT) and
                 TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) and
                 TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value>TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value>TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end;
    end;
   end;
   astnkOP_LT:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
       assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value<TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
                  TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value<TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkFLOAT) and
                 TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) and
                 TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value<TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value<TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end;
    end;
   end;
   astnkOP_GE:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
       assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value>=TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
                  TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value>=TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkFLOAT) and
                 TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) and
                 TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value>=TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value>=TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end;
    end;
   end;
   astnkOP_LE:begin
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) then begin
     result:=true;
    end;
    if OptimizeNode(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     result:=true;
    end;
    if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
       assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right) then begin
     if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkINTEGER) and
        TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value<=TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) and
                  TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value<=TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkFLOAT) and
                 TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Type_) and
                 TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_) then begin
      if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkINTEGER) and
         TPACCInstance(Instance).IsIntType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value<=TPACCAbstractSyntaxTreeNodeIntegerValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end else if (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Kind=astnkFLOAT) and
                  TPACCInstance(Instance).IsFloatType(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right.Type_) then begin
       NewNode:=TPACCAbstractSyntaxTreeNodeIntegerValue.Create(Instance,
                                                               astnkINTEGER,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Type_,
                                                               TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).SourceLocation,
                                                               BoolToInt[TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Value<=TPACCAbstractSyntaxTreeNodeFloatValue(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right).Value]);
       Node.Free;
       Node:=NewNode;
       result:=true;
      end;
     end;
    end;
   end;
  end;
 end;
end;

procedure TPACCHighLevelOptimizer.Process;
var Index:TPACCInt;
    Root:TPACCAbstractSyntaxTreeNodeTranslationUnit;
    Node:TPACCAbstractSyntaxTreeNode;
    TryAgain:boolean;
begin
 Root:=TPACCParser(TPACCInstance(Instance).Parser).Root;
 if assigned(Root) then begin
  repeat
   TryAgain:=false;
   for Index:=0 to Root.Children.Count-1 do begin
    Node:=Root.Children[Index];
    if assigned(Node) then begin
     try
      if OptimizeNode(Node) then begin
       TryAgain:=true;
      end;
     finally
      Root.Children[Index]:=Node;
     end;
    end;
   end;
  until not TryAgain; 
 end;
end;

end.
