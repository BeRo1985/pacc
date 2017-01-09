unit PACCAnalyzer;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PasMP,PUCU,PACCRawByteStringHashMap,PACCPointerHashMap,PACCTypes,PACCGlobals,
      PACCAbstractSyntaxTree;

type TPACCAnalyzer=class
      public

       Instance:TObject;

       constructor Create(const AInstance:TObject);
       destructor Destroy; override;

       procedure Process;

     end;

implementation

uses PACCInstance,PACCParser;

constructor TPACCAnalyzer.Create(const AInstance:TObject);
begin
 inherited Create;
 Instance:=AInstance;
end;

destructor TPACCAnalyzer.Destroy;
begin
 inherited Destroy;
end;

procedure TPACCAnalyzer.Process;
type TBooleans=array of boolean;
var FunctionUsedVariablesHashMap:TPACCPointerHashMap;
    FunctionUsedVariablesList:TList;
    FunctionInitializedVariables:TBooleans;
    FunctionUninitializedAccessVariables:TBooleans;
 procedure AddError(const s:TPUCUUTF8String;const SourceLocation:PPACCSourceLocation=nil;const DoAbort:boolean=false);
 begin
  TPACCInstance(Instance).AddError(s,SourceLocation,DoAbort);
 end;
 procedure AddWarning(const s:TPUCUUTF8String;const SourceLocation:PPACCSourceLocation=nil);
 begin
  TPACCInstance(Instance).AddWarning(s,SourceLocation);
 end;
 procedure Scan(const Node:TPACCAbstractSyntaxTreeNode);
 var Index:TPACCInt;
     SubNode:TPACCAbstractSyntaxTreeNode;
     LastFunctionInitializedVariables,LastFunctionUninitializedAccessVariables:TBooleans;
     OtherFunctionInitializedVariables,OtherFunctionUninitializedAccessVariables:TBooleans;
 begin
  if assigned(Node) then begin
   case Node.Kind of
    astnkNONE:begin
    end;
    astnkTRANSLATIONUNIT:begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeTranslationUnit(Node).Children.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeTranslationUnit(Node).Children[Index];
      if assigned(SubNode) then begin
       Scan(SubNode);
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
     if assigned(FunctionUsedVariablesHashMap) then begin
      FunctionUsedVariablesHashMap[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node)]:=TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node);
      if (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node).Index>=0) and
         (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node).Index<length(FunctionInitializedVariables)) then begin
       if not FunctionInitializedVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node).Index] then begin
        FunctionUninitializedAccessVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(Node).Index]:=true;       
       end;
      end;
     end;
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
        Scan(SubNode);
       end;
      end;
     end;
     if assigned(TPACCAbstractSyntaxTreeNodeAssembler(Node).OutputOperands) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeAssembler(Node).OutputOperands.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeAssembler(Node).OutputOperands[Index];
       if assigned(SubNode) then begin
        if (SubNode.Kind=astnkASSEMBLER_OPERAND) and
           assigned(TPACCAbstractSyntaxTreeNodeAssemblerOperand(SubNode).Expression) and
           (TPACCAbstractSyntaxTreeNodeAssemblerOperand(SubNode).Expression.Kind=astnkLVAR) and
           (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeAssemblerOperand(SubNode).Expression).Index>=0) and
           (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeAssemblerOperand(SubNode).Expression).Index<length(FunctionInitializedVariables)) then begin
         FunctionInitializedVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeAssemblerOperand(SubNode).Expression).Index]:=true;
        end;
        Scan(SubNode);
       end;
      end;
     end;
     if assigned(TPACCAbstractSyntaxTreeNodeAssembler(Node).Gotos) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeAssembler(Node).Gotos.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeAssembler(Node).Gotos[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
     end;
    end;
    astnkASSEMBLER_OPERAND:begin
     Scan(TPACCAbstractSyntaxTreeNodeAssemblerOperand(Node).Expression);
    end;
    astnkFUNCCALL:begin
     if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
     end;
     Scan(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).FunctionPointer);
     if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
     end;
     if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
     end;
     if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
     end;
     Scan(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Body);
    end;
    astnkFUNCPTR_CALL:begin
     if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
     end;
     Scan(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).FunctionPointer);
     if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
     end;
     if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
     end;
     if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
     end;
     Scan(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Body);
    end;
    astnkFUNCDESG:begin
     if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
     end;
     Scan(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).FunctionPointer);
     if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
     end;
     if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
     end;
     if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
     end;
     Scan(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Body);
    end;
    astnkFUNC:begin
     if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Arguments[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
     end;
     Scan(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).FunctionPointer);
     if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
     end;
     if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
     end;
     if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Labels[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
     end;
     FunctionUsedVariablesHashMap:=TPACCPointerHashMap.Create;
     FunctionUsedVariablesList:=TList.Create;
     try
      SetLength(FunctionInitializedVariables,TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables.Count);
      SetLength(FunctionUninitializedAccessVariables,TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables.Count);
      for Index:=0 to length(FunctionInitializedVariables)-1 do begin
       FunctionInitializedVariables[Index]:=false;
       FunctionUninitializedAccessVariables[Index]:=false;
      end;
      if assigned(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters) then begin
       for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters.Count-1 do begin
        SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Parameters[Index];
        if assigned(SubNode) and (SubNode.Kind=astnkLVAR) then begin
         if (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(SubNode).Index>=0) and
            (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(SubNode).Index<length(FunctionInitializedVariables)) then begin
          FunctionInitializedVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(SubNode).Index]:=true;
         end;
        end;
       end;
      end;
      Scan(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).Body);
      for Index:=0 to TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node).LocalVariables[Index];
       if assigned(SubNode) and (SubNode.Kind=astnkLVAR) then begin
        if not assigned(FunctionUsedVariablesHashMap[SubNode]) then begin
         AddWarning('Unused variable "'+TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(SubNode).VariableName+'"',@SubNode.SourceLocation);
        end;
        if (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(SubNode).Index>=0) and
           (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(SubNode).Index<length(FunctionUninitializedAccessVariables)) then begin
         if FunctionUninitializedAccessVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(SubNode).Index] then begin
          AddWarning('Read access to potentially non-initialized variable "'+TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(SubNode).VariableName+'"',@SubNode.SourceLocation);
         end;
        end;
       end;
      end;
     finally
      FunctionInitializedVariables:=nil;
      FunctionUninitializedAccessVariables:=nil;
      FreeAndNil(FunctionUsedVariablesHashMap);
      FreeAndNil(FunctionUsedVariablesList);
     end;
    end;
    astnkDECL:begin
     if assigned(TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationInitialization) then begin
      for Index:=0 to TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationInitialization.Count-1 do begin
       SubNode:=TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationInitialization[Index];
       if assigned(SubNode) then begin
        Scan(SubNode);
       end;
      end;
      if assigned(TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationVariable) and
         (TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationVariable.Kind=astnkLVAR) and
         (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationVariable).Index>=0) and
         (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationVariable).Index<length(FunctionInitializedVariables)) then begin
       FunctionInitializedVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationVariable).Index]:=true;
      end;
      Scan(TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationVariable);
      if assigned(TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationVariable) and
         (TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationVariable.Kind=astnkLVAR) and
         assigned(FunctionUsedVariablesHashMap) then begin
       FunctionUsedVariablesHashMap.Delete(TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeDeclaration(Node).DeclarationVariable));
      end;
     end;
    end;
    astnkINIT:begin
     Scan(TPACCAbstractSyntaxTreeNodeInitializer(Node).InitializionValue);
    end;
    astnkCONV:begin
     Scan(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand);
    end;
    astnkADDR:begin
     Scan(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand);
    end;
    astnkDEREF:begin
     Scan(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand);
    end;
    astnkFOR:begin
     Scan(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Initialization_);
     Scan(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Condition);
     Scan(TPACCAbstractSyntaxTreeNodeFORStatement(Node).BreakLabel);
     Scan(TPACCAbstractSyntaxTreeNodeFORStatement(Node).ContinueLabel);
     LastFunctionInitializedVariables:=copy(FunctionInitializedVariables);
     LastFunctionUninitializedAccessVariables:=copy(FunctionUninitializedAccessVariables);
     try
      Scan(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Step);
      Scan(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Body);
     finally
      FunctionInitializedVariables:=copy(LastFunctionInitializedVariables);
      FunctionUninitializedAccessVariables:=copy(LastFunctionUninitializedAccessVariables);
     end;
    end;
    astnkDO:begin
     Scan(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Condition);
     Scan(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).BreakLabel);
     Scan(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).ContinueLabel);
     Scan(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Body);
    end;
    astnkWHILE:begin
     Scan(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Condition);
     Scan(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).BreakLabel);
     Scan(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).ContinueLabel);
     LastFunctionInitializedVariables:=copy(FunctionInitializedVariables);
     LastFunctionUninitializedAccessVariables:=copy(FunctionUninitializedAccessVariables);
     try
      Scan(TPACCAbstractSyntaxTreeNodeWHILEorDOStatement(Node).Body);
     finally
      FunctionInitializedVariables:=copy(LastFunctionInitializedVariables);
      FunctionUninitializedAccessVariables:=copy(LastFunctionUninitializedAccessVariables);
     end;
    end;
    astnkSWITCH:begin
     Scan(TPACCAbstractSyntaxTreeNodeSWITCHStatement(Node).Value);
     Scan(TPACCAbstractSyntaxTreeNodeSWITCHStatement(Node).SwitchBreakLabel);
     Scan(TPACCAbstractSyntaxTreeNodeSWITCHStatement(Node).DefaultCaseLabel);
     LastFunctionInitializedVariables:=copy(FunctionInitializedVariables);
     LastFunctionUninitializedAccessVariables:=copy(FunctionUninitializedAccessVariables);
     try
      Scan(TPACCAbstractSyntaxTreeNodeSWITCHStatement(Node).Body);
     finally
      FunctionInitializedVariables:=copy(LastFunctionInitializedVariables);
      FunctionUninitializedAccessVariables:=copy(LastFunctionUninitializedAccessVariables);
     end;
    end;
    astnkIF:begin
     Scan(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Condition);
     LastFunctionInitializedVariables:=copy(FunctionInitializedVariables);
     LastFunctionUninitializedAccessVariables:=copy(FunctionUninitializedAccessVariables);
     try
      Scan(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Then_);
      OtherFunctionInitializedVariables:=copy(FunctionInitializedVariables);
      OtherFunctionUninitializedAccessVariables:=copy(FunctionUninitializedAccessVariables);
     finally
      FunctionInitializedVariables:=copy(LastFunctionInitializedVariables);
      FunctionUninitializedAccessVariables:=copy(LastFunctionUninitializedAccessVariables);
     end;
     LastFunctionInitializedVariables:=copy(FunctionInitializedVariables);
     LastFunctionUninitializedAccessVariables:=copy(FunctionUninitializedAccessVariables);
     try
      Scan(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Else_);
      for Index:=0 to length(FunctionInitializedVariables)-1 do begin
       if FunctionInitializedVariables[Index] and OtherFunctionInitializedVariables[Index] then begin
        LastFunctionInitializedVariables[Index]:=true;
       end;
      end;
     finally
      FunctionInitializedVariables:=copy(LastFunctionInitializedVariables);
      FunctionUninitializedAccessVariables:=copy(LastFunctionUninitializedAccessVariables);
     end;
    end;
    astnkTERNARY:begin
     Scan(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Condition);
     LastFunctionInitializedVariables:=copy(FunctionInitializedVariables);
     LastFunctionUninitializedAccessVariables:=copy(FunctionUninitializedAccessVariables);
     try
      Scan(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Then_);
      OtherFunctionInitializedVariables:=FunctionInitializedVariables;
      OtherFunctionUninitializedAccessVariables:=FunctionUninitializedAccessVariables;
     finally
      FunctionInitializedVariables:=copy(LastFunctionInitializedVariables);
      FunctionUninitializedAccessVariables:=copy(LastFunctionUninitializedAccessVariables);
     end;
     LastFunctionInitializedVariables:=copy(FunctionInitializedVariables);
     LastFunctionUninitializedAccessVariables:=copy(FunctionUninitializedAccessVariables);
     try
      Scan(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Else_);
      for Index:=0 to length(FunctionInitializedVariables)-1 do begin
       if FunctionInitializedVariables[Index] and OtherFunctionInitializedVariables[Index] then begin
        LastFunctionInitializedVariables[Index]:=true;
       end;
      end;
     finally
      FunctionInitializedVariables:=copy(LastFunctionInitializedVariables);
      FunctionUninitializedAccessVariables:=copy(LastFunctionUninitializedAccessVariables);
     end;
    end;
    astnkRETURN:begin
     Scan(TPACCAbstractSyntaxTreeNodeReturnStatement(Node).ReturnValue);
    end;
    astnkSTATEMENTS:begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeStatements(Node).Children.Count-1 do begin
      SubNode:=TPACCAbstractSyntaxTreeNodeStatements(Node).Children[Index];
      if assigned(SubNode) then begin
       Scan(SubNode);
      end;
     end;
    end;
    astnkSTRUCT_REF:begin
    end;
    astnkGOTO:begin
     Scan(TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).Label_);
    end;
    astnkCOMPUTED_GOTO:begin
     Scan(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand);
    end;
    astnkLABEL:begin
    end;
    astnkHIDDEN_LABEL:begin
    end;
    astnkBREAK:begin
     Scan(TPACCAbstractSyntaxTreeNodeBREAKOrContinueStatement(Node).Label_);
    end;
    astnkCONTINUE:begin
     Scan(TPACCAbstractSyntaxTreeNodeBREAKOrContinueStatement(Node).Label_);
    end;
    astnkOP_COMMA:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_ARROW:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_ASSIGN:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
     if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
        (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkLVAR) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index>=0) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index<length(FunctionInitializedVariables)) then begin
      FunctionInitializedVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index]:=true;
     end;
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
    end;
    astnkOP_SIZEOF:begin
    end;
    astnkOP_CAST:begin
     Scan(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand);
    end;
    astnkOP_NOT:begin
     Scan(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand);
    end;
    astnkOP_NEG:begin
     Scan(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand);
    end;
    astnkOP_PRE_INC:begin
     Scan(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand);
    end;
    astnkOP_PRE_DEC:begin
     Scan(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand);
    end;
    astnkOP_POST_INC:begin
     Scan(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand);
    end;
    astnkOP_POST_DEC:begin
     Scan(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand);
    end;
    astnkOP_LABEL_ADDR:begin
     Scan(TPACCAbstractSyntaxTreeNodeGOTOStatementOrLabelAddress(Node).Label_);
    end;
    astnkOP_ADD:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_SUB:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_MUL:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_DIV:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_MOD:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_AND:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_OR:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_XOR:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_SHL:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_SHR:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_SAR:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_LOG_AND:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_LOG_OR:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_LOG_NOT:begin
     Scan(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand);
    end;
    astnkOP_EQ:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_NE:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_GT:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_LT:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_GE:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_LE:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
    end;
    astnkOP_A_ADD:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
     if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
        (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkLVAR) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index>=0) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index<length(FunctionInitializedVariables)) then begin
      FunctionInitializedVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index]:=true;
     end;
    end;
    astnkOP_A_SUB:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
     if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
        (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkLVAR) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index>=0) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index<length(FunctionInitializedVariables)) then begin
      FunctionInitializedVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index]:=true;
     end;
    end;
    astnkOP_A_MUL:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
     if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
        (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkLVAR) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index>=0) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index<length(FunctionInitializedVariables)) then begin
      FunctionInitializedVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index]:=true;
     end;
    end;
    astnkOP_A_DIV:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
     if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
        (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkLVAR) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index>=0) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index<length(FunctionInitializedVariables)) then begin
      FunctionInitializedVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index]:=true;
     end;
    end;
    astnkOP_A_MOD:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
     if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
        (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkLVAR) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index>=0) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index<length(FunctionInitializedVariables)) then begin
      FunctionInitializedVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index]:=true;
     end;
    end;
    astnkOP_A_AND:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
     if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
        (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkLVAR) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index>=0) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index<length(FunctionInitializedVariables)) then begin
      FunctionInitializedVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index]:=true;
     end;
    end;
    astnkOP_A_OR:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
     if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
        (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkLVAR) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index>=0) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index<length(FunctionInitializedVariables)) then begin
      FunctionInitializedVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index]:=true;
     end;
    end;
    astnkOP_A_XOR:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
     if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
        (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkLVAR) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index>=0) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index<length(FunctionInitializedVariables)) then begin
      FunctionInitializedVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index]:=true;
     end;
    end;
    astnkOP_A_SHR:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
     if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
        (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkLVAR) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index>=0) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index<length(FunctionInitializedVariables)) then begin
      FunctionInitializedVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index]:=true;
     end;
    end;
    astnkOP_A_SHL:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
     if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
        (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkLVAR) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index>=0) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index<length(FunctionInitializedVariables)) then begin
      FunctionInitializedVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index]:=true;
     end;
    end;
    astnkOP_A_SAL:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
     if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
        (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkLVAR) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index>=0) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index<length(FunctionInitializedVariables)) then begin
      FunctionInitializedVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index]:=true;
     end;
    end;
    astnkOP_A_SAR:begin
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left);
     Scan(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right);
     if assigned(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left) and
        (TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left.Kind=astnkLVAR) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index>=0) and
        (TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index<length(FunctionInitializedVariables)) then begin
      FunctionInitializedVariables[TPACCAbstractSyntaxTreeNodeLocalGlobalVariable(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left).Index]:=true;
     end;
    end;
   end;
  end;
 end;
var Root:TPACCAbstractSyntaxTreeNodeTranslationUnit;
begin
 Root:=TPACCParser(TPACCInstance(Instance).Parser).Root;
 if assigned(Root) then begin
  FunctionUsedVariablesHashMap:=nil;
  FunctionUsedVariablesList:=nil;
  try
   Scan(Root);
  finally
   FunctionUsedVariablesHashMap.Free;
   FunctionUsedVariablesList.Free;
  end;
 end;
end;

end.
