unit PACCControlFlowGraph;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCPointerHashMap,PACCAbstractSyntaxTree;

type TPACCControlFlowGraphNodeList=class;

     TPACCControlFlowGraphNode=class
      private
       fInstance:TObject;
       fAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNode;
       fPredecessors:TPACCControlFlowGraphNodeList;
       fSuccessors:TPACCControlFlowGraphNodeList;
      public
       constructor Create(const AInstance:TObject;const AAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNode); reintroduce;
       destructor Destroy; override;
      published
       property AbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNode read fAbstractSyntaxTreeNode write fAbstractSyntaxTreeNode;
       property Predecessors:TPACCControlFlowGraphNodeList read fPredecessors;
       property Successors:TPACCControlFlowGraphNodeList read fSuccessors;
     end;

     TPACCControlFlowGraphNodeList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCControlFlowGraphNode;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCControlFlowGraphNode);
      public
       constructor Create;
       destructor Destroy; override;
       function Add(const AItem:TPACCControlFlowGraphNode):TPACCInt32; reintroduce;
       function FindAbstractSyntaxTreeNode(const ANode:TPACCAbstractSyntaxTreeNode):TPACCInt32;
       property Items[const AIndex:TPACCInt]:TPACCControlFlowGraphNode read GetItem write SetItem; default;
     end;

procedure GenerateControlFlowGraphs(const AInstance:TObject;const ARootAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNode);

implementation

uses PACCInstance;

constructor TPACCControlFlowGraphNode.Create(const AInstance:TObject;const AAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNode);
begin
 inherited Create;
 fInstance:=AInstance;
 TPACCInstance(fInstance).AllocatedObjects.Add(self);
 fAbstractSyntaxTreeNode:=AAbstractSyntaxTreeNode;
 fAbstractSyntaxTreeNode.ControlFlowGraphNode:=self;
 fPredecessors:=TPACCControlFlowGraphNodeList.Create;
 fSuccessors:=TPACCControlFlowGraphNodeList.Create;
end;

destructor TPACCControlFlowGraphNode.Destroy;
begin
 TPACCInstance(fInstance).AllocatedObjects.Remove(self);
 fPredecessors.Free;
 fSuccessors.Free;
 inherited Destroy;
end;

constructor TPACCControlFlowGraphNodeList.Create;
begin
 inherited Create;
end;

destructor TPACCControlFlowGraphNodeList.Destroy;
begin
 inherited Destroy;
end;

function TPACCControlFlowGraphNodeList.GetItem(const AIndex:TPACCInt):TPACCControlFlowGraphNode;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCControlFlowGraphNodeList.SetItem(const AIndex:TPACCInt;const AItem:TPACCControlFlowGraphNode);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

function TPACCControlFlowGraphNodeList.Add(const AItem:TPACCControlFlowGraphNode):TPACCInt32;
var Index:TPACCInt32;
begin
 for Index:=0 to Count-1 do begin
  if Items[Index].AbstractSyntaxTreeNode=AItem.AbstractSyntaxTreeNode then begin
   result:=-1;
   exit;
  end;
 end;
 result:=inherited Add(AItem);
end;


function TPACCControlFlowGraphNodeList.FindAbstractSyntaxTreeNode(const ANode:TPACCAbstractSyntaxTreeNode):TPACCInt32;
var Index:TPACCInt32;
begin
 for Index:=0 to Count-1 do begin
  if Items[Index].AbstractSyntaxTreeNode=ANode then begin
   result:=Index;
   exit;
  end;
 end;
 result:=-1;
end;

procedure GenerateControlFlowGraphs(const AInstance:TObject;const ARootAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNode);
var BreakContinueHashMap:TPACCPointerHashMap;
 function CreateNOP(var LastControlFlowGraphNode:TPACCControlFlowGraphNode):TPACCControlFlowGraphNode;
 begin
  result:=TPACCControlFlowGraphNode.Create(AInstance,TPACCAbstractSyntaxTreeNodeNOP.Create(AInstance,astnkNOP,nil,TPACCInstance(AInstance).SourceLocation));
  if assigned(LastControlFlowGraphNode) then begin
   result.Predecessors.Add(LastControlFlowGraphNode);
   LastControlFlowGraphNode.Successors.Add(result);
  end;
  LastControlFlowGraphNode:=result;
 end;
 function CreatePHI(var LastControlFlowGraphNode:TPACCControlFlowGraphNode):TPACCControlFlowGraphNode;
 begin
  result:=TPACCControlFlowGraphNode.Create(AInstance,TPACCAbstractSyntaxTreeNodePHI.Create(AInstance,astnkPHI,nil,TPACCInstance(AInstance).SourceLocation));
  if assigned(LastControlFlowGraphNode) then begin
   result.Predecessors.Add(LastControlFlowGraphNode);
   LastControlFlowGraphNode.Successors.Add(result);
  end;
  LastControlFlowGraphNode:=result;
 end;
 function ProcessStatement(const Node:TPACCAbstractSyntaxTreeNode;var LastControlFlowGraphNode:TPACCControlFlowGraphNode):TPACCControlFlowGraphNode;
 var ControlFlowGraphNode,a0,a1,b0,b1,c0,c1,d0,d1,e0,e1,f0,f1,g0,g1:TPACCControlFlowGraphNode;
     AbstractSyntaxTreeNodePhi:TPACCAbstractSyntaxTreeNodePhi;
     Index:TPACCInt32;
 begin
  result:=nil;
  if assigned(Node) then begin
   case Node.Kind of

    astnkSTATEMENTS:begin
     for Index:=0 to TPACCAbstractSyntaxTreeNodeStatements(Node).Children.Count-1 do begin
      ControlFlowGraphNode:=ProcessStatement(TPACCAbstractSyntaxTreeNodeStatements(Node).Children[Index],LastControlFlowGraphNode);
      if assigned(ControlFlowGraphNode) and not assigned(result) then begin
       result:=ControlFlowGraphNode;
      end;
     end;
    end;

    astnkIF,astnkTERNARY:begin

     result:=CreateNOP(LastControlFlowGraphNode);

     ProcessStatement(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Condition,LastControlFlowGraphNode);

     ControlFlowGraphNode:=TPACCControlFlowGraphNode.Create(AInstance,Node);
     if assigned(LastControlFlowGraphNode) then begin
      ControlFlowGraphNode.Predecessors.Add(LastControlFlowGraphNode);
      LastControlFlowGraphNode.Successors.Add(ControlFlowGraphNode);
     end;
     LastControlFlowGraphNode:=ControlFlowGraphNode;

     a0:=LastControlFlowGraphNode;
     b0:=LastControlFlowGraphNode;

     ProcessStatement(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Then_,a0);
     ProcessStatement(TPACCAbstractSyntaxTreeNodeIFStatementOrTernaryOperator(Node).Else_,b0);

     AbstractSyntaxTreeNodePhi:=TPACCAbstractSyntaxTreeNodePHI.Create(AInstance,astnkPHI,nil,Node.SourceLocation);

     if (a0<>LastControlFlowGraphNode) or
        (b0<>LastControlFlowGraphNode) then begin
      ControlFlowGraphNode:=TPACCControlFlowGraphNode.Create(AInstance,Node);
      if assigned(a0) then begin
       ControlFlowGraphNode.Predecessors.Add(a0);
       a0.Successors.Add(ControlFlowGraphNode);
      end;
      if assigned(b0) then begin
       ControlFlowGraphNode.Predecessors.Add(b0);
       b0.Successors.Add(ControlFlowGraphNode);
      end;
      LastControlFlowGraphNode:=ControlFlowGraphNode;
     end;

    end;

    astnkFOR:begin

     a1:=LastControlFlowGraphNode;
     a0:=ProcessStatement(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Initialization_,a1);
     if not assigned(a0) then begin
      a1:=LastControlFlowGraphNode;
      a0:=CreateNOP(a1);
     end;
     result:=a0;
     LastControlFlowGraphNode:=a0;

     b1:=LastControlFlowGraphNode;
     b0:=ProcessStatement(TPACCAbstractSyntaxTreeNodeFORStatement(Node).ContinueLabel,b1);
     if assigned(b0) then begin
      BreakContinueHashMap[TPACCAbstractSyntaxTreeNodeFORStatement(Node).ContinueLabel]:=b0;
     end else begin
      b1:=LastControlFlowGraphNode;
      b0:=CreateNOP(LastControlFlowGraphNode);
     end;
     LastControlFlowGraphNode:=b1;

     f1:=nil;
     f0:=ProcessStatement(TPACCAbstractSyntaxTreeNodeFORStatement(Node).BreakLabel,f1);
     if assigned(f0) then begin
      BreakContinueHashMap[TPACCAbstractSyntaxTreeNodeFORStatement(Node).BreakLabel]:=f0;
     end else begin
      f1:=nil;
      f0:=CreateNOP(f1);
     end;

     c1:=LastControlFlowGraphNode;
     c0:=ProcessStatement(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Condition,c1);
     if not assigned(c0) then begin
      c1:=LastControlFlowGraphNode;
      c0:=CreateNOP(c1);
     end;
     LastControlFlowGraphNode:=c1;

     d1:=LastControlFlowGraphNode;
     d0:=ProcessStatement(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Body,d1);
     if not assigned(d0) then begin
      d1:=LastControlFlowGraphNode;
      d0:=CreateNOP(d1);
     end;
     LastControlFlowGraphNode:=d1;

     e1:=LastControlFlowGraphNode;
     e0:=ProcessStatement(TPACCAbstractSyntaxTreeNodeFORStatement(Node).Step,e1);
     if not assigned(e0) then begin
      e1:=LastControlFlowGraphNode;
      e0:=CreateNOP(e1);
     end;

     f0.Predecessors.Add(e1);
     e1.Successors.Add(f0);
     LastControlFlowGraphNode:=f1;

     f0.Predecessors.Add(c1);
     c1.Successors.Add(f0);

     b0.Predecessors.Add(e1);
     e1.Successors.Add(b0);

    end;
    astnkWHILE:begin

     a1:=LastControlFlowGraphNode;
     a0:=ProcessStatement(TPACCAbstractSyntaxTreeNodeWHILEOrDoStatement(Node).ContinueLabel,a1);
     if assigned(a0) then begin
      BreakContinueHashMap[TPACCAbstractSyntaxTreeNodeWHILEOrDoStatement(Node).ContinueLabel]:=a0;
     end else begin
      a1:=LastControlFlowGraphNode;
      a0:=CreateNOP(LastControlFlowGraphNode);
     end;
     LastControlFlowGraphNode:=a1;
     result:=a1;

     b1:=LastControlFlowGraphNode;
     b0:=ProcessStatement(TPACCAbstractSyntaxTreeNodeWHILEOrDoStatement(Node).Condition,b1);
     if not assigned(b0) then begin
      b1:=LastControlFlowGraphNode;
      b0:=CreateNOP(b1);
     end;
     LastControlFlowGraphNode:=b0;

     d1:=nil;
     d0:=ProcessStatement(TPACCAbstractSyntaxTreeNodeWHILEOrDoStatement(Node).BreakLabel,d1);
     if assigned(d0) then begin
      BreakContinueHashMap[TPACCAbstractSyntaxTreeNodeWHILEOrDoStatement(Node).BreakLabel]:=d0;
     end else begin
      d1:=nil;
      d0:=CreateNOP(d1);
     end;

     c1:=LastControlFlowGraphNode;
     c0:=ProcessStatement(TPACCAbstractSyntaxTreeNodeWHILEOrDoStatement(Node).Body,c1);
     if not assigned(c0) then begin
      c1:=LastControlFlowGraphNode;
      c0:=CreateNOP(c1);
     end;
     LastControlFlowGraphNode:=c1;

     d0.Predecessors.Add(c1);
     c1.Successors.Add(d0);
     LastControlFlowGraphNode:=d1;

     d0.Predecessors.Add(b1);
     b1.Successors.Add(d0);

     a0.Predecessors.Add(c1);
     c1.Successors.Add(a0);

    end;

    astnkDO:begin

     a1:=LastControlFlowGraphNode;
     a0:=ProcessStatement(TPACCAbstractSyntaxTreeNodeWHILEOrDoStatement(Node).ContinueLabel,a1);
     if assigned(a0) then begin
      BreakContinueHashMap[TPACCAbstractSyntaxTreeNodeWHILEOrDoStatement(Node).ContinueLabel]:=a0;
     end else begin
      a1:=LastControlFlowGraphNode;
      a0:=CreateNOP(LastControlFlowGraphNode);
     end;
     LastControlFlowGraphNode:=a1;
     result:=a1;

     d1:=nil;
     d0:=ProcessStatement(TPACCAbstractSyntaxTreeNodeWHILEOrDoStatement(Node).BreakLabel,d1);
     if assigned(d0) then begin
      BreakContinueHashMap[TPACCAbstractSyntaxTreeNodeWHILEOrDoStatement(Node).BreakLabel]:=d0;
     end else begin
      d1:=nil;
      d0:=CreateNOP(d1);
     end;

     b1:=LastControlFlowGraphNode;
     b0:=ProcessStatement(TPACCAbstractSyntaxTreeNodeWHILEOrDoStatement(Node).Body,b1);
     if not assigned(b0) then begin
      b1:=LastControlFlowGraphNode;
      b0:=CreateNOP(b1);
     end;
     LastControlFlowGraphNode:=b1;

     c1:=LastControlFlowGraphNode;
     c0:=ProcessStatement(TPACCAbstractSyntaxTreeNodeWHILEOrDoStatement(Node).Condition,c1);
     if not assigned(b0) then begin
      c1:=LastControlFlowGraphNode;
      c0:=CreateNOP(c1);
     end;
     LastControlFlowGraphNode:=c0;

     d0.Predecessors.Add(c1);
     c1.Successors.Add(d0);
     LastControlFlowGraphNode:=d1;

     a0.Predecessors.Add(c1);
     c1.Successors.Add(a0);

    end;

    astnkRETURN:begin
     ControlFlowGraphNode:=TPACCControlFlowGraphNode.Create(AInstance,Node);
     if assigned(LastControlFlowGraphNode) then begin
      ControlFlowGraphNode.Predecessors.Add(LastControlFlowGraphNode);
      LastControlFlowGraphNode.Successors.Add(ControlFlowGraphNode);
     end;
     LastControlFlowGraphNode:=ControlFlowGraphNode;
     result:=ControlFlowGraphNode;
     ProcessStatement(TPACCAbstractSyntaxTreeNodeRETURNStatement(Node).ReturnValue,LastControlFlowGraphNode);
     LastControlFlowGraphNode:=nil;
    end;

    astnkBREAK,astnkCONTINUE:begin
     ControlFlowGraphNode:=TPACCControlFlowGraphNode.Create(AInstance,Node);
     if assigned(LastControlFlowGraphNode) then begin
      ControlFlowGraphNode.Predecessors.Add(LastControlFlowGraphNode);
      LastControlFlowGraphNode.Successors.Add(ControlFlowGraphNode);
     end;
     LastControlFlowGraphNode:=nil;
     result:=ControlFlowGraphNode;
     a0:=BreakContinueHashMap[TPACCAbstractSyntaxTreeNodeBREAKOrCONTINUEStatement(Node).Label_];
     if assigned(a0) then begin
      a0.Predecessors.Add(result);
      result.Successors.Add(a0);
     end;
    end;

    else begin

     ControlFlowGraphNode:=TPACCControlFlowGraphNode.Create(AInstance,Node);
     if assigned(LastControlFlowGraphNode) then begin
      ControlFlowGraphNode.Predecessors.Add(LastControlFlowGraphNode);
      LastControlFlowGraphNode.Successors.Add(ControlFlowGraphNode);
     end;
     LastControlFlowGraphNode:=ControlFlowGraphNode;
     result:=ControlFlowGraphNode;

     if Node is TPACCAbstractSyntaxTreeNodeUnaryOperator then begin
      ProcessStatement(TPACCAbstractSyntaxTreeNodeUnaryOperator(Node).Operand,LastControlFlowGraphNode);
     end else if Node is TPACCAbstractSyntaxTreeNodeBinaryOperator then begin
      ProcessStatement(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Left,LastControlFlowGraphNode);
      ProcessStatement(TPACCAbstractSyntaxTreeNodeBinaryOperator(Node).Right,LastControlFlowGraphNode);
     end;

    end;
   end;
  end;
 end;
 procedure ProcessFunctionDeclaration(const FunctionDeclarationAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration);
 var Index:TPACCInt32;
     Node:TPACCAbstractSyntaxTreeNode;
     LastControlFlowGraphNode:TPACCControlFlowGraphNode;
 begin
  if assigned(FunctionDeclarationAbstractSyntaxTreeNode) and
     (TPACCAbstractSyntaxTreeNode(FunctionDeclarationAbstractSyntaxTreeNode).Kind=astnkFUNC) and
     (FunctionDeclarationAbstractSyntaxTreeNode is TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration) then begin
   if assigned(FunctionDeclarationAbstractSyntaxTreeNode.Body) and
      (FunctionDeclarationAbstractSyntaxTreeNode.Body.Kind=astnkSTATEMENTS) and
      (FunctionDeclarationAbstractSyntaxTreeNode.Body is TPACCAbstractSyntaxTreeNodeStatements) then begin
    LastControlFlowGraphNode:=nil;
    BreakContinueHashMap:=TPACCPointerHashMap.Create;
    try
     ProcessStatement(TPACCAbstractSyntaxTreeNodeStatements(FunctionDeclarationAbstractSyntaxTreeNode.Body),LastControlFlowGraphNode);
    finally
     BreakContinueHashMap.Free;
    end;
   end;
  end else begin
   TPACCInstance(AInstance).AddError('Internal error 2017-01-18-10-23-0000',nil,true);
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
   if assigned(Node) and (Node.Kind=astnkFUNC) then begin
    ProcessFunctionDeclaration(TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(Node));
   end;
  end;
 end else begin
  TPACCInstance(AInstance).AddError('Internal error 2017-01-19-05-48-0000',nil,true);
 end;
end;

end.
