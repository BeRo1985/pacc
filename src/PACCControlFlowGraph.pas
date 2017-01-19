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

procedure GenerateControlFlowGraphForFunctionDeclarationAbstractSyntaxTreeNode(const AInstance,AFunctionDeclarationAbstractSyntaxTreeNode:TObject);

implementation

uses PACCInstance;

constructor TPACCControlFlowGraphNode.Create(const AInstance:TObject;const AAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNode);
begin
 inherited Create;
 fInstance:=AInstance;
 TPACCInstance(fInstance).AllocatedObjects.Add(self);
 fAbstractSyntaxTreeNode:=AAbstractSyntaxTreeNode;
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

procedure GenerateControlFlowGraphForFunctionDeclarationAbstractSyntaxTreeNode(const AInstance,AFunctionDeclarationAbstractSyntaxTreeNode:TObject);
var FunctionDeclarationAbstractSyntaxTreeNode:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration;
begin
 if assigned(AFunctionDeclarationAbstractSyntaxTreeNode) and
    (AFunctionDeclarationAbstractSyntaxTreeNode is TPACCAbstractSyntaxTreeNode) and
    (TPACCAbstractSyntaxTreeNode(AFunctionDeclarationAbstractSyntaxTreeNode).Kind=astnkFUNC) and
    (AFunctionDeclarationAbstractSyntaxTreeNode is TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration) then begin
  FunctionDeclarationAbstractSyntaxTreeNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(AFunctionDeclarationAbstractSyntaxTreeNode);
  if assigned(FunctionDeclarationAbstractSyntaxTreeNode.Body) and
     (FunctionDeclarationAbstractSyntaxTreeNode.Body.Kind=astnkSTATEMENTS) and
     (FunctionDeclarationAbstractSyntaxTreeNode.Body is TPACCAbstractSyntaxTreeNodeStatements) then begin
// Process();
  end;
 end else begin
  TPACCInstance(AInstance).AddError('Internal error 2017-01-18-10-23-0000',nil,true);
 end;
end;

end.
