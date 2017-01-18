unit PACCControlFlowGraph;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCPointerHashMap;

procedure GenerateControlFlowGraphForFunctionDeclarationAbstractTreeNode(const AInstance,AFunctionDeclarationAbstractTreeNode:TObject);

implementation

uses PACCInstance,PACCAbstractSyntaxTree;

procedure GenerateControlFlowGraphForFunctionDeclarationAbstractTreeNode(const AInstance,AFunctionDeclarationAbstractTreeNode:TObject);
var FunctionDeclarationAbstractTreeNode:TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration;
begin
 if assigned(AFunctionDeclarationAbstractTreeNode) and
    (AFunctionDeclarationAbstractTreeNode is TPACCAbstractSyntaxTreeNode) and
    (TPACCAbstractSyntaxTreeNode(AFunctionDeclarationAbstractTreeNode).Kind=astnkFUNC) and
    (AFunctionDeclarationAbstractTreeNode is TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration) then begin
  FunctionDeclarationAbstractTreeNode:=TPACCAbstractSyntaxTreeNodeFunctionCallOrFunctionDeclaration(AFunctionDeclarationAbstractTreeNode);
 end else begin
  TPACCInstance(AInstance).AddError('Internal error 2017-01-18-10-23-0000',nil,true);
 end;
end;

end.
