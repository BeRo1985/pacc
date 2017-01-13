unit PACCLinker;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCRawByteStringHashMap,PACCPointerHashMap;

type TPACCLinker=class;

     TPACCLinker=class
      private

       fInstance:TObject;

      public

       constructor Create(const AInstance:TObject); reintroduce; virtual;
       destructor Destroy; override;

       procedure AddImport(const ASymbolName,AImportLibraryName,AImportName:TPUCUUTF8String); virtual;

       procedure AddExport(const ASymbolName,AExportName:TPUCUUTF8String); virtual;

       procedure AddImports(const AInput:TPACCRawByteString;const AFileName:TPUCUUTF8String=''); virtual;

       procedure AddExports(const AInput:TPACCRawByteString;const AFileName:TPUCUUTF8String=''); virtual;

       procedure AddObject(const AObjectStream:TStream;const AObjectFileName:TPUCUUTF8String=''); virtual;

       procedure Link(const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String=''); virtual;

      published

       property Instance:TObject read fInstance;

     end;

     TPACCLinkerClass=class of TPACCLinker;

implementation

uses PACCInstance;

constructor TPACCLinker.Create(const AInstance:TObject);
begin
 inherited Create;

 fInstance:=AInstance;

end;

destructor TPACCLinker.Destroy;
begin
 inherited Destroy;
end;

procedure TPACCLinker.AddImport(const ASymbolName,AImportLibraryName,AImportName:TPUCUUTF8String);
begin
end;

procedure TPACCLinker.AddExport(const ASymbolName,AExportName:TPUCUUTF8String);
begin
end;

procedure TPACCLinker.AddImports(const AInput:TPACCRawByteString;const AFileName:TPUCUUTF8String='');
const WhiteSpaceChars=[#0..#32];
      IdentChars=['a'..'z','A'..'Z','0'..'9','_','@','$','.',#$80..#$ff{UTF-8}];
var Index,Len,StartIndex:TPACCInt32;
    LibraryName,SymbolName,ImportName:TPACCRawByteString;
begin
 Len:=length(AInput);

 Index:=1;
 while Index<=Len do begin

  while (Index<=Len) and (AInput[Index] in WhiteSpaceChars) do begin
   inc(Index);
  end;

  StartIndex:=Index;
  while (Index<=Len) and (AInput[Index] in IdentChars) do begin
   inc(Index);
  end;
  if StartIndex<Index then begin

   LibraryName:=copy(AInput,StartIndex,Index-StartIndex);

   while (Index<=Len) and (AInput[Index] in WhiteSpaceChars) do begin
    inc(Index);
   end;

   if (Index<=Len) and (AInput[Index]='(') then begin
    inc(Index);

    while not ((Index>Len) or (AInput[Index]=')')) do begin

     while (Index<=Len) and (AInput[Index] in WhiteSpaceChars) do begin
      inc(Index);
     end;

     StartIndex:=Index;
     while (Index<=Len) and (AInput[Index] in IdentChars) do begin
      inc(Index);
     end;
     if StartIndex<Index then begin

      SymbolName:=copy(AInput,StartIndex,Index-StartIndex);

      ImportName:=SymbolName;

      while (Index<=Len) and (AInput[Index] in WhiteSpaceChars) do begin
       inc(Index);
      end;

      if (Index<=Len) and (AInput[Index]='=') then begin
       inc(Index);

       while (Index<=Len) and (AInput[Index] in WhiteSpaceChars) do begin
        inc(Index);
       end;

       StartIndex:=Index;
       while (Index<=Len) and (AInput[Index] in IdentChars) do begin
        inc(Index);
       end;
       if StartIndex<Index then begin
        ImportName:=copy(AInput,StartIndex,Index-StartIndex);
        while (Index<=Len) and (AInput[Index] in WhiteSpaceChars) do begin
         inc(Index);
        end;
       end else begin
        TPACCInstance(Instance).AddError('Import file syntax error',nil,true);
       end;

       AddImport(SymbolName,LibraryName,ImportName);

      end;

     end else begin
      TPACCInstance(Instance).AddError('Import file syntax error',nil,true);
     end;

    end;

    if (Index<=Len) and (AInput[Index]=')') then begin
     inc(Index);
    end else begin
     TPACCInstance(Instance).AddError('Import file syntax error',nil,true);
    end;

   end else begin
    TPACCInstance(Instance).AddError('Import file syntax error',nil,true);
   end;

  end else begin
   TPACCInstance(Instance).AddError('Import file syntax error',nil,true);
  end;

  while (Index<=Len) and (AInput[Index] in WhiteSpaceChars) do begin
   inc(Index);
  end;

 end;

end;

procedure TPACCLinker.AddExports(const AInput:TPACCRawByteString;const AFileName:TPUCUUTF8String='');
begin
end;

procedure TPACCLinker.AddObject(const AObjectStream:TStream;const AObjectFileName:TPUCUUTF8String='');
begin
end;

procedure TPACCLinker.Link(const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String='');
begin
end;


end.
