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

       procedure AddArchive(const AArchiveStream:TStream;const AArchiveFileName:TPUCUUTF8String=''); virtual;

       procedure AddResources(const AResourcesStream:TStream;const AResourcesFileName:TPUCUUTF8String=''); virtual;

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
      IdentChars=['a'..'z','A'..'Z','0'..'9','_','@','?','$','.',#$80..#$ff{UTF-8}];
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

      end;

      AddImport(SymbolName,LibraryName,ImportName);

     end else begin
      TPACCInstance(Instance).AddError('Import file syntax error',nil,true);
     end;

     while (Index<=Len) and (AInput[Index] in WhiteSpaceChars) do begin
      inc(Index);
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
const WhiteSpaceChars=[#0..#32];
      IdentChars=['a'..'z','A'..'Z','0'..'9','_','@','$','.',#$80..#$ff{UTF-8}];
var Index,Len,StartIndex:TPACCInt32;
    SymbolName,ExportName:TPACCRawByteString;
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

   SymbolName:=copy(AInput,StartIndex,Index-StartIndex);

   ExportName:=SymbolName;

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
     ExportName:=copy(AInput,StartIndex,Index-StartIndex);
     while (Index<=Len) and (AInput[Index] in WhiteSpaceChars) do begin
      inc(Index);
     end;
    end else begin
     TPACCInstance(Instance).AddError('Import file syntax error',nil,true);
    end;

   end;

   AddExport(SymbolName,ExportName);

  end else begin
   TPACCInstance(Instance).AddError('Import file syntax error',nil,true);
  end;

 end;
end;

procedure TPACCLinker.AddObject(const AObjectStream:TStream;const AObjectFileName:TPUCUUTF8String='');
begin
end;

procedure TPACCLinker.AddArchive(const AArchiveStream:TStream;const AArchiveFileName:TPUCUUTF8String='');
const IMAGE_FILE_MACHINE_UNKNOWN=0;
type PSignature=^TSignature;
     TSignature=array[0..7] of ansichar;
     PHeader=^THeader;
     THeader=packed record
      Name:array[0..15] of ansichar; // Member name
      Date:array[0..11] of ansichar; // Member date, seconds, decimal ASCII
      UserID:array[0..5] of ansichar; // Member User ID, decimal ASCII
      GroupID:array[0..5] of ansichar; // Member Group ID, decimal ASCII
      FileMode:array[0..7] of ansichar; // Member file mode, octal
      FileSize:array[0..9] of ansichar; // Member file size, decimal ASCII
      HeaderEnd:array[0..1] of TPACCUInt8; // "`\n"
     end;
     PImportHeader=^TImportHeader;
     TImportHeader=packed record
      Sig1:TPACCUInt16;
      Sig2:TPACCUInt16;
      Version:TPACCUInt16;
      Machine:TPACCUInt16;
      TimeDateStamp:TPACCUInt32;
      SizeOfData:TPACCUInt32;
      OrdinalOrHint:TPACCUInt16;
      BitData:TPACCUInt16;
     end;
 function TrimHeaderString(const s:TPACCRawByteString):TPACCRawByteString;
 var i:TPACCInt32;
 begin
  result:=s;
  for i:=length(result) downto 1 do begin
   if not (result[i] in [#0..#32]) then begin
    SetLength(result,i);
    break;
   end;
  end;
 end;
var Stream:TMemoryStream;
    Signature:TSignature;
    Header:THeader;
    ImportHeader:TImportHeader;
    Name:TPACCRawByteString;
    FileSize:TPACCInt64;
begin
 AArchiveStream.Seek(0,soBeginning);
 if AArchiveStream.Size>=SizeOf(TSignature) then begin
  AArchiveStream.ReadBuffer(Signature,SizeOf(TSignature));
  if (Signature[0]='!') and
     (Signature[1]='<') and
     (Signature[2]='a') and
     (Signature[3]='r') and
     (Signature[4]='c') and
     (Signature[5]='h') and
     (Signature[6]='>') and
     (Signature[7]=#10) then begin
   while (AArchiveStream.Position+SizeOf(Header))<AArchiveStream.Size do begin
    AArchiveStream.ReadBuffer(Header,SizeOf(Header));
    if (Header.HeaderEnd[0]=$60) and (Header.HeaderEnd[1]=$0a) then begin
     Name:=TrimHeaderString(Header.Name);
     writeln(Name);
     FileSize:=StrToInt(TrimHeaderString(Header.FileSize));
     if FileSize>0 then begin
      Stream:=TMemoryStream.Create;
      try
       if Stream.CopyFrom(AArchiveStream,FileSize)<>FileSize then begin
        raise EReadError.Create('Stream read error');
       end;
       Stream.Seek(0,soBeginning);
       if (Name='\') or (Name='/') then begin
       end else if (Name='\\') or (Name='//') then begin
       end else begin
        if (Stream.Size>SizeOf(TImportHeader)) and
           (PImportHeader(Stream.Memory)^.Sig1=IMAGE_FILE_MACHINE_UNKNOWN) and
           (PImportHeader(Stream.Memory)^.Sig2=$ffff) then begin
         // TODO: Short import library parsing
        end else begin
         AddObject(Stream,Name);
        end;
       end;
      finally
       Stream.Free;
      end;
     end;
    end else begin
     break;
    end;
   end;
  end;
 end;
end;

procedure TPACCLinker.AddResources(const AResourcesStream:TStream;const AResourcesFileName:TPUCUUTF8String='');
begin
end;

procedure TPACCLinker.Link(const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String='');
begin
end;


end.
