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

procedure TPACCLinker.AddObject(const AObjectStream:TStream;const AObjectFileName:TPUCUUTF8String='');
begin
end;

procedure TPACCLinker.Link(const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String='');
begin
end;


end.
