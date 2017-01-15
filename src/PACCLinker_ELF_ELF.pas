unit PACCLinker_ELF_ELF;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCRawByteStringHashMap,PACCPointerHashMap,PACCLinker;

type TPACCLinker_ELF_ELF=class;

     TPACCLinker_ELF_ELF=class(TPACCLinker)
      private
      public

       constructor Create(const AInstance:TObject); override;
       destructor Destroy; override;

       procedure AddImport(const ASymbolName,AImportLibraryName,AImportName:TPUCUUTF8String); override;

       procedure AddExport(const ASymbolName,AExportName:TPUCUUTF8String); override;

       procedure AddObject(const AObjectStream:TStream;const AObjectFileName:TPUCUUTF8String=''); override;

       procedure AddResources(const AResourcesStream:TStream;const AResourcesFileName:TPUCUUTF8String=''); override;

       procedure Link(const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String=''); override;

      published

     end;

implementation

uses PACCInstance;

constructor TPACCLinker_ELF_ELF.Create(const AInstance:TObject);
begin
 inherited Create(AInstance);

end;

destructor TPACCLinker_ELF_ELF.Destroy;
begin


 inherited Destroy;
end;

procedure TPACCLinker_ELF_ELF.AddImport(const ASymbolName,AImportLibraryName,AImportName:TPUCUUTF8String);
begin
end;

procedure TPACCLinker_ELF_ELF.AddExport(const ASymbolName,AExportName:TPUCUUTF8String);
begin
end;

procedure TPACCLinker_ELF_ELF.AddObject(const AObjectStream:TStream;const AObjectFileName:TPUCUUTF8String='');
begin
end;

procedure TPACCLinker_ELF_ELF.AddResources(const AResourcesStream:TStream;const AResourcesFileName:TPUCUUTF8String=''); 
begin
end;

procedure TPACCLinker_ELF_ELF.Link(const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String='');
begin
end;

end.
