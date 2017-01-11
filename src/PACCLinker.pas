unit PACCLinker;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCRawByteStringHashMap,PACCPointerHashMap;

type TPACCLinker=class
      private
      public

       fInstance:TObject;

       constructor Create(const AInstance:TObject); reintroduce;
       destructor Destroy; override;

       procedure AddObject(const AObjectStream:TStream;const AObjectFileName:TPUCUUTF8String='');

       procedure Link(const AOutputStream:TStream);

     end;

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

procedure TPACCLinker.AddObject(const AObjectStream:TStream;const AObjectFileName:TPUCUUTF8String='');
begin
end;

procedure TPACCLinker.Link(const AOutputStream:TStream);
begin
end;

end.
