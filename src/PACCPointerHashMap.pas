unit PACCPointerHashMap;
{$i PACC.inc}

interface

uses PACCTypes;

type TPACCPointerHashMapData=pointer;

     PPACCPointerHashMapEntity=^TPACCPointerHashMapEntity;
     TPACCPointerHashMapEntity=record
      Key:pointer;
      Value:TPACCPointerHashMapData;
     end;

     TPACCPointerHashMapEntities=array of TPACCPointerHashMapEntity;

     TPACCPointerHashMapEntityIndices=array of longint;

     TPACCPointerHashMap=class
      private
       function FindCell(const Key:pointer):TPACCUInt32;
       procedure Resize;
      protected
       function GetValue(const Key:pointer):TPACCPointerHashMapData;
       procedure SetValue(const Key:pointer;const Value:TPACCPointerHashMapData);
      public
       Parent:TPACCPointerHashMap;
       RealSize:longint;
       LogSize:longint;
       Size:longint;
       Entities:TPACCPointerHashMapEntities;
       EntityToCellIndex:TPACCPointerHashMapEntityIndices;
       CellToEntityIndex:TPACCPointerHashMapEntityIndices;
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function Add(const Key:pointer;const Value:TPACCPointerHashMapData):PPACCPointerHashMapEntity;
       function Get(const Key:pointer;const CreateIfNotExist:boolean=false):PPACCPointerHashMapEntity;
       function Delete(const Key:pointer;const DoParent:boolean=false):boolean;
       property Values[const Key:pointer]:TPACCPointerHashMapData read GetValue write SetValue; default;
     end;

implementation

const CELL_EMPTY=-1;
      CELL_DELETED=-2;

      ENT_EMPTY=-1;
      ENT_DELETED=-2;

function HashPointer(const p:pointer):TPACCUInt32;
begin
 result:=(TPACCPtrUInt(p)*$5052acdb0){$ifdef cpu64} xor ((TPACCPtrUInt(p) shr 32)*$57559429){$endif};
 if result=0 then begin
  result:=$ffffffff;
 end;
end;

constructor TPACCPointerHashMap.Create;
begin
 inherited Create;
 Parent:=nil;
 RealSize:=0;
 LogSize:=0;
 Size:=0;
 Entities:=nil;
 EntityToCellIndex:=nil;
 CellToEntityIndex:=nil;
 Resize;
end;

destructor TPACCPointerHashMap.Destroy;
var Counter:longint;
begin
 Clear;
 for Counter:=0 to length(Entities)-1 do begin
  Entities[Counter].Key:=nil;
 end;
 SetLength(Entities,0);
 SetLength(EntityToCellIndex,0);
 SetLength(CellToEntityIndex,0);
 inherited Destroy;
end;

procedure TPACCPointerHashMap.Clear;
var Counter:longint;
begin
 for Counter:=0 to length(Entities)-1 do begin
  Entities[Counter].Key:=nil;
 end;
 RealSize:=0;
 LogSize:=0;
 Size:=0;
 SetLength(Entities,0);
 SetLength(EntityToCellIndex,0);
 SetLength(CellToEntityIndex,0);
 Resize;
end;

function TPACCPointerHashMap.FindCell(const Key:pointer):TPACCUInt32;
var HashCode,Mask,Step:TPACCUInt32;
    Entity:longint;
begin
 HashCode:=HashPointer(Key);
 Mask:=(2 shl LogSize)-1;
 Step:=((HashCode shl 1)+1) and Mask;
 if LogSize<>0 then begin
  result:=HashCode shr (32-LogSize);
 end else begin
  result:=0;
 end;
 repeat
  Entity:=CellToEntityIndex[result];
  if (Entity=ENT_EMPTY) or ((Entity<>ENT_DELETED) and (Entities[Entity].Key=Key)) then begin
   exit;
  end;
  result:=(result+Step) and Mask;
 until false;
end;

procedure TPACCPointerHashMap.Resize;
var NewLogSize,NewSize,Cell,Entity,Counter:longint;
    OldEntities:TPACCPointerHashMapEntities;
    OldCellToEntityIndex:TPACCPointerHashMapEntityIndices;
    OldEntityToCellIndex:TPACCPointerHashMapEntityIndices;
begin
 NewLogSize:=0;
 NewSize:=RealSize;
 while NewSize<>0 do begin
  NewSize:=NewSize shr 1;
  inc(NewLogSize);
 end;
 if NewLogSize<1 then begin
  NewLogSize:=1;
 end;
 Size:=0;
 RealSize:=0;
 LogSize:=NewLogSize;
 OldEntities:=Entities;
 OldCellToEntityIndex:=CellToEntityIndex;
 OldEntityToCellIndex:=EntityToCellIndex;
 Entities:=nil;
 CellToEntityIndex:=nil;
 EntityToCellIndex:=nil;
 SetLength(Entities,2 shl LogSize);
 SetLength(CellToEntityIndex,2 shl LogSize);
 SetLength(EntityToCellIndex,2 shl LogSize);
 for Counter:=0 to length(CellToEntityIndex)-1 do begin
  CellToEntityIndex[Counter]:=ENT_EMPTY;
 end;
 for Counter:=0 to length(EntityToCellIndex)-1 do begin
  EntityToCellIndex[Counter]:=CELL_EMPTY;
 end;
 for Counter:=0 to length(OldEntityToCellIndex)-1 do begin
  Cell:=OldEntityToCellIndex[Counter];
  if Cell>=0 then begin
   Entity:=OldCellToEntityIndex[Cell];
   if Entity>=0 then begin
    Add(OldEntities[Counter].Key,OldEntities[Counter].Value);
   end;
  end;
 end;
 for Counter:=0 to length(OldEntities)-1 do begin
  OldEntities[Counter].Key:=nil;
 end;
 SetLength(OldEntities,0);
 SetLength(OldCellToEntityIndex,0);
 SetLength(OldEntityToCellIndex,0);
end;

function TPACCPointerHashMap.Add(const Key:pointer;const Value:TPACCPointerHashMapData):PPACCPointerHashMapEntity;
var Entity:longint;
    Cell:TPACCUInt32;
begin
 result:=nil;
 while RealSize>=(1 shl LogSize) do begin
  Resize;
 end;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=@Entities[Entity];
  result^.Key:=Key;
  result^.Value:=Value;
  exit;
 end;
 Entity:=Size;
 inc(Size);
 if Entity<(2 shl LogSize) then begin
  CellToEntityIndex[Cell]:=Entity;
  EntityToCellIndex[Entity]:=Cell;
  inc(RealSize);
  result:=@Entities[Entity];
  result^.Key:=Key;
  result^.Value:=Value;
 end;
end;

function TPACCPointerHashMap.Get(const Key:pointer;const CreateIfNotExist:boolean=false):PPACCPointerHashMapEntity;
var Entity:longint;
    Cell:TPACCUInt32;
begin
 result:=nil;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=@Entities[Entity];
 end else if CreateIfNotExist then begin
  result:=Add(Key,nil);
 end;
 if assigned(Parent) and not assigned(result) then begin
  result:=Parent.Get(Key,CreateIfNotExist);
 end;
end;

function TPACCPointerHashMap.Delete(const Key:pointer;const DoParent:boolean=false):boolean;
var Entity:longint;
    Cell:TPACCUInt32;
begin
 result:=false;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  Entities[Entity].Key:=nil;
  Entities[Entity].Value:=nil;
  EntityToCellIndex[Entity]:=CELL_DELETED;
  CellToEntityIndex[Cell]:=ENT_DELETED;
  result:=true;
 end else if DoParent and assigned(Parent) then begin
  result:=Parent.Delete(Key);
 end;
end;

function TPACCPointerHashMap.GetValue(const Key:pointer):TPACCPointerHashMapData;
var Entity:longint;
    Cell:TPACCUInt32;
begin
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=Entities[Entity].Value;
 end else if assigned(Parent) then begin
  result:=Parent.GetValue(Key);
 end else begin
  result:=nil;
 end;
end;

procedure TPACCPointerHashMap.SetValue(const Key:pointer;const Value:TPACCPointerHashMapData);
begin
 Add(Key,Value);
end;

end.
