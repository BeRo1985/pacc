unit PACCInt64HashMap;
{$i PACC.inc}

interface

uses PACCTypes;

type TPACCInt64HashMapData=pointer;

     PPACCInt64HashMapEntity=^TPACCInt64HashMapEntity;
     TPACCInt64HashMapEntity=record
      Key:TPACCInt64;
      Value:TPACCInt64HashMapData;
     end;

     TPACCInt64HashMapEntities=array of TPACCInt64HashMapEntity;

     TPACCInt64HashMapEntityIndices=array of longint;

     TPACCInt64HashMap=class
      private
       function FindCell(const Key:TPACCInt64):TPACCUInt32;
       procedure Resize;
      protected
       function GetValue(const Key:TPACCInt64):TPACCInt64HashMapData;
       procedure SetValue(const Key:TPACCInt64;const Value:TPACCInt64HashMapData);
      public
       Parent:TPACCInt64HashMap;
       RealSize:longint;
       LogSize:longint;
       Size:longint;
       Entities:TPACCInt64HashMapEntities;
       EntityToCellIndex:TPACCInt64HashMapEntityIndices;
       CellToEntityIndex:TPACCInt64HashMapEntityIndices;
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function Add(const Key:TPACCInt64;const Value:TPACCInt64HashMapData):PPACCInt64HashMapEntity;
       function Get(const Key:TPACCInt64;const CreateIfNotExist:boolean=false):PPACCInt64HashMapEntity;
       function Delete(const Key:TPACCInt64;const DoParent:boolean=false):boolean;
       property Values[const Key:TPACCInt64]:TPACCInt64HashMapData read GetValue write SetValue; default;
     end;

implementation

const CELL_EMPTY=-1;
      CELL_DELETED=-2;

      ENT_EMPTY=-1;
      ENT_DELETED=-2;

function HashInt64(const p:TPACCInt64):TPACCUInt32;
begin
 result:=(TPACCUInt32(p)*$5052acdb0) xor (TPACCUInt32(p shr 32)*$57559429);
 if result=0 then begin
  result:=$ffffffff;
 end;
end;

constructor TPACCInt64HashMap.Create;
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

destructor TPACCInt64HashMap.Destroy;
var Counter:longint;
begin
 Clear;
 for Counter:=0 to length(Entities)-1 do begin
  Entities[Counter].Key:=0;
 end;
 SetLength(Entities,0);
 SetLength(EntityToCellIndex,0);
 SetLength(CellToEntityIndex,0);
 inherited Destroy;
end;

procedure TPACCInt64HashMap.Clear;
var Counter:longint;
begin
 for Counter:=0 to length(Entities)-1 do begin
  Entities[Counter].Key:=0;
 end;
 RealSize:=0;
 LogSize:=0;
 Size:=0;
 SetLength(Entities,0);
 SetLength(EntityToCellIndex,0);
 SetLength(CellToEntityIndex,0);
 Resize;
end;

function TPACCInt64HashMap.FindCell(const Key:TPACCInt64):TPACCUInt32;
var HashCode,Mask,Step:TPACCUInt32;
    Entity:longint;
begin
 HashCode:=HashInt64(Key);
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

procedure TPACCInt64HashMap.Resize;
var NewLogSize,NewSize,Cell,Entity,Counter:longint;
    OldEntities:TPACCInt64HashMapEntities;
    OldCellToEntityIndex:TPACCInt64HashMapEntityIndices;
    OldEntityToCellIndex:TPACCInt64HashMapEntityIndices;
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
  OldEntities[Counter].Key:=0;
 end;
 SetLength(OldEntities,0);
 SetLength(OldCellToEntityIndex,0);
 SetLength(OldEntityToCellIndex,0);
end;

function TPACCInt64HashMap.Add(const Key:TPACCInt64;const Value:TPACCInt64HashMapData):PPACCInt64HashMapEntity;
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

function TPACCInt64HashMap.Get(const Key:TPACCInt64;const CreateIfNotExist:boolean=false):PPACCInt64HashMapEntity;
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

function TPACCInt64HashMap.Delete(const Key:TPACCInt64;const DoParent:boolean=false):boolean;
var Entity:longint;
    Cell:TPACCUInt32;
begin
 result:=false;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  Entities[Entity].Key:=0;
  Entities[Entity].Value:=nil;
  EntityToCellIndex[Entity]:=CELL_DELETED;
  CellToEntityIndex[Cell]:=ENT_DELETED;
  result:=true;
 end else if DoParent and assigned(Parent) then begin
  result:=Parent.Delete(Key);
 end;
end;

function TPACCInt64HashMap.GetValue(const Key:TPACCInt64):TPACCInt64HashMapData;
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

procedure TPACCInt64HashMap.SetValue(const Key:TPACCInt64;const Value:TPACCInt64HashMapData);
begin
 Add(Key,Value);
end;

end.
