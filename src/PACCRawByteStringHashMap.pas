unit PACCRawByteStringHashMap;
{$i PACC.inc}

interface

uses PACCTypes;

type TPACCRawByteStringHashMapData=pointer;

     PPACCRawByteStringHashMapEntity=^TPACCRawByteStringHashMapEntity;
     TPACCRawByteStringHashMapEntity=record
      Key:TPACCRawByteString;
      Value:TPACCRawByteStringHashMapData;
     end;

     TPACCRawByteStringHashMapEntities=array of TPACCRawByteStringHashMapEntity;

     TPACCRawByteStringHashMapEntityIndices=array of longint;

     TPACCRawByteStringHashMap=class
      private
       function FindCell(const Key:TPACCRawByteString):TPACCUInt32;
       procedure Resize;
      protected
       function GetValue(const Key:TPACCRawByteString):TPACCRawByteStringHashMapData;
       procedure SetValue(const Key:TPACCRawByteString;const Value:TPACCRawByteStringHashMapData);
      public
       Parent:TPACCRawByteStringHashMap;
       RealSize:longint;
       LogSize:longint;
       Size:longint;
       Entities:TPACCRawByteStringHashMapEntities;
       EntityToCellIndex:TPACCRawByteStringHashMapEntityIndices;
       CellToEntityIndex:TPACCRawByteStringHashMapEntityIndices;
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function Add(const Key:TPACCRawByteString;const Value:TPACCRawByteStringHashMapData):PPACCRawByteStringHashMapEntity;
       function Get(const Key:TPACCRawByteString;const CreateIfNotExist:boolean=false):PPACCRawByteStringHashMapEntity;
       function Delete(const Key:TPACCRawByteString;const DoParent:boolean=false):boolean;
       property Values[const Key:TPACCRawByteString]:TPACCRawByteStringHashMapData read GetValue write SetValue; default;
     end;

implementation

const CELL_EMPTY=-1;
      CELL_DELETED=-2;

      ENT_EMPTY=-1;
      ENT_DELETED=-2;

function HashString(const Str:TPACCRawByteString):TPACCUInt32;
{$ifdef cpuarm}
var b:pansichar;
    len,h,i:TPACCUInt32;
begin
 result:=2166136261;
 len:=length(Str);
 h:=len;
 if len>0 then begin
  b:=pansichar(Str);
  while len>3 do begin
   i:=TPACCUInt32(pointer(b)^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
   inc(b,4);
   dec(len,4);
  end;
  if len>1 then begin
   i:=word(pointer(b)^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
   inc(b,2);
   dec(len,2);
  end;
  if len>0 then begin
   i:=byte(b^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
  end;
 end;
 result:=result xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;
{$else}
const m=TPACCUInt32($57559429);
      n=TPACCUInt32($5052acdb);
var b:pansichar;
    h,k,len:TPACCUInt32;
    p:{$ifdef fpc}qword{$else}int64{$endif};
begin
 len:=length(Str);
 h:=len;
 k:=h+n+1;
 if len>0 then begin
  b:=pansichar(Str);
  while len>7 do begin
   begin
    p:=TPACCUInt32(pointer(b)^)*{$ifdef fpc}qword{$else}int64{$endif}(n);
    h:=h xor TPACCUInt32(p and $ffffffff);
    k:=k xor TPACCUInt32(p shr 32);
    inc(b,4);
   end;
   begin
    p:=TPACCUInt32(pointer(b)^)*{$ifdef fpc}qword{$else}int64{$endif}(m);
    k:=k xor TPACCUInt32(p and $ffffffff);
    h:=h xor TPACCUInt32(p shr 32);
    inc(b,4);
   end;
   dec(len,8);
  end;
  if len>3 then begin
   p:=TPACCUInt32(pointer(b)^)*{$ifdef fpc}qword{$else}int64{$endif}(n);
   h:=h xor TPACCUInt32(p and $ffffffff);
   k:=k xor TPACCUInt32(p shr 32);
   inc(b,4);
   dec(len,4);
  end;
  if len>0 then begin
   if len>1 then begin
    p:=word(pointer(b)^);
    inc(b,2);
    dec(len,2);
   end else begin
    p:=0;
   end;
   if len>0 then begin
    p:=p or (byte(b^) shl 16);
   end;
   p:=p*{$ifdef fpc}qword{$else}int64{$endif}(m);
   k:=k xor TPACCUInt32(p and $ffffffff);
   h:=h xor TPACCUInt32(p shr 32);
  end;
 end;
 begin
  p:=(h xor (k+n))*{$ifdef fpc}qword{$else}int64{$endif}(n);
  h:=h xor TPACCUInt32(p and $ffffffff);
  k:=k xor TPACCUInt32(p shr 32);
 end;
 result:=k xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;
{$endif}

constructor TPACCRawByteStringHashMap.Create;
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

destructor TPACCRawByteStringHashMap.Destroy;
var Counter:longint;
begin
 Clear;
 for Counter:=0 to length(Entities)-1 do begin
  Entities[Counter].Key:='';
 end;
 SetLength(Entities,0);
 SetLength(EntityToCellIndex,0);
 SetLength(CellToEntityIndex,0);
 inherited Destroy;
end;

procedure TPACCRawByteStringHashMap.Clear;
var Counter:longint;
begin
 for Counter:=0 to length(Entities)-1 do begin
  Entities[Counter].Key:='';
 end;
 RealSize:=0;
 LogSize:=0;
 Size:=0;
 SetLength(Entities,0);
 SetLength(EntityToCellIndex,0);
 SetLength(CellToEntityIndex,0);
 Resize;
end;

function TPACCRawByteStringHashMap.FindCell(const Key:TPACCRawByteString):TPACCUInt32;
var HashCode,Mask,Step:TPACCUInt32;
    Entity:longint;
begin
 HashCode:=HashString(Key);
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

procedure TPACCRawByteStringHashMap.Resize;
var NewLogSize,NewSize,Cell,Entity,Counter:longint;
    OldEntities:TPACCRawByteStringHashMapEntities;
    OldCellToEntityIndex:TPACCRawByteStringHashMapEntityIndices;
    OldEntityToCellIndex:TPACCRawByteStringHashMapEntityIndices;
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
  OldEntities[Counter].Key:='';
 end;
 SetLength(OldEntities,0);
 SetLength(OldCellToEntityIndex,0);
 SetLength(OldEntityToCellIndex,0);
end;

function TPACCRawByteStringHashMap.Add(const Key:TPACCRawByteString;const Value:TPACCRawByteStringHashMapData):PPACCRawByteStringHashMapEntity;
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

function TPACCRawByteStringHashMap.Get(const Key:TPACCRawByteString;const CreateIfNotExist:boolean=false):PPACCRawByteStringHashMapEntity;
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

function TPACCRawByteStringHashMap.Delete(const Key:TPACCRawByteString;const DoParent:boolean=false):boolean;
var Entity:longint;
    Cell:TPACCUInt32;
begin
 result:=false;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  Entities[Entity].Key:='';
  Entities[Entity].Value:=nil;
  EntityToCellIndex[Entity]:=CELL_DELETED;
  CellToEntityIndex[Cell]:=ENT_DELETED;
  result:=true;
 end else if DoParent and assigned(Parent) then begin
  result:=Parent.Delete(Key);
 end;
end;

function TPACCRawByteStringHashMap.GetValue(const Key:TPACCRawByteString):TPACCRawByteStringHashMapData;
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

procedure TPACCRawByteStringHashMap.SetValue(const Key:TPACCRawByteString;const Value:TPACCRawByteStringHashMapData);
begin
 Add(Key,Value);
end;

end.
