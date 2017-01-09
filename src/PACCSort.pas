unit PACCSort;
{$i PACC.inc}

interface

uses PACCTypes,PasMP;

type TUntypedSortCompareFunction=function(const a,b:pointer):TPACCInt32;

     TIndirectSortCompareFunction=function(const a,b:pointer):TPACCInt32;

// Sorts data direct inplace
procedure UntypedDirectIntroSort(const pItems:pointer;const pLeft,pRight,pElementSize:TPACCInt32;const pCompareFunc:TUntypedSortCompareFunction);

// Sorts data indirect outplace with an extra pointer array
procedure IndirectIntroSort(const pItems:pointer;const pLeft,pRight:TPACCInt32;const pCompareFunc:TIndirectSortCompareFunction);

implementation

procedure MemorySwap(pA,pB:pointer;pSize:TPACCInt32);
var Temp:TPACCInt32;
begin
 while pSize>=SizeOf(TPACCInt32) do begin
  Temp:=TPACCUInt32(pA^);
  TPACCUInt32(pA^):=TPACCUInt32(pB^);
  TPACCUInt32(pB^):=Temp;
  inc(TPACCPtrUInt(pA),SizeOf(TPACCUInt32));
  inc(TPACCPtrUInt(pB),SizeOf(TPACCUInt32));
  dec(pSize,SizeOf(TPACCUInt32));
 end;
 while pSize>=SizeOf(TPACCUInt8) do begin
  Temp:=TPACCUInt8(pA^);
  TPACCUInt8(pA^):=TPACCUInt8(pB^);
  TPACCUInt8(pB^):=Temp;
  inc(TPACCPtrUInt(pA),SizeOf(TPACCUInt8));
  inc(TPACCPtrUInt(pB),SizeOf(TPACCUInt8));
  dec(pSize,SizeOf(TPACCUInt8));
 end;
end;

procedure UntypedDirectIntroSort(const pItems:pointer;const pLeft,pRight,pElementSize:TPACCInt32;const pCompareFunc:TUntypedSortCompareFunction);
type PByteArray=^TByteArray;
     TByteArray=array[0..$3fffffff] of TPACCUInt8;
     PStackItem=^TStackItem;
     TStackItem=record
      Left,Right,Depth:TPACCInt32;
     end;
var Left,Right,Depth,i,j,Middle,Size,Parent,Child,Pivot,iA,iB,iC:TPACCInt32;
    StackItem:PStackItem;
    Stack:array[0..31] of TStackItem;
begin
 if pLeft<pRight then begin
  StackItem:=@Stack[0];
  StackItem^.Left:=pLeft;
  StackItem^.Right:=pRight;
  StackItem^.Depth:=TPasMPMath.BitScanReverse((pRight-pLeft)+1) shl 1;
  inc(StackItem);
  while TPACCPtrUInt(pointer(StackItem))>TPACCPtrUInt(pointer(@Stack[0])) do begin
   dec(StackItem);
   Left:=StackItem^.Left;
   Right:=StackItem^.Right;
   Depth:=StackItem^.Depth;
   Size:=(Right-Left)+1;
   if Size<16 then begin
    // Insertion sort
    iA:=Left;
    iB:=iA+1;
    while iB<=Right do begin
     iC:=iB;
     while (iA>=Left) and
           (iC>=Left) and
           (pCompareFunc(pointer(@PByteArray(pItems)^[iA*pElementSize]),pointer(@PByteArray(pItems)^[iC*pElementSize]))>0) do begin
      MemorySwap(@PByteArray(pItems)^[iA*pElementSize],@PByteArray(pItems)^[iC*pElementSize],pElementSize);
      dec(iA);
      dec(iC);
     end;
     iA:=iB;
     inc(iB);
    end;
   end else begin
    if (Depth=0) or (TPACCPtrUInt(pointer(StackItem))>=TPACCPtrUInt(pointer(@Stack[high(Stack)-1]))) then begin
     // Heap sort
     i:=Size div 2;
     repeat
      if i>0 then begin
       dec(i);
      end else begin
       dec(Size);
       if Size>0 then begin
        MemorySwap(@PByteArray(pItems)^[(Left+Size)*pElementSize],@PByteArray(pItems)^[Left*pElementSize],pElementSize);
       end else begin
        break;
       end;
      end;
      Parent:=i;
      repeat
       Child:=(Parent*2)+1;
       if Child<Size then begin
        if (Child<(Size-1)) and (pCompareFunc(pointer(@PByteArray(pItems)^[(Left+Child)*pElementSize]),pointer(@PByteArray(pItems)^[(Left+Child+1)*pElementSize]))<0) then begin
         inc(Child);
        end;
        if pCompareFunc(pointer(@PByteArray(pItems)^[(Left+Parent)*pElementSize]),pointer(@PByteArray(pItems)^[(Left+Child)*pElementSize]))<0 then begin
         MemorySwap(@PByteArray(pItems)^[(Left+Parent)*pElementSize],@PByteArray(pItems)^[(Left+Child)*pElementSize],pElementSize);
         Parent:=Child;
         continue;
        end;
       end;
       break;
      until false;
     until false;
    end else begin
     // Quick sort width median-of-three optimization
     Middle:=Left+((Right-Left) shr 1);
     if (Right-Left)>3 then begin
      if pCompareFunc(pointer(@PByteArray(pItems)^[Left*pElementSize]),pointer(@PByteArray(pItems)^[Middle*pElementSize]))>0 then begin
       MemorySwap(@PByteArray(pItems)^[Left*pElementSize],@PByteArray(pItems)^[Middle*pElementSize],pElementSize);
      end;
      if pCompareFunc(pointer(@PByteArray(pItems)^[Left*pElementSize]),pointer(@PByteArray(pItems)^[Right*pElementSize]))>0 then begin
       MemorySwap(@PByteArray(pItems)^[Left*pElementSize],@PByteArray(pItems)^[Right*pElementSize],pElementSize);
      end;
      if pCompareFunc(pointer(@PByteArray(pItems)^[Middle*pElementSize]),pointer(@PByteArray(pItems)^[Right*pElementSize]))>0 then begin
       MemorySwap(@PByteArray(pItems)^[Middle*pElementSize],@PByteArray(pItems)^[Right*pElementSize],pElementSize);
      end;
     end;
     Pivot:=Middle;
     i:=Left;
     j:=Right;
     repeat
      while (i<Right) and (pCompareFunc(pointer(@PByteArray(pItems)^[i*pElementSize]),pointer(@PByteArray(pItems)^[Pivot*pElementSize]))<0) do begin
       inc(i);
      end;
      while (j>=i) and (pCompareFunc(pointer(@PByteArray(pItems)^[j*pElementSize]),pointer(@PByteArray(pItems)^[Pivot*pElementSize]))>0) do begin
       dec(j);
      end;
      if i>j then begin
       break;
      end else begin
       if i<>j then begin
        MemorySwap(@PByteArray(pItems)^[i*pElementSize],@PByteArray(pItems)^[j*pElementSize],pElementSize);
        if Pivot=i then begin
         Pivot:=j;
        end else if Pivot=j then begin
         Pivot:=i;
        end;
       end;
       inc(i);
       dec(j);
      end;
     until false;
     if i<Right then begin
      StackItem^.Left:=i;
      StackItem^.Right:=Right;
      StackItem^.Depth:=Depth-1;
      inc(StackItem);
     end;
     if Left<j then begin
      StackItem^.Left:=Left;
      StackItem^.Right:=j;
      StackItem^.Depth:=Depth-1;
      inc(StackItem);
     end;
    end;
   end;
  end;
 end;
end;

procedure IndirectIntroSort(const pItems:pointer;const pLeft,pRight:TPACCInt32;const pCompareFunc:TIndirectSortCompareFunction);
type PPointers=^TPointers;
     TPointers=array[0..$ffff] of pointer;
     PStackItem=^TStackItem;
     TStackItem=record
      Left,Right,Depth:TPACCInt32;
     end;
var Left,Right,Depth,i,j,Middle,Size,Parent,Child:TPACCInt32;
    Pivot,Temp:pointer;
    StackItem:PStackItem;
    Stack:array[0..31] of TStackItem;
begin
 if pLeft<pRight then begin
  StackItem:=@Stack[0];
  StackItem^.Left:=pLeft;
  StackItem^.Right:=pRight;
  StackItem^.Depth:=TPasMPMath.BitScanReverse((pRight-pLeft)+1) shl 1;
  inc(StackItem);
  while TPACCPtrUInt(pointer(StackItem))>TPACCPtrUInt(pointer(@Stack[0])) do begin
   dec(StackItem);
   Left:=StackItem^.Left;
   Right:=StackItem^.Right;
   Depth:=StackItem^.Depth;
   Size:=(Right-Left)+1;
   if Size<16 then begin
    // Insertion sort
    for i:=Left+1 to Right do begin
     Temp:=PPointers(pItems)^[i];
     j:=i-1;
     if (j>=Left) and (pCompareFunc(PPointers(pItems)^[j],Temp)>0) then begin
      repeat
       PPointers(pItems)^[j+1]:=PPointers(pItems)^[j];
       dec(j);
      until not ((j>=Left) and (pCompareFunc(PPointers(pItems)^[j],Temp)>0));
      PPointers(pItems)^[j+1]:=Temp;
     end;
    end;
   end else begin
    if (Depth=0) or (TPACCPtrUInt(pointer(StackItem))>=TPACCPtrUInt(pointer(@Stack[high(Stack)-1]))) then begin
     // Heap sort
     i:=Size div 2;
     Temp:=nil;
     repeat
      if i>0 then begin
       dec(i);
       Temp:=PPointers(pItems)^[Left+i];
      end else begin
       dec(Size);
       if Size>0 then begin
        Temp:=PPointers(pItems)^[Left+Size];
        PPointers(pItems)^[Left+Size]:=PPointers(pItems)^[Left];
       end else begin
        break;
       end;
      end;
      Parent:=i;
      Child:=(i*2)+1;
      while Child<Size do begin
       if ((Child+1)<Size) and (pCompareFunc(PPointers(pItems)^[Left+Child+1],PPointers(pItems)^[Left+Child])>0) then begin
        inc(Child);
       end;
       if pCompareFunc(PPointers(pItems)^[Left+Child],Temp)>0 then begin
        PPointers(pItems)^[Left+Parent]:=PPointers(pItems)^[Left+Child];
        Parent:=Child;
        Child:=(Parent*2)+1;
       end else begin
        break;
       end;
      end;
      PPointers(pItems)^[Left+Parent]:=Temp;
     until false;
    end else begin
     // Quick sort width median-of-three optimization
     Middle:=Left+((Right-Left) shr 1);
     if (Right-Left)>3 then begin
      if pCompareFunc(PPointers(pItems)^[Left],PPointers(pItems)^[Middle])>0 then begin
       Temp:=PPointers(pItems)^[Left];
       PPointers(pItems)^[Left]:=PPointers(pItems)^[Middle];
       PPointers(pItems)^[Middle]:=Temp;
      end;
      if pCompareFunc(PPointers(pItems)^[Left],PPointers(pItems)^[Right])>0 then begin
       Temp:=PPointers(pItems)^[Left];
       PPointers(pItems)^[Left]:=PPointers(pItems)^[Right];
       PPointers(pItems)^[Right]:=Temp;
      end;
      if pCompareFunc(PPointers(pItems)^[Middle],PPointers(pItems)^[Right])>0 then begin
       Temp:=PPointers(pItems)^[Middle];
       PPointers(pItems)^[Middle]:=PPointers(pItems)^[Right];
       PPointers(pItems)^[Right]:=Temp;
      end;
     end;
     Pivot:=PPointers(pItems)^[Middle];
     i:=Left;
     j:=Right;
     repeat
      while (i<Right) and (pCompareFunc(PPointers(pItems)^[i],Pivot)<0) do begin
       inc(i);
      end;
      while (j>=i) and (pCompareFunc(PPointers(pItems)^[j],Pivot)>0) do begin
       dec(j);
      end;
      if i>j then begin
       break;
      end else begin
       if i<>j then begin
        Temp:=PPointers(pItems)^[i];
        PPointers(pItems)^[i]:=PPointers(pItems)^[j];
        PPointers(pItems)^[j]:=Temp;
       end;
       inc(i);
       dec(j);
      end;
     until false;
     if i<Right then begin
      StackItem^.Left:=i;
      StackItem^.Right:=Right;
      StackItem^.Depth:=Depth-1;
      inc(StackItem);
     end;
     if Left<j then begin
      StackItem^.Left:=Left;
      StackItem^.Right:=j;
      StackItem^.Depth:=Depth-1;
      inc(StackItem);
     end;
    end;
   end;
  end;
 end;
end;

end.

