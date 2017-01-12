unit PACCTarget;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCAbstractSyntaxTree,PACCRawByteStringHashMap;

type TPACCTarget=class
      private

       fInstance:TObject;

       fPreprocessorCode:TPACCRawByteString;

       fSizeOfPointer:TPACCInt32;
       fSizeOf_PTRDIFF_T:TPACCInt32;
       fSizeOf_SIZE_T:TPACCInt32;
       fSizeOfBool:TPACCInt32;
       fSizeOfChar:TPACCInt32;
       fSizeOfShort:TPACCInt32;
       fSizeOfInt:TPACCInt32;
       fSizeOfLong:TPACCInt32;
       fSizeOfLongLong:TPACCInt32;
       fSizeOfFloat:TPACCInt32;
       fSizeOfDouble:TPACCInt32;
       fSizeOfLongDouble:TPACCInt32;
       fSizeOfEnum:TPACCInt32;

       fAlignmentOfPointer:TPACCInt32;
       fAlignmentOf_PTRDIFF_T:TPACCInt32;
       fAlignmentOf_SIZE_T:TPACCInt32;
       fAlignmentOfBool:TPACCInt32;
       fAlignmentOfChar:TPACCInt32;
       fAlignmentOfShort:TPACCInt32;
       fAlignmentOfInt:TPACCInt32;
       fAlignmentOfLong:TPACCInt32;
       fAlignmentOfLongLong:TPACCInt32;
       fAlignmentOfFloat:TPACCInt32;
       fAlignmentOfDouble:TPACCInt32;
       fAlignmentOfLongDouble:TPACCInt32;
       fAlignmentOfEnum:TPACCInt32;

       fMaximumAlignment:TPACCInt32;

      public

       constructor Create(const AInstance:TObject); reintroduce; virtual;
       destructor Destroy; override;

       class function GetName:TPACCRawByteString; virtual;

       function CheckCallingConvention(const AName:TPACCRawByteString):TPACCInt32; virtual;

       procedure GenerateCode(const ARoot:TPACCAbstractSyntaxTreeNode;const AOutputStream:TStream); virtual;

       procedure AssembleCode(const AInputStream,AOutputStream:TStream;const AInputFileName:TPUCUUTF8String=''); virtual;

       procedure LinkCode(const AInputStreams:TList;const AInputFileNames:TStringList;const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String=''); virtual;

      published

       property Instance:TObject read fInstance;

       property PreprocessorCode:TPACCRawByteString read fPreprocessorCode write fPreprocessorCode;

       property SizeOfPointer:TPACCInt32 read fSizeOfPointer write fSizeOfPointer;
       property SizeOf_PTRDIFF_T:TPACCInt32 read fSizeOf_PTRDIFF_T write fSizeOf_PTRDIFF_T;
       property SizeOf_SIZE_T:TPACCInt32 read fSizeOf_SIZE_T write fSizeOf_SIZE_T;
       property SizeOfBool:TPACCInt32 read fSizeOfBool write fSizeOfBool;
       property SizeOfChar:TPACCInt32 read fSizeOfChar write fSizeOfChar;
       property SizeOfShort:TPACCInt32 read fSizeOfShort write fSizeOfShort;
       property SizeOfInt:TPACCInt32 read fSizeOfInt write fSizeOfInt;
       property SizeOfLong:TPACCInt32 read fSizeOfLong write fSizeOfLong;
       property SizeOfLongLong:TPACCInt32 read fSizeOfLongLong write fSizeOfLongLong;
       property SizeOfFloat:TPACCInt32 read fSizeOfFloat write fSizeOfFloat;
       property SizeOfDouble:TPACCInt32 read fSizeOfDouble write fSizeOfDouble;
       property SizeOfLongDouble:TPACCInt32 read fSizeOfLongDouble write fSizeOfLongDouble;
       property SizeOfEnum:TPACCInt32 read fSizeOfEnum write fSizeOfEnum;

       property AlignmentOfPointer:TPACCInt32 read fAlignmentOfPointer write fAlignmentOfPointer;
       property AlignmentOf_PTRDIFF_T:TPACCInt32 read fAlignmentOf_PTRDIFF_T write fAlignmentOf_PTRDIFF_T;
       property AlignmentOf_SIZE_T:TPACCInt32 read fAlignmentOf_SIZE_T write fAlignmentOf_SIZE_T;
       property AlignmentOfBool:TPACCInt32 read fAlignmentOfBool write fAlignmentOfBool;
       property AlignmentOfChar:TPACCInt32 read fAlignmentOfChar write fAlignmentOfChar;
       property AlignmentOfShort:TPACCInt32 read fAlignmentOfShort write fAlignmentOfShort;
       property AlignmentOfInt:TPACCInt32 read fAlignmentOfInt write fAlignmentOfInt;
       property AlignmentOfLong:TPACCInt32 read fAlignmentOfLong write fAlignmentOfLong;
       property AlignmentOfLongLong:TPACCInt32 read fAlignmentOfLongLong write fAlignmentOfLongLong;
       property AlignmentOfFloat:TPACCInt32 read fAlignmentOfFloat write fAlignmentOfFloat;
       property AlignmentOfDouble:TPACCInt32 read fAlignmentOfDouble write fAlignmentOfDouble;
       property AlignmentOfLongDouble:TPACCInt32 read fAlignmentOfLongDouble write fAlignmentOfLongDouble;
       property AlignmentOfEnum:TPACCInt32 read fAlignmentOfEnum write fAlignmentOfEnum;

       property MaximumAlignment:TPACCInt32 read fMaximumAlignment write fMaximumAlignment;

     end;

     TPACCTargetClass=class of TPACCTarget;

var PACCRegisteredTargetClassList:TList;
    PACCRegisteredTargetClassHashMap:TPACCRawByteStringHashMap;

procedure PACCRegisterTarget(const ATargetClass:TPACCTargetClass);

implementation

uses PACCInstance;

constructor TPACCTarget.Create(const AInstance:TObject);
begin
 inherited Create;

 fInstance:=AInstance;

 fPreprocessorCode:='';
 
 fSizeOfPointer:=SizeOf(pointer);
 fSizeOf_PTRDIFF_T:=SizeOf(TPACCPtrInt);
 fSizeOf_SIZE_T:=SizeOf(TPACCPtrUInt);
 fSizeOfBool:=SizeOf(TPACCUInt8);
 fSizeOfChar:=SizeOf(TPACCUInt8);
 fSizeOfShort:=SizeOf(TPACCUInt16);
 fSizeOfInt:=SizeOf(TPACCUInt32);
 fSizeOfLong:=SizeOf(TPACCUInt64);
 fSizeOfLongLong:=SizeOf(TPACCUInt64);
 fSizeOfFloat:=SizeOf(TPACCFloat);
 fSizeOfDouble:=SizeOf(TPACCDouble);
 fSizeOfLongDouble:=SizeOf(TPACCLongDouble);
 fSizeOfEnum:=SizeOf(TPACCInt32);

 fAlignmentOfPointer:=SizeOf(pointer);
 fAlignmentOf_PTRDIFF_T:=SizeOf(TPACCPtrInt);
 fAlignmentOf_SIZE_T:=SizeOf(TPACCPtrUInt);
 fAlignmentOfBool:=SizeOf(TPACCUInt8);
 fAlignmentOfChar:=SizeOf(TPACCUInt8);
 fAlignmentOfShort:=SizeOf(TPACCUInt16);
 fAlignmentOfInt:=SizeOf(TPACCUInt32);
 fAlignmentOfLong:=SizeOf(TPACCUInt64);
 fAlignmentOfLongLong:=SizeOf(TPACCUInt64);
 fAlignmentOfFloat:=SizeOf(TPACCFloat);
 fAlignmentOfDouble:=SizeOf(TPACCDouble);
 fAlignmentOfLongDouble:=SizeOf(TPACCLongDouble);
 fAlignmentOfEnum:=SizeOf(TPACCInt32);

 fMaximumAlignment:=SizeOf(TPACCUInt64)*2;

end;

destructor TPACCTarget.Destroy;
begin
 inherited Destroy;
end;

class function TPACCTarget.GetName:TPACCRawByteString;
begin
 result:='';
end;

function TPACCTarget.CheckCallingConvention(const AName:TPACCRawByteString):TPACCInt32;
begin
 result:=-1;
end;

procedure TPACCTarget.GenerateCode(const ARoot:TPACCAbstractSyntaxTreeNode;const AOutputStream:TStream);
begin
end;

procedure TPACCTarget.AssembleCode(const AInputStream,AOutputStream:TStream;const AInputFileName:TPUCUUTF8String='');
begin
end;

procedure TPACCTarget.LinkCode(const AInputStreams:TList;const AInputFileNames:TStringList;const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String='');
var Index:TPACCInt32;
begin
 for Index:=0 to AInputStreams.Count-1 do begin
  if assigned(AInputStreams[Index]) then begin
   TPACCInstance(Instance).Linker.AddObject(TStream(AInputStreams[Index]),AInputFileNames[Index]);
  end;
 end;
 if assigned(AOutputStream) then begin
  TPACCInstance(Instance).Linker.Link(AOutputStream,AOutputFileName);
 end;
end;

procedure PACCRegisterTarget(const ATargetClass:TPACCTargetClass);
begin
 PACCRegisteredTargetClassList.Add(ATargetClass);
 PACCRegisteredTargetClassHashMap[ATargetClass.GetName]:=ATargetClass;
end;

initialization
 PACCRegisteredTargetClassList:=TList.Create;
 PACCRegisteredTargetClassHashMap:=TPACCRawByteStringHashMap.Create;
finalization
 PACCRegisteredTargetClassList.Free;
 PACCRegisteredTargetClassHashMap.Free;
end.
