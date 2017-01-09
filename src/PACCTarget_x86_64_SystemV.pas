unit PACCTarget_x86_64_SystemV;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCAbstractSyntaxTree,PACCTarget;

type TPACCTarget_x86_64_SystemV=class(TPACCTarget)
      private

      public

       constructor Create(const AInstance:TObject); override;
       destructor Destroy; override;

       class function GetName:TPACCRawByteString; override;

       function CheckCallingConvention(const AName:TPACCRawByteString):TPACCInt32; override;

       procedure GenerateCode(const ARoot:TPACCAbstractSyntaxTreeNode;const AOutputStream:TStream); override;

       procedure AssembleCode(const AInputStream,AOutputStream:TStream); override;

       procedure LinkCode(const AInputStreams:TList;const AOutputStream:TStream); override;

     end;

implementation

uses PACCInstance;

constructor TPACCTarget_x86_64_SystemV.Create(const AInstance:TObject);
begin
 inherited Create(AInstance);

 PreprocessorCode:='#define LP64'#10+
                   '#define __LP64__'#10+
                   '#define __x86_64'#10+
                   '#define __x86_64__'#10+
                   '';

 SizeOfPointer:=8;
 SizeOf_PTRDIFF_T:=8;
 SizeOf_SIZE_T:=8;
 SizeOfBool:=1;
 SizeOfChar:=1;
 SizeOfShort:=2;
 SizeOfInt:=4;
 SizeOfLong:=8;
 SizeOfLongLong:=8;
 SizeOfFloat:=4;
 SizeOfDouble:=8;
 SizeOfLongDouble:=8;
 SizeOfEnum:=4;

 AlignmentOfPointer:=8;
 AlignmentOf_PTRDIFF_T:=8;
 AlignmentOf_SIZE_T:=8;
 AlignmentOfBool:=1;
 AlignmentOfChar:=1;
 AlignmentOfShort:=2;
 AlignmentOfInt:=4;
 AlignmentOfLong:=8;
 AlignmentOfLongLong:=8;
 AlignmentOfFloat:=4;
 AlignmentOfDouble:=8;
 AlignmentOfLongDouble:=8;
 AlignmentOfEnum:=4;

 MaximumAlignment:=16;

end;

destructor TPACCTarget_x86_64_SystemV.Destroy;
begin
 inherited Destroy;
end;

class function TPACCTarget_x86_64_SystemV.GetName:TPACCRawByteString;
begin
 result:='x86_64_systemv';
end;

function TPACCTarget_x86_64_SystemV.CheckCallingConvention(const AName:TPACCRawByteString):TPACCInt32;
begin
 result:=-1;
end;

procedure TPACCTarget_x86_64_SystemV.GenerateCode(const ARoot:TPACCAbstractSyntaxTreeNode;const AOutputStream:TStream);
begin
end;

procedure TPACCTarget_x86_64_SystemV.AssembleCode(const AInputStream,AOutputStream:TStream);
begin
end;

procedure TPACCTarget_x86_64_SystemV.LinkCode(const AInputStreams:TList;const AOutputStream:TStream);
begin
end;

initialization
 PACCRegisterTarget(TPACCTarget_x86_64_SystemV);
end.


