program GenCodePageList;
{$APPTYPE CONSOLE}
uses
  Windows,
  SysUtils,
  Classes;

function LCIDToCodePage(ALcid:LCID):Cardinal;
var Buf:array[0..6] of AnsiChar;
begin
 GetLocaleInfo(ALcid,LOCALE_IDefaultAnsiCodePage,Buf,6);
 Result:=StrToIntDef(Buf,GetACP);
end;

var i:integer;
    t:textfile;
begin
 AssignFile(t,'..\PACCLinker_COFF_PE_LCIDToCodePageLookUpTable.inc');
 Rewrite(t);
 writeln(t,'const LCIDToCodePageLookUpTable:array[0..$ffff] of TPACCUInt16=(');
 for i:=0 to $ffff do begin
  if i<$ffff then begin
   write(t,LCIDToCodePage(i),',');
   if (i and 15)=15 then begin
    writeln(t);
   end;
  end else begin
   write(t,LCIDToCodePage(i));
  end;
 end;
 write(t,');');
 CloseFile(t);
end.
