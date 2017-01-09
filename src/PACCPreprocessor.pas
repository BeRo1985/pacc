unit PACCPreprocessor;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCRawByteStringHashMap;

type TPACCPreprocessorInputSourceKind=(iskNONE,iskFILE,iskMACRO);

     TPACCPreprocessorInputSourceLocation=record
      StartPosition:TPACCInt;
      EndPosition:TPACCInt;
      Line:TPACCInt;
      Column:TPACCInt;
     end;

     TPACCPreprocessorInputSourceLocations=array of TPACCPreprocessorInputSourceLocation;

     TPACCPreprocessorInputSource=record
      Kind:TPACCPreprocessorInputSourceKind;
      Name:TPUCUUTF8String;
      Index:TPACCInt;
     end;

     TPACCPreprocessorInputSources=array of TPACCPreprocessorInputSource;

     TPACCPreprocessorPragmaInfoItem=record
      CharPos:TPACCInt;
      Pragma:TPUCUUTF8String;
     end;

     TPACCPreprocessorPragmaInfo=array of TPACCPreprocessorPragmaInfoItem;

     TPACCPreprocessorOutputInfoItem=record
      FirstCharPos:TPACCInt32;
      LastCharPos:TPACCInt32;
      NewLineSincePos:TPACCInt32;
      SourceLocation:TPACCSourceLocation;
     end;

     TPACCPreprocessorOutputInfo=array of TPACCPreprocessorOutputInfoItem;

     TPACCPreprocessorDefines=array of TPUCUUTF8String;

     TPACCPreprocessor=class
      public

       Instance:TObject;

       PreprocessorChar:ansichar;

       SourceFiles:TStringList;
       IncludeDirectories:TStringList;
       PreprocessorDefines:TStringList;
       PreprocessorUndefines:TStringList;

       SourceLocation:TPACCSourceLocation;

       InputKind:TPACCPreprocessorInputSourceKind;
       InputName:TPUCUUTF8String;
       InputText:TPUCUUTF8String;

       InputSources:TPACCPreprocessorInputSources;
       CountInputSources:TPACCInt;

       PragmaInfos:TPACCPreprocessorPragmaInfo;
       CountPragmaInfos:TPACCInt;

       OutputInfos:TPACCPreprocessorOutputInfo;
       CountOutputInfos:TPACCInt;

       OutputText:TPUCUUTF8String;
       OutputTextLength:TPACCInt;

       CounterValue:TPACCInt;

       constructor Create(const AInstance:TObject);
       destructor Destroy; override;
       procedure ProcessIt;
       procedure ProcessString(const AInputText:TPUCUUTF8String;const AInputName:TPUCUUTF8String='');
       procedure ProcessFile(const AInputFileName:TPUCUUTF8String='');

    end;

implementation

uses PACCInstance;

function GetFileContent(fn:TPUCUUTF8String):TPACCRawByteString;
var FileStream:TFileStream;
begin
 result:='';
 FileStream:=TFileStream.Create(fn,fmOpenRead or fmShareDenyWrite);
 try
  SetLength(result,FileStream.Size);
  if length(result)>0 then begin
   FileStream.ReadBuffer(result[1],length(result));
  end;
 finally
  FileStream.Free;
 end;
end;

constructor TPACCPreprocessor.Create(const AInstance:TObject);
begin
 inherited Create;

 Instance:=AInstance;

 TPACCInstance(Instance).Preprocessor:=self;

 PreprocessorChar:='#';

 SourceFiles:=TStringList.Create;
 IncludeDirectories:=TStringList.Create;
 PreprocessorDefines:=TStringList.Create;
 PreprocessorDefines.NameValueSeparator:='=';
 PreprocessorUndefines:=TStringList.Create;
 PreprocessorUndefines.NameValueSeparator:='=';

 SourceLocation.Source:=-1;
 SourceLocation.Line:=0;
 SourceLocation.Column:=0;

 InputKind:=iskNONE;
 InputName:='';
 InputText:='';

 InputSources:=nil;
 CountInputSources:=0;

 PragmaInfos:=nil;
 CountPragmaInfos:=0;

 OutputInfos:=nil;
 CountOutputInfos:=0;

 OutputText:='';
 OutputTextLength:=0;

 CounterValue:=0;

end;

destructor TPACCPreprocessor.Destroy;
begin
 SourceFiles.Free;
 IncludeDirectories.Free;
 PreprocessorDefines.Free;
 PreprocessorUndefines.Free;
 InputName:='';
 InputText:='';
 SetLength(InputSources,0);
 SetLength(PragmaInfos,0);
 SetLength(OutputInfos,0);
 OutputText:='';
 inherited Destroy;
end;

procedure TPACCPreprocessor.ProcessIt;
type //TChars=set of ansichar;
     TToken=(tCHAR,tNUMBER,tNAME,
             tSTRING,TSTRINGUTF8,TSTRINGWCHAR,TSTRINGCHAR16,TSTRINGCHAR32,
             tCCHAR,TCCHARUTF8,TCCHARWCHAR,TCCHARCHAR16,TCCHARCHAR32);
     PInputStackItem=^TInputStackItem;
     TInputStackItem=record
      SourceLocation:TPACCSourceLocation;
      Buffer:TPUCUUTF8String;
      BufferLength:TPACCInt;
      BufferSourceLocationIndex:TPACCInt;
      BufferSourceLocations:TPACCPreprocessorInputSourceLocations;
      BufferPosition:TPACCInt;
      HideSet:TStringList;
     end;
     TInputStack=array of TInputStackItem;
     TMacroFunction=(mfNONE,mfFILE,mfLINE,mfTIME,mfDATE,mfPRAGMA,mfBASEFILE,mfCOUNTER,mfINCLUDELEVEL,mfTIMESTAMP);
     TMacroBodyItemKind=(mbikTEXT,mbikPARAMETER,mbikVAARGS,mbikSPLITTER);
     TMacroBodyItem=record
      Kind:TMacroBodyItemKind;
      Value:TPACCInt;
      Text:TPUCUUTF8String;
      Quote:boolean;
      CharQuote:boolean;
     end;
     TMacroBody=array of TMacroBodyItem;
     TMacro=record
      Defined:boolean;
      Name:TPUCUUTF8String;
      Body:TMacroBody;
      Parameters:TPACCInt;
      FunctionLike:boolean;
      VaArgs:boolean;
      MacroFunction:TMacroFunction;
     end;
     TMacros=array of TMacro;
const{WhiteSpace:TChars=[#9,#11..#13,#32,#255];
      WhiteSpaceEx:TChars=[#9,#10..#13,#32,#255];}
      UCS4WhiteSpace=[9,11..13,32,255];
      UCS4WhiteSpaceEx=[9,10..13,32,255];
      kwNONE=0;
      kwINCLUDE=1;
      kwINCLUDENEXT=2;
      kwINCLUDEONCE=3;
      kwINCLUDENEXTONCE=4;
      kwDEFINE=5;
      kwUNDEF=6;
      kwIF=7;
      kwIFDEF=8;
      kwIFNDEF=9;
      kwELIF=10;
      kwELSE=11;
      kwENDIF=12;
      kwLINE=13;
      kwPRAGMA=14;
      kwERROR=15;
      kwWARNING=16;
var InputStack:TInputStack;
    InputStackSize:TPACCInt;
    CurrentToken:TToken;
    CurrentTokenChar:TPACCInt;
    CurrentTokenValue:uint64;
    CurrentTokenString:TPUCUUTF8String;
    Macros:TMacros;
    CountMacros:TPACCInt;
    IFNestedLevel,MacroLevel,StringMode,NumberMode,CurrentChar:TPACCInt;
    LastSourceLocation:TPACCSourceLocation;
    InEval:boolean;
    MacroStringHashMap:TPACCRawByteStringHashMap;
    KeywordStringHashMap:TPACCRawByteStringHashMap;
    HideSet:TStringList;
 function RoundUpToPowerOfTwo(x:TPACCPtrUInt):TPACCPtrUInt;
 begin
  dec(x);
  x:=x or (x shr 1);
  x:=x or (x shr 2);
  x:=x or (x shr 4);
  x:=x or (x shr 8);
  x:=x or (x shr 16);
 {$ifdef cpu64}
  x:=x or (x shr 32);
 {$endif}
  result:=x+1;
 end;
 procedure AddError(const s:TPUCUUTF8String;const SourceLocation:PPACCSourceLocation=nil;const DoAbort:boolean=false);
 begin
  TPACCInstance(Instance).AddError(s,SourceLocation,DoAbort);
 end;
 procedure AddWarning(const s:TPUCUUTF8String;const SourceLocation:PPACCSourceLocation=nil);
 begin
  TPACCInstance(Instance).AddWarning(s,SourceLocation);
 end;
 function GetInputSourceIndex(const Kind:TPACCPreprocessorInputSourceKind;Name:TPUCUUTF8String):TPACCInt; forward;
 function hex2byte(c:TPUCUUTF32Char):TPACCInt;
 begin
  case c of
   ord('0')..ord('9'):begin
    result:=c-byte(ansichar('0'));
   end;
   ord('a')..ord('f'):begin
    result:=c-byte(ansichar('a'))+$a;
   end;
   ord('A')..ord('F'):begin
    result:=c-byte(ansichar('A'))+$a;
   end;
   else begin
    result:=0;
   end;
  end;
 end;
 function PreprocessInputSourceChars(Kind:TPACCPreprocessorInputSourceKind;const Name,Body:TPUCUUTF8String;var OutputSourceLocations:TPACCPreprocessorInputSourceLocations):TPUCUUTF8String;
 var i,OutLen,CountTempSourceLocations,SourceLocationIndex:TPACCInt;
     sc,tc:ansichar;
     OutStr,s:TPUCUUTF8String;
     OldSourceLocation:TPACCSourceLocation;
     FirstTrigraph:boolean;
     TempSourceLocations:TPACCPreprocessorInputSourceLocations;
  procedure AddSourceLocation;
  var Index:TPACCInt;
  begin
   if (CountTempSourceLocations=0) or
      ((CountTempSourceLocations>0) and
       ((TempSourceLocations[CountTempSourceLocations-1].Line<>LastSourceLocation.Line) or
        (TempSourceLocations[CountTempSourceLocations-1].Column<>LastSourceLocation.Column))) then begin
    if CountTempSourceLocations>0 then begin
     TempSourceLocations[CountTempSourceLocations-1].EndPosition:=OutLen;
    end;
    Index:=CountTempSourceLocations;
    inc(CountTempSourceLocations);
    if length(TempSourceLocations)<CountTempSourceLocations then begin
     SetLength(TempSourceLocations,CountTempSourceLocations*2);
    end;
    TempSourceLocations[Index].StartPosition:=OutLen+1;
    TempSourceLocations[Index].EndPosition:=OutLen+1;
    TempSourceLocations[Index].Line:=LastSourceLocation.Line;
    TempSourceLocations[Index].Column:=LastSourceLocation.Column;
   end;
  end;
  procedure AddChar(NewChar:ansichar);
  begin
   inc(OutLen);
   if OutLen>length(OutStr) then begin
    SetLength(OutStr,OutLen*2);
   end;
   OutStr[OutLen]:=NewChar;
  end;
  procedure AddString(const NewString:TPUCUUTF8String);
  var i:TPACCInt;
  begin
   for i:=1 to length(NewString) do begin
    AddChar(NewString[i]);
   end;
  end;
 begin
  result:='';
  TempSourceLocations:=nil;
  CountTempSourceLocations:=0;
  OldSourceLocation:=LastSourceLocation;
  try
   OutputSourceLocations:=nil;
   LastSourceLocation.Source:=GetInputSourceIndex(Kind,Name);
   LastSourceLocation.Line:=0;
   LastSourceLocation.Column:=0;
   s:=Body;
   if (length(s)>=3) and ((s[1]=#$ef) and (s[2]=#$bb) and (s[3]=#$bf)) then begin
    delete(s,1,3);
   end;

   // Processing of trigraphs
   if TPACCInstance(Instance).Options.HandleTrigraphs then begin
    OutStr:='';
    SetLength(OutStr,4096);
    OutLen:=0;
    FirstTrigraph:=true;
    TempSourceLocations:=nil;
    CountTempSourceLocations:=0;
    LastSourceLocation.Line:=0;
    LastSourceLocation.Column:=0;
    AddSourceLocation;
    i:=1;
    while i<=length(s) do begin
     sc:=s[i];
     case sc of
      #10:begin
       if ((i+1)<=length(s)) and (s[i+1]=#13) then begin
        inc(i,2);
       end else begin
        inc(i);
       end;
       inc(LastSourceLocation.Line);
       LastSourceLocation.Column:=0;
       AddChar(#10);
       AddSourceLocation;
      end;
      #13:begin
       if ((i+1)<=length(s)) and (s[i+1]=#10) then begin
        inc(i,2);
       end else begin
        inc(i);
       end;
       inc(LastSourceLocation.Line);
       LastSourceLocation.Column:=0;
       AddChar(#10);
       AddSourceLocation;
      end;
      '?':begin
       if ((i+2)<=length(s)) and (s[i+1]='?') and (s[i+2] in ['=','/','''','(',')','!','<','>','-']) then begin
        if TPACCInstance(Instance).Options.WarnTrigraphs then begin
         if FirstTrigraph or TPACCInstance(Instance).Options.WarnTrigraphsMore then begin
          FirstTrigraph:=false;
          AddWarning('Trigraph detected',@LastSourceLocation);
         end;
        end;
        case s[i+2] of
         '=':begin
          AddChar('#');
         end;
         '/':begin
          AddChar('\');
         end;
         '''':begin
          AddChar('^');
         end;
         '(':begin
          AddChar('[');
         end;
         ')':begin
          AddChar(']');
         end;
         '!':begin
          AddChar('|');
         end;
         '<':begin
          AddChar('{');
         end;
         '>':begin
          AddChar('}');
         end;
         '-':begin
          AddChar('~');
         end;
         else begin
          AddError('Internal error 2017-01-07-11-09-0000',@LastSourceLocation,true);
         end;
        end;
        inc(i,3);
        inc(LastSourceLocation.Column,3);
        AddSourceLocation;
       end else begin
        AddChar(sc);
        inc(i);
        inc(LastSourceLocation.Column);
       end;
      end;
      else begin
       AddChar(sc);
       inc(i);
      end;
     end;
    end;
    SetLength(TempSourceLocations,CountTempSourceLocations);
    if CountTempSourceLocations>0 then begin
     TempSourceLocations[CountTempSourceLocations-1].EndPosition:=OutLen;
    end;
    OutputSourceLocations:=copy(TempSourceLocations);
    s:=copy(OutStr,1,OutLen);
   end;

   // Processing of \ line breaks
   begin
    TempSourceLocations:=nil;
    CountTempSourceLocations:=0;
    SourceLocationIndex:=0;
    OutStr:='';
    SetLength(OutStr,4096);
    OutLen:=0;
    i:=1;
    LastSourceLocation.Line:=0;
    LastSourceLocation.Column:=0;
    AddSourceLocation;
    while i<=length(s) do begin
     if SourceLocationIndex<length(OutputSourceLocations) then begin
      while (SourceLocationIndex<length(OutputSourceLocations)) and
            ((OutputSourceLocations[SourceLocationIndex].StartPosition>i) or
             (OutputSourceLocations[SourceLocationIndex].EndPosition<i)) do begin
       inc(SourceLocationIndex);
      end;
      if SourceLocationIndex<length(OutputSourceLocations) then begin
       if (LastSourceLocation.Line<>OutputSourceLocations[SourceLocationIndex].Line) or
          (LastSourceLocation.Column<>OutputSourceLocations[SourceLocationIndex].Column) then begin
        LastSourceLocation.Line:=OutputSourceLocations[SourceLocationIndex].Line;
        LastSourceLocation.Column:=OutputSourceLocations[SourceLocationIndex].Column;
        AddSourceLocation;
       end;
      end;
     end;
     sc:=s[i];
     case sc of
      #10:begin
       if ((i+1)<=length(s)) and (s[i+1]=#13) then begin
        inc(i,2);
       end else begin
        inc(i);
       end;
       if SourceLocationIndex>=length(OutputSourceLocations) then begin
        inc(LastSourceLocation.Line);
        LastSourceLocation.Column:=0;
       end;
       AddChar(#10);
       AddSourceLocation;
      end;
      #13:begin
       if ((i+1)<=length(s)) and (s[i+1]=#10) then begin
        inc(i,2);
       end else begin
        inc(i);
       end;
       if SourceLocationIndex>=length(OutputSourceLocations) then begin
        inc(LastSourceLocation.Line);
        LastSourceLocation.Column:=0;
       end;
       AddChar(#10);
       AddSourceLocation;
      end;
      '\':begin
       if ((i+1)<=length(s)) and (s[i+1] in [#10,#13]) then begin
        inc(i);
        case s[i] of
         #10:begin
          if ((i+1)<=length(s)) and (s[i+1]=#13) then begin
           inc(i,2);
          end else begin
           inc(i);
          end;
          if SourceLocationIndex>=length(OutputSourceLocations) then begin
           inc(LastSourceLocation.Line);
          end;
         end;
         #13:begin
          if ((i+1)<=length(s)) and (s[i+1]=#10) then begin
           inc(i,2);
          end else begin
           inc(i);
          end;
          if SourceLocationIndex>=length(OutputSourceLocations) then begin
           inc(LastSourceLocation.Line);
          end;
         end;
        end;
        while (i<=length(s)) and (s[i] in [#0..#9,#11,#12,#14..#32]) do begin
         inc(i);
        end;
       end else begin
        AddChar(sc);
        inc(i);
        if SourceLocationIndex>=length(OutputSourceLocations) then begin
         inc(LastSourceLocation.Column);
        end;
       end;
      end;
      else begin
       AddChar(sc);
       inc(i);
       if SourceLocationIndex>=length(OutputSourceLocations) then begin
        inc(LastSourceLocation.Column);
       end;
      end;
     end;
    end;
    SetLength(TempSourceLocations,CountTempSourceLocations);
    if CountTempSourceLocations>0 then begin
     TempSourceLocations[CountTempSourceLocations-1].EndPosition:=OutLen;
    end;
    OutputSourceLocations:=copy(TempSourceLocations);
    s:=copy(OutStr,1,OutLen);
   end;

   // Processing of remaining input text preprocessing stuff
   begin
    TempSourceLocations:=nil;
    CountTempSourceLocations:=0;
    SourceLocationIndex:=0;
    OutStr:='';
    SetLength(OutStr,4096);
    OutLen:=0;
    i:=1;
    LastSourceLocation.Line:=0;
    LastSourceLocation.Column:=0;
    AddSourceLocation;
    while i<=length(s) do begin
     if SourceLocationIndex<length(OutputSourceLocations) then begin
      while (SourceLocationIndex<length(OutputSourceLocations)) and
            ((OutputSourceLocations[SourceLocationIndex].StartPosition>i) or
             (OutputSourceLocations[SourceLocationIndex].EndPosition<i)) do begin
       inc(SourceLocationIndex);
      end;
      if SourceLocationIndex<length(OutputSourceLocations) then begin
       if (LastSourceLocation.Line<>OutputSourceLocations[SourceLocationIndex].Line) or
          (LastSourceLocation.Column<>OutputSourceLocations[SourceLocationIndex].Column) then begin
        LastSourceLocation.Line:=OutputSourceLocations[SourceLocationIndex].Line;
        LastSourceLocation.Column:=OutputSourceLocations[SourceLocationIndex].Column;
        AddSourceLocation;
       end;
      end;
     end;
     sc:=s[i];
     case sc of
      #10:begin
       if ((i+1)<=length(s)) and (s[i+1]=#13) then begin
        inc(i,2);
       end else begin
        inc(i);
       end;
       if SourceLocationIndex>=length(OutputSourceLocations) then begin
        inc(LastSourceLocation.Line);
        LastSourceLocation.Column:=0;
       end;
       AddChar(#10);
       AddSourceLocation;
      end;
      #13:begin
       if ((i+1)<=length(s)) and (s[i+1]=#10) then begin
        inc(i,2);
       end else begin
        inc(i);
       end;
       if SourceLocationIndex>=length(OutputSourceLocations) then begin
        inc(LastSourceLocation.Line);
        LastSourceLocation.Column:=0;
       end;
       AddChar(#10);
       AddSourceLocation;
      end;
      '''','"':begin
       AddChar(sc);
       inc(i);
       if SourceLocationIndex>=length(OutputSourceLocations) then begin
        inc(LastSourceLocation.Column);
       end;
       while i<=length(s) do begin
        tc:=s[i];
        case tc of
         '''','"':begin
          AddChar(tc);
          inc(i);
          if SourceLocationIndex>=length(OutputSourceLocations) then begin
           inc(LastSourceLocation.Column);
          end;
          if tc=sc then begin
           break;
          end;
         end;
         '\':begin
          AddChar(tc);
          inc(i);
          if SourceLocationIndex>=length(OutputSourceLocations) then begin
           inc(LastSourceLocation.Column);
          end;
          if i<=length(s) then begin
           AddChar(s[i]);
           inc(i);
           if SourceLocationIndex>=length(OutputSourceLocations) then begin
            inc(LastSourceLocation.Column);
           end;
          end;
         end;
         else begin
          AddChar(tc);
          inc(i);
          if SourceLocationIndex>=length(OutputSourceLocations) then begin
           inc(LastSourceLocation.Column);
          end;
         end;
        end;
       end;
       AddSourceLocation;
      end;
      '\':begin
       if ((i+1)<=length(s)) and (s[i+1]='U') then begin
        if ((i+9)<=length(s)) and
           (s[i+2] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+3] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+4] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+5] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+6] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+7] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+8] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+9] in ['0'..'9','A'..'F','a'..'f']) then begin
         AddString(PUCUUTF32CharToUTF8((hex2byte(ord(s[i+2])) shl 28) or (hex2byte(ord(s[i+3])) shl 24) or (hex2byte(ord(s[i+4])) shl 20) or (hex2byte(ord(s[i+5])) shl 16) or (hex2byte(ord(s[i+6])) shl 12) or (hex2byte(ord(s[i+7])) shl 8) or (hex2byte(ord(s[i+8])) shl 4) or (hex2byte(ord(s[i+9])) shl 0)));
         inc(i,10);
         if SourceLocationIndex>=length(OutputSourceLocations) then begin
          inc(LastSourceLocation.Column,10);
         end;
        end else begin
         AddString('\U');
         inc(i,2);
         if SourceLocationIndex>=length(OutputSourceLocations) then begin
          inc(LastSourceLocation.Column,2);
         end;
        end;
       end else if ((i+1)<=length(s)) and (s[i+1]='u') then begin
        if ((i+5)<=length(s)) and
           (s[i+2] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+3] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+4] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+5] in ['0'..'9','A'..'F','a'..'f']) then begin
         AddString(PUCUUTF32CharToUTF8((hex2byte(ord(s[i+2])) shl 12) or (hex2byte(ord(s[i+3])) shl 8) or (hex2byte(ord(s[i+4])) shl 4) or (hex2byte(ord(s[i+5])) shl 0)));
         inc(i,6);
         if SourceLocationIndex>=length(OutputSourceLocations) then begin
          inc(LastSourceLocation.Column,6);
         end;
        end else begin
         AddString('\u');
         inc(i,2);
         if SourceLocationIndex>=length(OutputSourceLocations) then begin
          inc(LastSourceLocation.Column,2);
         end;
        end;
       end else if (i+1)<=length(s) then begin
 {      if s[i+1] in [#10,#13] then begin
         inc(i);
         case s[i] of
          #10:begin
           if ((i+1)<=length(s)) and (s[i+1]=#13) then begin
            inc(i,2);
           end else begin
            inc(i);
           end;
           inc(LastSourceLocation.Line);
          end;
          #13:begin
           if ((i+1)<=length(s)) and (s[i+1]=#10) then begin
            inc(i,2);
           end else begin
            inc(i);
           end;
           inc(LastSourceLocation.Line);
          end;
         end;
        end else}begin
         AddChar('\');
         AddChar(s[i+1]);
         inc(i,2);
         if SourceLocationIndex>=length(OutputSourceLocations) then begin
          inc(LastSourceLocation.Column,2);
         end;
        end;
       end;
       AddSourceLocation;
      end;
      else begin
       AddChar(sc);
       inc(i);
       if SourceLocationIndex>=length(OutputSourceLocations) then begin
        inc(LastSourceLocation.Column);
       end;
      end;
     end;
    end;
    SetLength(TempSourceLocations,CountTempSourceLocations);
    if CountTempSourceLocations>0 then begin
     TempSourceLocations[CountTempSourceLocations-1].EndPosition:=OutLen;
    end;
    OutputSourceLocations:=copy(TempSourceLocations);
    result:=copy(OutStr,1,OutLen);
   end;

  finally
   OutStr:='';
   LastSourceLocation:=OldSourceLocation;
   TempSourceLocations:=nil;
  end;
 end;
 function LookUpKeyword(const Name:TPUCUUTF8String):TPACCInt;
 begin
  result:=TPACCPtrInt(KeywordStringHashMap.Values[Name]);
 end;
 function LookUpEx(const Name:TPUCUUTF8String):TPACCInt;
 begin
  result:=TPACCPtrInt(MacroStringHashMap.Values[Name])-1;
 end;
 function LookUp(const Name:TPUCUUTF8String):TPACCInt;
 begin
  result:=LookUpEx(Name);
  if result>=0 then begin
   if not Macros[result].Defined then begin
    result:=-1;
   end;
  end;
 end;
 procedure AddDefine(const Name:TPUCUUTF8String;Body:TMacroBody;Parameters:TPACCInt;FunctionLike,VaArgs:boolean;MacroFunction:TMacroFunction);
 var i:TPACCInt;
 begin
  i:=LookUpEx(Name);
  if i<0 then begin
   i:=CountMacros;
   inc(CountMacros);
   if CountMacros>length(Macros) then begin
    SetLength(Macros,CountMacros*2);
   end;
  end;
  Macros[i].Defined:=true;
  Macros[i].Name:=Name;
  Macros[i].Body:=copy(Body,0,length(Body));
  Macros[i].Parameters:=Parameters;
  Macros[i].FunctionLike:=FunctionLike;
  Macros[i].VaArgs:=VaArgs;
  Macros[i].MacroFunction:=MacroFunction;
  MacroStringHashMap.Values[Name]:=pointer(TPACCPtrInt(i+1));
 end;
 procedure RemoveDefine(const Name:TPUCUUTF8String);
 var i:TPACCInt;
 begin
  i:=LookUpEx(Name);
  if i>=0 then begin
   Macros[i].Defined:=false;
  end;
 end;
 function GetInputSourceIndex(const Kind:TPACCPreprocessorInputSourceKind;Name:TPUCUUTF8String):TPACCInt;
 var i,j:TPACCInt;
 begin
  for i:=0 to CountInputSources-1 do begin
   if (InputSources[i].Kind=Kind) and (InputSources[i].Name=Name) then begin
    result:=i;
    exit;
   end;
  end;
  result:=CountInputSources;
  inc(CountInputSources);
  if (result+1)>length(InputSources) then begin
   SetLength(InputSources,RoundUpToPowerOfTwo(result+2));
  end;
  InputSources[result].Kind:=Kind;
  InputSources[result].Name:=Name;
  case Kind of
   iskMACRO:begin
    Name:='MACRO('+Name+')';
   end;
  end;
  j:=SourceFiles.IndexOf(Name);
  if j<0 then begin
   j:=SourceFiles.Add(Name);
  end;
  InputSources[result].Index:=j;
 end;
 procedure PushInputSource(Kind:TPACCPreprocessorInputSourceKind;const Name:TPUCUUTF8String;const Text:TPUCUUTF8String;const TextSourceLocations:TPACCPreprocessorInputSourceLocations);
 var i:TPACCInt;
 begin
  if InputStackSize>=length(InputStack) then begin
   SetLength(InputStack,(InputStackSize+1)*2);
  end;
  i:=InputStackSize;
  inc(InputStackSize);
  InputStack[i].SourceLocation.Source:=GetInputSourceIndex(Kind,Name);
  InputStack[i].SourceLocation.Line:=0;
  InputStack[i].SourceLocation.Column:=0;
  InputStack[i].Buffer:=Text;
  InputStack[i].BufferLength:=length(Text);
  InputStack[i].BufferSourceLocationIndex:=0;
  InputStack[i].BufferSourceLocations:=TextSourceLocations;
  InputStack[i].BufferPosition:=1;
  if assigned(InputStack[i].HideSet) then begin
   InputStack[i].HideSet.Clear;
  end else begin
   InputStack[i].HideSet:=TStringList.Create;
  end;
  if assigned(HideSet) then begin
   InputStack[i].HideSet.AddStrings(HideSet);
  end;
 end;
 procedure PopInputSource;
 begin
  if InputStackSize>0 then begin
   dec(InputStackSize);
  end;
 end;
 function NextChar:TPACCInt;
 var i:TPACCInt;
     InputStackItem:PInputStackItem;
 begin
  result:=0;
  i:=InputStackSize-1;
  while i>=0 do begin
   InputStackItem:=@InputStack[i];
   if InputStackItem^.BufferPosition<=InputStackItem^.BufferLength then begin
    if (InputStackItem^.SourceLocation.Source>=0) and (InputSources[InputStackItem^.SourceLocation.Source].Kind in [iskNONE,iskFILE]) then begin
     LastSourceLocation:=InputStackItem^.SourceLocation;
     SourceLocation:=LastSourceLocation;
    end;
    if InputStack[i].BufferSourceLocationIndex<length(InputStackItem^.BufferSourceLocations) then begin
     while (InputStack[i].BufferSourceLocationIndex<length(InputStackItem^.BufferSourceLocations)) and   
           ((InputStackItem^.BufferSourceLocations[InputStack[i].BufferSourceLocationIndex].StartPosition>InputStackItem^.BufferPosition) or
            (InputStackItem^.BufferSourceLocations[InputStack[i].BufferSourceLocationIndex].EndPosition<InputStackItem^.BufferPosition)) do begin
      inc(InputStack[i].BufferSourceLocationIndex);
     end;
     if InputStack[i].BufferSourceLocationIndex<length(InputStackItem^.BufferSourceLocations) then begin
      InputStackItem^.SourceLocation.Line:=InputStackItem^.BufferSourceLocations[InputStack[i].BufferSourceLocationIndex].Line;
      InputStackItem^.SourceLocation.Column:=InputStackItem^.BufferSourceLocations[InputStack[i].BufferSourceLocationIndex].Column;
     end;
    end;
    if ((InputStackItem^.BufferPosition+5)<=InputStackItem^.BufferLength) and
       ((InputStackItem^.Buffer[InputStackItem^.BufferPosition+0]=#$fd) and
        (InputStackItem^.Buffer[InputStackItem^.BufferPosition+1]=#$bf) and
        (InputStackItem^.Buffer[InputStackItem^.BufferPosition+2]=#$bf) and
        (InputStackItem^.Buffer[InputStackItem^.BufferPosition+3]=#$bf) and
        (InputStackItem^.Buffer[InputStackItem^.BufferPosition+4]=#$bf) and
        (InputStackItem^.Buffer[InputStackItem^.BufferPosition+5]=#$bf)) then begin
     result:=-1;
     inc(InputStackItem^.BufferPosition,6);
    end else begin
     result:=PUCUUTF8CodeUnitGetCharAndIncFallback(InputStackItem^.Buffer,InputStackItem^.BufferPosition);
     if (InputStack[i].BufferSourceLocationIndex>=length(InputStackItem^.BufferSourceLocations)) and (result=10) then begin
      inc(InputStackItem^.SourceLocation.Line);
      InputStackItem^.SourceLocation.Column:=0;
{    end else begin
      inc(InputStackItem^.SourceLocation.Column);}
     end;
    end;
    CurrentChar:=result;
    if InputStackItem^.BufferPosition>InputStackItem^.BufferLength then begin
     PopInputSource;
    end;
    break;
   end else begin
    PopInputSource;
    i:=InputStackSize-1;
   end;
  end;
 end;
 function GetCharAt(j:TPACCInt=0):TPACCInt;
 var i,p:TPACCInt;
     InputStackItem:PInputStackItem;
 begin
  result:=0;
  i:=InputStackSize-1;
  while (i>=0) and (j>=0) do begin
   InputStackItem:=@InputStack[i];
   p:=InputStackItem^.BufferPosition;
   while (p<=InputStackItem^.BufferLength) and (j>=0) do begin
    if ((p+5)<=InputStackItem^.BufferLength) and
       ((InputStackItem^.Buffer[p+0]=#$fd) and
        (InputStackItem^.Buffer[p+1]=#$bf) and
        (InputStackItem^.Buffer[p+2]=#$bf) and
        (InputStackItem^.Buffer[p+3]=#$bf) and
        (InputStackItem^.Buffer[p+4]=#$bf) and
        (InputStackItem^.Buffer[p+5]=#$bf)) then begin
     result:=-1;
     inc(p,6);
    end else begin
     result:=PUCUUTF8CodeUnitGetCharAndIncFallback(InputStackItem^.Buffer,p);
    end;
    dec(j);
   end;
   dec(i);
  end;
  if j>0 then begin
   result:=0;
  end;
 end;
 function GetToken:TToken;
 type PPreprocessorEncoding=^TPACCPreprocessorEncoding;
      TPACCPreprocessorEncoding=(PREPROCESSOR_ENCODING_NONE,PREPROCESSOR_ENCODING_UTF8,PREPROCESSOR_ENCODING_WCHAR,PREPROCESSOR_ENCODING_CHAR16,PREPROCESSOR_ENCODING_CHAR32);
  function ParseString(const IsChar,Passthrough:boolean;const Encoding:TPACCPreprocessorEncoding):TToken;
  var OutString:TPUCUUTF32String;
      OutStringLength,i,c,TerminateChar,BitsPerChar,BitShift:TPACCInt;
      v:uint64;
   procedure AddChar(NewChar:ansichar);
   begin
    if OutStringLength>=length(OutString) then begin
     SetLength(OutString,(OutStringLength+1)*2);
    end;
    OutString[OutStringLength]:=byte(ansichar(NewChar));
    inc(OutStringLength);
   end;
   procedure AddChar32(NewChar:longword);
   begin
    if OutStringLength>=length(OutString) then begin
     SetLength(OutString,(OutStringLength+1)*2);
    end;
    OutString[OutStringLength]:=byte(ansichar(NewChar));
    inc(OutStringLength);
   end;
  begin
   OutString:=nil;
   OutStringLength:=0;
   try
    TerminateChar:=CurrentChar;
    NextChar;
    while (CurrentChar<>TerminateChar) and not (CurrentChar in [0,10]) do begin
     if CurrentChar=ord('\') then begin
      NextChar;
      case CurrentChar of
       ord('a'):begin
        AddChar(#7);
        NextChar;
       end;
       ord('b'):begin
        AddChar(#8);
        NextChar;
       end;
       ord('t'):begin
        AddChar(#9);
        NextChar;
       end;
       ord('n'):begin
        AddChar(#10);
        NextChar;
       end;
       ord('v'):begin
        AddChar(#11);
        NextChar;
       end;
       ord('f'):begin
        AddChar(#12);
        NextChar;
       end;
       ord('r'):begin
        AddChar(#13);
        NextChar;
       end;
       ord('\'):begin
        AddChar('\');
        NextChar;
       end;
       ord(''''):begin
        AddChar('''');
        NextChar;
       end;
       ord('"'):begin
        AddChar('"');
        NextChar;
       end;
       ord('?'):begin
        AddChar('?');
        NextChar;
       end;
       ord('U'):begin
        if (GetCharAt(1) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
           (GetCharAt(2) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
           (GetCharAt(3) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
           (GetCharAt(4) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
           (GetCharAt(5) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
           (GetCharAt(6) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
           (GetCharAt(7) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
           (GetCharAt(8) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) then begin
         AddChar32((Hex2Byte(GetCharAt(1)) shl 28) or
                   (Hex2Byte(GetCharAt(2)) shl 24) or
                   (Hex2Byte(GetCharAt(3)) shl 20) or
                   (Hex2Byte(GetCharAt(4)) shl 16) or
                   (Hex2Byte(GetCharAt(5)) shl 12) or
                   (Hex2Byte(GetCharAt(6)) shl 8) or
                   (Hex2Byte(GetCharAt(7)) shl 4) or
                   (Hex2Byte(GetCharAt(8)) shl 0));
         NextChar;
         NextChar;
         NextChar;
         NextChar;
         NextChar;
         NextChar;
         NextChar;
         NextChar;
         NextChar;
        end else begin
         NextChar;
        end;
       end;
       ord('u'):begin
        if (GetCharAt(1) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
           (GetCharAt(2) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
           (GetCharAt(3) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
           (GetCharAt(4) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) then begin
         AddChar32((Hex2Byte(GetCharAt(1)) shl 12) or
                   (Hex2Byte(GetCharAt(2)) shl 8) or
                   (Hex2Byte(GetCharAt(3)) shl 4) or
                   (Hex2Byte(GetCharAt(4)) shl 0));
         NextChar;
         NextChar;
         NextChar;
         NextChar;
         NextChar;
        end else begin
         NextChar;
        end;
       end;
       ord('x'),ord('X'):begin
        if (GetCharAt(1) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) and
           (GetCharAt(2) in [ord('0')..ord('9'),ord('A')..ord('F'),ord('a')..ord('f')]) then begin
         AddChar32((Hex2Byte(GetCharAt(1)) shl 4) or
                   (Hex2Byte(GetCharAt(2)) shl 0));
         NextChar;
         NextChar;
         NextChar;
        end else begin
         NextChar;
        end;
       end;
       ord('0')..ord('7'):begin
        c:=0;
        while CurrentChar in [ord('0')..ord('7')] do begin
         c:=(c*8)+(CurrentChar-ord('0'));
         NextChar;
        end;
        AddChar32(c);
       end;
       else begin
        AddChar32(CurrentChar);
        NextChar;
       end;
      end;
     end else begin
      AddChar32(CurrentChar);
      NextChar;
     end;
    end;
    if CurrentChar=TerminateChar then begin
     NextChar;
    end;
    SetLength(OutString,OutStringLength);
    if IsChar then begin
     if OutStringLength<1 then begin
      AddError('Empty character constant',nil,false);
     end else if OutStringLength>1 then begin
      AddWarning('Multi-character character constant',nil);
     end;
     case Encoding of
      PREPROCESSOR_ENCODING_UTF8:begin
       BitsPerChar:=32;
      end;
      PREPROCESSOR_ENCODING_WCHAR:begin
       BitsPerChar:=16;
      end;
      PREPROCESSOR_ENCODING_CHAR16:begin
       BitsPerChar:=16;
      end;
      PREPROCESSOR_ENCODING_CHAR32:begin
       BitsPerChar:=32;
      end;
      else begin
       BitsPerChar:=8;
      end;
     end;
     v:=0;
     BitShift:=0;
     for i:=0 to OutStringLength-1 do begin
      v:=v or (OutString[i] shl BitShift);
      inc(BitShift,BitsPerChar);
     end;
     CurrentTokenValue:=v;
     CurrentTokenChar:=v;
     CurrentTokenString:=PUCUUTF32CharToUTF8(v);
     case Encoding of
      PREPROCESSOR_ENCODING_UTF8:begin
       result:=tCCHARUTF8;
      end;
      PREPROCESSOR_ENCODING_WCHAR:begin
       result:=tCCHARWCHAR;
      end;
      PREPROCESSOR_ENCODING_CHAR16:begin
       result:=tCCHARCHAR16;
      end;
      PREPROCESSOR_ENCODING_CHAR32:begin
       result:=tCCHARCHAR32;
      end;
      else begin
       result:=tCCHAR;
      end;
     end;
    end else begin
     CurrentTokenString:='';
     for i:=0 to OutStringLength-1 do begin
      CurrentTokenString:=CurrentTokenString+PUCUUTF32CharToUTF8(OutString[i]);
     end;
     case Encoding of
      PREPROCESSOR_ENCODING_UTF8:begin
       result:=tSTRINGUTF8;
      end;
      PREPROCESSOR_ENCODING_WCHAR:begin
       result:=tSTRINGWCHAR;
      end;
      PREPROCESSOR_ENCODING_CHAR16:begin
       result:=tSTRINGCHAR16;
      end;
      PREPROCESSOR_ENCODING_CHAR32:begin
       result:=tSTRINGCHAR32;
      end;
      else begin
       result:=tSTRING;
      end;
     end;
    end;
   finally
    SetLength(OutString,0);
   end;
  end;
 var lc,c,sc:TPUCUUTF32Char;
     cc,i,j:TPACCInt;
 begin
  c:=NextChar;
  case c of
   ord('/'):begin
    if MacroLevel=0 then begin
     c:=GetCharAt(0);
     case c of
      ord('/'):begin
       NextChar;
       c:=NextChar;
       while not (c in [0,10]) do begin
        c:=NextChar;
       end;
       result:=tCHAR;
       CurrentToken:=tCHAR;
       CurrentTokenChar:=c;
       CurrentTokenString:=PUCUUTF32CharToUTF8(c);
      end;
      ord('*'):begin
       NextChar;
       lc:=0;
       c:=NextChar;
       cc:=1;
       repeat
        {if (lc=ord('/')) and (c=ord('*')) then begin
         inc(cc);
         c:=0;
        end else} if (lc=ord('*')) and (c=ord('/')) then begin
         dec(cc);
         if cc=0 then begin
          break;
         end;
        end;
        lc:=c;
        c:=NextChar;
       until c=0;
       if cc>0 then begin
        AddError('Not closed /* */ comment',nil,true);
       end;
       c:=ord(' ');
       result:=tCHAR;
       CurrentToken:=tCHAR;
       CurrentTokenChar:=c;
       CurrentTokenString:=PUCUUTF32CharToUTF8(c);
      end;
      else begin
       c:=ord('/');
       result:=tCHAR;
       CurrentToken:=tCHAR;
       CurrentTokenChar:=c;
       CurrentTokenString:=PUCUUTF32CharToUTF8(c);
      end;
     end;
    end else begin
     c:=ord('/');
     result:=tCHAR;
     CurrentToken:=tCHAR;
     CurrentTokenChar:=c;
     CurrentTokenString:=PUCUUTF32CharToUTF8(c);
    end;
   end;
   ord('0')..ord('9'),ord('.'):begin
    case NumberMode of
     1:begin
      // Preprocessor-parse number
      CurrentTokenString:=PUCUUTF32CharToUTF8(c);
      i:=0;
      while GetCharAt(i) in [ord('0')..ord('9'),ord('.'),ord('+'),ord('-'),ord('A')..ord('Z'),ord('a')..ord('z')] do begin
       inc(i);
      end;
      while i>0 do begin
       CurrentTokenString:=CurrentTokenString+PUCUUTF32CharToUTF8(NextChar);
       dec(i);
      end;
      CurrentToken:=tNUMBER;
      result:=CurrentToken;
     end;
     2:begin
      // Eval-parse number
      CurrentTokenString:=PUCUUTF32CharToUTF8(c);
      i:=0;
      repeat
       case GetCharAt(i) of
        ord('0')..ord('9'),ord('.'),ord('+'),ord('-'),ord('A')..ord('Z'),ord('a')..ord('z'),ord('_'),$00000100..$7fffffff:begin
         inc(i);
        end;
        else begin
         break;
        end;
       end;
      until false;
      while i>0 do begin
       CurrentTokenString:=CurrentTokenString+PUCUUTF32CharToUTF8(NextChar);
       dec(i);
      end;
      CurrentToken:=tNUMBER;
      result:=CurrentToken;
     end;
     else begin
      // Passthrough number
      CurrentTokenString:=PUCUUTF32CharToUTF8(c);
      i:=0;
      repeat
       case GetCharAt(i) of
        ord('0')..ord('9'),ord('.'),ord('+'),ord('-'),ord('A')..ord('Z'),ord('a')..ord('z'),ord('_'),$00000100..$7fffffff:begin
         inc(i);
        end;
        else begin
         break;
        end;
       end;
      until false;
      while i>0 do begin
       CurrentTokenString:=CurrentTokenString+PUCUUTF32CharToUTF8(NextChar);
       dec(i);
      end;
      CurrentToken:=tNUMBER;
      result:=CurrentToken;
     end;
    end;
   end;
   ord('"'):begin
    case StringMode of
     1:begin
      // Preprocessor-parsing string literal
      CurrentTokenString:='"';
      i:=0;
      repeat
       c:=GetCharAt(i);
       if (c=ord('"')) or (c=0) then begin
        inc(i);
        break;
       end else if c=ord('\') then begin
        if (GetCharAt(i+1)=ord('{')) then begin
         inc(i,2);
         j:=0;
         repeat
          c:=GetCharAt(i);
          if c=ord('{') then begin
           inc(j);
          end else if c=ord('}') then begin
           if j=0 then begin
            break;
           end else begin
            dec(j);
           end;
          end else if c=0 then begin
           break;
          end else begin
           inc(i);
          end;
         until false;
        end else begin
         inc(i,2);
        end;
       end else begin
        inc(i);
       end;
      until false;
      while i>0 do begin
       CurrentTokenString:=CurrentTokenString+PUCUUTF32CharToUTF8(NextChar);
       dec(i);
      end;
      CurrentToken:=tSTRING;
      result:=CurrentToken;
     end;
     2:begin
      // Eval-parsing string literal
      CurrentToken:=ParseString(false,false,PREPROCESSOR_ENCODING_NONE);
      result:=CurrentToken;
     end;
     else begin
      // Passthrough string literal
      CurrentTokenString:='"';
      i:=0;
      repeat
       c:=GetCharAt(i);
       if (c=ord('"')) or (c=0) then begin
        inc(i);
        break;
       end else if c=ord('\') then begin
        if (GetCharAt(i+1)=ord('{')) then begin
         inc(i,2);
         j:=0;
         repeat
          c:=GetCharAt(i);
          if c=ord('{') then begin
           inc(j);
          end else if c=ord('}') then begin
           if j=0 then begin
            break;
           end else begin
            dec(j);
           end;
          end else if c=0 then begin
           break;
          end else begin
           inc(i);
          end;
         until false;
        end else begin
         inc(i,2);
        end;
       end else begin
        inc(i);
       end;
      until false;
      while i>0 do begin
       CurrentTokenString:=CurrentTokenString+PUCUUTF32CharToUTF8(NextChar);
       dec(i);
      end;
      CurrentToken:=tSTRING;
      result:=CurrentToken;
     end;
    end;
   end;
   ord(''''):begin
    case StringMode of
     1:begin
      // Preprocessor-parsing string literal
      CurrentTokenString:='''';
      i:=0;
      repeat
       c:=GetCharAt(i);
       if (c=ord('''')) and (GetCharAt(i+1)=ord('''')) then begin
        inc(i,2);
       end else if (c=ord('''')) or (c=0) then begin
        inc(i);
        break;
       end else begin
        inc(i);
       end;
      until false;
      while i>0 do begin
       CurrentTokenString:=CurrentTokenString+PUCUUTF32CharToUTF8(NextChar);
       dec(i);
      end;
      CurrentToken:=tSTRING;
      result:=CurrentToken;
     end;
     2:begin
      // Eval-parsing char literal
      CurrentToken:=ParseString(true,false,PREPROCESSOR_ENCODING_NONE);
      result:=CurrentToken;
     end;
     else begin
      // Passthrough char literal
      CurrentTokenString:='''';
      i:=0;
      repeat
       c:=GetCharAt(i);
       if (c=ord('''')) and (GetCharAt(i+1)=ord('''')) then begin
        inc(i,2);
       end else if (c=ord('''')) or (c=0) then begin
        inc(i);
        break;
       end else if c=ord('\') then begin
        inc(i,2);
       end else begin
        inc(i);
       end;
      until false;
      while i>0 do begin
       CurrentTokenString:=CurrentTokenString+PUCUUTF32CharToUTF8(NextChar);
       dec(i);
      end;
      CurrentToken:=tCCHAR;
      result:=CurrentToken;
     end;
    end;
   end;
   else begin
    if ((c in [ord('L'),ord('U'),ord('u')]) and (GetCharAt(0) in [ord(''''),ord('"')])) or
       ((c=ord('u')) and (GetCharAt(0)=ord('8')) and (GetCharAt(1) in [ord(''''),ord('"')])) then begin
     if StringMode=2 then begin
      // Eval-parsing string/char literal
      case c of
       ord('L'):begin
        c:=NextChar;
        CurrentToken:=ParseString(c=ord(''''),false,PREPROCESSOR_ENCODING_WCHAR);
        result:=CurrentToken;
       end;
       ord('U'):begin
        c:=NextChar;
        CurrentToken:=ParseString(c=ord(''''),false,PREPROCESSOR_ENCODING_CHAR32);
        result:=CurrentToken;
       end;
       ord('u'):begin
        if GetCharAt(0)=ord('8') then begin
         NextChar;
         c:=NextChar;
         CurrentToken:=ParseString(c=ord(''''),false,PREPROCESSOR_ENCODING_UTF8);
         result:=CurrentToken;
        end else begin
         c:=NextChar;
         CurrentToken:=ParseString(c=ord(''''),false,PREPROCESSOR_ENCODING_CHAR16);
         result:=CurrentToken;
        end;
       end;
       else begin
        c:=NextChar;
        CurrentToken:=ParseString(c=ord(''''),false,PREPROCESSOR_ENCODING_UTF8);
        result:=CurrentToken;
       end;
      end;
     end else begin
      // Passthrough string/char literal
      case c of
       ord('L'):begin
        c:=NextChar;
        if c=ord('''') then begin
         result:=tCCHARWCHAR;
        end else begin
         result:=tSTRINGWCHAR;
        end;
       end;
       ord('U'):begin
        c:=NextChar;
        if c=ord('''') then begin
         result:=tCCHARCHAR32;
        end else begin
         result:=tSTRINGCHAR32;
        end;
       end;
       ord('u'):begin
        if GetCharAt(0)=ord('8') then begin
         NextChar;
         c:=NextChar;
         if c=ord('''') then begin
          result:=tCCHARUTF8;
         end else begin
          result:=tSTRINGUTF8;
         end;
        end else begin
         c:=NextChar;
         if c=ord('''') then begin
          result:=tCCHARCHAR16
         end else begin
          result:=tSTRINGCHAR16;
         end;
        end;
       end;
       else begin
        c:=NextChar;
        if c=ord('''') then begin
         result:=tCCHARUTF8;
        end else begin
         result:=tSTRINGUTF8;
        end;
       end;
      end;
      sc:=c;
      CurrentTokenString:=PUCUUTF32CharToUTF8(sc);
      i:=0;
      repeat
       c:=GetCharAt(i);
       if (c=sc) or (c=0) then begin
        inc(i);
        break;
       end else if c=10 then begin
        break;
       end else if c=ord('\') then begin
        inc(i,2);
       end else begin
        inc(i);
       end;
      until false;
      while i>0 do begin
       CurrentTokenString:=CurrentTokenString+PUCUUTF32CharToUTF8(NextChar);
       dec(i);
      end;
      CurrentToken:=result;
     end;
    end else if (c in [ord('A')..ord('Z'),ord('a')..ord('z'),ord('_')]) or (c>255) then begin
     result:=tNAME;
     CurrentToken:=result;
     CurrentTokenString:=PUCUUTF32CharToUTF8(c);
     i:=0;
     c:=GetCharAt(i);
     while (c in [ord('A')..ord('Z'),ord('a')..ord('z'),ord('_'),ord('0')..ord('9')]) or (c>255) do begin
      inc(i);
      c:=GetCharAt(i);
     end;
     while i>0 do begin
      CurrentTokenString:=CurrentTokenString+PUCUUTF32CharToUTF8(NextChar);
      dec(i);
     end;
    end else begin
     result:=tCHAR;
     CurrentToken:=tCHAR;
     CurrentTokenChar:=c;
     CurrentTokenString:=PUCUUTF32CharToUTF8(c);
    end;
   end;
  end;
 end;
 procedure SkipGetToken; forward;
 procedure SkipExtGetToken; forward;
 procedure SkipBlankEx; forward;
 procedure SkipBlank; forward;
 function Destringize(const s:TPUCUUTF8String):TPUCUUTF8String;
 var i:TPACCInt;
     c,sc,nc:TPUCUUTF32Char;
 begin
  result:='';
  i:=1;
  while i<=length(s) do begin
   sc:=PUCUUTF8CodeUnitGetCharAndIncFallback(s,i);
   case sc of
    ord('\'):begin
     if i<=length(s) then begin
      nc:=PUCUUTF8CodeUnitGetCharAndIncFallback(s,i);
      case nc of
       ord('a'):begin
        result:=result+#7;
       end;
       ord('b'):begin
        result:=result+#8;
       end;
       ord('t'):begin
        result:=result+#9;
       end;
       ord('n'):begin
        result:=result+#10;
       end;
       ord('v'):begin
        result:=result+#11;
       end;
       ord('f'):begin
        result:=result+#12;
       end;
       ord('r'):begin
        result:=result+#13;
       end;
       ord('\'):begin
        result:=result+'\';
       end;
       ord(''''):begin
        result:=result+'''';
       end;
       ord('"'):begin
        result:=result+'"';
       end;
       ord('?'):begin
        result:=result+'?';
       end;
       ord('U'):begin
        if ((i+7)<=length(s)) and
           (s[i+0] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+1] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+2] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+3] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+4] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+5] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+6] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+7] in ['0'..'9','A'..'F','a'..'f']) then begin
         c:=(hex2byte(ord(s[i+0])) shl 28) or (hex2byte(ord(s[i+1])) shl 24) or (hex2byte(ord(s[i+2])) shl 20) or (hex2byte(ord(s[i+3])) shl 16) or (hex2byte(ord(s[i+4])) shl 12) or (hex2byte(ord(s[i+5])) shl 8) or (hex2byte(ord(s[i+6])) shl 4) or (hex2byte(ord(s[i+7])) shl 0);
         inc(i,8);
         result:=result+PUCUUTF32CharToUTF8(c);
        end else begin
         result:=result+'\U';
        end;
       end;
       ord('u'):begin
        if ((i+4)<=length(s)) and
           (s[i+0] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+1] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+2] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+3] in ['0'..'9','A'..'F','a'..'f']) then begin
         c:=(hex2byte(ord(s[i+0])) shl 12) or (hex2byte(ord(s[i+1])) shl 8) or (hex2byte(ord(s[i+2])) shl 4) or (hex2byte(ord(s[i+3])) shl 0);
         inc(i,4);
         result:=result+PUCUUTF32CharToUTF8(c);
        end else begin
         result:=result+'\u';
        end;
       end;
       ord('x'),ord('X'):begin
        if ((i+2)<length(s)) and
           (s[i+0] in ['0'..'9','A'..'F','a'..'f']) and
           (s[i+1] in ['0'..'9','A'..'F','a'..'f']) then begin
         c:=(hex2byte(ord(s[i+0])) shl 4) or (hex2byte(ord(s[i+1])) shl 0);
         inc(i,2);
         result:=result+PUCUUTF32CharToUTF8(c);
        end else begin
         result:=result+('\'+PUCUUTF32CharToUTF8(nc));
        end;
       end;
       ord('0')..ord('7'):begin
        c:=nc-ord('0');
        while (i<=length(s)) and (s[i] in ['0'..'7']) do begin
         c:=(c*8)+(ord(s[i])-ord('0'));
         inc(i);
        end;
        result:=result+PUCUUTF32CharToUTF8(c);
       end;
       else begin
        result:=result+PUCUUTF32CharToUTF8(nc);
       end;
      end;
     end;
    end;
    else begin
     result:=result+PUCUUTF32CharToUTF8(sc);
    end;
   end;
  end;
 end;
 function Stringize(const s:TPUCUUTF8String;Quote,CharQuote:boolean):TPUCUUTF8String;
 const hexchars:array[0..$f] of ansichar='0123456789ABCDEF';
 var i:TPACCInt;
     c:TPUCUUTF32Char;
 begin
  result:='';
  if Quote or CharQuote then begin
   i:=1;
   while i<=length(s) do begin
    c:=PUCUUTF8CodeUnitGetCharAndIncFallback(s,i);
    case c of
     0:begin
      result:=result+'\0';
     end;
     7:begin
      result:=result+'\a';
     end;
     8:begin
      result:=result+'\b';
     end;
     9:begin
      result:=result+'\t';
     end;
     10:begin
      result:=result+'\n';
     end;
     11:begin
      result:=result+'\v';
     end;
     12:begin
      result:=result+'\f';
     end;
     13:begin
      result:=result+'\r';
     end;
     ord('\'):begin
      result:=result+'\\';
     end;
     ord(''''):begin
      result:=result+'\''';
     end;
     ord('"'):begin
      result:=result+'\"';
     end;
     ord('?'):begin
      result:=result+'\?';
     end;
     else begin
      if c<128 then begin
       result:=result+PUCUUTF32CharToUTF8(c);
      end else if c<=$ffff then begin
       result:=result+'\u'+hexchars[(c shr 12) and $f]+hexchars[(c shr 8) and $f]+hexchars[(c shr 4) and $f]+hexchars[c and $f];
      end else begin
       result:=result+'\U'+hexchars[(c shr 28) and $f]+hexchars[(c shr 24) and $f]+hexchars[(c shr 20) and $f]+hexchars[(c shr 16) and $f]+hexchars[(c shr 12) and $f]+hexchars[(c shr 8) and $f]+hexchars[(c shr 4) and $f]+hexchars[c and $f];
      end;
     end;
    end;
    if CharQuote then begin
     exit;
    end;
   end;
  end else begin
   result:=s;
  end;
 end;
 procedure ExtGetToken;
 var mi,i,j,k,NestedLevel,CountParameters,OldStringMode,OldNumberMode:TPACCInt;
     b:TPUCUUTF8String;
     Parameters:array of TPUCUUTF8String;
     InputStackItem:TInputStackItem;
     DateTime:TDateTime;
 begin
  b:='';
  Parameters:=nil;
  try
   SetLength(Parameters,16);
   repeat
    HideSet.Clear;
    if InputStackSize>0 then begin
     InputStackItem:=InputStack[InputStackSize-1];
     HideSet.AddStrings(InputStackItem.HideSet);
    end else begin
     InputStackItem.SourceLocation.Source:=-1;
    end;
    if GetToken<>tNAME then begin
     break;
    end;
    mi:=LookUp(CurrentTokenString);
    if mi<0 then begin
     break;
    end;
    if Macros[mi].MacroFunction<>mfNONE then begin
     case Macros[mi].MacroFunction of
      mfFILE:begin
       if InputStackItem.SourceLocation.Source>=0 then begin
        HideSet.Add(Macros[mi].Name);
        PushInputSource(iskMACRO,Macros[mi].Name,'"'+Stringize(InputSources[InputStackItem.SourceLocation.Source].Name,true,false)+'"',nil);
       end else begin
        AddError(Macros[mi].Name+': Unknown file',nil,true);
        continue;
       end;
      end;
      mfLINE:begin
       if InputStackItem.SourceLocation.Source>=0 then begin
        HideSet.Add(Macros[mi].Name);
        PushInputSource(iskMACRO,Macros[mi].Name,IntToStr(InputStackItem.SourceLocation.Line+1),nil);
       end else begin
        AddError(Macros[mi].Name+': Unknown line number',nil,true);
        continue;
       end;
      end;
      mfTIME:begin
       HideSet.Add(Macros[mi].Name);
       PushInputSource(iskMACRO,Macros[mi].Name,'"'+FormatDateTime('hh:nn:ss',Now)+'"',nil);
      end;
      mfDATE:begin
       HideSet.Add(Macros[mi].Name);
       PushInputSource(iskMACRO,Macros[mi].Name,'"'+FormatDateTime('mmm dd yyyy',Now)+'"',nil);
      end;
      mfPRAGMA:begin
       OldStringMode:=StringMode;
       OldNumberMode:=NumberMode;
       StringMode:=1;
       NumberMode:=1;
       SkipGetToken;
       if not ((CurrentToken=tCHAR) and (CurrentTokenChar=ord('('))) then begin
        AddError(Macros[mi].Name+': missing (',nil,true);
        StringMode:=OldStringMode;
        NumberMode:=OldNumberMode;
        exit;
       end;
       SkipGetToken;
       if CurrentToken<>tSTRING then begin
        AddError(Macros[mi].Name+': string was expected',nil,true);
        StringMode:=OldStringMode;
        NumberMode:=OldNumberMode;
        exit;
       end;
       i:=CountPragmaInfos;
       inc(CountPragmaInfos);
       if (i+1)>length(PragmaInfos) then begin
        SetLength(PragmaInfos,RoundUpToPowerOfTwo(i+2));
       end;
       PragmaInfos[i].CharPos:=OutputTextLength+1;
       if length(CurrentTokenString)<2 then begin
        AddError(Macros[mi].Name+': string not closed',nil,true);
        StringMode:=OldStringMode;
        NumberMode:=OldNumberMode;
        exit;
       end;
       if CurrentTokenString[1]<>CurrentTokenString[length(CurrentTokenString)] then begin
        AddError(Macros[mi].Name+': invalid string',nil,true);
        StringMode:=OldStringMode;
        NumberMode:=OldNumberMode;
        exit;
       end;
       PragmaInfos[i].Pragma:=Destringize(copy(CurrentTokenString,2,length(CurrentTokenString)-2));
       SkipGetToken;
       if not ((CurrentToken=tCHAR) and (CurrentTokenChar=ord(')'))) then begin
        AddError(Macros[mi].Name+': missing )',nil,true);
        StringMode:=OldStringMode;
        NumberMode:=OldNumberMode;
        exit;
       end;
       StringMode:=OldStringMode;
       NumberMode:=OldNumberMode;
      end;
      mfBASEFILE:begin
       HideSet.Add(Macros[mi].Name);
       PushInputSource(iskMACRO,Macros[mi].Name,'"'+Stringize(InputName,true,false)+'"',nil);
      end;
      mfCOUNTER:begin
       HideSet.Add(Macros[mi].Name);
       PushInputSource(iskMACRO,Macros[mi].Name,IntToStr(CounterValue),nil);
       inc(CounterValue);
      end;
      mfINCLUDELEVEL:begin
       HideSet.Add(Macros[mi].Name);
       PushInputSource(iskMACRO,Macros[mi].Name,IntToStr(InputStackSize),nil);
      end;
      mfTIMESTAMP:begin
       DateTime:=Now;
       if InputStackSize>0 then begin
        i:=InputStack[InputStackSize-1].SourceLocation.Source;
        if (i>=0) and (InputSources[i].Kind=iskFILE) then begin
         try
          DateTime:=FileDateToDateTime(FileAge(InputSources[i].Name));
         except
          DateTime:=Now;
         end;
        end;
       end;
       HideSet.Add(Macros[mi].Name);
       PushInputSource(iskMACRO,Macros[mi].Name,'"'+FormatDateTime('ddd mmm dd hh:nn:ss yyyy',DateTime)+'"',nil);
      end;
      else begin
       AddError(Macros[mi].Name+': Unknown macro function',nil,true);
       continue;
      end;
     end;
    end else begin
     if HideSet.IndexOf(Macros[mi].Name)>=0 then begin
      break;
     end else begin
      if Macros[mi].FunctionLike then begin
       i:=0;
       while GetCharAt(i) in UCS4WhiteSpace do begin
        inc(i);
       end;
       if GetCharAt(i)=ord('(') then begin
        SkipGetToken;
        if not ((CurrentToken=tCHAR) and (CurrentTokenChar=ord('('))) then begin
         AddError(Macros[mi].Name+': missing (',nil,true);
         exit;
        end;
        HideSet.Add(Macros[mi].Name);
        SkipGetToken;
        CountParameters:=0;
        while not ((CurrentToken=tCHAR) and (CurrentTokenChar in [0,ord(')')])) do begin
         while (CurrentToken=tCHAR) and (CurrentTokenChar in UCS4WhiteSpaceEx) do begin
          GetToken;
         end;
         SetLength(b,0);
         NestedLevel:=0;
         repeat
          if (CurrentToken=tCHAR) and (CurrentTokenChar=0) then begin
           break;
          end;
          if NestedLevel=0 then begin
           if (CurrentToken=tCHAR) and (CurrentTokenChar in UCS4WhiteSpace) then begin
            i:=0;
            while GetCharAt(i) in UCS4WhiteSpace do begin
             inc(i);
            end;
            if GetCharAt(i) in [ord(','),ord(')')] then begin
             SkipExtGetToken;
            end;
           end;
           if (CurrentToken=tCHAR) and (CurrentTokenChar in [ord(','),ord(')')]) then begin
            break;
           end;
          end;
          if (CurrentToken=tCHAR) and (CurrentTokenChar=ord('(')) then begin
           b:=b+'(';
           inc(NestedLevel);
          end else if (CurrentToken=tCHAR) and (CurrentTokenChar=ord(')')) then begin
           b:=b+')';
           dec(NestedLevel);
          end else begin
           b:=b+CurrentTokenString;
          end;
          ExtGetToken;
         until false;
         if (CurrentToken=tCHAR) and (CurrentTokenChar=ord(',')) then begin
          SkipGetToken;
         end;
         if CountParameters>=length(Parameters) then begin
          SetLength(Parameters,(CountParameters+1)*2);
         end;
         Parameters[CountParameters]:=b;
         inc(CountParameters);
        end;
        if not ((CurrentToken=tCHAR) and (CurrentTokenChar=ord(')'))) then begin
         AddError(Macros[mi].Name+': missing )',nil,true);
         continue;
        end;
        if (CountParameters<Macros[mi].Parameters) or ((CountParameters>Macros[mi].Parameters) and not Macros[mi].VaArgs) then begin
         AddError(Macros[mi].Name+': Wrong amount of parameters',nil,true);
         continue;
        end;
        k:=1;
        b:='';
        for i:=0 to length(Macros[mi].Body)-1 do begin
         if Macros[mi].Body[i].CharQuote then begin
          b:=b+'''';
         end else if Macros[mi].Body[i].Quote then begin
          b:=b+'"';
         end;
         case Macros[mi].Body[i].Kind of
          mbikTEXT:begin
           if (i>1) and (Macros[mi].Body[i].Kind<>mbikTEXT) then begin
            k:=length(b);
           end;
           b:=b+Stringize(Macros[mi].Body[i].Text,Macros[mi].Body[i].Quote,Macros[mi].Body[i].CharQuote);
          end;
          mbikPARAMETER:begin
           if Macros[mi].Body[i].Value<CountParameters then begin
            b:=b+Stringize(Parameters[Macros[mi].Body[i].Value],Macros[mi].Body[i].Quote,Macros[mi].Body[i].CharQuote);
            k:=length(b)+1;
           end;
          end;
          mbikVAARGS:begin
           if CountParameters<=Macros[mi].Parameters then begin
            for j:=length(b) downto k+1 do begin
             if (ord(b[j]) in UCS4WhiteSpaceEx) then begin
             end else begin
              case b[j] of
               ',':begin
                b:=copy(b,1,j-1);
                break;
               end;
               else begin
                break;
               end;
              end;
             end;
            end;
           end else begin
            for j:=Macros[mi].Parameters to CountParameters-1 do begin
             b:=b+Stringize(Parameters[j],Macros[mi].Body[i].Quote,Macros[mi].Body[i].CharQuote);
             if (j+1)<CountParameters then begin
              b:=b+',';
             end;
            end;
            k:=length(b)+1;
           end;
          end;
          mbikSPLITTER:begin
           b:=b+#$fd#$bf#$bf#$bf#$bf#$bf;
          end;
         end;
         if Macros[mi].Body[i].CharQuote then begin
          b:=b+'''';
         end else if Macros[mi].Body[i].Quote then begin
          b:=b+'"';
         end;
        end;    
        inc(MacroLevel);
        PushInputSource(iskMACRO,Macros[mi].Name,b,nil);
        dec(MacroLevel);
       end else begin
        break;
       end;
      end else begin
       b:='';
       for i:=0 to length(Macros[mi].Body)-1 do begin
        case Macros[mi].Body[i].Kind of
         mbikTEXT:begin
          b:=b+Stringize(Macros[mi].Body[i].Text,Macros[mi].Body[i].Quote,Macros[mi].Body[i].CharQuote);
         end;
        end;
       end;
       inc(MacroLevel);
       HideSet.Add(Macros[mi].Name);
       PushInputSource(iskMACRO,Macros[mi].Name,b,nil);
       dec(MacroLevel);
      end;
     end;
    end;
   until false;
  finally
   SetLength(Parameters,0);
  end;
 end;
 procedure SkipGetToken;
 begin
  repeat
   GetToken;
   if (CurrentToken=tCHAR) and (CurrentTokenChar in UCS4WhiteSpace) then begin
    continue;
   end;
   break;
  until false;
 end;
 procedure SkipEOL;
 var i:TPACCInt;
 begin
  i:=0;
  while not (GetCharAt(i) in [0,10]) do begin
   inc(i);
  end;
  while i>0 do begin
   NextChar;
   dec(i);
  end;
 end;
 procedure SkipBlankEx;
 var i:TPACCInt;
 begin
  i:=0;
  while GetCharAt(i) in UCS4WhiteSpaceEx do begin
   inc(i);
  end;
  while i>0 do begin
   NextChar;
   dec(i);
  end;
 end;
 procedure SkipBlank;
 var i:TPACCInt;
 begin
  i:=0;
  while GetCharAt(i) in UCS4WhiteSpace do begin
   inc(i);
  end;
  while i>0 do begin
   NextChar;
   dec(i);
  end;
 end;
 procedure SkipExtGetToken;
 begin
  repeat
   ExtGetToken;
   if (CurrentToken=tCHAR) and (CurrentTokenChar in UCS4WhiteSpace) then begin
    continue;
   end;
   break;
  until false;
 end;
 procedure AddToOutput(const s:TPUCUUTF8String);
 var l,i:TPACCInt;
 begin
  l:=length(s);
  if l>0 then begin
   if CountOutputInfos>0 then begin
    if OutputInfos[CountOutputInfos-1].LastCharPos<>OutputTextLength then begin
     OutputInfos[CountOutputInfos-1].LastCharPos:=OutputTextLength;
    end;
    if (OutputInfos[CountOutputInfos-1].SourceLocation.Source<>SourceLocation.Source) or
       (OutputInfos[CountOutputInfos-1].SourceLocation.Line<>SourceLocation.Line) then begin
     i:=CountOutputInfos;
     inc(CountOutputInfos);
     if CountOutputInfos>length(OutputInfos) then begin
      SetLength(OutputInfos,CountOutputInfos*2);
     end;
     OutputInfos[i].FirstCharPos:=OutputTextLength+1;
     OutputInfos[i].LastCharPos:=OutputTextLength+1;
     OutputInfos[i].NewLineSincePos:=OutputTextLength+1;
     OutputInfos[i].SourceLocation:=LastSourceLocation;
    end;
   end else begin
    i:=CountOutputInfos;
    inc(CountOutputInfos);
    if CountOutputInfos>length(OutputInfos) then begin
     SetLength(OutputInfos,CountOutputInfos*2);
    end;
    OutputInfos[i].FirstCharPos:=OutputTextLength+1;
    OutputInfos[i].LastCharPos:=OutputTextLength+1;
    OutputInfos[i].NewLineSincePos:=OutputTextLength+1;
    OutputInfos[i].SourceLocation:=LastSourceLocation;
   end;
   begin
    i:=OutputTextLength+l;
    if length(OutputText)<=i then begin
     SetLength(OutputText,RoundUpToPowerOfTwo(i+1));
    end;
    Move(s[1],OutputText[OutputTextLength+1],l);
    OutputTextLength:=i;
   end;
  end;
 end;
 function ParseNumber(s:TPUCUUTF8String):TPACCInt;
 var i:TPACCInt;
     sv:TPACCInt;
     b:boolean;
 begin
  b:=false;
  while (length(s)>0) and (s[length(s)] in ['l','L','u','U']) do begin
   Delete(s,length(s),1);
  end;
  if (length(s)>0) and (s[1]='0') then begin
   if (length(s)>1) and (s[2] in ['x','X']) then begin
    result:=0;
    for i:=3 to length(s) do begin
     if (not b) and (((result*16) div 16)<>result) then begin
      b:=true;
      AddWarning('TERM: constant too large for destination type',nil);
     end;
     case s[i] of
      '0'..'9':begin
       sv:=ord(s[i])-ord('0');
      end;
      'a'..'f':begin
       sv:=(ord(s[i])-ord('a'))+$a;
      end;
      'A'..'F':begin
       sv:=(ord(s[i])-ord('A'))+$a;
      end;
      else begin
       sv:=0;
      end;
     end;
     result:=(result*16)+sv;
    end;
   end else begin
    result:=0;
    for i:=2 to length(s) do begin
     if (not b) and (((result*8) div 8)<>result) then begin
      b:=true;
      AddWarning('TERM: constant too large for destination type',nil);
     end;
     result:=(result*8)+(ord(s[i])-ord('0'));
    end;
   end;
  end else begin
   result:=0;
   for i:=1 to length(s) do begin
    if (not b) and (((result*10) div 10)<>result) then begin
     b:=true;
     AddWarning('TERM: constant too large for destination type');
    end;
    result:=(result*10)+(ord(s[i])-ord('0'));
   end;
  end;
 end;
 procedure DoINCLUDE(DoIncludeNext,DoIncludeOnce:boolean);
 var cfn,fn,ffn,d,t2:TPUCUUTF8String;
     c,m:TPUCUUTF32Char;
     OK:boolean;
     t:TPUCUUTF8String;
     i,OldStringMode,OldNumberMode:TPACCInt;
     fs:TStream;
     TextSourceLocations:TPACCPreprocessorInputSourceLocations;
 begin
  TextSourceLocations:=nil;
  try
   OldStringMode:=StringMode;
   OldNumberMode:=NumberMode;
   StringMode:=1;
   NumberMode:=1;
   cfn:='';
   if DoIncludeNext then begin
    AddWarning(PreprocessorChar+'include_next is a GCC extension!',nil);
    if (LastSourceLocation.Source>=0) and (LastSourceLocation.Source<CountInputSources) then begin
     cfn:=copy(InputSources[LastSourceLocation.Source].Name,0,length(InputSources[LastSourceLocation.Source].Name));
    end;
   end;
   d:='';
   t2:='';
   fn:='';
   ffn:='';
   SkipExtGetToken;
   if (CurrentToken=tCHAR) and (CurrentTokenChar=ord('<')) then begin
    OK:=false;
    repeat
     c:=NextChar;
     if c=ord('>') then begin
      OK:=true;
      break;
     end else if c in [0,10] then begin
      break;
     end else begin
      fn:=fn+PUCUUTF32CharToUTF8(c);
     end;
    until false;
    if not OK then begin
     AddError(PreprocessorChar+'include: missing >',nil,true);
     StringMode:=OldStringMode;
     NumberMode:=OldNumberMode;
     exit;
    end;
    m:=ord('<');
   end else if (CurrentToken=tSTRING) and (length(CurrentTokenString)>1) and (CurrentTokenString[1]='"') then begin
    if CurrentTokenString[length(CurrentTokenString)]<>'"' then begin
     AddError(PreprocessorChar+'include: missing "',nil,true);
     StringMode:=OldStringMode;
     NumberMode:=OldNumberMode;
     exit;
    end;
    fn:=copy(CurrentTokenString,2,length(CurrentTokenString)-2);
    m:=ord('"');
   end else begin
    AddError(PreprocessorChar+'include: missing filename',nil,true);
    StringMode:=OldStringMode;
    NumberMode:=OldNumberMode;
    exit;
   end;
   ffn:='';
   if (m=ord('"')) and (CountInputSources>0) and FileExists(ExtractFilePath(InputSources[0].Name)+fn) then begin
    ffn:=ExtractFilePath(InputSources[0].Name)+fn;
    if ffn=cfn then begin
     ffn:='';
    end;
   end;
   if (length(ffn)=0) or not FileExists(ffn) then begin
    for i:=0 to IncludeDirectories.Count-1 do begin
     d:=IncludeDirectories[i];
     if FileExists(d+fn) then begin
      ffn:=d+fn;
      if ffn=cfn then begin
       ffn:='';
      end else begin
       break;
      end;
     end;
    end;
   end;
   if (length(ffn)=0) or not FileExists(ffn) then begin
    if FileExists(fn) then begin
     ffn:=fn;
     if ffn=cfn then begin
      ffn:='';
     end;
    end;
   end;
   if (length(ffn)=0) or not FileExists(ffn) then begin
    if m=ord('"') then begin
     AddError(PreprocessorChar+'include: File "'+fn+'" not found',nil,true);
    end else begin
     AddError(PreprocessorChar+'include: File <'+fn+'> not found',nil,true);
    end;
    StringMode:=OldStringMode;
    NumberMode:=OldNumberMode;
    exit;
   end;
   try
    fs:=TFileStream.Create(ffn,fmOpenRead or fmShareDenyNone);
    try
     if assigned(fs) then begin
      SetLength(t,fs.Size);
      if length(t)>0 then begin
       if fs.Read(t[1],length(t))<>length(t) then begin
        StringMode:=OldStringMode;
        NumberMode:=OldNumberMode;
        raise Exception.Create('');
       end;
       t2:=PreprocessInputSourceChars(iskFILE,fn,t,TextSourceLocations);
      end;
     end else begin
      StringMode:=OldStringMode;
      NumberMode:=OldNumberMode;
      raise Exception.Create('');
     end;
    finally
     fs.Free;
    end;
    if DoIncludeOnce then begin
     for i:=0 to CountInputSources-1 do begin
      if (InputSources[i].Kind=iskFILE) and (InputSources[i].Name=fn) then begin
       exit;
      end;
     end;
    end;
    PushInputSource(iskFILE,fn,t2,TextSourceLocations);
    StringMode:=OldStringMode;
    NumberMode:=OldNumberMode;
   except
    if m=ord('"') then begin
     AddError(PreprocessorChar+'include: File "'+fn+'" I/O error',nil,true);
    end else begin
     AddError(PreprocessorChar+'include: File <'+fn+'> I/O error',nil,true);
    end;
   end;
  finally
   TextSourceLocations:=nil;
  end;
 end;
 procedure DoDEFINE;
 var Name:TPUCUUTF8String;
     Parameters:array of TPUCUUTF8String;
     Body:TMacroBody;
     Quote,CharQuote:boolean;
     i,j,b,CountParameters,BodySize:TPACCInt;
     FunctionLike,VaArgs:boolean;
 begin
  Name:='';
  SkipGetToken;
  if CurrentToken<>tNAME then begin
   AddWarning(PreprocessorChar+'define: missing macro name',nil);
   SkipEOL;
   exit;
  end;
  FunctionLike:=false;
  VaArgs:=false;
  Name:=CurrentTokenString;
  GetToken;
  if (CurrentToken=tCHAR) and (CurrentTokenChar=ord('(')) then begin
   FunctionLike:=true;
   Parameters:=nil;
   SetLength(Parameters,16);
   CountParameters:=0;
   repeat
    SkipGetToken;
    if (CurrentToken=tCHAR) and (CurrentTokenChar=ord('.')) and (GetCharAt(0)=ord('.')) and (GetCharAt(1)=ord('.')) then begin
     GetToken;
     GetToken;
     SkipGetToken;
     VaArgs:=true;
     break;
    end;
    if CurrentToken<>tNAME then begin
     break;
    end;
    if CountParameters>=length(Parameters) then begin
     SetLength(Parameters,(CountParameters+1)*2);
    end;
    Parameters[CountParameters]:=copy(CurrentTokenString,0,length(CurrentTokenString));
    inc(CountParameters);
    SkipGetToken;
    if not ((CurrentToken=tCHAR) and (CurrentTokenChar=ord(','))) then begin
     break;
    end;
   until false;
   SetLength(Parameters,CountParameters);
   if not ((CurrentToken=tCHAR) and (CurrentTokenChar=ord(')'))) then begin
    AddWarning(PreprocessorChar+'define: bad macro parameter',nil);
    SkipEOL;
    exit;
   end;
   CurrentToken:=tCHAR;
   CurrentTokenChar:=ord(' ');
  end;
  while (CurrentToken=tCHAR) and (CurrentTokenChar in UCS4WhiteSpace) do begin
   GetToken;
  end;
  Body:=nil;
  SetLength(Body,16);
  BodySize:=0;
  Quote:=false;
  CharQuote:=false;
  while not ((CurrentToken=tCHAR) and (CurrentTokenChar in [0,10])) do begin
{  if (CurrentToken=tCHAR) and (CurrentTokenChar=ord('\')) and (GetCharAt(0) in [0,10]) then begin
    GetToken;
    GetToken;
    b:=BodySize;
    inc(BodySize);
    if BodySize>length(Body) then begin
     SetLength(Body,BodySize*2);
    end;
    Body[b].Kind:=mbikTEXT;
    Body[b].Text:=#10;
    Body[b].Quote:=false;
    Body[b].CharQuote:=false;
    continue;
   end else}if ((CurrentToken=tCHAR) and (CurrentTokenChar=ord(PreprocessorChar))) or
               ((CurrentToken=tCHAR) and (CurrentTokenChar=ord('%')) and (GetCharAt(0)=ord(':'))) then begin
    if (CurrentToken=tCHAR) and (CurrentTokenChar=ord('%')) and (GetCharAt(0)=ord(':')) then begin
     NextChar;
    end;
    b:=BodySize;
    inc(BodySize);
    if BodySize>length(Body) then begin
     SetLength(Body,BodySize*2);
    end;
    Body[b].Kind:=mbikSPLITTER;
    Body[b].Quote:=false;
    Body[b].CharQuote:=false;
    GetToken;
    if (CurrentToken=tCHAR) and (CurrentTokenChar=ord('@')) then begin
     GetToken;
     CharQuote:=true;
    end else if ((CurrentToken=tCHAR) and (CurrentTokenChar=ord(PreprocessorChar))) or
                ((CurrentToken=tCHAR) and (CurrentTokenChar=ord('%')) and (GetCharAt(0)=ord(':'))) then begin
     if (CurrentToken=tCHAR) and (CurrentTokenChar=ord('%')) and (GetCharAt(0)=ord(':')) then begin
      NextChar;
     end;
     GetToken;
     if Quote or CharQuote then begin
      Body[b].Kind:=mbikTEXT;
      Body[b].Text:='##';
      Body[b].Quote:=Quote;
      Body[b].CharQuote:=CharQuote;
     end else begin
      if length(Body)>0 then begin
       if Body[length(Body)-1].Kind=mbikTEXT then begin
        while (length(Body[length(Body)-1].Text)>0) and (ord(Body[length(Body)-1].Text[length(Body[length(Body)-1].Text)]) in UCS4WhiteSpaceEx) do begin
         Delete(Body[length(Body)-1].Text,length(Body[length(Body)-1].Text),1);
        end;
       end else if length(Body)>1 then begin
        if Body[length(Body)-1].Kind=mbikSPLITTER then begin
         if Body[length(Body)-2].Kind=mbikTEXT then begin
          while (length(Body[length(Body)-2].Text)>0) and (ord(Body[length(Body)-2].Text[length(Body[length(Body)-2].Text)]) in UCS4WhiteSpaceEx) do begin
           Delete(Body[length(Body)-2].Text,length(Body[length(Body)-2].Text),1);
          end;
         end;
        end;
       end;
      end;
     end;
    end else begin
     Quote:=true;
    end;
    while (CurrentToken=tCHAR) and (CurrentTokenChar in UCS4WhiteSpace) do begin
     GetToken;
    end;
    continue;
   end;
   if CurrentToken=tNAME then begin
    if CurrentTokenString='__VA_ARGS__' then begin
     if VaArgs then begin
      b:=BodySize;
      inc(BodySize);
      if BodySize>length(Body) then begin
       SetLength(Body,BodySize*2);
      end;
      Body[b].Kind:=mbikVAARGS;
      Body[b].Quote:=Quote;
      Body[b].CharQuote:=CharQuote;
     end else begin
      AddWarning(PreprocessorChar+'define: __VA_ARGS__ not allowed here',nil);
      exit;
     end;
    end else begin
     j:=-1;
     for i:=0 to length(Parameters)-1 do begin
      if Parameters[i]=CurrentTokenString then begin
       j:=i;
       break;
      end;
     end;
     if j<0 then begin
      b:=BodySize;
      inc(BodySize);
      if BodySize>length(Body) then begin
       SetLength(Body,BodySize*2);
      end;
      Body[b].Kind:=mbikTEXT;
      Body[b].Text:=CurrentTokenString;
      Body[b].Quote:=Quote;
      Body[b].CharQuote:=CharQuote;
     end else begin
      b:=BodySize;
      inc(BodySize);
      if BodySize>length(Body) then begin
       SetLength(Body,BodySize*2);
      end;
      Body[b].Kind:=mbikParameter;
      Body[b].Value:=i;
      Body[b].Quote:=Quote;
      Body[b].CharQuote:=CharQuote;
     end;
    end;
   end else begin
    if CurrentToken=tCHAR then begin
     if not (CurrentTokenChar in [13,10]) then begin
      b:=BodySize;
      inc(BodySize);
      if BodySize>length(Body) then begin
       SetLength(Body,BodySize*2);
      end;
      Body[b].Kind:=mbikTEXT;
      Body[b].Text:=PUCUUTF32CharToUTF8(CurrentTokenChar);
      Body[b].Quote:=Quote;
      Body[b].CharQuote:=CharQuote;
     end;
    end else begin
     b:=BodySize;
     inc(BodySize);
     if BodySize>length(Body) then begin
      SetLength(Body,BodySize*2);
     end;
     Body[b].Kind:=mbikTEXT;
     Body[b].Text:=CurrentTokenString;
     Body[b].Quote:=Quote;
     Body[b].CharQuote:=CharQuote;
    end;
   end;
   Quote:=false;
   CharQuote:=false;
   GetToken;
  end;
  SetLength(Body,BodySize);
  AddDefine(Name,Body,length(Parameters),FunctionLike,VaArgs,mfNONE);
 end;
 procedure DoUNDEF;
 begin
  SkipGetToken;
  if CurrentToken<>tNAME then begin
   AddWarning(PreprocessorChar+'undef: missing macro name',nil);
   SkipEOL;
   exit;
  end;
  RemoveDefine(CurrentTokenString);
  SkipEOL;
 end;
 function SearchELSEENDIF(var Keyword:TPACCInt):boolean;
 var NestedLevel:TPACCInt;
 begin
  Keyword:=kwNONE;
  CurrentToken:=tCHAR;
  CurrentTokenChar:=10;
  NestedLevel:=1;
  repeat
   while not ((CurrentToken=tCHAR) and (CurrentTokenChar in [0,10])) do begin
    GetToken;
   end;
   SkipGetToken;
   if ((CurrentToken=tCHAR) and (CurrentTokenChar=ord(PreprocessorChar))) or
      ((CurrentToken=tCHAR) and (CurrentTokenChar=ord('%')) and (GetCharAt(0)=ord(':'))) then begin
    if (CurrentToken=tCHAR) and (CurrentTokenChar=ord('%')) and (GetCharAt(0)=ord(':')) then begin
     NextChar;
    end;
    SkipExtGetToken;
    Keyword:=LookUpKeyword(CurrentTokenString);
    if CurrentToken=tNAME then begin
     if Keyword in [kwIF,kwIFDEF,kwIFNDEF] then begin
      inc(NestedLevel);
     end else if Keyword=kwENDIF then begin
      dec(NestedLevel);
      if NestedLevel=0 then begin
       dec(IFNestedLevel);
       result:=false;
       exit;
      end;
     end else if (NestedLevel=1) and (Keyword in [kwELSE,kwELIF]) then begin
      result:=true;
      exit;
     end;
    end;
   end else if (CurrentToken=tCHAR) and (CurrentTokenCHAR=0) then begin
    AddError('missing #endif',nil,true);
    result:=true;
    exit;
   end;
  until false;                                                 
 end;
 function EvalString(s:TPUCUUTF8String):boolean;
 type TEvalToken=(etNONE,etCHAR,etNAME,etNUMBER,etDIV,etMINUS,etPLUS,etLT,
                  etLEQ,etLSH,etGT,etGEQ,etRSH,etEQ,etNOT,etNEQ,etAND,etLAND,
                  etOR,etLOR,etMOD,etMUL,etXOR,etLNOT,etLPAR,etRPAR,etCOMMA,
                  etQUEST,etCOLON,etUMINUS,etUPLUS,etASSIGN);
      TEvalTokenItem=record
       Token:TEvalToken;
       s:TPUCUUTF8String;
      end;
      TEvalTokenFIFO=record
       Tokens:array of TEvalTokenItem;
       CountTokens:TPACCInt;
       Index:TPACCInt;
      end;
      TValue=record
       case Sign:boolean of
        false:(us:longword);
        true:(s:TPACCInt);
      end;
      TEvalTokenPrecLUT=array[TEvalToken] of TPACCInt;
 const TokenUnary=[etLNOT,etNOT,etUPLUS,etUMINUS];
       TokenBinary=[etMUL,etDIV,etMOD,etPLUS,etMINUS,etLSH,etRSH,etLT,etLEQ,etGT,etGEQ,etEQ,etNEQ,etAND,etXOR,etOR,etLAND,etLOR,etCOMMA];
       WrongToken=TokenUnary+TokenBinary+[etQUEST,etCOLON];
       OpPrecLUT:TEvalTokenPrecLUT=(666, // etNONE
                                    666, // etCHAR
                                    666, // etNAME
                                    666, // etNUMBER
                                    12,  // etDIV
                                    11,  // etMINUS
                                    11,  // etPLUS
                                    9,   // etLT
                                    9,   // etLEQ
                                    10,  // etLSH
                                    9,   // etGT
                                    9,   // etGEQ
                                    10,  // etRSH
                                    8,   // etEQ
                                    13,  // etNOT
                                    8,   // etNEQ
                                    7,   // etAND,
                                    4,   // etLAND
                                    5,   // etOR
                                    3,   // etLOR
                                    12,  // etMOD
                                    12,  // etMUL
                                    6,   // etXOR
                                    13,  // etLNOT
                                    666, // etLPAR
                                    666, // etRPAR
                                    1,   // etCOMMA
                                    2,   // etQUEST
                                    666, // etCOLON
                                    13,  // etUMINUS
                                    13,  // etUPLUS
                                    666  // etASSIGN
                                   );
 var FIFO:TEvalTokenFIFO;
  procedure AddToken(Token:TEvalToken;const s:TPUCUUTF8String);
  var i:TPACCInt;
  begin
   i:=FIFO.CountTokens;
   inc(FIFO.CountTokens);
   if FIFO.CountTokens>length(FIFO.Tokens) then begin
    SetLength(FIFO.Tokens,FIFO.CountTokens*2);
   end;
   FIFO.Tokens[i].Token:=Token;
   FIFO.Tokens[i].s:=s;
  end;
  procedure Tokenize(s:TPUCUUTF8String);
  var i:TPACCInt;
      c:TPUCUUTF32Char;
      ns:TPUCUUTF8String;
  begin
   i:=1;
   while i<=length(s) do begin
    c:=PUCUUTF8CodeUnitGetCharAndIncFallback(s,i);
    case c of
     ord('/'):begin
      AddToken(etDIV,'/');
     end;
     ord('-'):begin
      AddToken(etMINUS,'-');
     end;
     ord('+'):begin
      AddToken(etPLUS,'+');
     end;
     ord('<'):begin
      if (i<=length(s)) and (s[i]='=') then begin
       AddToken(etLEQ,'<=');
       inc(i);
      end else if (i<=length(s)) and (s[i]='<') then begin
       AddToken(etLSH,'<<');
       inc(i);
      end else begin
       AddToken(etLT,'<');
      end;
     end;
     ord('>'):begin
      if (i<=length(s)) and (s[i]='=') then begin
       AddToken(etGEQ,'>=');
       inc(i);
      end else if (i<=length(s)) and (s[i]='>') then begin
       AddToken(etRSH,'>>');
       inc(i);
      end else begin
       AddToken(etGT,'>');
      end;
     end;
     ord('='):begin
      if (i<=length(s)) and (s[i]='=') then begin
       AddToken(etEQ,'==');
       inc(i);
      end else begin
       AddToken(etASSIGN,'=');
      end;
     end;
     ord('~'):begin
      AddToken(etNOT,'~');
     end;
     ord('!'):begin
      if (i<=length(s)) and (s[i]='=') then begin
       AddToken(etNEQ,'!=');
       inc(i);
      end else begin
       AddToken(etLNOT,'!');
      end;
     end;
     ord('&'):begin
      if (i<=length(s)) and (s[i]='&') then begin
       AddToken(etLAND,'&&');
       inc(i);
      end else begin
       AddToken(etAND,'&');
      end;
     end;
     ord('|'):begin
      if (i<=length(s)) and (s[i]='|') then begin
       AddToken(etLOR,'||');
       inc(i);
      end else begin
       AddToken(etOR,'|');
      end;
     end;
     ord('%'):begin
      AddToken(etMOD,'%');
     end;
     ord('*'):begin
      AddToken(etMUL,'*');
     end;
     ord('^'):begin
      AddToken(etXOR,'^');
     end;
     ord('('):begin
      AddToken(etLPAR,'(');
     end;
     ord(')'):begin
      AddToken(etRPAR,')');
     end;
     ord(','):begin
      AddToken(etCOMMA,',');
     end;
     ord('?'):begin
      AddToken(etQUEST,'?');
     end;
     ord(':'):begin
      AddToken(etCOLON,':');
     end;
     ord('0')..ord('9'):begin
      ns:=PUCUUTF32CharToUTF8(c);
      while (i<=length(s)) and (s[i] in ['0'..'9','a'..'z','A'..'Z']) do begin
       ns:=ns+s[i];
       inc(i);
      end;
      AddToken(etNUMBER,ns);
     end;
     else begin
      AddToken(etCHAR,PUCUUTF32CharToUTF8(c));
     end;
    end;
   end;
  end;
  procedure PreprocessTokens;
  var i:TPACCInt;
  begin
   i:=0;
   while i<FIFO.CountTokens do begin
    if FIFO.Tokens[i].Token=etPLUS then begin
     if (i>0) and not (FIFO.Tokens[i-1].Token in [etNUMBER,etNAME,etCHAR,etRPAR]) then begin
      FIFO.Tokens[i].Token:=etUPLUS;
     end;
    end else if FIFO.Tokens[i].Token=etMINUS then begin
     if (i>0) and not (FIFO.Tokens[i-1].Token in [etNUMBER,etNAME,etCHAR,etRPAR]) then begin
      FIFO.Tokens[i].Token:=etUMINUS;
     end;
    end;
    inc(i);
   end;
  end;
  function OpPrec(Token:TEvalToken):TPACCInt;
  begin
   result:=OpPrecLUT[Token];
{  case Token of
    etLNOT,etNOT,etUPLUS,etUMINUS:begin
     result:=13;
    end;
    etMUL,etDIV,etMOD:begin
     result:=12;
    end;
    etPLUS,etMINUS:begin
     result:=11;
    end;
    etLSH,etRSH:begin
     result:=10;
    end;
    etLT,etLEQ,etGT,etGEQ:begin
     result:=9;
    end;
    etEQ,etLNEQ:begin
     result:=8;
    end;
    etAND:begin
     result:=7;
    end;
    etXOR:begin
     result:=6;
    end;
    etOR:begin
     result:=5;
    end;
    etLAND:begin
     result:=4;
    end;
    etLOR:begin
     result:=3;
    end;
    etQUEST:begin
     result:=2;
    end;
    etCOMMA:begin
     result:=1;
    end;
    else begin
     result:=666;
    end;
   end;}
  end;
  function BoolVal(v:TValue):boolean;
  begin
   if v.Sign then begin
    result:=v.s<>0;
   end else begin
    result:=v.us<>0;
   end;
  end;
  function ParseNumber(s:TPUCUUTF8String):TValue;
  var i:TPACCInt;
      sv:TPACCInt;
      usv:longword;
      b:boolean;
  begin
   b:=false;
   FillChar(result,SizeOf(TValue),#0);
   result.Sign:=true;
   while (length(s)>0) and (s[length(s)] in ['l','L','u','U']) do begin
    if (length(s)>0) and (s[length(s)] in ['u','U']) then begin
     result.Sign:=false;
    end;
    Delete(s,length(s),1);
   end;
   if result.Sign then begin
    if (length(s)>0) and (s[1]='0') then begin
     if (length(s)>1) and (s[2] in ['x','X']) then begin
      result.s:=0;
      for i:=3 to length(s) do begin
       if (not b) and (((result.s*16) div 16)<>result.s) then begin
        b:=true;
        AddWarning('TERM: constant too large for destination type',nil);
       end;
       case s[i] of
        '0'..'9':begin
         sv:=ord(s[i])-ord('0');
        end;
        'a'..'f':begin
         sv:=(ord(s[i])-ord('a'))+$a;
        end;
        'A'..'F':begin
         sv:=(ord(s[i])-ord('A'))+$a;
        end;
        else begin
         sv:=0;
        end;
       end;
       result.s:=(result.s*16)+sv;
      end;
     end else begin
      result.s:=0;
      for i:=2 to length(s) do begin
       if (not b) and (((result.s*8) div 8)<>result.s) then begin
        b:=true;
        AddWarning('TERM: constant too large for destination type',nil);
       end;
       result.s:=(result.s*8)+(ord(s[i])-ord('0'));
      end;
     end;
    end else begin
     result.s:=0;
     for i:=1 to length(s) do begin
      if (not b) and (((result.s*10) div 10)<>result.s) then begin
       b:=true;
       AddWarning('TERM: constant too large for destination type',nil);
      end;
      result.s:=(result.s*10)+(ord(s[i])-ord('0'));
     end;
    end;
   end else begin
    if (length(s)>0) and (s[1]='0') then begin
     if (length(s)>1) and (s[2] in ['x','X']) then begin
      result.us:=0;
      for i:=3 to length(s) do begin
       if (not b) and (((result.us*16) div 16)<>result.us) then begin
        b:=true;
        AddWarning('TERM: constant too large for destination type',nil);
       end;
       case s[i] of
        '0'..'9':begin
         usv:=ord(s[i])-ord('0');
        end;
        'a'..'f':begin
         usv:=(ord(s[i])-ord('a'))+$a;
        end;
        'A'..'F':begin
         usv:=(ord(s[i])-ord('A'))+$a;
        end;
        else begin
         usv:=0;
        end;
       end;
       result.us:=(result.us*16)+usv;
      end;
     end else begin
      result.us:=0;
      for i:=2 to length(s) do begin
       if (not b) and (((result.us*8) div 8)<>result.us) then begin
        b:=true;
        AddWarning('TERM: constant too large for destination type',nil);
       end;
       result.us:=(result.us*8)+longword(ord(s[i])-ord('0'));
      end;
     end;
    end else begin
     result.us:=0;
     for i:=1 to length(s) do begin
      if (not b) and (((result.us*10) div 10)<>result.us) then begin
       b:=true;
       AddWarning('TERM: constant too large for destination type',nil);
      end;
      result.us:=(result.us*10)+longword(ord(s[i])-ord('0'));
     end;
    end;
   end;
  end;
  function DoTokenUnary(Token:TEvalToken;v:TValue):TValue;
  begin
   case Token of
    etLNOT:begin
     result.sign:=true;
     result.s:=ord(v.s=0);
    end;
    etNOT:begin
     result.sign:=v.sign;
     if result.sign then begin
      result.s:=not v.s;
     end else begin
      result.us:=not v.us;
     end;
    end;
    etPLUS:begin
     result:=v;
    end;
    etMINUS:begin
     result.sign:=v.sign;
     if result.sign then begin
      result.s:=-v.s;
     end else begin
      result.us:=-v.us;
     end;
    end;
    else begin
     result:=v;
    end;
   end;
  end;
  {$hints off}
  function DoTokenBinary(Token:TEvalToken;v1,v2:TValue):TValue;
  var iv2:TPACCInt;
  begin
   FillChar(result,SizeOf(TValue),#0);
   iv2:=0;
   case Token of
    etMUL,etDIV,etMOD,etPLUS,etMINUS,etAND,etXOR,etOR:begin
     if (not v1.Sign) or (not v2.Sign) then begin
      if v1.Sign then begin
       v1.Sign:=false;
       v1.us:=v1.s;
      end else if v2.Sign then begin
       v2.Sign:=false;
       v2.us:=v2.s;
      end;
      result.Sign:=false;
     end else begin
      result.Sign:=true;
     end;
    end;
    etLT,etLEQ,etGT,etGEQ,etEQ,etNEQ:begin
     if (not v1.Sign) or (not v2.Sign) then begin
      if v1.Sign then begin
       v1.Sign:=false;
       v1.us:=v1.s;
      end else if v2.Sign then begin
       v2.Sign:=false;
       v2.us:=v2.s;
      end
     end;
     result.Sign:=true;
    end;
    etLAND,etLOR:begin
     result.Sign:=true;
    end;
    etLSH,etRSH:begin
     result.Sign:=v1.Sign;
     if v2.Sign then begin
      iv2:=v2.s;
     end else begin
      iv2:=v2.us;
     end;
    end;
    etCOMMA:begin
     result.Sign:=v2.Sign;
    end;
   end;
   case Token of
    etMUL:begin
     if result.Sign then begin
      result.s:=v1.s*v2.s;
     end else begin
      result.us:=v1.us*v2.us;
     end;
    end;
    etDIV:begin
     if result.Sign then begin
      if v2.s=0 then begin
       AddError('TERM: divide by zero',nil,false);
       result.s:=0;
      end else begin
       result.s:=v1.s div v2.s;
      end;
     end else begin
      if v2.s=0 then begin
       AddError('TERM: divide by zero',nil,false);
       result.us:=0;
      end else begin
       result.us:=v1.us div v2.us;
      end;
     end;
    end;
    etMOD:begin
     if result.Sign then begin
      if v2.s=0 then begin
       AddError('TERM: divide by zero',nil,false);
       result.s:=0;
      end else begin
       result.s:=v1.s mod v2.s;
      end;
     end else begin
      if v2.s=0 then begin
       AddError('TERM: divide by zero',nil,false);
       result.us:=0;
      end else begin
       result.us:=v1.us mod v2.us;
      end;
     end;
    end;
    etPLUS:begin
     if result.Sign then begin
      result.s:=v1.s+v2.s;
     end else begin
      result.us:=v1.us+v2.us;
     end;
    end;
    etMINUS:begin
     if result.Sign then begin
      result.s:=v1.s-v2.s;
     end else begin
      result.us:=v1.us-v2.us;
     end;
    end;
    etLSH:begin
     if result.Sign then begin
      result.s:=v1.s*(1 shl iv2);
     end else begin
      result.us:=v1.us shl iv2;
     end;
    end;
    etRSH:begin
     if result.Sign then begin
      result.s:=v1.s div (1 shl iv2);
     end else begin
      result.us:=v1.us shr iv2;
     end;
    end;
    etLT:begin
     if v1.Sign then begin
      if v1.s<v2.s then begin
       result.s:=1;
      end else begin
       result.s:=0;
      end;
     end else begin
      if v1.us<v2.us then begin
       result.us:=1;
      end else begin
       result.us:=0;
      end;
     end;
    end;
    etLEQ:begin
     if v1.Sign then begin
      if v1.s<=v2.s then begin
       result.s:=1;
      end else begin
       result.s:=0;
      end;
     end else begin
      if v1.us<=v2.us then begin
       result.us:=1;
      end else begin
       result.us:=0;
      end;
     end;
    end;
    etGT:begin
     if v1.Sign then begin
      if v1.s>v2.s then begin
       result.s:=1;
      end else begin
       result.s:=0;
      end;
     end else begin
      if v1.us>v2.us then begin
       result.us:=1;
      end else begin
       result.us:=0;
      end;
     end;
    end;
    etGEQ:begin
     if v1.Sign then begin
      if v1.s>=v2.s then begin
       result.s:=1;
      end else begin
       result.s:=0;
      end;
     end else begin
      if v1.us>=v2.us then begin
       result.us:=1;
      end else begin
       result.us:=0;
      end;
     end;
    end;
    etEQ:begin
     if v1.Sign then begin
      if v1.s=v2.s then begin
       result.s:=1;
      end else begin
       result.s:=0;
      end;
     end else begin
      if v1.us=v2.us then begin
       result.us:=1;
      end else begin
       result.us:=0;
      end;
     end;
    end;
    etNEQ:begin
     if v1.Sign then begin
      if v1.s<>v2.s then begin
       result.s:=1;
      end else begin
       result.s:=0;
      end;
     end else begin
      if v1.us<>v2.us then begin
       result.us:=1;
      end else begin
       result.us:=0;
      end;
     end;
    end;
    etAND:begin
     if result.Sign then begin
      result.s:=v1.s and v2.s;
     end else begin
      result.us:=v1.us and v2.us;
     end;
    end;
    etXOR:begin
     if result.Sign then begin
      result.s:=v1.s xor v2.s;
     end else begin
      result.us:=v1.us xor v2.us;
     end;
    end;
    etOR:begin
     if result.Sign then begin
      result.s:=v1.s or v2.s;
     end else begin
      result.us:=v1.us or v2.us;
     end;
    end;
    etLAND:begin
     if v1.Sign then begin
      if (v1.s<>0) and (v2.s<>0) then begin
       result.s:=1;
      end else begin
       result.s:=0;
      end;
     end else begin
      if (v1.us<>0) and (v2.us<>0) then begin
       result.us:=1;
      end else begin
       result.us:=0;
      end;
     end;
    end;
    etLOR:begin
     if v1.Sign then begin
      if (v1.s<>0) or (v2.s<>0) then begin
       result.s:=1;
      end else begin
       result.s:=0;
      end;
     end else begin
      if (v1.us<>0) or (v2.us<>0) then begin
       result.us:=1;
      end else begin
       result.us:=0;
      end;
     end;
    end;
    etCOMMA:begin
     result:=v2;
    end;
   end;
  end;
  function DoLevel(MinPrec:TPACCInt;DoEval:boolean):TValue;
  var at:TEvalTokenItem;
      bp:TPACCInt;
      r1,r2,tr:TValue;
      qb:boolean;
  begin
   FillChar(result,SizeOf(TValue),#0);
   result.Sign:=true;
   if FIFO.Index>=FIFO.CountTokens then begin
    AddError('TERM: truncated constant integral expression',nil,false);
    exit;
   end;
   at:=FIFO.Tokens[FIFO.Index];
   inc(FIFO.Index);
   case at.Token of
    etLPAR:begin
     result:=DoLevel(0,DoEval);
     if FIFO.Index>=FIFO.CountTokens then begin
      AddError('TERM: truncated constant integral expression',nil,false);
      exit;
     end;
     at:=FIFO.Tokens[FIFO.Index];
     inc(FIFO.Index);
     if at.Token<>etRPAR then begin
      AddError('TERM: a right parenthesis was expected',nil,false);
      exit;
     end;
    end;
    etNUMBER:begin
     result:=ParseNumber(at.s);
    end;
    else begin
     if at.Token in TokenUnary then begin
      result:=DoTokenUnary(at.Token,DoLevel(OpPrec(at.Token),DoEval));
     end else if at.Token in WrongToken then begin
      AddError('TERM: rogue operator '''+at.s+''' in constant integral',nil,false);
      exit;
     end else begin
      AddError('TERM: invalid token in constant integral expression',nil,false);
      exit;
     end;
    end;
   end;
   repeat
    if FIFO.Index>=FIFO.CountTokens then begin
     exit;
    end;
    at:=FIFO.Tokens[FIFO.Index];
    inc(FIFO.Index);
    if at.Token in TokenBinary then begin
     bp:=OpPrec(at.Token);
     if bp>MinPrec then begin
      if ((at.Token=etLOR) and BoolVal(result)) or ((at.Token=etLAND) and not BoolVal(result)) then begin
       tr:=DoLevel(bp,false);
       if DoEval then begin
        result.Sign:=true;
        case at.Token of
         etLOR:result.s:=1;
         etLAND:result.s:=0;
        end;
       end;
      end else begin
       tr:=DoLevel(bp,DoEval);
       result:=DoTokenBinary(at.Token,result,tr);
      end;
      continue;
     end;
    end else if at.Token=etQUEST then begin
     bp:=OpPrec(etQUEST);
     if bp>=MinPrec then begin
      qb:=BoolVal(result);
      r1:=DoLevel(bp,DoEval and qb);
      if FIFO.Index>=FIFO.CountTokens then begin
       AddError('TERM: truncated constant integral expression',nil,false);
       exit;
      end;
      at:=FIFO.Tokens[FIFO.Index];
      inc(FIFO.Index);
      if at.Token<>etCOLON then begin
       AddError('TERM: a colon was expected',nil,false);
       exit;
      end;
      r2:=DoLevel(bp,DoEval and not qb);
      if DoEval then begin
       if qb then begin
        result:=r1;
       end else begin
        result:=r2;
       end;
      end;
      continue;
     end;
    end;
    dec(FIFO.Index);
    break;
   until false;
  end;
 var v:TValue;
 begin
  FillChar(FIFO,SizeOf(TEvalTokenFIFO),#0);
  SetLength(FIFO.Tokens,16);
  FIFO.CountTokens:=0;
  try
   Tokenize(s);
   PreprocessTokens;
   v:=DoLevel(0,true);
   result:=v.us<>0;
  finally
   SetLength(FIFO.Tokens,0);
  end;
 end;
 function Eval:boolean;
 var s,mn:TPUCUUTF8String;
     i,l,k,h,OldStringMode,OldNumberMode:TPACCInt;
     j:uint64;
     OldInEval:boolean;
 begin
  s:='';
  mn:='';
  OldStringMode:=StringMode;
  OldNumberMode:=NumberMode;
  StringMode:=2;
  NumberMode:=2;
  OldInEval:=InEval;
  InEval:=true;
  result:=false;
  l:=InputStackSize;
  while (InputStackSize>l) or not (GetCharAt(0) in [0,10]) do begin
   ExtGetToken;
   if CurrentToken=tNAME then begin
    if CurrentTokenString='defined' then begin
     SkipGetToken;
     if (CurrentToken=tCHAR) and (CurrentTokenChar=ord('(')) then begin
      SkipGetToken;
      if CurrentToken<>tNAME then begin
       AddError('TERM: missing macro name',nil,true);
       SkipEOL;
       exit;
      end;
      mn:=copy(CurrentTokenString,0,length(CurrentTokenString));
      SkipExtGetToken;
      if not ((CurrentToken=tCHAR) and (CurrentTokenChar=ord(')'))) then begin
       AddError('TERM: missing )',nil,true);
       SkipEOL;
       exit;
      end;
     end else begin
      if CurrentToken<>tNAME then begin
       AddError('TERM: missing macro name',nil,true);
       SkipEOL;
       exit;
      end;
      mn:=copy(CurrentTokenString,0,length(CurrentTokenString));
     end;
     if LookUp(mn)>=0 then begin
      s:=s+'1L';
     end else begin
      s:=s+'0L';
     end;
    end else begin
     s:=s+'0';
    end;
   end else if CurrentToken in [tCCHAR,tCCHARUTF8,tCCHARWCHAR,tCCHARCHAR16,tCCHARCHAR32] then begin
    s:=s+IntToStr(CurrentTokenValue);
    if (CurrentTokenValue>255) or (CurrentToken in [tCCHARUTF8,tCCHARWCHAR,tCCHARCHAR16,tCCHARCHAR32]) then begin
     s:=s+'L';
    end;
   end else if CurrentToken in [tSTRING,tSTRINGUTF8,tSTRINGWCHAR,tSTRINGCHAR16,tSTRINGCHAR32] then begin
    AddError('TERM: string literal not allowed here',nil,true);
   end else if CurrentToken=tNUMBER then begin
    s:=s+CurrentTokenString;
   end else begin
    if not ((CurrentToken=tCHAR) and (CurrentTokenChar in UCS4WhiteSpaceEx)) then begin
     s:=s+CurrentTokenString;
    end;
   end;
  end;
  result:=EvalString(s);
  InEval:=OldInEval;
  StringMode:=OldStringMode;
  NumberMode:=OldNumberMode;
 end;
 procedure DoIF;
 var Keyword:TPACCInt;
 begin
  inc(IFNestedLevel);
  if not Eval then begin
   repeat
    Keyword:=kwNONE;
    SearchELSEENDIF(Keyword);
    if not ((CurrentToken=tNAME) and (Keyword=kwELIF)) then begin
     SkipEOL;
     exit;
    end;
   until Eval;
  end;
 end;
 procedure DoIFDEF(ShouldResultValue:boolean);
 var Keyword:TPACCInt;
 begin
  SkipGetToken;
  if CurrentToken<>tNAME then begin
   if ShouldResultValue then begin
    AddWarning(PreprocessorChar+'ifdef: missing identifier',nil);
   end else begin
    AddWarning(PreprocessorChar+'ifndef: missing identifier',nil);
   end;
   SkipEOL;
   exit;
  end;
  inc(IFNestedLevel);
  if (LookUp(CurrentTokenString)>=0)<>ShouldResultValue then begin
   repeat
    Keyword:=kwNONE;
    SearchELSEENDIF(Keyword);
    if not ((CurrentToken=tNAME) and (Keyword=kwELIF)) then begin
     SkipEOL;
     exit;
    end;
   until Eval;
  end;
 end;
 procedure DoELSEELIF;
 var Keyword:TPACCInt;
 begin
  if IFNestedLevel=0 then begin
   AddError('missing #if',nil,true);
  end else begin
   Keyword:=kwNONE;
   while SearchELSEENDIF(Keyword) do begin
    Keyword:=kwNONE;
   end;
  end;
  SkipEOL;
 end;
 procedure DoENDIF;
 begin
  if IFNestedLevel=0 then begin
   AddError('missing #if',nil,true);
  end else begin
   dec(IFNestedLevel);
  end;
  SkipEOL;
 end;
 procedure DoLINE;
 var l,s,OldStringMode,OldNumberMode:TPACCInt;
 begin
  OldStringMode:=StringMode;
  OldNumberMode:=NumberMode;
  StringMode:=1;
  NumberMode:=1;
  SkipExtGetToken;
  if CurrentToken<>tNUMBER then begin
   AddWarning(PreprocessorChar+'line: missing number',nil);
   SkipEOL;
   StringMode:=OldStringMode;
   NumberMode:=OldNumberMode;
   exit;
  end;
//l:=LastLine;
  s:=LastSourceLocation.Source;
  l:=ParseNumber(CurrentTokenString);
  SkipExtGetToken;
  if (CurrentToken=tCHAR) and (CurrentTokenChar in [0,10]) then begin
   LastSourceLocation.Line:=l;
   if InputStackSize>0 then begin
    InputStack[InputStackSize-1].SourceLocation.Line:=LastSourceLocation.Line-1;
   end;
   PushInputSource(iskNONE,'',#10,nil);
   StringMode:=OldStringMode;
   NumberMode:=OldNumberMode;
   exit;
  end else if CurrentToken=tSTRING then begin
   if length(CurrentTokenString)<2 then begin
    AddError(PreprocessorChar+'line: string not closed',nil,true);
    StringMode:=OldStringMode;
    NumberMode:=OldNumberMode;
    exit;
   end;
   if CurrentTokenString[1]<>CurrentTokenString[length(CurrentTokenString)] then begin
    AddError(PreprocessorChar+'line: invalid string',nil,true);
    StringMode:=OldStringMode;
    NumberMode:=OldNumberMode;
    exit;
   end;
   s:=GetInputSourceIndex(iskFILE,Destringize(copy(CurrentTokenString,2,length(CurrentTokenString)-2)));
  end else if CurrentToken=tNAME then begin
   s:=GetInputSourceIndex(iskFILE,CurrentTokenString);
  end;
  while not ((CurrentToken=tCHAR) and (CurrentTokenChar in [0,10])) do begin
   GetToken;
  end;
  if InputStackSize>0 then begin
   LastSourceLocation.Source:=s;
   LastSourceLocation.Line:=l;
   LastSourceLocation.Column:=0;
   InputStack[InputStackSize-1].SourceLocation.Source:=LastSourceLocation.Source;
   InputStack[InputStackSize-1].SourceLocation.Line:=LastSourceLocation.Line-1;
   InputStack[InputStackSize-1].SourceLocation.Column:=0;
  end;
  PushInputSource(iskNONE,'',#10,nil);
  StringMode:=OldStringMode;
  NumberMode:=OldNumberMode;
 end;
 procedure DoPRAGMA;
 var s:TPUCUUTF8String;
     i:TPACCInt;
 begin
  s:='';
  repeat
   i:=NextChar;
   if (CurrentTokenChar<0) or (i in [0,10]) then begin
    break;
   end;
   s:=s+PUCUUTF32CharToUTF8(i);
  until false;
  i:=CountPragmaInfos;
  inc(CountPragmaInfos);
  if (i+1)>length(PragmaInfos) then begin
   SetLength(PragmaInfos,RoundUpToPowerOfTwo(i+2));
  end;
  PragmaInfos[i].CharPos:=OutputTextLength+1;
  PragmaInfos[i].Pragma:=s;
 end;
 procedure DoERROR;
 var s:TPUCUUTF8String;
 begin
  s:='';
  CurrentTokenString:='';
  SkipBlank;
  while not ((CurrentToken=tCHAR) and (CurrentTokenChar in [0,10])) do begin
   s:=s+CurrentTokenString;
   GetToken;
  end;
  AddError(s,nil,true);
 end;
 procedure DoWARNING;
 var s:TPUCUUTF8String;
 begin
  s:='';
  CurrentTokenString:='';
  SkipBlank;
  while not ((CurrentToken=tCHAR) and (CurrentTokenChar in [0,10])) do begin
   s:=s+CurrentTokenString;
   GetToken;
  end;
  AddWarning(s,nil);
 end;
var NewLine:boolean;
    MacroBody:TMacroBody;
    i:TPACCInt;
    s:TPUCUUTF8String;
    InputTextSourceLocations:TPACCPreprocessorInputSourceLocations;
begin
 MacroStringHashMap:=TPACCRawByteStringHashMap.Create;
 try
  KeywordStringHashMap:=TPACCRawByteStringHashMap.Create;
  try
   HideSet:=TStringList.Create;
   InputStack:=nil;
   InputStackSize:=0;
   Macros:=nil;
   CountMacros:=0;
   MacroBody:=nil;
   try
    KeywordStringHashMap.Add('include',pointer(TPACCPtrInt(kwINCLUDE)));
    KeywordStringHashMap.Add('include_next',pointer(TPACCPtrInt(kwINCLUDENEXT)));
    KeywordStringHashMap.Add('include_once',pointer(TPACCPtrInt(kwINCLUDEONCE)));
    KeywordStringHashMap.Add('includeonce',pointer(TPACCPtrInt(kwINCLUDEONCE)));
    KeywordStringHashMap.Add('import',pointer(TPACCPtrInt(kwINCLUDEONCE)));
    KeywordStringHashMap.Add('include_next_once',pointer(TPACCPtrInt(kwINCLUDENEXTONCE)));
    KeywordStringHashMap.Add('import_next',pointer(TPACCPtrInt(kwINCLUDENEXTONCE)));
    KeywordStringHashMap.Add('define',pointer(TPACCPtrInt(kwDEFINE)));
    KeywordStringHashMap.Add('undef',pointer(TPACCPtrInt(kwUNDEF)));
    KeywordStringHashMap.Add('if',pointer(TPACCPtrInt(kwIF)));
    KeywordStringHashMap.Add('ifdef',pointer(TPACCPtrInt(kwIFDEF)));
    KeywordStringHashMap.Add('ifndef',pointer(TPACCPtrInt(kwIFNDEF)));
    KeywordStringHashMap.Add('elif',pointer(TPACCPtrInt(kwELIF)));
    KeywordStringHashMap.Add('else',pointer(TPACCPtrInt(kwELSE)));
    KeywordStringHashMap.Add('endif',pointer(TPACCPtrInt(kwENDIF)));
    KeywordStringHashMap.Add('line',pointer(TPACCPtrInt(kwLINE)));
    KeywordStringHashMap.Add('pragma',pointer(TPACCPtrInt(kwPRAGMA)));
    KeywordStringHashMap.Add('error',pointer(TPACCPtrInt(kwERROR)));
    KeywordStringHashMap.Add('warning',pointer(TPACCPtrInt(kwWARNING)));
    LastSourceLocation.Source:=-1;
    LastSourceLocation.Line:=-1;
    LastSourceLocation.Column:=-1;
    InEval:=false;
    SetLength(MacroBody,0);
    AddDefine('__FILE__',MacroBody,0,false,false,mfFILE);
    AddDefine('__LINE__',MacroBody,0,false,false,mfLINE);
    AddDefine('__TIME__',MacroBody,0,false,false,mfTIME);
    AddDefine('__DATE__',MacroBody,0,false,false,mfDATE);
    AddDefine('_Pragma',MacroBody,0,false,false,mfPRAGMA);
    AddDefine('__BASE_FILE__',MacroBody,0,false,false,mfBASEFILE);
    AddDefine('__COUNTER__',MacroBody,0,false,false,mfCOUNTER);
    AddDefine('__INCLUDE_LEVEL__',MacroBody,0,false,false,mfINCLUDELEVEL);
    AddDefine('__TIMESTAMP__',MacroBody,0,false,false,mfTIMESTAMP);
    for i:=0 to PreprocessorDefines.Count-1 do begin
     s:=PreprocessorDefines.ValueFromIndex[i];
     if length(s)<>0 then begin
      SetLength(MacroBody,1);
      MacroBody[0].Kind:=mbikTEXT;
      MacroBody[0].Text:=s;
      MacroBody[0].Quote:=false;
      MacroBody[0].CharQuote:=false;
     end else begin
      SetLength(MacroBody,0);
     end;
     AddDefine(PreprocessorDefines.Names[i],MacroBody,0,false,false,mfNONE);
    end;
    begin
     SetLength(MacroBody,1);
     MacroBody[0].Kind:=mbikTEXT;
     MacroBody[0].Text:='1';
     MacroBody[0].Quote:=false;
     MacroBody[0].CharQuote:=false;
     AddDefine('__PACC__',MacroBody,0,false,false,mfNONE);
     AddDefine('__STDC__',MacroBody,0,false,false,mfNONE);
     AddDefine('__STDC_HOSTED__',MacroBody,0,false,false,mfNONE);
     AddDefine('__STDC_NO_ATOMICS__',MacroBody,0,false,false,mfNONE);
     AddDefine('__STDC_NO_COMPLEX__',MacroBody,0,false,false,mfNONE);
     AddDefine('__STDC_NO_THREADS__',MacroBody,0,false,false,mfNONE);
     AddDefine('__STDC_NO_VLA__',MacroBody,0,false,false,mfNONE);
     AddDefine('__STDC_UTF_16__',MacroBody,0,false,false,mfNONE);
     AddDefine('__STDC_UTF_32__',MacroBody,0,false,false,mfNONE);
    end;
    begin
     SetLength(MacroBody,1);
     MacroBody[0].Kind:=mbikTEXT;
     if TPACCInstance(Instance).Target.SizeOfPointer=8 then begin
      MacroBody[0].Text:='unsigned long';
     end else if TPACCInstance(Instance).Target.SizeOfPointer=2 then begin
      MacroBody[0].Text:='unsigned short';
     end else begin
      MacroBody[0].Text:='unsigned int';
     end;
     MacroBody[0].Quote:=false;
     MacroBody[0].CharQuote:=false;
     AddDefine('__SIZE_TYPE__',MacroBody,0,false,false,mfNONE);
    end;
    begin
     SetLength(MacroBody,1);
     MacroBody[0].Kind:=mbikTEXT;
     if TPACCInstance(Instance).Target.SizeOfPointer=8 then begin
      MacroBody[0].Text:='long';
     end else if TPACCInstance(Instance).Target.SizeOfPointer=2 then begin
      MacroBody[0].Text:='short';
     end else begin
      MacroBody[0].Text:='int';
     end;
     MacroBody[0].Quote:=false;
     MacroBody[0].CharQuote:=false;
     AddDefine('__PTRDIFF_TYPE__',MacroBody,0,false,false,mfNONE);
    end;
    begin
     SetLength(MacroBody,1);
     MacroBody[0].Kind:=mbikTEXT;
     MacroBody[0].Text:='int';
     MacroBody[0].Quote:=false;
     MacroBody[0].CharQuote:=false;
     AddDefine('__WCHAR_TYPE__',MacroBody,0,false,false,mfNONE);
    end;
    begin
     SetLength(MacroBody,1);
     MacroBody[0].Kind:=mbikTEXT;
     MacroBody[0].Text:='201112L';
     MacroBody[0].Quote:=false;
     MacroBody[0].CharQuote:=false;
     AddDefine('__STDC_VERSION__',MacroBody,0,false,false,mfNONE);
    end;
    begin
     SetLength(MacroBody,1);
     MacroBody[0].Kind:=mbikTEXT;
     MacroBody[0].Text:='201103L';
     MacroBody[0].Quote:=false;
     MacroBody[0].CharQuote:=false;
     AddDefine('__STDC_ISO_10646__',MacroBody,0,false,false,mfNONE);
    end;
    begin
     begin
      SetLength(MacroBody,1);
      MacroBody[0].Kind:=mbikTEXT;
      MacroBody[0].Text:=IntToStr(TPACCInstance(Instance).Target.SizeOfPointer);
      MacroBody[0].Quote:=false;
      MacroBody[0].CharQuote:=false;
      AddDefine('__SIZEOF_POINTER__',MacroBody,0,false,false,mfNONE);
     end;
     begin
      SetLength(MacroBody,1);
      MacroBody[0].Kind:=mbikTEXT;
      MacroBody[0].Text:=IntToStr(TPACCInstance(Instance).Target.SizeOf_PTRDIFF_T);
      MacroBody[0].Quote:=false;
      MacroBody[0].CharQuote:=false;
      AddDefine('__SIZEOF_PTRDIFF_T__',MacroBody,0,false,false,mfNONE);
     end;
     begin
      SetLength(MacroBody,1);
      MacroBody[0].Kind:=mbikTEXT;
      MacroBody[0].Text:=IntToStr(TPACCInstance(Instance).Target.SizeOf_SIZE_T);
      MacroBody[0].Quote:=false;
      MacroBody[0].CharQuote:=false;
      AddDefine('__SIZEOF_SIZE_T__',MacroBody,0,false,false,mfNONE);
     end;
     begin
      SetLength(MacroBody,1);
      MacroBody[0].Kind:=mbikTEXT;
      MacroBody[0].Text:=IntToStr(TPACCInstance(Instance).Target.SizeOfChar);
      MacroBody[0].Quote:=false;
      MacroBody[0].CharQuote:=false;
      AddDefine('__SIZEOF_CHAR__',MacroBody,0,false,false,mfNONE);
     end;
     begin
      SetLength(MacroBody,1);
      MacroBody[0].Kind:=mbikTEXT;
      MacroBody[0].Text:=IntToStr(TPACCInstance(Instance).Target.SizeOfShort);
      MacroBody[0].Quote:=false;
      MacroBody[0].CharQuote:=false;
      AddDefine('__SIZEOF_SHORT__',MacroBody,0,false,false,mfNONE);
     end;
     begin
      SetLength(MacroBody,1);
      MacroBody[0].Kind:=mbikTEXT;
      MacroBody[0].Text:=IntToStr(TPACCInstance(Instance).Target.SizeOfInt);
      MacroBody[0].Quote:=false;
      MacroBody[0].CharQuote:=false;
      AddDefine('__SIZEOF_INT__',MacroBody,0,false,false,mfNONE);
     end;
     begin
      SetLength(MacroBody,1);
      MacroBody[0].Kind:=mbikTEXT;
      MacroBody[0].Text:=IntToStr(TPACCInstance(Instance).Target.SizeOfLong);
      MacroBody[0].Quote:=false;
      MacroBody[0].CharQuote:=false;
      AddDefine('__SIZEOF_LONG__',MacroBody,0,false,false,mfNONE);
     end;
     begin
      SetLength(MacroBody,1);
      MacroBody[0].Kind:=mbikTEXT;
      MacroBody[0].Text:=IntToStr(TPACCInstance(Instance).Target.SizeOfLongLong);
      MacroBody[0].Quote:=false;
      MacroBody[0].CharQuote:=false;
      AddDefine('__SIZEOF_LONG_LONG__',MacroBody,0,false,false,mfNONE);
     end;
     begin
      SetLength(MacroBody,1);
      MacroBody[0].Kind:=mbikTEXT;
      MacroBody[0].Text:=IntToStr(TPACCInstance(Instance).Target.SizeOfFloat);
      MacroBody[0].Quote:=false;
      MacroBody[0].CharQuote:=false;
      AddDefine('__SIZEOF_FLOAT__',MacroBody,0,false,false,mfNONE);
     end;
     begin
      SetLength(MacroBody,1);
      MacroBody[0].Kind:=mbikTEXT;
      MacroBody[0].Text:=IntToStr(TPACCInstance(Instance).Target.SizeOfDouble);
      MacroBody[0].Quote:=false;
      MacroBody[0].CharQuote:=false;
      AddDefine('__SIZEOF_DOUBLE__',MacroBody,0,false,false,mfNONE);
     end;
     begin
      SetLength(MacroBody,1);
      MacroBody[0].Kind:=mbikTEXT;
      MacroBody[0].Text:=IntToStr(TPACCInstance(Instance).Target.SizeOfLongDouble);
      MacroBody[0].Quote:=false;
      MacroBody[0].CharQuote:=false;
      AddDefine('__SIZEOF_LONG_DOUBLE__',MacroBody,0,false,false,mfNONE);
     end;
    end;
    begin
     SetLength(MacroBody,1);
     MacroBody[0].Kind:=mbikTEXT;
     MacroBody[0].Text:='alignof';
     MacroBody[0].Quote:=false;
     MacroBody[0].CharQuote:=false;
     AddDefine('__alignof__',MacroBody,0,false,false,mfNONE);
    end;
    begin
     SetLength(MacroBody,1);
     MacroBody[0].Kind:=mbikTEXT;
     MacroBody[0].Text:='const';
     MacroBody[0].Quote:=false;
     MacroBody[0].CharQuote:=false;
     AddDefine('__const__',MacroBody,0,false,false,mfNONE);
    end;
    begin
     SetLength(MacroBody,1);
     MacroBody[0].Kind:=mbikTEXT;
     MacroBody[0].Text:='inline';
     MacroBody[0].Quote:=false;
     MacroBody[0].CharQuote:=false;
     AddDefine('__inline__',MacroBody,0,false,false,mfNONE);
    end;
    begin
     SetLength(MacroBody,1);
     MacroBody[0].Kind:=mbikTEXT;
     MacroBody[0].Text:='restrict';
     MacroBody[0].Quote:=false;
     MacroBody[0].CharQuote:=false;
     AddDefine('__restrict',MacroBody,0,false,false,mfNONE);
     AddDefine('__restrict__',MacroBody,0,false,false,mfNONE);
    end;
    begin
     SetLength(MacroBody,1);
     MacroBody[0].Kind:=mbikTEXT;
     MacroBody[0].Text:='signed';
     MacroBody[0].Quote:=false;
     MacroBody[0].CharQuote:=false;
     AddDefine('__signed__',MacroBody,0,false,false,mfNONE);
    end;
    begin
     SetLength(MacroBody,1);
     MacroBody[0].Kind:=mbikTEXT;
     MacroBody[0].Text:='typeof';
     MacroBody[0].Quote:=false;
     MacroBody[0].CharQuote:=false;
     AddDefine('__typeof__',MacroBody,0,false,false,mfNONE);
    end;
    begin
     SetLength(MacroBody,1);
     MacroBody[0].Kind:=mbikTEXT;
     MacroBody[0].Text:='volatile';
     MacroBody[0].Quote:=false;
     MacroBody[0].CharQuote:=false;
     AddDefine('__volatile__',MacroBody,0,false,false,mfNONE);
    end;
    for i:=0 to PreprocessorUndefines.Count-1 do begin
     RemoveDefine(PreprocessorUndefines.Names[i]);
    end;
    SetLength(MacroBody,0);
    SetLength(InputSources,16);
    SetLength(PragmaInfos,16);
    SetLength(OutputInfos,16);
    SetLength(OutputText,4096);
    CountInputSources:=0;
    CountPragmaInfos:=0;
    CountOutputInfos:=0;
    OutputTextLength:=0;
    StringMode:=0;
    NumberMode:=0;
    InputTextSourceLocations:=nil;
    try
     s:=PreprocessInputSourceChars(InputKind,InputName,InputText,InputTextSourceLocations);
     PushInputSource(InputKind,InputName,s,InputTextSourceLocations);
    finally
     InputTextSourceLocations:=nil;
    end;
    if length(TPACCInstance(Instance).Target.PreprocessorCode)>0 then begin
     InputTextSourceLocations:=nil;
     try
      s:=PreprocessInputSourceChars(iskNONE,'@target@',TPACCInstance(Instance).Target.PreprocessorCode,InputTextSourceLocations);
      PushInputSource(iskNONE,'@target@',s,InputTextSourceLocations);
     finally     
      InputTextSourceLocations:=nil;
     end;
    end;
    NewLine:=true;
    IFNestedLevel:=0;
    MacroLevel:=0;
    while InputStackSize>0 do begin
     ExtGetToken;
     if (((CurrentToken=tCHAR) and (CurrentTokenChar=ord(PreprocessorChar))) or
         ((CurrentToken=tCHAR) and (CurrentTokenChar=ord('%')) and (GetCharAt(0)=ord(':')))) and NewLine then begin
      if (CurrentToken=tCHAR) and (CurrentTokenChar=ord('%')) and (GetCharAt(0)=ord(':')) then begin
       NextChar;
      end;
      SkipGetToken;
      case LookUpKeyword(CurrentTokenString) of
       kwINCLUDE:begin
        DoINCLUDE(false,false);
       end;
       kwINCLUDENEXT:begin
        DoINCLUDE(true,false);
       end;
       kwINCLUDEONCE:begin
        DoINCLUDE(false,true);
       end;
       kwINCLUDENEXTONCE:begin
        DoINCLUDE(true,true);
       end;
       kwDEFINE:begin
        DoDEFINE;
       end;
       kwUNDEF:begin
        DoUNDEF;
       end;
       kwIF:begin
        DoIF;
       end;
       kwIFDEF:begin
        DoIFDEF(true);
       end;
       kwIFNDEF:begin
        DoIFDEF(false);
       end;
       kwELIF:begin
        DoELSEELIF;
       end;
       kwELSE:begin
        DoELSEELIF;
       end;
       kwENDIF:begin
        DoENDIF;
       end;
       kwLINE:begin
        DoLINE;
       end;
       kwPRAGMA:begin
        DoPRAGMA;
       end;
       kwERROR:begin
        DoERROR;
       end;
       kwWARNING:begin
        DoWARNING;
       end;
       else begin
        AddWarning('undefined statement: '+CurrentTokenString,nil);
        SkipEOL;
       end;
      end;
     end else begin
      case CurrentToken of
       tSTRING,tCCHAR:begin
        AddToOutput(CurrentTokenString);
        NewLine:=false;
       end;
       tSTRINGUTF8,tCCHARUTF8:begin
        AddToOutput('u8');
        AddToOutput(CurrentTokenString);
        NewLine:=false;
       end;
       tSTRINGWCHAR,tCCHARWCHAR:begin
        AddToOutput('L');
        AddToOutput(CurrentTokenString);
        NewLine:=false;
       end;
       tSTRINGCHAR16,tCCHARCHAR16:begin
        AddToOutput('u');
        AddToOutput(CurrentTokenString);
        NewLine:=false;
       end;
       tSTRINGCHAR32,tCCHARCHAR32:begin
        AddToOutput('U');
        AddToOutput(CurrentTokenString);
        NewLine:=false;
       end;
       tCHAR:begin
        if CurrentTokenChar in [0..255] then begin
         case CurrentTokenChar of
          0:begin
//         AddToOutput(CurrentTokenString);
          end;
          10:begin
           NewLine:=true;
          end;
          9,11,12,13,32:begin
          end;
          else begin
           NewLine:=false;
          end;
         end;
        end else if CurrentTokenChar<0 then begin
         continue;
        end;
        AddToOutput(CurrentTokenString);
       end;
       else begin
        NewLine:=false;
        AddToOutput(CurrentTokenString);
       end;
      end;
     end;
    end;
    if (CountOutputInfos>0) and (OutputInfos[CountOutputInfos-1].LastCharPos<>OutputTextLength) then begin
     OutputInfos[CountOutputInfos-1].LastCharPos:=OutputTextLength;
    end;
    SetLength(InputSources,CountInputSources);
    SetLength(PragmaInfos,CountPragmaInfos);
    SetLength(OutputInfos,CountOutputInfos);
    SetLength(OutputText,OutputTextLength);
   finally
    for i:=0 to length(InputStack)-1 do begin
     FreeAndNil(InputStack[i].HideSet);
    end;
    SetLength(InputStack,0);
    SetLength(Macros,0);
    SetLength(MacroBody,0);
    HideSet.Free;
   end;
  finally
   KeywordStringHashMap.Free;
  end;
 finally
  MacroStringHashMap.Free;
 end;
end;

procedure TPACCPreprocessor.ProcessString(const AInputText:TPUCUUTF8String;const AInputName:TPUCUUTF8String='');
begin
 InputText:=AInputText;
 InputName:=AInputName;
 ProcessIt;
end;

procedure TPACCPreprocessor.ProcessFile(const AInputFileName:TPUCUUTF8String);
begin
 InputText:=GetFileContent(AInputFileName);
 InputName:=AInputFileName;
 ProcessIt;
end;

end.
