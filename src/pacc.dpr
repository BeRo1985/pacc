program pacc;
{$i PACC.inc}
{$if defined(Win32) or defined(Win64)}
 {$apptype console}
{$ifend}

{%File 'PACC.inc'}
{%File 'SASM.inc'}
{%File 'SASMDataContent.inc'}

uses
  SysUtils,
  Classes,
  PasMP in 'PasMP.pas',
  PasDblStrUtils in 'PasDblStrUtils.pas',
  PUCU in 'PUCU.pas',
  SASMCore in 'SASMCore.pas',
  SASMData in 'SASMData.pas',
  PACCTypes in 'PACCTypes.pas',
  PACCRawByteStringHashMap in 'PACCRawByteStringHashMap.pas',
  PACCPointerHashMap in 'PACCPointerHashMap.pas',
  PACCSort in 'PACCSort.pas',
  PACCGlobals in 'PACCGlobals.pas',
  PACCAbstractSyntaxTree in 'PACCAbstractSyntaxTree.pas',
  PACCTarget in 'PACCTarget.pas',
  PACCPreprocessor in 'PACCPreprocessor.pas',
  PACCLexer in 'PACCLexer.pas',
  PACCParser in 'PACCParser.pas',
  PACCAnalyzer in 'PACCAnalyzer.pas',
  PACCHighLevelOptimizer in 'PACCHighLevelOptimizer.pas',
  PACCControlFlowGraph in 'PACCControlFlowGraph.pas',
  PACCLinker in 'PACCLinker.pas',
  PACCLinker_COFF_PE in 'PACCLinker_COFF_PE.pas',
  PACCLinker_COFF_PE_LCIDToCodePageLookUpTable in 'PACCLinker_COFF_PE_LCIDToCodePageLookUpTable.pas',
  PACCLinker_ELF_ELF in 'PACCLinker_ELF_ELF.pas',
  PACCTarget_x86_32 in 'PACCTarget_x86_32.pas',
  PACCTarget_x86_64_SystemV in 'PACCTarget_x86_64_SystemV.pas',
  PACCInstance in 'PACCInstance.pas';

var ParameterIndex,CountParameters,Index:TPACCInt32;
    Parameter:TPUCUUTF8String;

    IncludeDirectories,Defines,Undefines,InputFiles,OutputFiles:TStringList;

    Options:TPACCOptions;

    ShowHelp:boolean=false;

    OnlyRunPreprocessAndCompilationSteps:boolean=false;

    OnlyRunPreprocessAndCompileAndAssembleSteps:boolean=false;

    OnlyRunPreprocessStep:boolean=false;

    OutputLock:TPasMPCriticalSection;

    HasErrors:TPasMPBool32=false;

    TargetClass:TPACCTargetClass;

    AssembledCodeStreams:TList;

    AssembledCodeFileNames:TStringList;

    TargetList:TStringList;

procedure Compile(const InputFileName,OutputFileName:TPUCUUTF8String;const InputIndex:TPACCInt32);
var Instance:TPACCInstance;
    AssemblerCodeStream:TMemoryStream;
    AssembledCodeStream:TMemoryStream;
    StringList:TStringList;
    Extension:TPUCUUTF8String;
begin
 Instance:=TPACCInstance.Create(TargetClass,Options);
 try

  Extension:=LowerCase(ExtractFileExt(InputFileName));

  Instance.Preprocessor.IncludeDirectories.AddStrings(IncludeDirectories);
  Instance.Preprocessor.PreprocessorDefines.AddStrings(Defines);
  Instance.Preprocessor.PreprocessorUndefines.AddStrings(Undefines);
  try

   if (Extension='.asm') or (Extension='.s') then begin

    if not OnlyRunPreprocessAndCompilationSteps then begin

     AssemblerCodeStream:=TMemoryStream.Create;
     try

      OutputLock.Acquire;
      try
       writeln('Assembling ',ExtractFileName(InputFileName),' . . .');
      finally
       OutputLock.Release;
      end;
      AssemblerCodeStream.LoadFromFile(InputFileName);

      AssembledCodeStream:=TMemoryStream.Create;
      try

       Instance.Target.AssembleCode(AssemblerCodeStream,AssembledCodeStream,InputFileName);

       if OnlyRunPreprocessAndCompileAndAssembleSteps then begin

        OutputLock.Acquire;
        try
         writeln('Writing ',ExtractFileName(OutputFileName),' . . .');
        finally
         OutputLock.Release;
        end;
        AssembledCodeStream.SaveToFile(OutputFileName);

       end else begin

        OutputLock.Acquire;
        try
         writeln('Storing assembled code for ',ExtractFileName(InputFileName),' for the linking step . . .');
         AssembledCodeStreams[InputIndex]:=AssembledCodeStream;
         AssembledCodeFileNames[InputIndex]:=ChangeFileExt(InputFileName,'.o');
         AssembledCodeStream:=nil;
        finally
         OutputLock.Release;
        end;

       end;

      finally
       AssembledCodeStream.Free;
      end;

     finally
      AssemblerCodeStream.Free;
     end;

    end;

   end else if (Extension='.c') or (Extension='.h') then begin

    OutputLock.Acquire;
    try
     writeln('Preprocessing ',ExtractFileName(InputFileName),' . . .');
    finally
     OutputLock.Release;
    end;
    Instance.Preprocessor.ProcessFile(InputFileName);

    if OnlyRunPreprocessStep then begin

     StringList:=TStringList.Create;
     try
      StringList.Text:=Instance.Preprocessor.OutputText;
      StringList.SaveToFile(OutputFileName);
     finally
      StringList.Free;
     end;
     OutputLock.Acquire;
     try
      writeln('Writing ',ExtractFileName(OutputFileName),' . . .');
     finally
      OutputLock.Release;
     end;

    end else begin

     OutputLock.Acquire;
     try
      writeln('Lexing ',ExtractFileName(InputFileName),' . . .');
     finally
      OutputLock.Release;
     end;
     Instance.Lexer.Process;

     OutputLock.Acquire;
     try
      writeln('Parsing ',ExtractFileName(InputFileName),' . . .');
     finally
      OutputLock.Release;
     end;
     Instance.Parser.Process;

     OutputLock.Acquire;
     try
      writeln('Analyzing ',ExtractFileName(InputFileName),' . . .');
     finally
      OutputLock.Release;
     end;
     Instance.Analyzer.Process;

     OutputLock.Acquire;
     try
      writeln('High-level optimizing ',ExtractFileName(InputFileName),' . . .');
     finally
      OutputLock.Release;
     end;
     Instance.HighLevelOptimizer.Process;

     OutputLock.Acquire;
     try
      writeln('Generating assembler code for ',ExtractFileName(InputFileName),' . . .');
     finally
      OutputLock.Release;
     end;
     AssemblerCodeStream:=TMemoryStream.Create;
     try

      Instance.Target.GenerateCode(Instance.Parser.Root,AssemblerCodeStream);

      if OnlyRunPreprocessAndCompilationSteps then begin

       OutputLock.Acquire;
       try
        writeln('Writing ',ExtractFileName(OutputFileName),' . . .');
       finally
        OutputLock.Release;
       end;
       AssemblerCodeStream.SaveToFile(OutputFileName);

      end else begin

       AssembledCodeStream:=TMemoryStream.Create;
       try

        OutputLock.Acquire;
        try
         writeln('Assembling code for ',ExtractFileName(InputFileName),' . . .');
        finally
         OutputLock.Release;
        end;
        Instance.Target.AssembleCode(AssemblerCodeStream,AssembledCodeStream,ChangeFileExt(InputFileName,'.s'));

        if OnlyRunPreprocessAndCompileAndAssembleSteps then begin

         OutputLock.Acquire;
         try
          writeln('Writing ',ExtractFileName(OutputFileName),' . . .');
         finally
          OutputLock.Release;
         end;
         AssembledCodeStream.SaveToFile(OutputFileName);

        end else begin

         OutputLock.Acquire;
         try
          writeln('Storing assembled code for ',ExtractFileName(InputFileName),' for the linking step . . .');
          AssembledCodeStreams[InputIndex]:=AssembledCodeStream;
          AssembledCodeFileNames[InputIndex]:=ChangeFileExt(InputFileName,'.o');
          AssembledCodeStream:=nil;
         finally
          OutputLock.Release;
         end;

        end;

       finally
        AssembledCodeStream.Free;
       end;

      end;

     finally
      AssemblerCodeStream.Free;
     end;

    end;

   end else if (Extension='.o') or (Extension='.obj') then begin

    AssembledCodeStream:=TMemoryStream.Create;
    try

     OutputLock.Acquire;
     try
      writeln('Loading ',ExtractFileName(InputFileName),' . . .');
     finally
      OutputLock.Release;
     end;
     AssembledCodeStream.LoadFromFile(InputFileName);
     OutputLock.Acquire;
     try
      AssembledCodeStreams[InputIndex]:=AssembledCodeStream;
      AssembledCodeFileNames[InputIndex]:=InputFileName;
      AssembledCodeStream:=nil;
     finally
      OutputLock.Release;
     end;

    finally
     AssembledCodeStream.Free;
    end;

   end;

  except

   on e:EPACCError do begin
//  writeln('Error: ["',TPACCPreprocessor(Instance.Preprocessor).SourceFiles[e.SourceLocation.Source],'"][',e.SourceLocation.Line+1,'] ',e.Message);
   end;
   on e:Exception do begin
    OutputLock.Acquire;
    try
     writeln('['+e.ClassName+']: error: '+e.Message);
     TPasMPInterlocked.Write(HasErrors,true);
    finally
     OutputLock.Release;
    end;

   end;
  end;
  if Instance.HasWarnings or Instance.HasErrors then begin
   OutputLock.Acquire;
   try
    if Instance.HasWarnings then begin
     write(ErrOutput,Instance.Warnings.Text);
    end;
    if Instance.HasErrors then begin
     write(ErrOutput,Instance.Errors.Text);
     TPasMPInterlocked.Write(HasErrors,true);
    end;
   finally
    OutputLock.Release;
   end;
  end;
 finally
  Instance.Free;
 end;
end;

procedure ParallelFORCompileFunction(const Job:PPasMPJob;const ThreadIndex:TPasMPInt32;const Data:pointer;const FromIndex,ToIndex:TPasMPNativeInt);
var Index:TPasMPNativeInt;
begin
 Index:=FromIndex;
 while Index<=ToIndex do begin
  if Index<OutputFiles.Count then begin
   Compile(InputFiles[Index],OutputFiles[Index],Index);
  end else begin
   Compile(InputFiles[Index],'',Index);
  end;
  inc(Index);
 end;
end;

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

var Instance:TPACCInstance;
    InputStream,OutputStream:TMemoryStream;
    InputFileName,Extension:TPUCUUTF8String;
begin

 TargetClass:=PACCRegisteredTargetClassList[0];

 OutputLock:=TPasMPCriticalSection.Create;

 Options:=PACCDefaultOptions;

 IncludeDirectories:=TStringList.Create;

 Defines:=TStringList.Create;

 Undefines:=TStringList.Create;

 InputFiles:=TStringList.Create;

 OutputFiles:=TStringList.Create;

 AssembledCodeStreams:=TList.Create;

 AssembledCodeFileNames:=TStringList.Create;

 try
  CountParameters:=ParamCount;

  ParameterIndex:=1;
  while ParameterIndex<=CountParameters do begin
   Parameter:=ParamStr(ParameterIndex);
   inc(ParameterIndex);
   if (length(Parameter)>0) and (Parameter[1]='-') then begin
    if Parameter='-c' then begin
     OnlyRunPreprocessAndCompileAndAssembleSteps:=true;
    end else if Parameter='-D' then begin
     if ParameterIndex<=CountParameters then begin
      Defines.Add(ParamStr(ParameterIndex));
      inc(ParameterIndex);
     end;
    end else if Parameter='-E' then begin
     OnlyRunPreprocessStep:=true;
    end else if Parameter='-h' then begin
     ShowHelp:=true;
    end else if Parameter='-I' then begin
     if ParameterIndex<=CountParameters then begin
      IncludeDirectories.Add(ParamStr(ParameterIndex));
      inc(ParameterIndex);
     end;
    end else if Parameter='-o' then begin
     if ParameterIndex<=CountParameters then begin
      OutputFiles.Add(ParamStr(ParameterIndex));
      inc(ParameterIndex);
     end;
    end else if Parameter='-shared' then begin
     Options.CreateSharedLibrary:=true;
    end else if Parameter='-S' then begin
     OnlyRunPreprocessAndCompilationSteps:=true;
    end else if Parameter='-t' then begin
     if ParameterIndex<=CountParameters then begin
      TargetClass:=PACCRegisteredTargetClassHashMap[TPACCRawByteString(ParamStr(ParameterIndex))];
      inc(ParameterIndex);
     end else begin
      TargetClass:=nil;
     end;
    end else if Parameter='-T' then begin
     if ParameterIndex<=CountParameters then begin
      GlobalPasMPMaximalThreads:=StrToIntDef(ParamStr(ParameterIndex),GlobalPasMPMaximalThreads);
      inc(ParameterIndex);
     end;
    end else if Parameter='-U' then begin
     if ParameterIndex<=CountParameters then begin
      Undefines.Add(ParamStr(ParameterIndex));
      inc(ParameterIndex);
     end;
    end else if (length(Parameter)>1) and (Parameter[1]='-') and (Parameter[2]='W') then begin
     if Parameter='-Wall' then begin
      Options.EnableWarnings:=true;
     end else if Parameter='-Werror' then begin
      Options.WarningsAreErrors:=true;
     end;
    end else if (length(Parameter)>1) and (Parameter[1]='-') and (Parameter[2]='w') then begin
     Options.EnableWarnings:=false;
    end;
   end else begin
    InputFiles.Add(Parameter);
   end;
  end;

  writeln('PACC - PAscal C Compiler');
  writeln('Version ',PACCVersionString);
  writeln(PACCCopyrightString);

  if (InputFiles.Count>0) and not ShowHelp then begin

   if assigned(TargetClass) then begin

    writeln('Target: ',TargetClass.GetName);

    TPasMP.CreateGlobalInstance;

    Instance:=TPACCInstance.Create(TargetClass,Options);
    try

     if OnlyRunPreprocessStep or
        OnlyRunPreprocessAndCompilationSteps or
        OnlyRunPreprocessAndCompileAndAssembleSteps then begin
      for Index:=OutputFiles.Count to InputFiles.Count-1 do begin
       if OnlyRunPreprocessStep then begin
        OutputFiles.Add(ChangeFileExt(InputFiles[Index],'.ppc'));
       end else if OnlyRunPreprocessAndCompilationSteps then begin
        OutputFiles.Add(ChangeFileExt(InputFiles[Index],'.s'));
       end else if OnlyRunPreprocessAndCompileAndAssembleSteps then begin
        OutputFiles.Add(ChangeFileExt(InputFiles[Index],'.o'));
       end else begin
        OutputFiles.Add(ChangeFileExt(InputFiles[Index],Instance.Target.GetDefaultOutputExtension));
       end;
      end;
     end else begin
      for Index:=0 to InputFiles.Count-1 do begin
       AssembledCodeStreams.Add(nil);
       AssembledCodeFileNames.Add('');
      end;
      if OutputFiles.Count>0 then begin
       for Index:=OutputFiles.Count-1 downto 1 do begin
        OutputFiles.Delete(Index);
       end;
      end else begin
       OutputFiles.Add(ChangeFileExt(InputFiles[0],Instance.Target.GetDefaultOutputExtension));
      end;
     end;

     GlobalPasMP.Invoke(GlobalPasMP.ParallelFor(nil,0,InputFiles.Count-1,ParallelFORCompileFunction));

     if not (OnlyRunPreprocessStep or
             OnlyRunPreprocessAndCompilationSteps or
             OnlyRunPreprocessAndCompileAndAssembleSteps or
             HasErrors) then begin

      OutputStream:=TMemoryStream.Create;
      try
       try

        if (OutputFiles.Count=1) and (InputFiles.Count>0) then begin

         InputFileName:=ChangeFileExt(InputFiles[0],'.imp');
         if FileExists(InputFileName) and (InputFiles.IndexOf(InputFileName)<0) then begin
          InputFiles.Add(InputFileName);
         end;

         InputFileName:=ChangeFileExt(InputFiles[0],'.exp');
         if FileExists(InputFileName) and (InputFiles.IndexOf(InputFileName)<0) then begin
          InputFiles.Add(InputFileName);
         end;

         InputFileName:=ChangeFileExt(InputFiles[0],'.res');
         if FileExists(InputFileName) and (InputFiles.IndexOf(InputFileName)<0) then begin
          InputFiles.Add(InputFileName);
         end;

        end;

        for Index:=0 to InputFiles.Count-1 do begin
         InputFileName:=InputFiles[Index];
         Extension:=LowerCase(ExtractFileExt(InputFileName));
         if (Extension='.a') or (Extension='.lib') then begin
          writeln('Loading archive from ',ExtractFileName(InputFileName),' . . .');
          InputStream:=TMemoryStream.Create;
          try
           InputStream.LoadFromFile(InputFileName);
           Instance.Linker.AddArchive(InputStream,InputFileName);
          finally
           InputStream.Free;
          end;
         end else if Extension='.exp' then begin
          writeln('Loading exports from ',ExtractFileName(InputFileName),' . . .');
          Instance.Linker.AddExports(GetFileContent(InputFileName),InputFileName);
         end else if Extension='.imp' then begin
          writeln('Loading imports from ',ExtractFileName(InputFileName),' . . .');
          Instance.Linker.AddImports(GetFileContent(InputFileName),InputFileName);
         end else if Extension='.res' then begin
          writeln('Loading resources from ',ExtractFileName(InputFileName),' . . .');
          InputStream:=TMemoryStream.Create;
          try
           InputStream.LoadFromFile(InputFileName);
           Instance.Linker.AddResources(InputStream,InputFileName);
          finally
           InputStream.Free;
          end;
         end;
        end;

        writeln('Linking to ',ExtractFileName(OutputFiles[0]),' . . .');
        Instance.Target.LinkCode(AssembledCodeStreams,AssembledCodeFileNames,OutputStream,OutputFiles[0]);
        OutputStream.SaveToFile(OutputFiles[0]);
       except
       end;
      finally
       OutputStream.Free;
      end;

      if Instance.HasWarnings or Instance.HasErrors then begin
       OutputLock.Acquire;
       try
        if Instance.HasWarnings then begin
         write(ErrOutput,Instance.Warnings.Text);
        end;
        if Instance.HasErrors then begin
         write(ErrOutput,Instance.Errors.Text);
         TPasMPInterlocked.Write(HasErrors,true);
        end;
       finally
        OutputLock.Release;
       end;
      end;

     end;

    finally
     Instance.Free;
    end;

   end else begin

    writeln('Error: No target specified');
    HasErrors:=true;

   end;

  end else begin

   ShowHelp:=true;

  end;

  if ShowHelp then begin
   // abcdefghijklmnopqrstuvwxyz
   writeln('  Usage: '+ChangeFileExt(ExtractFileName(ParamStr(0)),'')+' [options] [input files]');
   writeln('Options: -c                         Only run preprocess, compile, and assemble steps');
   writeln('         -D <symbol>(=<value>)      Predefine symbol as a macro');
   writeln('         -E                         Only run preprocess step');
   writeln('         -h                         Show help');
   writeln('         -I <direcrtory>            Add directory to include search path');
   writeln('         -o <output file>           With -c, -E or -S: The n-th output file name of n-th input file name in same order');
   writeln('                                            Otherwise: The single output file name of the linked binary file');
   writeln('         -shared                    Create shared library');
   writeln('         -S                         Only run preprocess and compilation steps');
   writeln('         -t <target>                Select target');
   writeln('         -T <number of CPU threads> Number of CPU threads to use for to compile all input files');
   writeln('         -U <symbol>                Undefine symbol');
   writeln('         -w                         Disable all warnings');
   writeln('         -Wall                      Enable all warnings');
   writeln('         -Werror                    Make all warnings into errors');
// writeln('         -Xlinker <arg>             Pass <arg> on to the linker');
   if PACCRegisteredTargetClassList.Count>0 then begin
    TargetList:=TStringList.Create;
    try
     TargetList.Add(TPACCTargetClass(PACCRegisteredTargetClassList[0]).GetName);
     for Index:=1 to PACCRegisteredTargetClassList.Count-1 do begin
      TargetList.Add(TPACCTargetClass(PACCRegisteredTargetClassList[Index]).GetName);
     end;
     TargetList.Sort;
     writeln('Targets: ',TargetList[0]);
     for Index:=1 to TargetList.Count-1 do begin
      writeln('         ',TargetList[Index]);
     end;
    finally
     TargetList.Free;
    end;
   end;
  end;

 finally

  OutputLock.Free;

  IncludeDirectories.Free;

  Defines.Free;

  Undefines.Free;

  InputFiles.Free;

  OutputFiles.Free;

  for Index:=0 to AssembledCodeStreams.Count-1 do begin
   TObject(AssembledCodeStreams[Index]).Free;
   AssembledCodeStreams[Index]:=nil;
  end;
  AssembledCodeStreams.Free;

  AssembledCodeFileNames.Free;

 end;

 DebuggerWaitEnterKey;

 if HasErrors then begin
  halt(1);
 end else begin
  halt(0);
 end;

end.
