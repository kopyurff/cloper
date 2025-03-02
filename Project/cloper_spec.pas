{ cloper_spec
  The unit of Cloning Optimizer (Cloper) tool.

  The file contains functionality for reading parameters of command line and to
  perform top level calls for special calls of tool:
  1. Creation of new .cfg file;
  2. Retrieving of disk serials;
  3. Showing of disk items, which correspond to search filters;
  4. Signing of disk by different identifier & showing list of plugged disks.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit cloper_spec;

interface
{ ReadParam
  Reads parameters of command line.
}
function ReadParam(ES : String; var val : String) : Boolean;

procedure WriteNewCfgFile(bShort : Boolean);

function GetDiskSerials(const VolID: String) : String;

procedure ShowDiskItems(bDir: Boolean; SchMsk, Serials: String);

procedure SignDisk(NewID: String);

implementation
uses
 Types, Windows, SysUtils, StrUtils,
 cloper_scrn, cloper_conf, cloper_into, cloper_base, cloper_funs, cloper_echo,
 DskTools, VolTools, StrTools, ConTools;

{ ReadParam
  Reads parameters of command line.
}
function ReadParam(ES : String; var val : String) : Boolean;
var i : Integer; s : String; begin
 Result := false; ES := LowerCase(Trim(ES));
 for i := 1 to 5 do begin
  val := LowerCase(Trim(ReplaceStr(ParamStr(i), '"', '')));
  if val = '' then break else if val.Chars[0] = '/' then
   val := RightStr(val, Length(val) - 1);

  if 0 < Pos(':', val) then begin
   if LeftStr(val, Pos(':', val) - 1) = ES then begin
    Result := true; val := RightStr(val, Length(val) - Pos(':', val)); exit
   end
  end else
   if val = ES then begin
    Result := true; val := ''; exit
   end;
 end;
 val := ''
end;

procedure WriteNewCfgFile(bShort : Boolean);
var
 f: TextFile; Bak, Ext, Str : String; i : Integer;
 function GS(SLS, FRS : String) : String;
 begin
  if bShort then Result := SLS else Result := SLS + FRS
 end;
begin
 try
  PrintShortHeader;
  WriteLn('The tool was started to create new .cfg file with sample values.');
  WriteLn('The target file name:'); WriteLn('>> ' + CfgFile); WriteLn;
  if FileExists(CfgFile) then begin
   Ext := ExtractFileExt(CfgFile);
   Bak := LeftStr(CfgFile, Length(CfgFile) - Length(Ext)) + '.bak';
   if FileExists(Bak) then begin
    writeln('The .bak file of older file already exists...');
    write('Overwrite it by current .cfg file (Y/N) ? ');
    readln(Str); Str := LowerCase(Trim(Str));
    if Str <> 'y' then begin
     writeln('Task aborted by user, press enter key to exit...'); readln; exit
    end;
    DeleteFile(Bak)
   end;
   MoveFile(PWideChar(CfgFile), PWideChar(Bak));
   writeln('Saved existing ' + Ext + ' file as .bak file...');
  end;
  AssignFile(f, CfgFile); ReWrite(f);
  Writeln(f, '[CommonSettings]');
  Writeln(f, GS('RunMode=ECHO','                ; Run mode (SILENT for script calls, default ECHO to print messages to screen)'));
  Writeln(f, GS('PauseAtEnd=YES','              ; Don''t exit after completion, wait key input (YES/NOT, default YES)'));
  Writeln(f, GS('IdSourceLssTarget=YES','       ; The source disk ID is less than ID of target (YES) or by data comparison (NOT), default YES'));
  Writeln(f, GS('UseUsnQueries=NOT','           ; Query NTFS USN journal to get volume items (YES) or use plain scan (NOT), default NOT'));
  Writeln(f, GS('MinVolumeSize=750','           ; Minimum volume size to process in MBytes  (0..1000, default 750)'));
  Writeln(f, GS('LogFile=YES','                 ; Write all messages to log-file "cloper.log" (YES/NOT, default NOT)'));
  Writeln(f, GS('LogLowPriErrors=NOT','         ; Log low priority errors (YES/NOT, default NOT)'));
  Writeln(f, GS('LogLowPriSysErrors=NOT','      ; Log low priority system errors (YES/NOT, default NOT)'));
  Writeln(f, GS('LogLowPriMessages=YES','       ; Log low priority messages (YES/NOT, default NOT)'));
  Writeln(f, GS('EchoLowPriMessages=YES','      ; Print on screen low priority messages'));
  Writeln(f, GS('ProcessPriority=HIGH','        ; Process priority of tool (NORMAL, HIGH or REALTIME, default HIGH)'));
  Writeln(f, GS('TimeStamp=' + FormatDateTime('YYMMDDHHMMSS', Now),'      ; Creation date time of this file, format YYMMDDHHMMSS, to correct time of tool'));
  Writeln(f, '');
  Writeln(f, '[VolumeCleanup]');
  if not bShort then
   Writeln(f, '; Folders to remove before optimizations (Folders#):');
  Writeln(f, 'Folders1=ProgramData\{*-*-*-*-*}*');
  if not bShort then
   Writeln(f, '; Files & subfolders to remove before optimizations (Files#):');
  Writeln(f, 'Files1=Users\*\AppData\Local\Temp\*');
  Writeln(f, 'Files2=Users\*\AppData\Local\Microsoft\Windows\Temporary Internet Files\*');
  Writeln(f, 'Files3=Users\*\AppData\Local\Microsoft\Windows\INetCache\*');
  Writeln(f, 'Files4=Windows\Temp\*');
  Writeln(f, 'Files5=pagefile.sys');
  Writeln(f, '');
  Writeln(f, '[Optimization]');
  Writeln(f, GS('MatchBlocks=YES','             ; Search matching fragments in target blocks (YES/NOT, default YES)'));
  Writeln(f, GS('ModificationThreshold=48','    ; Modification threshold for coinciding files (0..75%, default 48)'));
  Writeln(f, GS('MemoryUseLimit=900','          ; Upper size limit for file data in MB (100..1500, default 900)'));
  Writeln(f, GS('ClusterWise=YES','             ; Compare clusters (YES) or sectors (NOT), default YES'));
  Writeln(f, GS('MoveToBlocks=YES','            ; Move all data in front of persistent location (YES/NOT, default YES)'));
  Writeln(f, ';');
  Writeln(f, GS('MoveNewData=YES','             ; Defragment new data (YES/NOT, default YES)'));
  Writeln(f, GS('ConsolidateNewData=YES','      ; Consolidate all new data on disk (YES/NOT, default YES)'));
  Writeln(f, ';');
  Writeln(f, GS('PatchFreeSpace=YES','          ; Patch free space of source by target data (YES/NOT, default YES)'));
  Writeln(f, ';');
  Writeln(f, GS('CompressData=YES','            ; Apply NTFS compression to data (YES/NOT, default YES)'));
  Writeln(f, GS('NtfsCompressionType=DEFAULT',' ; DEFAULT or LZNT1 compression type, DEFAULT is default value'));
  if not bShort then
   Writeln(f, '; The masks to select files, which will be not compressed. String delimiter "|", e.g.: *.cab|*.mp4');
  Writeln(f, 'SkipCompressionForFiles=*.rar|*.zip|*.7zip|*.cab|*.mp4|*.avi');
  Writeln(f, GS('ReportFoundSkipFiles=NOT','    ; (YES/NOT, default NOT)'));
  Writeln(f, '');
  Writeln(f, '[Cloning]');
  Writeln(f, GS('BufferSizeKB=7680','           ; The size of read buffer in KB, default 7680 KB, range 5120..10240 KB.'));
  Writeln(f, GS('ActivateTargetDisk=YES','      ; (YES/NOT, default YES), activate target disk, set offline source disk'));
  Writeln(f, GS('AssignLetters=T,U,V','         ; C,D,..,Y,Z or nothing, assign labels to active volumes of online disk'));
  Writeln(f, GS('RestoreLetters=NOT','          ; (YES/NOT, default NOT), set original labels to online disk after task'));
  Writeln(f, '');
  Writeln(f, '[Testing]');
  Writeln(f, GS('ReportFiles=NOT','             ; (YES/NOT, default NOT), write lists of failed files to text files'));
  Writeln(f, GS('EndUserLetters=C,D,E','        ; label letters C..Z of optimized volumes for end user VM'));
  CloseFile(f);
  WriteLn('The new .cfg file was successfully created.');
  Writeln;
  writeln('Press enter key to exit...'); ReadLn
 except on E:Exception do
  writeln('Error ' + E.Message)
 end;
end;

function GetDiskSerials(const VolID: String) : String;
begin
 Result := VolTools.GetDiskSerials(VolIDToDiskNumber(VolID))
end;

procedure ShowDiskItems(bDir: Boolean; SchMsk, Serials: String);
var
 i, NumPart, NumDskSrc, NumDskTgt: Integer; items: TStringDynArray;
begin
 PrintShortHeader(false);
 NumPart := GetVolumeNumbersBySerials(Serials, NumDskSrc, SrcVolSpec.VolumeNumber,
                                               NumDskTgt, TgtVolSpec.VolumeNumber,
                                               []);
 if NumPart < 0 then begin
  OutputMessage(mFailure, 'Failed to find 2 disks with serials ' + Serials +
        '.', mcError);
  exit
 end;
 WriteLn('Started ', SelStr(bDir, 'folder', 'file'), 's search on source volumes...');
 WriteLn;
 while 0 < NumPart do begin
  if not GetVolSpecs(SrcVolSpec) then
   if PrintFailure(SrcVolSpec, 4) then exit;

  WriteLn('The source disk #' + IntToStr(NumDskSrc) +
        ', volume #' + IntToStr(SrcVolSpec.VolumeNumber) + ':');
  if bDir then
   items := FindPathsOnDisk(SrcVolSpec.Root + SchMsk)
  else
   items := FindFilesOnDisk(SrcVolSpec.Root + SchMsk);
  if items[0] = '' then
   WriteLn('No items found...')
  else begin
   WriteLn('Found ' + IntToStr(Length(items)) + ' items:');
   for i := 0 to Length(items) - 1 do
    WriteLn('> ' + RightStr(items[i], Length(items[i]) - Length(SrcVolSpec.Root)));
   WriteLn
  end;

  // Get numbers of next volumes if disks have several active partitions:
  NumPart := GetNextVolumeNumbers(NumPart + 1, [NumDskSrc, NumDskTgt])
 end;
 AnyKeyToExit
end;

procedure SignDisk(NewID: String);
 function PrintInfo: TStringDynArray;
 const
  TabCols: array[0..3] of TCRTColumn = ((Align:crtCenter;TextColor:Word(ccLightGray)),
                                        (Align:crtCenter;TextColor:Word(ccLightGray)),
                                        (Align:crtLeft  ;TextColor:Word(ccLightGray)),
                                        (Align:crtCenter;TextColor:Word(ccLightGray)));
 var
  c1, c2, c3: TStringDynArray;
  procedure RowToPrint(s1, s2, s3, s4: String);
  var
   i: Integer;
  begin
   i := Length(c1);
   SetLength(c1    , i + 1);     c1[i] := s1; SetLength(c2, i + 1); c2[i] := s2;
   SetLength(Result, i + 1); Result[i] := s3; SetLength(c3, i + 1); c3[i] := s4
  end;
 var
  CRTGrid : TCRTGrid;
  DskIdx: Integer; sDskID, sDskStyle: String; DskStyle: Byte; bAlloc: Boolean;
 begin
  CRTGrid := TCRTGrid.Create(0, $0, true, true, Word(ccDarkGray));
  CRTGrid.AddColumns(TabCols);
  RowToPrint('Number', 'Style', 'Identifier', 'Allocated');
  DskIdx := 0;
  bAlloc := PartInfoAction(piaChkAlloc, DskIdx, sDskID, DskStyle);
  while DskStyle < High(Byte) do begin
   case DskStyle of
   0: sDskStyle := 'RAW'; 1: sDskStyle := 'GPT'; 2: sDskStyle := 'MBR';
   end;
   if bAlloc then
    RowToPrint(PadLeft(IntToStr(DskIdx + 1), 2, '0'), sDskStyle, sDskID, 'YES')
   else
    RowToPrint(PadLeft(IntToStr(DskIdx + 1), 2, '0'), sDskStyle, sDskID, 'NOT');
   Inc(DskIdx);
   bAlloc := PartInfoAction(piaChkAlloc, DskIdx, sDskID, DskStyle)
  end;
  CRTGrid.DrawGrid([c1, c2, Result, c3],
                   'MOUNTED DISKS AND THEIR IDENTIFIERS:', $6, crtLeft)
 end;
var
 Ids: TStringDynArray; sNum: String; i: Integer; ds: Byte;
begin
 Ids := PrintInfo;
 if NewID <> '' then begin
  NewID := UpperCase(Trim(NewID)); sNum := '';
  for i := 1 to Length(Ids) - 1 do if NewID = Trim(Ids[i]) then begin
   OutputMessage(mUserError, ' The disk #' + IntToStr(i) + ' already has identifier "' + NewId + '"');
   AnyKeyToExit; exit
  end else if Pos('-', NewID) = Pos('-', Trim(Ids[i])) then begin
   if sNum <> '' then sNum := sNum + ', '; sNum := sNum + IntToStr(i)
  end;
  WriteLn('Specified to set the new signature value "' + NewID + '".');
  WriteLn('This signature can be applied to disk(s) ' + sNum + '.');
  OutputMessage(mWarning, 'The change of disk ID with installed OS can make it not bootable.');
  WriteLn;
  if 0 < Pos(', ', sNum) then begin
   Write('Type disk number to change it''s ID: '); readln(i);
   if 0 = Pos(IntToStr(i), sNum) then begin
    OutputMessage(mUserError, ' Invalid number of disk ' + IntToStr(i) + '.');
    AnyKeyToExit; exit
   end;
  end;
  WriteLn;
  if AnyKeyToExit('Y') then
   PartInfoAction(piaSetID, i - 1, NewID, ds)
  else
   exit;
  WriteLn;
  PrintInfo
 end;
 AnyKeyToExit
end;

end.
