{ cloper_conf
  The unit of Cloning Optimizer (Cloper) tool.

  The file encapsulates functionality for initialization of program variables
  by their configuration values given by .cfg file or by command line.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit cloper_conf;

interface
uses
 Types, cloper_typs;

type
 TTlVnData = (tvdVersion = 0, tvdDates = 1, tvdAuthor = 2);

function CheckCfgExists : Boolean;

function ReadCfgValue(Section, Ident, Default: String;
  bLowerCase: Boolean = true): String; overload;
function ReadCfgValue(Section, Ident: String; var Result_: String;
  Default: String; bLowerCase: Boolean = true): Boolean; overload;

procedure WriteCfgValue(const Section, Ident, Value: string);

procedure ReadCommonCfgValues;
procedure ReadOptimizationCfgValues(var bMoveNewData, bOnlyDefragment,
  bMatchBlocks, bMoveToBlocks, bPatchFreeSpace: Boolean; var NoNtfsCext: String;
  var MemoryUseLimit: Cardinal);
procedure ReadTestingCfgValues(NumPart: Integer);
function GetReadStep: Cardinal;

function GetAssignLetters(bChkCfg: Boolean = true) : TStringDynArray;

function GetToolVersionData(dattype : TTlVnData = tvdVersion) : String;

procedure DefineTimeShift;

{ GetVolumeNumberBySerials
  Obtains 1st volume number of disk, which has active USN and is valid for
  optimization. For support of disks with several active partitions added
  parameter begnum. Returns disk number of source/target partitions.
}
function GetVolumeNumbersBySerials(const Serials: String;
  var NumDskSrc, NumVolSrc, NumDskTgt, NumVolTgt: Integer;
  ExclDskNums : array of Integer; begnum: Integer = 1): Integer;

{ GetNextVolumeNumbers
  Obtains next volume number of disks, which have active USN and are valid
  for optimization. Returns partition number of source/target disks or -1.
}
function GetNextVolumeNumbers(begnum: Integer;
  DskNums: array of Integer): Integer;

procedure ReportTestingEvent(ETyp: TTstEvents; FilePath: String;
                          bMarker: Boolean = false);
procedure RemoveEmptyTestingReports;
var
 CfgFile : String;

implementation
uses
  Windows, SysUtils, StrUtils, cloper_base, VolTools, DskTools,
  VolFiles, DateUtils, cloper_echo, cloper_scrn, cloper_into,
  cloper_funs, StrTools;

function GetToolVersionData(dattype : TTlVnData) : String;
begin
 case dattype of
 tvdVersion : Result := '1.12';
 tvdDates   : Result := '2015-2017';
 tvdAuthor  : Result := 'Anton Kopiev';
 end
end;

function CheckCfgExists : Boolean;
begin
 Result := FileExists(CfgFile);
end;

function ReadCfgValue(Section, Ident, Default: String;
  bLowerCase: Boolean = true): String;
var
 Buffer: array[0..2047] of Char;
begin
 SetString(Result, Buffer, GetPrivateProfileString(MarshaledString(Section),
   MarshaledString(Ident), MarshaledString(Default), Buffer, Length(Buffer),
   MarshaledString(CfgFile)));
 if 0 < Pos(';', Result) then // Enable inline comments:
  Result := LeftStr(Result, Pos(';', Result) - 1);
 if bLowerCase then
  Result := LowerCase(Result);
 Result := Trim(Result)
end;

function ReadCfgValue(Section, Ident: String; var Result_: String;
  Default: String; bLowerCase: Boolean = true): Boolean;
begin
 Result_ := ReadCfgValue(Section, Ident, Default, bLowerCase);
 Result := Result_ <> Default;
end;

procedure WriteCfgValue(const Section, Ident, Value: string);
begin
 if not WritePrivateProfileString(MarshaledString(Section), MarshaledString(Ident),
                                  MarshaledString(Value), MarshaledString(CfgFile)) then
  OutputMessage(mSystemErr, 'Write failure to configuration file "' + CfgFile
      + '"')
end;

procedure ReadCommonCfgValues;
const
 Common = 'CommonSettings';
var
 sPRIORITY_CLASS : String;
begin
 SilentMode      := ReadCfgValue(Common, 'RunMode', 'echo') = 'silent';
 if SilentMode and bOptTask then
  OutType := Integer(oLogFile);
 bPauseAtEnd     := ReadCfgValue(Common, 'PauseAtEnd', 'yes') <> 'not';
 ErrLowPriOutput := ReadCfgValue(Common, 'LogLowPriErrors', 'not') = 'yes';
 SysELowPrOutput := ReadCfgValue(Common, 'LogLowPriSysErrors', 'not') = 'yes';
 LogLowPriOutput := ReadCfgValue(Common, 'LogLowPriMessages', 'not') = 'yes';
 EcoLowPriOutput := ReadCfgValue(Common, 'EchoLowPriMessages', 'not') = 'yes';
 bRestoreLetters := ReadCfgValue('Cloning', 'RestoreLetters', 'not') = 'yes';
 bActivateTgtDsk := ReadCfgValue('Cloning', 'ActivateTargetDisk', 'yes') <> 'not';
 MinVolumeSize   := GetDigitFromString(
                    ReadCfgValue(Common, 'MinVolumeSize', '750'), 0, 1000, 750);
 sPRIORITY_CLASS := ReadCfgValue(Common, 'ProcessPriority', 'high');
 if sPRIORITY_CLASS = 'normal' then
  PRIORITY_CLASS := NORMAL_PRIORITY_CLASS
 else
  if sPRIORITY_CLASS = 'high' then
   PRIORITY_CLASS := HIGH_PRIORITY_CLASS
  else
   if sPRIORITY_CLASS = 'realtime' then
    PRIORITY_CLASS := REALTIME_PRIORITY_CLASS
end;

procedure ReadOptimizationCfgValues(var bMoveNewData, bOnlyDefragment,
  bMatchBlocks, bMoveToBlocks, bPatchFreeSpace: Boolean;
  var NoNtfsCext: String; var MemoryUseLimit: Cardinal);
begin
 bMoveNewData := ReadCfgValue('Optimization', 'MoveNewData', 'yes') <> 'not';
 bOnlyDefragment := ReadCfgValue('Optimization', 'ConsolidateNewData', 'yes') = 'not';
 if ReadCfgValue('Optimization', 'NtfsCompressionType', 'default') <> 'lznt1' then
   COMPRESSION_FORMAT := COMPRESSION_FORMAT_DEFAULT
 else
   COMPRESSION_FORMAT := COMPRESSION_FORMAT_LZNT1;
 bMatchBlocks := ReadCfgValue('Optimization', 'MatchBlocks', 'yes') <> 'not';
 bMoveToBlocks := ReadCfgValue('Optimization', 'MoveToBlocks', 'yes') <> 'not';
 bCompressData := ReadCfgValue('Optimization', 'CompressData', 'yes') <> 'not';

 if bMatchBlocks or bMoveToBlocks then begin
  if bCompressData then
   NoNtfsCext := ReadCfgValue('Optimization', 'SkipCompressionForFiles', '', true)
  else
   NoNtfsCext := '';
  ModificationThreshold :=
      GetDigitFromString(ReadCfgValue('Optimization',
      'ModificationThreshold', '48'), 0, 75, 48);
  if bMatchBlocks then begin
   MemoryUseLimit :=
       GetDigitFromString(ReadCfgValue('Optimization',
       'MemoryUseLimit', '900'), 100, 1500, 900) * 1024; // KB
   bClusterWise := ReadCfgValue('Optimization', 'ClusterWise', 'yes') <> 'not';
  end
 end;
 bPatchFreeSpace := ReadCfgValue('Optimization', 'PatchFreeSpace', 'yes') <> 'not';
end;

procedure ReadTestingCfgValues(NumPart: Integer);
var
 i, j: Integer; UL: TStringDynArray; usu: String;
begin
 bReportFiles := ReadCfgValue('Testing', 'ReportFiles', 'not') = 'yes';
 if bReportFiles then begin
  EndUserLetters := ReadCfgValue('Testing', 'EndUserLetters', 'C,E,D', false);
  Assert(0 < Length(SrcLabels), 'SrcLabels isn''t initialized in ReadTestingCfgValues');
  UL := SplitString(EndUserLetters, ',');
  usu := '$';
  for i := 0 to Length(UL) - 1 do begin
   UL[i] := Trim(UL[i]);
   if 1 < Length(UL[i]) then UL[i] := UL[i][1];
   if 0 = Pos(UL[i], 'CDEFGHIJKLMNOPQRSTUVWXYZ') then UL[i] := '';
   if 0 < Pos('$' + UL[i] + '$', usu) then
    UL[i] := ''
   else
    if UL[i] <> '' then usu := usu + UL[i] + '$'
  end;
  i := 0;
  for j := NumPart - 1 to Length(SrcLabels) - 1 do
   if Length(UL) <= i then break else begin
    if Trim(UL[i]) <> '' then SrcLabels[j].ULab := UL[i] + ':\';
    Inc(i)
   end;
 end;
end;

function GetReadStep: Cardinal;
begin
 // Default "read by 7.5 Mb", max read\write is 10 Mb.
 Result := GetDigitFromString(ReadCfgValue('Cloning', 'BufferSizeKB', '7680'),
    5120, 10240, 7680) * 1024 div SrcVolSpec.BytesPerSector
end;

procedure DefineTimeShift; // YYMMDDHHMMSS
var
 sTimeStamp, sts : String;
 chr : Char;
 fs: TFormatSettings;
 fda: TWin32FindData;
 smt: TSystemTime;
 ft: FileTime;
 TimeStamp, CrtnTime : TDateTime;
 TimeStampMs, CrtnTimeMs : Int64;
begin
 sTimeStamp := ReadCfgValue('CommonSettings', 'TimeStamp', '');
 if Length(sTimeStamp) <> 12 then exit;
 sts := sTimeStamp;
 for chr in ['0','1','2','3','4','5','6','7','8','9'] do
  sts := ReplaceStr(sts, chr, '');
 if sts <> '' then exit;
 if not IsValidDateTime(StrToInt('20' + MidStr(sTimeStamp, 0, 2)),
                        StrToInt(MidStr(sTimeStamp, 3, 2)),
                        StrToInt(MidStr(sTimeStamp, 5, 2)),
                        StrToInt(MidStr(sTimeStamp, 7, 2)),
                        StrToInt(MidStr(sTimeStamp, 9, 2)),
                        StrToInt(MidStr(sTimeStamp, 11, 2)), 0) then exit;
 TimeStamp := EncodeDateTime(StrToInt('20' + MidStr(sTimeStamp, 0, 2)),
                             StrToInt(MidStr(sTimeStamp, 3, 2)),
                             StrToInt(MidStr(sTimeStamp, 5, 2)),
                             StrToInt(MidStr(sTimeStamp, 7, 2)),
                             StrToInt(MidStr(sTimeStamp, 9, 2)),
                             StrToInt(MidStr(sTimeStamp, 11, 2)), 0);
 FindFirstFile(PChar(CfgFile), fda);
 FileTimeToLocalFileTime(fda.ftCreationTime, ft);
 FileTimeToSystemTime(ft, smt);
 CrtnTime := SystemTimeToDateTime(smt);
 TimeShift := SecondsBetween(CrtnTime, TimeStamp)
end;

procedure WriteMsgToLog(Msg : String);
var
 f: TextFile;
begin
 try
  AssignFile(f, LogFile); if FileExists(LogFile) then Append(f) else ReWrite(f);
  Writeln(f, Msg); CloseFile(f)
 finally
  //
 end;
end;

function GetAssignLetters(bChkCfg: Boolean = true) : TStringDynArray;
var
 i : Integer;
 procedure GetExistingLetters(var Labels: TVolLabels);
 var
  j : Integer;
 begin
  i := 0;
  for j := 0 to Length(Labels) - 1 do
   if Labels[j].VLab <> '' then begin
    Inc(i); SetLength(Result, i); Result[i - 1] := Labels[j].VLab
   end;
  if i = 0 then begin SetLength(Result, 1); Result[0] := '' end
 end;
begin
 if not bChkCfg and bRestoreLetters then begin
  Assert((0 < Length(SrcLabels)) and (Length(SrcLabels) = Length(TgtLabels)),
          'The arrays of available disk volumes are not initialized.');
  if ReadCfgValue('Cloning', 'ActivateTargetDisk', 'yes') = 'yes' then
   GetExistingLetters(TgtLabels)
  else
   GetExistingLetters(SrcLabels)
 end else begin
  Result := SplitString(ReadCfgValue('Cloning', 'AssignLetters', ''), ',');
  if Length(Result) = 0 then begin
    SetLength(Result, 1); Result[0] := ''; exit;
  end;
  for i := 0 to Length(Result) - 1 do Result[i] := UpperCase(Trim(Result[i]));
  if Trim(Result[0]) = '' then exit;
  i := 0;
  repeat
   if Pos(Result[i], 'QWERTYUIOPSDFGHJKLZXCVNM') = 0 then begin
    OutputMessage(mUserError, 'Label name "' + Result[i] +
           '" isn''t allowed, ignored "AssignLetters".');
    SetLength(Result, 1); Result[0] := ''; exit
   end;
   Inc(i);
  until i >= Length(Result)
 end
end;

{ GetVolumeNumberBySerials
  Obtains 1st volume number of disk, which has active USN and is valid for
  optimization. For support of disks with several active partitions added
  parameter begnum. Returns partition number of source/target disks or -1.
}
function GetVolumeNumbersBySerials(const Serials: String;
  var NumDskSrc, NumVolSrc, NumDskTgt, NumVolTgt: Integer;
  ExclDskNums : array of Integer; begnum: Integer = 1): Integer;
type
 TIM = record
  NP, MI: Integer;
 end;
label exi; var
 i, j, k, m: Integer; b : Boolean; guid : String;
 PrErMd : Cardinal; buf : Boolean;

 aim: array of TIM;
 procedure AddInfoMsg(NP, MI: Integer);
 var
  i: Integer;
 begin
  i := Length(aim);
  if (i = 0) or (aim[i - 1].NP <> NP) then begin
   SetLength(aim, i + 1); aim[i].NP := NP; aim[i].MI := MI
  end
 end;
begin
 NumDskSrc := -1; NumVolSrc := 0; NumDskTgt := -1; NumVolTgt := 0; Result := -1;
 if begnum < 1 then begnum := 1;
 PrErMd := SetErrorMode(SEM_FAILCRITICALERRORS);//Don't ask to insert CD or card
 buf := ReadCfgValue('CommonSettings', 'IdSourceLssTarget', 'not') = 'yes';

 for i := 0 to 9 do begin
  b := false;
  for j := 0 to Length(ExclDskNums) - 1 do if ExclDskNums[j] = i then begin
   b := true; break
  end;
  if b then continue; // Another instance of program works with disks.

  guid := GetVolumeGuid(cfsDiskPartition, [i, 1]);
  if guid = '' then continue; // Skip non-initialized disks.

  if GetDiskSerials(i) = UpperCase(Serials) then begin
   b := false; j := begnum;
   while j < 8 do begin
    guid := GetVolumeGuid(cfsDiskPartition, [i, j]);
    if guid = '' then break;
    k := 1;
    repeat
     if guid = GetVolumeGuid(cfsGlobalVolume, [k]) then break;
     Inc(k)
    until (27 <= k) and (guid = '');
    if 27 <= k then exit;
    if bUseUsnQueries then begin
     b := CheckUsnActive(k);
     if not b then AddInfoMsg(j, 2)
    end else begin
     b := IsNtfsFileSytem(k);
     if not b then AddInfoMsg(j, 3)
    end;

    if b then begin
     if NumDskSrc < 0 then NumVolSrc := k else NumVolTgt := k;
     break
    end else Inc(j);
   end;
   if b then begin
    if NumDskSrc < 0 then NumDskSrc := i else NumDskTgt := i
   end;
   if (0 < NumDskSrc) and (0 < NumDskTgt) then begin
    if GetVolumeSerial(GetVolumeGuid(cfsGlobalVolume, [NumVolSrc]))
    =  GetVolumeSerial(GetVolumeGuid(cfsGlobalVolume, [NumVolTgt])) then
    begin
     Result := j;
     if not buf then
      DefineSourceTargetByFileCount(NumDskSrc, NumDskTgt, NumVolSrc, NumVolTgt)
    end;
    goto exi
   end
  end
 end;
exi:
 SetErrorMode(PrErMd);
 if 0 <= Result then
  for i := 0 to Length(aim) - 1 do
   AddDelayedLogMsgs(InfoToLog(aim[i].MI, [aim[i].NP]))
end;

{ GetNextVolumeNumbers
  Obtains next volume number of disks, which have active USN and are valid
  for optimization. Returns partition number of source/target disks or -1.
}
function GetNextVolumeNumbers(begnum: Integer;
  DskNums: array of Integer): Integer;
var
 i, j, k : Integer; H : THandle; guid : String;
begin
 Result := -1;
 if(Length(DskNums) <> 2) or (DskNums[0] < 0) or (DskNums[1] < 0) then exit;
 SrcVolSpec.VolumeNumber := -1;
 TgtVolSpec.VolumeNumber := -1;
 if begnum < 1 then begnum := 1;
 for i := 0 to 1 do begin
  j := begnum;
  while j < 8 do begin
   guid := GetVolumeGuid(cfsDiskPartition, [DskNums[i], j]);
   if guid = '' then exit;
   k := 1;
   repeat
    if guid = GetVolumeGuid(cfsVolume, [k]) then break;
    Inc(k)
   until (27 <= k) and (guid = '');
   if 27 <= k then exit;
   if bUseUsnQueries and not CheckUsnActive (k) then
    OutputMessage(mLogNoDate, InfoToLog(2, [j]))
   else
    if not bUseUsnQueries and not IsNtfsFileSytem(k) then
     OutputMessage(mLogNoDate, InfoToLog(3, [j]))
    else begin
     if i = 0 then
      SrcVolSpec.VolumeNumber := k
     else
      TgtVolSpec.VolumeNumber := k;
     break
    end;
   Inc(j)
  end;
 end;
 if (0 < SrcVolSpec.VolumeNumber) and (0 < TgtVolSpec.VolumeNumber) then begin
  if GetVolumeSerial(GetVolumeGuid(cfsVolume, [SrcVolSpec.VolumeNumber]))
  =  GetVolumeSerial(GetVolumeGuid(cfsVolume, [TgtVolSpec.VolumeNumber]))then
   Result := j
 end
end;

type
 TRepTstEvt = record HC: TCardinalDynArray; Err: Boolean; FN, FH: String; end;
var
 hcRTE: array[0..6] of TRepTstEvt = (
  (HC:nil; Err:False; FN:'hndlFails'; FH:'was impossible to obtain handle'),
  (HC:nil; Err:False; FN:'dfrgFails'; FH:'defragmentation failed'),
  (HC:nil; Err:False; FN:'cmprFails'; FH:'NTFS compression failed'),
  (HC:nil; Err:False; FN:'cmprSkips'; FH:'was specified to skip compression'),
  (HC:nil; Err:False; FN:'dataChngd'; FH:'was found background layout modification after defragmentation'),
  (HC:nil; Err:False; FN:'remvFails'; FH:'deletion failed during cleanup (includes folders)'),
  (HC:nil; Err:False; FN:'volatDmgd'; FH:'was impossible to define layout or found volatile behavior')
 );

procedure ReportTestingEvent(ETyp: TTstEvents; FilePath: String;
                          bMarker: Boolean = false);
var
 f: TextFile; t: STring;

 procedure _ReWrite(ei: Integer);
 var
  Hdr: TStringDynArray; i: Integer;
 begin
  ReWrite(f);
  Hdr := PrintHeader(2);
  for i := 0 to Length(Hdr) - 1 do Writeln(f, Hdr[i]);
  Hdr := GetAlignedArray(
   'The automatically created report contains list of files for which '
   + hcRTE[ei].FH + '. The report''s data corresponds to the last tool run ' +
   'and will be rewritten after its next start.', 80, 2);
  for i := 0 to Length(Hdr) - 1 do Writeln(f, Hdr[i]);
  Writeln(f, PadRight('', 79, #$2500))
 end;
 procedure _ReportTestingEvent(ei: Integer; var HC: TCardinalDynArray);
 begin
  t := '_ctreport.' + hcRTE[ei].FN + '.txt';
  if     bMarker and ((FilePath <> '') or FileExists(t))
  or not bMarker and FileExists(SrcVolSpec.GUID + FilePath)
  or not bMarker and DirectoryExists(SrcVolSpec.GUID + FilePath) then
   try
    AssignFile(f, t);
    if (0 < Length(HC)) and FileExists(t) then Append(f) else _ReWrite(ei);
    if bMarker then begin
     if FilePath <> '' then
      FilePath := PadLap(' ' + Trim(FilePath) + ' ', 79, ':')
    end else
     if SrcVolSpec.ULab = '' then
      FilePath := SrcVolSpec.GUID + FilePath
     else
      FilePath := SrcVolSpec.ULab + FilePath;
    if (FilePath = '') or not HashCodesOfStrings(FilePath, HC) then
     WriteLn(f, FilePath)
   finally
    CloseFile(f)
   end
 end;
begin
 try
  _ReportTestingEvent(Int32(ETyp), hcRTE[Int32(ETyp)].HC)
 except on E: Exception do begin
  // File locked, expected ReWrite failure.
  if not hcRTE[Int32(ETyp)].Err then
   OutputMessage(mInternalErr,
      'The testing report file is locked, failure: ' + E.Message);
  hcRTE[Int32(ETyp)].Err := true
 end end
end;

procedure RemoveEmptyTestingReports;
var
 i: Integer; FN: String;
begin
 for i := 0 to Length(hcRTE) - 1 do begin
  FN := '_ctreport.' + hcRTE[i].FN + '.txt';
  if FileExists(FN) and (Length(hcRTE[i].HC) = 0) then DeleteFileNT32(FN)
 end
end;

{ CheckNameNewFile
  Checks the file name matches file filters to skip NTFS compression.
  Reports found files if it was specified to do.
}
var
 bReportFSCF: Boolean; MasksFSCF: TStringDynArray; hLibCnf: HMODULE = 0;
function CheckNameNewFile(FileName: String; var CurSkpCnt: Integer; NumCall: Byte = 1) : Boolean;
var
 str : String; i : Integer;
begin
 Result := true;
 case NumCall of
 0: begin
  str := ReadCfgValue('Optimization', 'SkipCompressionForFiles', '');
  bReportFSCF := ReadCfgValue('Optimization', 'ReportFoundSkipFiles', 'not') = 'yes';
  hLibCnf := LoadCloperF;
  if str <> '' then
   MasksFSCF := SplitString(str, '|');
  if bReportFSCF then
   OutputMessage(mLogNoDate,
      '<align>Scanning new files to exclude specified files from NFTS compression:',
      mcSub2Cat)
 end;
 1: for i := 0 to Length(MasksFSCF) - 1 do
     if MaskFits(MasksFSCF[i], ExtractFileName(FileName), hLibCnf) then begin
      Inc(CurSkpCnt);
      if bReportFSCF then
       OutputMessage(mLogNoDate, 'Skip #' +
        PadLeft(IntToStr(CurSkpCnt), 3, '0') + ':<align>' + FileName);
      if bReportFiles then begin
       ReportTestingEvent(teCprSkip, 'NEW FILES', true);
       ReportTestingEvent(teCprSkip, FileName);
      end;
      Result := false
     end;
 2: begin
  FreeLibrary(hLibCnf); hLibCnf := 0;
  if bReportFSCF then
   OutputMessage(mLogNoDtLP,
      '<align>Finished scan of new files to exclude from NTFS compression.',
      mcSub2Cat)
 end;
 end;
end;

initialization
 CfgFile := ExtractFilePath(ParamStr(0)) + 'cloper.cfg';
 LogFile := ChangeFileExt(ParamStr(0), '.log');
 LogMsgString := WriteMsgToLog;
 CheckNewFile := CheckNameNewFile;
 fReportTestingEvent := @ReportTestingEvent;
end.
