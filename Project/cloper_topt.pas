{ cloper_topt
  The unit of Cloning Optimizer (Cloper) tool.

  The file contains functionality:
  1. The main routine to perform calls of optimization task for disk;
  2. Preliminary cleanup of processed volume;
  3. Consolidation (defragmentation) & compression of new data;
  4. Free space patching routine of source volume by target data.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit cloper_topt;

interface

{ CleanupsAndOptimizations
  Performs optimizations of specified disk.
}
function CleanupsAndOptimizations(NumDskSrc, NumDskTgt, NumPart: Integer): Boolean;

implementation
uses
 Types, SysUtils, StrUtils, Windows, VolFiles, Defragger,
 cloper_conf, cloper_base, cloper_funs, cloper_typs, cloper_zero, cloper_echo,
 cloper_oper, cloper_byte, cloper_into, cloper_scrn,
 DskTools, VolTools, StrTools, ConTools;

var
 bMoveNewData, bOnlyDefragmentNewData,
 bMatchBlocks,
 bMoveToBlocks, bPatchFreeSpace : Boolean;

function CleanupSourceVolume(NumPart : Integer; bTerminate : Boolean = false): Boolean;
var
 i, j : Integer;
 fldval, fndmsk : String;
 fndits : TStringDynArray;
 dtbeg : TDateTime;
 babsent : Boolean;
begin
 Result := false;
 try
  dtbeg := Now;
  OutputMessage(mLogHighPri, 'Started cleanup of volume #' +
      IntToStr(SrcVolSpec.VolumeNumber), mcSubCat);
  OutputMessage(mLogNoDate, '<align>Checking & deleting directories...', mcSub2Cat);

  // Check lost zeroing files & folder:
  DeleteDirectory(SrcVolSpec.Root + '$CZFSTMP.$$$');

  babsent := true;
  i := 1;
  fldval := ReadCfgValue('VolumeCleanup', 'Folders' + IntToStr(i), '');
  while fldval <> '' do begin
   fndits := FindPathsOnDisk(SrcVolSpec.Root + fldval);

   if fndits[0] <> '' then
    for j := 0 to Length(fndits) - 1 do begin
     babsent := false;

     case DeleteDirectory(fndits[j]) of
     drDone:
      OutputMessage(mLogNoDtLP, 'Deleted:<align>' +
        ExcludeTrailingPathDelimiter(ReplaceStr(LowerCase(fndits[j]),
        LowerCase(SrcVolSpec.Root), '[PART' + IntToStr(NumPart) + ']\')));
     drError:
      if bTerminate then exit else
       OutputMessage(mInternalErr, 'Failed deletion of "' +
        ExcludeTrailingPathDelimiter(ReplaceStr(LowerCase(fndits[j]),
         LowerCase(SrcVolSpec.Root), '[PART' + IntToStr(NumPart) + ']\')) + '"');
     end;
    end;
   Inc(i);
   fldval := ReadCfgValue('VolumeCleanup', 'Folders' + IntToStr(i), '');
  end;

  if babsent then
   OutputMessage(mLogNoDtLP,
          '<align>The directories fitting deletion conditions weren''t found');

  OutputMessage(mLogNoDate,
        '<align>Checking & removing files & subfolders in directories...', mcSub2Cat);
  babsent := true;
  i := 1;
  fldval := ReadCfgValue('VolumeCleanup', 'Files' + IntToStr(i), '');
  while fldval <> '' do begin
   fndmsk := ExtractFileName(fldval);
   fndits := FindPathsToFiles(SrcVolSpec.Root + fldval);

   if fndits[0] <> '' then
    for j := 0 to Length(fndits) - 1 do begin
     babsent := false;
     case CleanUpDirectory(fndits[j], fndmsk, bReportFiles) of
     drDone:
       OutputMessage(mLogNoDtLP, 'Cleaned:<align>' +
         ReplaceStr(LowerCase(fndits[j]), LowerCase(SrcVolSpec.Root),
         '[PART' + IntToStr(NumPart) + ']\') + '<' + fndmsk + '>');
     drError:
      if bTerminate then exit else
       OutputMessage(mWarning, 'Not deleted part of items in "' +
         ReplaceStr(LowerCase(fndits[j]), LowerCase(SrcVolSpec.Root),
         '[PART' + IntToStr(NumPart) + ']\') + '<' + fndmsk + '>"');
     end
   end;
   Inc(i);
   fldval := ReadCfgValue('VolumeCleanup', 'Files' + IntToStr(i), '')
  end;
  if babsent then
   OutputMessage(mLogNoDtLP,
          '<align>The files fitting deletion conditions weren''t found');

   OutputMessage(mLogHighPri, 'The checks of volume #' +
    IntToStr(SrcVolSpec.VolumeNumber) +
    ' to do its cleanup was completed in ' +
    SSeconds(Now, dtbeg) + ' sec', mcSubCat);
   Result := true
 except on E:Exception do begin
  OutputMessage(mFailure, 'Exception on attempt to cleanup data');
  OutputMessage(mInternalErr, E.Message);
 end end;
end;

{ CompressDefragmentNewFiles
  Performs NTFS compression of big new files, defragments & consolidates them.
}
function CompressDefragmentNewFiles(VolHdl: THandle; VolNum: Integer;
                                    var Files: TStringDynArray;
                                    bDoNTFSC: TBooleanDynArray;
                                    NewFlsSzs: TCardinalDynArray;
                                    bOnlyDefr: Boolean): Boolean;
var
 i, mtlres, csuc : Integer;
 BeginLcn, EndLcn, Lcn : ULONG64;
 Fragments, RealClusters, RealClusterU, SectCnt, SectCur : DWORD; bCompr: Boolean;
 CountCprsS, CountFgmtS, CountCprsF, CountFgmtF1, CountFgmtF2, CountFgmtF3: Cardinal;

 function GetCrtCntArr : TExtendedDynArray;
 begin
  if bCompressData then
   if 0 < ExpSize then
    Result := CrtcArr([CountFgmtS, CountCprsS, CountFgmtF1, CountCprsF,
                      CountFgmtF2, CountFgmtF3, ExpSize * CluSzToBt div 1048576])
   else
    Result := CrtcArr([CountFgmtS, CountCprsS, CountFgmtF1, CountCprsF,
                      CountFgmtF2, CountFgmtF3])
  else
   Result := CrtcArr([CountFgmtS, CountFgmtF1, CountFgmtF2, CountFgmtF3])
 end;
begin
 Inc(SubTasksNum);
 Result := false;
 try
  if VolHdl = INVALID_HANDLE_VALUE then
   if PrintFailure(SrcVolSpec, 17) then
    exit;
  SectCur := 0;
  for i := 0 to Length(Files) - 1 do begin
   NewFlsSzs[i] := NewFlsSzs[i] div SrcVolSpec.BytesPerSector;
   SectCnt := SectCnt + NewFlsSzs[i]
  end;
  TCrtcProgress.Initialize(
        'compression & defragmentation of new data', 'new data scan',
        ['Defragmentation had failures for',
        'Count of defragger failures',
        'Number of failed fragmented files'], [
        'files', '', ''], SectCnt, 0, '', Byte(mcSubCat));
  if Length(Files) = 0 then begin
   OutputMessage(mLogNoDate, cMsgNoData); Result := true; exit
  end;
  if bOnlyDefr then
   TCrtcProgress.InsertSummaryString(0, 'Defragmented', 'files')
  else
   TCrtcProgress.InsertSummaryString(0, 'Consolidated', 'files');
  if bCompressData then begin
   TCrtcProgress.InsertSummaryString(1, 'Compressed', 'files');
   TCrtcProgress.InsertSummaryString(3, 'Compression failed for', 'files');
   if 0 < ExpSize then
    TCrtcProgress.InsertSummaryString(-1, 'Upper limit for snapshot growth', 'Mbytes')
  end;
  if 0 < Length(Files) then
   if bCompressData or not bOnlyDefr then
   begin // Sizes modification to have averaged change of progress indicator.
    SectCur := SectCnt div Length(Files);
    for i := 0 to Length(Files) - 1 do
     NewFlsSzs[i] := (SectCur + NewFlsSzs[i]) div 2
   end;
  CountCprsS := 0; CountCprsF  := 0; bCompr := false;
  CountFgmtS := 0; CountFgmtF1 := 0; CountFgmtF2 := 0; CountFgmtF3 := 0;
  SectCur := 0; BeginLcn := 0; EndLcn := 0;
  SetPriorityClass(GetCurrentProcess(), PRIORITY_CLASS);
  for i := 0 to Length(Files) - 1 do begin
   Lcn := 0;
   if bDoNTFSC[i] then begin
    if bCompressData and (0 < ExpSize) then begin
     AnalyzeFile(SrcVolSpec.GUID + Files[i], Fragments, RealClusterU, Lcn);
     bCompr := false
    end;
    case NtfsSetCompression(SrcVolSpec.GUID + Files[i], COMPRESSION_FORMAT) of
    2: begin Inc(CountCprsS); bCompr := bCompressData; Sleep(1) end;
    3: begin
       OutputMessage(mLowPriErr, 'Compression failed for "' + Files[i] + '"');
       Inc(CountCprsF);
       if bReportFiles then ReportTestingEvent(teCprFail, Files[i])
    end;
    4: begin
       OutputMessage(mLowPriErr, 'Couldn''t get handler to compress "' + Files[i] + '"');
       Inc(CountCprsF);
       if bReportFiles then ReportTestingEvent(teHdlFail, Files[i])
    end;
    end
   end;

   if AnalyzeFile(SrcVolSpec.GUID + Files[i], Fragments, RealClusters, Lcn) and
     (0 < RealClusters) and (0 < Fragments) and
     ((1 < Fragments) or not bOnlyDefr) then
   begin
    mtlres := 0; csuc := 0;
    repeat
     if EndLcn - BeginLcn < RealClusters + 2 then
     begin
      // For data consolidation it tries to find blocks at the start of volume:
      if csuc = 0 then
       FindFreeBlock(VolHdl, 0, RealClusters, BeginLcn, EndLcn)
      else
       FindFreeBlock(VolHdl, Lcn + csuc * RealClusters, RealClusters,
         BeginLcn, EndLcn)
     end;

     mtlres := MoveToLcn(VolHdl, BeginLcn, SrcVolSpec.GUID + Files[i],
       RealClusters, Lcn);
     if mtlres = 0 then begin
      BeginLcn := BeginLcn + RealClusters; Inc(CountFgmtS); csuc := 4
     end else begin
      OutputMessage(mLowPriErr, 'Defragger failure for "' + Files[i] + '"');
      if csuc = 0 then Inc(CountFgmtF1); Inc(CountFgmtF2);
      EndLcn := 0; BeginLcn := 0; Sleep(15); Inc(csuc)
     end
    until csuc = 4;
    if mtlres <> 0 then begin
     Inc(CountFgmtF3);
     OutputMessage(mInternalErr, 'Defragmentation failed for "' + Files[i] + '"');
     if bReportFiles then
      ReportTestingEvent(teDfrFail, Files[i])
    end
   end;
   if bCompr and (0 < ExpSize) then begin
    ExpSize := ExpSize + RealClusters - RealClusterU; bCompr := false
   end;
   SectCur := SectCur + NewFlsSzs[i];
   TCrtcProgress.Echo(SectCur, GetCrtCntArr)
  end;
  TCrtcProgress.Summary('NEW DATA OPTIMIZATION SUMMARY:', GetCrtCntArr);
  Result := true
 except
  on E : Exception do begin
   TCrtcProgress.Finalize;
   OutputMessage(mInternalErr, 'Failed with exception: ' + E.Message);
   OutputSysError(true)
  end;
 end;
 SetPriorityClass(GetCurrentProcess(), NORMAL_PRIORITY_CLASS);
 SetLength(Files, 0); SetLength(bDoNTFSC, 0); SetLength(NewFlsSzs, 0)
end;

{ PatchFreeSpace
  Collates free space of source disk to the data of target. Creates patch files
  with target data in source disk in front of target data.
}
function PatchFreeSpace(hScVol: THandle): Boolean;
const
 // Overrides global variable (patching only free space after move to blocks)
 bMoveToBlocks = false;
var
 hTgVol : THandle;
 CrToKb : Integer;
 BegLcn, EndLcn, LcnBeg, LcnEnd: ULONG64;
 cFrSeg, MemFree, cUsSeg, cPtchF, MemPtch, MemFail, MemDrpd, cDrpdF: Cardinal;
begin
 Inc(SubTasksNum);
 Result := false;
 if hScVol = INVALID_HANDLE_VALUE then
  if PrintFailure(SrcVolSpec, 17) then
   exit;
 hTgVol := CreateFile(PWChar(ExcludeTrailingBackslash(TgtVolSpec.GUID)),
      GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
      OPEN_EXISTING, FILE_FLAG_NO_BUFFERING or FILE_FLAG_RANDOM_ACCESS, 0);
 if hTgVol = INVALID_HANDLE_VALUE then
  if PrintFailure(TgtVolSpec, 20) then
   exit;
 LockUnLock(hTgVol, true);

 CrToKb := SectorsPerCluster * SrcVolSpec.BytesPerSector div 1024;
 try
  TCrtcProgress.Initialize(
        'patching free space by target data',
        'patching',
        ['Number of free source blocks', 'Size of scanned source free blocks',
         'Number of non-zero subblocks', 'Number of patch files', 'Patched', 'Failed'],
        ['', 'Kbytes', '', '', 'Kbytes', 'Kbytes'],
        Defragger.MaxLcn, 0, '', Byte(mcSubCat));
  SetPriorityClass(GetCurrentProcess(), PRIORITY_CLASS);

  if bMoveToBlocks then
   OutputMessage(mLogNoDate,
     '<align>All target blocks are filled by source files, patching free space');

  InitializeFindPatches;
  cFrSeg := 0; MemFree := 0; cUsSeg := 0; cPtchF := 0; MemPtch := 0; MemFail := 0;
  BegLcn := 2; EndLcn := 2; LcnBeg := 0; LcnEnd := 0;
  while 0 < GetNextFreeBlock(hScVol, BegLcn, EndLcn) do begin
   if bMoveToBlocks and ((EndLcn < LcnBeg) or (LcnEnd < EndLcn)) then begin
    LcnBeg := BegLcn - 1; LcnEnd := LcnBeg;
    while (0 < GetNextFreeBlock(hTgVol, LcnBeg, LcnEnd)) and (LcnBeg <= EndLcn) do
    begin
     if LcnEnd <= EndLcn then
      cUsSeg := cUsSeg + FindTargetDataAndPatch(MemPtch, MemFail, cPtchF,
                                                hScVol, LcnBeg, LcnEnd, hTgVol)
     else
      cUsSeg := cUsSeg + FindTargetDataAndPatch(MemPtch, MemFail, cPtchF,
                                                hScVol, LcnBeg, EndLcn, hTgVol);
     TCrtcProgress.Echo(EndLcn,
     [cFrSeg, MemFree * CrToKb, cUsSeg, cPtchF, MemPtch * CrToKb, MemFail * CrToKb])
    end
   end else
    cUsSeg := cUsSeg + FindTargetDataAndPatch(MemPtch, MemFail, cPtchF,
                                              hScVol, BegLcn, EndLcn, hTgVol);

   Inc(cFrSeg); Inc(MemFree, EndLcn - BegLcn + 1);
   TCrtcProgress.Echo(EndLcn,
   [cFrSeg, MemFree * CrToKb, cUsSeg, cPtchF, MemPtch * CrToKb, MemFail * CrToKb])
  end;

  OutputMessage(mEcoHighPri, 'Started check of the patch files weren''t replaced...');
  VerifyPatches(MemPtch, MemFail, cPtchF, MemDrpd, cDrpdF);
  if cDrpdF = 0 then
   OutputMessage(mEcoNoDtHP, '<align>Pass')
  else
   OutputMessage(mEcoNoDtHP,
          Format('<align>Dropped %d patch files (%d KB) of volatile system areas',
          [cDrpdF, MemDrpd * CrToKb]));

  TCrtcProgress.Summary('PATCHING FREE SPACE SUMMARY:',
   [cFrSeg, MemFree * CrToKb, cUsSeg, cPtchF, MemPtch * CrToKb, MemFail * CrToKb]);
  Result := true
 except
  on E : Exception do begin
   TCrtcProgress.Finalize;
   OutputMessage(mInternalErr, 'Failed with exception: ' + E.Message);
   OutputSysError(true)
  end
 end;
 SetPriorityClass(GetCurrentProcess(), NORMAL_PRIORITY_CLASS);
 LockUnLock(hTgVol, false);
 CloseHandle(hTgVol)
end;

function PerformVolumeOptimization(NumVolSrc, NumVolTgt: Integer;
                                              var bSkip: Boolean): Boolean;
label
 fin;
var
 VolHdl: THandle;
 OldFiles, DelFiles, NewFiles: TStringDynArray;
 bDoNTFSC: TBooleanDynArray; NewFlsSzs: TCardinalDynArray;
 dwClusterSizeInBytes: DWORD;
 dtbv: TDateTime;
begin
 Result := false;

 dtbv := Now;
 if not GetVolumeClusterSize(NumVolSrc, dwClusterSizeInBytes) then begin
  OutputMessage(mFailure, 'Couldn''t read cluster size of volume');
  exit
 end else begin
  SectorsPerCluster := Byte(dwClusterSizeInBytes div SrcVolSpec.BytesPerSector);
  if SectorsPerCluster = 0 then begin
   OutputMessage(mFailure, 'Couldn''t read cluster size of volume');
   exit
  end;
  ReadStep := GetReadStep
 end;

 if bMatchBlocks or bMoveToBlocks or bMoveNewData then begin
  OutputMessage(mLogHighPri, 'Started data acquisition for volumes #%d and #%d',
                             [NumVolSrc, NumVolTgt], mcSubCat);
  SetLength(OldFiles, 0); SetLength(DelFiles, 0);
  SetLength(NewFiles, 0); SetLength(bDoNTFSC, 0); SetLength(NewFlsSzs, 0);

  if bUseUsnQueries then begin
   if not GetFilesByUsnQuery(NumVolSrc, NumVolTgt, OldFiles, DelFiles, NewFiles,
                             bDoNTFSC, NewFlsSzs, dwClusterSizeInBytes) then
    exit
  end else
   if not GetFilesByScanning(NumVolSrc, NumVolTgt, OldFiles, DelFiles, NewFiles,
                             bDoNTFSC, NewFlsSzs, dwClusterSizeInBytes) then
    exit;

  OutputMessage(mLogHighPri, 'The data of volumes #%d and #%d was acquired in %s sec',
                [NumVolSrc, NumVolTgt, SSeconds(Now, dtbv)], mcSubCat)
 end;
 bSkip := false;
 case InitVolumeData(VolHdl, SrcVolSpec.GUID) of
 0: if PrintFailure(SrcVolSpec, 17) then exit;
 1: begin
   ResetStorageBus(SrcVolSpec.DeviceNumber); // likely damaged, m/b will recover
   case InitVolumeData(VolHdl, SrcVolSpec.GUID) of
   0: if PrintFailure(SrcVolSpec, 17) then exit;
   1: begin
    OutputMessage(mWarning, 'The data of source volume #%d doesn''t match it''s file map, skipped',
                 [NumVolSrc]);
    Result := true; bSkip := true; exit
   end end;
 end end;
 if IsDebuggingRun then
  ConsoleWrite('== >>  Passed initialization of ' + SrcVolSpec.GUID)
 else
  LockUnLock(VolHdl, true);

 if bMatchBlocks or bMoveToBlocks then
  if not OptimizePersistentData(bMatchBlocks, bMoveToBlocks, VolHdl,
                                NumVolSrc, NumVolTgt,
                                DelFiles, OldFiles, NewFiles,
                                bDoNTFSC, NewFlsSzs) then
   goto fin;

 if bMoveNewData then
  if not CompressDefragmentNewFiles(VolHdl, NumVolSrc, NewFiles,
                                    bDoNTFSC, NewFlsSzs, bOnlyDefragmentNewData) then
   goto fin;

 if bPatchFreeSpace then
  if not PatchFreeSpace(VolHdl) then
   goto fin;

 Result := true;
fin:
 if IsDebuggingRun then
  ConsoleWrite('<< ==  Passed task for ' + SrcVolSpec.GUID)
 else
  LockUnLock(VolHdl, true);
 CloseHandle(VolHdl); ResetVolume(SrcVolSpec.GUID)
end;

{ CleanupsAndOptimizations
  Performs optimizations of specified disk.
}
function CleanupsAndOptimizations(NumDskSrc, NumDskTgt, NumPart: Integer): Boolean;
var
 dtbd : TDateTime; bSkip, bAsgn: Boolean; ETyp: TTstEvents;

 procedure IniBeforeVolTask;
 begin
  ModifyCaption(true);
  if bAsgn then ShiftLabelsToValidPartition(NumPart);
  SrcVolSpec.ULab := SrcLabels[NumPart - 1].ULab;
  if bMatchBlocks then Inc(SubTasksCnt); if bMoveToBlocks   then Inc(SubTasksCnt);
  if bMoveNewData then Inc(SubTasksCnt); if bPatchFreeSpace then Inc(SubTasksCnt);
  if bMatchBlocks then Inc(SubTasksCnt); // <- Search & move (2)
  Inc(SubTasksCnt)
 end;
begin
 Result := false;
 dtbd := Now;
 ReadOptimizationCfgValues(bMoveNewData, bOnlyDefragmentNewData,
                           bMatchBlocks, bMoveToBlocks,
                           bPatchFreeSpace, NoNtfsCext, MemoryUseLimit);
 OutputMessage(mLogHighPri, 'Started cleanup and optimization of source disk #%d',
               [NumDskSrc], mcCat);

 if IniDiskLabels(NumDskSrc, NumDskTgt, dlReadDrop) then
  OutputMessage(mEcoNoDtLP, '<align>Removed volume labels on both disks, to be restored after completion');
 bAsgn := AssignLabelsLocally(NumPart);
 ReportDelayedLogMsgs;
 ReadTestingCfgValues(NumPart);

 // Cleanup testing reports if they exist:
 for ETyp := teHdlFail to teVltDmgd do ReportTestingEvent(ETyp, '', true);

 while 0 < NumPart do begin
  // Read info of source & target volumes, mostly to check their usable state:
  if not GetVolSpecs(SrcVolSpec) then if PrintFailure(SrcVolSpec, 4) then exit;
  if not GetVolSpecs(TgtVolSpec) then if PrintFailure(TgtVolSpec, 5) then exit;
  if SrcVolSpec.TotalNumOfClusters <> TgtVolSpec.TotalNumOfClusters then
   if PrintFailure(SrcVolSpec, 6) then exit;
  if SrcVolSpec.SectorsPerCluster <> TgtVolSpec.SectorsPerCluster then
   if PrintFailure(SrcVolSpec, 7) then exit;
  if SrcVolSpec.BytesPerSector <> TgtVolSpec.BytesPerSector then
   if PrintFailure(SrcVolSpec, 8) then exit;
  if (SrcVolSpec.DeviceNumber = TgtVolSpec.DeviceNumber)
  and(SrcVolSpec.PartitionNumber = TgtVolSpec.PartitionNumber) then
   if PrintFailure(SrcVolSpec, 3) then exit;
  if 0 < SrcVolSpec.ErrorCode + TgtVolSpec.ErrorCode then exit;

  with SrcVolSpec do
   if UINT64(MinVolumeSize) < UINT64(TotalNumOfClusters) * SectorsPerCluster *
          BytesPerSector div 1048576 then
   begin
    IniBeforeVolTask;

    // Delete files & folders to minimize size of target file with disk image:
    if not CleanupSourceVolume(NumPart) then
     exit;

    // Compress & defragment old & new files of source volume:
    bSkip := false;
    if bMoveNewData or bMoveToBlocks or bMatchBlocks or bPatchFreeSpace then
     if not PerformVolumeOptimization(SrcVolSpec.VolumeNumber,
                                      TgtVolSpec.VolumeNumber, bSkip) then
      exit;

    if bSkip then Inc(SubTasksNum) else
     if not FillByZeroes then // Fill free space of source volume by zero bytes:
      exit
   end else
    OutputMessage(mLogNoDate, InfoToLog(1, [NumPart, MinVolumeSize]));

  // Get numbers of next volumes if disks have several active partitions:
  NumPart := GetNextVolumeNumbers(NumPart + 1, [NumDskSrc, NumDskTgt])
 end;
 OutputMessage(mLogHighPri, 'The disk #%d was cleaned and optimized in %s sec',
              [NumDskSrc, SSeconds(Now, dtbd)], mcCat);
 Result := true
end;

end.
