{ cloper_clon
  The unit of Cloning Optimizer (Cloper) tool.

  The file encapsulates functionality for cloning of source modified data to
  the target disk.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit cloper_clon;

interface
uses
 cloper_typs, cloper_conf, SysUtils, DskTools, Types, ConTools, cloper_echo;

type
 TWriteData = record
  ScTgBuf : TByteDynArray;
  Beg, Siz, Limit : UInt32;
 end;
var
 WD : TWriteData = (ScTgBuf : nil; Beg : 0; Siz : 0; Limit : 0);

var
 ScGeometry, TgGeometry: TDiskGeometry;
 SrcDrvInfo, TgtDrvInfo: TDriveInfo;

function CloneDisk(NumDskSrc, NumDskTgt : Integer) : Boolean;

implementation
uses
 Windows, cloper_funs, cloper_base, VolTools;

function WriteData(DskSpec: TDiskGeometry; hTgFil : THandle) : Boolean;
var
 b : Boolean;
begin
 if Length(WD.ScTgBuf) > 0 then begin
    Result := WriteSectors(hTgFil, WD.Beg, WD.Siz, @WD.ScTgBuf[0],
      DskSpec.BytesPerSector) > 0;
    SetLength(WD.ScTgBuf, 0)
 end else Result := false;
 WD.Beg := 0; WD.Siz := 0
end;

function FileSize(ScGeometry: TDiskGeometry; bExp: Boolean = true): UINT32;
begin
 Result := TCrtcProgress.AuxCI[1] * ScGeometry.BytesPerSector div 1048576;
 if bExp then
  Result := Result + Result div 3
end;

procedure CompareAndUpdate(var ScGeometry: TDiskGeometry;
                           hScFil, hTgFil : THandle; uisa : Int64; uisz : UInt32);
var
 ScBuf, TgBuf : TByteDynArray;
 SzScBuf, SzTgBuf : Cardinal;
 i, j, k, m, x, y : Integer;
begin
 SzScBuf := ReadSectors(hScFil, ScBuf, uisa, uisz, ScGeometry.BytesPerSector);
 SzTgBuf := ReadSectors(hTgFil, TgBuf, uisa, uisz, ScGeometry.BytesPerSector);
 if SzScBuf <> SzTgBuf then
  if PrintFailure(SrcVolSpec, 11) then exit;
 if SzScBuf = 0 then exit;

 i := 0; x := 0; y := 0;
 repeat
  TCrtcProgress.AuxCI[3] := TCrtcProgress.AuxCI[2];
  TCrtcProgress.AuxCI[6] := TCrtcProgress.AuxCI[5];
  for j := 0 to ScGeometry.BytesPerSector - 1 do begin
   k := i + j;
   if k > SzScBuf then break;
   with TCrtcProgress do begin
    if ScBuf[k] <> 0 then if AuxCI[3] = AuxCI[2] then Inc(AuxCI[2]);
    if TgBuf[k] <> 0 then if AuxCI[6] = AuxCI[5] then Inc(AuxCI[5])
   end;
   if ScBuf[k] <> TgBuf[k] then begin
    if 0 < WD.Siz then begin
     if(WD.Beg + WD.Siz <> uisa + x) then
      if not WriteData(ScGeometry, hTgFil) then
       TCrtcProgress.Message(mInternalErr, 'Write failed at sector ' +
                IntToStr(uisa + x) + ' ...');
     if WD.Siz + 1 >= WD.Limit then begin
      if not WriteData(ScGeometry, hTgFil) then
       TCrtcProgress.Message(mInternalErr, 'Write failed at sector ' +
            IntToStr(uisa + x) + ' ...')
     end
    end;
    with TCrtcProgress do begin
     Inc(AuxCI[1]);
     if AuxCI[3] < AuxCI[2] then Inc(AuxCI[4]);
     if AuxCI[6] < AuxCI[5] then Inc(AuxCI[7])
    end;

    if WD.Beg = 0 then WD.Beg := uisa + x;
    Inc(WD.Siz);
    y := Length(WD.ScTgBuf);
    SetLength(WD.ScTgBuf, y + ScGeometry.BytesPerSector);
    m := i;
    repeat
     WD.ScTgBuf[y + m - i] := ScBuf[m];
     Inc(m)
    until(m >= SzScBuf) or (m >= i + ScGeometry.BytesPerSector);
    break
   end
  end;
  i := i + ScGeometry.BytesPerSector; Inc(x)
 until i > SzScBuf;
 TCrtcProgress.Echo(uisa + i div ScGeometry.BytesPerSector,
    [FileSize(ScGeometry)  , FileSize(ScGeometry, false),
     TCrtcProgress.AuxCI[1], TCrtcProgress.AuxCI[4],
     TCrtcProgress.AuxCI[7], TCrtcProgress.AuxCI[2]])
end;

function CheckWaitResult(NumDskSrc, NumDskTgt: Integer; SleepSec: Integer = 60): Boolean;
var
 Hndl: THandle; d, s, t, c: Integer; b : Boolean;
 aGuid: array[0..MAX_PATH] of AnsiChar; sGuid: String;
begin
 Result := false; c := 0; b := true;
 if IoDeviceControl(Format(cfsPhysDrive, [NumDskTgt]), IOCTL_STORAGE_CHECK_VERIFY2) then
  repeat
   Sleep(1000); Inc(c);
   if b then
    b := not VerifyDisk(NumDskTgt)
   else begin
    Hndl := FindFirstVolume(@aGuid[0], MAX_PATH);
    if Hndl = INVALID_HANDLE_VALUE then exit;
    s := 0; t := 0;
    repeat
     SetString(sGuid, aGuid, MAX_PATH);
     sGuid := Copy(sGuid, 0, Pos(#0, sGuid) - 1);
     d := GetDevicePartitionNumberById(sGuid);
     if d = NumDskTgt then Inc(t) else if d = NumDskSrc then Inc(s)
    until not FindNextVolume(Hndl, @aGuid[0], MAX_PATH);
    FindVolumeClose(Hndl);
    Result := (0 < s) and (s = t)
   end;
   if Result then break;
   IoDeviceControl(Format(cfsPhysDrive, [NumDskTgt]), IOCTL_DISK_UPDATE_PROPERTIES);
  until SleepSec < c;

 if not Result then begin
  if c = 0 then
   OutputMessage(mInternalErr, 'Failed check of target storage after completion')
  else
   if b then
    OutputMessage(mInternalErr, 'Failed to check target geometry after completion')
   else
    OutputMessage(mInternalErr, 'Failed to find matching partitions after completion');
  OutputMessage(mSystemErr, 'Assigning labels to volumes can fail')
 end
end;

function CloneDisk(NumDskSrc, NumDskTgt : Integer) : Boolean;
var
 uicr, uisz : Int64;
 bAllocated, bSetOnline, bWinXP : Boolean;
 mbrs, mbrt: TByteDynArray;
 i : Integer;
 hScFil, hTgFil : THandle;
 arg, StrBSN, StrScDkId, StrTgDkId: String;
label
 exi;
begin
 Inc(SubTasksNum);
 Result := false;
 Assert(DisksStyle in [1, 2], 'The data of task is not initialized, aborted...');
 if DisksStyle = 1 then StrBSN := 'Boot Sector' else StrBSN := 'Main Boot Record';

 if 0 < SrcVolSpec.ErrorCode + TgtVolSpec.ErrorCode then
   exit;
 if not GetGeometry(NumDskSrc, ScGeometry) then
  if PrintFailure(SrcVolSpec, 4) then
   exit;
 bAllocated := PartInfoAction(piaChkAlloc, NumDskTgt);
 if bAllocated then begin
  if not GetGeometry(NumDskTgt, TgGeometry) then
   if PrintFailure(TgtVolSpec, 5) then
    exit;
  if not CompareGeometries(ScGeometry, TgGeometry)
  or not CompareDrivesInfo(NumDskSrc, NumDskTgt) then
   if PrintFailure(SrcVolSpec, 13) then
    exit
 end;
                    // Optimal buffer size is 262144 bytes,
                    // Reference http://zabkat.com/blog/buffered-disk-access.htm
 WD.Limit := 10240; // Default "read by 5 Mb", max read\write is 10 Mb.
 try
  if ReadCfgValue('Cloning', 'BufferSizeKB', arg, '0') then begin
   if(1 < StrToInt64(arg))and(StrToInt64(arg) <= 20480)then
    WD.Limit := Round(StrToInt64(arg) * 1024 / ScGeometry.BytesPerSector)
  end
 except
  WD.Limit := 10240
 end;
 // Align with usual cluster size (4Kb):
 WD.Limit := (WD.Limit div 8 + 1) * 8;
 bSetOnline := ReadCfgValue('Cloning', 'ActivateTargetDisk', 'yes') = 'yes';
 bWinXP := OSMajorMinorVersion < 60;

 // Get handles w/o share:
 if not GetFileHandle(NumDskSrc, GENERIC_READ, 0, hScFil) then
  if PrintFailure(SrcVolSpec, 9) then goto exi;
 if not GetFileHandle(NumDskTgt,
                      GENERIC_READ or GENERIC_WRITE,
                      FILE_SHARE_DELETE or FILE_SHARE_READ or FILE_SHARE_WRITE,
                      hTgFil) then
  if PrintFailure(TgtVolSpec, 10) then exit;

 if 0 < ReadSectors(hScFil, mbrs, 0, 1, 1024) then
  if 0 < ReadSectors(hTgFil, mbrt, 0, 1, 1024) then begin
   GetSetDiskSignInSectorArray(true, StrScDkId, mbrs, DisksStyle);
   GetSetDiskSignInSectorArray(true, StrTgDkId, mbrt, DisksStyle)
  end else begin
   if PrintFailure(TgtVolSpec, 5) then exit
 end else
  if PrintFailure(TgtVolSpec, 4) then exit;

 uisz := ScGeometry.Cylinders * ScGeometry.TracksPerCylinder *
    ScGeometry.SectorsPerTrack;

 TCrtcProgress.Initialize('Transfering source data to target disk',
    'disk cloning',
    ['Expected snapshot growth',
     'Total size of written sectors',
     'Number of different sectors',
     'Different source non-zero sectors',
     'Different target non-zero sectors',
     'Source sectors with non-zero data'],
     ['Mbytes ' + #$B1 + '25%', 'Mbytes', '', '', '', ''], uisz, 0,
     'The cloning of disk #' + IntToStr(NumDskSrc) +
    ' to disk #' + IntToStr(NumDskTgt));
 uicr := WD.Limit * ScGeometry.BytesPerSector div 1048576;
 OutputMessage(mEcoNoDtHP, '<align>The size of buffer is ' +
    IntToStr(uicr) + ' MB, expected RAM usage ' +
    IntToStr(3 * uicr - uicr div 2) + #177 + IntToStr(uicr div 2) + ' MB');

 if bSetOnline then begin
  CloseHandle(hScFil);
  CloseHandle(hTgFil);

  if bAllocated then
   if DestroyDiskSystem(NumDskTgt) then
    OutputMessage(mEcoNoDtHP,
     '<align>Deleted file system of target disk to get full write access')
   else if PrintFailure(15) then goto exi;

  if bWinXP then // WinXP: change source disk ID to avoid signature collision
   PartInfoAction(piaSetRandomID, NumDskSrc);

  if not GetFileHandle(NumDskSrc, GENERIC_READ, 0, hScFil) then
   if PrintFailure(SrcVolSpec, 9) then goto exi;
  if not GetFileHandle(NumDskTgt,
                       GENERIC_READ or GENERIC_WRITE,
                       FILE_SHARE_DELETE or FILE_SHARE_READ or FILE_SHARE_WRITE,
                       hTgFil) then
   if PrintFailure(TgtVolSpec, 10) then goto exi;
 end else begin
  if DestroyDiskSystem(0, hTgFil) then
   OutputMessage(mEcoNoDtHP,
     '<align>Deleted file system of target disk to get full write access')
  else if PrintFailure(15) then goto exi
 end;
 OutputMessage(mEcoNoDtHP,
    '<align>Started copying of modified data to target disk');

 uicr := 1; // Skip cloning of 1st sector until completion.
 repeat
  CompareAndUpdate(ScGeometry, hScFil, hTgFil, uicr, WD.Limit);
  if 0 < SrcVolSpec.ErrorCode then goto exi;
  uicr := uicr + WD.Limit
 until uicr >= uisz;

 if not WriteData(ScGeometry, hTgFil) then if WD.Siz > 0 then
  OutputMessage(mLowPriErr,
    'Final write failed at sector ' + IntToStr(WD.Beg) + ' ...');
 CloseHandle(hScFil);

 if bWinXP then begin
  GetSetDiskSignInSectorArray(false, StrTgDkId, mbrs, DisksStyle);

  if 0 = WriteSectors(hTgFil, 0, 1, @mbrs[0], 1024) then
   if PrintFailure(16) then goto exi;
  if DisksStyle = 1 then
   PartInfoAction(piaSetID, NumDskTgt, StrTgDkId, DisksStyle, hTgFil)
  else begin
   if not IoDeviceControl(hTgFil, IOCTL_DISK_UPDATE_PROPERTIES) then
    OutputMessage(mWarning, 'Failed to update target disk properties after cloning completion');
  end;
  Sleep(250);
 end else begin
  if 0 < WriteSectors(hTgFil, 0, 1, @mbrs[0], 1024) then
   OutputMessage(mLogNoDate, '<align>Successfully restored ' + StrBSN + ' of target disk')
  else if PrintFailure(16) then goto exi;
  if not IoDeviceControl(hTgFil, IOCTL_DISK_UPDATE_PROPERTIES) then
   OutputMessage(mWarning, 'Failed to update target disk properties after cloning completion');
 end;
 CloseHandle(hTgFil);
 ResetVolume(NumDskTgt, false);

 if bWinXP then begin
  if PartInfoAction(piaSetID, NumDskTgt, StrScDkId, DisksStyle) then
   OutputMessage(mLogNoDate,
     '<align>The disk signature of source disk was set to target value');
  ResetVolume(NumDskTgt, false);

  if bSetOnline then
   // Return source disk ID back to original value (set offline):
   if PartInfoAction(piaSetID, NumDskSrc, StrScDkId, DisksStyle) then
    OutputMessage(mLogNoDate,
      '<align>The source disk was successfully set offline')
   else
    OutputMessage(mWarning,
      'Failed attempt to restore signature of source disk. ' +
      'In the case if it contains installed Windows, the OS may become unbootable '
      + 'with error oxc000000e. ' +
      'To fix it restore manually the original ID value "' +
       StrScDkId + '" or fix BCD with external tool')
 end else if bSetOnline then
  if SwapDisksOffLineState(NumDskSrc, NumDskTgt, StrScDkId) then
   OutputMessage(mLogNoDate,
     '<align>The source disk was successfully set offline')
  else
   OutputMessage(mInternalErr,
   'The disks have not signature''s conflict or the specified signature differs');
 Result := true;
exi:
 TCrtcProgress.Summary('DISK CLONING SUMMARY:',
          [FileSize(ScGeometry)  , FileSize(ScGeometry, false),
           TCrtcProgress.AuxCI[1], TCrtcProgress.AuxCI[4],
           TCrtcProgress.AuxCI[7], TCrtcProgress.AuxCI[2]], Result)
end;

end.
