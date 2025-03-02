{ cloper_funs
  The unit of Cloning Optimizer (Cloper) tool.

  The file encapsulates uncategorized top level functionality for calls in tool.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit cloper_funs;

interface
uses
 cloper_typs, cloper_base, Windows, StrUtils, Types, Classes;
type
 TdlAction = (dlRead = 1, dlReadDrop = 2, dlSet = 3);

function GetVolSpecs(var DS : TVolSpecs): Boolean;

function LockUnLock(hFile: THandle; bLock: Boolean) : Boolean;

function ResetVolume(SID : String) : Boolean; overload;
function ResetVolume(VolNum : Integer; bVol : Boolean = true) : Boolean; overload;

function SwapDisksOffLineState(NumDskSrc, NumDskTgt: Integer; StrDkId: String): Boolean;

function DiskLabels(DiskNum: Cardinal;
                    var Labels: TVolLabels;
                    dlAction: TdlAction = dlRead;
                    TgtVols: TVolLabels = nil;
                    TgtUpdt: Boolean = false) : Boolean;
function IniDiskLabels(DskNumS, DskNumT: Cardinal;
                       dlAction: TdlAction = dlRead): Boolean;

function AssignLabelsLocally(PartNum: Integer): Boolean;
procedure ShiftLabelsToValidPartition(CurPart: Integer);

function CreateNewSystemFile(var FB0F: TFB0File; FileName: String = '';
                                                 Sever: Boolean = true): Boolean;

function SearchResolveSignatureCollision(Serials: String): Boolean;

procedure ReportDataModified(Stage, FileName: String; EvType: TTstEvents = teDatChng);
procedure ReportTestingEventF(ETyp: TTstEvents; FilePath: String);

procedure ReportDelayedLogMsgs;

implementation
uses
 SysUtils, cloper_echo, cloper_into, cloper_scrn, cloper_conf,
 DskTools, VolTools, ConTools;

{ GetDiskSpecs
  Writes information of volume to TVolSpecs variable.
}
function GetVolSpecs(var DS : TVolSpecs): Boolean;
 procedure GetPhysicalDisk(Partition: String);
 type
  _STORAGE_DEVICE_NUMBER = record
   DeviceType: ULONG;
   DeviceNumber: ULONG;
   PartitionNumber: ULONG;
  end;
 const
  IOCTL_STORAGE_GET_DEVICE_NUMBER = $2D1080;
 var
  F: THandle;
  Info: _STORAGE_DEVICE_NUMBER;
  Dummy: DWORD;
 begin
  F := CreateFile(PChar(ExcludeTrailingPathDelimiter(Partition)), 0,
      FILE_SHARE_DELETE or FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
      OPEN_EXISTING, 0, 0);
  Win32Check(F <> INVALID_HANDLE_VALUE);
  try
   Win32Check(DeviceIoControl(F, IOCTL_STORAGE_GET_DEVICE_NUMBER, nil, 0,
        @Info, SizeOf(Info), Dummy, nil));
   DS.DeviceNumber := Info.DeviceNumber;
   DS.PartitionNumber := Info.PartitionNumber;
   DS.DeviceType := Info.DeviceType;
  finally
   CloseHandle(F);
  end;
 end;
begin
// https://msdn.microsoft.com/en-us/library/windows/desktop/aa364975%28v=vs.85%29.aspx
 Result := false;
 if DS.VolumeNumber < 0 then begin
  ZeroMemory(@DS, sizeof(TVolSpecs)); DS.VolumeNumber := -1; exit
 end;

 DS.Root := '\\?\GLOBALROOT\Device\HarddiskVolume' + IntToStr(DS.VolumeNumber) + '\';
 DS.GUID := GetVolumeGuid(DS.Root);
 DS.RtGUID := '\\?\GLOBALROOT\Device\' + ExtractVolumeIdString(DS.GUID) + '\';
 Result := GetDiskFreeSpace(PChar(DS.GUID),
                            DS.SectorsPerCluster, DS.BytesPerSector,
                            DS.NumOfFreeClusters, DS.TotalNumOfClusters);
 if Result then
  GetPhysicalDisk(DS.Root);
 DS.ErrorCode := 0;
end;

{ LockUnLock
  Locks & unclocks volume.
}
function LockUnLock(hFile: THandle; bLock: Boolean) : Boolean;
var
 junk : Cardinal;
begin
 if bLock then
  Result := DeviceIoControl(hFile, FSCTL_LOCK_VOLUME, nil, 0, nil, 0, junk, nil)
 else
  Result := DeviceIoControl(hFile, FSCTL_UNLOCK_VOLUME, nil, 0, nil, 0, junk, nil)
end;

{ ResetVolume
  Calls API controls codes FSCTL_DISMOUNT_VOLUME & IOCTL_STORAGE_RESET_DEVICE.
  Can be called for volume & for disk (bVol).
}
function ResetVolume(SID : String) : Boolean;
var
 Handle : THandle;
 junk : Cardinal;
begin
 Result := false;
 Sleep(1);
 Handle := CreateFile(PWChar(ExcludeTrailingBackslash(SID)), GENERIC_READ,
    FILE_SHARE_DELETE or FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
    OPEN_EXISTING, 0, 0);
 if Handle = INVALID_HANDLE_VALUE then
  exit;

 if DeviceIoControl(Handle, FSCTL_DISMOUNT_VOLUME, nil, 0, nil, 0, junk, nil) then
  Result := DeviceIoControl(Handle, IOCTL_STORAGE_RESET_DEVICE, nil, 0, nil,
      0, junk, nil);
 CloseHandle(Handle);
 Sleep(1);
 Result := true
end;
function ResetVolume(VolNum : Integer; bVol : Boolean = true) : Boolean;
var
 SID : String;
begin
 if bVol then
  SID := Format(cfsGlobalVolume, [VolNum])
 else
  SID := Format(cfsPhysDrive, [VolNum]);
 Result := ResetVolume(SID)
end;

function SwapDisksOffLineState(NumDskSrc, NumDskTgt: Integer; StrDkId: String): Boolean;
const
 _ = 275; // Wait step to give the time for disk system to do update properly
 L = 120;
var
 StrScDkId, StrTgDkId: String; C: Cardinal;
begin
 C := 0;
 repeat
  Sleep(_);
  PartInfoAction(piaGetIDtype, NumDskSrc, StrScDkId, DisksStyle);
  PartInfoAction(piaGetIDtype, NumDskTgt, StrTgDkId, DisksStyle);
  Inc(C)
 until (L < C) or (StrScDkId <> '') and (StrTgDkId <> '');
 Result := (StrDkId = StrScDkId) and (StrDkId = StrTgDkId);
 if not Result then exit;
 PartInfoAction(piaSetRandomID, NumDskSrc);
 PartInfoAction(piaSetRandomID, NumDskTgt);
 SetDiskOnLine(NumDskTgt);
 ResetVolume(NumDskTgt, false);
 Result := WaitDiskUpdates(NumDskTgt, L * _);
 if Result then
  PartInfoAction(piaSetID, NumDskTgt, StrDkId, DisksStyle);
 PartInfoAction(piaSetID, NumDskSrc, StrDkId, DisksStyle)
end;

{ DiskLabels
  Used to read, drop & set labels of disk.
}
function DiskLabels(DiskNum: Cardinal; var Labels: TVolLabels;
                    dlAction: TdlAction = dlRead;
                    TgtVols: TVolLabels = nil;
                    TgtUpdt: Boolean = false) : Boolean;
 procedure IniDiskLabels(var Labs: TVolLabels; tCall: Byte = 0);
 var
  i, j: Integer; Guid, Lab: String;
 begin
  if tCall = 0 then i := 8 else begin
   i := Length(Labs);
   ResetVolume(DiskNum, false)
  end;
  Lab := '';
  for j := 1 to i do begin
   if tCall = 0 then
    Guid := GetVolumeGuid(Format(cfsDiskPartition, [DiskNum, j]))
   else begin
    SetVolumeOffOnLine(true, Format(cfsGlobalDevDskPart, [DiskNum, j]));

    Guid := GetVolumeGuidByEnum(Format(cfsDiskPartition, [DiskNum, j]));
    if Guid <> '' then
     Lab := GetVolumePathNames(Guid)[0];
    if Lab <> '' then
     DeleteMountPointOfVolume(Lab)
   end;
   if Guid <> '' then begin
    if Length(Labs) < j then SetLength(Labs, j);
    Labs[j - 1].GUID := Guid;
    if not TgtUpdt then begin
     Labs[j - 1].VLab := ''; Labs[j - 1].ULab := ''; Labs[j - 1].Valid := 0
    end
   end else if tCall = 0 then
    break
  end;
 end;
var
 i : Integer; sL: String;
begin
 Result := false;
 if Length(Labels) = 0 then IniDiskLabels(Labels);
 if Length(Labels) = 0 then exit;

 if dlAction in [dlRead, dlReadDrop] then
  for i := 0 to Length(Labels) - 1 do begin
   Labels[i].VLab := GetVolumePathNames(Labels[i].GUID)[0];
   if Labels[i].VLab <> '' then begin
    if Length(Labels[i].VLab) = 1 then Labels[i].VLab := Labels[i].VLab + ':\';
    Result := true
   end;
  end;

 if dlAction = dlReadDrop then begin
  for i := 0 to Length(Labels) - 1 do
   if Labels[i].VLab <> '' then
    DeleteMountPointOfVolume(Labels[i].VLab)
 end else
  if (dlAction = dlSet) and (TgtVols <> nil) and
    (Length(Labels) = Length(TgtVols)) then
  begin
   if TgtUpdt then
    IniDiskLabels(TgtVols, 1);
   for i := 0 to Length(Labels) - 1 do
    if Labels[i].VLab <> '' then begin
     DeleteMountPointOfVolume(Labels[i].VLab);
     if AddMountPointOfVolume(TgtVols[i].GUID, Labels[i].VLab) then
      Result := true
    end else begin
     sL := GetVolumePathNames(Labels[i].GUID)[0];
     if sL <> '' then
      DeleteMountPointOfVolume(sL)
    end
  end
end;
function IniDiskLabels(DskNumS, DskNumT: Cardinal; dlAction: TdlAction = dlRead): Boolean;
begin
 Result := DiskLabels(DskNumS, SrcLabels, dlAction);
 if Result then
  DiskLabels(DskNumT, TgtLabels, dlAction)
 else
  Result := DiskLabels(DskNumT, TgtLabels, dlAction)
end;

{ AssignLabelsLocally
  Assign volume labels locally using string of .cfg file.
  Does nothing if it is empty.
}
function AssignLabelsLocally(PartNum: Integer): Boolean;
var
 aAL: TStringDynArray; i, j: Integer;
begin
 Result := false;
 if (PartNum < 1) or (Length(SrcLabels) < PartNum) or bRestoreLetters then exit;
 aAL := GetAssignLetters; if aAL[0] = '' then exit;
 for i := 0 to Length(SrcLabels) - 1 do SrcLabels[i].VLab := '';
 i := 0;
 for j := PartNum - 1 to Length(SrcLabels) - 1 do
  if Length(aAL) <= i then break else begin
   SrcLabels[j].VLab := Trim(aAL[i] + RightStr(SrcLabels[j].VLab,
       Length(SrcLabels[j].VLab) - 1));
   if Length(SrcLabels[j].VLab) = 1 then
    SrcLabels[j].VLab := SrcLabels[j].VLab + ':\';
   SrcLabels[j].Valid := 1;
   Inc(i)
  end;
 Result := true
end;

{ ShiftLabelsToValidPartition
  Shifts label letters to the partitions index which are valid for optimization.
  It deletes labels of not valid partition, as a result they have not labels
  after completion.
}
procedure ShiftLabelsToValidPartition(CurPart: Integer);
var
 i, j: Integer;
begin
 if (0 < CurPart) and (CurPart <= Length(SrcLabels)) then Dec(CurPart) else exit;
 i := -1;
 repeat Inc(i) until (SrcLabels[i].Valid = 1) or (i = CurPart);
 if SrcLabels[i].Valid <> 1 then exit;

 if i < CurPart then begin
  for j := Length(SrcLabels) - 1 downto CurPart do begin
   SrcLabels[j].VLab  := SrcLabels[j - CurPart + i].VLab;
   SrcLabels[j].ULab  := SrcLabels[j - CurPart + i].ULab;
   SrcLabels[j].Valid := SrcLabels[j - CurPart + i].Valid
  end;
  for j := i to CurPart - 1 do begin
   SrcLabels[j].VLab := ''; SrcLabels[j].ULab := ''; SrcLabels[j].Valid := 0
  end
 end;
 SrcLabels[CurPart].Valid := 2
end;

function CreateNewSystemFile(var FB0F: TFB0File; FileName : String = '';
                                                 Sever: Boolean = true): Boolean;
var
 ErrCod: Integer;
begin
 Result := true;
 if FileName <> '' then
  FB0F.FileName := FileName;
 try
  if FileExists(FB0F.FileName) then begin
   if FB0F.Handle <> INVALID_HANDLE_VALUE then
    CloseHandle(FB0F.Handle);
   DeleteFileNT32(FB0F.FileName);
   Sleep(5)
  end;

  FB0F.Handle := CreateFile(PWChar(FB0F.FileName),
          GENERIC_WRITE or GENERIC_READ, 0, nil, CREATE_ALWAYS,
//          FILE_FLAG_OVERLAPPED or  (SYSTEM ERROR #87: The parameter is incorrect)
          FILE_FLAG_NO_BUFFERING or
          FILE_FLAG_SEQUENTIAL_SCAN or
          FILE_ATTRIBUTE_TEMPORARY or
          FILE_ATTRIBUTE_READONLY or
          FILE_ATTRIBUTE_HIDDEN or
          FILE_ATTRIBUTE_SYSTEM
          , 0);
  if FB0F.Handle = INVALID_HANDLE_VALUE then begin
   OutputSysError(Sever);
   exit
  end
 except
  ErrCod := GetLastError;
  if ErrCod = 0 then ErrCod := ERROR_DISK_FULL; // x64: ErrCod = 0, inside ntdll
  OutputSysError(Sever, '', ErrCod);
  exit
 end;
 FB0F.Size := 0;
 Result := false
end;

procedure CaptionOfSilentTask(CurPerc: UInt64; ShortName: String);
var
 Hdl: THandle; Name: String;
begin
 if SilentMode then begin
  Name := Format('cloper %d of %d: %d', [SubTasksNum, SubTasksCnt,
      CurPerc]) + '% ';
  if Length(Name) + Length(ShortName) < MAX_PATH then
   Name := Name + ShortName
  else
   Name := Name + LeftStr(ShortName, MAX_PATH - Length(Name) - 3) + '...';
  Name := LowerCase(Name);
  Hdl := GetConsoleWindow;
  if LowerCase(Trim(GetWinCaption(Hdl))) <> Trim(Name) then
   SetWinCaption(Hdl, Name)
 end
end;

function SearchResolveSignatureCollision(Serials: String): Boolean;
const
 _ = 1000;
var
 i, j, k, m: Integer; S1, S2: String; D1, D2: Byte;
begin
 i := -1;
 repeat
  Inc(i); Result := GetDiskSerials(i) = UpperCase(Serials)
 until Result or (i = 10);
 if Result then PartInfoAction(piaChkAlloc, i, S1, D1) else exit;
 Result := false;
 for j := i + 1 to 9 do
  if PartInfoAction(piaChkAlloc, j, S2, D2) and (S1 = S2) and (D1 = D2) then
  begin
   OutputMessage(mLogHighPri, 'Modified signature of disk #%d to resolve conflict, checking disk...', [j], mcWarning);
   PartInfoAction(piaSetRandomID, j);
   if SetDiskOnLine(j) then ResetVolume(j, false);
   Result := WaitDiskUpdates(j);
   if Result then begin // Win32_Volume alias assigns with delay, check:
    k := 0; S1 := GetVolumeGuid(cfsDiskPartition, [j, 1]);;
    repeat
     m := 1;
     repeat
      S2 := GetVolumeGuid(cfsGlobalVolume, [m]);
      if S1 = S2 then break else Inc(m)
     until (27 <= m);
     Sleep(_); Inc(k)
    until (S1 = S2) or (10 < k);
    Result := (S1 = S2) and (GetDiskSerials(j) = UpperCase(Serials));
    if Result then begin
     k := 0; m := 0; PartInfoAction(piaChkAlloc, i, S1, D1);
     repeat
      Sleep(_); Inc(k); PartInfoAction(piaGetIDtype, j, S2, D2);
      Result := D1 = D2; if Result then Inc(m) else Dec(m)
     until (m = 3) or (10 < k)
    end
   end;
   if Result then break
  end
end;

var
 ReportDataChange : Boolean = true;

procedure ReportDataModified(Stage, FileName: String; EvType: TTstEvents = teDatChng);
begin
 if ReportDataChange then begin
  OutputMessage(mLogHighPri, 'Identified data modification by unknown background process...', mcWarning);
  OutputMessage(mWarning, 'Recommended to avoid any other data changes during optimization task');
  ReportDataChange := false
 end;
 if bReportFiles then begin
  ReportTestingEvent(EvType, Stage, true);
  ReportTestingEvent(EvType, FileName)
 end;
end;

procedure ReportTestingEventF(ETyp: TTstEvents; FilePath: String);
begin
 ReportTestingEvent(ETyp, FilePath)
end;

procedure ReportDelayedLogMsgs;
var
 i, j: Integer;
begin
 i := Length(DelayedLogMsgs) - 1;
 if 0 <= i then begin
  for j := 0 to i do OutputMessage(mLogNoDate, DelayedLogMsgs[i]);
  SetLength(DelayedLogMsgs, 0)
 end
end;

initialization
 TCrtcProgress.EchoOfSilentTask := @CaptionOfSilentTask;
end.
