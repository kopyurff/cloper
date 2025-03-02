{ VolTools
  The unit of Cloning Optimizer (Cloper) tool.

  The file encapsulates functionality for calls of logical volume APIs.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit VolTools;

interface
uses
 Types, Windows, cloper_typs, cloper_into;

function GetVolumeGuid(DriveName: String): String; overload;
function GetVolumeGuid(FormStr: String; const vals: array of const): String; overload;

function GetVolumeGuidByEnum(DriveName : String) : String;

function GetVolumeFileSystem(Drive: String; var FileSystem: String): Boolean; overload;
function GetVolumeFileSystem(VolNum: Integer; var FileSystem: String): Boolean; overload;
function GetVolumeClusterSize(Drive: String; var dwClusterSizeInBytes: DWORD): Boolean; overload;
function GetVolumeClusterSize(VolNum : Integer; var dwClusterSizeInBytes: DWORD): Boolean; overload;

function SetVolumeOffOnLine(OnLine: Boolean; Handle : THandle) : Boolean; overload;
function SetVolumeOffOnLine(OnLine: Boolean; VolID : String;
                            Access: DWORD = GENERIC_READ or GENERIC_WRITE;
                            Share: DWORD = FILE_SHARE_READ or FILE_SHARE_WRITE;
                            CrDsp: DWORD = OPEN_EXISTING;
                            Flags: DWORD = FILE_ATTRIBUTE_DEVICE): Boolean; overload;
function IsVolumeMounted(VolID : String) : Boolean;
function GetVolumeLabel(Drive : String): string;
function VolIDToDiskNumber(const VolID: String): Integer;
function VolumeIDsToVolumeNumber(const VolID: String): String;
function GetDevicePartitionNumberById(VolID: String; bPartition : Boolean = false): Integer;
function GetVolumeSerial(Drive : String; bByHandle : Boolean = true) : String;
function GetVolumePathNames(VolID : String) : TStringDynArray;
function DeleteMountPointOfVolume(PathName : String) : Boolean;
function AddMountPointOfVolume(VolID : String; PathName : String) : Boolean;

function AddVolumeAlias(VolID : String; Alias : String; bMount : Boolean = false) : Boolean;
function DeleteVolumeAlias(VolID : String; Alias : String; bMount : Boolean = false) : Boolean;
function GetVolumeAliases(VolID : String) : TStringDynArray; overload;
function GetVolumeAliases(VolID : String; var ava : TStringDynArray) : Boolean; overload;

function SubstAliasCreate(DriveLetter: Char; const Path: string): Boolean;
function SubstAliasRemove(DriveLetter: Char): Boolean;
function SubstAliasQuery(DriveLetter: Char): string;

function GetDiskSerials(pdnum : Integer) : String;

implementation
uses
 SysUtils, StrUtils, cloper_base;

function Get1stVolumeGuid : String;
var
 Hndl : THandle;
 unique_volume_name: array[0..MAX_PATH] of AnsiChar;
begin
 Hndl := FindFirstVolume(@unique_volume_name[0], MAX_PATH);
 if Hndl = INVALID_HANDLE_VALUE then
  Result := ''
 else begin
  SetString(Result, unique_volume_name, MAX_PATH);
  Result := LeftStr(Result, Pos(#0, Result) - 1);
  FindVolumeClose(Hndl)
 end
end;

function GetDevicePartitionNumberByIdImp(VolID: String;
  var PartitionNumber: Integer): Integer; forward;

function GetVolumeGuidByEnum(DriveName : String) : String;
label
 scs, clh;
var
 Hndl : THandle;
 unique_volume_name: array[0..MAX_PATH] of AnsiChar;
 cltp : Byte;
 guid, SbD, SbV : String;
 DN, VN, ND, NV : Integer; hLibCnf: HMODULE;
begin
 Result := '';
 try
  hLibCnf := LoadCloperF;
  if MaskFits('_[*Volume{*-*-*-*-*}*]', DriveName, hLibCnf) then begin
   DriveName := IncludeTrailingBackslash(DriveName);
   cltp := 2
  end else begin
   DriveName := ExcludeTrailingBackslash(DriveName);
   SbV := SubStrFittingMask('_[HarddiskVolume]', DriveName, VN, hLibCnf);
   cltp := 0;
   if SbV <> '' then
    VN := StrToInt(RightStr(DriveName, Length(DriveName) - VN - Length(SbV) + 1))
   else begin
    SbV := SubStrFittingMask('_[Partition]', DriveName, VN, hLibCnf);
    if SbV <> '' then begin
     cltp := 1;
     SbD := LeftStr(DriveName, VN - 1);
     VN := StrToInt(RightStr(DriveName, Length(DriveName) - VN - Length(SbV) + 1));
     SbV := SubStrFittingMask('_[Harddisk]', SbD, DN, hLibCnf);
     DN := StrToInt(RightStr(SbD, Length(SbD) - DN - Length(SbV) + 1))
    end else
     raise Exception.Create('Unknown identifier format "' + DriveName + '".')
   end
  end
 except on E:Exception do begin
  DoMessageOutput(mInternalErr, 'Failed to parse volume identifier: '
    + E.Message);
  exit
 end end;

 Hndl := FindFirstVolume(@unique_volume_name[0], MAX_PATH);
 if Hndl = INVALID_HANDLE_VALUE then exit;

 repeat
  SetString(guid, unique_volume_name, MAX_PATH);
  guid := LeftStr(guid, Pos(#0, guid) - 1);
  case cltp of
  0: begin
   SbD := ExcludeTrailingBackslash(VolumeIDsToVolumeNumber(guid));
   SbV := SubStrFittingMask('_[HarddiskVolume]', SbD, NV, hLibCnf);
   if SbV <> '' then begin
    SbV := RightStr(SbD, Length(SbD) - NV - Length(SbV) + 1);
    if SbV <> '' then begin
     NV := StrToInt(SbV);
     if VN = NV then goto scs
    end
   end
  end;
  1: begin
   ND := GetDevicePartitionNumberByIdImp(guid, NV);
   if (-1 < ND) and (DN = ND) and (VN = NV) then goto scs
  end;
  2: if DriveName = guid then goto scs;
  end;
 until not FindNextVolume(Hndl, @unique_volume_name[0], MAX_PATH);
 goto clh;
scs:
 Result := guid;
clh:
 FreeLibrary(hLibCnf); FindVolumeClose(Hndl)
end;

{ GetVolumeGuid
 Obtains unique volume identifier by its letter name, \\.\HarddiskVolume#\ or
 by \\.\Harddisk#Partition#\.
}
function GetVolumeGuid(DriveName : String) : String;
type
  TGetVolumePathName = function(lpszFileName: PAnsiChar;
    lpszVolumePathName: PAnsiChar; cchBufferLength: DWORD): BOOL; stdcall;
  TGetVolumeNameForVolumeMountPoint = function(lpszVolumeMountPoint: PAnsiChar;
    lpszVolumeName: PAnsiChar; cchBufferLength: DWORD): BOOL; stdcall;
var
 GetVolumePathName : TGetVolumePathName;
 GetVolumeNameForVolumeMountPoint : TGetVolumeNameForVolumeMountPoint;
 unique_volume_name: array[0..MAX_PATH] of AnsiChar;
 lpszVolumePathName : PAnsiChar;
 DriveNameA : AnsiString;
begin
 if (0 = Pos('globalroot', LowerCase(DriveName)))
 and (OSMajorMinorVersion < 60) then // XP doesn't support Win32_Volume, enum
  Result := GetVolumeGuidByEnum(DriveName)
 else begin
  DriveName := IncludeTrailingBackslash(DriveName);
  @GetVolumePathName := GetProcAddress(GetModuleHandle('kernel32'),
    'GetVolumePathNameA');
  @GetVolumeNameForVolumeMountPoint :=
    GetProcAddress(GetModuleHandle('kernel32'),
    'GetVolumeNameForVolumeMountPointA');
  Result := '';
  if not Assigned(GetVolumePathName)
  or not Assigned(GetVolumeNameForVolumeMountPoint) then exit;

  SetAnsiString(@DriveNameA, PWideChar(DriveName), MAX_PATH, 0);
  SetLength(DriveNameA, Length(DriveName));
  GetMem(lpszVolumePathName, MAX_PATH);
  if GetVolumePathName(PAnsiChar(DriveNameA), lpszVolumePathName, MAX_PATH) then
   if GetVolumeNameForVolumeMountPoint(PAnsiChar(DriveNameA),
                                      @unique_volume_name[0], MAX_PATH) then
   begin
    SetString(Result, unique_volume_name, MAX_PATH);
    Result := LeftStr(Result, Pos(#0, Result) - 1)
   end else begin
    if GetLastError() = 4390 then //The file or directory is not a reparse point
     DoOutputSysError(true, 'OS requires reboot or manual reactivation of disk')
    else
     DoOutputSysError(true);
    Result := ''
   end;
 end
end;
function GetVolumeGuid(FormStr : String; const vals: array of const) : String;
begin
 Result := GetVolumeGuid(Format(FormStr, vals))
end;

{ GetVolumeClusterSize(2)
  Get cluster size of volume in bytes.
}
function GetVolumeClusterSize(Drive: String;
  var dwClusterSizeInBytes: DWORD): Boolean;
var
 dwSectorsPerCluster, dwBytesPerSector, aux : DWORD;
 Buf : array [0..MAX_PATH] of AnsiChar;
 DriveA : AnsiString;
begin
 dwClusterSizeInBytes := 0;
 Result := GetDiskFreeSpace(PWChar(Drive), dwSectorsPerCluster,
  dwBytesPerSector, aux, aux);
 if not Result then exit;

 dwClusterSizeInBytes := dwSectorsPerCluster * dwBytesPerSector;
end;

function GetVolumeClusterSize(VolNum : Integer;
  var dwClusterSizeInBytes: DWORD): Boolean;
begin
  Result := GetVolumeClusterSize('\\.\globalroot\Device\HarddiskVolume' +
    IntToStr(VolNum) + '\', dwClusterSizeInBytes);
end;

{ GetVolumeFileSystem(2)
  Gets string with name of volume file system.
}
function GetVolumeFileSystem(Drive: String; var FileSystem: String): Boolean;
var
 dwSectorsPerCluster, dwBytesPerSector, aux : DWORD;
 Buf : array [0..MAX_PATH] of AnsiChar;
 DriveA : AnsiString;
begin
 FileSystem := 'N/A'; Result := True;
 SetAnsiString(@DriveA, PWideChar(Drive), MAX_PATH, 0);
 SetLength(DriveA, Length(Drive));

 if GetVolumeInformationA(PAnsiChar(DriveA), nil, 0, @aux, aux,
  aux, Buf, SizeOf(Buf)) then
 begin
  SetString(FileSystem, Buf, StrLen(Buf));
  FileSystem := AnsiUpperCase(FileSystem)
 end else Result := False
end;

function GetVolumeFileSystem(VolNum : Integer; var FileSystem: String): Boolean;
begin
  Result := GetVolumeFileSystem
    ('\\.\globalroot\Device\HarddiskVolume' + IntToStr(VolNum) + '\', FileSystem);
end;

{ SetVolumeOffOnLine (2)
  Sets volume online/offline.
  Notes:
  #1. Format of VolID: GUID or \\?\GLOBALROOT\Device\Harddisk#\Partition#
  #2. OnLine = false:  to avoid error 0x57 "The parameter is incorrect" call
                       DeviceIoControl with IOCTL_STORAGE_RESET_DEVICE for
                       physical disk before next attempt to obtain handle of
                       volume.
}
function SetVolumeOffOnLine(OnLine: Boolean; Handle: THandle): Boolean;
begin
 Result := Handle <> INVALID_HANDLE_VALUE;
 if not Result then begin
  DoMessageOutput(mInternalErr, 'SetVolumeOffOnLine: Invalid handle');
  exit
 end;
 if OnLine then begin
  Result := IoDeviceControl(Handle, IOCTL_VOLUME_ONLINE);
  if Result then
   IoDeviceControl(Handle, IOCTL_DISK_UPDATE_PROPERTIES)
 end else begin
  Result := false;
  if not FlushFileBuffers(Handle) then
   DoOutputSysError(false);
  if IoDeviceControl(Handle, FSCTL_LOCK_VOLUME) then begin
   if IoDeviceControl(Handle, FSCTL_DISMOUNT_VOLUME) then
    if IoDeviceControl(Handle, IOCTL_VOLUME_OFFLINE) then
     Result := true;
    IoDeviceControl(Handle, FSCTL_UNLOCK_VOLUME)
  end
 end
end;
function SetVolumeOffOnLine(OnLine: Boolean; VolID: String;
                            Access: DWORD = GENERIC_READ or GENERIC_WRITE;
                            Share: DWORD = FILE_SHARE_READ or FILE_SHARE_WRITE;
                            CrDsp: DWORD = OPEN_EXISTING;
                            Flags: DWORD = FILE_ATTRIBUTE_DEVICE): Boolean;
var
 Handle: THandle;
begin
 Handle := CreateFile(PChar(VolID), Access, Share, nil, CrDsp, Flags, 0);
 Result := Handle <> INVALID_HANDLE_VALUE;
 if Result then begin
  Result := SetVolumeOffOnLine(OnLine, Handle);
  CloseHandle(Handle)
 end else
  DoOutputSysError(true)
end;

// Checks the volume is mounted (true) or not mounted (false).
function IsVolumeMounted(VolID : String) : Boolean;
var
 Handle : THandle; Dum : Cardinal;
begin
 Handle := CreateFile(GetDrvLbl(VolID), GENERIC_READ, FILE_SHARE_READ or
  FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
 if Handle <> INVALID_HANDLE_VALUE then
  Result := DeviceIoControl(Handle, FSCTL_IS_VOLUME_MOUNTED, nil, 0, nil, 0,
   Dum, nil)
 else
  Result := false
end;

// Obtains the string of volume label name by its letter or identifier.
function GetVolumeLabel(Drive : String): string;
var
  aux1, aux2, aux3 : DWORD;
  Buf: array [0..MAX_PATH] of AnsiChar;
  DriveA : AnsiString;
begin
 SetAnsiString(@DriveA, PWideChar(Drive), MAX_PATH, 0);
 SetLength(DriveA, Length(Drive));

 if GetVolumeInformationA(PAnsiChar(DriveA), Buf, SizeOf(Buf), @aux1, aux1,
   aux1, nil, 0) then
  begin
  SetString(Result, Buf, StrLen(Buf));   { Set return result }
  Result := AnsiUpperCase(Result)
 end else Result := '';
end;

// Obtains disk number using, \Device\HarddiskVolume#, GUID
// or letter A:, B: ... Z: of volume. The 1st disk has value '0'.
function VolIDToDiskNumber(const VolID: String): Integer;
var
 Handle: THandle;
 Readed: DWORD;
 Dummy: Int64;
{$ALIGN 8}
var
  Offsets : record
   NumberOfPhysicalOffsets : ULONG;
   PhysicalOffset : array[0..0] of record
    DiskNumber : ULONG;
    Offset : LONGLONG;
   end;
  end;
const
 IOCTL_VOLUME_LOGICAL_TO_PHYSICAL = $560020;
begin  // IOCTL_STORAGE_GET_DEVICE_NUMBER
 Result := -1;
 Handle := CreateFile(GetDrvLbl(VolID), GENERIC_READ, FILE_SHARE_READ or
  FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
 if Handle <> INVALID_HANDLE_VALUE then
  try
   Dummy := 0;
   if DeviceIoControl(Handle, IOCTL_VOLUME_LOGICAL_TO_PHYSICAL, @Dummy,
     SizeOf(Dummy), @Offsets, SizeOf(Offsets), Readed, nil) then
     Result := Offsets.PhysicalOffset[0].DiskNumber;
  finally
   CloseHandle(Handle)
  end;
end;

// Obtains volume number (\Device\HarddiskVolume#) using its GUID
// or using its letters A:, B: ... Z:.
// WARNING: Often returns \Device\HarddiskVolume without number (alias).
function VolumeIDsToVolumeNumber(const VolID: String): String;
var
 sda : TStringDynArray;
begin
 if GetVolumeAliases(VolID, sda) then
  Result := sda[Length(sda) - 1]
 else
  Result := 'Error #' + IntToStr(GetLastError) + ': ' + SysErrorMessage(GetLastError)
end;

function GetDevicePartitionNumberByIdImp(VolID: String; var PartitionNumber : Integer): Integer;
const
 IOCTL_STORAGE_GET_DEVICE_NUMBER = $2D1080;
type
 TDeviceInfo = packed record
   DeviceType, DeviceNumber, PartitionNumber : DWORD
 end;
var
 hDevice: THandle;
 BytesReturned: DWORD;
 DeviceInfo: TDeviceInfo;
begin
 Result  := -1;
 VolID   := ExcludeTrailingBackslash(VolID);
 hDevice := CreateFile(GetDrvLbl(VolID), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
 if hDevice = INVALID_HANDLE_VALUE then begin
  Result := Low(Int32);
  PartitionNumber := Low(Int32)
 end else
  try
   if DeviceIoControl(hDevice, IOCTL_STORAGE_GET_DEVICE_NUMBER, nil, 0,
     @DeviceInfo, SizeOf(TDeviceInfo), BytesReturned, nil) then
   begin
    Result := DeviceInfo.DeviceNumber;
    PartitionNumber := DeviceInfo.PartitionNumber
   end
  finally
    CloseHandle(hDevice)
  end
end;
// Obtains physical disk number or number of partition, using
// volume number \\.\Device\HarddiskVolume# or disk letter (e.g. \\.\C:).
function GetDevicePartitionNumberById(VolID: String; bPartition : Boolean = false): Integer;
var
 PartitionNumber : Integer;
begin
 Result := GetDevicePartitionNumberByIdImp(VolID, PartitionNumber);
 if bPartition then
  Result := PartitionNumber
end;

{ GetVolumeSerial
 Obtains the formatted string of volume serial number.
}
function GetVolumeSerial(Drive: String; bByHandle: Boolean = true) : String;
 var
  PDrive : PWChar;
 procedure ByName;
 var
  D_Id, Tmp1, Tmp2: DWORD;
 begin
  D_Id := 0;
  GetVolumeInformation(PDrive, Nil, 0, @D_Id, Tmp1, Tmp2, Nil, 0);
  if D_Id <> 0 then
   Result := Format('%.4X-%.4X', [HiWord(D_Id), LoWord(D_Id)])
 end;
 procedure ByHandle;
 var
  RootHandle: Cardinal;
  FileInfo: TByHandleFileInformation;
 begin
  RootHandle := CreateFileW(PDrive, 0, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if RootHandle <> INVALID_HANDLE_VALUE then
   if GetFileInformationByHandle(RootHandle, FileInfo) then
    Result := Format('%.4X-%.4X', [HiWord(FileInfo.dwVolumeSerialNumber), LoWord(FileInfo.dwVolumeSerialNumber)])
 end;
begin
 Result := '';
 PDrive := PWChar(Drive);
 if bByHandle then ByHandle else ByName;
 if Result = '' then
  Result := 'Error #' + IntToStr(GetLastError) + ': ' + SysErrorMessage(GetLastError)
end;

{ GetVolumePathNames ( == Mount Points)
  Returns array drive letter & folders for given volume or '' if no drive letter
  VolID - volume GUID (or drive letter).
}
function GetVolumePathNames(VolID : String) : TStringDynArray;
type
 TGetVolumePathNamesForVolumeName = function
                      (lpszVolumeName: PAnsiChar;
                      lpszVolumePathNames: PAnsiChar; cchBufferLength: DWORD;
                      lpcchReturnLength: PDWORD): BOOL; stdcall;
const
 BufSize = 2048;
var
 GetVolumePathNamesForVolumeName : TGetVolumePathNamesForVolumeName;
 buffer : array [0..BufSize] of AnsiChar;
 Count, i, j : Integer;
 VolIDA : AnsiString;
begin
 SetLength(Result, 1);
 Result[0] := '';
 @GetVolumePathNamesForVolumeName := GetProcAddress(GetModuleHandle(Kernel32),
   'GetVolumePathNamesForVolumeNameA');
 if not Assigned(GetVolumePathNamesForVolumeName)then exit;
 SetAnsiString(@VolIDA, PWideChar(VolID), MAX_PATH, 0);
 SetLength(VolIDA, Length(VolID));
 if not GetVolumePathNamesForVolumeName(PAnsiChar(VolIDA),
   buffer, BufSize, @Count) or (Count = 0) then
  exit;

 if 60 <= OSMajorMinorVersion then Count := Count - 3;
 i := 0;
 for j := 0 to Count do
  if buffer[j] = #0 then begin
   i := Length(Result);
   SetLength(Result, i + 1);
  end else
   Result[i] := Result[i] + buffer[j]
end;

{ DeleteMountPointOfVolume
  API extension to delete a drive letter or mounted folder.
}
function DeleteMountPointOfVolume(PathName : String) : Boolean;
type
 TDeleteVolumeMountPoint = function(lpszVolumeMountPoint: PWideChar)
                : BOOL; stdcall;
var
 DeleteVolumeMountPoint : TDeleteVolumeMountPoint;
begin
 @DeleteVolumeMountPoint := GetProcAddress(GetModuleHandle('kernel32'),
   'DeleteVolumeMountPointW');
 Result := false;
 if not Assigned(DeleteVolumeMountPoint) then exit;
 Result := DeleteVolumeMountPoint(PWideChar(PathName))
end;

{ AddMountPointOfVolume
  API extension to associate a volume with a drive letter or a directory on
  another volume.
}
function AddMountPointOfVolume(VolID : String; PathName : String) : Boolean;
type
 TSetVolumeMountPoint = function(lpszVolumeMountPoint : PWideChar;
                                 lpszVolumeName: PWideChar): BOOL; stdcall;
var
 SetVolumeMountPoint : TSetVolumeMountPoint;
begin
 @SetVolumeMountPoint := GetProcAddress(GetModuleHandle('kernel32'),
   'SetVolumeMountPointW');
 Result := false;
 if not Assigned(SetVolumeMountPoint) then exit;
 VolID := IncludeTrailingBackslash(VolID);
 Result := SetVolumeMountPoint(PWideChar(PathName), PWideChar(VolID))
end;

{ GetVolumeAliases
  Gets all defined aliases of volume using its GUID
  or using its letters A:, B: ... Z:.
}
function GetVolumeAliases(VolID : String) : TStringDynArray;
var
  lpQuery : array[0..MAXCHAR - 1] of Char;
  i, j, Count : Integer;
begin
 SetLength(Result, 1); Result[0] := '';
 VolID := ExtractVolumeIdString(VolID);
 Count := QueryDosDevice(PChar(VolID), @lpQuery[0], MAXCHAR);
 i := 0;
 for j := 0 to Count - 4 do
  if lpQuery[j] = #0 then begin
   i := Length(Result);
   SetLength(Result, i + 1);
  end else
   Result[i] := Result[i] + lpQuery[j];
 if 2 < Count then
  Result[i] := Result[i] + lpQuery[Count - 3]
end;

function GetVolumeAliases(VolID : String; var ava : TStringDynArray) : Boolean;
begin
 ava := GetVolumeAliases(VolID);
 Result := (1 < Length(ava)) or (ava[0] <> '');
end;

{ AddVolumeAlias
  Adds new alias of volume using its GUID
  or using its letters A:, B: ... Z:.
}
function AddVolumeAlias(VolID : String; Alias : String; bMount : Boolean = false) : Boolean;
var
 sda : TStringDynArray;
 s : String;
 function FindADA : Integer;
 var
  i : Integer;
 begin
  Result := -1;
  for i := 0 to Length(sda) - 1 do
   if Pos(LowerCase(ExtractVolumeIdString(sda[i])), s) = 1 then begin
    Result := i;
    exit
   end;
 end;
var i : Integer; begin
 Result := false;
 VolID := ExtractVolumeIdString(VolID);
 s := LowerCase(ExtractVolumeIdString(Alias));
 if GetVolumeAliases(VolID, sda) then begin
  Result := -1 < FindADA;
  if Result then exit;
 end;
 if bMount then begin
  if not DefineDosDevice(0, PChar(VolID), PChar(Alias)) then
   exit;
 end else
  if not DefineDosDevice(DDD_RAW_TARGET_PATH, PChar(VolID), PChar(Alias)) then
   exit;
 if not GetVolumeAliases(VolID, sda) then
  exit;
 // Have not independent definition of system alias \\.\Device\HarddiskVolume#
 // New alias expected at 1st position of array.
 if FindADA <> 0 then
  raise Exception.Create('Unexpected index of added volume alias.');
 Result := true;
end;

{ DeleteVolumeAlias
  Deletes new alias of volume using its GUID
  or using its letters A:, B: ... Z:.
}
function DeleteVolumeAlias(VolID : String; Alias : String; bMount : Boolean = false) : Boolean;
var
  sda : TStringDynArray;
  i, j : Integer;
  b : Boolean;
  s : String;
begin
 Result := false;
 if not GetVolumeAliases(VolID, sda) then
  exit;
 s := LowerCase(ExtractVolumeIdString(Alias));
 b := false;
 if 1 < Length(sda) then j := Length(sda) - 2 else j := Length(sda) - 1;

 for i := 0 to j do // The last alias is the system name of volume.
  if Pos(LowerCase(ExtractVolumeIdString(sda[i])), s) = 1 then begin
   b := true;
   break
  end;
 if b then begin
  VolID := ExtractVolumeIdString(VolID);
  if bMount then
   Result := DefineDosDevice(DDD_REMOVE_DEFINITION, PChar(VolID), PChar(Alias))
  else
   Result := DefineDosDevice(DDD_RAW_TARGET_PATH or DDD_REMOVE_DEFINITION or
                             DDD_EXACT_MATCH_ON_REMOVE,
                             PChar(VolID), PChar(Alias))
 end;
end;

function SubstAliasCreate(DriveLetter: Char; const Path: string): Boolean;
var drvno: byte;
    buff: array[0..256] of char;
    FPath: string;

  function AddSlash(const Path: string): string; {добавляем \ если надо}
  begin
    if (Path = '') or (Path[Length(Path)] <> '\') then Result:=Path+'\'
    else Result:=Path;
  end;

begin
 if Path[Length(Path)] = '\' then
  FPath := Copy(Path,1,Length(Path)-1)
 else
  FPath := Path;
 if Win32Platform <> VER_PLATFORM_WIN32_NT then
  raise Exception.Create('Designed for use only on windows platforms');

 buff[0] := DriveLetter;
 buff[1] := ':';
 buff[2] := #0;
 Result := DefineDosDevice(0, buff, PChar(Path));
end;

function SubstAliasRemove(DriveLetter: Char): Boolean;
var drvno: byte;
    Drive,Path: string;
begin
 if Win32Platform <> VER_PLATFORM_WIN32_NT then
  raise Exception.Create('Designed for use only on windows platforms');
 SetLength(Drive, 3);
 Drive[1] := DriveLetter;
 Drive[2] := ':';
 Drive[3] := #0;
 Path := SubstAliasQuery(DriveLetter);
 Result := DefineDosDevice(DDD_REMOVE_DEFINITION, PChar(Drive), PChar(Path));
end;

function SubstAliasQuery(DriveLetter: Char): string;
var drvno: byte;
    buff: array[0..256] of char;
    lbuff: array[0..256] of char;
begin
 if Win32Platform <> VER_PLATFORM_WIN32_NT then
    raise Exception.Create('Not designed for non-windows platforms');
 lbuff[0] := DriveLetter;
 lbuff[1] := ':';
 lbuff[2] := #0;
 buff[0] := #0;
 QueryDosDevice(lbuff, buff, 256);
 Result := StrPas(buff);
 if Copy(Result, 1, 4) = '\??\' then
  Result := Copy(Result, 5, Length(Result))
 else
  Result := '';
end;

function GetDiskSerials(pdnum : Integer) : String;
var
 i : Integer; guid : String;
begin
 Result := '';
 for i := 1 to 10 do begin
  guid := GetVolumeGuid(cfsDiskPartition, [pdnum, i]);
  if guid <> '' then begin
   if Result <> '' then Result := Result + '-';
   Result := Result + GetVolumeSerial(guid)
  end
 end;
end;

initialization
 Guid1Vol := Get1stVolumeGuid;
end.
