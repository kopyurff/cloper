{ DskTools
  The unit of Cloning Optimizer (Cloper) tool.

  The file encapsulates functionality for calls of physical disk APIs.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit DskTools;

interface
uses
 Types, Windows, cloper_typs;
type
 TPartInfoAction = (piaGetIDtype = 0,
                    piaSetRandomID = 1, piaSetID = 2, piaChkAlloc = 3,
  // To set disk system information of 1 disk to another, not useful:
  piaClone = 4, piaCloneRndID = 5, piaCloneSetID = 6
 );

function GetGeometry(DriveNumber: Byte; var Geometry : TDiskGeometry) : Boolean; overload;
function GetGeometry(Drive: String; var Geometry : TDiskGeometry) : Boolean; overload;
function CompareGeometries(ScGeometry, TgGeometry : TDiskGeometry) : Boolean;

function GetDriveInfo(DriveNumber: Byte; DriveInfo: PDriveInfo;
                      StartingSector: DWORD = 0; BytesPerSector: DWORD = 512): Boolean;
function CompareDrivesInfo(Drive1I, Drive2I : TDriveInfo) : Boolean; overload;
function CompareDrivesInfo(Drive1Num, Drive2Num : Byte) : Boolean; overload;

// Warning: Irreversibly deletes (destroys) file system of disk with index NumDsk.
function DestroyDiskSystem(NumDsk: Integer;
  Handle: THandle = INVALID_HANDLE_VALUE): Boolean;

function CreateDiskSystem(NumDsk: Integer; Handle: THandle = 0;
  Signature: String = ''; Style: Byte = 2): Boolean;

function VerifyDisk(NumDsk: Integer): Boolean;

// Checks the disk is writable, in some cases may be used to check online status.
function IsDiskWritable(NumDsk : Integer) : Boolean;

// Checks the disk is online (can return incorrect results, generally works).
function IsDiskOnline(NumDsk : Integer) : Boolean;

// Checks the updating disk is again available online.
function WaitDiskUpdates(NumDsk: Integer; Wait: Cardinal = 60000): Boolean;

// Warning: Windows 7 and higher.
function SetDiskOnLine(NumDsk: Integer;
  Handle: THandle = INVALID_HANDLE_VALUE): Boolean;

function ResetStorageBus(DiskNum : Cardinal) : Boolean;

// Gets/Sets disk identifier inside byte array of 1st 2 sectors.
procedure GetSetDiskSignInSectorArray(bGet: Boolean;
                                  var Sign: String;
                                  var SrAr: TByteDynArray;
                                      DTyp: Byte);

function PartInfoAction(pia: TPartInfoAction; DskIndex: Integer;
  var sDskID: String; var DskStyle: Byte; Handle: THandle = 0;
  TgtDskIndex: UInt64 = High(UInt64); TgtHdl: THandle = 0): Boolean; overload;

function PartInfoAction(pia: TPartInfoAction; DskNum: Integer;
  Handle: THandle = 0; DskNumID: UInt64 = High(UInt64);
  TgtHdl: UInt64 = High(UInt64)): Boolean; overload;

implementation
uses
 SysUtils, StrTools, cloper_base;

{ DestroyDiskSystem
  Destroys file system of disk.
}
function DestroyDiskSystem(NumDsk: Integer;
  Handle: THandle = INVALID_HANDLE_VALUE): Boolean;
var
 junk : Cardinal;
 bCloseHandle : Boolean;
begin
 Result := false;
 bCloseHandle := Handle = INVALID_HANDLE_VALUE;
 if Handle = INVALID_HANDLE_VALUE then
  Handle := CreateFile(PChar('\\.\PhysicalDrive' + IntToStr(NumDsk)),
      GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
 if Handle = INVALID_HANDLE_VALUE then
  exit;

 if DeviceIoControl(Handle, IOCTL_DISK_DELETE_DRIVE_LAYOUT, nil, 0, nil, 0,
    junk, nil) then
  Result := DeviceIoControl(Handle, IOCTL_DISK_UPDATE_PROPERTIES, nil, 0, nil,
      0, junk, nil)
 else
  DoOutputSysError(true);

 if bCloseHandle then CloseHandle(Handle)
end;

function CreateDiskSystem(NumDsk: Integer; Handle: THandle = 0;
  Signature: String = ''; Style: Byte = 2): Boolean;
type
 // Original description of objects is given by JEDI library reference:
 // https://github.com/graemeg/freepascal/blob/master/packages/winunits-jedi/src/jwawinioctl.pas
 // GPT part of declarations was saved to have correct size of type.
 CREATE_DISK_MBR = record
  Signature: DWORD;
 end;
 CREATE_DISK_GPT = record
  DiskId: TGuid; // Unique disk id for the disk.
  MaxPartitionCount: DWORD; // Maximim number of partitions allowable.
 end;
 PARTITION_STYLE = (   // 0 - MBR, 1 - GPT, 2 - RAW
  PARTITION_STYLE_MBR, PARTITION_STYLE_GPT, PARTITION_STYLE_RAW
 );
 CREATE_DISK = record
  PartitionStyle: PARTITION_STYLE;
  case Integer of
   0: (Mbr: CREATE_DISK_MBR);
   1: (Gpt: CREATE_DISK_GPT);
 end;
var
 junk : Cardinal; disk: CREATE_DISK; bCloseHandle : Boolean;
begin
 Result := false;
 bCloseHandle := Handle = 0;
 if (Handle = 0) or (Handle = INVALID_HANDLE_VALUE) then
  Handle := CreateFile(PWChar(Format(cfsPhysDrive, [NumDsk])),
      GENERIC_WRITE or GENERIC_READ, FILE_SHARE_WRITE or FILE_SHARE_READ
      , nil, OPEN_EXISTING,
      0, 0);
 if Handle = INVALID_HANDLE_VALUE then
  exit;
 ZeroMemory(@disk, SizeOf(CREATE_DISK));
 if Style = 1 then begin
  disk.PartitionStyle := PARTITION_STYLE_GPT;
 end else if Style = 2 then
  disk.PartitionStyle := PARTITION_STYLE_MBR
 else
  raise Exception.Create('CreateDiskSystem: Unknown disk style.');
 if Signature = '' then begin
  Randomize;
  if Style = 1 then begin
   disk.GPT.DiskId.D1 := Random($FFFFFFFF);
   disk.GPT.DiskId.D2 := Random($FFFF);
   disk.GPT.DiskId.D3 := Random($FFFF);
  end else if Style = 2 then
   disk.Mbr.Signature := Random($FFFFFFFF)
 end else
  if Style = 1 then
   disk.GPT.DiskId := StrToGuid(Signature)
  else
   disk.Mbr.Signature := StrToInt64(Signature);

 if DeviceIoControl(Handle, IOCTL_DISK_CREATE_DISK, @disk, SizeOf(CREATE_DISK),
    nil, 0, junk, nil) then begin
  Result := true;
  DeviceIoControl(Handle, IOCTL_DISK_UPDATE_PROPERTIES, nil, 0, nil,
      0, junk, nil)
 end else
  DoOutputSysError(true, 'CreateDiskSystem');
 if bCloseHandle then CloseHandle(Handle)
end;

function VerifyDisk(NumDsk: Integer): Boolean;
type
 VERIFY_INFORMATION = record
  StartingOffset: LARGE_INTEGER;
  Length: UINT64;
 end;
var
 Geometry : TDiskGeometry;
 VerInf: VERIFY_INFORMATION;
 Handle: THandle; jk: Cardinal;
begin
 Result := false;
 if not GetGeometry(NumDsk, Geometry) then
  exit;
 Handle := CreateFile(PWChar(Format(cfsPhysDrive, [NumDsk])), GENERIC_READ,
    FILE_SHARE_DELETE or FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
    OPEN_EXISTING, 0, 0);
 if Handle = INVALID_HANDLE_VALUE then
  exit;
 ZeroMemory(@VerInf, SizeOf(VERIFY_INFORMATION));
 with Geometry do
  VerInf.Length := Cylinders * TracksPerCylinder * SectorsPerTrack * BytesPerSector;

 Result := DeviceIoControl(Handle, IOCTL_DISK_VERIFY, @VerInf,
    SizeOf(VERIFY_INFORMATION), nil, 0, jk, nil);
 if not Result then
  DoOutputSysError(true);
 CloseHandle(Handle)
end;

function IsDiskWritable(NumDsk : Integer) : Boolean;
var
 Handle : THandle;
 junk : Cardinal;
begin
 Result := false;
 if not GetFileHandle(NumDsk, GENERIC_READ, FILE_SHARE_READ, Handle) then exit;

 Result := DeviceIoControl(Handle, IOCTL_DISK_IS_WRITABLE, nil, 0, nil, 0,
    junk, nil);
 CloseHandle(Handle)
end;

function IsDiskOnline(NumDsk : Integer) : Boolean;
var
 Handle : THandle;
 junk : Cardinal;
begin
 Result := false;
 if not IsDiskWritable(NumDsk) then
  exit
 else
  if not PartInfoAction(piaChkAlloc, NumDsk) then
   exit;
 if not GetFileHandle(NumDsk + 1, 1,
    GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, Handle) then
    exit;
 CloseHandle(Handle);
 Result := true
end;

{ WaitDiskUpdates
  Checks the updating disk is again available online.
}
function WaitDiskUpdates(NumDsk : Integer; Wait: Cardinal = 60000): Boolean;
const
  _ = 275;

 procedure DoChk(var Res: Integer; SID: String; ContCode: DWORD);
 begin
  if IoDeviceControl(SID, ContCode) then Inc(Res); Sleep(_)
 end;
var
 Count, Res: Integer; sDskId: String; DskStl: Byte;
begin
 Count := Wait;
 repeat
  Sleep(_); Res := 0;
  DoChk(Res, csGlobalMountMan, IOCTL_MOUNTMGR_CHECK_UNPROCESSED_VOLUMES);
  DoChk(Res, Format(cfsGlobalDevHardDsk, [NumDsk]), IOCTL_DISK_UPDATE_PROPERTIES);
  DoChk(Res, Format(cfsGlobalDevHardDsk, [NumDsk]), IOCTL_STORAGE_CHECK_VERIFY2);
  Dec(Count, _); Result := (Res = 3) and IsDiskOnline(NumDsk)
 until (Count < 0) or Result;
 if Result then begin
  repeat
   Sleep(_); Dec(Count, _); PartInfoAction(piaGetIDtype, NumDsk, sDskId, DskStl)
  until (DskStl = 255) or (Count < 0);
  Result := DskStl in [1, 2]
 end
end;

{ SetDiskOnLine
  Sets disk online.
  Note: Damages BCB OS boot files & gives OS not bootable (oxc000000e error).
        Sets offline doesn't work.
}
function SetDiskOnLine(NumDsk: Integer;
  Handle: THandle = INVALID_HANDLE_VALUE): Boolean;
type
 SET_DISK_ATTRIBUTES = record
   Version : DWORD;
   Persist : BOOLEAN;
   Reserved1 : PBOOL; // BOOLEAN   Reserved1[3];
   Attributes : DWORDLONG;
   AttributesMask : DWORDLONG;
   Reserved2 : PDWORD; // DWORD     Reserved2[4];
 end;
const
 IOCTL_DISK_SET_DISK_ATTRIBUTES = $7C0F4;
 DISK_ATTRIBUTE_OFFLINE = 1;
 DISK_ATTRIBUTE_ONLINE = 0;
var
 sda : SET_DISK_ATTRIBUTES;
 junk : Cardinal;
 bCloseHandle : Boolean;
begin
 Result := false;
 bCloseHandle := Handle = INVALID_HANDLE_VALUE;
 if Handle = INVALID_HANDLE_VALUE then
  Handle := CreateFile(PChar(Format(cfsPhysDrive, [NumDsk])),
      GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
 if Handle = INVALID_HANDLE_VALUE then begin
  PrintFailure(10);
  exit
 end;

 ZeroMemory(@sda, sizeof(sda));
 sda.Version := sizeof(SET_DISK_ATTRIBUTES);
 sda.Reserved1 := 0;
 sda.Reserved2 := 0;
 sda.Attributes := DISK_ATTRIBUTE_OFFLINE;
 sda.AttributesMask := DISK_ATTRIBUTE_ONLINE;
 if DeviceIoControl(Handle, IOCTL_DISK_SET_DISK_ATTRIBUTES, @sda, sda.Version,
    nil, 0, junk, nil) then
   Result := DeviceIoControl(Handle, IOCTL_DISK_UPDATE_PROPERTIES, nil, 0, nil,
      0, junk, nil);
 if bCloseHandle then CloseHandle(Handle)
end;

function ResetStorageBus(DiskNum : Cardinal) : Boolean;
type
 SCSI_ADDRESS = record
  Length: DWORD; PortNumber, PathId, TargetId, Lun: BYTE;
 end;
 STORAGE_BUS_RESET_REQUEST = record
  PathId: BYTE;
 end;
 STORAGE_BREAK_RESERVATION_REQUEST = record
  Length: DWORD; _unused, PathId, TargetId, Lun: BYTE;
 end;
var
 Address: SCSI_ADDRESS; // Use Address as STORAGE_BREAK_RESERVATION_REQUEST
 Reset: STORAGE_BUS_RESET_REQUEST;
 junk: Cardinal;
 Handle: THandle;
begin
 Result := false;
 Handle := CreateFile(PWideChar(Format(cfsPhysDrive, [DiskNum])),
    GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
    OPEN_EXISTING, 0, 0);
 if Handle = INVALID_HANDLE_VALUE then exit;

 ZeroMemory(@Address, SizeOf(Address));
 Address.Length := SizeOf(Address);
 Result := DeviceIoControl(Handle, IOCTL_SCSI_GET_ADDRESS, nil, 0, @Address,
    SizeOf(Address), junk, nil);
 if Result then begin
  IoDeviceControl(Handle, IOCTL_STORAGE_FIND_NEW_DEVICES);
  CloseHandle(Handle);
  Handle := CreateFile(PWideChar(Format(cfsScsi, [Address.PortNumber])),
    GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
    OPEN_EXISTING, 0, 0);
  if Handle = INVALID_HANDLE_VALUE then exit;

  // if RESET_BUS failed, use BREAK_RESERVATION with SCSI_ADDRESS (same signature):
  Reset.PathId := Address.PathId;
  if DeviceIoControl(Handle, IOCTL_STORAGE_RESET_BUS, @Reset,
     SizeOf(Reset), nil, 0, junk, nil) then
   Result := true
  else
   Result := DeviceIoControl(Handle, IOCTL_STORAGE_BREAK_RESERVATION,
        @Address, SizeOf(Address), nil, 0, junk, nil);
  IoDeviceControl(Handle, IOCTL_SCSI_RESCAN_BUS)
 end else begin // not SCSI disk, call dismount volume for physical disk:
  Result := IoDeviceControl(Handle, FSCTL_DISMOUNT_VOLUME);
  DeviceIoControl(Handle, IOCTL_STORAGE_RESET_DEVICE, nil, 0, nil, 0, junk, nil)
 end;
 CloseHandle(Handle)
end;

{ GetSetDiskSignInSectorArray
  Gets\Sets disk identifier inside byte array of 1st 2 sectors.
}
procedure GetSetDiskSignInSectorArray(bGet: Boolean;
                                  var Sign: String;
                                  var SrAr: TByteDynArray;
                                      DTyp: Byte);
var
 Guid: TGuid;
 GptA: array[0..15] of Byte absolute Guid;
 MbrI: Cardinal;
 MbrA: array[0..4] of Byte absolute MbrI;
 i : Integer;
begin
 case DTyp of
 1: begin // Bytes ## 568..583 - disk identifier:
  if bGet then begin
   for i := 568 to 583 do GptA[i - 568] := SrAr[i];
   Sign := GuidToStr(Guid)
  end else begin
   Guid := StrToGuid(Sign);
   for i := 568 to 583 do SrAr[i] := GptA[i - 568]
  end;
 end;
 2: begin // Bytes ## 440..443 - disk identifier:
  if bGet then begin
   for i := 440 to 443 do MbrA[i - 440] := SrAr[i];
   Sign := Format('%x', [MbrI])
  end else begin
   MbrI := StrToUInt64(Sign);
   for i := 440 to 443 do SrAr[i] := MbrA[i - 440]
  end;
 end;
 else
  raise Exception.Create('GetSetDiskSignInSectorArray: Unknown disk type "' +
      IntToStr(DTyp) + '".')
 end
end;

{ PartInfoAction
  Function performs next actions:
   piaGetIDtype  : Reads disk information;
   piaChkAlloc   : Checks the disk is allocated, reads disk information;
   piaSetRandomID: Generates new MBR identifier of disk;
   piaSetID      : Sets new MBR identifier to disk;
   piaClone      : Clones disk information to another (TgtDskIndex).
  Returns true on success & disk identifier to sDskID, disk style to DskStyle.
  Failed call sets High(Byte) to DskStyle.
}
function PartInfoAction(pia: TPartInfoAction; DskIndex: Integer;
  var sDskID: String; var DskStyle: Byte; Handle: THandle = 0;
  TgtDskIndex: UInt64 = HighUInt64; TgtHdl: THandle = 0): Boolean;
type
 // Original description of objects is given by JEDI library reference:
 // https://github.com/graemeg/freepascal/blob/master/packages/winunits-jedi/src/jwawinioctl.pas
 PARTITION_STYLE = (   // 0 - MBR, 1 - GPT, 2 - RAW
  PARTITION_STYLE_MBR, PARTITION_STYLE_GPT, PARTITION_STYLE_RAW
 );
 PARTITION_INFORMATION_MBR = record
  PartitionType: BYTE;
  BootIndicator: BOOLEAN;
  RecognizedPartition: BOOLEAN;
  HiddenSectors: DWORD;
 end;
 PARTITION_INFORMATION_GPT = record
  PartitionType: TGUID; // Partition type. See table 16-3.
  PartitionId: TGUID; // Unique GUID for this partition.
  Attributes: DWORD64; // See table 16-4.
  Name: array [0..35] of WCHAR; // Partition Name in Unicode.
 end;
 PARTITION_INFORMATION_EX = record
  PartitionStyle: PARTITION_STYLE;
  StartingOffset: LARGE_INTEGER;
  PartitionLength: LARGE_INTEGER;
  PartitionNumber: DWORD;
  RewritePartition: BOOLEAN;
  case Integer of
  0: (Mbr: PARTITION_INFORMATION_MBR);
  1: (Gpt: PARTITION_INFORMATION_GPT);
 end;

 DRIVE_LAYOUT_INFORMATION_MBR = record
  Signature: DWORD;
 end;
 DRIVE_LAYOUT_INFORMATION_GPT = record
  DiskId: TGUID;
  StartingUsableOffset: LARGE_INTEGER;
  UsableLength: LARGE_INTEGER;
  MaxPartitionCount: DWORD;
 end;
 DRIVE_LAYOUT_INFORMATION_EX = record
  PartitionStyle: DWORD;
  PartitionCount: DWORD;
  Union: record
   case Integer of
   0: (Mbr: DRIVE_LAYOUT_INFORMATION_MBR);
   1: (Gpt: DRIVE_LAYOUT_INFORMATION_GPT);
  end;
  PartitionEntry: array [0..0] of PARTITION_INFORMATION_EX;
 end;

var
 junk : DWORD; buffer : array[0..4095] of BYTE;

 function SetDriveLayout(TgtHdl: THandle) : Boolean;
 begin
  Result := false;
  if DeviceIoControl(TgtHdl, IOCTL_DISK_SET_DRIVE_LAYOUT_EX, @buffer[0], junk,
     nil, 0, junk, nil) then
   Result := DeviceIoControl(TgtHdl, IOCTL_DISK_UPDATE_PROPERTIES, nil, 0,
       nil, 0, junk, nil)
  else
   DoOutputSysError(true)
 end;

var
 li : ^DRIVE_LAYOUT_INFORMATION_EX;
 bCloseHandle : Boolean;
 Guid : TGuid;
 i: Integer;
label
 exi, fai;
begin
 Result := false;
 bCloseHandle := Handle = 0;
 if Handle = 0 then
  Handle := CreateFile(PWChar(Format(cfsPhysDrive, [DskIndex])),
      GENERIC_WRITE or GENERIC_READ, 0, nil, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL, 0);
 if Handle = INVALID_HANDLE_VALUE then begin bCloseHandle := false; goto fai end;

 if not DeviceIoControl(Handle, IOCTL_DISK_GET_DRIVE_LAYOUT_EX, nil, 0,
    @buffer[0], 4096, junk, nil) then
  goto exi;
 li := @buffer[0];

 if li.PartitionCount = 0 then begin
   DskStyle := 0; sDskID := ''; goto exi
 end else
   case Li.PartitionEntry[0].PartitionStyle of
   PARTITION_STYLE_RAW: begin
     DskStyle := 0; sDskID := ''; goto exi
   end;
   PARTITION_STYLE_GPT: begin
     DskStyle := 1;
     case pia of
     piaSetRandomID, piaCloneRndID: begin
       Randomize;
       li.Union.GPT.DiskId.D1 := Random($FFFFFFFF);
       li.Union.GPT.DiskId.D2 := Random($FFFF);
       li.Union.GPT.DiskId.D3 := Random($FFFF);
       for i := 0 to 7 do li.Union.GPT.DiskId.D4[i] := Random($FF)
     end;
     piaCloneSetID:
       li.Union.Gpt.DiskId := StrToGuid(sDskID);
     piaSetID:
       li.Union.Gpt.DiskId := StrToGuid(sDskID);
     end;
     sDskID := GuidToStr(li.Union.Gpt.DiskId)
   end;
   PARTITION_STYLE_MBR: begin
     DskStyle := 2;
     case pia of
     piaSetRandomID, piaCloneRndID: begin
       Randomize; li.Union.Mbr.Signature := Random($FFFFFFFF)
     end;
     piaCloneSetID:
       li.Union.Mbr.Signature := StrToUInt64(sDskID, '-');
     piaSetID:
       li.Union.Mbr.Signature := StrToUInt64(sDskID, '-');
     end;
     sDskID := PadLeft(Format('%x', [li.Union.Mbr.Signature]), 8, '0')
   end; end;
 if pia = piaGetIDtype then begin Result := true; goto exi end;
 if pia = piaChkAlloc then begin
  Result := 1 < li.PartitionCount; goto exi
 end;

 if pia in [piaClone, piaCloneRndID, piaCloneSetID] then begin
  if TgtDskIndex < HighUInt64 then
   TgtHdl := CreateFile(PWChar('\\.\PHYSICALDRIVE' + IntToStr(TgtDskIndex)),
       GENERIC_WRITE or GENERIC_READ, 0, nil, OPEN_EXISTING,
       FILE_ATTRIBUTE_NORMAL, 0);
  if TgtHdl = INVALID_HANDLE_VALUE then exit;
  Result := SetDriveLayout(TgtHdl);
  if TgtDskIndex < HighUInt64 then CloseHandle(TgtHdl)
 end else
  Result := SetDriveLayout(Handle);

 if Result then begin
exi:
  if bCloseHandle then CloseHandle(Handle);
  exit
 end;
fai:
 if bCloseHandle then CloseHandle(Handle);
 DskStyle := High(Byte); sDskID := ''
end;

function PartInfoAction(pia: TPartInfoAction; DskNum: Integer;
                     Handle: THandle = 0;
                   DskNumID: UInt64 = HighUInt64;
                     TgtHdl: UInt64 = HighUInt64): Boolean;
var
 sDskID : String; DskStyle : Byte;
begin
 case pia of
 piaCloneSetID: begin
  Assert((DskNumID < HighUInt64) and (0 < TgtHdl), 'Wrong parameters. ' +
      'To specify signature of target disk use DskNumID for disk number '
      + 'and TgtHdl for new signature ID.');
  sDskID := Format('%x', [TgtHdl])
 end; piaSetID: begin
  Assert(DskNumID < HighUInt64, 'Incorrect ID value in parameter DskNumID');
  sDskID := Format('%x', [DskNumID])
 end;
 end;
 if pia in [piaClone, piaCloneRndID, piaCloneSetID] then
  if(DskNumID < HighUInt64) and ((TgtHdl <= 0) or (TgtHdl = INVALID_HANDLE_VALUE))then
   raise Exception.Create('Incorrect parameter DskNumID');

 Result := PartInfoAction(pia, DskNum, sDskID, DskStyle, Handle,
    DskNumID, TgtHdl)
end;

function GetGeometry(DriveNumber: Byte; var Geometry : TDiskGeometry) : Boolean;
begin
 Result := GetGeometry('\\.\PhysicalDrive' + IntToStr(DriveNumber), Geometry);
end;
function GetGeometry(Drive: String; var Geometry : TDiskGeometry) : Boolean;
var
 hFile : THandle; junk : Cardinal;
begin
 Result := GetFileHandle(Drive, GENERIC_READ, FILE_SHARE_READ, hFile); if not Result then exit;

 Result := DeviceIoControl(hFile, IOCTL_DISK_GET_DRIVE_GEOMETRY, nil, 0,
    @Geometry, SizeOf(TDiskGeometry),junk,nil) and (junk = SizeOf(TDiskGeometry));
 CloseHandle(hFile);
end;

function CompareGeometries(ScGeometry, TgGeometry : TDiskGeometry) : Boolean;
begin
 Result := (ScGeometry.Cylinders = TgGeometry.Cylinders)
        and(ScGeometry.MediaType = TgGeometry.MediaType)
        and(ScGeometry.TracksPerCylinder = TgGeometry.TracksPerCylinder)
        and(ScGeometry.SectorsPerTrack = TgGeometry.SectorsPerTrack)
        and(ScGeometry.BytesPerSector = TgGeometry.BytesPerSector)
end;

function GetDriveInfoImp(var MainExPartOffset: DWORD;
                                  DriveNumber: Byte; DriveInfo: PDriveInfo;
                               StartingSector: DWORD = 0;
                               BytesPerSector: DWORD = 512): Boolean;
const
 PartitionTableOffset = $1be;
 ExtendedPartitions = [5, $f];
var
 buf: array of Byte; CurExPartOffset: DWORD; i: Integer;
begin
 SetLength(buf, BytesPerSector);
 if ReadSectors(DriveNumber, MainExPartOffset + StartingSector, 1, @buf[0]) = 0 then
 begin
  Result := False;
  exit
 end;
 Move(buf[PartitionTableOffset], DriveInfo.PartitionTable, SizeOf(TPartitionTable));
 Finalize(buf);

 Result := True;
 for i := 0 to 3 do // for every record of Partition Table
  if DriveInfo.PartitionTable[i].SystemIndicator in ExtendedPartitions then
  begin
   New(DriveInfo.LogicalDrives[i]);
   if MainExPartOffset = 0 then
   begin
    MainExPartOffset := DriveInfo.PartitionTable[i].StartingSector;
    CurExPartOffset := 0;
   end else CurExPartOffset := DriveInfo.PartitionTable[i].StartingSector;
   Result := Result and GetDriveInfoImp(MainExPartOffset,
                                        DriveNumber, DriveInfo.LogicalDrives[i],
                                        CurExPartOffset)
  end else
   DriveInfo.LogicalDrives[i] := nil
end;

function GetDriveInfo(DriveNumber: Byte; DriveInfo: PDriveInfo;
                   StartingSector: DWORD = 0; BytesPerSector: DWORD = 512): Boolean;
var
 MainExPartOffset: DWORD;
begin
 MainExPartOffset := 0;
 Result := GetDriveInfoImp
      (MainExPartOffset, DriveNumber, DriveInfo, StartingSector, BytesPerSector)
end;

function CompareDrivesInfo(Drive1I, Drive2I : TDriveInfo) : Boolean;
var
 i, j : Integer;
 function ComparePartitions(P1, P2 : TPartitionTableEntry) : Boolean;
 begin
  Result := (P1.BootIndicator = P2.BootIndicator)
  and (P1.StartingHead = P2.StartingHead)
  and (P1.StartingCylAndSect = P2.StartingCylAndSect)
  and (P1.SystemIndicator = P2.SystemIndicator)
  and (P1.EndingHead = P2.EndingHead)
  and (P1.EndingCylAndSect = P2.EndingCylAndSect)
  and (P1.StartingSector = P2.StartingSector)
  and (P1.NumberOfSects = P2.NumberOfSects);
 end;
begin
 Result := false;
 for i := 0 to Length(Drive1I.PartitionTable) - 1 do
  if not ComparePartitions(Drive1I.PartitionTable[i],
   Drive2I.PartitionTable[i]) then
    exit;
 for i := 0 to Length(Drive1I.LogicalDrives) - 1 do
  if (Drive1I.LogicalDrives[i] <> nil)
  and(Drive2I.LogicalDrives[i] <> nil)then begin
   for j := 0 to Length(Drive1I.PartitionTable) - 1 do
    if not ComparePartitions(Drive1I.LogicalDrives[i].PartitionTable[j],
                             Drive2I.LogicalDrives[i].PartitionTable[j]) then
     exit
  end else if (Drive1I.LogicalDrives[i] = nil)
           and(Drive2I.LogicalDrives[i] = nil) then
            break
           else
            exit;
 Result := true
end;

function CompareDrivesInfo(Drive1Num, Drive2Num : Byte) : Boolean;
var
 Drive1I, Drive2I : TDriveInfo;
begin
 Result := false;
 if not GetDriveInfo(Drive1Num, @Drive1I, 0) then exit else
  if not GetDriveInfo(Drive2Num, @Drive2I, 0) then exit;

 Result := CompareDrivesInfo(Drive1I, Drive2I)
end;

end.
