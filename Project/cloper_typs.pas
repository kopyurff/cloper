{ cloper_typs
  The unit of Cloning Optimizer (Cloper) tool.

  The basic unit encapsulates declarations of types, constants & external
  functions.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit cloper_typs;

interface
uses
 Windows, Types;

type
 _ControlBlock = record // All msdos data structures must be packed on a 1 byte boundary
   StartingSector : DWORD;
	 NumberOfSectors : WORD;
   pBuffer : DWORD;
 end;
 _DIOC_REGISTERS = record
   reg_EBX : DWORD;
   reg_EDX : DWORD;
   reg_ECX : DWORD;
   reg_EAX : DWORD;
   reg_EDI : DWORD;
   reg_ESI : DWORD;
   reg_Flags : DWORD;
 end;

 PartitionType = (FAT12 = 1, FAT16A = 4, EXTENDEDA = 5,
                             FAT16B = 6, NTFS = 7,
                             FAT32 = 11, EXTENDEDB = 15);

 TDriveBusType = (
  dbtUnknown, dbtScsi, dbtAtapi, dbtAta, dbt1394, dbtSsa, dbtFibre,
  dbtUsb, dbtRAID, dbtiScsi, dbtSas, dbtSata, dbtSd, dbtMmc, dbtVirtual,
  dbtFileBackedVirtual
 );

 TDeviceType = (
  dtUnknown ); // todo: implement

 TDriveInfoResult = record
  Drive:        string;
  VendorID:     string;
  ProductID:    string;
  Revision:     string;
  Serial:       string;
  BusType:      TDriveBusType;
  Removable:    Boolean;
  //
  DeviceType:   TDeviceType;
  DeviceNumber: Integer;
  Partition:    Integer;
 end;

 TDiskGeometry = packed record
   Cylinders: Int64;

   MediaType: DWORD;
   TracksPerCylinder: DWORD;
   SectorsPerTrack: DWORD;
   BytesPerSector: DWORD;
 end;

 TPartitionTableEntry = packed record
  BootIndicator: Byte;          // $80, active volume
  StartingHead: Byte;
  StartingCylAndSect: Word;
  SystemIndicator: Byte; // Byte(PartitionType)
  EndingHead: Byte;
  EndingCylAndSect: Word;
  StartingSector: DWORD;
  NumberOfSects: DWORD;
 end;
 TPartitionTable = packed array [0..3] of TPartitionTableEntry;
 PDriveInfo = ^TDriveInfo;
 TDriveInfo = record
  PartitionTable: TPartitionTable;
  LogicalDrives: array [0..3] of PDriveInfo;
 end;

 _UNICODE_STRING = record
   Length: USHORT;
   MaximumLength: USHORT;
   Buffer: PWideChar;
 end;
 UNICODE_STRING = _UNICODE_STRING;

 PObjectAttributes = ^TObjectAttributes;
 TObjectAttributes = packed record
  Length: Cardinal;
  RootDirectory: THandle;
  ObjectName: PUnicodeString;
  Attributes: Cardinal;
  SecurityDescriptor: Pointer;
  SecurityQualityOfService: Pointer;
 end;

 PPARTITION_INFORMATION = ^PARTITION_INFORMATION;
 {$EXTERNALSYM PPARTITION_INFORMATION}
 _PARTITION_INFORMATION = record
  StartingOffset : LARGE_INTEGER;
  PartitionLength : LARGE_INTEGER;
  HiddenSectors : DWORD;
  PartitionNumber : DWORD;
  PartitionType : BYTE;
  BootIndicator : ByteBool;
  RecognizedPartition : ByteBool;
  RewritePartition : ByteBool;
 end;
 {$EXTERNALSYM _PARTITION_INFORMATION}
 PARTITION_INFORMATION = _PARTITION_INFORMATION;
 {$EXTERNALSYM PARTITION_INFORMATION}
 TPartitionInformation = PARTITION_INFORMATION;
 PPartitionInformation = PPARTITION_INFORMATION;

 PDRIVE_LAYOUT_INFORMATION = ^DRIVE_LAYOUT_INFORMATION;
 DRIVE_LAYOUT_INFORMATION = record
  PartitionCount : DWORD;
  Signature : DWORD;
  PartitionEntry : array [0..0] of PARTITION_INFORMATION;
 end;

// Structures for FSCTL_GET_NTFS_VOLUME_DATA.
// The user must pass the basic buffer below.  Ntfs
// will return as many fields as available in the extended
// buffer which follows immediately after the VOLUME_DATA_BUFFER.
// http://www.developpez.net/forums/d705267/environnements-developpement/delphi/api-com-sdks/lecture-mft-fat-d-volume-ntfs/

  PNTFS_VOLUME_DATA_BUFFER = ^NTFS_VOLUME_DATA_BUFFER;
  {$EXTERNALSYM PNTFS_VOLUME_DATA_BUFFER}
  NTFS_VOLUME_DATA_BUFFER = record
   VolumeSerialNumber: LARGE_INTEGER;
   NumberSectors: LARGE_INTEGER;
   TotalClusters: LARGE_INTEGER;
   FreeClusters: LARGE_INTEGER;
   TotalReserved: LARGE_INTEGER;
   BytesPerSector: DWORD;
   BytesPerCluster: DWORD;
   BytesPerFileRecordSegment: DWORD;
   ClustersPerFileRecordSegment: DWORD;
   MftValidDataLength: LARGE_INTEGER;
   MftStartLcn: LARGE_INTEGER;
   Mft2StartLcn: LARGE_INTEGER;
   MftZoneStart: LARGE_INTEGER;
   MftZoneEnd: LARGE_INTEGER;
  end;
  {$EXTERNALSYM NTFS_VOLUME_DATA_BUFFER}
  TNtfsVolumeDataBuffer = NTFS_VOLUME_DATA_BUFFER;
  PNtfsVolumeDataBuffer = PNTFS_VOLUME_DATA_BUFFER;

 PSTARTING_VCN_INPUT_BUFFER = ^STARTING_VCN_INPUT_BUFFER;
 {$EXTERNALSYM PSTARTING_VCN_INPUT_BUFFER}
 STARTING_VCN_INPUT_BUFFER = packed record
  StartingVcn: LARGE_INTEGER;
 end;
 {$EXTERNALSYM STARTING_VCN_INPUT_BUFFER}
 TStartingVcnInputBuffer = STARTING_VCN_INPUT_BUFFER;
 PStartingVcnInputBuffer = PSTARTING_VCN_INPUT_BUFFER;

 PSTARTING_LCN_INPUT_BUFFER = ^STARTING_LCN_INPUT_BUFFER;
 {$EXTERNALSYM PSTARTING_LCN_INPUT_BUFFER}
 STARTING_LCN_INPUT_BUFFER = record
  StartingLcn: LARGE_INTEGER;
 end;
 {$EXTERNALSYM STARTING_LCN_INPUT_BUFFER}
 TStartingLcnInputBuffer = STARTING_LCN_INPUT_BUFFER;
 PStartingLcnInputBuffer = PSTARTING_LCN_INPUT_BUFFER;

 PVOLUME_BITMAP_BUFFER = ^VOLUME_BITMAP_BUFFER;
 {$EXTERNALSYM PVOLUME_BITMAP_BUFFER}
 VOLUME_BITMAP_BUFFER = record
  StartingLcn: LARGE_INTEGER;
  BitmapSize: LARGE_INTEGER;
  Buffer: array [0..0] of BYTE;
 end;
 {$EXTERNALSYM VOLUME_BITMAP_BUFFER}
 TVolumeBitmapBuffer = VOLUME_BITMAP_BUFFER;
 PVolumeBitmapBuffer = PVOLUME_BITMAP_BUFFER;

 TExt = packed record
  NextVcn: LARGE_INTEGER;
  Lcn:     LARGE_INTEGER;
 end;
 TExclude = record
  Start_ : ULONG64;
  End_ : ULONG64;
 end;

 PRETRIEVAL_POINTERS_BUFFER = ^_RETRIEVAL_POINTERS_BUFFER;
_RETRIEVAL_POINTERS_BUFFER = packed record
  ExtentCount: DWORD;
  StartingVcn: TLargeInteger;
  Extents: array[0..0] of TExt;
 end;

type
 TVolSpecs = record
  SectorsPerCluster, BytesPerSector, NumOfFreeClusters, TotalNumOfClusters: Longword;
  DeviceType, DeviceNumber, PartitionNumber, VolumeNumber: Integer;
    // \\.\DeviceName%DeviceNumber%
  Root, GUID, RtGUID, ULab : String;
  ErrorCode : Byte;
 end;

 TTstEvents = (teHdlFail = 0, teDfrFail = 1, teCprFail = 2,
               teCprSkip = 3, teDatChng = 4, teDelFail = 5,
               teVltDmgd = 6);
 TReportTestingEvent = procedure(ETyp: TTstEvents; FilePath: String;
                              bMarker: Boolean = false);

 PLARGE_INTEGER = ^LARGE_INTEGER;
 TLargeIntegerDynArray = array of LARGE_INTEGER;
 PLargeIntegerDynArray = ^TLargeIntegerDynArray;
 PIntegerDynArray = ^TIntegerDynArray;
 TUInt64DynArray = array of UINT64;
 PUInt64DynArray = ^TUInt64DynArray;
 TExtendedDynArray = array of Extended;

 TExtent = packed record
  Vcs: Cardinal;
  Lcn: LARGE_INTEGER;
  Chc: TUInt64DynArray;
 end;
 PExtent = ^TExtent;
 TExtents = array of TExtent;
 PExtents = ^TExtents;

 FILE_LAYOUT_INFORMATION = record
  FName : String;
  ClusCount : DWORD;
  State : Byte; // 1 - dropped, 2 - in use, 4 - deferred, 8 - done
                // 16 - to be compressed, 32 - compression done,
                // 64 - damaged or volatile;
  Extents: TExtents;
  HC : TUInt64DynArray;
 end;
 TFileLayoutInformation = FILE_LAYOUT_INFORMATION;
 PFileLayoutInformation = ^TFileLayoutInformation;
 TFilesLayoutInformation = record
  ClusCount : DWORD;
  Data : array of TFileLayoutInformation;
  LcnIdx, HCidx, HCbuf : TLargeIntegerDynArray;
  SizIdx : array of Cardinal;
  SzHC: Cardinal;
  bHCini: Boolean;
 end;
 PFilesLayoutInformation = ^TFilesLayoutInformation;
 TVolLabel = record
  VLab : String;
  ULab : String;
  Valid: Byte;
  GUID : String;
 end;
 TVolLabels = array of TVolLabel;

 THashCodes = record
  Lcn: UINT64;
  Siz: Cardinal;
  Cod : TUInt64DynArray;
  Ind : TIntegerDynArray;
 end;

 TFB0File = record
  Size : Int64;
  Handle : THandle;
  FileName : String;
 end;

// function SetFileInformationByHandle(hFile: THandle;
//              FileInformationClass: FILE_INFO_BY_HANDLE_CLASS;
//              lpFileInformation: Pointer; dwBufferSize: DWORD): BOOL; stdcall;
//              external 'kernel32.dll' delayed;
 function FindFirstVolume(lpszVolumeName: PChar;
              cchBufferLength: DWORD): THandle;
            stdcall external Kernel32 name 'FindFirstVolumeA';
 function FindNextVolume(hFindVolume: THandle;
              lpszVolumeName: PChar; cchBufferLength: DWORD): BOOL;
            stdcall external Kernel32 name 'FindNextVolumeA';
 function FindVolumeClose(hFindVolume: THandle): BOOL;
            stdcall external Kernel32 name 'FindVolumeClose';
 function GetVolumeNameForVolumeMountPoint(lpszVolumeMountPoint
              : LPCSTR; lpszVolumeName: LPSTR; cchBufferLength: DWORD): BOOL;
            stdcall; external 'kernel32.dll' name 'GetVolumeNameForVolumeMountPointA';
 function FindFirstVolumeMountPoint(lpszRootPathName,
              lpszVolumeMountPoint: PChar; cchBufferLength: DWORD): THandle;
            stdcall external Kernel32 name 'FindFirstVolumeMountPointA';
 function FindNextVolumeMountPoint(hFindVolumeMountPoint: THandle;
              lpszVolumeMountPoint: PChar; cchBufferLength: DWORD): BOOL;
            stdcall external Kernel32 name 'FindNextVolumeMountPointA';
 function FindVolumeMountPointClose(hFindVolumeMountPoint
              : THandle): BOOL;
            stdcall external Kernel32 name 'FindVolumeMountPointClose';
 function GetVolumePathName(lpszFileName: LPCTSTR;
              lpszVolumePathName: LPTSTR; cchBufferLength: DWORD): BOOL;
            stdcall external Kernel32 name 'GetVolumePathNameA';
 function GetVolumePathNamesForVolumeName(lpszVolumeName: LPCTSTR;
              lpszVolumePathNames: LPTSTR; cchBufferLength: DWORD;
              lpcchReturnLength: PDWORD): BOOL;
            stdcall external Kernel32 name 'GetVolumePathNamesForVolumeNameA';
 function DeleteVolumeMountPoint(lpszVolumeMountPoint: LPCSTR)
                : BOOL; stdcall;
                external 'Kernel32.dll' name 'DeleteVolumeMountPointA';

 procedure RtlInitUnicodeString(DestinationString: PUnicodeString;
   SourceString: LPWSTR); stdcall; external 'ntdll.dll';
 function NtDeleteFile(ObjectAttributes: PObjectAttributes): DWORD; stdcall;
   external 'ntdll.dll';
 function RtlDosPathNameToNtPathName_U(DosName: PWChar;
   var NtName: UNICODE_STRING; DosFilePath: PPChar; NtFilePath: PUnicodeString)
   : BOOL; stdcall; external 'ntdll.dll';

 function GetConsoleWindow: HWND; stdcall; external Kernel32;

const
  HighUInt64 = High(UInt64);
  HighUInt32 = High(UInt32);

  WINDOWS_STRING_LENGTH = $0400;

  COMPRESSION_FORMAT_NONE     = ($0000);
  {$EXTERNALSYM COMPRESSION_FORMAT_NONE}
  COMPRESSION_FORMAT_DEFAULT  = ($0001);
  {$EXTERNALSYM COMPRESSION_FORMAT_DEFAULT}
  COMPRESSION_FORMAT_LZNT1    = ($0002);
  {$EXTERNALSYM COMPRESSION_FORMAT_LZNT1}

  // Declarations of IOCTL control codes:
  IOCTL_SCSI_BASE = FILE_DEVICE_CONTROLLER;

  IOCTL_SCSI_RESCAN_BUS = (IOCTL_SCSI_BASE shl 16) or
    (FILE_ANY_ACCESS shl 14) or ($0407 shl 2) or (METHOD_BUFFERED);
  IOCTL_SCSI_GET_INQUIRY_DATA = (IOCTL_SCSI_BASE shl 16) or
    (FILE_ANY_ACCESS shl 14) or ($0403 shl 2) or (METHOD_BUFFERED);
  IOCTL_SCSI_GET_CAPABILITIES = (IOCTL_SCSI_BASE shl 16) or
    (FILE_ANY_ACCESS shl 14) or ($0404 shl 2) or (METHOD_BUFFERED);
  IOCTL_SCSI_GET_ADDRESS = (IOCTL_SCSI_BASE shl 16) or
    (FILE_ANY_ACCESS shl 14) or ($0406 shl 2) or (METHOD_BUFFERED);
  IOCTL_SCSI_MINIPORT = (IOCTL_SCSI_BASE shl 16) or
    (FILE_ANY_ACCESS shl 14) or ($0402 shl 2) or (METHOD_BUFFERED);
  IOCTL_SCSI_PASS_THROUGH = (IOCTL_SCSI_BASE shl 16) or
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($0401 shl 2) or
    (METHOD_BUFFERED);
  IOCTL_SCSI_PASS_THROUGH_DIRECT = (IOCTL_SCSI_BASE shl 16) or
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($0405 shl 2) or
    (METHOD_BUFFERED);
  IOCTL_DISK_ARE_VOLUMES_READY = (IOCTL_DISK_BASE shl 16) or
    ((FILE_READ_ACCESS) shl 14) or ($0087 shl 2) or (METHOD_BUFFERED);
  IOCTL_DISK_VOLUMES_ARE_READY = (IOCTL_DISK_BASE shl 16) or
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($0088 shl 2) or
    (METHOD_BUFFERED);

  IOCTL_HID_FLUSH_QUEUE = (FILE_DEVICE_KEYBOARD shl 16) or
    ((FILE_ANY_ACCESS) shl 14) or ($0065 shl 2) or (METHOD_NEITHER);

  MOUNTMGRCONTROLTYPE = $6d; // == Ord('m')
  IOCTL_MOUNTMGR_CREATE_POINT = (MOUNTMGRCONTROLTYPE shl 16) or
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($0 shl 2) or
    (METHOD_BUFFERED);
  IOCTL_MOUNTMGR_DELETE_POINTS = (MOUNTMGRCONTROLTYPE shl 16) or
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($1 shl 2) or
    (METHOD_BUFFERED);
  IOCTL_MOUNTMGR_QUERY_POINTS = (MOUNTMGRCONTROLTYPE shl 16) or
    ((FILE_ANY_ACCESS) shl 14) or ($2 shl 2) or (METHOD_BUFFERED);
  IOCTL_MOUNTMGR_DELETE_POINTS_DBONLY = (MOUNTMGRCONTROLTYPE shl 16) or
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($3 shl 2) or
    (METHOD_BUFFERED);
  IOCTL_MOUNTMGR_NEXT_DRIVE_LETTER = (MOUNTMGRCONTROLTYPE shl 16) or
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($4 shl 2) or
    (METHOD_BUFFERED);
  IOCTL_MOUNTMGR_AUTO_DL_ASSIGNMENTS = (MOUNTMGRCONTROLTYPE shl 16) or
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($5 shl 2) or
    (METHOD_BUFFERED);
  IOCTL_MOUNTMGR_VOLUME_MOUNT_POINT_CREATED = (MOUNTMGRCONTROLTYPE shl 16) or
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($6 shl 2) or
    (METHOD_BUFFERED);
  IOCTL_MOUNTMGR_VOLUME_MOUNT_POINT_DELETED = (MOUNTMGRCONTROLTYPE shl 16) or
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($7 shl 2) or
    (METHOD_BUFFERED);
  IOCTL_MOUNTMGR_CHANGE_NOTIFY = (MOUNTMGRCONTROLTYPE shl 16) or
    ((FILE_READ_ACCESS) shl 14) or ($8 shl 2) or
    (METHOD_BUFFERED);
  IOCTL_MOUNTMGR_KEEP_LINKS_WHEN_OFFLINE = (MOUNTMGRCONTROLTYPE shl 16) or
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($9 shl 2) or
    (METHOD_BUFFERED);
  IOCTL_MOUNTMGR_CHECK_UNPROCESSED_VOLUMES = (MOUNTMGRCONTROLTYPE shl 16) or
    ((FILE_READ_ACCESS) shl 14) or ($A shl 2) or
    (METHOD_BUFFERED);
  IOCTL_MOUNTMGR_VOLUME_ARRIVAL_NOTIFICATION = (MOUNTMGRCONTROLTYPE shl 16) or
    ((FILE_READ_ACCESS) shl 14) or ($B shl 2) or
    (METHOD_BUFFERED);
  IOCTL_MOUNTMGR_QUERY_AUTO_MOUNT = (MOUNTMGRCONTROLTYPE shl 16) or
    ((FILE_ANY_ACCESS) shl 14) or ($E shl 2) or
    (METHOD_BUFFERED);
  IOCTL_MOUNTMGR_SET_AUTO_MOUNT = (MOUNTMGRCONTROLTYPE shl 16) or
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($F shl 2) or
    (METHOD_BUFFERED);

  //IOCTL_STORAGE_REINITIALIZE_MEDIA = ?

implementation

end.
