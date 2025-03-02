{ Defragger
  The unit of Cloning Optimizer (Cloper) tool.

  Contains functionality for retrieving and defragmentation of volume data.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit Defragger;

interface
uses
 Windows, Types, ConTools, cloper_echo, cloper_typs;

function InitVolumeData(var Handle: THandle; VolumeName: String): Byte;

function FindFreeBlock(VolumeHandle: THandle; MinimumLcn: ULONG64;
  MinimumSize: DWORD; var BeginLcn: ULONG64; var EndLcn: ULONG64): Boolean;

function MaxFreeBlockOfFragment(VolumeHandle: THandle; var BeginLcn: ULONG64;
  var EndLcn: ULONG64): Cardinal;
function GetNextFreeBlock(VolumeHandle: THandle; var BeginLcn: ULONG64;
  var EndLcn: ULONG64; StartLcn: ULONG64 = HighUInt64): Cardinal;

function MoveToLcn(VolumeHandle: THandle; NewLcn: UInt64; FileName: String;
                   var RealClusters: DWORD;
                   var Lcn: UInt64;
                   bWait : Boolean = false;
                   Strategy: Integer = 0): Integer;

function AnalyzeFile(FileName: String; var Fragments: DWORD;
  var RealClusters: DWORD; var Lcn: ULONG64;
  bCompressedClusters: Boolean = true): Boolean;

function GetFileClusters(VolId, FileName : String;
                             var Extents : TExtents) : Cardinal; overload;
function GetFileClusters(VolId, FileName : String;
                          var FileLayout : TFileLayoutInformation) : Boolean; overload;
function GetFreeBlocks(VolumeHandle: THandle;
                        var Extents: TExtents;
                           BeginLcn: ULONG64 = 0;
                             EndLcn: ULONG64 = HighUInt64;
                           StartLcn: ULONG64 = HighUInt64): Cardinal;

function IsNotSystemLcn(Lcn : LARGE_INTEGER) : Boolean;
function MinLcnForFile(Siz: Cardinal) : UINT64;

var
 MaxLcn : ULONG64 = 0;
implementation
uses
 SysUtils, StrUtils, cloper_base;

const
 szVOLUME_BITMAP_BUFFER = sizeof(VOLUME_BITMAP_BUFFER) + 1024 * 1024 * 40;
 szSTARTING_LCN_INPUT_BUFFER = sizeof(STARTING_LCN_INPUT_BUFFER);
var
 Excludes : array[0..2] of TExclude;
 ExsOrder : array[0..2] of Integer;

{ GetFileClusters
  Obtains file layouts in volume & common number of clusters.
}
function GetFileClusters(VolId, FileName : String; var Extents : TExtents) : Cardinal;
var
  FileHandle : THandle;
  InBuffer : STARTING_VCN_INPUT_BUFFER;
  PreviousVcn : ULONG64;
  PreviousLcn : ULONG64;
  Lcn : ULONG64; Fragments : DWORD;
  Res, i, ClusCount : Integer;
  d, w : DWORD;
  ExtentData : PRETRIEVAL_POINTERS_BUFFER;
  bytes : array of Byte;
begin
 Fragments := 0; ClusCount := 0; Result := 0; Lcn := 0;
 SetLength(Extents, 0);

 // Open the item as a file or as a directory. If the item could
 // not be opened then exit.
 FileHandle := CreateFile(PWChar(VolId + FileName), FILE_READ_ATTRIBUTES,
       FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
       nil, OPEN_EXISTING, FILE_FLAG_NO_BUFFERING, 0);
 if FileHandle = INVALID_HANDLE_VALUE then exit;

 // Get the clustermap of this file. The loop will repeat if there
 //   are more datablocks than fit in the Data.
 InBuffer.StartingVcn.QuadPart := 0;
 PreviousLcn := High(ULONG64);
 PreviousVcn := 0;
 repeat
  SetLength(bytes, 1024 * 1024); // Minimum size 32 bytes.

  Res := Integer(DeviceIoControl(FileHandle, FSCTL_GET_RETRIEVAL_POINTERS,
    @InBuffer, sizeof(STARTING_VCN_INPUT_BUFFER), @Bytes[0],
    Length(Bytes), w, nil));
  if Res = 0 then begin
   Res := GetLastError();
   if Res <> ERROR_MORE_DATA then begin
    CloseHandle(FileHandle);
    exit
   end;
  end;
  ExtentData := @bytes[0];
  // Count the number of fragments, and calculate the total number of
  // clusters used by this file.
  if PreviousVcn = 0 then
   PreviousVcn := ExtentData.StartingVcn;
  for d := 0 to ExtentData.ExtentCount - 1 do begin
   FlipHighLow(ExtentData.Extents[d].NextVcn);
   FlipHighLow(ExtentData.Extents[d].Lcn);

   if PreviousLcn = High(ULONG64) then
    Lcn := UInt64(ExtentData.Extents[d].Lcn);
   if ExtentData.Extents[d].Lcn.QuadPart <> -1 then begin
    if UInt64(ExtentData.Extents[d].Lcn) <> PreviousLcn then begin
     SetLength(Extents, Fragments + 1);
     Extents[Fragments].Vcs := Extents[Fragments].Vcs
                  + DWORD(UInt64(ExtentData.Extents[d].NextVcn) - PreviousVcn);
     Extents[Fragments].Lcn := ExtentData.Extents[d].Lcn;
     Fragments := Fragments + 1
    end else
     Extents[Fragments - 1].Vcs := Extents[Fragments - 1]
             .Vcs + DWORD(UInt64(ExtentData.Extents[d].NextVcn) -
             PreviousVcn);
    PreviousLcn := ExtentData.Extents[d].Lcn.QuadPart +
          DWORD(UInt64(ExtentData.Extents[d].NextVcn) - PreviousVcn);
    ClusCount := ClusCount +
          DWORD(UInt64(ExtentData.Extents[d].NextVcn) - PreviousVcn);
   end;
   if ExtentData.Extents[d].NextVcn.QuadPart <> -1 then
    // -1 == ERROR_MORE_DATA & factual end of file with LowPart == Sector#
    if ExtentData.Extents[d].NextVcn.HighPart < 0 then
     PreviousVcn := ExtentData.Extents[d].NextVcn.LowPart
    else
     PreviousVcn := ExtentData.Extents[d].NextVcn.QuadPart
  end;
  // Next datablock.
  if Res = ERROR_MORE_DATA then
    InBuffer.StartingVcn.QuadPart := ExtentData.Extents[d - 1]
      .NextVcn.QuadPart
 until Res <> ERROR_MORE_DATA;
 CloseHandle(FileHandle); SetLength(bytes, 0);

 Assert(0 <= ClusCount, 'Negative number of file clusters.');
 Result := ClusCount
end;

{ GetFileClusters
  Obtains the file layout in volume. If the file could not be opened or the
  file is very small and stored by Windows inside MFT then it returns false.
}
function GetFileClusters(VolId, FileName : String;
                         var FileLayout : TFileLayoutInformation) : Boolean;
begin
 FileLayout.FName := FileName;
 FileLayout.ClusCount := GetFileClusters(VolId, FileName, FileLayout.Extents);
 Result := 0 < FileLayout.ClusCount
end;

{ InitVolumeData
  Preliminary intialization of data for volume. Returns INVALID_HANDLE_VALUE in
  the case if couldn' open volume or if the queries of NTFS data had unexpected
  response. The return byte value:
  0: Failed to obtain handle;
  1: FSCTL_GET_VOLUME_BITMAP gave unexpected result;
  2: Success.
}
function InitVolumeData(var Handle: THandle; VolumeName: String): Byte;
label memfree; var
  InBuffer : ^STARTING_LCN_INPUT_BUFFER;
  Data : ^VOLUME_BITMAP_BUFFER;
  Res : Integer;
  NtfsData : NTFS_VOLUME_DATA_BUFFER;
  w : DWORD; Lcn, CntFClus: UINT64;
  Index : Integer;
  IndexMax : DWORD;
  Mask : BYTE;
 procedure GetExsOrder;
 var
  i, j, k, m: Integer;
 begin
  i := 0;
  for j := 0 to 2 do if Excludes[j].Start_ < Excludes[i].Start_ then i := j;
  j := 0; ExsOrder[j] := i;
  repeat
   k := -1;
   for m := 0 to 2 do
    if Excludes[i].Start_ < Excludes[m].Start_ then
     if k < 0 then k := m else
      if Excludes[m].Start_ < Excludes[k].Start_ then k := m;
   i := k; Inc(j); ExsOrder[j] := i
  until j = 2
 end;
begin
 // Initialize.
 Excludes[0].Start_ := 0;
 Excludes[0].End_ := 0;
 Excludes[1].Start_ := 0;
 Excludes[1].End_ := 0;
 Excludes[2].Start_ := 0;
 Excludes[2].End_ := 0;
 MaxLCN := 0;
 Result := 0;
 // Open the Volume's Handle. If error then leave.
 Handle := CreateFile(PWChar(ExcludeTrailingBackslash(VolumeName)),
                      GENERIC_READ,
                      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING,
                      FILE_FLAG_NO_BUFFERING or FILE_FLAG_RANDOM_ACCESS, 0);
 if Handle = INVALID_HANDLE_VALUE then exit;

 // Setup the list of clusters that cannot be used. The Master File
 // Table cannot be moved and cannot be used by files. All this is
 // only necessary for NTFS volumes.
 Res := Integer(DeviceIoControl(Handle, FSCTL_GET_NTFS_VOLUME_DATA,
        nil, 0, @NtfsData, SizeOf(NtfsData), w, nil));
 if Res <> 0 then begin
  // Note: NtfsData.TotalClusters.QuadPart should be exactly the same
  //       as the MaxLcn that was determined in the previous block.
  Excludes[0].Start_ := NtfsData.MftStartLcn.QuadPart;
  Excludes[0].End_   := NtfsData.MftStartLcn.QuadPart +
      NtfsData.MftValidDataLength.QuadPart div NtfsData.BytesPerCluster;
  Excludes[1].Start_ := NtfsData.MftZoneStart.QuadPart;
  Excludes[1].End_   := NtfsData.MftZoneEnd.QuadPart;
  Excludes[2].Start_ := NtfsData.Mft2StartLcn.QuadPart;
  Excludes[2].End_   := NtfsData.Mft2StartLcn.QuadPart +
      NtfsData.MftValidDataLength.QuadPart div NtfsData.BytesPerCluster
 end else begin
  CloseHandle(Handle); Handle := INVALID_HANDLE_VALUE; exit
 end;
 GetExsOrder; // Get order of Excludes regions:

 // Check result of FSCTL_GET_VOLUME_BITMAP
 GetMem(Data, szVOLUME_BITMAP_BUFFER);
 GetMem(InBuffer, szSTARTING_LCN_INPUT_BUFFER);
 CntFClus := 0; Lcn := 0;
 repeat
  // Fetch a block of cluster data.
  InBuffer^.StartingLcn.QuadPart := Lcn;
  Res := Integer(DeviceIoControl(Handle, FSCTL_GET_VOLUME_BITMAP,
    InBuffer, szSTARTING_LCN_INPUT_BUFFER, Data, szVOLUME_BITMAP_BUFFER,
    IndexMax, nil));
  if Res = 0 then begin
   Res := GetLastError();
   if Res <> ERROR_MORE_DATA then goto memfree
  end;
  Lcn := UInt64(Data^.StartingLcn); Index := 0; Mask := 1;
  if UInt64(Data^.BitmapSize) div 8 < IndexMax then
   IndexMax := UInt64(Data^.BitmapSize) div 8;
  while Index < IndexMax do begin
   if Data^.Buffer[Index] and Mask = 0 then Inc(CntFClus);
   if Mask = 128 then begin
    Mask := 1;
    Index := Index + 1
   end else
    Mask := Mask shl 1;
   Lcn := Lcn + 1
  end;
 until(Res <> ERROR_MORE_DATA)
 and  (Lcn < UInt64(Data^.StartingLcn) + UInt64(Data^.BitmapSize));

 MaxLcn := UInt64(Data^.StartingLcn) + UInt64(Data^.BitmapSize);
 if CntFClus + MaxLcn <= UInt64(NtfsData.FreeClusters) + Lcn then
  CntFClus := UInt64(NtfsData.FreeClusters) + Lcn - CntFClus - MaxLcn
 else
  CntFClus := CntFClus + MaxLcn - UInt64(NtfsData.FreeClusters) - Lcn;

 if UInt64(NtfsData.FreeClusters) div 100 < CntFClus then begin
  CloseHandle(Handle); Handle := INVALID_HANDLE_VALUE; Result := 1
 end else
  Result := 2;
memfree:
 FreeMem(Data, szVOLUME_BITMAP_BUFFER);
 FreeMem(InBuffer, szSTARTING_LCN_INPUT_BUFFER)
end;

{ AnalyzeFile
  Returns number of file fragments and size of file in clusters. If the file
  couldn't be opened then return zero fragments. Very small files are stored
  by Windows in the directory and the number of clusters will be zero.
}
function AnalyzeFile(
    FileName : String;
    var Fragments : DWORD;
    var RealClusters : DWORD;
    var Lcn : ULONG64;
    bCompressedClusters : Boolean = true // true to count compressed clusters
    ) : Boolean;
var
  FileHandle : THandle;
  InBuffer : STARTING_VCN_INPUT_BUFFER;
  PreviousVcn : ULONG64;
  PreviousLcn : ULONG64;
  Res : Integer;
  d, w : DWORD;
  ExtentData : PRETRIEVAL_POINTERS_BUFFER;
  bytes : array of Byte;
begin
 Result := false;
 Fragments := 0; RealClusters := 0; Lcn := 0;

 // Open the item as a file or as a directory. If the item could
 // not be opened then exit.
 FileHandle := CreateFile(PWChar(FileName), FILE_READ_ATTRIBUTES,
       FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
       nil, OPEN_EXISTING,
       FILE_FLAG_NO_BUFFERING or FILE_FLAG_SEQUENTIAL_SCAN, 0);
 if FileHandle = INVALID_HANDLE_VALUE then exit;

 // Get the clustermap of this file. The loop will repeat if there
 //   are more datablocks than fit in the Data.
 InBuffer.StartingVcn.QuadPart := 0;
 PreviousLcn := High(ULONG64);
 PreviousVcn := 0;
 repeat
  SetLength(bytes, 1024 * 1024); // Minimum size 32 bytes.

  Res := Integer(DeviceIoControl(FileHandle, FSCTL_GET_RETRIEVAL_POINTERS,
    @InBuffer, sizeof(STARTING_VCN_INPUT_BUFFER), @Bytes[0],
    Length(Bytes), w, nil));
  if Res = 0 then begin
   Res := GetLastError();
   if Res <> ERROR_MORE_DATA then break
  end;
  ExtentData := @bytes[0];

  // Count the number of fragments, and calculate the total number of
  // clusters used by this file.
  if PreviousVcn = 0 then
   PreviousVcn := ExtentData.StartingVcn;
  for d := 0 to ExtentData.ExtentCount - 1 do begin
   FlipHighLow(ExtentData.Extents[d].NextVcn);
   FlipHighLow(ExtentData.Extents[d].Lcn);

   if PreviousLcn = High(ULONG64) then
    Lcn := UInt64(ExtentData.Extents[d].Lcn);
   if ExtentData.Extents[d].Lcn.QuadPart <> -1 then begin
    if UInt64(ExtentData.Extents[d].Lcn) <> PreviousLcn then
     Fragments := Fragments + 1;
    PreviousLcn := ExtentData.Extents[d].Lcn.QuadPart +
          DWORD(UInt64(ExtentData.Extents[d].NextVcn) - PreviousVcn);
    RealClusters := RealClusters +
          DWORD(UInt64(ExtentData.Extents[d].NextVcn) - PreviousVcn);
   end;
   if (ExtentData.Extents[d].Lcn.QuadPart <> -1) or bCompressedClusters then
    if ExtentData.Extents[d].NextVcn.QuadPart <> -1 then
     // -1 == ERROR_MORE_DATA & factual end of file with LowPart == Sector#
     if ExtentData.Extents[d].NextVcn.HighPart < 0 then
      PreviousVcn := ExtentData.Extents[d].NextVcn.LowPart
     else
      PreviousVcn := ExtentData.Extents[d].NextVcn.QuadPart
  end;
  // Next datablock.
  if Res = ERROR_MORE_DATA then
    InBuffer.StartingVcn.QuadPart := ExtentData.Extents[d - 1]
      .NextVcn.QuadPart
 until Res <> ERROR_MORE_DATA;

 CloseHandle(FileHandle); SetLength(bytes, 0); Result := true
end;

{ IsNotSystemLcn
  Checks the LCN is not inside closed areas of NTFS volume.
}
function IsNotSystemLcn(Lcn : LARGE_INTEGER) : Boolean;
begin
 Assert(    (0 < Excludes[0].End_)
        and (0 < Excludes[1].End_)
        and (0 < Excludes[2].End_), 'The volume data is not initialized');
 Result :=  ((Lcn.QuadPart < Excludes[0].Start_) or (Excludes[0].End_ <= Lcn.QuadPart))
        and ((Lcn.QuadPart < Excludes[1].Start_) or (Excludes[1].End_ <= Lcn.QuadPart))
        and ((Lcn.QuadPart < Excludes[2].Start_) or (Excludes[2].End_ <= Lcn.QuadPart))
end;

{ MinLcnForFile
  Gets minimum LCN outside of system regions for given size of fragment.
}
function MinLcnForFile(Siz: Cardinal) : UINT64;
var
 i: Integer;
begin
 Result := 0;
 for i := 0 to 2 do
  if Result + Siz <= Excludes[ExsOrder[i]].Start_ then exit else
   Result := Excludes[ExsOrder[i]].End_ + 1
end;

{ FindFreeBlock
  Look for a block of empty clusters on disk. If the MinimumLcn is
  not zero then the block must be at or above it, if the MinimumSize is
  not zero then the block must have at least that many contiguous
  free clusters. Return true if succes, false if no block was found or
  an error occurred.
}
function FindFreeBlock(
    VolumeHandle : THandle;
    MinimumLcn : ULONG64;            // Cluster must be at or above this LCN.
    MinimumSize : DWORD;             // Cluster must be at least this big.
    var BeginLcn : ULONG64;          // Res, LCN of begin of cluster.
    var EndLcn : ULONG64) : Boolean; // Res, LCN of end of cluster.
label
 success, memfree;
var
  InBuffer : ^STARTING_LCN_INPUT_BUFFER;
  Data : ^VOLUME_BITMAP_BUFFER;
  Lcn : ULONG64;
  ClusterStart : ULONG64;
  Index : Integer;
  IndexMax : DWORD;
  Mask : BYTE;
  InUse : Integer;
  PrevInUse : Integer;
  Res : Integer;
begin
 // Main loop to walk through the entire clustermap.
 Result := false;
 Lcn := MinimumLcn; ClusterStart := 0; PrevInUse := 1;
 GetMem(InBuffer, szSTARTING_LCN_INPUT_BUFFER);
 GetMem(Data, szVOLUME_BITMAP_BUFFER);
 repeat
  // Sanity check.
  if (0 < MaxLcn) and (MaxLcn <= Lcn) then goto memfree;

  // Fetch a block of cluster data.
  InBuffer^.StartingLcn.QuadPart := Lcn;
  Res := Integer(DeviceIoControl(VolumeHandle, FSCTL_GET_VOLUME_BITMAP,
    InBuffer, szSTARTING_LCN_INPUT_BUFFER, Data, szVOLUME_BITMAP_BUFFER,
    IndexMax, nil));
  if Res = 0 then begin
   Res := GetLastError();
   if Res <> ERROR_MORE_DATA then goto memfree
  end;

// Analyze the clusterdata. We resume where the previous block left
// off. If a cluster is found that matches the criteria then return
// it's LCN (Logical Cluster Number).
  Lcn := UInt64(Data^.StartingLcn); Index := 0; Mask := 1;
  if UInt64(Data^.BitmapSize) div 8 < IndexMax then
   IndexMax := UInt64(Data^.BitmapSize) div 8;
  while Index < IndexMax do begin
   InUse := (Data^.Buffer[Index] and Mask);
   if(((Excludes[0].Start_ <= Lcn) and (Lcn < Excludes[0].End_))
   or ((Excludes[1].Start_ <= Lcn) and (Lcn < Excludes[1].End_))
   or ((Excludes[2].Start_ <= Lcn) and (Lcn < Excludes[2].End_)))then
    InUse := 1;
   if (PrevInUse = 0) and (InUse <> 0) then begin
    if (MinimumLcn <= ClusterStart) and (MinimumSize <= Lcn - ClusterStart) then
     goto success;
   end;
   if (PrevInUse <> 0) and (InUse = 0) then ClusterStart := Lcn;
   PrevInUse := InUse;
   if Mask = 128 then begin
    Mask := 1;
    Index := Index + 1
   end else
    Mask := Mask shl 1;
   Lcn := Lcn + 1
  end;
 until(Res <> ERROR_MORE_DATA)
 and  (Lcn < UInt64(Data^.StartingLcn) + UInt64(Data^.BitmapSize));

 if PrevInUse = 0 then begin
  if (MinimumLcn <= ClusterStart) and (MinimumSize <= Lcn - ClusterStart) then
  begin
success:
   BeginLcn := ClusterStart;
   EndLcn := Lcn;
   Result := true
  end
 end;
memfree:
 FreeMem(InBuffer, szSTARTING_LCN_INPUT_BUFFER);
 FreeMem(Data, szVOLUME_BITMAP_BUFFER)
end;

{ MaxFreeBlockOfFragment
  Look for a block of empty clusters on disk inside fragment.
  Returns 1st & last LCNs of free block with maximum size.
  Must be called with 1st & last LCNs of fragment in BeginLcn & EndLcn.
}
function MaxFreeBlockOfFragment(
    VolumeHandle : THandle;
    var BeginLcn : ULONG64;
    var EndLcn : ULONG64) : Cardinal;
label
 memfree;
var
  InBuffer : ^STARTING_LCN_INPUT_BUFFER;
  Data : ^VOLUME_BITMAP_BUFFER;
  Lcn, ClusterStart, LcnB, LcnE : ULONG64;
  Index : Integer;
  IndexMax : DWORD;
  Mask : BYTE;
  InUse : Integer;
  PrevInUse : Integer;
  Res : Integer;

  function CheckSetRange(var Size: Cardinal; BLcn, ELcn: ULONG64): Boolean;
  begin
   Result := false;
   if (BLcn + Size <= ELcn) and (EndLcn + 1 <> BeginLcn + Size) then begin
    if (BLcn <= BeginLcn) and (EndLcn <= ELcn)
    or (BeginLcn <= BLcn) and (BLcn < EndLcn)
    or (BeginLcn < ELcn) and (ELcn <= EndLcn) then
    begin
     if BLcn    < BeginLcn then LcnB :=    BeginLcn else LcnB := BLcn;
     if EndLcn + 1 <  ELcn then LcnE  := 1 + EndLcn else LcnE := ELcn;
     Size := LcnE - LcnB
    end;
    Result := EndLcn + 1 = BeginLcn + Size
   end
  end;
begin
 Result := 0; LcnB := BeginLcn; LcnE := BeginLcn;
 Lcn := BeginLcn; ClusterStart := 0; PrevInUse := 1;
 GetMem(InBuffer, szSTARTING_LCN_INPUT_BUFFER);
 GetMem(Data, szVOLUME_BITMAP_BUFFER);

 // Main loop to walk through the entire clustermap.
 repeat
  // Sanity check.
  if (0 < MaxLcn) and (MaxLcn <= Lcn) then goto memfree;

  // Fetch a block of cluster data.
  InBuffer^.StartingLcn.QuadPart := Lcn;
  Res := Integer(DeviceIoControl(VolumeHandle, FSCTL_GET_VOLUME_BITMAP,
    InBuffer, szSTARTING_LCN_INPUT_BUFFER, Data, szVOLUME_BITMAP_BUFFER,
    IndexMax, nil));
  if Res = 0 then begin
   Res := GetLastError();
   if Res <> ERROR_MORE_DATA then goto memfree
  end;

// Analyze the clusterdata. We resume where the previous block left
// off. If a cluster is found that matches the criteria then return
// it's LCN (Logical Cluster Number).
  Lcn := UInt64(Data^.StartingLcn); Index := 0; Mask := 1;
  if UInt64(Data^.BitmapSize) div 8 < IndexMax then
   IndexMax := UInt64(Data^.BitmapSize) div 8;
  while (Index < IndexMax) and (Lcn < EndLcn + 2) do begin
   InUse := (Data^.Buffer[Index] and Mask);
   if(((Excludes[0].Start_ <= Lcn) and (Lcn < Excludes[0].End_))
   or ((Excludes[1].Start_ <= Lcn) and (Lcn < Excludes[1].End_))
   or ((Excludes[2].Start_ <= Lcn) and (Lcn < Excludes[2].End_)))then
    InUse := 1;
   if (PrevInUse = 0) and (InUse <> 0) then
    if CheckSetRange(Result, ClusterStart, Lcn) then break;

   if (PrevInUse <> 0) and (InUse = 0) then ClusterStart := Lcn;
   PrevInUse := InUse;
   if Mask = 128 then begin
    Mask := 1;
    Index := Index + 1
   end else
    Mask := Mask shl 1;
   if Lcn <= EndLcn then Lcn := Lcn + 1 else break
  end;
 until   (EndLcn < Lcn)
      or (Res <> ERROR_MORE_DATA)
      and(Lcn < UInt64(Data^.StartingLcn) + UInt64(Data^.BitmapSize));
 if PrevInUse = 0 then
  CheckSetRange(Result, ClusterStart, Lcn);
 BeginLcn := LcnB; if BeginLcn < LcnE then EndLcn := LcnE - 1;
memfree:
 FreeMem(InBuffer, szSTARTING_LCN_INPUT_BUFFER);
 FreeMem(Data, szVOLUME_BITMAP_BUFFER)
end;

{ GetNextFreeBlock
  Look for a block of empty clusters on disk.
  Returns 1st & last LCNs of the 1st free block after previous block.
  [in] 1st & last LCNs of fragment in BeginLcn & EndLcn or zeroes to initialize.
}
function GetNextFreeBlock(VolumeHandle: THandle;
                          var BeginLcn: ULONG64;
                            var EndLcn: ULONG64;
                              StartLcn: ULONG64 = HighUInt64): Cardinal;
label
 memfree;
var
  InBuffer : ^STARTING_LCN_INPUT_BUFFER;
  Data : ^VOLUME_BITMAP_BUFFER;
  Lcn, ClusterStart : ULONG64;
  Index : Integer;
  IndexMax : DWORD;
  Mask : BYTE;
  InUse : Integer;
  PrevInUse : Integer;
  Res : Integer;
begin
 Result := 0;
 if StartLcn = HighUInt64 then Lcn := EndLcn + 1 else Lcn := StartLcn;
 ClusterStart := EndLcn + 1; PrevInUse := 1;
 GetMem(InBuffer, szSTARTING_LCN_INPUT_BUFFER);
 GetMem(Data, szVOLUME_BITMAP_BUFFER);

 // Main loop to walk through the entire clustermap.
 repeat
  // Sanity check.
  if (0 < MaxLcn) and (MaxLcn <= Lcn) then goto memfree;

  // Fetch a block of cluster data.
  InBuffer^.StartingLcn.QuadPart := Lcn;
  Res := Integer(DeviceIoControl(VolumeHandle, FSCTL_GET_VOLUME_BITMAP,
    InBuffer, szSTARTING_LCN_INPUT_BUFFER, Data, szVOLUME_BITMAP_BUFFER,
    IndexMax, nil));
  if Res = 0 then begin
   Res := GetLastError();
   if Res <> ERROR_MORE_DATA then goto memfree
  end;

// Analyze the clusterdata. We resume where the previous block left
// off. If a cluster is found that matches the criteria then return
// it's LCN (Logical Cluster Number).
  Lcn := UInt64(Data^.StartingLcn);
  Index := 0; Mask := 1;
  if UInt64(Data^.BitmapSize) div 8 < IndexMax then
   IndexMax := UInt64(Data^.BitmapSize) div 8;
  while Index < IndexMax do begin
   InUse := (Data^.Buffer[Index] and Mask);
   if(((Excludes[0].Start_ <= Lcn) and (Lcn < Excludes[0].End_))
   or ((Excludes[1].Start_ <= Lcn) and (Lcn < Excludes[1].End_))
   or ((Excludes[2].Start_ <= Lcn) and (Lcn < Excludes[2].End_)))then
    InUse := 1;

   if (PrevInUse = 0) and (InUse <> 0) then
    if EndLcn < ClusterStart then break;

   if (PrevInUse <> 0) and (InUse = 0) then ClusterStart := Lcn;
   PrevInUse := InUse;
   if Mask = 128 then begin
    Mask := 1;
    Index := Index + 1
   end else
    Mask := Mask shl 1;
   Lcn := Lcn + 1
  end;
 until   (EndLcn < ClusterStart)
      or (Res <> ERROR_MORE_DATA)
      and(Lcn < UInt64(Data^.StartingLcn) + UInt64(Data^.BitmapSize));
 if EndLcn < ClusterStart then begin
  BeginLcn := ClusterStart;
  if (PrevInUse = 0) and (InUse = 0) then EndLcn := MaxLcn - 1 else
   EndLcn := Lcn - 1;
  Result := EndLcn - BeginLcn + 1
 end;
memfree:
 FreeMem(InBuffer, szSTARTING_LCN_INPUT_BUFFER);
 FreeMem(Data, szVOLUME_BITMAP_BUFFER)
end;

{ GetFreeBlocks
  Look for a blocks of empty clusters on disk.
  Returns all found clusters extents as array and the size of all found blocks.
  [in] 1st & last LCNs of fragment in BeginLcn & EndLcn or default for all disk.
  [in] optional parameter StartLcn to use different start Lcn for search.
}
function GetFreeBlocks(VolumeHandle: THandle;
                        var Extents: TExtents;
                           BeginLcn: ULONG64 = 0;
                             EndLcn: ULONG64 = HighUInt64;
                           StartLcn: ULONG64 = HighUInt64): Cardinal;
label
 memfree;
var
  InBuffer : ^STARTING_LCN_INPUT_BUFFER;
  Data : ^VOLUME_BITMAP_BUFFER;
  Lcn, ClusterStart : ULONG64;
  Index : Integer;
  IndexMax : DWORD;
  Mask : BYTE;
  InUse : Integer;
  PrevInUse : Integer;
  Res : Integer;

  procedure AddExtent;
  var
   i: Integer;
  begin
   if (BeginLcn <= ClusterStart) and (Lcn <= EndLcn) then begin
    i := Length(Extents); SetLength(Extents, i + 1);
    Extents[i].Lcn.QuadPart := ClusterStart;
    Extents[i].Vcs := Lcn - ClusterStart;
    Result := Result + Extents[i].Vcs
   end
  end;
begin
 Result := 0; SetLength(Extents, 0); if EndLcn < BeginLcn then exit;
 if StartLcn = HighUInt64 then Lcn := BeginLcn else Lcn := StartLcn;
 if EndLcn <> HighUInt64 then Inc(EndLcn);
 ClusterStart := BeginLcn; PrevInUse := 1;
 GetMem(InBuffer, szSTARTING_LCN_INPUT_BUFFER);
 GetMem(Data, szVOLUME_BITMAP_BUFFER);

 // Main loop to walk through the entire clustermap.
 repeat
  // Sanity check.
  if (0 < MaxLcn) and (MaxLcn <= Lcn) then goto memfree;

  // Fetch a block of cluster data.
  InBuffer^.StartingLcn.QuadPart := Lcn;
  Res := Integer(DeviceIoControl(VolumeHandle, FSCTL_GET_VOLUME_BITMAP,
    InBuffer, szSTARTING_LCN_INPUT_BUFFER, Data, szVOLUME_BITMAP_BUFFER,
    IndexMax, nil));
  if Res = 0 then begin
   Res := GetLastError();
   if Res <> ERROR_MORE_DATA then goto memfree
  end;

// Analyze the clusterdata. We resume where the previous block left
// off. If a cluster is found that matches the criteria then return
// it's LCN (Logical Cluster Number).
  Lcn := UInt64(Data^.StartingLcn);
  Index := 0; Mask := 1;
  if UInt64(Data^.BitmapSize) div 8 < IndexMax then
   IndexMax := UInt64(Data^.BitmapSize) div 8;
  while Index < IndexMax do begin
   InUse := (Data^.Buffer[Index] and Mask);
   if(((Excludes[0].Start_ <= Lcn) and (Lcn < Excludes[0].End_))
   or ((Excludes[1].Start_ <= Lcn) and (Lcn < Excludes[1].End_))
   or ((Excludes[2].Start_ <= Lcn) and (Lcn < Excludes[2].End_)))then
    InUse := 1;

   if (PrevInUse = 0) and (InUse <> 0) then AddExtent;

   if (PrevInUse <> 0) and (InUse = 0) then ClusterStart := Lcn;
   PrevInUse := InUse;
   if Mask = 128 then begin
    Mask := 1;
    Index := Index + 1
   end else
    Mask := Mask shl 1;
   if Lcn < EndLcn then Lcn := Lcn + 1 else break
  end;
 until   (EndLcn <= Lcn)
      or (Res <> ERROR_MORE_DATA)
      and(Lcn < UInt64(Data^.StartingLcn) + UInt64(Data^.BitmapSize));

 if PrevInUse = 0 then AddExtent; // <==> Lcn = EndLcn
memfree:
 FreeMem(InBuffer, szSTARTING_LCN_INPUT_BUFFER);
 FreeMem(Data, szVOLUME_BITMAP_BUFFER)
end;

function IsMoved(
    FileName : String;
    var Frg : DWORD;
    var ExpS : DWORD;
    var ExpL : ULONG64;
    bCmprClus : Boolean = true;
    Wait : Cardinal = 12) : Boolean;
var
 FctS: DWORD; FctL: ULONG64; Step: Cardinal;
begin
 Result := AnalyzeFile(FileName, Frg, FctS, FctL, bCmprClus)
           and (ExpL = FctL) and (ExpS shr 6 <= FctS) and (FctS <= ExpS shl 6);
 if Result then begin
  Step := 0;
  repeat
   Sleep(3); Inc(Step, 3);
   Result := AnalyzeFile(FileName, Frg, FctS, FctL, bCmprClus)
             and (ExpL = FctL) and (ExpS shr 6 <= FctS) and (FctS <= ExpS shl 6)
  until not Result or (Wait <= Step) or (ExpS = FctS);
 end;
 ExpL := FctL; ExpS := FctS
end;

{ MoveToLcn
  Move a file to a new location on disk. The new location must be a
  contiguous block of free clusters. Moving the file will automatically
  defragment it.
  Return codes:
  0  Success.
  1  Cannot move file because new location is inside the file.
  2  Cannot move file because it has no clusters.
  3  The file could not be opened, permission denied.
  4  Error in Windows defragmenter API.
  5  Windows has fragmented the file.
  6  If bWait reports damaged file error if file has unrealistic map after move.
}
function MoveToLcn(VolumeHandle : THandle;
    NewLcn : UInt64;
    FileName : String;
    var RealClusters : DWORD; // If error calculates number of uncompressed clusters
    var Lcn : UInt64;
    bWait : Boolean = false;
    Strategy : Integer = 0
    ) : Integer;
type
 MOVE_FILE_DATA = record
  FileHandle : HWND;
//  Reserved : ULONG; // x64 target gives sys\err #87
  StartingVcn : LARGE_INTEGER;
  StartingLcn : LARGE_INTEGER;
  ClusterCount : ULONG; // NumVcns
//  Reserved1 : ULONG; // x64 target gives sys\err #87
 end;
var
  MoveParams : MOVE_FILE_DATA;
  ExtentData : PRETRIEVAL_POINTERS_BUFFER;
  bytes : array of Byte;
  PreviousVcn : ULONG64;
  Res : Integer;
  ErrorCode, Fragments, w, d : DWORD;
begin
 // If the new location is inside the file then we cannot move it.
 Result := 1;
 if (Lcn <= NewLcn) and (NewLcn < Lcn + RealClusters) then exit;
 Lcn := 0;
 // If the file has no size then we cannot move it.
 Result := 2;
 if RealClusters = 0 then exit;

 // Open the item as a file or as a directory. If the item could
 // not be opened then exit.
 MoveParams.FileHandle := CreateFile(PWChar(FileName),
  FILE_READ_ATTRIBUTES, FILE_SHARE_READ or FILE_SHARE_WRITE or
  FILE_SHARE_DELETE, nil, OPEN_EXISTING, FILE_FLAG_NO_BUFFERING, 0);

 if MoveParams.FileHandle = INVALID_HANDLE_VALUE then begin
  Result := 3;
  exit
 end;

 // Walk through the clustermap of the item and move all segments to the
 // new location. If the clusters at the old location are contiguous then
 // move as a single block.
 PreviousVcn := 0;
 MoveParams.StartingLcn.QuadPart := NewLcn;
 MoveParams.StartingVcn.QuadPart := 0;
 MoveParams.ClusterCount := 0;
 ErrorCode := ERROR_MORE_DATA;
 while ErrorCode = ERROR_MORE_DATA do begin
  ErrorCode := NO_ERROR;
  SetLength(bytes, 1024 * 1024);

  Res := Integer(DeviceIoControl(MoveParams.FileHandle,
    FSCTL_GET_RETRIEVAL_POINTERS, @PreviousVcn,
    sizeof(STARTING_VCN_INPUT_BUFFER), @Bytes[0], Length(Bytes), w, 0));
  if Res = 0 then begin
   ErrorCode := GetLastError();
   if ErrorCode <> ERROR_MORE_DATA then break
  end;
  ExtentData := @bytes[0];

  for d := 0 to ExtentData.ExtentCount - 1 do
  begin
   FlipHighLow(ExtentData.Extents[d].NextVcn);
   FlipHighLow(ExtentData.Extents[d].Lcn);

   if UInt64(ExtentData.Extents[d].Lcn.QuadPart) <> High(ULONG64) then begin
    if Strategy = 0 then begin
     MoveParams.ClusterCount := MoveParams.ClusterCount +
              DWORD(ExtentData.Extents[d].NextVcn.QuadPart - PreviousVcn);
    end else begin
     MoveParams.StartingVcn.QuadPart := PreviousVcn;
     MoveParams.ClusterCount :=
              DWORD(ExtentData.Extents[d].NextVcn.QuadPart - PreviousVcn);
     Res := Integer(DeviceIoControl(VolumeHandle, FSCTL_MOVE_FILE,
       @MoveParams, SizeOf(MoveParams), nil, 0, w, nil));
     if Res = 0 then begin
       ErrorCode := GetLastError();
       break
     end;
     MoveParams.StartingLcn.QuadPart := MoveParams.StartingLcn.QuadPart +
       MoveParams.ClusterCount;
    end;
    if UInt64(ExtentData.Extents[d].NextVcn) <> High(ULONG64) then
     if ExtentData.Extents[d].NextVcn.HighPart < 0 then // -1 == ERROR_MORE_DATA
      PreviousVcn := ExtentData.Extents[d].NextVcn.LowPart
     else
      PreviousVcn := ExtentData.Extents[d].NextVcn.QuadPart
   end
  end
 end;
 if(Strategy = 0)and(ErrorCode = NO_ERROR)and(MoveParams.ClusterCount > 0)then
 begin
  Res := Integer(DeviceIoControl(VolumeHandle, FSCTL_MOVE_FILE, @MoveParams,
     SizeOf(MoveParams), nil, 0, w, nil));
  if Res = 0 then ErrorCode := GetLastError()
 end;

 FlushFileBuffers(MoveParams.FileHandle);    // Is this useful? Can't hurt.
 CloseHandle(MoveParams.FileHandle);
 SetLength(bytes, 0);

 // If FSCTL_GET_RETRIEVAL_POINTERS or FSCTL_MOVE_FILE reported an error
 // then show the error message and return 4. These errors are very rare
 // and only happen in very special circumstances, such as the volume
 // being dismounted or something like that.
 Lcn := NewLcn;
 if (ErrorCode <> ERROR_MORE_DATA) and (ErrorCode <> NO_ERROR) then begin
  Result := 4;
  if not bWait then
   AnalyzeFile(FileName, Fragments, RealClusters, Lcn, false)
  else
   if not IsMoved(FileName, Fragments, RealClusters, Lcn, false) and (Lcn = NewLcn) then
    Result := 6;
  OutputSysError(false, 'MoveToLcn|FSCTL_MOVE_FILE', ErrorCode);
  exit
 end else
  if not bWait then
   AnalyzeFile(FileName, Fragments, RealClusters, Lcn)
  else
   if not IsMoved(FileName, Fragments, RealClusters, Lcn) and (Lcn = NewLcn) then
    Result := 6;

 // If Windows has fragmented the file by moving it then return 5.
 if Result = 2 then if 1 < Fragments then Result := 5 else Result := 0
end;

end.
