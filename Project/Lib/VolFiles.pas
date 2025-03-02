{ VolFiles
  The unit of Cloning Optimizer (Cloper) tool.

  Encapsulates data acquisition of volume with NTFS file system using plain
  items scanning & queries of active USN journal.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit VolFiles;

interface
uses
 Types;

function GetFilesByUsnQuery(nsrc, ntgt: Integer;
      var OldFiles, DelFiles, NewFiles: TStringDynArray;
                          var bDoNTFSC: TBooleanDynArray;
                          var NewFlsSz: TCardinalDynArray;
                           ClusterSize: Integer): Boolean;

function GetFilesByScanning(nsrc, ntgt: Integer;
      var OldFiles, DelFiles, NewFiles: TStringDynArray;
                          var bDoNTFSC: TBooleanDynArray;
                          var NewFlsSz: TCardinalDynArray;
                           ClusterSize: Integer): Boolean;

function IsNtfsFileSytem(vn: Integer): Boolean;
function CheckUsnActive(vn : Integer) : Boolean;
function CountUsnRecords(vn : Integer) : NativeUInt;
function CountFilesByScanning(vn: Integer): NativeUInt;
procedure DefineSourceTargetByFileCount(var SrcDc, TgtDc, SrcVo, TgtVo: Integer);

type
 TCheckFileName = function(FileName: String; var CurSkpCnt: Integer;
   NumCall: Byte = 1): Boolean;
var
 CheckNewFile : TCheckFileName;

implementation
uses
 Windows, SysUtils, MftQueryUsn, cloper_typs, cloper_base, VolTools, StrTools;

type
 TUSNRecord_ = record
  FileReferenceNumber, ParentFileReferenceNumber: UInt64;
  FileAttributes: Cardinal;
  Name, AName : String;
  Files : TIntegerDynArray;
 end;
 TAUSNRecord = array of TUSNRecord_;

// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Call back functions can't be nested on x64 (24.0.22858.6822).           // ++
function CbfUSNRecord(AUSN: PUSNRecord; var AUSNR: TAUSNRecord): Boolean;  // ++
var                                                                        // ++
 i : Integer; USNR : TUSNRecord;                                           // ++
begin                                                                      // ++
 USNR := USNRecFromPointer(AUSN);                                          // ++
 i := Length(AUSNR);                                                       // ++
 SetLength(AUSNR, i + 1);                                                  // ++
 AUSNR[i].FileReferenceNumber := USNR.FileReferenceNumber;                 // ++
 AUSNR[i].ParentFileReferenceNumber := USNR.ParentFileReferenceNumber;     // ++
 AUSNR[i].FileAttributes := USNR.FileAttributes;                           // ++
 AUSNR[i].Name := GetNameFromRec(AUSN); // USNR.FileName.Ptr;              // ++
 Result := true                                                            // ++
end;                                                                       // ++
                                                                           // ++
procedure GetUsnPer(i, j : Integer; var arr : Pointer);                    // ++
var AUSNR : TAUSNRecord; USNR : TUSNRecord_; begin                         // ++
 AUSNR := TAUSNRecord(arr^);                                               // ++
 USNR := AUSNR[I]; AUSNR[I] := AUSNR[J]; AUSNR[J] := USNR                  // ++
end;                                                                       // ++
                                                                           // ++
function GetUsnCmp(I, J : Integer; arr : Pointer) : Boolean;               // ++
var AUSNR : TAUSNRecord; begin                                             // ++
 AUSNR := TAUSNRecord(arr^);                                               // ++
 Result := AUSNR[I].FileReferenceNumber < AUSNR[J].FileReferenceNumber     // ++
end;                                                                       // ++
                                                                           // ++
type                                                                       // ++
 TArrToSort = record                                                       // ++
   Names, Sizes : Pointer;                                                 // ++
 end;                                                                      // ++
TUInt64DynArray = array of UInt64;                                         // ++
                                                                           // ++
procedure GetFilesPer(i, j : Integer; var arr : Pointer);                  // ++
var                                                                        // ++
 NewFlsSz : TCardinalDynArray; NewFlsNms : TStringDynArray;               // ++
 Name : String; Size : UInt64;                                             // ++
begin                                                                      // ++
 NewFlsNms := TStringDynArray(TArrToSort(arr^).Names^);                    // ++
 NewFlsSz := TCardinalDynArray(TArrToSort(arr^).Sizes^);                  // ++
 Name := NewFlsNms[I]; NewFlsNms[I] := NewFlsNms[J]; NewFlsNms[J] := Name; // ++
 Size := NewFlsSz[I]; NewFlsSz[I] := NewFlsSz[J]; NewFlsSz[J] := Size; // ++
end;                                                                       // ++
                                                                           // ++
function GetFilesCmp(I, J : Integer; arr : Pointer) : Boolean;             // ++
var                                                                        // ++
 NewFlsSz : TCardinalDynArray;                                            // ++
begin                                                                      // ++
 NewFlsSz := TCardinalDynArray(TArrToSort(arr^).Sizes^);                  // ++
 Result := NewFlsSz[I] < NewFlsSz[J]                                     // ++
end;                                                                       // ++
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function CheckNewFiles(var NewFiles: TStringDynArray;
                       var bDoNTFSC: TBooleanDynArray;
                      var NewFlsSz: TCardinalDynArray;
                        ClusterSize: Integer): Boolean;
var
 ATS: TArrToSort; i, j, k: Integer;
begin
 Result := Length(NewFiles) = 0;
 Assert(@CheckNewFile <> nil,
   'The delegate function to check names of new files isn''t assigned');
 if Length(NewFlsSz) = 0 then exit else Result := true;

 ATS.Names := @NewFiles; ATS.Sizes := @NewFlsSz;
 QuickSort(@ATS, 0, Length(NewFiles) - 1, @GetFilesCmp, @GetFilesPer);

 // Obtaining final array of files for their optimization:
 // Ignore small files in $MFT (<= 1024) and skip compression of small files with
 // size less than the size of cluster. Checks filters to skip compression.
 i := 0;
 CheckNewFile('', i, 0);
 SetLength(bDoNTFSC, Length(NewFiles));
 j := 0;
 for k := 0 to Length(NewFiles) - 1 do
  if NewFlsSz[k] <= 1024 then
   Inc(j)
  else begin
   NewFiles[k - j] := NewFiles[k]; NewFlsSz[k - j] := NewFlsSz[k];
   if NewFlsSz[k] <= ClusterSize then
     bDoNTFSC[k - j] := false
   else
    bDoNTFSC[k - j] := CheckNewFile(NewFiles[k], i)
  end;
 j := Length(NewFiles) - j;
 SetLength(bDoNTFSC, j); SetLength(NewFiles, j); SetLength(NewFlsSz, j);
 CheckNewFile('', i, 2)
end;

function GetFilesByUsnQuery(nsrc, ntgt: Integer;
      var OldFiles, DelFiles, NewFiles: TStringDynArray;
                          var bDoNTFSC: TBooleanDynArray;
                         var NewFlsSz: TCardinalDynArray;
                           ClusterSize: Integer): Boolean;

 function GetUsn(N : Integer; var AUSNR : TAUSNRecord) : Boolean;
 var
  h : THandle;
  AMFTEnumBuff: Pointer;
 begin
  SetLength(AUSNR, 0);
  h := GetRootHandle('globalroot\Device\HarddiskVolume' + IntToStr(Round(N)),
    false, true);
  try
   AMFTEnumBuff := AllocMFTEnumBuffer(H);
   EnumMFTEntries(h, AMFTEnumBuff, @CbfUSNRecord, @AUSNR);
   CloseHandle(H);
   QuickSort(@AUSNR, 0, Length(AUSNR) - 1, @GetUsnCmp, @GetUsnPer);
   Result := false
  except
   Result := true
  end;
 end;
 function BS(AUSNR: TAUSNRecord; FRN : UInt64; var ib: Integer;
     ExpAtr : Cardinal; res : Boolean = true): Boolean;
 var
  i, j, k, m : Integer;
 begin
  m := Length(AUSNR); i := (m + 1 - ib) div 2;
  j := ib; ib := ib + i; Result := not res; k := 0;
  while i > 0 do begin
   if FRN < AUSNR[ib].FileReferenceNumber then begin
    ib := ib - i; if k > 0 then i := i div 2; k := -1
   end else
    if AUSNR[ib].FileReferenceNumber < FRN then begin
     ib := ib + i; if k < 0 then i := i div 2; k := 1
    end else begin
     if 0 < AUSNR[ib].FileAttributes and ExpAtr then
      Result := res
     else
      Result := not res;
     exit
    end;
   if ib < j then begin
    ib := j; k := - k; i := i div 2
   end else if m <= ib then begin
    ib := m - 1; k := - k; i := i div 2
   end;
  end;
  if FRN <> AUSNR[ib].FileReferenceNumber then begin
   if (j < ib) and (FRN = AUSNR[ib - 1].FileReferenceNumber) then Dec(ib);
   if (ib < m - 1) and (FRN = AUSNR[ib + 1].FileReferenceNumber) then Inc(ib);
  end;
  if FRN = AUSNR[ib].FileReferenceNumber then
   if 0 < AUSNR[ib].FileAttributes and ExpAtr then
    Result := res
   else
    Result := not res
  else
   Result := not res
 end;

 function CGPWF(VS: TVolSpecs; AUSNR: TAUSNRecord; Idx: Integer;
        Name: String; var Path: String; ResVal: Boolean = false): Boolean;
 var
  i, j, k, m : Integer;
  FFData : WIN32_FIND_DATA;
  Ids : TCardinalDynArray;
  h : THandle;
 begin
  i := 0; k := -1; j := Idx; Result := not ResVal; Path := '';
  while BS(AUSNR, AUSNR[j].ParentFileReferenceNumber, i,
    FILE_ATTRIBUTE_DIRECTORY) do
  begin
   if j = Idx then begin
    for m := 0 to Length(AUSNR[j].Files) - 1 do
     if AUSNR[AUSNR[j].Files[m]].Name = Name then begin
      Result := ResVal; break
     end;
    if Result = ResVal then exit;
    k := Length(AUSNR[i].Files); SetLength(AUSNR[i].Files, k + 1);
    AUSNR[i].Files[k] := Idx;
   end;
   j := i; i := Length(Ids); SetLength(Ids, i + 1); Ids[i] := j; i := 0;
  end;
  for i := Length(Ids) - 1 downto 0 do begin
   if AUSNR[Ids[i]].AName <> '' then
    Path := Path + AUSNR[Ids[i]].AName + '\'
   else begin
    h := FindFirstFile(PWChar(VS.Root + Path + AUSNR[Ids[i]].Name), FFData);
    if h = INVALID_HANDLE_VALUE then
     AUSNR[Ids[i]].AName := UpperCase(AUSNR[Ids[i]].Name)
    else begin
     Windows.FindClose(h);
     AUSNR[Ids[i]].AName := FFData.cAlternateFileName;
     if AUSNR[Ids[i]].AName = '' then
      AUSNR[Ids[i]].AName := UpperCase(AUSNR[Ids[i]].Name)
    end;
    Path := Path + AUSNR[Ids[i]].AName + '\'
   end
  end
 end;
 function CFP_FVP(SAUSNR, TAUSNR: TAUSNRecord; IxS, IxT: Integer): Boolean;
 var
  PhS, PhT: String;
 begin
  CGPWF(SrcVolSpec, SAUSNR, IxS, SAUSNR[IxS].Name, PhS);
  CGPWF(TgtVolSpec, TAUSNR, IxT, TAUSNR[IxT].Name, PhT); Result := PhS = PhT
 end;
const
 SkipAttributes = FILE_ATTRIBUTE_DIRECTORY or FILE_ATTRIBUTE_SPARSE_FILE or
                  FILE_ATTRIBUTE_REPARSE_POINT; // FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM
var
 SAUSNR, TAUSNR : TAUSNRecord;
 i, j, k : Integer; h : THandle;
 OldIdx, DelIdx, NewIdx : TIntegerDynArray;
 FFData : WIN32_FIND_DATA;
 FlSiz : UInt64; Path, FlNam: String; PthHsh: TCardinalDynArray;
 ATS : TArrToSort;
begin
 Result := false;
 if GetUsn(nsrc, SAUSNR) then exit else if GetUsn(ntgt, TAUSNR) then exit;
 i := 0;
 for j := 0 to Length(SAUSNR) - 1 do
  if BS(TAUSNR, SAUSNR[j].FileReferenceNumber, i, $FFFFFFFF, false) then
  begin
   k := Length(NewIdx); SetLength(NewIdx, k + 1); NewIdx[k] := j
  end else if CFP_FVP(SAUSNR, TAUSNR, j, i) then begin
   k := Length(OldIdx); SetLength(OldIdx, k + 1); OldIdx[k] := j
  end else begin
   k := Length(NewIdx); SetLength(NewIdx, k + 1); NewIdx[k] := j;
   k := Length(DelIdx); SetLength(DelIdx, k + 1); DelIdx[k] := i
  end;
 i := 0;
 for j := 0 to Length(TAUSNR) - 1 do
  if BS(SAUSNR, TAUSNR[j].FileReferenceNumber, i, $FFFFFFFF, false) then
  begin
   k := Length(DelIdx); SetLength(DelIdx, k + 1); DelIdx[k] := j
  end;

 // The list of full path names of deleted files without any sort
 for i := 0 to Length(DelIdx) - 1 do with TAUSNR[DelIdx[i]] do
  if (FileAttributes and SkipAttributes = 0)
  and CGPWF(SrcVolSpec, TAUSNR, DelIdx[i], Name, Path) then begin
   if Length(TgtVolSpec.GUID + Path + Name) < MAX_PATH then
    FlNam := Path + LowerCase(Name)
   else begin
    h := FindFirstFile(PWChar(TgtVolSpec.Root + Path + Name), FFData);
    if h <> INVALID_HANDLE_VALUE then continue;
    Windows.FindClose(h);
    FlNam := Path + LowerCase(FFData.cAlternateFileName)
   end;
   if FileExists(SrcVolSpec.Root + FlNam) then begin
    if HashCodesOfStrings(FlNam, PthHsh, true, false) then begin
     k := Length(OldFiles); SetLength(OldFiles, k + 1); OldFiles[k] := FlNam
    end
   end else begin
    k := Length(DelFiles); SetLength(DelFiles, k + 1); DelFiles[k] := FlNam
   end
  end;
 SetLength(TAUSNR, 0);

 // The list of full path names of old files without any sort
 for i := 0 to Length(OldIdx) - 1 do with SAUSNR[OldIdx[i]] do
  if (FileAttributes and SkipAttributes = 0)
  and CGPWF(SrcVolSpec, SAUSNR, OldIdx[i], Name, Path) then begin
   if Length(SrcVolSpec.GUID + Path + Name) < MAX_PATH then
    FlNam := Path + LowerCase(Name)
   else begin
    h := FindFirstFile(PWChar(SrcVolSpec.Root + Path + Name), FFData);
    if h <> INVALID_HANDLE_VALUE then continue;
    Windows.FindClose(h);
    FlNam := Path + LowerCase(FFData.cAlternateFileName);
   end;
   if HashCodesOfStrings(FlNam, PthHsh, true, false) then begin
    k := Length(OldFiles); SetLength(OldFiles, k + 1); OldFiles[k] := FlNam
   end
  end;

 // The list of full path names of old files without sorted by file size
 for i := 0 to Length(NewIdx) - 1 do with SAUSNR[NewIdx[i]] do
  if (FileAttributes and SkipAttributes = 0)
  and CGPWF(SrcVolSpec, SAUSNR, NewIdx[i], Name, Path) then begin
   FlNam := Path + LowerCase(Name);
   if Length(SrcVolSpec.GUID + FlNam) < MAX_PATH then begin
    h := CreateFile(PChar(SrcVolSpec.GUID + FlNam), GENERIC_READ,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if h = INVALID_HANDLE_VALUE then continue;
    FlSiz := GetFileSize(h, nil);
    CloseHandle(h);
    h := CreateFile(PChar(TgtVolSpec.GUID + FlNam),
      GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
      OPEN_EXISTING, 0, 0);
    if h <> INVALID_HANDLE_VALUE then begin CloseHandle(h); continue end
   end else begin
    h := FindFirstFile(PWChar(SrcVolSpec.Root + FlNam), FFData);
    if h = INVALID_HANDLE_VALUE then continue;
    Windows.FindClose(h);
    FlNam := Path + LowerCase(FFData.cAlternateFileName);
    FlSiz := UInt64(FFData.nFileSizeLow) or (UInt64(FFData.nFileSizeHigh) shl 32);
    h := CreateFile(PChar(SrcVolSpec.GUID + FlNam), GENERIC_READ,
        FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if h = INVALID_HANDLE_VALUE then continue;
    CloseHandle(h);
    h := FindFirstFile(PWChar(TgtVolSpec.GUID + FlNam), FFData);
    if h <> INVALID_HANDLE_VALUE then begin Windows.FindClose(h); continue end;
   end;
   k := Length(NewFiles);
   SetLength(NewFiles, k + 1); NewFiles[k] := FlNam;
   SetLength(NewFlsSz, k + 1); NewFlsSz[k] := FlSiz
  end;
 SetLength(SAUSNR, 0);
 Result := CheckNewFiles(NewFiles, bDoNTFSC, NewFlsSz, ClusterSize)
end;

function GetFilesByScanning(nsrc, ntgt: Integer;
      var OldFiles, DelFiles, NewFiles: TStringDynArray;
                          var bDoNTFSC: TBooleanDynArray;
                         var NewFlsSz: TCardinalDynArray;
                           ClusterSize: Integer): Boolean;
const
 faSkip = faDirectory or faSymLink;

 function AName(FullPath, Name: String): String;
 var
  FFData: WIN32_FIND_DATA; h: THandle;
 begin
  h := FindFirstFile(PWChar(FullPath + Name), FFData);
  if h = INVALID_HANDLE_VALUE then Result := '' else begin
   Windows.FindClose(h);
   if FFData.cAlternateFileName = '' then Result := Name else
    Result := FFData.cAlternateFileName
  end
 end;
 procedure AddFile(var Files: TStringDynArray; var HCS: TCardinalDynArray;
                   Root, RelPath, Name: String);
 var
  FFData: WIN32_FIND_DATA; h: THandle; i: Integer;
 begin
  if Length(Root + RelPath + Name) < MAX_PATH then
   Name := UpperCase(RelPath) + LowerCase(Name)
  else begin
   h := FindFirstFile(PWChar(Root + RelPath + Name), FFData);
   if h = INVALID_HANDLE_VALUE then exit;
   Windows.FindClose(h);
   if FFData.cAlternateFileName <> '' then Name := FFData.cAlternateFileName;
   Name := UpperCase(RelPath) + LowerCase(Name)
  end;
  if HashCodesOfStrings(Name, HCS, true, false) then begin
   i := Length(Files); SetLength(Files, i + 1); Files[i] := Name
  end;
 end;
 procedure SearchItems(var Files: TStringDynArray; var HCS: TCardinalDynArray;
                       Root, RelPath: String);
 var
  F: TSearchRec;
 begin
  if DirectoryExists(Root + RelPath) then
  begin
   if RelPath <> '' then
    RelPath := IncludeTrailingBackslash(RelPath);
   if FindFirst(Root + RelPath + '*', faAnyFile, F) = 0 then
    try
     repeat
      if (F.Attr and faDirectory <> 0) then begin
       if String(F.Name).Chars[0] <> '.' then
        if GetFileAttributes(PChar(Root + RelPath + F.Name)) and FILE_ATTRIBUTE_REPARSE_POINT = 0 then
         SearchItems(Files, HCS, Root, RelPath + AName(Root + RelPath, F.Name))
      end else
       if (F.Attr <> faInvalid) and (F.Attr and faSkip = 0) then
        if GetFileAttributes(PChar(Root + RelPath + F.Name)) and FILE_ATTRIBUTE_REPARSE_POINT = 0 then
         AddFile(Files, HCS, Root, RelPath, F.Name)
    until FindNext(F) <> 0
   finally
    FindClose(F)
   end
  end
 end;
 function GetFileSize(FilePath: String): Cardinal;
 var
  h: THandle;
 begin
  h := CreateFile(PChar(FilePath), GENERIC_READ,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if h = INVALID_HANDLE_VALUE then Result := HighUInt32 else begin
   Result := Windows.GetFileSize(h, nil);
   CloseHandle(h)
  end
 end;
var
 SrcFiles, TgtFiles: TStringDynArray; SrcHCS, TgtHCS: TCardinalDynArray;
 i, j: Integer; s: Cardinal;
begin
 SearchItems(SrcFiles, SrcHCS, SrcVolSpec.GUID, '');
 SearchItems(TgtFiles, TgtHCS, TgtVolSpec.GUID, '');
 for i := 0 to Length(SrcFiles) - 1 do
  if HashCodesOfStrings(SrcFiles[i], TgtHCS, false) then begin
   j := Length(OldFiles); SetLength(OldFiles, j + 1); OldFiles[j] := SrcFiles[i]
  end else begin
   s := GetFileSize(SrcVolSpec.GUID + SrcFiles[i]);
   if s <> HighUInt32 then begin
    j := Length(NewFiles); SetLength(NewFiles, j + 1); NewFiles[j] := SrcFiles[i];
    SetLength(NewFlsSz, j + 1); NewFlsSz[j] := s
   end
  end;
 for i := 0 to Length(TgtFiles) - 1 do
  if HashCodesOfStrings(TgtFiles[i], SrcHCS, false, false) then begin
   j := Length(DelFiles); SetLength(DelFiles, j + 1); DelFiles[j] := TgtFiles[i]
  end;
 Result := CheckNewFiles(NewFiles, bDoNTFSC, NewFlsSz, ClusterSize)
end;

function IsNtfsFileSytem(vn: Integer): Boolean;
var
 FileSystem: String;
begin
 Result := GetVolumeFileSystem(vn, FileSystem) and (FileSystem = 'NTFS')
end;

{ CheckUsnActive
  Checks the volume has active USN journal.
}
function CheckUsnActive(vn: Integer): Boolean;
var
 H : THandle;
begin
 Result := IsNtfsFileSytem(vn); if not Result then exit;
 H := GetRootHandle('globalroot\Device\HarddiskVolume' + IntToStr(vn),
  false, true);
 if H = INVALID_HANDLE_VALUE then exit;
 Result := AllocMFTEnumBuffer(H, false) <> nil;
 CloseHandle(H)
end;

// Call back functions can't be nested on x64 (24.0.22858.6822).
function CountUsnRecordsCallBack(AUSN: PUSNRecord; var pc: IntPtr): Boolean;
var
 c : NativeUInt;
begin
 c := NativeUInt(pc); Inc(c); pc := c; Result := true
end;

{ CountUsnRecords
  Counts number of USN records to define source volume by their comparison.
}
function CountUsnRecords(vn: Integer): NativeUInt;
var
 H : THandle; AMFTEnumBuff : Pointer;
begin
 Result := 0; if not IsNtfsFileSytem(vn) then exit;
 H := GetRootHandle('globalroot\Device\HarddiskVolume' + IntToStr(vn),
   false, true);
 if H = INVALID_HANDLE_VALUE then exit;
 AMFTEnumBuff := AllocMFTEnumBuffer(H, false);
 if AMFTEnumBuff <> nil then begin
  EnumMFTEntries(H, AMFTEnumBuff, @CountUsnRecordsCallBack, @Result);
  FreeMem(AMFTEnumBuff)
 end;
 CloseHandle(H)
end;

{ CountFilesByScanning
  Counts files on volume.
}
function CountFilesByScanning(vn: Integer): NativeUInt;
const
 faSkip = faDirectory or faSymLink;

 function AName(FullPath, Name: String): String;
 var
  FFData: WIN32_FIND_DATA; h: THandle;
 begin
  h := FindFirstFile(PWChar(FullPath + Name), FFData);
  if h = INVALID_HANDLE_VALUE then Result := '' else begin
   Windows.FindClose(h);
   if FFData.cAlternateFileName = '' then Result := Name else
    Result := FFData.cAlternateFileName
  end
 end;
 procedure SearchItems(Root, RelPath: String);
 var
  F: TSearchRec;
 begin
  if DirectoryExists(Root + RelPath) then
  begin
   if RelPath <> '' then
    RelPath := IncludeTrailingBackslash(RelPath);
   if FindFirst(Root + RelPath + '*', faAnyFile, F) = 0 then
    try
     repeat
      if (F.Attr and faDirectory <> 0) then begin
       if String(F.Name).Chars[0] <> '.' then
        if GetFileAttributes(PChar(Root + RelPath + F.Name)) and FILE_ATTRIBUTE_REPARSE_POINT = 0 then
         SearchItems(Root, RelPath + AName(Root + RelPath, F.Name))
      end else
       if (F.Attr <> faInvalid) and (F.Attr and faSkip = 0) then
        if GetFileAttributes(PChar(Root + RelPath + F.Name)) and FILE_ATTRIBUTE_REPARSE_POINT = 0 then
         Inc(Result)
    until FindNext(F) <> 0
   finally
    FindClose(F)
   end
  end
 end;
begin
 SearchItems(GetVolumeGuid('\\.\HarddiskVolume' + IntToStr(vn)), '')
end;

{ DefineSourceTargetByFileCount
  Counts items of 2 disks and selects source disk with bigger number of files.
}
procedure DefineSourceTargetByFileCount(var SrcDc, TgtDc, SrcVo, TgtVo: Integer);
var
 DskNumCnt1, DskNumCnt2: Cardinal;
 Guid1, Guid2: String;
 P, VolNum1, VolNum2: Integer;

 function GetVolNum(Guid: String): Integer;
 var
  i: Integer;
 begin
  Result := -1; i := 1;
  repeat
   if guid = GetVolumeGuid(Format(cfsGlobalVolume, [i])) then begin
    Result := i; exit
   end;
   Inc(i)
  until (27 <= i) and (guid = '');
 end;
 function GetGuids: Boolean;
 begin
  Inc(P);
  Guid1 := GetVolumeGuid(Format(cfsDiskPartition, [SrcDc, P]));
  Guid2 := GetVolumeGuid(Format(cfsDiskPartition, [TgtDc, P]));
  VolNum1 := GetVolNum(Guid1); VolNum2 := GetVolNum(Guid2);
  Result := (-1 < VolNum1) and (-1 < VolNum2) and (Guid1 <> '') and (Guid2 <> '')
 end;
begin
 P := 0; DskNumCnt1 := 0; DskNumCnt2 := 0;
 while GetGuids do
  if bUseUsnQueries and CheckUsnActive(VolNum1) and CheckUsnActive(VolNum2) then
  begin
   DskNumCnt1 := DskNumCnt1 + CountUsnRecords(VolNum1);
   DskNumCnt2 := DskNumCnt2 + CountUsnRecords(VolNum2)
  end else
   if IsNtfsFileSytem(VolNum1) and IsNtfsFileSytem(VolNum2) then begin
    DskNumCnt1 := DskNumCnt1 + CountFilesByScanning(VolNum1);
    DskNumCnt2 := DskNumCnt2 + CountFilesByScanning(VolNum2)
   end;
 if DskNumCnt1 < DskNumCnt2 then begin
  P := TgtDc; TgtDc := SrcDc; SrcDc := P; P := TgtVo; TgtVo := SrcVo; SrcVo := P
 end
end;

end.
