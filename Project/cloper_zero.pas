{ cloper_zero
  The unit of Cloning Optimizer (Cloper) tool.

  The unit encapsulates funcionality for zeroing free space of volume.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit cloper_zero;

interface
uses
 cloper_typs;

{ FillByZeroes
  Fills free space of disk by writing bytes to files. Can perform several runs,
  the single run corresponds to zeroing.
}
function FillByZeroes : Boolean;

implementation
uses
 cloper_funs, cloper_base, Windows, Types, Classes, SysUtils, VolTools, StrTools,
 ConTools, cloper_echo;

function ZFVS_FinalizeFile(var Handle: THandle; Size: Int64;
  var ErrCnt: Integer; bReportError : Boolean = false): Boolean;
type
 FILE_END_OF_FILE_INFO = record
   EndOfFile: LARGE_INTEGER;
 end;
 TSetFileInformationByHandle = function(hFile: THandle;
    FileInformationClass: ShortInt; lpFileInformation: Pointer;
    dwBufferSize: DWORD): BOOL; stdcall;
label
 exi;
const
 FileEndOfFileInfo = 6;
var
 SetFileInformationByHandle : TSetFileInformationByHandle;
 eof: FILE_END_OF_FILE_INFO;
begin
 Result := false;
 if Handle = INVALID_HANDLE_VALUE then exit else if ToolPlatform = 'x64' then
  goto exi;
 if OSMajorMinorVersion < 60 then
  // WinXP needs this library on disk for its use (in SDK, optional).
  @SetFileInformationByHandle := GetProcAddress(GetModuleHandle('fileextd'),
    'SetFileInformationByHandle')
 else
  @SetFileInformationByHandle := GetProcAddress(GetModuleHandle('kernel32'),
    'SetFileInformationByHandle');
 if not Assigned(SetFileInformationByHandle) then goto exi;

 // Set the final size of file:
 eof.EndOfFile.QuadPart := Size;
 Result := SetFileInformationByHandle(Handle, FileEndOfFileInfo, @eof,
    SizeOf(eof));
 if not Result then begin
  Inc(ErrCnt);
  if bReportError then OutputSysError(true)
 end;
exi:
 FlushFileBuffers(Handle);
 CloseHandle(Handle);
 Handle := INVALID_HANDLE_VALUE
end;

function ZFVS_DeleteFile(FileName : PChar) : Boolean; begin
 if FileExists(FileName) then begin
  DeleteFileNT32(FileName);
  ResetVolume(SrcVolSpec.GUID);
  if FileExists(FileName) then DeleteFileNT32(FileName)
 end;
 Result := FileExists(FileName);
end;

{ FillByZeroes(Imp)
  Fills free space of disk by writing bytes to files. Can perform several runs,
  the single run corresponds to zeroing.

  Uses new files for every next part of free space (count/2) with changed size
  of buffer (buffer/3). The 1st files are written until the free space is less
  than BufSize * NumOfFiles / 3.

  Sample for original 57000000000 of free bytes:
  Starting      Files Buffer    Populated         Ending
  free space     num    size      space        free space
  57000000000 -> 512  2097152 (43240585728) -> 13759414272   ( /3 )
  13759414272 -> 256  1048576 ( 9172942848) ->  4586471424   ( /3 )
   4586471424 -> 128   542288 ( 3774873600) ->  1528823808   ( /3 )
   1258291200 ->  64   262144 (  943718400) ->   509607936   ( /3 )
    314572800 ->  32   131072 (  235929600) ->   169869312   ( /3 )
     78643200 ->  16    65536 (   58982400) ->    56623104   ( /3 )
     19660800 ->   8    32758 (   14745600) ->    18874368   ( /3 )
      4915200 ->   4    16384 (    3686400) ->     6291456   ( /3 )
      1228800 ->   2     8192 (     921600) ->     2097152   ( /3 )
       307200 ->   1     4096 (     307200) ->           0   (-> 0)
}
function FillByZeroesImp(var path: String): Boolean;
const
  BufSize = 2097152; {~2MB}
  NumOfRuns = 1; // If the number of runs is odd then zeroing on every run.
                 // If the number of runs is even then:
                 // "Number Of Current Run" mod 2 = 1 - write units,
                 // "Number Of Current Run" mod 2 = 0 - write zeroes.
  NumOfFiles = 512;                // Start with this number of files.
  CntOfFiles = 2 * NumOfFiles - 1; // Array size with member for every new file
  FileNames = 'CZFS%s.$$$';
var
  NumBytes, FreeBytesO, FreeBytesN, FilesSize: Int64;
  i, j, RC, FlNmCnt, ErrCnt, ErrCod, BegNOF, EndNOF, SubNOF, CntNOF,
    cPass, BufSizeBeg, BufSizeCur: Integer;
  BytesA : TByteDynArray;
  BytesP : PByte absolute BytesA;
  FB0F : array[1..CntOfFiles] of TFB0File;
  junk : DWORD;
  bfin, bend : Boolean; dt: TDateTime;

  function _GetNewFileNames : Boolean;
  var
   i : Integer;
  begin
   Result := true;
   for i := 1 to CntOfFiles do begin
    ZeroMemory(@FB0F[i], sizeof(TFB0File));
    FB0F[i].FileName := path + Format(FileNames,
        [PadLeft(IntToStr(FlNmCnt), 4, '0')]);
    Inc(FlNmCnt);
   end;
   Result := false
  end;
  function _GetNextFiles: Boolean;
  var
   i : Integer; Cluster : Integer;
  begin
   Result := true; FilesSize := 0; cPass := 1;
   for i := BegNOF to EndNOF do
    ZFVS_FinalizeFile(FB0F[i].Handle, FB0F[i].Size, ErrCnt);
   ResetVolume(SrcVolSpec.GUID);
   Sleep(5000); // 5 sec for driver to clean cache.
   bfin := false;

   // Update volume information after finalization of previous files:
   if not GetVolSpecs(SrcVolSpec) then
    if EchoFailure(4) then exit;
   Cluster := SrcVolSpec.SectorsPerCluster * SrcVolSpec.BytesPerSector;
   FreeBytesN := Int64(SrcVolSpec.NumOfFreeClusters) * Cluster;

   BufSizeCur := BufSizeCur div 2;
   if BufSizeCur < Cluster then BufSizeCur := Cluster;

   if BufSizeCur = 0 then begin Result := false; exit end else
    if BufSizeCur <> Length(BytesA) then
     SetLength(BytesA, BufSizeCur);

   SubNOF := SubNOF div 2;
   if SubNOF = 0 then begin Result := false; exit end;
   BegNOF := EndNOF + 1;
   EndNOF := EndNOF + SubNOF;
   Sleep(5);
   for i := BegNOF to EndNOF do if CreateNewSystemFile(FB0F[i], '', false) then
   begin
    Sleep(5);
    Inc(ErrCnt);
    if not GetVolSpecs(SrcVolSpec) and EchoFailure(4) then exit;
    FreeBytesN := Int64(SrcVolSpec.NumOfFreeClusters) * Cluster;
    if bend or (FreeBytesN < BufSizeCur) then begin
     OutputMessage(mLowPriErr, GetFailureByNumber(2));
     if i = BegNOF then begin
      EndNOF := BegNOF; SubNOF := 0
     end else begin
      Dec(SubNOF, EndNOF - i + 1); EndNOF := i - 1
     end;
     break
    end else
     if EchoFailure(2) then exit
   end else
    Sleep(5);

   if 0 < SubNOF then CntNOF := CntNOF + SubNOF;
   Result := false
  end;
  // Attempt to fill left free space after completion. It's likely locked.
  procedure _DoLastPatch;
  var
   i: Integer;
  begin
   if FreeBytesN = 0 then exit;
   BufSizeCur := SrcVolSpec.SectorsPerCluster * SrcVolSpec.BytesPerSector;
   SetLength(BytesA, BufSizeCur);
   if (NumOfRuns mod 2 = 0) and (RC mod 2 = 1) then i := 255 else i := 0;
   FillMemory(BytesA, BufSizeCur, i);
   i := 0;
   while (0 < FreeBytesN) and (i < Length(FB0F)) do begin
    if FB0F[i].Size = 0 then begin
     if CreateNewSystemFile(FB0F[i]) then
      Inc(ErrCnt)
     else begin
      while (0 < FreeBytesN) and WriteFile(FB0F[i].Handle, BytesP^, BufSizeCur, junk, nil) do begin
       FlushFileBuffers(FB0F[i].Handle);
       FreeBytesN := FreeBytesN - BufSizeCur;
       FB0F[i].Size := FB0F[i].Size + BufSizeCur;
       Sleep(10)
      end;
      ZFVS_FinalizeFile(FB0F[i].Handle, FB0F[i].Size, ErrCnt);
      Sleep(2500);
      if GetVolSpecs(SrcVolSpec) then
       FreeBytesN := Int64(SrcVolSpec.NumOfFreeClusters) * BufSizeCur
      else begin
       Inc(ErrCnt); break
      end
     end
    end;
    Inc(i)
   end;
  end;
begin
 Result := false;
 Inc(SubTasksNum);
 try
  path := SrcVolSpec.Root + '$CZFSTMP.$$$\';
  if DirectoryExists(path) then begin
   if CleanUpDirectory(path, Format(FileNames, ['CZFS*'])) = drError then
    if EchoFailure(12) then
     exit
  end else begin
   if CreateDirectory(PWChar(path), nil) then
    SetFileAttributes(PWChar(path), FILE_ATTRIBUTE_DIRECTORY or
          FILE_ATTRIBUTE_READONLY or FILE_ATTRIBUTE_HIDDEN or
          FILE_ATTRIBUTE_SYSTEM)
   else
    if EchoFailure(2) then
     exit
  end;
  ErrCnt := 0;
  RC := 0;
  FlNmCnt := 1;
  if _GetNewFileNames then exit else
   ResetVolume(SrcVolSpec.GUID);

  // Update source volume information after optimizations:
  if not GetVolSpecs(SrcVolSpec) then
   if PrintFailure(SrcVolSpec, 4) then exit;

  NumBytes := Int64(SrcVolSpec.NumOfFreeClusters) * SrcVolSpec.SectorsPerCluster
                                               * SrcVolSpec.BytesPerSector;
  TCrtcProgress.Initialize('zeroing free space of volume #' +
                           IntToStr(SrcVolSpec.VolumeNumber),
        'zeroing',
        ['Number of system write errors'], [''],
        NumOfRuns * NumBytes, 0, '', Byte(mcSubCat));

  BufSizeBeg := (BufSize + (SrcVolSpec.BytesPerSector - 1))
                  and (not (SrcVolSpec.BytesPerSector - 1));
  repeat
   BegNOF := 1;
   SubNOF := NumOfFiles;
   EndNOF := SubNOF;
   CntNOF := SubNOF;

   if RC = 0 then RC := 1 else begin
    if _GetNewFileNames then exit;
    ResetVolume(SrcVolSpec.GUID);
    // Update source volume information:
    if not GetVolSpecs(SrcVolSpec) then
     if EchoFailure(4) then exit;
    NumBytes := Int64(SrcVolSpec.NumOfFreeClusters) * SrcVolSpec.SectorsPerCluster
                                                    * SrcVolSpec.BytesPerSector
   end;

   Sleep(750);
   FilesSize := 0;
   for i := BegNOF to EndNOF do if CreateNewSystemFile(FB0F[i]) then
    if EchoFailure(2) then exit;
   bend := false;

   SetLength(BytesA, BufSizeBeg);
   if NumOfRuns mod 2 = 0 then
    if RC mod 2 = 1 then
     FillMemory(BytesA, BufSizeBeg, 255)
    else
     ZeroMemory(BytesA, BufSizeBeg)
   else
    ZeroMemory(BytesA, BufSizeBeg);
   OutputMessage(mEcoNoDtLP,
        '<align>Started filling of volume''s free space by "' +
        IntToStr(BytesA[0]) + '" bytes...');

   FreeBytesN := NumBytes;
   BufSizeCur := BufSizeBeg;
   repeat
    FreeBytesO := FreeBytesN;
    cPass := 0;
    for i := BegNOF to EndNOF do begin
     if BufSizeCur = BufSizeBeg then begin
      if 3 * FreeBytesN < UInt64(BufSizeCur) * NumOfFiles then
       if _GetNextFiles then exit else begin
        OutputMessage(mEcoLowPri, 'Reached end of volume, ' +
                'system errors are possible at the rest part');
        bend := true;
        break
       end
     end else begin
      if SubNOF = 1 then begin
       if FreeBytesN < BufSizeCur then //  == 4096 (cluster size)
        if _GetNextFiles then exit else break
      end else
       if (3 * FreeBytesN < FilesSize) or (3 * FreeBytesN < BufSizeCur) then
        if _GetNextFiles then exit else break
     end;
     bfin := true;

     if WriteFile(FB0F[i].Handle, BytesP^, BufSizeCur, junk, nil) then begin
      FlushFileBuffers(FB0F[i].Handle);
      Inc(cPass);
      FB0F[i].Size := FB0F[i].Size + BufSizeCur;
      FreeBytesN := FreeBytesN - BufSizeCur;
      FilesSize := FilesSize + BufSizeCur;

      TCrtcProgress.AuxCI[1] := TCrtcProgress.AuxCI[1] + BufSizeCur;
      TCrtcProgress.Echo(TCrtcProgress.AuxCI[1], [ErrCnt]);
      if FreeBytesN = 0 then break
     end else begin
      // Other write processes decrease free space, gives errors:
      // ERROR_DISK_FULL (112), ERROR_INVALID_PARAMETER (87).
      // Minor failures by default, flushes files & decreases write buffer:
      ErrCod := GetLastError();
      Inc(ErrCnt);
      if (ErrCod <> ERROR_DISK_FULL) and (ErrCod <> ERROR_INVALID_PARAMETER) then
       OutputSysError(true, '', ErrCod);
      if _GetNextFiles then exit else break
     end
    end;
    if (cPass = 0) and (BufSizeBeg < FreeBytesN) then begin
     OutputSysError(true,
            'FillByZeroes, last system error', ErrCod);
     OutputMessage(mWarning,
            'Optimization failure, cleaning source disk and exiting...');
     for i := 1 to CntNOF do begin
      if ZFVS_DeleteFile(PWChar(FB0F[i].FileName)) then
       if EchoFailure(12) then
        exit;
      ZeroMemory(@FB0F[i], sizeof(TFB0File))
     end;
     ResetVolume(SrcVolSpec.GUID);
     exit
    end
   until (0 = FreeBytesN) or (0 = BufSizeCur) or (0 = SubNOF);
   OutputMessage(mEcoLowPri, 'Finalization of write operations...');
   if bfin then _GetNextFiles;
   _DoLastPatch;

   dt := Now;
   ResetVolume(SrcVolSpec.DeviceNumber, false);
   for i := 1 to CntNOF do begin
    if Round((Now - dt) * 1440) = 0 then // 2 minutes, sometimes too slow.
     if ZFVS_DeleteFile(PWChar(FB0F[i].FileName)) then
      if EchoFailure(12) then
       exit;
    ZeroMemory(@FB0F[i], sizeof(TFB0File))
   end;
   ResetVolume(SrcVolSpec.GUID);
   Sleep(1000);
   Inc(RC)
  until RC > NumOfRuns;
  DeleteDirectory(path);
  ResetVolume(SrcVolSpec.GUID);
  Result := true
 except on E : Exception do begin
  OutputMessage(mInternalErr, E.Message);
  OutputSysError(true);
  OutputSysError(true,
            'FillByZeroes, last system error', ErrCod);
 end end;
 TCrtcProgress.InsertSummaryString(-1, 'The number of not zeroed bytes is', '');
 TCrtcProgress.Summary('ZEROING FREE SPACE SUMMARY:',
      [ErrCnt, FreeBytesN], Result)
end;

function FillByZeroes : Boolean;
var
 path: String;
begin
 Result := FillByZeroesImp(path);
 if not Result then begin
  DeleteDirectory(path);
  ResetVolume(SrcVolSpec.GUID)
 end
end;

end.
