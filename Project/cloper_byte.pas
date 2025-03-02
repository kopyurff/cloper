{ cloper_byte
  The unit of Cloning Optimizer (Cloper) tool.

  The file contains basic functionality for volume data read & comparison and
  basic routines to perform patching of source volume segment by target data.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit cloper_byte;

interface
uses
 Windows, cloper_typs;
type
 TPchBlkType = (pbData = 0, pbScrap = 1, pbFF = 2, pb00 = 3);

 function ByteWiseCompare(FL: TFileLayoutInformation): Boolean; overload;
 function ByteWiseCompare(hScVol: THandle;
                          FLS: TFileLayoutInformation): Boolean; overload;
 function ByteWiseCompare(hScVol: THandle;
                          BL: UINT64; EL: UINT64 = 0;
                          hTgVol: THandle = INVALID_HANDLE_VALUE): Boolean; overload;

 function CountEqualSectors(hScVol: THandle;
                            BL: UINT64; EL: UINT64 = 0;
                            hTgVol: THandle = INVALID_HANDLE_VALUE): Cardinal;

 function PatchBlock(hScVol: THandle; BL: UINT64; typ: TPchBlkType = pbData;
                                      EL: UINT64 = 0;
                                      hTgVol: THandle = INVALID_HANDLE_VALUE): Boolean;

procedure InitializeFindPatches;
function FindTargetDataAndPatch(var MemPtch, MemFail, cPtchF: Cardinal;
                                hScVol: THandle; BegLcn: UINT64; EndLcn: UINT64;
                                hTgVol: THandle = INVALID_HANDLE_VALUE): Cardinal;
procedure VerifyPatches(var MemPtch, MemFail, cPtchF, MemDrpd, cDrpdF: Cardinal);

var
 MaxLCN : LARGE_INTEGER;

implementation
uses
 SysUtils, Types, cloper_funs, cloper_base, Defragger, StrTools;

var
 Patch : TFB0File = (Size: 0; Handle: INVALID_HANDLE_VALUE; FileName: '');

function ByteWiseCompare(FL: TFileLayoutInformation): Boolean; overload;
var
 hScFil, hTgFil: THandle;
 ScBuf, TgBuf: TByteDynArray; SzScBuf, SzTgBuf: Cardinal;
 uisa: Int64; uisz, size: UInt32; i, j, k: Int32;
begin
 Result := false;
 if not GetFileHandle(SrcVolSpec.GUID + FL.FName, GENERIC_READ, FILE_SHARE_READ,
        hScFil, FILE_FLAG_NO_BUFFERING or FILE_FLAG_SEQUENTIAL_SCAN) then
  exit;
 if not GetFileHandle(TgtVolSpec.GUID + FL.FName, GENERIC_READ, FILE_SHARE_READ,
        hTgFil, FILE_FLAG_NO_BUFFERING or FILE_FLAG_SEQUENTIAL_SCAN) then
  exit;
 uisa := 0; size := FL.ClusCount * SectorsPerCluster; k := 0;
 with SrcVolSpec do repeat
  if uisa + ReadStep < size then uisz := ReadStep else uisz := size - uisa;
  SzScBuf := ReadSectors(hScFil, ScBuf, uisa, uisz, BytesPerSector);
  SzTgBuf := ReadSectors(hTgFil, TgBuf, uisa, uisz, BytesPerSector);
  i := 0;
  while (i < SzScBuf) and (i < SzTgBuf) do begin
   for j := i to i + BytesPerSector - 1 do if ScBuf[j] <> TgBuf[j] then begin
    Inc(k); break
   end;
   i := i + BytesPerSector
  end;
  uisa := uisa + ReadStep
 until uisa >= size;
 CloseHandle(hScFil); CloseHandle(hTgFil);
 Result := 100 * k < ModificationThreshold * size
end;

function ByteWiseCompare(hScVol: THandle;
                         FLS: TFileLayoutInformation): Boolean; overload;
var
 hTgVol: THandle;
 ScBuf, TgBuf: TByteDynArray; SzScBuf, SzTgBuf: Cardinal;
 pb, pn: UInt64; bps, ss: UInt32; i, j, k, m: Int32;
begin
 Result := false;
 hTgVol := CreateFile(PWChar(ExcludeTrailingBackslash(TgtVolSpec.GUID)),
    GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
    OPEN_EXISTING, FILE_FLAG_NO_BUFFERING or FILE_FLAG_SEQUENTIAL_SCAN, 0);
 if hTgVol = INVALID_HANDLE_VALUE then exit;

 i := 0; bps := SrcVolSpec.BytesPerSector;
 for j := 0 to Length(FLS.Extents) - 1 do begin
  pb := SectorsPerCluster * FLS.Extents[j].Lcn.QuadPart;
  pn := SectorsPerCluster * FLS.Extents[j].Vcs + pb;
  repeat
   if pb + ReadStep < pn then ss := ReadStep else ss := pn - pb;
   SzTgBuf := ReadSectors(hTgVol, TgBuf, pb, ss, bps);
   SzScBuf := ReadSectors(hScVol, ScBuf, pb, ss, bps);
   k := 0;
   repeat
    for m := k to k + bps - 1 do if ScBuf[m] <> TgBuf[m] then begin
     Inc(i); break
    end;
    k := k + bps
   until SzScBuf <= k;
   pb := pb + ss
  until pn <= pb
 end;
 CloseHandle(hTgVol);
 Result := 100 * i < ModificationThreshold * SectorsPerCluster * FLS.ClusCount
end;

function CountEqualSectors(hScVol: THandle;
                           BL: UINT64; EL: UINT64 = 0;
                           hTgVol: THandle = INVALID_HANDLE_VALUE): Cardinal;
var
 ScBuf, TgBuf: TByteDynArray; SzScBuf, SzTgBuf: Cardinal; bTgVol: Boolean;
 bps, st: UInt32; i, j: Int32;
begin
 Result := 0; bTgVol := hTgVol = INVALID_HANDLE_VALUE;
 if bTgVol then begin
  hTgVol := CreateFile(PWChar(ExcludeTrailingBackslash(TgtVolSpec.GUID)),
     GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
     OPEN_EXISTING, FILE_FLAG_NO_BUFFERING or FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if hTgVol = INVALID_HANDLE_VALUE then exit
 end;
 if EL = 0 then EL := BL;
 bps := SrcVolSpec.BytesPerSector;
 BL := SectorsPerCluster * BL; EL := SectorsPerCluster * EL;
 Result := EL - BL + SectorsPerCluster;
 repeat
  if BL + ReadStep <= EL then st := ReadStep else
   st := EL - BL + SectorsPerCluster;
  SzTgBuf := ReadSectors(hTgVol, TgBuf, BL, st, bps);
  SzScBuf := ReadSectors(hScVol, ScBuf, BL, st, bps);
  i := 0;
  repeat
   for j := i to i + bps - 1 do if ScBuf[j] <> TgBuf[j] then begin
    Dec(Result); break
   end;
   i := i + bps
  until SzScBuf <= i;
  BL := BL + st
 until EL < BL;
 if bTgVol then CloseHandle(hTgVol)
end;

function ByteWiseCompare(hScVol: THandle; BL: UINT64; EL: UINT64 = 0;
                         hTgVol: THandle = INVALID_HANDLE_VALUE): Boolean; overload;
begin
 Result := CountEqualSectors(hScVol, BL, EL, hTgVol)
         > (EL - BL + 1) * SectorsPerCluster
                         * (100 - ModificationThreshold) div 100
end;

type
 TPatchFile = record
  Lcn: UInt64;
  Size: Cardinal;
  FileName : String;
 end;
 TPatchFileDynArray = array of TPatchFile;
var
 PatchFiles: TPatchFileDynArray;
 bPatchFileIni: Boolean = false;
 DecIntLng: Cardinal = 0;

procedure InitializeFindPatches;
begin
 SetLength(PatchFiles, 0); DecIntLng := 0; bPatchFileIni := true;
end;

function PatchBlock(hScVol: THandle; BL: UINT64; typ: TPchBlkType = pbData;
                                     EL: UINT64 = 0;
                                     hTgVol: THandle = INVALID_HANDLE_VALUE): Boolean;
label exi, fai; var
 bCsTgHd : Boolean; PchFil : TFB0File;
 TgBuf: TByteDynArray; SzScBuf, SzTgBuf: Cardinal;
 BS, ES, LC, CL: UInt64; BPS, RC, bsf, stf: Cardinal;

 function FullNameForNewFile : String;
 var
  i: Integer; Path: String;
 begin
  Path := SrcVolSpec.GUID + '$CZFSTMP.$$$\Patch';
  if DecIntLng = 0 then begin
   DecIntLng := Length(IntToStr(High(Int32)));
   if not DirectoryExists(Path) then
    ForceDirectories(Path)
  end;

  repeat
   Randomize;
   i := Random(High(Int32));
   Result := Path + '\CPB' + PadLeft(IntToStr(i), DecIntLng, '0') + '.$$$'
  until not FileExists(Result)
 end;
begin
 Result := false;
 if typ in [pbData, pbScrap] then begin
  bCsTgHd := hTgVol = INVALID_HANDLE_VALUE;
  if bCsTgHd then begin
   hTgVol := CreateFile(PWChar(ExcludeTrailingBackslash(TgtVolSpec.GUID)),
       GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
       OPEN_EXISTING, FILE_FLAG_NO_BUFFERING or FILE_FLAG_SEQUENTIAL_SCAN, 0);
   if hTgVol = INVALID_HANDLE_VALUE then exit
  end
 end;

 LC := BL; BS := SectorsPerCluster * BL; RC := 1; BPS := SrcVolSpec.BytesPerSector;
 if EL = 0 then begin ES := BS; EL := BL end else ES := SectorsPerCluster * EL;
 if typ = pbScrap then begin
  if CreateNewSystemFile(PchFil, FullNameForNewFile) then goto fai;
  RC := EL - BL + 1; bsf := 0
 end else
  if Patch.Handle = INVALID_HANDLE_VALUE then begin
   if CreateNewSystemFile(Patch, SrcVolSpec.GUID + '$CZFSTMP.$$$\COPDP.$$$') then
    goto fai;
   SzTgBuf := ReadSectors(hTgVol, TgBuf, BL, SectorsPerCluster, BPS);
   SzScBuf := WriteSectors(Patch.Handle, 0, SectorsPerCluster, @TgBuf[0], BPS) // Create file
  end;

 repeat
  if typ = pbScrap then begin
   if BS + ReadStep <= ES then stf := ReadStep else stf := ES - BS + SectorsPerCluster;
   SzTgBuf := ReadSectors(hTgVol, TgBuf, BS, stf, BPS);
   SzScBuf := WriteSectors(PchFil.Handle, bsf, stf, @TgBuf[0], BPS);
   BS := BS + stf; bsf := bsf + stf
  end else begin
   if (MoveToLcn(hScVol, LC, Patch.FileName, RC, CL) <> 0) or (LC <> CL) then
    goto fai;
   if typ = pbData then
    SzTgBuf := ReadSectors(hTgVol, TgBuf, BS, SectorsPerCluster, BPS)
   else begin
    SzTgBuf := SectorsPerCluster * BPS; SetLength(TgBuf, SzTgBuf);
    if typ = pbFF then FillMemory(TgBuf, SzTgBuf, 255) else
     ZeroMemory(TgBuf, SzTgBuf)
   end;
   SzScBuf := WriteSectors(Patch.Handle, 0, SectorsPerCluster, @TgBuf[0], BPS);
   BS := BS + SectorsPerCluster; Inc(LC)
  end;
 until ES < BS;

 if typ = pbScrap then begin
  FlushFileBuffers(PchFil.Handle);
  Sleep(1);
  if (MoveToLcn(hScVol, LC, PchFil.FileName, RC, CL) <> 0) or (LC <> CL) or
        (RC <> EL + 1 - BL) then
   goto fai;
  CloseHandle(PchFil.Handle);
  if bPatchFileIni then begin
   BS := Length(PatchFiles); SetLength(PatchFiles, BS + 1);
   PatchFiles[BS].FileName := PchFil.FileName;
   PatchFiles[BS].Lcn := LC; PatchFiles[BS].Size := RC
  end
 end else begin
  FlushFileBuffers(Patch.Handle);
  Sleep(1);
  if not FindFreeBlock(hScVol, MaxLCN.QuadPart, RC, LC, CL) then goto fai else
   if not (MoveToLcn(hScVol, LC, Patch.FileName, RC, CL) in [0, 5]) then
    goto fai
 end;
 Result := true;
fai:
 if not Result then
  if typ = pbScrap then begin
   if PchFil.Handle <> INVALID_HANDLE_VALUE then begin
    CloseHandle(PchFil.Handle);
    if FileExists(PchFil.FileName) then DeleteFileNT32(PchFil.FileName)
   end;
  end else if Patch.Handle <> INVALID_HANDLE_VALUE then begin
   CloseHandle(Patch.Handle); Patch.Handle := INVALID_HANDLE_VALUE
  end;
exi:
 if (typ in [pbData, pbScrap]) and bCsTgHd then CloseHandle(hTgVol)
end;

function FindTargetDataAndPatch(var MemPtch, MemFail, cPtchF: Cardinal;
                                hScVol: THandle; BegLcn: UINT64; EndLcn: UINT64;
                                hTgVol: THandle = INVALID_HANDLE_VALUE): Cardinal;
var
 bTgVol, bFBlok, bFound : Boolean;
 TgBuf: TByteDynArray; SzTgBuf: Cardinal;
 bps, step, ScrIdx: UInt32; i, j: Int32;
 BegScr, EndScr, LcnBeg, Lcn, LcnEnd: UINT64;

 procedure DoPatch(LcnBeg : UINT64);
 var
  stp : Cardinal; bMax, bPas, bFai : Boolean;
 begin
  bMax := true; Lcn := 0;
  repeat
   if bMax or (LcnEnd - LcnBeg + 1 < stp) then begin
    bFai := false;
    stp := LcnEnd - LcnBeg + 1
   end;
   if bPas and (2 * stp < LcnEnd - LcnBeg + 1) then begin
    stp := 2 * stp;
    Sleep(25)
   end;

   bPas := PatchBlock(hScVol, LcnBeg, pbScrap, LcnBeg + stp - 1, hTgVol);
   while not bPas and (1 < stp) do begin
    if 0 < stp div 3 then stp := stp div 3 else stp := 1;
    Sleep(25);
    bPas := PatchBlock(hScVol, LcnBeg, pbScrap, LcnBeg + stp - 1, hTgVol);
    bMax := false
   end;
   if bPas then begin
    if bFai then bMax := true;
    MemPtch := MemPtch + stp;
    Inc(cPtchF)
   end else begin
    Assert(stp = 1, 'FindTargetDataAndPatch.DoPatch: Unexpected size "1 <> ' + IntToStr(stp) + '" of failed patch.');
    bFai := true;
    MemFail := MemFail + 1
   end;
   LcnBeg := LcnBeg + stp
  until LcnEnd < LcnBeg;
 end;
begin
 Result := 0; bTgVol := hTgVol = INVALID_HANDLE_VALUE;
 if bTgVol then begin
  hTgVol := CreateFile(PWChar(ExcludeTrailingBackslash(TgtVolSpec.GUID)),
     GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
     OPEN_EXISTING, FILE_FLAG_NO_BUFFERING or FILE_FLAG_RANDOM_ACCESS, 0);
  if hTgVol = INVALID_HANDLE_VALUE then exit
 end;
 if EndLcn = 0 then EndLcn := BegLcn;
 bps := SrcVolSpec.BytesPerSector;
 BegScr := SectorsPerCluster * BegLcn; EndScr := SectorsPerCluster * EndLcn;

 Result := 0; bFBlok := false;
 repeat
  if BegScr + ReadStep <= EndScr then step := ReadStep else
   step := EndScr - BegScr + SectorsPerCluster;
  SzTgBuf := ReadSectors(hTgVol, TgBuf, BegScr, step, bps);
  i := 0; Lcn := 0;
  repeat
   bFound := false;
   for j := i to i + bps - 1 do if 0 < TgBuf[j] then begin
    bFound := true; ScrIdx := i; break
   end;
   if bFound and not bFBlok then begin
    LcnBeg := BegScr div SectorsPerCluster
           + (BegScr mod SectorsPerCluster + ScrIdx div bps) div SectorsPerCluster;
    if Lcn = 0 then Inc(Result) else
     if LcnEnd + 1 < LcnBeg then begin DoPatch(Lcn); Inc(Result) end else
      LcnBeg := Lcn;
    bFBlok := true
   end else if not bFound and bFBlok then begin
    Lcn := LcnBeg;
    LcnEnd := BegScr div SectorsPerCluster
           + (BegScr mod SectorsPerCluster + ScrIdx div bps) div SectorsPerCluster;

    bFBlok := false;
    i := i + (SectorsPerCluster - i div bps mod SectorsPerCluster - 1) * bps
   end;
   i := i + bps
  until SzTgBuf <= i;
  if not bFBlok and (0 < Lcn) then
   DoPatch(Lcn);

  BegScr := BegScr + step
 until EndScr < BegScr;

 if bFBlok then begin
  BegScr := BegScr - step;
  LcnEnd := BegScr div SectorsPerCluster
         + (BegScr mod SectorsPerCluster + ScrIdx div bps) div SectorsPerCluster;
  DoPatch(LcnBeg)
 end;

 if bTgVol then CloseHandle(hTgVol)
end;

procedure VerifyPatches(var MemPtch, MemFail, cPtchF, MemDrpd, cDrpdF: Cardinal);
var
 i: Integer; FL: UInt64; FM, RC: Cardinal;
begin
 MemDrpd := 0; cDrpdF := 0;
 if not bPatchFileIni or (Length(PatchFiles) = 0) then exit;

 for i := 0 to Length(PatchFiles) - 1 do with PatchFiles[i] do begin
  AnalyzeFile(FileName, FM, RC, FL);
  if (Size <> RC) or (Lcn <> FL) or (FM <> 1) then begin
   MemPtch := MemPtch - RC; Dec(cPtchF); MemFail := MemFail + RC;
   MemDrpd := MemDrpd + RC; Inc(cDrpdF);
   if FileExists(FileName) then DeleteFileNT32(FileName)
  end;
 end;
 bPatchFileIni := false; SetLength(PatchFiles, 0)
end;

end.
