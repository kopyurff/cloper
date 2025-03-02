{ cloper_hash
  The unit of Cloning Optimizer (Cloper) tool.

  The file contains basic functionality for search of the source data fitting
  target data blocks:
  1. Routines for obtaining source & target data hash codes, indexation of
     source codes;
  2. Routine for search of the source data fitting target data.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit cloper_hash;

interface
uses
 cloper_typs, Types, Windows;

type
 THshCodAction = (hcDrop = 0, hcAdd = 1, hcAddIndex = 2, hcIndex = 3);

function GetTargetHashCodes(PEx: PExtent): Boolean;

function HashCodesIndex(var CntIndx: Cardinal; VolHnd: THandle;
                                PFL: PFilesLayoutInformation; ix: Integer;
                                Typ: THshCodAction): Boolean;

procedure DropAllHashData(PSN, PSM: PFilesLayoutInformation);
procedure DropAllFitsData;
procedure DropFitData(idx: Integer; var ares: TBooleanDynArray);

function BinSchIdx(ind: Integer; var i: Integer; ie: Integer = -2): Boolean;

function SearchBestFits(PEx: PExtent;
                        PFL: PFilesLayoutInformation;
               VcnBg, ClsSz: Cardinal;
                       bNew: Boolean;
             var fabi, faei: Integer;
                 var ResTyp: Byte;
   var FndFtsSiz, FndFlsSiz: UINT64): Boolean;

function CheckFoundBestFits(PEx: PExtent;
                   VcnBg, ClsSz: Cardinal;
                 var fabi, faei: Integer) : Boolean;

function SearchDropChangedBgd(var irp: TIntegerDynArray;
                 var CntIndx, cMchDrp: Cardinal;
             var FndFtsSiz, FndFlsSiz: UINT64;
                               VolHnd: THandle;
                                  Typ: Byte;
                                  PFL: PFilesLayoutInformation) : Cardinal;

var
 FitBeg, FitSiz, FitCnt: TCardinalDynArray;
 FitInd: TIntegerDynArray;
 IdxIdx: TIntegerDynArray;
 IdxPEx: TIntegerDynArray;
 PExDon: Boolean = true;
 FitPEx: array of PExtent;
 AvaVcn: TLargeIntegerDynArray;
 IAvPEx: TIntegerDynArray;
 AvaPEx: array of TLargeIntegerDynArray;
 PExIdx: TIntegerDynArray;
 SchTim: TDateTime = 0;

implementation
uses
 SysUtils, cloper_base;

function GetHashCode(b : TByteDynArray; bi, bs : Cardinal) : UINT64;
var
 i, r : Integer;
begin
 Result := 0; r := SizeOf(UINT64) * 8 - 2;
 for i := bi to bi + bs - 1 do
  Result := Result shl 2 or Result shr r xor b[i];
end;

function BinSchSHC(Cod: UINT64; SSH: THashCodes; var idx : Integer) : Boolean;
var
 i, j: Integer;
begin
 Result := false;
 j := Length(SSH.Ind) - 1; i := j; idx := i;
 while i > 1 do begin
  if idx < 0 then idx := idx + i else if j < idx then idx := idx - i;
  i := (i + i mod 2) div 2;
  if Cod < SSH.Cod[SSH.Ind[idx]] then
   idx := idx - i
  else
   if SSH.Cod[SSH.Ind[idx]] < Cod then
    idx := idx + i
   else break
 end;
 i := 1;
 repeat
  if (0 <= idx + i) and (idx + i <= j) then
   if SSH.Cod[SSH.Ind[idx + i]] = Cod then
    Result := true
   else
    if Result then break;
  Dec(i)
 until (idx + i < 0) or (i < -1) and not Result;
 if Result then idx := idx + i + 1 else idx := -1
end;

function GetTargetHashCodes(PEx: PExtent): Boolean;
var
 hTgVol: THandle; BL, EL: UINT64; // 1st & last LCNs of block
 Buf: TByteDynArray; bps, ss, SzBuf: Cardinal; i: UInt64; j: Int32;
begin
 Result := false; if 0 < Length(PEx.Chc) then exit;
 BL := PEx.Lcn.QuadPart;
 EL := BL + PEx.Vcs - 1;

 hTgVol := CreateFile(PWChar(ExcludeTrailingBackslash(TgtVolSpec.GUID)),
    GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
    OPEN_EXISTING, FILE_FLAG_NO_BUFFERING or FILE_FLAG_SEQUENTIAL_SCAN, 0);
 if hTgVol = INVALID_HANDLE_VALUE then exit;

 BL := ClusterInCodSizes * BL; i := 0; bps := SrcVolSpec.BytesPerSector;
 EL := ClusterInCodSizes * EL + ClusterInCodSizes - 1;
 repeat
  if BL + ReadStep div CodeSizeInSectors <= EL then ss := ReadStep else
   ss := CodeSizeInSectors * (EL - BL + 1);
  SzBuf := ReadSectors(hTgVol, Buf, CodeSizeInSectors * BL, ss, bps);
  i := 0;
  repeat
   with PEx^ do begin
    j := Length(Chc); SetLength(Chc, j + 1);
    Chc[j] := GetHashCode(Buf, i, CodeSizeInSectors * bps)
   end;
   i := i + CodeSizeInSectors * bps
  until SzBuf <= i;
  BL := BL + ss div CodeSizeInSectors
 until EL < BL;
 CloseHandle(hTgVol); Result := true
end;

procedure DHCper(i, j : Integer; var P : Pointer);
var
 F : PFilesLayoutInformation; L : LARGE_INTEGER;
begin
 F := PFilesLayoutInformation(P);
 if F.bHCini then begin
  L := F.HCidx[i]; F.HCidx[i] := F.HCidx[j]; F.HCidx[j] := L
 end else begin
  L := F.HCbuf[i]; F.HCbuf[i] := F.HCbuf[j]; F.HCbuf[j] := L
 end;
end;

function DHCcmp(i, j : Integer; P : Pointer) : Boolean;
var
 F: PFilesLayoutInformation; ii, ij: PLARGE_INTEGER; di, dj: PFileLayoutInformation;
begin
 F := PFilesLayoutInformation(P);
 if F.bHCini then begin
  ii := @F.HCidx[i]; ij := @F.HCidx[j]
 end else begin
  ii := @F.HCbuf[i]; ij := @F.HCbuf[j]
 end;
 di := @F.Data[ii.LowPart]; dj := @F.Data[ij.LowPart];

 if Length(di.HC) <= ii.HighPart then Result := false else
  if Length(dj.HC) <= ij.HighPart then Result := true else
   if di.HC[ii.HighPart] = dj.HC[ij.HighPart] then
    Result := ii.QuadPart <= ij.QuadPart
   else
    Result := di.HC[ii.HighPart] < dj.HC[ij.HighPart]
end;

function DHCbuf(F: PFilesLayoutInformation; i, b: Integer) : Byte;
var
 ii, ib: PLARGE_INTEGER; di, db: PFileLayoutInformation;
begin
 ii := @F.HCidx[i]; di := @F.Data[ii.LowPart];
 ib := @F.HCbuf[b]; db := @F.Data[ib.LowPart];
 if Length(di.HC) <= ii.HighPart then Result := 0 else
  if Length(db.HC) <= ib.HighPart then Result := 1 else
   if di.HC[ii.HighPart] = db.HC[ib.HighPart] then
    if ii.QuadPart = ib.QuadPart then Result := 1 else
     if ii.QuadPart < ib.QuadPart then Result := 2 else Result := 3
   else
    if di.HC[ii.HighPart] < db.HC[ib.HighPart] then Result := 2 else
     Result := 3
end;

function DHCidx(PFL: PFilesLayoutInformation; ind: Integer): UINT64;
begin
 if Length(PFL.Data[PFL.HCidx[ind].LowPart].HC) = 0 then
  Result := HighUInt64
 else
  Result := PFL.Data[PFL.HCidx[ind].LowPart].HC[PFL.HCidx[ind].HighPart]
end;

function BinSchDHC(DHC: UINT64 ; PFL: PFilesLayoutInformation;
               var idx: Integer; dcb: Integer = -1;
                                 dce: Integer = -1;
                                 ind: Integer = -1;
                                 idb: Integer = -1) : Boolean;
 function Cmp(idx : Integer; var ico, ifo, ifi : Integer) : Byte;
 var
  CurDHC: UINT64;
 begin
  CurDHC := DHCidx(PFL, idx);
  if CurDHC < DHC then Result := 1 else
   if DHC < CurDHC then Result := 2 else begin
    if (-1 < dcb) and (dcb <= dce) then begin
     if PFL.HCidx[idx].HighPart < dcb then Result := 1 else
      if dce < PFL.HCidx[idx].HighPart then Result := 2 else
       Result := 0;
     if (-1 < ico) and (-1 < dcb) and ((ico < dcb) or (dce < ico)) then
      ico := dce
    end else Result := 0;
    if (Result = 0) and (-1 < ico) then // Search minimum position index:
     if PFL.HCidx[idx].HighPart < ico then begin
      Result := 3; ico := PFL.HCidx[idx].HighPart
     end else if ico < PFL.HCidx[idx].HighPart then
      Result := 2
   end;
  if Result = 0 then begin
   if -1 < ind then begin
    if PFL.HCidx[idx].LowPart < ind then Result := 1 else
     if ind < PFL.HCidx[idx].LowPart then Result := 2;
    if (-1 < ifo) and (-1 < ind) and (ifo <> ind) then
     ifo := ind
   end;
   if (Result = 0) and (-1 < ifo) then // Search minimum file index:
    if (PFL.HCidx[idx].LowPart < ifo) or (idx = ifi) then begin
     Result := 3; ifi := idx; ifo := PFL.HCidx[idx].LowPart
    end else if ifo < PFL.HCidx[idx].LowPart then
     Result := 2
  end
 end;
var
 i, j, k, m, n: Integer; iTstCtr : Integer;
begin
 Result := false;   iTstCtr := 0;
 j := Length(PFL.HCidx) - 1;
 if idb < 0 then begin i := j; idx := i end else begin i := j div 4; idx := idb end;
 k := j; m := j; n := 0;
 while i > 1 do begin
  if idx < 0 then idx := idx + i else if j < idx then idx := idx - i;
  i := (i + i mod 2) div 2;
  case Cmp(idx, k, m, n) of
  0: break; 1: idx := idx + i; 2: idx := idx - i;
  3: begin
   idx := idx - 3 * (i + i mod 2) div 4; if 1 < i then i := 2 * i
  end; end
 end;

 i := 1; k := -1; m := -1;
 repeat
  if (0 <= idx + i) and (idx + i <= j) then
   case Cmp(idx + i, k, m, n) of 0: Result := true; 1: break; end;
  Dec(i)
 until (idx + i < 0) or (i < -1) and not Result;
 idx := idx + i + 1
end;

procedure SortHashIndex(PFL: PFilesLayoutInformation);
var
 i, j, k: Integer; b: Boolean;
begin
 if PFL.bHCini then i := Length(PFL.HCidx) else i := Length(PFL.HCbuf);
 Dec(i);
 if i < 0 then exit else
  QuickSort(PFL, 0, i, @DHCcmp, @DHCper);

 if PFL.bHCini then PFL.bHCini := false else begin
  b := false; j := Length(PFL.HCidx) + i; SetLength(PFL.HCidx, j + 1);
  repeat
   if j = i then
    for i := i downto 0 do PFL.HCidx[i] := PFL.HCbuf[i]
   else begin
    k := j - i - 1;
    case DHCbuf(PFL, k, i) of
    0: begin
     PFL.HCidx[j].QuadPart := 0; b := true
    end;
    1: begin
     PFL.HCidx[j].QuadPart := 0; b := true; Dec(i)
    end;
    2: begin
     SetLargeInteger(PFL.HCidx[j], PFL.HCbuf[i].HighPart, PFL.HCbuf[i].LowPart);
     Dec(i)
    end;
    3: SetLargeInteger(PFL.HCidx[j], PFL.HCidx[k].HighPart, PFL.HCidx[k].LowPart);
    end;
    Dec(j)
   end
  until i < 0;
  SetLength(PFL.HCbuf, 0);
  if b then begin
   i := Length(PFL.HCidx) - 1; j := 0;
   for k := 0 to i do
    if PFL.HCidx[k].QuadPart = 0 then Inc(j) else if 0 < j then
     PFL.HCidx[k - j] := PFL.HCidx[j];
   if 0 < j then
    SetLength(PFL.HCidx, i - j + 1)
  end
 end
end;

function DropFileHashCodesIndexes(PFL: PFilesLayoutInformation;
                                  idx: Integer): Boolean;
var
 i, j, k, m: Integer; H: UINT64;
begin
 if(idx < 0) or (Length(PFL.Data) <= idx) then
  raise Exception.Create('Obtained incorrect index of data item in DropFileHashIndexes');
 Result := Length(PFL.Data[idx].HC) = 0;
 if PFL.bHCini then
  raise Exception.Create('DropFileHashIndexes called before end of indexation.')
 else begin
  j := 0; H := PFL.Data[idx].HC[j];
  for i := 0 to Length(PFL.Data[idx].HC) - 1 do
   if PFL.Data[idx].HC[i] < H then begin j := i; H := PFL.Data[idx].HC[j] end;

  if BinSchDHC(H, PFL, i, j, j, idx) then begin
   j := 0; k := Length(PFL.HCidx) - 1;
   for m := i to k do
    if PFL.HCidx[m].LowPart = idx then
     Inc(j)
    else if 0 < j then
     SetLargeInteger(PFL.HCidx[m - j], PFL.HCidx[m].HighPart, PFL.HCidx[m].LowPart);
   Result := 0 < j;
   if Result then
    SetLength(PFL.HCidx, k + 1 - j)
  end else
   raise Exception.Create('Couldn''t find 1st index in DropFileHashIndexes')
 end;
 SetLength(PFL.Data[idx].HC, 0)
end;
// ****************************************************************************

function GetFileHashCodesV(VolHnd: THandle; var HC: TUInt64DynArray; pb, pe: UInt64): Boolean; overload;
var
 Buf: TByteDynArray; SzBuf: Cardinal; bps, st: UInt32; i, j: Int32;
begin
 Result := 0 < Length(HC); if Result then exit;
 if VolHnd = INVALID_HANDLE_VALUE then exit;

 bps := SrcVolSpec.BytesPerSector;
 pb := ClusterInCodSizes * pb; pe := ClusterInCodSizes * (pe + 1) - 1;
 repeat
  if pb + ReadStep div CodeSizeInSectors < pe then st := ReadStep else
   st := CodeSizeInSectors * (pe - pb + 1);
  SzBuf := ReadSectors(VolHnd, Buf, CodeSizeInSectors * pb, st, bps);
  i := 0;
  repeat
   j := Length(HC); SetLength(HC, j + 1);
   HC[j] := GetHashCode(Buf, i, CodeSizeInSectors * bps);
   i := i + CodeSizeInSectors * bps
  until Length(Buf) <= i;
  pb := pb + st div CodeSizeInSectors
 until pe <= pb;
 Result := true
end;
function GetFileHashCodesV(VolHnd: THandle; PFL: PFileLayoutInformation): Boolean; overload;
begin
 Assert(Length(PFL.Extents) = 1, 'Can not obtain hash codes of fragmented file.');
 Result := GetFileHashCodesV(VolHnd, PFL.HC,
                                     LBE(@PFL.Extents[0]), LEE(@PFL.Extents[0]))
end;

procedure GetFileDHC_Indexes(PFL: PFilesLayoutInformation;
                             idx: Integer; bSort: Boolean = true);
var
 i, j, k: Integer; p: PLargeIntegerDynArray;
begin
 if Length(PFL.Data[idx].HC) = 0 then
  exit;
 if PFL.bHCini then p := @(PFL.HCidx) else p := @(PFL.HCbuf);

 i := Length(p^); SetLength(p^, i + Length(PFL.Data[idx].HC));
 j := Length(p^) - 1;
 for k := i to j do SetLargeInteger(p^[k], k - i, idx);
 if bSort then
  SortHashIndex(PFL)
end;

function HashCodesIndex(var CntIndx: Cardinal; VolHnd: THandle;
                                PFL: PFilesLayoutInformation; ix: Integer;
                                Typ: THshCodAction): Boolean;
begin
 if Typ in [hcDrop, hcAdd, hcAddIndex] then begin
  if 0 < Length(PFL.Data[ix].HC) then begin
   if Typ = hcDrop then begin
    Result := DropFileHashCodesIndexes(PFL, ix);

    if Result and (0 < CntIndx) then Dec(CntIndx)
   end;
  end
 end;
 if Typ in [hcAdd, hcAddIndex] then begin
  if Length(PFL.Data[ix].Extents) = 1 then begin
   Result := GetFileHashCodesV(VolHnd, @PFL.Data[ix]);
   if Result then begin
    GetFileDHC_Indexes(PFL, ix, Typ = hcAddIndex); Inc(CntIndx)
   end
  end else
   Result := false
 end;
 if Typ = hcIndex then begin
  SortHashIndex(PFL);
  Result := true
 end;
end;

procedure DropAllHashData(PSN, PSM: PFilesLayoutInformation);
 procedure DropTargetHD(PT: PFilesLayoutInformation);
 var
  i, j: Integer;
 begin
  for i := 0 to Length(PT.Data) - 1 do
   for j := 0 to Length(PT.Data[i].Extents) - 1 do
    SetLength(PT.Data[i].Extents[j].Chc, 0)
 end;
 procedure DropSourceHD(PS: PFilesLayoutInformation);
 var
  i, j: Integer;
 begin
  for i := 0 to Length(PS.Data) - 1 do SetLength(PS.Data[i].HC, 0);
  SetLength(PS.HCidx, 0);
 end;
var i: Integer; begin
 DropSourceHD(PSN); DropSourceHD(PSM);

 SetLength(IAvPEx, 0); SetLength(AvaPEx, 0)
end;

procedure DropAllFitsData;
begin
 SetLength(FitInd, 0); SetLength(FitCnt, 0);
 SetLength(FitSiz, 0); SetLength(FitBeg, 0);
 SetLength(IdxIdx, 0);
 SetLength(FitPEx, 0); SetLength(IdxPEx, 0)
end;

procedure DropFitData(idx: Integer; var ares: TBooleanDynArray);
var
 i, j: Integer; b: Boolean;
begin
 i := Length(FitInd) - 1; b := false;
 for j := 0 to i do if idx = IdxIdx[j] then b := true else begin
  if idx < IdxIdx[j] then Dec(IdxIdx[j]);
  if b then IdxIdx[j - 1] := IdxIdx[j]
 end;
 b := false;
 for j := idx + 1 to i do
 begin
  FitInd[j - 1] := FitInd[j]; FitBeg[j - 1] := FitBeg[j];
  FitCnt[j - 1] := FitCnt[j]; FitSiz[j - 1] := FitSiz[j];
  IdxPEx[j - 1] := IdxPEx[j];
  if 0 < Length(ares) then ares[j - 1] := ares[j]
 end;
 SetLength(FitInd, i); SetLength(FitCnt, i); SetLength(FitSiz, i);
 SetLength(FitBeg, i); SetLength(IdxPEx, i); SetLength(IdxIdx, i);
 if 0 < Length(ares) then SetLength(ares, i)
end;


function BinSchIdx(ind: Integer; var i: Integer; ie: Integer = -2): Boolean;
 function V(i : Integer): Integer; begin Result := FitInd[IdxIdx[i]] end;
var
 j, k: Integer;
begin
 Result := false;
 if ie < -1 then k := Length(IdxIdx) - 1 else if 0 <= ie then k := ie else
 begin
  i := 0; exit
 end;
 j := k; i := j;
 while j > 1 do begin
  if i < 0 then i := i + j else if k < i then i := i - j;
  j := (j + j mod 2) div 2;
  if ind < V(i) then i := i - j else if V(i) < ind then i := i + j else break
 end;
 j := 1;
 repeat
  if (0 <= i + j) and (i + j <= k) then
   if V(i + j) = ind then Result := true else if V(i + j) < ind then break;
  Dec(j)
 until (i + j < 0) or (j < -1);
 i := i + j + 1
end;

function BinSchPExIdx(ind: Integer; var i: Integer): Boolean;
var
 j, k: Integer;
begin
 Result := false; k := Length(PExIdx) - 1; j := k; i := j;
 while j > 1 do begin
  if i < 0 then i := i + j else if k < i then i := i - j;
  j := (j + j mod 2) div 2;
  if ind < PExIdx[i] then i := i - j else
   if PExIdx[i] < ind then i := i + j else break
 end;
 j := 1;
 repeat
  if (0 <= i + j) and (i + j <= k) then
   if PExIdx[i + j] = ind then Result := true else
    if PExIdx[i + j] < ind then break;
  Dec(j)
 until (i + j < 0) or (j < -1);
 i := i + j + 1
end;

function SearchDropChangedBgd(var irp: TIntegerDynArray;
                 var CntIndx, cMchDrp: Cardinal;
             var FndFtsSiz, FndFlsSiz: UINT64;
                               VolHnd: THandle;
                                  Typ: Byte;
                                  PFL: PFilesLayoutInformation) : Cardinal;
var
 i, j, k, m, n, a: Integer; b : Boolean; CH: TUInt64DynArray;
 idx: TIntegerDynArray; ares: TBooleanDynArray;
 z: Integer;
begin
 Result := 0; SetLength(irp, 0); SetLength(idx, 0); SetLength(ares, 0);
 try
  for i := 0 to Length(PFL.SizIdx) - 1 do
   if 0 < Length(PFL.Data[PFL.SizIdx[i]].HC) then begin
    j := PFL.SizIdx[i];
    with PFL.Data[j] do if State and 64 = 64 then b := true else begin
     Assert(    (Length(Extents) = 1)
            and (Extents[0].Vcs = Length(HC) div ClusterInCodSizes),
            'Layout data and hash codes doesn''t match');
     SetLength(CH, 0);
     GetFileHashCodesV(VolHnd, CH, LBE(@Extents[0]), LEE(@Extents[0]));
     k := 0; m := Length(CH); n := m div 100; Dec(m);
     for a := 0 to m do begin // 1 % tolerance threshold
      if HC[a] <> CH[a] then Inc(k); if n < k then break
     end;
     b := n < k;
     if b then begin
      a := Length(irp); SetLength(irp, a + 1); irp[a] := j; State := State or 64
     end
    end;
    if b then begin
     if Typ = 1 then k := - 1 - j else k := j;
     if BinSchIdx(k, m) then begin
      m := IdxIdx[m];
      Inc(cMchDrp); Dec(FndFtsSiz, FitCnt[m]); Dec(FndFlsSiz, FitSiz[m]);
      if not BinSchPExIdx(IdxPEx[m], n) then begin
       a := Length(PExIdx); SetLength(PExIdx, a + 1);
       for z := a - 1 downto n do PExIdx[z + 1] := PExIdx[z];
       PExIdx[n] := IdxPEx[m]
      end;
      DropFitData(m, ares)
     end;
     k := Length(idx); SetLength(idx, k + 1); idx[k] := j;
     SetLength(PFL.Data[j].HC, 0)
    end
   end;
  Result := Length(idx);
  if Result = 0 then exit else QuickSort(@idx, 0, Result - 1);
  i := 0;
  for j := 0 to Length(PFL.HcIdx) - 1 do
   if BinSchInt(idx, PFL.HcIdx[j].LowPart, k) then
    Inc(i)
   else if 0 < i then PFL.HcIdx[j - i] := PFL.HcIdx[j];
  SetLength(PFL.HcIdx, Length(PFL.HcIdx) - i)
 except
  on E:Exception do
   raise Exception.Create('SearchDropBgdChanges: ' + E.Message)
 end
end;

function SearchBestFitsImp(PEx: PExtent;
                           PFL: PFilesLayoutInformation;
                        Bg, Sz: Cardinal;
                          bNew: Boolean;
                var fabi, faei: Integer;
                    var ResTyp: Byte;
      var FndFtsSiz, FndFlsSiz: UINT64;
                     OnlyCheck: Boolean) : Boolean;

 function CoinFrag(fbi, fsz: Cardinal; i: Integer; bPr: Boolean) : Boolean;
 begin
  if PEx = FitPEx[IdxPEx[i]] then
   if bPr then
    Result := (fbi = FitBeg[i]) and (FitBeg[i] + FitSiz[i] = fbi + fsz)
   else
    Result := (FitBeg[i] <= fbi) and (fbi < FitBeg[i] + FitSiz[i])
           or (fbi < FitBeg[i]) and (FitBeg[i] + FitSiz[i] < fbi + fsz)
           or (FitBeg[i] < fbi + fsz) and (fbi + fsz <= FitBeg[i] + FitSiz[i])
  else
   Result := false
 end;
 function BinSchFits(fbi, fsz: Cardinal; var i: Integer; bPr: Boolean) : Boolean;
 var
  j, k, m: Integer;
 begin
  if (fabi < 0) or (faei < 0) then begin
   j := 0; k := Length(FitInd) - 1
  end else begin
   j := fabi; k := faei
  end;
  Result := false; m := k - j; i := k;
  if j < k then
   repeat
    if i < j then i := i + m else if k < i then i := i - m;
    m := (m + m mod 2) div 2;
    if PEx = FitPEx[IdxPEx[i]] then
     if FitBeg[i] + FitSiz[i] <= fbi then
      i := i + m
     else
      if fbi + fsz <= FitBeg[i] then
       i := i - m
     else break
    else
     if FitPEx[IdxPEx[i]].Lcn.QuadPart < PEx.Lcn.QuadPart then i := i + m
     else
      if PEx.Lcn.QuadPart < FitPEx[IdxPEx[i]].Lcn.QuadPart then i := i - m
   until m < 2
  else m := 1;
  repeat
   if (j <= i + m) and (i + m <= k) then begin
    if CoinFrag(fbi, fsz, i + m, bPr) then
     Result := true
    else if PEx = FitPEx[IdxPEx[i + m]] then begin
     if FitBeg[i + m] + FitSiz[i + m] <= fbi then break
    end else
     if FitPEx[IdxPEx[i + m]].Lcn.QuadPart < PEx.Lcn.QuadPart then break
   end else if i + m < j then break;
   Dec(m)
  until (m < -1) and not Result;
  i := i + m + 1
 end;
 function ChckFits(CCN, DCN, CCO, DCO : Cardinal; DoNtfsC: Boolean) : Boolean;
 begin
  // Maximum number of fits, minimum number of differences:
  Result := (CCO <= CCN) and (DCN < DCO) or (CCO < CCN) and (DCN <= DCO);
  if Result then
   if bCompressData and DoNtfsC then
    // <==> number of differences is below average compression ratio.
    Result := 100 * DCN < ModificationThreshold * (CCN + DCN)
 end;
 function ChkFit(bif, szf, ctf: Cardinal; DoNtfsC: Boolean): Boolean;
 var
  i, j, k, m: Integer;
 begin
  if BinSchFits(bif, szf, i, false) then begin
   j := i; k := 0; m := 0;
   while (j < Length(FitInd)) and CoinFrag(bif, szf, j, false) do begin
    k := k + FitCnt[j]; m := m + FitSiz[j] - FitCnt[j];
    if i < j then
     m := m + FitBeg[j] - FitBeg[j - 1] - FitSiz[j - 1];
    Inc(j)
   end;
   if (k = 0) and (m = 0) then
    Result := false // not happens, checked earlier.
   else
    Result := ChckFits(ctf, szf - ctf, k, m, DoNtfsC)
  end else
   Result := true
 end;
 function PrevUse(ctf, inf: Integer; var i: Integer; var bnw: Boolean): Boolean;
 var
  j: Integer;
 begin
  Result := true; if bNew then inf := - 1 - inf;
  if BinSchIdx(inf, j) then begin
   bnw := true;
   if FitCnt[IdxIdx[j]] < ctf then i := IdxIdx[j] else Result := false
  end else
   bnw := false
 end;
 function ChckNew(inf: Integer; bif, szf: Cardinal): Boolean;
 var
  i, j: Integer;
 begin
  if Length(IdxIdx) = 0 then
   Result := true
  else begin
   if bNew then inf := - 1 - inf;

   if Length(IdxIdx) = 1 then
    Result := (FitInd[IdxIdx[0]] <> inf) or not CoinFrag(bif, szf, 0, true)
   else
    if BinSchFits(bif, szf, i, true) and BinSchIdx(inf, j) then
     Result := i <> IdxIdx[j]
    else
     Result := true
  end
 end;
 function Resize(Size: Integer; bCngIdx: Boolean = true): Integer;
 begin
  Result := Length(FitInd);
  if bCngIdx and ((-1 < fabi) or (-1 < faei)) then
   faei := faei + Size - Result;
  if faei < fabi then begin fabi := -1; faei := -1 end;

  SetLength(IdxIdx, Size); SetLength(IdxPEx, Size);
  SetLength(FitInd, Size); SetLength(FitCnt, Size);
  SetLength(FitSiz, Size); SetLength(FitBeg, Size); Result := Size
 end;
 function GetPExIdx(bOnlSch : Boolean = false): Integer;
 var
  i, j: Integer; b : Boolean;
 begin
  i := Length(FitPEx) - 1; b := true;
  if (i = 0) and (PEx = FitPEx[0]) then begin
   b := false; Result := 0
  end else begin
   j := i; Result := j;
   while j > 1 do begin
    if Result < 0 then Result := Result + j else if i < Result then
     Result := Result - j;
    j := (j + j mod 2) div 2;
    if PEx.Lcn.QuadPart < FitPEx[Result].Lcn.QuadPart then
     Result := Result - j
    else if FitPEx[Result].Lcn.QuadPart < PEx.Lcn.QuadPart then
     Result := Result + j
    else begin b := false; break end
   end;
   if b then begin
    j := 1;
    repeat
     if (0 <= Result + j) and (Result + j <= i) then
      if FitPEx[Result + j].Lcn.QuadPart = PEx.Lcn.QuadPart then
       b := false
      else if FitPEx[Result + j].Lcn.QuadPart < PEx.Lcn.QuadPart then
       break;
     Dec(j)
    until (Result + j < 0) or (j < -1);
    Result := Result + j + 1
   end
  end;
  if b then if bOnlSch then Result := -1 else begin
   i := i + 2; SetLength(FitPEx, i); SetLength(AvaPEx, i); SetLength(IAvPEx, i);
   for j := i - 1 downto Result + 1 do begin
    FitPEx[j] := FitPEx[j - 1]; IAvPEx[j] := IAvPEx[j - 1]
   end;
   Dec(i); SetLength(AvaPEx[i], Length(AvaVcn));
   for j := 0 to Length(AvaVcn) - 1 do
    with AvaVcn[j] do SetLargeInteger(AvaPEx[i][j], HighPart, LowPart);
   FitPEx[Result] := PEx; IAvPEx[Result] := i;
   for j := 0 to Length(IdxPEx) - 1 do if Result <= IdxPEx[j] then Inc(IdxPEx[j]);
   for j := 0 to Length(PExIdx) - 1 do if Result <= PExIdx[j] then Inc(PExIdx[j]);
   if not PExDon then begin
    BinSchPExIdx(Result, i);
    SetLength(PExIdx, Length(PExIdx) + 1);
    for j := Length(PExIdx) - 1 downto i + 1 do PExIdx[j] := PExIdx[j - 1];
    PExIdx[i] := Result
   end;
  end
 end;

 procedure AddFit(inf: Integer; bif, szf, ctf: Cardinal;
                 ino: Integer; var cei: Integer);
 var
  i, j, k, m, n, a: Integer;
 begin
  if cei < 0 then cei := GetPExIdx; if bNew then inf := - 1 - inf;
  i := Length(FitInd);

  if -1 < ino then begin
   FndFlsSiz := FndFlsSiz - FitSiz[ino]; FndFtsSiz := FndFtsSiz - FitCnt[ino];
   if (cei <> IdxPEx[ino]) and not BinSchPExIdx(IdxPEx[ino], j) or
            PExDon and (cei = IdxPEx[ino]) and (FitBeg[ino] < Bg) and
            not BinSchPExIdx(cei, j) then
   begin
    k := Length(PExIdx); SetLength(PExIdx, k + 1);
    for m := k downto j + 1 do PExIdx[m] := PExIdx[m - 1];
    PExIdx[j] := IdxPEx[ino];
    if cei = IdxPEx[ino] then PExDon := false
   end;
   Dec(i);
   for j := ino + 1 to i do begin
    FitInd[j - 1] := FitInd[j]; FitCnt[j - 1] := FitCnt[j];
    FitSiz[j - 1] := FitSiz[j]; FitBeg[j - 1] := FitBeg[j];
    IdxPEx[j - 1] := IdxPEx[j]
   end;
   k := 0;
   for j := 0 to Length(IdxIdx) - 1 do
    if IdxIdx[j] = ino then Inc(k) else begin
     if ino < IdxIdx[j] then Dec(IdxIdx[j]);
     if 0 < k then IdxIdx[j - k] := IdxIdx[j]
    end;
   if ino < fabi then Dec(fabi); if ino <= faei then Dec(faei); Resize(i, false)
  end;

  if i = 0 then begin
   Resize(1); j := 0; k := 0; IdxIdx[0] := 0;
   FitInd[0] := inf; FitCnt[0] := ctf; FitBeg[0] := bif; FitSiz[0] := szf;
   IdxPEx[0] := cei
  end else begin
   if BinSchFits(bif, szf, k, false) then begin
    m := k;
    while (m < Length(FitInd)) and CoinFrag(bif, szf, m, false) do Inc(m);
    m := m - k - 1;
    n := 0;
    for a := 0 to Length(IdxIdx) - 1 do
     if (k <= IdxIdx[a]) and (IdxIdx[a] <= k + m) then begin
      FndFlsSiz := FndFlsSiz - FitSiz[IdxIdx[a]];
      FndFtsSiz := FndFtsSiz - FitCnt[IdxIdx[a]];
      Inc(n)
     end else begin
      if (0 < m) and (k + m < IdxIdx[a]) then
       IdxIdx[a] := IdxIdx[a] - m;
      if 0 < n then IdxIdx[a - n] := IdxIdx[a]
     end;
    if 0 < m then begin
     for n := k + m + 1 to i - 1 do begin
      FitInd[n - m] := FitInd[n]; FitCnt[n - m] := FitCnt[n];
      FitSiz[n - m] := FitSiz[n]; FitBeg[n - m] := FitBeg[n];
      IdxPEx[n - m] := IdxPEx[n]
     end;
     i := Resize(Length(FitInd) - m)
    end;
    BinSchIdx(inf, j, Length(IdxIdx) - 2); ResTyp := 2; m := i
   end else begin
    BinSchIdx(inf, j); m := Resize(i + 1);
    for n := Length(FitInd) - 1 downto k + 1 do begin
     FitInd[n] := FitInd[n - 1]; FitCnt[n] := FitCnt[n - 1];
     FitSiz[n] := FitSiz[n - 1]; FitBeg[n] := FitBeg[n - 1];
     IdxPEx[n] := IdxPEx[n - 1]
    end
   end;
   FitInd[k] := inf; FitCnt[k] := ctf; FitBeg[k] := bif; FitSiz[k] := szf;
   IdxPEx[k] := cei;

   if 1 < Length(IdxIdx) then
    for a := Length(IdxIdx) - 1 downto 0 do begin
     if j < a then
      IdxIdx[a] := IdxIdx[a - 1]
     else
      if i = m then break;
     if (i < m) and (a <> j) and (k <= IdxIdx[a]) then
      Inc(IdxIdx[a])
    end
   else if Length(IdxIdx) = 0 then
    SetLength(IdxIdx, 1);
   IdxIdx[j] := k
  end;
  if (fabi < 0) or (k < fabi) then fabi := k; FndFlsSiz := FndFlsSiz + FitSiz[k];
  if (faei < 0) or (faei < k) then faei := k; FndFtsSiz := FndFtsSiz + FitCnt[k]
 end;

 var Chkd : TLargeIntegerDynArray;

 function BinSchChkd(FS, IFF: Integer): Boolean;
 var
  F: LARGE_INTEGER; i, j, k: Integer;
 begin // returns false result for internal use
  SetLargeInteger(F, FS, IFF); i := Length(Chkd) - 1; j := i; k := i;
  if i = 0 then Result := Chkd[0].QuadPart <> F.QuadPart else Result := true;
  while 1 < j do begin
   if k < 0 then k := k + j else if i < k then k := k - j;
   j := (j + j mod 2) div 2;
   if F.QuadPart < Chkd[k].QuadPart then k := k - j else
    if Chkd[k].QuadPart < F.QuadPart then k := k + j else break
  end;
  if(1 < i)and(0 <= k)and(k <= i)then Result := Chkd[k].QuadPart <> F.QuadPart
 end;
 procedure UpdChkd(FS: Integer; AIF: TIntegerDynArray);
 const
  SizeLimit = 8388608; // 64 MB
 var
  F: LARGE_INTEGER; i, j: Integer;
 begin
  i := 0;
  for j := 0 to Length(Chkd) - 1 do
   if Chkd[j].HighPart + PFL.Data[Chkd[j].LowPart].ClusCount < FS then
    Inc(i)
   else if 0 < i then
    Chkd[j - i] := Chkd[j];
  if SizeLimit < Length(Chkd) then begin
   if 0 < i then SetLength(Chkd, Length(Chkd) - i); exit
  end;
  j := Length( AIF) - 1; SetLength(Chkd, Length(Chkd) + j + 1 - i);
  i := Length(Chkd) - 1; SetLargeInteger(F, FS, AIF[j]);
  repeat
   if (j < i) and (F.QuadPart < Chkd[i - j - 1].QuadPart) then
    Chkd[i] := Chkd[i - j - 1]
   else begin
    Chkd[i] := F;
    if 0 < j then begin Dec(j); SetLargeInteger(F, FS, AIF[j]) end else break
   end;
   Dec(i)
  until false
 end;

 function CkFs(bpi, fpi, ind, Ed: Integer;
                  var RS, RI, OI: Integer;
                 var MS, RSz, RC: Cardinal;
                          var MV: UInt64;
                          bPlain: Boolean = false): Cardinal;
 var
  FS, i, j: Int32; FSz, FC: UInt32; bnw: Boolean; AIF: TIntegerDynArray;
 begin
  Result := 0; FS := bpi - fpi;
  if PEx.Chc[bpi] = MV then Inc(MS) else begin MS := 0; MV := PEx.Chc[bpi] end;
  repeat
   i := PFL.HCidx[ind].LowPart;
   with PFL.Data[i] do if ((State and 2) = 2) and (MS < ClusCount) then begin
    FSz := Length(HC) - 1;
    if (0 <= FSz) and (FS + FSz <= Ed) and ChckNew(i, FS, FSz + 1)
                                       and(bPlain or BinSchChkd(FS, i))then
    begin
     FC := 0; for j := 0 to FSz do if HC[j] = PEx.Chc[FS + j] then Inc(FC);
     Inc(FSz);
     if (RC < FC) and ChkFit(FS, FSz, FC, ((State and 32) = 32))
                  and PrevUse(FC, i, OI, bnw) then begin
      RI := i; RS := FS; RSz := FSz; RC := FC
     end else if bnw then
      ResTyp := 2;
     if not bPlain then begin
      j := Length(AIF); SetLength(AIF, j + 1); AIF[j] := i
     end
    end
   end;
   Inc(Result); Inc(ind)
  until (Length(PFL.HCidx) <= ind) or (fpi <> PFL.HCidx[ind].HighPart) or
        (PEx.Chc[bpi] <> DHCidx(PFL, ind));

  if 0 < Length(AIF) then UpdChkd(FS, AIF)
 end;
 function Scale(Cnt: Cardinal; dif: Integer; var f: Cardinal): Cardinal;
 const
  Limit = 1; // Limit for the number of comparisons per block.
 var
  a, b: Cardinal;
 begin
  a := f * Limit; b := 2 * dif * Cnt;
  if 3 * a < b then f := f * 2 else // <- Rarification of checks by scaling step
   if (1 < f) and ((b < a) or (dif < ClusterInCodSizes * f)) then
    f := f div 2;
  Result := f
 end;
var
 i, j, k, m, n, a, z, ei, Ed, RS, RI, OI: Int32; c, f, RSz, RC, MS: UInt32;
 MV: UInt64; t: TDateTime; b: Boolean;
begin
 Result := not OnlyCheck; if (Length(PEx.Chc) < Bg + Sz) or (Sz = 0) then exit;
 if OnlyCheck then
  if (fabi < 0) and (faei < 0) and BinSchFits(Bg, Sz, i, false) then begin
   fabi := i; faei := i; i := Length(IdxPEx) - 1;
   while (0 < fabi) and (IdxPEx[fabi - 1] = IdxPEx[faei]) do Dec(fabi);
   while (faei < i) and (IdxPEx[fabi] = IdxPEx[faei + 1]) do Inc(faei);
   RSz := 0; for i := fabi to faei do RSz := RSz + FitSiz[i];
   Result := RSz < Length(PEx.Chc);
   exit
  end else exit;

 t := Now; ei := -1; MS := 0; MV := HighUInt64; b := Sz < 4 * ClusterInCodSizes;
 Ed := Bg + Sz - 1;
 for i := Bg to Ed do begin
  j := i - Bg; k := j mod ClusterInCodSizes;
  RC := 0; OI := -1; m := k; c := 0; f := 1;
  if b then begin
   repeat
    if BinSchDHC(PEx.Chc[i], PFL, m, k, k) then
     CkFs(i, PFL.HCidx[m].HighPart, m, Ed, RS, RI, OI, MS, RSz, RC, MV, true);
    k := k + ClusterInCodSizes
   until j < k
  end else
   while (m <= j) and BinSchDHC(PEx.Chc[i], PFL, n, m, j) do begin
    z := k - PFL.HCidx[n].HighPart mod ClusterInCodSizes;
    if z = 0 then begin
     c := c + CkFs(i, PFL.HCidx[n].HighPart, n, Ed, RS, RI, OI, MS, RSz, RC, MV);
     m := m + Scale(c, j - m, f) * ClusterInCodSizes
    end else begin
     if z < 0 then z := z + ClusterInCodSizes;
     z := PFL.HCidx[n].HighPart + z;
     if BinSchDHC(PEx.Chc[i], PFL, a, z, z, -1, n) then
      c := c + CkFs(i, z, a, Ed, RS, RI, OI, MS, RSz, RC, MV);
     m := z + Scale(c, j - m, f) * ClusterInCodSizes
    end
   end;
  if 0 < RC then begin
   Result := false; MS := 0; AddFit(RI, RS, RSz, RC, OI, ei)
  end;
 end;
 if (-1 < fabi) and (-1 < faei) then begin
  RC := 0;
  for i := fabi to faei do RC := RC + FitCnt[i]; if RC = Sz then ResTyp := 1
 end;
 if PExDon then begin
  if (-1 < fabi) and (-1 < faei) then i := IdxPEx[fabi] else i := GetPExIdx(true);
  if (-1 < i) and BinSchPExIdx(i, i) then begin
   j := Length(PExIdx) - 2;
   for k := i to j do PExIdx[k] := PExIdx[k + 1];
   SetLength(PExIdx, j + 1)
  end;
 end;
 SchTim := SchTim + Now - t
end;

function SearchBestFits(PEx: PExtent;
                        PFL: PFilesLayoutInformation;
               VcnBg, ClsSz: Cardinal;
                       bNew: Boolean;
             var fabi, faei: Integer;
                 var ResTyp: Byte;
   var FndFtsSiz, FndFlsSiz: UINT64) : Boolean;
begin
 VcnBg := VcnBg * ClusterInCodSizes;
 ClsSz := ClsSz * ClusterInCodSizes;

 Result := SearchBestFitsImp(PEx, PFL, VcnBg, ClsSz, bNew, fabi, faei,
            ResTyp, FndFtsSiz, FndFlsSiz, false)
end;

function CheckFoundBestFits(PEx: PExtent;
                   VcnBg, ClsSz: Cardinal;
                 var fabi, faei: Integer) : Boolean;
var
 bNew: Boolean;
 PFL: PFilesLayoutInformation;
 ResTyp: Byte;
 FndFtsSiz, FndFlsSiz: UINT64;
begin
 VcnBg := VcnBg * ClusterInCodSizes;
 ClsSz := ClsSz * ClusterInCodSizes;
 Result := SearchBestFitsImp(PEx, PFL, VcnBg, ClsSz, bNew, fabi, faei,
            ResTyp, FndFtsSiz, FndFlsSiz, true)
end;

end.
