{ cloper_oper
  The unit of Cloning Optimizer (Cloper) tool.

  The unit with functionality for optimization of data existing on both volumes:
  1. Routines of original data retrieving for optimization;
  2. Top level routines for search of source data matching target data;
  3. Routines for relocation of matched data;
  4. Routines for filling of source areas in front of target data blocks by
     source files.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit cloper_oper;

interface

uses
 Types, ConTools, cloper_echo;

{ OptimizePersistentData
  Performs comparison of old files allocation in the source and target volumes,
  moves source data in the front of target location if it gives optimization.
}
function OptimizePersistentData(bMatchBlocks, bMoveToBlocks: Boolean;
                                VolHdl: THandle; NumVolSrc, NumVolTgt: Integer;
      var DelFiles, OldFiles, NewFiles: TStringDynArray;
                          var bDoNTFSC: TBooleanDynArray;
                         var NewFlsSzs: TCardinalDynArray): Boolean;

var
 NoNtfsCext : String;
 MemoryUseLimit : Cardinal = 921600; // 900 MB

 implementation
uses
 SysUtils, StrUtils, Windows, cloper_typs, cloper_funs, cloper_base, Defragger,
 cloper_byte, cloper_hash, cloper_into, StrTools;

var
 DelLays, OsLays, OtLays, NewLays : TFilesLayoutInformation;

procedure CluLcnPer(i, j : Integer; var pFLI : Pointer);
var
 FLI : PFilesLayoutInformation; LI : LARGE_INTEGER;
begin
 FLI := PFilesLayoutInformation(pFLI);
 LI := FLI.LcnIdx[i]; FLI.LcnIdx[i] := FLI.LcnIdx[j]; FLI.LcnIdx[j] := LI
end;

function CluLcnCmp(i, j : Integer; pFLI : Pointer) : Boolean;
var
 FLI : PFilesLayoutInformation; ii, ij : PLARGE_INTEGER;
begin
 FLI := PFilesLayoutInformation(pFLI); ii := @FLI.LcnIdx[i]; ij := @FLI.LcnIdx[j];
 Result := FLI.Data[ii.HighPart].Extents[ii.LowPart].Lcn.QuadPart
         < FLI.Data[ij.HighPart].Extents[ij.LowPart].Lcn.QuadPart
end;

procedure CluSizPer(i, j : Integer; var pFLI : Pointer);
var
 FLI : PFilesLayoutInformation; SI : Cardinal;
begin
 FLI := PFilesLayoutInformation(pFLI);
 SI := FLI.SizIdx[i]; FLI.SizIdx[i] := FLI.SizIdx[j]; FLI.SizIdx[j] := SI
end;

function CluSizCmp(i, j : Integer; pFLI : Pointer) : Boolean;
var
 FLI : PFilesLayoutInformation;
begin
 FLI := PFilesLayoutInformation(pFLI);
 Result := FLI.Data[FLI.SizIdx[i]].ClusCount < FLI.Data[FLI.SizIdx[j]].ClusCount
end;

function InBlock(LtB, LtE: LARGE_INTEGER; ExS: PExtent): Int8; overload;
var
 LsB, LsE: LARGE_INTEGER;
begin
 LsB := ExS.Lcn; LsE.QuadPart := ExS.Lcn.QuadPart + ExS.Vcs - 1;
 if (LtB.QuadPart = LsB.QuadPart) and (LsE.QuadPart = LtE.QuadPart) then
  Result := 0
 else
  if (LtB.QuadPart <= LsB.QuadPart) and (LsE.QuadPart <= LtE.QuadPart) then
   Result := 1
  else
   if LtE.QuadPart < LsB.QuadPart then
    Result := 5
   else
    if LsE.QuadPart < LtB.QuadPart then
     Result := 3
    else
     if (LsB.QuadPart <= LtB.QuadPart) and (LtE.QuadPart <= LsE.QuadPart) then
       Result := 6
     else
      if (LsB.QuadPart <= LtE.QuadPart) and (LtE.QuadPart < LsE.QuadPart) then
       Result := 4
     else
      if (LsB.QuadPart < LtB.QuadPart) and (LtB.QuadPart <= LsE.QuadPart) then
       Result := 2
      else
       Result := Low(Int8)
end;

function InBlock(ExT, ExS: PExtent): Int8; overload;
begin
 Result := InBlock(ExT.Lcn, LARGE_INTEGER(ExT.Lcn.QuadPart + ExT.Vcs - 1), ExS)
end;

function InBlock(TLB: UINT64; TCSz: Cardinal; ExS: PExtent): Int8; overload;
begin
 Result := InBlock(LARGE_INTEGER(TLB), LARGE_INTEGER(TLB + TCSz - 1), ExS)
end;

function InBlock(ExT: PExtent; SF: PFilesLayoutInformation;
                              sli: LARGE_INTEGER): Int8; overload;
begin
 if (Length(SF.Data) <= sli.HighPart)
 or (Length(SF.Data[sli.HighPart].Extents) <= sli.LowPart) then
  Result := Low(Int8)
 else
  Result := InBlock(ExT, @SF.Data[sli.HighPart].Extents[sli.LowPart])
end;

function InBlock(TF: PFilesLayoutInformation; tli: LARGE_INTEGER;
                 SF: PFilesLayoutInformation; sli: LARGE_INTEGER;
             bAnyEx: Boolean = false): Int8; overload;
begin
 if bAnyEx or (Length(SF.Data[sli.HighPart].Extents) = 1) then
  Result := InBlock(@TF.Data[tli.HighPart].Extents[tli.LowPart], SF, sli)
 else
  Result := Low(Int8)
end;

function BinSchCI(Ext: TExtent; F: PFilesLayoutInformation;
              var idx: Integer; opt: TSetOfByte = [0, 1]) : Boolean; overload;
var
 i, j: Integer; rib : Int8;
begin
 Result := false;
 j := Length(F.LcnIdx) - 1; i := j; idx := i;
 while i > 1 do begin
  if idx < 0 then idx := idx + i else if j < idx then idx := idx - i;
  i := (i + i mod 2) div 2;
  rib := InBlock(@Ext, F, F.LcnIdx[idx]);
  if rib in opt then break else
   case rib of
    Low(Int8): begin
     if 0 < idx then Dec(idx) else if idx < j then Inc(idx) else break;
     case InBlock(@Ext, F, F.LcnIdx[idx]) of
      4, 5: idx := idx - i;
      2, 3: idx := idx + i;
     end;
    end;
    4, 5: idx := idx - i;
    2, 3: idx := idx + i;
   else break end;
 end;
 i := 1;
 repeat
  if (0 <= idx + i) and (idx + i <= j) then
   if InBlock(@Ext, F, F.LcnIdx[idx + i]) in opt then Result := true else
    if Result then break;
  Dec(i)
 until (idx + i < 0) or (i < -1) and not Result;
 if Result then idx := idx + i + 1 else idx := -1
end;

function BinSchCI(Lcn: UINT64; Siz: Cardinal; SFL: PFilesLayoutInformation;
              var idx: Integer; opt: TSetOfByte = [0, 1]) : Boolean; overload;
var
 Ext: TExtent;
begin
 Ext.Lcn.QuadPart := Lcn; Ext.Vcs := Siz; Result := BinSchCI(Ext, SFL, idx, opt)
end;

function BinSchCI(TF: PFilesLayoutInformation; tli: LARGE_INTEGER;
                  SF: PFilesLayoutInformation; var idx: Integer;
                 opt: TSetOfByte = [0, 1]): Boolean; overload;
begin
 Result := BinSchCI(TF.Data[tli.HighPart].Extents[tli.LowPart], SF, idx, opt)
end;

procedure ReIndexCleanSI(SF: PFilesLayoutInformation);
var
 i, j, k: Integer; b: Boolean;
begin
 if Length(SF.SizIdx) = 0 then exit;

 i := Length(SF.SizIdx) - 2;
 QuickSort(SF, 0, i + 1, @CluSizCmp, @CluSizPer);
 j := 0; b := true;
 with SF^ do begin
  for k := 0 to i do
   if SizIdx[k] = SizIdx[k + 1] then begin
    b := k < i;
    Inc(j)
   end else if Data[SizIdx[k]].ClusCount = 0 then
    Inc(j)
   else
    if 0 < j then SizIdx[k - j] := SizIdx[k];
  if b and(Data[SizIdx[i + 1]].ClusCount = 0)then
   Inc(j);
  if 0 < j then
   SetLength(SizIdx, i + 2 - j)
 end;
end;

function BinSchSI(Sz: Cardinal; SF: PFilesLayoutInformation;
             var idx: Integer; bMax: Boolean = false): Boolean; overload;
var
 i, j, cSz: Integer;
begin
 Result := false; j := Length(SF.SizIdx) - 1; i := j; idx := i;
 while i > 1 do begin
  if idx < 0 then idx := idx + i else if j < idx then idx := idx - i;
  i := (i + i mod 2) div 2;
  cSz := SF.Data[SF.SizIdx[idx]].ClusCount;
  if cSz < Sz then idx := idx + i else if Sz < cSz then idx := idx - i else
   break;
 end;
 i := 1; cSz := idx;
 repeat
  if (0 <= idx + i) and (idx + i <= j) then
   if SF.Data[SF.SizIdx[idx + i]].ClusCount = Sz then begin
    Result := true; if bMax then break;
   end else begin
    if Result then break;
    if Sz < SF.Data[SF.SizIdx[idx + i]].ClusCount then cSz := idx + i;
   end;
  Dec(i)
 until (idx + i < 0) or (i < -1) and not Result;
 if Result then if bMax then begin
  idx := idx + i;
  while (idx < Length(SF.SizIdx) - 1)
    and (SF.Data[SF.SizIdx[idx + 1]].ClusCount = Sz) do Inc(idx)
 end else idx := idx + i + 1 else idx := cSz - 1
end;

function BinSchSI(SF: PFilesLayoutInformation;
                 ind: Integer; var idx: Integer): Boolean; overload;
begin
 Result := BinSchSI(SF.Data[ind].ClusCount, SF, idx);
 if Result then begin
  Result := false;
  while SF.SizIdx[idx] <> ind do begin
   Inc(idx);
   if idx >= Length(SF.SizIdx) then begin idx := -1; exit end
  end;
  Result := true
 end;
end;

function ReIndexCleanLI(F: PFilesLayoutInformation): Boolean;
var
 i, j, k: Integer;
begin
 Result := false; i := 0; j := Length(F.LcnIdx) - 1;
 for k := 0 to j do with F.LcnIdx[k] do
  if  Length(F.Data[HighPart].Extents) <= LowPart then begin
   if Length(F.Data[HighPart].Extents) = 0 then begin
    F.ClusCount := F.ClusCount - F.Data[HighPart].ClusCount;
    F.Data[HighPart].ClusCount := 0
   end;
   Inc(i);
   Result := true
  end else if Result then
   F.LcnIdx[k - i] := F.LcnIdx[k];

 if Result then begin j := j - i; SetLength(F.LcnIdx, j + 1) end;
 QuickSort(F, 0, j, @CluLcnCmp, @CluLcnPer);

 i := 0; Dec(j);
 for k := 0 to j do
  if F.LcnIdx[k].QuadPart = F.LcnIdx[k + 1].QuadPart then
   Inc(i)
  else if 0 < i then
   F.LcnIdx[k - i] := F.LcnIdx[k];

 if 0 < i then SetLength(F.LcnIdx, j - i + 2)
end;

// 1 - dropped, 2 - in use, 4 - deferred, 8 - done
procedure SetState(var Layout: TFileLayoutInformation; State: Byte);
var
 i : Integer;
begin
 if Layout.State and State = 0 then begin
  for i in [1, 2, 4, 8] do Layout.State := Layout.State or i xor i;
  Layout.State := Layout.State or State
 end
end;
function IsState(State, Status: Byte) : Boolean; overload;
begin
 Result := State and Status = Status
end;
function IsState(Layout: TFileLayoutInformation; State: Byte) : Boolean; overload;
begin
 Result := IsState(Layout.State, State)
end;
function IsState(Layouts: PFilesLayoutInformation; idx: Integer; State: Byte) : Boolean; overload;
begin
 Result := IsState(Layouts.Data[idx].State, State)
end;

function ChgExpSize(AddV: Int64; bDec: Boolean = true; bRep: Boolean = true): Boolean; overload;
label Fai; var C, A: Int64;
begin
 Result := false; if AddV < 0 then A := -AddV else A := AddV;
 if ExpSize < 2 * A then begin
  OutputMessage(mLowPriErr, 'ChgExpSize: too big value AddV = %d (ExpSize = %d)', [AddV, ExpSize]);
  exit
 end;
 C := Int64(SrcVolSpec.TotalNumOfClusters);
 if (-C < AddV) and (AddV < C) then begin
  if bDec then
   if AddV <= ExpSize then Dec(ExpSize, AddV) else if bRep then goto Fai else exit
  else
   if 0 <= AddV + Int64(ExpSize) then Inc(ExpSize, AddV) else
    if bRep then goto Fai else exit
 end else if bRep then goto Fai else exit;
 Result := true;
 exit;
Fai:
 OutputMessage(mLowPriErr, 'ChgExpSize: AddV = %d, ExpSize = %d', [AddV, ExpSize])
end;

function ChgExpSize(FileL: TFileLayoutInformation;
                     bDec: Boolean = true; AddV: Int64 = 0): Boolean; overload;
begin
 with FileL do begin
  Result := (0 < ClusCount) and (ClusCount < Int64(SrcVolSpec.TotalNumOfClusters));
  if Result and ChgExpSize(Int64(ClusCount) + AddV, bDec, false) then
   exit;
  OutputMessage(mLowPriErr, 'ChgExpSize: file "%s", size %d, AddV = %d, ExpSize = %d',
                            [FName, ClusCount, AddV, ExpSize])
 end
end;

function ChgExpSize(SF: PFilesLayoutInformation; ID: Integer;
                  bDec: Boolean = true; AddV: Integer = 0): Boolean; overload;
begin
 Result := ChgExpSize(SF.Data[ID], bDec, AddV)
end;

var
 CntDone: Cardinal = 0; CntIndx: Cardinal = 0; CntMemo: UINT64 = 0;
 CntDfrF: Cardinal = 0; CntCmpr: Cardinal = 0;
 CntDefr: Integer  = 0; CntCmpF: Cardinal = 0;
 SizBefCmpr : UINT64 = 0;
 SizAftCmpr : UINT64 = 0;
 CntMovF, CntClnd, CntClnF: Cardinal;

function ExtLID(F: PFilesLayoutInformation; idx: Integer): PExtent; overload;
begin
 try
  if F.LcnIdx[idx].LowPart < Length(F.Data[F.LcnIdx[idx].HighPart].Extents) then
   Result := @F.Data[F.LcnIdx[idx].HighPart].Extents[F.LcnIdx[idx].LowPart]
  else
   Result := nil
 except on E: Exception do
  raise Exception.Create(Format('ExtLID(1) [%d/%d]: %s',
                    [F.LcnIdx[idx].HighPart, F.LcnIdx[idx].LowPart, E.Message]))
 end
end;

function ExtLID(F: PFilesLayoutInformation; idx: LARGE_INTEGER): PExtent; overload;
begin
 try
  if idx.LowPart < Length(F.Data[idx.HighPart].Extents) then
   Result := @F.Data[idx.HighPart].Extents[idx.LowPart]
  else
   Result := nil
 except on E: Exception do
  raise Exception.Create(Format('ExtLID(2) [%d/%d]: %s',
                                [idx.HighPart, idx.LowPart, E.Message]))
 end
end;

function DatLID(F: PFilesLayoutInformation; idx: Integer): PFileLayoutInformation;
begin
 Result := @F.Data[F.LcnIdx[idx].HighPart]
end;

function GetLayout(Typ: Byte; VolGuid, FileName: String;
            var Layout: TFileLayoutInformation) : Boolean;
var
 CntExFr, CluCnt : Cardinal;
begin
 if (0 < Typ) and (Layout.State and 1 = 0) then begin
  CntExFr := Length(Layout.Extents); CluCnt := Layout.ClusCount;

  Result := GetFileClusters(VolGuid, FileName, Layout);
  with Layout do begin
   if (CluCnt <> ClusCount) and (CluCnt < ExpSize + ClusCount) then
    Result := ChgExpSize(Layout, false, -Int32(CluCnt));

   if State and 64 = 0 then
    if ((State and 16) =  0)
    and((CluCnt < ClusCount shr 2) or (ClusCount shl 2 < CluCnt))
    or ((State and 16) = 16)
    and((CluCnt < ClusCount shr 16) or (ClusCount shl 16 < CluCnt)) then
    begin
     State := State or 64;
     if bReportFiles and (@fReportTestingEvent <> nil) then
      fReportTestingEvent(teVltDmgd, FileName)
    end;

   if (1 < CntExFr) and (Length(Extents) = 1) then Inc(CntDefr)
   else
    if (1 = CntExFr) and (1 < Length(Extents)) then
     if 0 < CntDefr then Dec(CntDefr);

   if ((State and 48) = 48) and IsNtfsCompressed(VolGuid + FileName) then
   begin
    if Result then begin
     SizBefCmpr := SizBefCmpr + CluCnt; SizAftCmpr := SizAftCmpr + ClusCount
    end;
    State := State xor 16
   end
  end
 end else
  Result := GetFileClusters(VolGuid, FileName, Layout)
end;

function DropLcnIndex(F: PFilesLayoutInformation; hi, li: Integer;
                      bRSz : Boolean = false; bAll : Boolean = true;
                      bChgSize : Boolean = true) : Integer; overload;
var
 i : Integer;
begin
 Result := -1;
 if (Length(F.LcnIdx) = 0) or (hi < 0) and (li < 0) then exit;
 if li < 0 then Result := 0 else begin
  Result := 1;
  if not bAll and (ExtLID(F, li) <> nil) then Dec(F.ClusCount, ExtLID(F, li).Vcs)
 end;
 if hi < 0 then hi := F.LcnIdx[li].HighPart;

 for i := li + 1 to Length(F.LcnIdx) - 1 do
  if (bAll or (li < 0) and (Result = 0)) and (hi = F.LcnIdx[i].HighPart) then
  begin
   if not bAll and (ExtLID(F, i) <> nil) then Dec(F.ClusCount, ExtLID(F, i).Vcs);
   Inc(Result)
  end else if 0 < Result then
   F.LcnIdx[i - Result] := F.LcnIdx[i];
 if 0 < Result then begin
  if bRSz then
   SetLength(F.LcnIdx, Length(F.LcnIdx) - Result);
  if bAll then
   Dec(F.ClusCount, F.Data[hi].ClusCount)
 end
end;

function DropLcnIndex(F: PFilesLayoutInformation;
                var idx: TIntegerDynArray;
                   bRSz: Boolean = true) : Integer; overload;
var
 i, j: Integer;
begin
 if Length(idx) = 0 then exit else QuickSort(@idx, 0, Length(idx) - 1);
 Result := 0;
 for i := 0 to Length(F.LcnIdx) - 1 do
  if BinSchInt(idx, F.LcnIdx[i].HighPart, j) then
   Inc(Result)
  else if 0 < Result then
   F.LcnIdx[i - Result] := F.LcnIdx[i];

 if bRSz then
  SetLength(F.LcnIdx, Length(F.LcnIdx) - Result)
end;

function DropSizIndex(SF: PFilesLayoutInformation; idx: Integer;
                    bRSz: Boolean = false; bAll: Boolean = false): Boolean;
var
 i : Integer;
begin
 Result := false;
 if (Length(SF.SizIdx) = 0) or (Length(SF.SizIdx) <= idx) then exit;
 i := - 1 - idx;
 if idx < 0 then
  if BinSchSI(SF, i, idx) then begin
   if SF.Data[i].ClusCount = 0 then begin
    ReIndexCleanSI(SF);
    Result := true;
    exit
   end;
  end else begin ReIndexCleanSI(SF); if not BinSchSI(SF, i, idx) then exit end;
 Result := true;

 if bAll then begin
  for i := idx to Length(SF.SizIdx) - 1 do
   SF.SizIdx[i - idx] := SF.SizIdx[i];
  if bRSz then
   SetLength(SF.SizIdx, Length(SF.SizIdx) - idx)
 end else begin
  for i := idx + 1 to Length(SF.SizIdx) - 1 do
   SF.SizIdx[i - 1] := SF.SizIdx[i];
  if bRSz then
   SetLength(SF.SizIdx, Length(SF.SizIdx) - 1)
 end
end;

function HashCodes(VolHnd: THandle; SF: PFilesLayoutInformation; ix: Integer;
                   Typ : THshCodAction): Boolean;
begin
 Result := HashCodesIndex(CntIndx, VolHnd, SF, ix, Typ);
 CntMemo := (Length(NewLays.HCidx) + Length(OsLays.HCidx)) div 64
end;

function DropIndexSeS(SF: PFilesLayoutInformation;
                      idx: Integer; lci: Integer = -1; State: Byte = 255): Boolean;
begin
 if idx < 0 then
  if -1 < lci then idx := SF.LcnIdx[lci].HighPart else
   raise Exception.Create('DropIndexSeS: Unexpected negative value of main index');

 Result := DropSizIndex(SF, - 1 - idx, true);
 lci := DropLcnIndex(SF, idx, lci, true, true);

 if State < 255 then SetState(SF.Data[idx], State);

 Result := Result and (0 < lci)
end;

function GetLayouts(VolGuid: String; var Files: TStringDynArray;
              F: PFilesLayoutInformation; bDrp: Boolean = true;
                                          bISz: Boolean = false;
                                        bNtfsC: TBooleanDynArray = nil): Boolean;
var
 bOsLays: Boolean; hLibCnf: HMODULE; aNNCE: TStringDynArray;
 i, j, k, m, n : Integer;
 function IsNotSystemFile(Layout: TFileLayoutInformation): Boolean;
 var
  i: Integer;
 begin
  Result := true;
  for i := 0 to Length(Layout.Extents) - 1 do begin
   Result := IsNotSystemLcn(Layout.Extents[i].Lcn); if not Result then exit
  end;
 end;
 function IsForNtfsCompression(Layout: TFileLayoutInformation; idx: Integer): Boolean;
 var
  i: Integer;
 begin
  if bNtfsC = nil then begin
   Result := not bDrp and bISz;
   if bOsLays then
    for i := 0 to Length(aNNCE) - 1 do
     if MaskFits(aNNCE[i], ExtractFileName(Layout.FName), hLibCnf) then begin
      Result := false;
      break
     end
  end else
   Result := bNtfsC[idx];

  if Result then
   Result := not IsNtfsCompressed(SrcVolSpec.GUID + Layout.FName)
  else
   if bOsLays and bReportFiles then begin
    fReportTestingEvent(teCprSkip, 'OLD MODIFIED FILES', true);
    fReportTestingEvent(teCprSkip, Layout.FName)
   end;

  if Result then begin // == [4Kb ... 40Gb], 4k cluster
   Result := (1 < Layout.ClusCount) and (Layout.ClusCount < 10485760);
   if not Result and (bNtfsC <> nil) then bNtfsC[idx] := false
  end
 end;
 procedure SearchDropJunctionFiles;
 var
  i, j: Integer; F1, F2: PFileLayoutInformation; b: Boolean;
 begin // System hidden junction paths & files give physical duplicates, cleanup:
  i := 0;
  while i + 1 < Length(F.LcnIdx) do begin
   if (LBE(ExtLID(F, i)) = LBE(ExtLID(F, i + 1)))
   and(LEE(ExtLID(F, i)) = LEE(ExtLID(F, i + 1)))then begin
    F1 := DatLID(F, i); F2 := DatLID(F, i + 1);
    b := Length(F1.Extents) = Length(F2.Extents);
    if b then
     for j := 0 to Length(F1.Extents) - 1 do
      if(LBE(@F1.Extents[j]) <> LBE(@F2.Extents[j]))
      or(LEE(@F1.Extents[j]) <> LEE(@F2.Extents[j]))then begin b := false; break end
   end else b := false;
   if b then DropIndexSeS(F, -1, i, 1) else Inc(i)
  end
 end;
begin
 Result := false;
 try
  bOsLays := not bDrp and bISz and (bNtfsC = nil);
  if bOsLays then begin
   hLibCnf := LoadCloperF; aNNCE := SplitString(NoNtfsCext, '|')
  end;
  SetLength(F.Data, Length(Files));
  F.ClusCount := 0;
  SetLength(F.HCidx, 0); F.bHCini := true;
  SetLength(F.HCbuf, 0); F.SzHC := 0;
  n := 0;
  for i := 0 to Length(Files) - 1 do
   if GetLayout(0, VolGuid, Files[i], F.Data[i - n]) then
   begin
    if IsNotSystemFile(F.Data[i - n]) then begin
     with F.Data[i - n] do begin
      if IsForNtfsCompression(F.Data[i - n], i) then State := 18 else
       State := 2;
      F.ClusCount := F.ClusCount + ClusCount;
      SetLength(HC, 0)
     end;
     j := Length(F.LcnIdx);
     SetLength(F.LcnIdx, j + Length(F.Data[i - n].Extents));
     if bISz then begin
      SetLength(F.SizIdx, i - n + 1); F.SizIdx[i - n] := i - n
     end;
     for m := j to Length(F.LcnIdx) - 1 do
      SetLargeInteger(F.LcnIdx[m], i - n, m - j)
    end else begin
     SetLength(F.Data[i - n].Extents, 0); Inc(n)
    end
   end else Inc(n);
  if 0 < n then
   SetLength(F.Data, Length(Files) - n);
  if bDrp then
   SetLength(Files, 0);
  if bISz then
   ReIndexCleanSI(F);
  if 0 < Length(F.Data) then begin
   QuickSort(F, 0, Length(F.LcnIdx) - 1, @CluLcnCmp, @CluLcnPer);
   SearchDropJunctionFiles
  end;
  if bOsLays then FreeLibrary(hLibCnf);
  Result := true
 except
  on E : Exception do begin
   OutputMessage(mInternalErr, 'Failed with exception: ' + E.Message);
   OutputSysError(true)
  end
 end
end;

function GetMaxLCN(Lays : TFilesLayoutInformation) : LARGE_INTEGER; overload;
begin
 if 0 < Length(Lays.LcnIdx) then begin
  Result := Lays.LcnIdx[Length(Lays.LcnIdx) - 1];
  Result.QuadPart := Lays.Data[Result.HighPart].Extents[Result.LowPart]
     .Lcn.QuadPart + Lays.Data[Result.HighPart].Extents[Result.LowPart].Vcs
 end else
  Result.QuadPart := 0
end;

procedure GetMaxLCN; overload;
var
 LCN : LARGE_INTEGER;
begin
 MaxLCN := GetMaxLCN(DelLays); LCN := GetMaxLCN(OtLays);
 if MaxLCN.QuadPart < LCN.QuadPart then MaxLCN := LCN
end;

procedure RebuildIndex(Typ: Byte; SF: PFilesLayoutInformation;
                    hi, li: Integer; brc: Boolean); overload;
var
 i, j, k : Integer; TopLcn : LARGE_INTEGER; blc : Boolean;
begin
 i := DropLcnIndex(SF, hi, li);
 if GetLayout(Typ, SrcVolSpec.GUID, SF.Data[hi].FName, SF.Data[hi]) then
 begin
  SF.ClusCount := SF.ClusCount + SF.Data[hi].ClusCount;
  i := Length(SF.LcnIdx) - i; j := High(Cardinal) - i + 1;
  if Length(SF.LcnIdx) <> i + Length(SF.Data[hi].Extents) then
   SetLength(SF.LcnIdx, i + Length(SF.Data[hi].Extents));
  for k := i to Length(SF.LcnIdx) - 1 do begin
   SF.LcnIdx[k].HighPart := hi; SF.LcnIdx[k].LowPart := j + k;
   SF.ClusCount := SF.ClusCount + ExtLID(SF, SF.LcnIdx[k]).Vcs
  end;
  blc := false;
  if 1 < Length(SF.LcnIdx) then
   if (i = 0)
   or (0 < Length(SF.Data[SF.LcnIdx[  i  ].HighPart].Extents))
   and(0 < Length(SF.Data[SF.LcnIdx[i - 1].HighPart].Extents))
   and(ExtLID(SF, i).Lcn.QuadPart < ExtLID(SF, i - 1).Lcn.QuadPart)then
    blc := ReIndexCleanLI(SF);
  if (blc or brc) and (1 < Length(SF.SizIdx)) then
   ReIndexCleanSI(SF)
 end else begin
  if 0 < i then
   SetLength(SF.LcnIdx, Length(SF.LcnIdx) - i);
  DropSizIndex(SF, - 1 - hi, true)
 end;
end;

procedure RebuildIndex(Typ: Byte; SF: PFilesLayoutInformation;
                   var idx: TIntegerDynArray;
                     bnoup: Boolean = false;
                     brszi: Boolean = true); overload;
var
 i, j, k, m: Integer; ind: TIntegerDynArray; L, LL, ML: UINT64; b: Boolean;
 function _L(i : Integer) : UINT64;
 begin
  Result := SF.Data[SF.LcnIdx[i].HighPart].Extents[SF.LcnIdx[i].LowPart]
      .Lcn.QuadPart
 end;
begin
 if Length(idx) = 0 then exit else DropLcnIndex(SF, idx);

 if 0 = Length(SF.LcnIdx) then LL := 0 else LL := _L(Length(SF.LcnIdx) - 1);
 ML := 0; b := true;

 for i := 0 to Length(idx) - 1 do if -1 < idx[i] then
  if bnoup or GetLayout(Typ, SrcVolSpec.GUID, SF.Data[idx[i]].FName,
                                              SF.Data[idx[i]]) then
  begin
   if bnoup then with SF.Data[idx[i]] do begin
    ClusCount := 0;
    for j := 0 to Length(Extents) - 1 do
     ClusCount := ClusCount + Extents[j].Vcs;
   end;
   SF.ClusCount := SF.ClusCount + SF.Data[idx[i]].ClusCount;
   j := Length(SF.LcnIdx);
   SetLength(SF.LcnIdx, j + Length(SF.Data[idx[i]].Extents));
   k := High(Cardinal) - j + 1;
   for m := j to Length(SF.LcnIdx) - 1 do begin
    SF.LcnIdx[m].HighPart := idx[i]; SF.LcnIdx[m].LowPart := k + m
   end;
   L := _L(j); if b then b := LL <= L; if b then b := ML <= L; if b then ML := L
 end;
 if not b and (1 < Length(SF.LcnIdx)) then
  b := ReIndexCleanLI(SF);
 if brszi and (b or (1 < Length(SF.SizIdx))) then
  ReIndexCleanSI(SF);
 SetLength(idx, 0)
end;

function CompressFile(var FileL: TFileLayoutInformation) : Boolean;
var
 F: Cardinal; L: ULONG64; btwo, bfl: Boolean;
begin
 Result := false;
 if ((FileL.State and 48) = 16) and (1 < FileL.ClusCount) then begin
  btwo := true; bfl := true;
  repeat
   case NtfsSetCompression(SrcVolSpec.GUID + FileL.FName, COMPRESSION_FORMAT) of
   1: begin
    bfl := false; break
   end;
   2: begin // Call before move & rebuild index, doesn't update it:
    Sleep(1); FileL.State := FileL.State or 32; Inc(CntCmpr);
    bfl := false; Result := true; break
   end;
   3: begin
    OutputMessage(mLowPriErr, 'Compression failed for "%s"', [FileL.FName]);
    if bReportFiles then ReportTestingEventF(teCprFail, FileL.FName)
   end;
   4: begin
    OutputMessage(mLowPriErr, 'Couldn''t get handler to compress "%s"', [FileL.FName]);
    if bReportFiles then ReportTestingEventF(teHdlFail, FileL.FName)
   end;
   end;
   btwo := not btwo; Sleep(5)
  until btwo;
 end else bfl := false;
 if bfl then begin
  Inc(CntCmpF); FileL.State := FileL.State or 16 xor 16 // 1 attempt.
 end;
end;

function CompressDefragHash(Typ: Byte; hScVol: THandle;
                              F: PFilesLayoutInformation;
                             ix: Integer;
                         var LC: UINT64;
                         var RC: Cardinal;
                      bCompress: Boolean = true;
                          bHash: Boolean = false;
                        bDefrag: Boolean = false): Byte; overload;
 function NewLcn(Exs: TExtents): UINT64;
 begin
  with Exs[Length(Exs) - 1] do Result := Lcn.QuadPart + Vcs
 end;
 function _Compress : Boolean;
 var ocCmpr, ocCmpF: Cardinal;
 begin
  if bCompress then begin
   ocCmpF := CntCmpF; ocCmpr := CntCmpr;
   Result := CompressFile(F.Data[ix]) and((ocCmpr < CntCmpr)
                                       or (ocCmpF < CntCmpF)) // can be handled?
  end else Result := false
 end;
label fai; var bExLcn: Boolean; CL: UINT64; begin
 if bCompress then
  bCompress := F.Data[ix].State and 16 = 16;
 if bCompress or (1 <> Length(F.Data[ix].Extents)) then Result := 0 else
  if bDefrag then Result := 0 else Result := 2;

 if Result = 0 then with F.Data[ix] do begin
  if ((Length(Extents) = 0) or (bCompress and _Compress))
  and(not GetLayout(Typ, SrcVolSpec.GUID, FName, F.Data[ix])) then goto fai;
  if Length(Extents) = 0 then goto fai;
  bExLcn := LC <> HighUInt64;
  if bExLcn then CL := LC else if bDefrag then CL := 0 else CL := UINT64(MaxLCN);
  LC := 0; RC := ClusCount;
  if (1 < Length(Extents)) or bDefrag or bExLcn and (LBE(@Extents[0]) <> CL) then
  begin
   if bExLcn then begin
    if MoveToLcn(hScVol, CL, SrcVolSpec.GUID + FName, RC, LC, true) <> 0 then
     goto fai
   end else
    if not FindFreeBlock(hScVol, CL, RC, CL, LC) then begin
     if not FindFreeBlock(hScVol, NewLcn(Extents), RC, CL, LC) then
      goto fai;
     if MoveToLcn(hScVol, CL, SrcVolSpec.GUID + FName, RC, LC) <> 0 then
      goto fai
    end else
     if MoveToLcn(hScVol, CL, SrcVolSpec.GUID + FName, RC, LC) <> 0 then begin
      if not FindFreeBlock(hScVol, NewLcn(Extents), RC, CL, LC) then
       goto fai;
      if MoveToLcn(hScVol, CL, SrcVolSpec.GUID + FName, RC, LC) <> 0 then
       goto fai
     end;
    if not GetLayout(Typ, SrcVolSpec.GUID, FName, F.Data[ix]) then goto fai;
   if Length(Extents) <> 1 then goto fai
  end;
  Result := 1
 end;
 if bHash then
  if (Length(F.Data[ix].HC) = 0) and (F.Data[ix].State and 64 = 0) then
   HashCodes(hScVol, F, ix, hcAdd);
 exit;
fai:
 Inc(CntDfrF); // Undefraggable or unmovable.
 if bReportFiles then ReportTestingEventF(teDfrFail, F.Data[ix].FName)
end;
function CompressDefragHash(Typ: Byte; hScVol: THandle;
                              F: PFilesLayoutInformation;
                             ix: Integer;
                      bCompress: Boolean = false;
                          bHash: Boolean = true;
                        bDefrag: Boolean = false): Byte; overload;
var
 L: UINT64; R: Cardinal;
begin
 L := HighUInt64; R := HighUInt32;
 Result := CompressDefragHash(Typ, hScVol, F, ix, L, R, bCompress, bHash, bDefrag)
end;

var
 FrBks: TExtents;

function ExIn(A, B, C, E: UInt64; bin: Boolean = false): Boolean; overload;
begin
 if bin then
  Result := (C <= A) and (B <= E)
 else
  Result := (A <= C)and(C <= B) or (A <= E)and(E <= B) or (C < A)and(B < E)
end;
function ExIn(Es: TExtents; Et: PExtent; i: Integer; bin: Boolean = false): Boolean; overload;
begin
 Result := ExIn(LBE(Et), LEE(Et), LBE(@Es[i]), LEE(@Es[i]), bin)
end;
function BiShEx(Es: TExtents; Et: PExtent; var i: Integer; bin: Boolean = false): Boolean; overload;
var
 j, k: Integer;
begin
 Result := false; k := Length(Es) - 1; j := k; i := j;
 while j > 1 do begin
  if i < 0 then i := i + j else if k < i then i := i - j;
  j := (j + j mod 2) div 2;
  if LEE(Et) < LBE(@Es[i]) then i := i - j else
   if LEE(@Es[i]) < LBE(Et) then i := i + j else break
 end;
 j := 1;
 repeat
  if (0 <= i + j) and (i + j <= k) then
   if ExIn(Es, Et, i + j, bin) then Result := true else
    if LEE(@Es[i + j]) < LBE(Et) then break;
  Dec(j)
 until (i + j < 0) or not Result and (j < -1);
 i := i + j + 1
end;
function BiShEx(Es: TExtents; BL: UInt64; var i: Integer;
               bin: Boolean = false; RC: Cardinal = 1): Boolean; overload;
var
 Ex: TExtent;
begin
 Ex.Lcn.QuadPart := BL; Ex.Vcs := RC; Result := BiShEx(Es, @Ex, i, bin)
end;
procedure MoveFreeBlok(F: PFileLayoutInformation; BL: UINT64; RC: Cardinal;
                     bin: Boolean = false);
var
 i, j, k: Integer; E: PExtent;
begin
 E := @F.Extents[0]; j := Length(FrBks);
 BiShEx(FrBks, LBE(E), i, true); if i + 1 = j then Inc(i);

 if (i = 0) or (i = j) or (LEE(@FrBks[i - 1]) + 1 < LBE(E)) then begin
  if (i = j) or (LBE(E) + E.Vcs < LBE(@FrBks[i])) then begin
   SetLength(FrBks, j + 1);
   for k := j downto i + 1 do FrBks[k] := FrBks[k - 1]; Inc(j);
   FrBks[i].Vcs := E.Vcs
  end else
   Inc(FrBks[i].Vcs, E.Vcs);
  FrBks[i].Lcn.QuadPart := LBE(E)
 end else begin Dec(i); Inc(FrBks[i].Vcs, E.Vcs) end;

 if BiShEx(FrBks, BL, i, bin, RC) then
  if (LBE(@FrBks[i]) < BL) and (BL + RC <= LEE(@FrBks[i])) then begin
   SetLength(FrBks, j + 1);
   for k := j downto i + 1 do FrBks[k] := FrBks[k - 1];
   Dec(FrBks[i + 1].Vcs, BL + RC - LBE(@FrBks[i + 1]));
   FrBks[i + 1].Lcn.QuadPart := BL + RC;
   FrBks[i].Vcs := BL - LBE(@FrBks[i])
  end else begin
   if LBE(@FrBks[i]) = BL then Inc(FrBks[i].Lcn.QuadPart, RC);
   Dec(FrBks[i].Vcs, RC)
  end
end;
function FindFreeBlok(LB: UINT64; RC: Cardinal; var BL, EL: UINT64): Boolean;
var
 i: Integer;
begin
 Result := false; BiShEx(FrBks, LB, i, true);
 for i := i to Length(FrBks) - 1 do if RC <= FrBks[i].Vcs then begin
  Result := true; BL := FrBks[i].Lcn.QuadPart; exit
 end
end;

// ****************************************************************************
// Preliminary cleanup

procedure DropFilesWithSimilarBlocks;
var
 FLT, FLS: TFileLayoutInformation; i, j, k, ti, si: Integer; bCmp: Boolean;
begin
 i := 0;
 while i < Length(OtLays.LcnIdx) do begin
  if OtLays.LcnIdx[i].LowPart = 0 then
   if BinSchCI(@OtLays, OtLays.LcnIdx[i], @OsLays, j, [0]) then begin
    ti := OtLays.LcnIdx[i].HighPart; FLT := OtLays.Data[ti];
    si := OsLays.LcnIdx[j].HighPart; FLS := OsLays.Data[si];
    if (FLT.FName = FLS.FName) and (Length(FLT.Extents) = Length(FLS.Extents)) then
    begin
     bCmp := true;
     for k := 0 to Length(FLT.Extents) - 1 do
      if(FLT.Extents[k].Lcn.QuadPart <> FLS.Extents[k].Lcn.QuadPart)
      or(FLT.Extents[k].Vcs          <> FLS.Extents[k].Vcs         ) then begin
       bCmp := false;
       break
      end;
     if bCmp and ByteWiseCompare(FLT) then begin
      DropIndexSeS(@OsLays, si, -1, 1);
      DropLcnIndex(@OtLays, ti, i, true, false);
      if (Length(OtLays.Data[ti].Extents) = 1)
      or (OtLays.Data[ti].ClusCount = 0) then
       SetState(OtLays.Data[ti], 1);
      continue
     end
    end
   end;
  Inc(i)
 end
end;

procedure DropFilesInBlocks(hScVol: THandle; SF: PFilesLayoutInformation);
const
 opts = [0, 6];
var
 i, j, k, m: Integer; L: UINT64; bC, D, O: Boolean; E: TExtent; hTgVol: THandle;
begin
 if GetFileHandle(ExcludeTrailingPathDelimiter(TgtVolSpec.GUID),
                  GENERIC_READ, FILE_SHARE_READ, hTgVol,
                  FILE_FLAG_NO_BUFFERING or FILE_FLAG_RANDOM_ACCESS) then
 begin
  i := 0;
  while i < Length(SF.LcnIdx) do if SF.LcnIdx[i].LowPart = 0 then begin
   bC := true; L := LBE(ExtLID(SF, i)); j := 0;
   for k := 0 to Length(SF.Data[SF.LcnIdx[i].HighPart].Extents) - 1 do begin
    E := SF.Data[SF.LcnIdx[i].HighPart].Extents[k];

    D := BinSchCI(E, @DelLays, m, opts); O := BinSchCI(E, @OtLays, m, opts);

    if D or O then
     bC := ByteWiseCompare(hScVol, LBE(@E), LEE(@E), hTgVol)
    else
     bC := false;

    if not bC then break else if LBE(@E) < L then Inc(j);
   end;
   if bC then with SF.Data[SF.LcnIdx[i].HighPart] do begin
    if D then Dec(DelLays.ClusCount, ClusCount) else Dec(OtLays.ClusCount, ClusCount);
    DropIndexSeS(SF, SF.LcnIdx[i].HighPart, i, 1);
    if 0 < j then Dec(i, j)
   end else Inc(i)
  end else
   Inc(i);
  CloseHandle(hTgVol)
 end;
end;

// ****************************************************************************
// Matching source data to target data

// Callback functions for JoinExtents procedure:
function ExtRngFrBks(P: Pointer; var H: Integer): Integer; begin
 Result := 0; H := Length(TExtents(P^)) - 1
end;
function GetExtFrBks(P: Pointer; Index: Integer): PExtent; begin
 Result := @TExtents(P^)[Index]
end;
function ExtRngTgBks(P: Pointer; var H: Integer): Integer;
begin
 Result := 0; H := Length(PFilesLayoutInformation(P).LcnIdx) - 1
end;
function GetExtTgBks(P: Pointer; Index: Integer): PExtent;
begin
 Result := ExtLID(PFilesLayoutInformation(P), Index);
end;


{ FindMatchesFitInBlocks
  Searches physically moved data on volume and relocates it in front of target
  persistent location.
}
function FindMatchesFitInBlocks(hScVol: THandle) : Boolean;
var
  TgBks: TExtents;

 procedure SetReIndex(var idx: TIntegerDynArray; ID: Integer);
 var
  i : Integer;
 begin
  i := Length(idx); SetLength(idx, i + 1); idx[i] := ID
 end;

 procedure FindMatches;
 var
  Count: Cardinal; AvaDatSiz, FndFtsSiz, FndFlsSiz, TgtMemUse: UINT64;

  function ADUPercent: Integer; begin
   if AvaDatSiz = 0 then Result := 0 else
    Result := 100 * FndFtsSiz * CodeSizeInSectors div AvaDatSiz div SectorsPerCluster
  end;
  function GetCrtcArr : TExtendedDynArray;
  begin
   Result := CrtcArr([ADUPercent,
                      FndFtsSiz * CodSzToBt div 1024,
                      Length(IdxIdx),
                      FndFlsSiz * CodSzToBt div 1024,
                      TgtMemUse div 1024, CntDefr, CntIndx, CntMemo, CntDfrF])
  end;
  procedure FindFitsMsg(Cnt: Cardinal; MsgType: TMsgType; Msg: String);
  begin
   OutputMessage(MsgType, Msg); TCrtcProgress.Echo(Cnt, GetCrtcArr, false)
  end;

  function SearchFits(PEx: PExtent; FL: PFilesLayoutInformation;
                     V, S: Cardinal;
                     bNew: Boolean;
               var bi, ei: Integer;
               var ResTyp: Byte): Boolean;
  begin
   if (Length(FL.SizIdx) = 0) or (S < FL.Data[FL.SizIdx[0]].ClusCount) then
    Result := true
   else
    Result := SearchBestFits(PEx, FL, V, S, bNew, bi, ei, ResTyp,
                                            FndFtsSiz, FndFlsSiz)
  end;
  function GetBlockFits(PEx: PExtent; V, S: Cardinal;
                                var bi, ei: Integer): Cardinal; forward;

  function GetSubBlockFits(PEx: PExtent; V, S: Cardinal;
                                 var bir, eir: Integer): Cardinal;
  label fai; var
   L: UINT64; st, df: Cardinal; bi, ib, ie: Integer;

   function LcnBeg(idx: Integer): UINT64; begin
    Result := FitPEx[IdxPEx[idx]].Lcn.QuadPart + FitBeg[idx] div ClusterInCodSizes
   end;
   function LcnEnd(idx: Integer): UINT64; begin
    Result := FitPEx[IdxPEx[idx]].Lcn.QuadPart
            + (FitBeg[idx] + FitSiz[idx]) div ClusterInCodSizes
   end;
   function GetBegIdx(var idx: Integer): Boolean; var i: Integer; begin
    i := Length(FitBeg) - 1;
    while (0 < idx) and (L < LcnBeg(idx - 1)) do Dec(idx);
    while (idx < i) and (L > LcnBeg(idx    )) do Inc(idx);
    if (0 <= idx) and (idx <= i) then Result := FitPEx[IdxPEx[idx]] = PEx else
     Result := false
   end;
   function GetEndIdx(var idx: Integer): Boolean; var i: Integer; begin
    i := Length(FitBeg) - 1;
    while (idx < i) and (LcnEnd(idx + 1) < L + S) do Inc(idx);
    while (0 < idx) and (LcnEnd(idx    ) > L + S) do Dec(idx);
    if (0 <= idx) and (idx <= i) then Result := FitPEx[IdxPEx[idx]] = PEx else
     Result := false
   end;
  begin
   bi := Length(FitBeg) - 1;
   if (0 <= bi) and (0 <= bir) and (eir <= bi) then begin
    while (bir < bi) and (FitBeg[bir] < V    ) do Inc(bir);
    while ( 0 < eir) and (V + S < FitBeg[eir]) do Dec(eir);
    if (eir < bir) or (FitBeg[bir] < V) or (V + S < FitBeg[eir]) then
     goto fai
   end else
    goto fai;
   bi := bir; L := PEx.Lcn.QuadPart + V;

   if V < FitBeg[bi] div ClusterInCodSizes then begin
    st := GetBlockFits(PEx, V, FitBeg[bi] div ClusterInCodSizes - V, ib, ie);
    if 0 < st then bi := ie + 1
   end;

   while (bi + 1 < Length(FitBeg)) and (PEx = FitPEx[IdxPEx[bi + 1]]) and
          (FitBeg[bi + 1] < ClusterInCodSizes * (V + S)) do
   begin
    st := FitBeg[bi] + FitSiz[bi];
    if st < FitBeg[bi + 1] then begin
     df := FitBeg[bi + 1] - st;
     st := GetBlockFits(PEx, st div ClusterInCodSizes,
                             df div ClusterInCodSizes, ib, ie);
     if 0 < st then bi := ie
    end;
    Inc(bi)
   end;
   if GetEndIdx(bi) then begin
    st := FitBeg[bi] + FitSiz[bi]; df := (V + S) * ClusterInCodSizes;
    if st < df then begin
     df := df - st;
     st := GetBlockFits(PEx, st div ClusterInCodSizes,
                             df div ClusterInCodSizes, ib, ie);
     if 0 < st then bi := ie
    end
   end;
   eir := bi;
   if GetBegIdx(bir) and GetEndIdx(eir) then begin
    Result := eir - bir + 1;
    exit
   end;
fai:
   bir := -1; eir := -1; Result := 0
  end;

  function GetBlockFits(PEx: PExtent; V, S: Cardinal; var bi, ei: Integer): Cardinal;
  var
   rt: Byte;
  begin
   bi := -1; ei := -1; rt := 0; Result := 0;
   if SearchFits(PEx, @OsLays, V, S, false, bi, ei, rt) then begin
    if SearchFits(PEx, @NewLays, V, S, true, bi, ei, rt) then
     exit
   end else if rt <> 1 then
    SearchFits(PEx, @NewLays, V, S, true, bi, ei, rt);

   case rt of
   0: if (-1 < bi) and (-1 < ei) then
       Result := ei - bi + 1;
   1: Result := ei - bi + 1;
   2: Result := GetSubBlockFits(PEx, V, S, bi, ei);
   end
  end;
  function CheckGetBlockFits(PEx: PExtent; V, S: Cardinal): Cardinal;
  var bi, ei: Integer;
  begin
   bi := -1; ei := -1;
   if CheckFoundBestFits(PEx, V, S, bi, ei) then
    Result := GetSubBlockFits(PEx, V, S, bi, ei)
   else
    Result := GetBlockFits(PEx, V, S, bi, ei)
  end;

  procedure UpdateJoinFrBks(FO: TExtents; var F: TExtents);
  var
   FN: TExtents;
  begin
   GetFreeBlocks(hScVol, FN); JoinExtents(F, @FO, @FN, @GetExtFrBks, @ExtRngFrBks)
  end;

  function CheckHasAvailableFragments(Ex: PExtent; var MS: Integer): Boolean;
   function ExLID(F: PFilesLayoutInformation; idx: Integer): PExtent; begin
    if (idx < 0) or (Length(F.LcnIdx) <= idx) then Result := nil else begin
     Result := ExtLID(F, idx);
     if (Result <> nil) and (InBlock(Result, Ex) in [3, 5]) then Result := nil
    end;
   end;
   function GetLcn(T: Byte; P: PExtent; var B, E: UInt64): Byte; begin
    if (LEE(P) < LBE(Ex)) or (LEE(Ex) < LBE(P)) then begin
     B := 1; E := 0
    end else begin B := LBE(P); E := LEE(P) end; Result := T
   end;
   function AVE: UInt64; begin
    if Length(AvaVcn) = 0 then Result := 0 else
     with AvaVcn[Length(AvaVcn)-1] do Result := LBE(Ex) + LowPart + HighPart - 1
   end;
   procedure AddVal(var B, E: UInt64); var V, S: Integer; begin
    V := Length(AvaVcn);
    if AVE + 1 < B then begin
     SetLength(AvaVcn, V + 1); S := E - B + 1;
     SetLargeInteger(AvaVcn[V], S, B - LBE(Ex));
     if MS < S then MS := S
    end else if AVE < E then begin // <- files layout data error case, adjust:
     Dec(V); S := E - AVE; Inc(AvaVcn[V].HighPart, S);
     if MS < AvaVcn[V].HighPart then MS := AvaVcn[V].HighPart
    end else S := 0;
    Inc(AvaDatSiz, S); B := 0; E := 0
   end;
  const
   op = [0, 1, 2, 4, 6];
  var
   T, o, n, f: Integer; BL, EL, LB, LE: UInt64; OE, NE: PExtent;
   b: Boolean; begdt: TDateTime;
  begin
   begdt := Now; Result := false; b := BiShEx(FrBks, Ex, f);
   BinSchCI(Ex^,  @OsLays, o, op); OE := ExLID( @OsLays, o);
   BinSchCI(Ex^, @NewLays, n, op); NE := ExLID(@NewLays, n);
   MS := 0; SetLength(AvaVcn, 0); BL := 0; EL := 0; LE := 0;
   while (EL < LEE(Ex)) and ((OE <> nil) or (NE <> nil) or b) do begin
    Result := true; T := 0;
    if b then T := GetLcn(0, @FrBks[f], LB, LE) else LB := HighUInt64;
    if (NE <> nil) and (LBE(NE) < LB) then T := GetLcn(1, NE, LB, LE);
    if (OE <> nil) and (LBE(OE) < LB) then T := GetLcn(2, OE, LB, LE);

    if LB <= LE then
     if EL = 0 then begin
      if LB < LBE(Ex) then BL := LBE(Ex) else BL := LB;
      if LEE(Ex) < LE then EL := LEE(Ex) else EL := LE
     end else if LB <= EL + 1 then
      if LEE(Ex) < LE then EL := LEE(Ex) else begin if EL < LE then EL := LE end
     else
      AddVal(BL, EL);
    case T of 0: Inc(f); 1: Inc(n); 2: Inc(o) end;
    case T of
    0: if f < Length(FrBks) then b := ExIn(FrBks, Ex, f) else b := false;
    1: NE := ExLID(@NewLays, n); 2: OE := ExLID(@OsLays, o)
    end
   end;
   if 0 < EL then AddVal(BL, EL);
   TCrtcProgress.TimeShift := TCrtcProgress.TimeShift + Now - begdt
  end;

  function HciCount(F: PFilesLayoutInformation): Cardinal; begin
   Result := Length(F.HCidx) + Length(F.HCbuf)
  end;
  function CheckRamUse(F: PFilesLayoutInformation;
                  Common: Boolean = true; AMemoryLimit: Cardinal = 0): Boolean;
  begin
   if AMemoryLimit = 0 then AMemoryLimit := MemoryUseLimit;
   if Common then
    Result := AMemoryLimit < TgtMemUse div 1024
                           + (HciCount(@OsLays) + HciCount(@NewLays)) div 64
   else
    Result := AMemoryLimit < TgtMemUse div 1024 + HciCount(F) div 32
  end;

  function GetSourceHashCodesIndexes(b2nd: Boolean;
                                        F: PFilesLayoutInformation;
                              Count, Size: Cardinal;
                                  var ids: TIntegerDynArray): Byte; overload;

  var
   s, i, j: Integer; BDT: TDateTime;
  begin
   Result := 2;
   if (0 < Length(F.SizIdx))
   and(F.Data[F.SizIdx[Length(F.SizIdx) - 1]].ClusCount <= F.SzHC) then exit;
   Result := 1;
   if CheckRamUse(F, b2nd) then exit;

   Result := 4; if not b2nd then SetLength(ids, 0);
   BinSchSI(F.SzHC, F, i, true); if 0 < i then Dec(i) else if i < 0 then i := 0;
   BinSchSI(  Size, F, s, true);
   while i <= s do with F.Data[F.SizIdx[i]] do begin
    if (State and 2 = 2) and (0 = Length(HC)) and (0 < ClusCount) then begin
     BDT := Now;
     if CompressDefragHash(3, hScVol, F, F.SizIdx[i]) in [0, 1] then begin
      j := Length(ids); SetLength(ids, j + 1); ids[j] := F.SizIdx[i]
     end;
     TCrtcProgress.TimeShift := TCrtcProgress.TimeShift + Now - BDT;
     TCrtcProgress.Echo(Count - Length(PExIdx), GetCrtcArr);
     F.SzHC := ClusCount;
     if CheckRamUse(F, b2nd) then begin Result := 3; break end
    end;
    Inc(i)
   end;
   if s + 1 = Length(F.SizIdx) then Result := 2
  end;

  function GetSourceHashCodesIndexes(Count, Size: Cardinal): Boolean; overload;
   function ReportSetOverflow(Subs: String; F: PFilesLayoutInformation): Boolean;
   const
    LimMsg = 'Reached memory limit for %s files, only items smaller %d KB';
   var
    Size: Cardinal;
   begin
    Size := (F.SzHC + 1) * SectorsPerCluster * SrcVolSpec.BytesPerSector div 1024;
    FindFitsMsg(Count - Length(PExIdx), mWarning, Format(LimMsg, [Subs, Size]));
    F.SzHC := HighUInt32; Result := true
   end;
  label exi; var
   NLres, OLres: Byte; oids, nids: TIntegerDynArray; Shft, BgDT: TDateTime;
  begin
   Shft := TCrtcProgress.TimeShift; BgDT := Now; Result := true;
   if(Size <= NewLays.SzHC) or (0 = Length(NewLays.SizIdx)) then NLres := 0 else
    NLres := GetSourceHashCodesIndexes(false, @NewLays, Count, Size, nids);
   if(Size <= OsLays .SzHC) or (0 = Length(OsLays .SizIdx)) then OLres := 0 else
    OLres := GetSourceHashCodesIndexes(false,  @OsLays, Count, Size, oids);
   if 0 < NLres + OLres then UpdateJoinFrBks(FrBks, FrBks);
   if (NLres in [0, 2]) and (OLres in [0, 2]) then goto exi;

   if (NLres = 2) and (OLres in [1, 3]) then
    OLres := GetSourceHashCodesIndexes(true,  @OsLays, Count, Size, oids);
   if (OLres = 2) and (NLres in [1, 3]) then
    NLres := GetSourceHashCodesIndexes(true, @NewLays, Count, Size, nids);

   with NewLays do if bHCini and (0 < Length(HCidx)) or (0 < Length(HCbuf)) then
    HashCodes(0, @NewLays, 0, hcIndex);
   with  OsLays do if bHCini and (0 < Length(HCidx)) or (0 < Length(HCbuf)) then
    HashCodes(0,  @OsLays, 0, hcIndex);

   Result := (NLres in [0, 2, 4]) and (OLres in [0, 2, 4]);
   if (NLres in [1, 2, 3]) and (OLres in [1, 3]) then
    Result := ReportSetOverflow('old',  @OsLays);
   if (OLres in [1, 2, 3]) and (NLres in [1, 3]) then
    Result := ReportSetOverflow('new', @NewLays);
  exi:
   if 0 < Length(nids) then RebuildIndex(1, @NewLays, nids, true, false);
   if 0 < Length(oids) then RebuildIndex(2,  @OsLays, oids, true, false);
   TCrtcProgress.TimeShift := Shft + Now - BgDT
  end;
  procedure Find(var Count: Cardinal); overload;
  var
   i, j, MUS: Integer; dt: TDateTime;
  begin
   try
    if Length(FrBks) = 0 then begin
     dt := Now;
     TCrtcProgress.Echo(0, GetCrtcArr);
     GetFreeBlocks(hScVol, FrBks);
     TCrtcProgress.TimeShift := TCrtcProgress.TimeShift + Now - dt
    end;

    for i := 0 to Length(TgBks) - 1 do begin
     if CheckHasAvailableFragments(@TgBks[i], MUS) then begin
      dt := Now;
      if GetTargetHashCodes(@TgBks[i]) then
       TgtMemUse := TgtMemUse + sizeof(UINT64) * Length(TgBks[i].Chc);
      if CheckRamUse(nil, true, RamOverflowLimit) and EchoFailure(25) then break;
      TCrtcProgress.TimeShift := TCrtcProgress.TimeShift + Now - dt;

      if 0 < Length(TgBks[i].Chc) then begin
       PExDon := GetSourceHashCodesIndexes(Count, MUS);
       for j := 0 to Length(AvaVcn) - 1 do with AvaVcn[j] do
        CheckGetBlockFits(@TgBks[i], LowPart, HighPart)
      end
     end;
     Inc(Count);
     TCrtcProgress.Echo(Count - Length(PExIdx), GetCrtcArr)
    end
   except
    on E : Exception do begin
     TCrtcProgress.Finalize;
     OutputMessage(mInternalErr, 'Failed with exception: ' + E.Message);
     OutputSysError(true)
    end
   end
  end;
  function DropVolatileData(var cMchDrp: Cardinal;
                         Typ: Byte; FL: PFilesLayoutInformation): Cardinal; overload;
  const
   cStage = 'MATCHING DATA';
  var
   i, j, k: Integer; b: Boolean; Es: TExtents; idx: TIntegerDynArray;
  begin
   for i := 0 to Length(FL.SizIdx) - 1 do begin
    j := FL.SizIdx[i];
    if 0 < Length(FL.Data[j].HC) then begin
     GetFileClusters(SrcVolSpec.GUID, FL.Data[j].FName, Es);
     b := false;
     if Length(Es) <> 1 then begin
      FL.Data[j].State := FL.Data[j].State or 64;
      b := true
     end else with FL.Data[j].Extents[0] do
      if (Lcn.QuadPart <> Es[0].Lcn.QuadPart) or (Vcs <> Es[0].Vcs) then begin
       if Es[0].Vcs <> Vcs then FL.Data[j].State := FL.Data[j].State or 64;
       b := true
      end;
     if b then begin
      SetLength(FL.Data[j].Extents, Length(Es));
      for k := 0 to Length(Es) - 1 do with FL.Data[j].Extents[k] do begin
       Lcn.QuadPart := Es[k].Lcn.QuadPart; Vcs := Es[k].Vcs
      end;
      SetReIndex(idx, j);
      if (FL.Data[j].State and 64 = 64) and (0 < Length(Es)) then
       ReportDataModified(cStage, FL.Data[j].FName, teVltDmgd)
     end
    end
   end;
   RebuildIndex(Typ, FL, idx);
   Result := SearchDropChangedBgd(idx, CntIndx, cMchDrp, FndFtsSiz, FndFlsSiz,
                               hScVol, Typ, FL);
   for i := 0 to Length(idx) - 1 do
    ReportDataModified(cStage, FL.Data[idx[i]].FName, teVltDmgd)
  end;
  procedure DropVolatileData; overload;
  var
   BgDT: TDateTime; cIdxDrp, cMchDrp: Cardinal;
  begin
   BgDT := Now;
   cMchDrp := 0;
   cIdxDrp := DropVolatileData(cMchDrp, 1, @NewLays)
            + DropVolatileData(cMchDrp, 2, @OsLays);
   FindFitsMsg(Count - Length(PExIdx), mLogHighPri,
               SelNSt(cIdxDrp, 'file', 'Dropped volatile index of ', '',
                               'Data check passed') +
               SelNSt(cMchDrp, 'file', ' & matches of '));
   TCrtcProgress.TimeShift := TCrtcProgress.TimeShift + Now - BgDT
  end;
 var
  Cnt: Cardinal; FdFtSz: UINT64; i, j: Integer;
 begin
  try
   Inc(SubTasksNum);
   Count := JoinExtents(TgBks, @DelLays, @OtLays, @GetExtTgBks, @ExtRngTgBks);
   Result := false;
   DropAllFitsData;
   TCrtcProgress.Initialize('search source files matching target fragments',
                            'search of matches',
                            ['Fitted fraction of usable target data',
                            'Total size of found matches',
                            'Number of fitted files',
                            'Total size of fitted files',
                            'Memory usage by target data',
                            'Number of defragmented source files',
                            'Number of indexed source files',
                            'Memory usage by source data',
                            'Failed to defragment'],
             ['%', 'Kbytes', '', 'Kbytes', 'Kbytes', '', '', 'Kbytes', 'files'],
             Count, 0, '', Byte(mcSub2Cat));
   Cnt := 0; AvaDatSiz := 0; FndFtsSiz := 0; FndFlsSiz := 0; TgtMemUse := 0;
   OutputMessage(mEcoNoDtHP, 'Notes:<align>The time assessment discounts data preparation. Collating %srs',
                             [SelStr(bClusterWise, 'cluste', 'secto')]);
   Find(Cnt);
   SetLength(AvaVcn, 0); SetLength(FrBks, 0);

   if 0 < TCrtcProgress.TimeShift + SchTim then
    FindFitsMsg(Count - Length(PExIdx), mLogNoDate,
           '<align>Spent ' + SSeconds(TCrtcProgress.TimeShift) +
           ' sec on data acquisition, ' + SSeconds(SchTim) +
           ' sec on making comparisons');
   FindFitsMsg(Count - Length(PExIdx), mEcoHighPri,
         'Prepared auxiliary data, check it''s valid & finalize search...');
   DropVolatileData;

   Cnt := Length(PExIdx); FdFtSz := FndFtsSiz;
   while 0 < Length(PExIdx) do begin
    Randomize;
    i := PExIdx[Random(Length(PExIdx))];

    PExDon := true;
    for j := 0 to Length(AvaPEx[IAvPEx[i]]) - 1 do
     with AvaPEx[IAvPEx[i]][j] do
      CheckGetBlockFits(FitPEx[i], LowPart, HighPart);

    if not bClusterWise then
     if 0 < Cnt then Dec(Cnt) else begin
      if 100 * (FndFtsSiz - FdFtSz) div FdFtSz = 0 then begin
       FindFitsMsg(Count - Length(PExIdx), mEcoNoDtHP,
             '<align>Low rate of revealing new matches, terminated as a good result');
       SetLength(PExIdx, 0);
       break
      end;
      Cnt := Length(PExIdx); FdFtSz := FndFtsSiz
     end;
    TCrtcProgress.Echo(Count - Length(PExIdx), GetCrtcArr)
   end;

   DropAllHashData(@NewLays, @OsLays);
   for i := 0 to Length(TgBks) - 1 do SetLength(TgBks[i].Chc, 0);
   OutputMessage(mEcoNoDtHP, '<align>Dropped auxiliary data to free memory', mcSub2Cat);

   TCrtcProgress.RemoveSummaryStrings([4, 6, 7]);
   TCrtcProgress.Summary('MOVED DATA SEARCH SUMMARY:', [ADUPercent,
                         FndFtsSiz * CodSzToBt div 1024,
                         Length(IdxIdx),
                         FndFlsSiz * CodSzToBt div 1024, CntDefr, CntDfrF]);
   Result := true;
   Sleep(25)
  except
   on E : Exception do begin
    TCrtcProgress.Finalize;
    OutputMessage(mInternalErr, 'Failed with exception: ' + E.Message);
    OutputSysError(true)
   end
  end
 end;

 procedure ApplyMatches;
 const
  opts = [0, 1, 2, 4, 6];
 var
  FndFits, CntMovd, CntBlkd: UInt32; idxn, idxo: TIntegerDynArray; bid: Boolean;

  function IsBlockUsed(BL: UINT64;
                       RC: Cardinal; var UTyp: Byte; var idx: Integer): Boolean;
  var
   i: Integer;
  begin
   Result := BinSchCI(BL, RC, @NewLays, idx, opts);
   if Result then begin UTyp := 1; exit end;
   Result := BinSchCI(BL, RC,  @OsLays, idx, opts);
   if Result then begin UTyp := 2; exit end else UTyp := 0;
   Result := not BiShEx(FrBks, BL, i, true, RC)
  end;
  function FileLayout(idx: Integer): PFileLayoutInformation;
  begin
   if 0 <= FitInd[idx] then Result := @OsLays.Data[FitInd[idx]] else
    Result := @NewLays.Data[- 1 - FitInd[idx]]
  end;
  function IsFileInPlace(idx: Integer): Boolean;
  var
   FN, RC: Cardinal; Lcn: UINT64;
  begin
   AnalyzeFile(SrcVolSpec.GUID + FileLayout(idx).FName, FN, RC, Lcn);
   Result := (FN = 1) and (Lcn = FitPEx[IdxPEx[idx]].Lcn.QuadPart
                               + FitBeg[idx] div ClusterInCodSizes)
  end;
  procedure MoveFrBks(Moved: Byte; F: PFileLayoutInformation; BL: UINT64; RC: Cardinal);
  begin
   case Moved of
   0: MoveFreeBlok(F, BL, RC);
   5: if Length(F.Extents) = 1 then MoveFreeBlok(F, BL, RC) else
       GetFreeBlocks(hScVol, FrBks)
   end
  end;

  function CleanBlock(Typ: Byte; SF: PFilesLayoutInformation;
                       ID: Integer; NFL: UINT64): Boolean;
  var
   L, BL, CL, EL: UInt64; MovRes: Int32; RC: Cardinal;
  begin
   L := UInt64(MaxLCN); Result := true;
   repeat
    BL := L; CL := 0; RC := SF.Data[ID].ClusCount;
    with SF.Data[ID] do if FindFreeBlok(BL, RC, BL, EL) then begin
     MovRes := MoveToLcn(hScVol, BL, SrcVolSpec.GUID + FName, RC, CL);
     if MovRes in [0, 5] then begin
      MoveFrBks(MovRes, @SF.Data[ID], BL, RC);
      DropIndexSeS(SF, ID); Inc(CntClnd);
      if Typ = 1 then SetReIndex(idxn, ID) else SetReIndex(idxo, ID);
      break
     end else if L = UInt64(MaxLCN) then begin
      GetFreeBlocks(hScVol, FrBks);
      L := NFL
     end else begin DropIndexSeS(SF, ID, -1, 4); Inc(CntClnF); exit end
    end else if L = UInt64(MaxLCN) then
     L := NFL
    else begin bid := true; exit end // Too much fragmented, delay
   until false;
   if L = NFL then bid := true; Result := false
  end;

  function CheckFreeBlock(Typ: Byte; ID: Integer; BL: UINT64; RC: Cardinal): Byte;
  var
   idx: Integer; UTyp: Byte;
  begin
   Result := 0;
   while IsBlockUsed(BL, RC, UTyp, idx) do
    case UTyp of
    0: begin Inc(CntBlkd); exit end;
    1: with NewLays.LcnIdx[idx] do
        if (Typ = 1) and (ID = HighPart)
                     and (NewLays.Data[HighPart].Extents[0].Lcn.QuadPart = BL)
                     and (NewLays.Data[HighPart].Extents[0].Vcs = RC) then
        begin
         Result := 2; exit
        end else
         if CleanBlock(1, @NewLays, HighPart, BL + RC) then exit;
    2: with OsLays.LcnIdx[idx] do
        if (Typ = 2) and (ID = HighPart)
                     and (OsLays.Data[HighPart].Extents[0].Lcn.QuadPart = BL)
                     and (OsLays.Data[HighPart].Extents[0].Vcs = RC) then
        begin
         Result := 2; exit
        end else
         if CleanBlock(2, @OsLays, HighPart, BL + RC) then exit
    end;
   Result := 1
  end;

  function HasOtherMatches(Typ: Byte; ID: Integer; BL: UINT64; RC: Cardinal): Boolean;
   function _(var Res: Boolean; F: PFilesLayoutInformation; Tp: Byte = 1): Byte;
   var
    i, j, k: Integer;
   begin
    Result := 0; if Typ = 1 then i := - 1 - ID else i := ID;
    if BinSchCI(BL, RC, F, j, opts) then
     while (j < Length(F.LcnIdx)) and (InBlock(BL, RC, ExtLID(F, j)) in opts) do
     begin
      if (Typ = Tp) and (F.LcnIdx[j].HighPart = ID) then begin
       Res := false; Result := 1; exit
      end;
      if BinSchIdx(i, k) and (Result = 0) then begin
       Res := true; Result := 2; if Typ = Tp then Inc(Result) else exit
      end;
      Inc(j)
     end
   end;
  begin
   Result := false; if _(Result, @NewLays) in [0, 2] then _(Result, @OsLays, 2)
  end;

  function FillBlockByFits(bHOM: Boolean; Typ: Byte; F: PFilesLayoutInformation;
                             ID: Integer; BL: UINT64; RC: Cardinal;
                        var idx: TIntegerDynArray): Byte;
   procedure _SetDoneExcludeId(LB: UINT64; CR: Cardinal; Moved: Byte = 255);
   var
    i, j: Integer;
   begin
    i := 0;
    for j := 0 to Length(idx) - 1 do
     if idx[j] = ID then Inc(i) else if 0 < i then idx[j - i] := idx[j];
    if 0 < i then SetLength(idx, Length(idx) - i);

    MoveFrBks(Moved, @F.Data[ID], LB, CR); DropIndexSeS(F, ID, -1, 1);
    Result := 2; ChgExpSize(RC); Inc(CntDone); Inc(CntMovd)
   end;
  var
   MovRes: Integer; LB: UINT64; CR: Cardinal;
  begin
   if bHOM and HasOtherMatches(Typ, ID, BL, RC) then begin Result := 1; exit end;
   Result := 0;
   case CheckFreeBlock(Typ, ID, BL, RC) of
   1: begin
     LB := 0; CR := RC;
     MovRes := MoveToLcn(hScVol, BL, SrcVolSpec.GUID + F.Data[ID].FName, CR, LB, true);
     if MovRes in [0, 5] then begin
      if BL = LB then
       if (95 * RC div 100 <= CR) and (CR <= 105 * RC div 100) then begin
        _SetDoneExcludeId(LB, CR, MovRes); exit
       end else begin
        RebuildIndex(0, F, ID, -1, false); Result := 3
       end
      else
       RebuildIndex(Typ, F, ID, -1, false)
     end else if MovRes = 6 then begin
      RebuildIndex(0, F, ID, -1, false); Result := 3
     end;
     Inc(CntMovF); SetState(F.Data[ID], 4)
    end;
   2: _SetDoneExcludeId(BL, RC)
   end
  end;
  function GetCrtcArr : TExtendedDynArray;
  begin
   Result := CrtcArr([CntMovd, FndFits * CodSzToBt div 1024, CntMovF, CntClnd,
                      CntClnF, ExpSize * CluSzToBt div 1048576])
  end;
  function ApplyMatch(bHOM: Boolean;
                  var cHOM: Cardinal; var bDrp: Boolean; idx: Integer): Boolean;
  var
   i: Integer; Res: Byte;
  begin
   if bid then begin
    RebuildIndex(1, @NewLays, idxn); RebuildIndex(2, @OsLays, idxo); bid := false
   end;
   i := IdxPEx[idx];

   if 0 <= FitInd[idx] then
    Res := FillBlockByFits(bHOM, 2, @OsLays, FitInd[idx],
        FitPEx[i].Lcn.QuadPart + FitBeg[idx] div ClusterInCodSizes,
                                 FitSiz[idx] div ClusterInCodSizes, idxo)
   else
    Res := FillBlockByFits(bHOM, 1, @NewLays, -1 - FitInd[idx],
        FitPEx[i].Lcn.QuadPart + FitBeg[idx] div ClusterInCodSizes,
                                 FitSiz[idx] div ClusterInCodSizes, idxn);
   Result := Res = 2; bDrp := Res = 3;
   if Result then begin
    ChgExpSize(FitCnt[idx] * CodSzToBt div CluSzToBt);
    FndFits := FndFits + FitCnt[idx]
   end else if Res = 1 then Inc(cHOM);
   TCrtcProgress.Echo(CntMovd, GetCrtcArr)
  end;

 var
   VltFileCnt, VltFtsSiz, VltFlsSiz : Cardinal;
  procedure DropMdfFitData(idx: Integer; var r: TBooleanDynArray);
  begin
   Inc(VltFileCnt); Inc(VltFtsSiz, FitCnt[idx]); Inc(VltFlsSiz, FitSiz[idx]);
   ReportDataModified('MATCHED DATA', FileLayout(idx).FName, teVltDmgd);
   DropFitData(idx, r)
  end;
  procedure UpdateIndex(Typ: Byte; SF: PFilesLayoutInformation;
                      var r: TBooleanDynArray; var x: TIntegerDynArray);
  var
   i, j: Integer; S: String;
  begin
   S := ';';
   if 0 < Length(x) then begin
    for i := 0 to Length(x) - 1 do S := S + IntToStr(x[i]) + ';';
    RebuildIndex(Typ, SF, x)
   end;
   for i := 0 to Length(SF.Data) - 1 do
    if (0 = SF.Data[i].State and 1) and (0 = Pos(Format(';%d;', [i]), S)) then
     SetReindex(x, i);

   for i := 0 to Length(FitInd) - 1 do if r[i] then begin
    if Typ = 1 then
     if FitInd[i] < 0 then j := - 1 - FitInd[i] else continue
    else
     if FitInd[i] < 0 then continue else j := FitInd[i];
    if not IsFileInPlace(i) then begin
     SetState(SF.Data[j], 2); SetReindex(x, j); Dec(CntDone); Dec(CntMovd);
     ChgExpSize(FitCnt[i] * CodSzToBt div CluSzToBt, false);
     FndFits := FndFits - FitCnt[i];
     ReportDataModified('MATCHED DATA', FileLayout(i).FName);
     r[i] := false
    end
   end;
   RebuildIndex(Typ, SF, x);
   i := 0;
   repeat
    if FileLayout(i).State and 64 = 0 then Inc(i) else DropMdfFitData(i, r)
   until i = Length(FitInd);
  end;
 var
  HOM: array[0..2] of UInt32; r: TBooleanDynArray; f: TIntegerDynArray;
  i, j, k: Int32; b, d: Boolean;
 begin
  try
   Inc(SubTasksNum);
   Result := false;
   if Length(FitInd) = 0 then begin DropAllFitsData; exit end;
   i := Length(FitInd);
   TCrtcProgress.Initialize('move matched files to fit data of target blocks',
                            'matched files fitting',
                            ['Moved in front of target blocks',
                            'Sizes sum of all fitted matches',
                            'Amount of moves to target failures',
                            'Moved to clean blocks',
                            'Failed move to clean blocks',
                            'Upper limit for snapshot growth'],
                       ['files', 'Kbytes', 'files', 'files', 'files', 'Mbytes'],
                       i, 0, '', Byte(mcSub2Cat));
   FndFits := 0; VltFileCnt := 0; VltFtsSiz := 0; VltFlsSiz := 0;
   CntMovd := 0; CntBlkd := 0; CntMovF := 0; CntClnd := 0; CntClnF := 0;
   SetLength(idxn, 0); SetLength(idxo, 0);
   for j := 0 to 2 do HOM[j] := 0; SetLength(r, i); SetLength(f, 0);
   bid := false; Dec(i); GetFreeBlocks(hScVol, FrBks);

   for j := 0 to i do begin
    r[j] := ApplyMatch(true, HOM[1], d, j);
    if d then DropMdfFitData(j, r) else if HOM[2] < HOM[1] then begin
     HOM[2] := HOM[1]; k := Length(f); SetLength(f, k + 1); f[k] := j
    end
   end;
   i := Length(f); b := true;
   while 0 < i do begin
    Randomize; j := Random(i);
    if not b then b := true else
     if HOM[0] + i < HOM[1] then begin b := false; HOM[0] := HOM[1] end;
    r[f[j]] := ApplyMatch(b, HOM[1], d, f[j]);
    
    if d then DropMdfFitData(f[j], r);
    
    if (HOM[1] = HOM[2]) or d then begin
     Dec(i); for k := j to i - 1 do f[k] := f[k + 1]; SetLength(f, i)
    end else HOM[2] := HOM[1]
   end;
   OutputMessage(mEcoHighPri, 'Secondary verification scans...');
   TCrtcProgress.Echo(CntMovd, GetCrtcArr, false);
   j := 0;
   repeat
    k := CntDone;
    CntBlkd := 0; GetFreeBlocks(hScVol, FrBks);
    UpdateIndex(1, @NewLays, r, idxn); UpdateIndex(2, @OsLays, r, idxo);
    i := 0;
    repeat
     if r[i] then Inc(i) else
     begin
      r[i] := ApplyMatch(false, HOM[1], d, i);

      if d then DropMdfFitData(i, r) else Inc(i)
     end
    until i = Length(FitInd);
    Inc(j)
   until CntDone = k;
   OutputMessage(mLogNoDate, '<align>Finished after %d scans', [j]);

   DropAllFitsData;
   if 0 < CntBlkd then
    OutputMessage(mLogNoDate,
            '<align>The system data blocked relocation of %d files', [CntBlkd]);
   if 0 < VltFileCnt then
    OutputMessage(mLogNoDate, '<align>Dropped %d volatile files [%d/%d KB]',
                  [VltFileCnt, VltFtsSiz, VltFlsSiz]);

   RebuildIndex(1, @NewLays, idxn); RebuildIndex(2, @OsLays, idxo);
   TCrtcProgress.Summary('MATCHED DATA RELOCATION SUMMARY:', GetCrtcArr);
   Result := true
  except
   on E : Exception do begin
    TCrtcProgress.Finalize;
    OutputMessage(mInternalErr, 'Failed with exception: ' + E.Message);
    OutputSysError(true)
   end
  end
 end;
begin
 FindMatches; if Result then ApplyMatches else Inc(SubTasksNum)
end; // FindMatchesFitInBlocks

// ****************************************************************************
// Moving files in front of target data blocks.

function LayoutInBlocks(SF: PFileLayoutInformation): Boolean;
const
 opts = [0, 6];
var
 i, j: Integer;
begin
 Result := false;
 for i := 0 to Length(SF.Extents) - 1 do
  if  not BinSchCI(SF.Extents[i], @DelLays, j, opts)
  and not BinSchCI(SF.Extents[i], @OtLays, j, opts) then
   exit;
 Result := true
end;

procedure CleanBlock(Typ: Byte; hScVol: THandle;
                      TF: PFilesLayoutInformation; TI: LARGE_INTEGER;
                      SF: PFilesLayoutInformation);
const
 opts = [0, 1, 2, 4, 6];
var
 idx: TIntegerDynArray; MR: Integer;
 i, j, k: Integer; BL, CL, EL: UInt64; Attempt, RC: Cardinal; bcl, bri: Boolean;
 function LAfterTgBk: UInt64;
 begin
  with ExtLID(TF, TI)^ do Result := Lcn.QuadPart + Vcs + 1;
 end;
 procedure MoveFrBks(F: PFileLayoutInformation; BL: UINT64; RC: Cardinal);
 begin
  case MR of
  0: MoveFreeBlok(F, BL, RC);
  5: if Length(F.Extents) = 1 then MoveFreeBlok(F, BL, RC) else
      GetFreeBlocks(hScVol, FrBks)
  end
 end;
begin
 if BinSchCI(TF, TI, SF, i, opts) then begin
  bcl := true;
  while i < Length(SF.LcnIdx) do begin
   j := SF.LcnIdx[i].HighPart;
   case InBlock(TF, TI, SF, SF.LcnIdx[i], true) of
   0, 1: if IsState(SF, j, 4) or bCompressData and IsState(SF, j, 16) then
          bcl := true
         else
          bcl := not LayoutInBlocks(@SF.Data[j]);
   2, 4, 6: bcl := true
   else
    break
   end;

   if bcl then with SF.Data[j] do begin
    bri := false; Attempt := 0;
    repeat
     RC := ClusCount;
     case Attempt of
      0: BL := UINT64(MaxLCN); 1: BL := LAfterTgBk; 2: BL := MinLcnForFile(RC);
      3: begin Inc(CntClnF); Inc(i); break end // Too much fragmented, next time
     end;
     if  FindFreeBlok(BL, RC, BL, EL)
     and(InBlock(BL, EL, ExtLID(TF, TI)) in [3, 5]) then begin
      bri := true; CL := 0;
      MR := MoveToLcn(hScVol, BL, SrcVolSpec.GUID + FName, RC, CL);
      if(MR in [0, 5])and(InBlock(CL, CL + RC - 1, ExtLID(TF, TI)) in [3, 5])then
      begin
       MoveFrBks(@SF.Data[j], BL, RC);
       DropIndexSeS(SF, j, i, 2); Inc(CntClnd); break
      end else if 0 < Attempt then begin // Unmovable, exclude.
       GetFreeBlocks(hScVol, FrBks);
       DropIndexSeS(SF, j, i, 4); Inc(CntClnF); Inc(i); break
      end
     end;
     Inc(Attempt)
    until false;
    if bri then begin k := Length(idx); SetLength(idx, k + 1); idx[k] := j end
   end else begin
    DropIndexSeS(SF, j, i, 8); ChgExpSize(SF, j); Inc(CntDone)
   end
  end;
  RebuildIndex(Typ, SF, idx)
 end
end;

function FillSubBlock(Typ: Byte; hScVol: THandle; SF: PFilesLayoutInformation;
                       BL: ULONG64; F: Cardinal; var MinSiz: Cardinal): Boolean;
var
 i, j, k, m: Int32; S, R: UInt32; EL, L: ULONG64; id: TIntegerDynArray; b: Boolean;

 function CheckMinSize: Boolean;
 begin
  if Length(SF.SizIdx) = 0 then MinSiz := 0 else
   MinSiz := SF.Data[SF.SizIdx[0]].ClusCount;
  Result := (0 < MinSiz) and (MinSiz <= F)
 end;
 procedure SetMoved(bUp: Boolean = true); begin
  F := F - R; BL := BL + R; ChgExpSize(R);
  DropIndexSeS(SF, k, -1, 8); Inc(CntDone);
  if bUp then
   GetFileClusters(SrcVolSpec.GUID, SF.Data[k].FName, SF.Data[k].Extents)
 end;
 function SegmentChanged: Boolean;
 var
  i: Integer; FB: TExtents;
 begin
  Result := 0 < F; GetFreeBlocks(hScVol, FB, BL, EL); F := 0; b := false;
  for i := 0 to Length(FB) - 1 do with FB[i] do if MinSiz <= Vcs then begin
   Result := (Lcn.QuadPart <> BL) or (Lcn.QuadPart + Vcs <> EL + 1);
   F := Vcs; BL := Lcn.QuadPart;
   break
  end;
  if F = 0 then BL := EL
 end;
begin
 Result := CheckMinSize; if Result then EL := BL + F - 1 else exit;
 repeat
  b := true; j := 1;
  while CheckMinSize do begin
   j := 1; // 3 attempts to move different files.
   repeat
    if Typ < 3 then S := F else begin // Scales down size to select small files:
     if 8 < F then
      if MinSiz < F then S := F div Trunc(Ln(1 + F/MinSiz)/Ln(2)) else S := F
     else
      S := F;
     if MinSiz < S then begin Randomize; S := MinSiz + Random(S - MinSiz) end
    end;
    BinSchSI(S, SF, i, true);
    repeat
     while (0 <= i) and (0 = SF.Data[SF.SizIdx[i]].State and 80)
         and LayoutInBlocks(@SF.Data[SF.SizIdx[i]]) do begin
      ChgExpSize(SF, SF.SizIdx[i]); DropIndexSeS(SF, SF.SizIdx[i], -1, 8);
      Inc(CntDone); Dec(i)
     end;
     if (0 <= i) and (64 = SF.Data[SF.SizIdx[i]].State and 64) then begin
      Dec(i); continue
     end
    until (i < 0) or (SF.Data[SF.SizIdx[i]].State and 64 = 0);

    Result := CheckMinSize; if not Result or (i < 0) then exit;
    k := SF.SizIdx[i];
    with SF.Data[k] do begin
     if bCompressData and (State and 16 = 16) then begin
      L := BL;
      if (CompressDefragHash(Typ, hScVol, SF, k, L, R) = 1) and (BL = L) then
      begin
       SetMoved(false); break
      end
     end;

     L := 0; R := ClusCount;
     case MoveToLcn(hScVol, BL, SrcVolSpec.GUID + FName, R, L, true) of
     0: if (BL = L) and (L + R <= EL + 1) then begin
         SetMoved; break
        end else begin
         m := Length(id); SetLength(id, m + 1); id[m] := k; Inc(j);
         if SegmentChanged then break
        end;
     5: begin // Undefraggable, if the 1st fragment is in place then ok.
         if BL = L then begin
          RebuildIndex(Typ, SF, k, -1, true); R := Extents[0].Vcs;
          SetMoved(false)
         end else begin
          m := Length(id); SetLength(id, m + 1); id[m] := k; Inc(j)
         end;
         SegmentChanged; break
     end else if SegmentChanged then break else begin
         Inc(j); Inc(CntMovF); DropIndexSeS(SF, k, -1, 4)
     end end;
    end;
    Dec(F)
   until (F < MinSiz) or (j = 4);
   if 1 < j then Result := false; if (F < MinSiz) or (j = 4) then break
  end;
  if ((j = 4) or b and (BL + F < EL)) and not SegmentChanged then break
 until (EL < BL) or (F < MinSiz) or (Length(SF.SizIdx) = 0);
 if 0 < Length(id) then begin Inc(CntMovF); RebuildIndex(Typ, SF, id) end
end;

function FillBlock(Typ: Byte; hScVol: THandle; ET: PExtent;
                    SF: PFilesLayoutInformation;
            var MinSiz: Cardinal): Boolean; overload;

 function MaxFreeBlock(LB, LE: ULONG64; var BL, EL: ULONG64) : Cardinal;
 begin
  BL := LB; EL := LE; Result := MaxFreeBlockOfFragment(hScVol, BL, EL)
 end;
 procedure FillSegment(LB, LE: ULONG64);
 var
  BS: Cardinal; BL, EL: ULONG64;
 begin
  if 0 < MinSiz then begin
   BS := MaxFreeBlock(LB, LE, BL, EL);
   if MinSiz <= BS then begin
    if FillSubBlock(Typ, hScVol, SF, BL, BS, MinSiz) then begin
     if EL < LE then FillSegment(EL + 1, LE);
     if LB < BL then FillSegment(LB, BL - 1)
    end else
     Result := false
   end
  end
 end;
begin
 Result := true; FillSegment(LBE(ET), LEE(ET))
end;

procedure UseClustersOfTgtFiles(Typ: Byte;
                             hScVol: THandle;
                             TF, SF: PFilesLayoutInformation;
                            var Cnt: Cardinal;
                                OCD: Cardinal = HighUInt32;
                               bEnd: Boolean = false);

 function GetCrtCntArr(CntCons: Cardinal = 0): TExtendedDynArray;
 begin
  if CntCons = 0 then
   if bCompressData then begin
    Result := CrtcArr([CntDone, CntMovF, CntDefr, CntDfrF, CntCmpr, CntCmpF,
                      CntClnd, CntClnF, ExpSize * CluSzToBt div 1048576])
   end else
    Result := CrtcArr([CntDone, CntMovF, CntDefr, CntDfrF,
                      CntClnd, CntClnF, ExpSize * CluSzToBt div 1048576])
  else
   if bCompressData then begin
    Result := CrtcArr([CntDone, CntMovF, CntCons, CntDefr, CntDfrF, CntCmpr, CntCmpF,
                      CntClnd, CntClnF, ExpSize * CluSzToBt div 1048576])
   end else
    Result := CrtcArr([CntDone, CntMovF, CntCons, CntDefr, CntDfrF,
                      CntClnd, CntClnF, ExpSize * CluSzToBt div 1048576])
 end;
 procedure ScanReviveIdx(Typ: Byte; SF: PFilesLayoutInformation);
 var
  ix : TIntegerDynArray; i, j : Integer; Es: TExtents; R: Cardinal; b: Boolean;
 begin
  ReIndexCleanLI(SF);
  ReIndexCleanSI(SF);
  for i := 0 to Length(SF.Data) - 1 do begin
   if IsState(SF.Data[i], 4) then
    b := true
   else if IsState(SF.Data[i], 8) then with SF.Data[i] do begin
    R := GetFileClusters(SrcVolSpec.GUID, FName, Es);
    if Length(Extents) = Length(Es) then begin
     b := false;
     for j := 0 to Length(Es) - 1 do with Extents[j] do begin
      if (UINT64(Lcn) <> UINT64(Es[j].Lcn)) or (Vcs <> Es[j].Vcs) then begin
       b := true; break
      end
     end
    end else b := true;
    if b then begin ChgExpSize(R, false); Dec(CntDone); Inc(CntMovF);
     ReportDataModified('MOVE TO BLOCKS', FName)
    end
   end else b := false;

   if b then begin
    SetState(SF.Data[i], 2); j := Length(ix); SetLength(ix, j + 1); ix[j] := i
   end
  end;
  RebuildIndex(Typ, SF, ix)
 end;
 function Summary(bFin: Boolean = false; CntCons: Cardinal = 0): Boolean;
 begin
  Result := bEnd;
  if bEnd and (bFin or (Length(DelLays.LcnIdx) + Length(OtLays.LcnIdx) +
                        Length(NewLays.LcnIdx) + Length(OsLays.LcnIdx) = 0))then
  begin
   Result := false;
   TCrtcProgress.Summary('SOURCE DATA RELOCATION SUMMARY:', GetCrtCntArr(CntCons))
  end;
 end;
label Fin; var MinSiz, CntCons: Cardinal; i: Integer; begin
 try
  if Length(TF.LcnIdx) = 0 then if Summary then goto Fin else exit;
  if 0 < OCD then begin ScanReviveIdx(2, @OsLays); ScanReviveIdx(1, @NewLays) end;
  if Length(SF.SizIdx) = 0 then if Summary then goto Fin else exit;
  GetFreeBlocks(hScVol, FrBks);
  MinSiz := 1; i := 0; CntCons := 0;
  while i < Length(TF.LcnIdx) do begin
   if Typ < 3 then Inc(Cnt);
   with TF.Data[TF.LcnIdx[i].HighPart] do
    if State and 1 = 1 then
     DropLcnIndex(TF, -1, i, true, true)
    else begin
     if (TF.LcnIdx[i].LowPart = 0) and (State and 8 = 8) then begin
      Inc(Cnt); SetState(TF.Data[TF.LcnIdx[i].HighPart], 2)
     end;
     if (0 < MinSiz) and (MinSiz <= ExtLID(TF, i).Vcs) then begin
      CleanBlock(1, hScVol, TF, TF.LcnIdx[i], @NewLays);
      CleanBlock(2, hScVol, TF, TF.LcnIdx[i],  @OsLays);

      if Typ in [2, 4] then CntCons := Length(SF.LcnIdx);
      if FillBlock(Typ, hScVol, ExtLID(TF, i), SF, MinSiz) then
       if TF.LcnIdx[i].LowPart + 1 = Length(Extents) then begin
        Dec(Cnt); SetState(TF.Data[TF.LcnIdx[i].HighPart], 8);
        if Typ in [2, 4] then Inc(Cnt, CntCons - Length(SF.LcnIdx))
       end
     end;
     Inc(i);
     TCrtcProgress.Echo(Cnt, GetCrtCntArr)
    end
  end;
Fin:
  if bEnd and (OCD <= CntDone) and (CntDone <= OCD + OCD div 100) then begin
   CntCons := 0;
   if 0 < Length(OsLays.LcnIdx) then begin
    ScanReviveIdx(1, @OsLays);
    if Cnt + Length(OsLays.LcnIdx) < TCrtcProgress.EndValue then begin
     i := TCrtcProgress.EndValue - Cnt - Length(OsLays.LcnIdx);
     OCD := 1 + (TCrtcProgress.EndValue - Cnt) div Length(OsLays.LcnIdx)
    end else
     i := 0;
    TCrtcProgress.InsertSummaryString(2, 'Consolidated', 'files');
    OutputMessage(mLogHighPri,
           'Consolidating residuary old modified files of source volume...');
    TCrtcProgress.Echo(Cnt, GetCrtCntArr(CntCons), false);
    repeat
     with OsLays.LcnIdx[0] do begin
      CompressDefragHash(Typ, hScVol, @OsLays, HighPart, bCompressData, false, true);
      DropIndexSeS(@OsLays, HighPart, -1, 8)
     end;
     if 0 < i then begin Inc(Cnt, 1 + OCD); Dec(i, OCD) end else Inc(Cnt);
     Inc(CntCons); TCrtcProgress.Echo(Cnt, GetCrtCntArr(CntCons))
    until Length(OsLays.LcnIdx) = 0
   end;

   if 0 < SizBefCmpr then begin
    SizBefCmpr := 100 * SizAftCmpr div SizBefCmpr;
    OutputMessage(mEcoNoDtHP,
         '<align>The average NTFS compression ratio for current fileset is %d%',
                 [SizBefCmpr]);
    OutputMessage(mLogNoDate,
     '<align>The ModificationThreshold optimal value is in the range [%d..%d]',
                 [1 + SizBefCmpr div 2, SizBefCmpr])
   end;
   Summary(true, CntCons)
  end
 except
  on E : Exception do begin
   TCrtcProgress.Finalize;
   OutputMessage(mInternalErr, 'Failed with exception: ' + E.Message);
   OutputSysError(true)
  end
 end;
end;

procedure MoveToBlocks(hScVol: THandle);
 procedure CrtInitialize;
 var
  asl, asr: TStringDynArray;
  procedure AddRow(sl, sr: String);
  var
   i: Integer;
  begin
   i := Length(asl);
   SetLength(asl, i + 1); asl[i] := sl; SetLength(asr, i + 1); asr[i] := sr
  end;
 begin
  AddRow('Moved to target blocks', 'files');
  AddRow('Failed move to target blocks', 'files');
  AddRow('Defragmented', 'files');
  AddRow('Failed to defragment', 'files');
  if bCompressData then begin
   AddRow('Compressed', 'files'); AddRow('Failed to compress', 'files')
  end;
  AddRow('Moved to clean blocks', 'files');
  AddRow('Failed move to clean blocks', 'files');
  AddRow('Upper limit for snapshot growth', 'Mbytes');

  TCrtcProgress.Initialize
                      ('move source data in front of target blocks',
                      'relocation', asl, asr, Length(OsLays.LcnIdx) + 2 *
                      (Length(DelLays.LcnIdx) + Length(OtLays.LcnIdx)), 0, '',
                      Byte(mcSub2Cat))
 end;
var
 Cnt, CntDon: Cardinal;
begin
 Inc(SubTasksNum);
 CrtInitialize;
 if Length(DelLays.LcnIdx) + Length(OtLays.LcnIdx) = 0 then begin
  OutputMessage(mLogNoDate, cMsgNoData); exit
 end;
 Cnt := 0; CntDone := 0; CntMovF := 0; CntClnd := 0; CntClnF := 0;
 UseClustersOfTgtFiles(2, hScVol, @OtLays , @OsLays , Cnt, 0);
 UseClustersOfTgtFiles(2, hScVol, @DelLays, @OsLays , Cnt);
 UseClustersOfTgtFiles(1, hScVol, @DelLays, @NewLays, Cnt);
 UseClustersOfTgtFiles(1, hScVol, @OtLays , @NewLays, Cnt);
 repeat
  CntDon := CntDone;
  UseClustersOfTgtFiles(4, hScVol, @OtLays , @OsLays , Cnt, 0);
  UseClustersOfTgtFiles(4, hScVol, @DelLays, @OsLays , Cnt, 0);
  UseClustersOfTgtFiles(3, hScVol, @DelLays, @NewLays, Cnt, 0);
  UseClustersOfTgtFiles(3, hScVol, @OtLays , @NewLays, Cnt, CntDon, true)
 until CntDone <= CntDon + CntDon div 100
end;

procedure DropLaysTruncNewFiles(var NewFiles : TStringDynArray;
                                var bDoNTFSC : TBooleanDynArray;
                                var NewFlsSzs: TCardinalDynArray);
var i, j, k, m: Integer; begin
 i := 0; j := 0;
 for k := 0 to Length(NewLays.Data) - 1 do
  if (NewLays.Data[k].State and 6 <> 0) and (0 < NewLays.Data[k].ClusCount) then
   for m := i to Length(NewFiles) - 1 do
    if NewLays.Data[k].FName = NewFiles[m] then begin
     NewFiles[j] := NewFiles[m]; NewFlsSzs[j] := NewFlsSzs[m];
     bDoNTFSC[j] := NewLays.Data[k].State and 16 = 16;
     Inc(j); i := m + 1; break
    end;

 if 0 < j then begin SetLength(NewFiles, j); SetLength(bDoNTFSC, j) end;

 SetLength(DelLays.Data, 0); SetLength(DelLays.LcnIdx, 0);
 SetLength(OtLays .Data, 0); SetLength(OtLays .LcnIdx, 0);
 with OsLays do begin
  SetLength(LcnIdx, 0); SetLength(HCidx, 0); SetLength(Data, 0);
  SetLength(SizIdx, 0); SetLength(HCbuf, 0)
 end;
 with NewLays do begin
  SetLength(LcnIdx, 0); SetLength(HCidx, 0); SetLength(Data, 0);
  SetLength(SizIdx, 0); SetLength(HCbuf, 0)
 end;
 DropAllFitsData;
 Sleep(1000)
end;

{ OptimizePersistentData
  Performs comparison of old files allocation in the source and target volumes,
  moves source data in the front of target location if it gives optimization.
}
function OptimizePersistentData(bMatchBlocks, bMoveToBlocks: Boolean;
                                VolHdl: THandle; NumVolSrc, NumVolTgt: Integer;
                            var DelFiles, OldFiles, NewFiles: TStringDynArray;
                            var bDoNTFSC: TBooleanDynArray;
                            var NewFlsSzs: TCardinalDynArray): Boolean;
label
 exi;
var
 dtbv : TDateTime; CntFls : Cardinal; _ExpSize, DrpClu : UINT64;
begin
 Result := false;
 dtbv := Now;
 OutputMessage(mLogHighPri,
               'Started optimization of persistent data for volume #%d', [NumVolSrc], mcSubCat);
 if VolHdl = INVALID_HANDLE_VALUE then if PrintFailure(SrcVolSpec, 17) then exit;
 if bClusterWise then begin
  CodeSizeInSectors := SrcVolSpec.SectorsPerCluster; ClusterInCodSizes := 1
 end else begin
  CodeSizeInSectors := 1; ClusterInCodSizes := SrcVolSpec.SectorsPerCluster
 end;
 CodSzToBt := CodeSizeInSectors * SrcVolSpec.BytesPerSector;
 CluSzToBt := SectorsPerCluster * SrcVolSpec.BytesPerSector;

 SetPriorityClass(GetCurrentProcess(), PRIORITY_CLASS);
 OutputMessage(mLogHighPri, 'Defining layout of files on volume #%d, which were deleted on #%d...',
                          [NumVolTgt, NumVolSrc], mcSub2Cat);
 if not GetLayouts(TgtVolSpec.GUID, DelFiles, @DelLays) then
  goto exi;
 OutputMessage(mLogNoDate, '<align>Target volume has %d deleted valid files', [Length(DelLays.Data)]);
 OutputMessage(mLogNoDate, '<align>Number of clusters is %d', [DelLays.ClusCount]);
 OutputMessage(mLogHighPri, 'Defining layout of files existing on both volumes...', mcSub2Cat);
 if not GetLayouts(SrcVolSpec.GUID, OldFiles, @OsLays, false, true) then
  goto exi;
_ExpSize := OsLays.ClusCount;

 if not GetLayouts(TgtVolSpec.GUID, OldFiles, @OtLays) then
  goto exi;
 OutputMessage(mLogNoDate, '<align>Both volumes have %d coinciding valid files', [Length(OsLays.Data)]);
 OutputMessage(mLogNoDate, '<align>Number of clusters on source volume is %d', [OsLays.ClusCount]);
 OutputMessage(mLogNoDate, '<align>Corresponding number of target clusters is %d', [OtLays.ClusCount]);
 GetMaxLCN;
 OutputMessage(mLogHighPri, 'Searching modified files...', mcSub2Cat);
 DropFilesWithSimilarBlocks;
 OutputMessage(mLogNoDate, '<align>Number of files with modified blocks is %d', [Length(OsLays.SizIdx)]);
 OutputMessage(mLogNoDate, '<align>Number of modified clusters on source volume is %d', [OsLays.ClusCount]);
 OutputMessage(mLogNoDate, '<align>Corresponding number of target clusters is %d', [OtLays.ClusCount]);
 OutputMessage(mLogHighPri, 'Defining layout of new files...', mcSub2Cat);
 if not GetLayouts(SrcVolSpec.GUID, NewFiles, @NewLays, false, true, bDoNTFSC) then
  goto exi;
 OutputMessage(mLogNoDate, '<align>Source volume has %d new valid files', [Length(NewLays.Data)]);
 OutputMessage(mLogNoDate, '<align>Number of clusters is %d', [NewLays.ClusCount]);
 if NewLays.ClusCount + OsLays.ClusCount = 0 then begin
  if bMatchBlocks  then Inc(SubTasksNum, 2);
  if bMoveToBlocks then Inc(SubTasksNum);
  OutputMessage(mLogNoDate, cMsgNoData)
 end else begin
  OutputMessage(mLogHighPri, 'Excluding source files matching data of target blocks...', mcSub2Cat);
  CntFls := Length(OsLays.SizIdx) + Length(NewLays.SizIdx);
  DrpClu := NewLays.ClusCount + OsLays.ClusCount;
  DropFilesInBlocks(VolHdl, @NewLays); DropFilesInBlocks(VolHdl, @OsLays);

  if CntFls <> Length(OsLays.SizIdx) + Length(NewLays.SizIdx) then
   OutputMessage(mLogNoDate, '<align>Dropped %d files in front of target blocks (%d clusters)',
                 [CntFls - Length(OsLays.SizIdx) - Length(NewLays.SizIdx),
                  DrpClu - NewLays.ClusCount - OsLays.ClusCount]);
  OutputMessage(mLogNoDate, '<align>The number of old files to process is %d', [Length(OsLays.SizIdx)]);
  OutputMessage(mLogNoDate, '<align>The number of new files to process is %d', [Length(NewLays.SizIdx)]);
  ExpSize := ExpSize
           + (_ExpSize - OsLays.ClusCount) * ModificationThreshold div 100
           + 2 * OsLays.ClusCount + DelLays.ClusCount + NewLays.ClusCount;
  OutputMessage(mLogNoDate, '<align>Number of source new & modified clusters is %d', [OsLays.ClusCount + NewLays.ClusCount]);
  OutputMessage(mLogNoDate, '<align>Number of target clusters for optimization is %d', [OtLays.ClusCount + DelLays.ClusCount]);
  OutputMessage(mLogNoDate, '<align>Upper limit for snapshot growth is %d Mbytes', [ExpSize * CluSzToBt div 1048576]);
  OutputMessage(mLogHighPri, 'The files layout on both volumes was acquired in %s sec',
                             [SSeconds(Now, dtbv)], mcSub2Cat);
  if bMatchBlocks then
   FindMatchesFitInBlocks(VolHdl);
  if bMoveToBlocks then
   MoveToBlocks(VolHdl)
 end;

 DropLaysTruncNewFiles(NewFiles, bDoNTFSC, NewFlsSzs);
 OutputMessage(mLogHighPri, 'Completed optimization of persistent data of volume #%d in %s sec',
               [NumVolSrc, SSeconds(Now, dtbv)], mcSubCat);
 Result := true;
exi:
 SetPriorityClass(GetCurrentProcess(), NORMAL_PRIORITY_CLASS)
end;

end.
