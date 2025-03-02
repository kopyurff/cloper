{ cloper_base
  The unit of Cloning Optimizer (Cloper) tool.

  The file encapsulates uncategorized basic functionality to call it from
  tool units.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit cloper_base;

interface
uses
 Types, Windows, cloper_typs;

type
 TMsgType = (mFailure = 1, mInternalErr = 2, mSystemErr = 3, mLowPriErr = 4,
    mSysLowPrE = 5, mWarning = 10,
    mLogHighPri = 6, mLogNoDate = 7, mLogLowPri = 8, mLogNoDtLP = 9,
    mEcoHighPri = 11, mEcoLowPri = 12, mEcoNoDtHP = 13, mEcoNoDtLP = 14,
    mUserError = 15);

 TOutType = (oScreen = 1, oLogFile = 2, oSend = 4);
 TOutTypes = set of TOutType;
 TSetOfByte = set of Byte;

 TDelResult = (drDone = 0, drEmptyAbsent = 1, drError = 2);

 TPerProc = procedure(i, j : Integer; var arr : Pointer);
 TCmpFunc = function(i, j : Integer; arr : Pointer) : Boolean;

const
 {$ifdef WIN32}
  ToolPlatform = 'x86';
 {$else}
  ToolPlatform = 'x64';
 {$endif}

var
 OSMajorMinorVersion : Integer;
 Guid1Vol : String;
 bOptTask : Boolean = false;
 bPauseAtEnd : Boolean = true;
 bUseUsnQueries : Boolean = false;

 DisksStyle: Byte = 0; // MBR = 2, GPT = 1
 SectorsPerCluster: Byte = 0;
 CodeSizeInSectors: Byte = 1;
 ClusterInCodSizes: Byte = 8;

 bRestoreLetters : Boolean = false;
 bActivateTgtDsk : Boolean = true;
 MinVolumeSize : Cardinal = 750;
 COMPRESSION_FORMAT : UInt16 = COMPRESSION_FORMAT_DEFAULT;
 PRIORITY_CLASS : UInt16 = HIGH_PRIORITY_CLASS;
 ModificationThreshold : Byte = 50;
 bCompressData : Boolean = true;
 bClusterWise : Boolean = false;

 bReportFiles: Boolean = false;
 EndUserLetters: String = 'C,E,D';

procedure ConsoleWrite(msg : String); overload;
procedure ConsoleWrite(fmt: String; const vals: array of const); overload;

// Quick sort with minor modifications.
procedure QuickSort(A: Pointer; L, R: Integer;
                                Cmp: TCmpFunc = nil; Per: TPerProc = nil);

function BinSchInt(iar: TIntegerDynArray; val: Integer;
               var ind: Integer; bi: Integer = -1; ei: Integer = -1): Boolean;

function GetFailureByNumber(var DiskSpecs: TVolSpecs; num: Integer): String; overload;
function GetFailureByNumber(num: Integer): String; overload;
function InfoToLog(num: Integer; const vals: array of const;
                bAlign: Boolean = true): String;

function PrintFailure(var DiskSpecs: TVolSpecs; num : Integer; Msg: String = ''): Boolean; overload;
function PrintFailure(num: Integer; Msg: String = ''): Boolean; overload;

{ IsDoMessageOutput
  Checks parameters to define output according msg type or not.
}
function IsDoMessageOutput(const MsgType: TMsgType;
  const OutTypes: TOutTypes = []): Boolean;

{ IsNtfsCompressed
  Checks the file is NTFS compressed.
}
function IsNtfsCompressed(const FileName: string): Boolean;

{ NtfsSetCompression
  Checks the file is NTFS compressed, compresses it if it is not.
}
function NtfsSetCompression(const FileName: string;
 const State: Short = COMPRESSION_FORMAT_LZNT1): Byte;

{ DoMessageOutput
  Prepares message string for its output, performs output according its type.
  Returns true if the string was printed to console.
}
function DoMessageOutput(MsgType : TMsgType; Message_ : String) : Integer;

function GetSysErrorForOutput(Sever: Boolean; var ResMsg: String;
  var ErrorCode: DWORD; SubMessage_: String = ''): TMsgType;

function DoOutputSysError(Sever: Boolean; SubMessage_: String = '';
  ErrorCode: DWORD = 0) : Integer;

procedure AddDelayedLogMsgs(LogMsg: String);

// Auxiliary functions:
function CrtcArr(c: array of Extended): TExtendedDynArray;
function PathInsideVolume(FilePath: String; bFolder: Boolean = false): String;
function GetDrvLtr(Drive : String) : String;
function GetDrvLbl(Drive : String) : PChar;
function ExtractVolumeIdString(VolID : String) : String;

function LBE(Et: PExtent): UInt64;
function LEE(Et: PExtent): UInt64;
type
 TGetExtent = function(P: Pointer; Idx: Integer): PExtent;
 TExtsRange = function(P: Pointer; var Hgh: Integer): Integer;
function JoinExtents(var Res: TExtents;
                        O, N: Pointer; PG: TGetExtent; PL: TExtsRange): Integer;

function GetFileHandle(Drive: String; Access, Share : DWORD; var hFile: THandle;
                       FileFlag: DWORD = 0) : Boolean; overload;
function GetFileHandle(DriveNumber: Byte; Access, Share : DWORD; var hFile: THandle) : Boolean; overload;
function GetFileHandle(DriveNumber, PartitionNumber: Byte;
                       Access, Share : DWORD; var hFile: THandle) : Boolean; overload;

function IoDeviceControl(SID: String; ContCode: DWORD;
                         Access: DWORD = GENERIC_READ or GENERIC_WRITE;
                         Share: DWORD = FILE_SHARE_READ or FILE_SHARE_WRITE;
                         CrDsp: DWORD = OPEN_EXISTING;
                         Flags: DWORD = FILE_ATTRIBUTE_NORMAL): Boolean; overload;
function IoDeviceControl(Handle: THandle; ContCode: DWORD): Boolean; overload;

function __Mul(a, b: DWORD; var HiDWORD: DWORD): DWORD;

function CTL_CODE(DeviceType, Funct, Method, Access : DWORD) : DWORD;

procedure FlipHighLow(var li: LARGE_INTEGER);
procedure SetLargeInteger(var li: LARGE_INTEGER; HhPt: Longint; LwPt: DWORD);

function WriteSectors(hFile: THandle; StartingSector, SectorCount: DWORD;
                      Buffer: Pointer; BytesPerSector: DWORD = 512): DWORD; overload;
function ReadSectors(hFile: THandle; StartingSector, SectorCount: DWORD;
                     Buffer: Pointer; BytesPerSector: DWORD = 512): DWORD; overload;
function ReadSectors(hFile: THandle; var Buffer: TByteDynArray;
                     StartingSector, SectorCount: DWORD;
                     BytesPerSector: DWORD = 512): DWORD; overload;
function ReadSectors(Drive: String; StartingSector, SectorCount: DWORD;
                     Buffer: Pointer; BytesPerSector: DWORD = 512): DWORD; overload;
function ReadSectors(DriveNumber: Byte; StartingSector, SectorCount: DWORD;
                     Buffer: Pointer; BytesPerSector: DWORD = 512): DWORD; overload;

{ DeleteFileNT32
  Deletes file using ntdll or kernel32 APIs.
}
function DeleteFileNT32(wsFileName: WideString) : Boolean;

{ DeleteDirectory
  Recursively cleans files & subfolders, deletes folder.
}
function DeleteDirectory(PathName: String; bReportFiles: Boolean = false) : TDelResult;

{ CleanUpDirectory
  Recursively cleans files & subfolders in directory.
}
function CleanUpDirectory(Dir: string; const Msk: string;
                 bReportFiles: Boolean = false) : TDelResult;

function GetWinCaption(const AHandle: THandle): string; inline;
procedure SetWinCaption(const AHandle: THandle; Caption: String); inline;

function GetWinClass(const AHandle: THandle): string; inline;
function IsDebuggingRun : Boolean;
function IsProcessInsideWindowContainer(const ClassNames: array of String): Boolean;
function CheckOtherInstancesRun: Boolean;

function GetOSMajorMinorVersion : Integer;
function GetWin32_OSName: String;

type
 TMemUnit = (muByte = 0, muKB = 1, muMB = 2, muGB = 3);

function GetRAMsize(MemUnit : TMemUnit = muMB) : UInt64;

{ AnyKey (2)
  Checks user input to console and exits. If pressed needed button returns it.
}
function AnyKey(const SchKeys : array of Char) : Char;
function AnyKeyToExit(Submit : Char = #0) : Boolean;

type
 TDoMessageOutput = procedure(Msg : String);
 TGetVolumeFileSystem = function(VolNum : Integer; var FileSystem: String): Boolean;

var
 LogFile : String;
 OutType : Integer = Integer(oScreen) or Integer(oLogFile);
 ErrLowPriOutput : Boolean = false;
 LogLowPriOutput : Boolean = false;
 SysELowPrOutput : Boolean = false;
 EcoLowPriOutput : Boolean = false;
 SilentMode : Boolean = false;
 SubTasksCnt: Cardinal = 1;
 SubTasksNum: Cardinal = 0;
 TimeShift : Integer = 0;
 LogMsgString, SendMsgString : TDoMessageOutput;
 DelayedLogMsgs: TStringDynArray = nil;

 SrcVolSpec, TgtVolSpec: TVolSpecs;
 SrcLabels, TgtLabels : TVolLabels;
 ReadStep : Cardinal;
 ExpSize  : UINT64 = 0;
 CluSzToBt: Integer = 0;
 CodSzToBt: Integer = 0;

 fReportTestingEvent : TReportTestingEvent;
const
 RamOverflowLimit = 1843200;

 cfsDevDskPart = '\Device\Harddisk%d\Partition%d';
 cfsGlobalDevDskPart = '\\?\GLOBALROOT\Device\Harddisk%d\Partition%d';
 cfsGlobalDevHardDsk = '\\?\GLOBALROOT\Device\Harddisk%d\Partition0';
 cfsVolume = '\\?\HarddiskVolume%d';
 cfsGlobalVolume = '\\?\GLOBALROOT\Device\HarddiskVolume%d';
 cfsDiskPartition = '\\?\Harddisk%dPartition%d';
 cfsPhysDrive = '\\?\PhysicalDrive%d';
 cfsScsi = '\\?\Scsi%d:';
 csGlobalMountMan = '\\?\GLOBALROOT\Device\MountPointManager';

 cMsgNoData = '<align>There is not data to process';

implementation
uses
 Messages, tlhelp32, cloper_into, SysUtils, StrUtils, StrTools;

{ ConsoleWrite
  Auxiliary functions to do tracing into the events log in debug time.
}
procedure ConsoleWrite(msg : String);
begin
 if IsDebuggingRun then
  OutputDebugString(PChar('[' + FormatDT(Now, false) + ']  >>             ' + msg
    + '             <<'))
end;
procedure ConsoleWrite(fmt: String; const vals: array of const);
begin
  ConsoleWrite(Format(fmt, vals))
end;
type
 TEcoMessage = record
   MsgType : TMsgType;
   Message : String;
 end;
 TArrEcoMessage = array of TEcoMessage;
var
 ArrEcoMessage : TArrEcoMessage;

{ IsDoMessageOutput
  Checks parameters to define output according msg type or not.
}
function IsDoMessageOutput(const MsgType: TMsgType;
  const OutTypes: TOutTypes = []): Boolean;
begin
 Result := false;
 if SilentMode and (oScreen in OutTypes) then exit;
 Result :=  ((MsgType <> mLogLowPri) or LogLowPriOutput)
         and((MsgType <> mLogNoDtLP) or LogLowPriOutput)
         and((MsgType <> mLowPriErr) or ErrLowPriOutput)
         and((MsgType <> mSysLowPrE) or SysELowPrOutput)
         and((MsgType <> mEcoLowPri) or EcoLowPriOutput)
         and((MsgType <> mEcoNoDtLP) or EcoLowPriOutput)
end;

{ DoMessageOutput
  Prepares message string for its output, performs output according msg type.
}
function DoMessageOutput(MsgType : TMsgType; Message_ : String) : Integer;
var
 Log, Eco, Deb, sdt : String; i : Integer;
begin
 Result := 0;
 Log := Message_;
 sdt := '[' + FormatDT(Now) + '] ';
 if MsgType in [mLogHighPri, mLogLowPri, mEcoHighPri, mEcoLowPri] then begin
  Log := sdt + Log; Eco := Log; Deb := Trim(Message_);
 end else begin
  if MsgType = mFailure then begin
   Log := 'FAILURE ' + Log; Eco := Log
  end else
   if MsgType in [mInternalErr, mLowPriErr] then begin
    Eco := PadRight('ERROR:', Length(sdt), ' ') + Log;
    Log := 'ERROR: ' + Log
   end else
    if MsgType = mWarning then begin
     Eco := PadRight('WARNING:', Length(sdt), ' ') + Log;
     Log := 'WARNING: ' + Log
    end else
     if MsgType = mUserError then begin
      Eco := PadRight('USER ERROR:', Length(sdt), ' ') + Log;
      Message_ := 'USER ERROR: ' + Log;
     end else
      if MsgType in [mLogNoDate, mLogNoDtLP, mEcoNoDtHP, mEcoNoDtLP] then
      begin
       i := Pos('<align>', LowerCase(Log));
       if 0 < i then begin
        Eco := PadRight(LeftStr(Log, i - 1), Length(sdt), ' ') +
            RightStr(Log, Length(Log) - 6 - i);
        if MsgType in [mLogNoDate, mLogNoDtLP] then
         Log := Eco
       end else
        Eco := Log
      end else Eco := Log;
  Deb := Trim(Log)
 end;

 if IsDoMessageOutput(MsgType) then begin
  if Integer(oScreen) and OutType > 0 then begin
   Writeln(PadRight(Eco, 79, ' '));
   if bOptTask then Sleep(25);
   Result := 1 + Length(Eco) div 80
  end;
  if not(MsgType in [mEcoHighPri, mEcoLowPri, mEcoNoDtHP, mEcoNoDtLP,
      mUserError]) then begin
   if(Integer(oLogFile) and OutType > 0) and Assigned(LogMsgString) then
    LogMsgString(Log);

   ConsoleWrite(Deb)
  end;

  if(Integer(oSend) and OutType > 0) and Assigned(SendMsgString) then
   SendMsgString(Log);
 end;
end;

function GetSysErrorForOutput(Sever: Boolean; var ResMsg: String;
  var ErrorCode: DWORD; SubMessage_: String = ''): TMsgType;
begin
 if Sever then Result := mSystemErr else Result := mSysLowPrE;

 if ErrorCode = NO_ERROR then
  ErrorCode := GetLastError();
 if SubMessage_ = '' then ResMsg := '' else
    ResMsg := '[' + SubMessage_ + '] ';
 ResMsg := ResMsg + 'SYSTEM ERROR #' + IntToStr(ErrorCode) + ': ' +
    SysErrorMessage(ErrorCode)
end;

function DoOutputSysError(Sever: Boolean; SubMessage_: String = '';
  ErrorCode: DWORD = 0) : Integer;
var
 ResMsg : String;
begin
 DoMessageOutput(GetSysErrorForOutput(Sever, ResMsg, ErrorCode,
    SubMessage_), ResMsg);
 Result := ErrorCode
end;

// -----------------------------------------------------------------------------
// Functions to define parameters of processes & windows.
function GetWinClass(const AHandle: THandle): string; inline;
var
  WindowText: array[0..255] of Char;
begin
  SetString(Result, WindowText,
    GetClassName(AHandle, PChar(@WindowText), Length(WindowText)));
end;

function GetWinCaption(const AHandle: THandle): string; inline;
var
  WindowText: array[0..255] of Char;
begin
  SetString(Result, WindowText,
    GetWindowText(AHandle, PChar(@WindowText), Length(WindowText)));
end;

procedure SetWinCaption(const AHandle: THandle; Caption: String); inline;
begin
  SetWindowText(AHandle, Caption);
end;

{ IsDebuggingRun
  Checks debugger attached to the current running application.
}
function IsDebuggingRun : boolean;
type
  TDebugProc = function : boolean;
    stdcall;
var
  Kernel32: HMODULE;
  DebugProc: TDebugProc;
begin { DebuggerPresent }
  Result := False;
  Kernel32 := GetModuleHandle('kernel32');
  if Kernel32<>0 then
  begin
    @DebugProc := GetProcAddress(Kernel32, 'IsDebuggerPresent');
    if Assigned(DebugProc) then
      Result := DebugProc
  end; { Kernel32 <> 0 }
end; { IsDebuggingRun }

function IsProcessInsideWindowContainer(const ClassNames: array of String): Boolean;
var
 ClsNm : String; i : Integer;
begin
 ClsNm := GetWinClass(GetConsoleWindow);
 for i := 0 to Length(ClassNames) - 1 do
  if ClsNm = ClassNames[i] then begin
   Result := true;
   exit
  end;
 if Length(ClassNames) = 0 then
  Result := ClsNm = 'ConsoleWindowClass'
 else
  Result := false
end;

function CheckOtherInstancesRun: Boolean;
var
 HdlProc, HdlMod: THandle;
 ProcEntry: TProcessEntry32;
 ModEntry: TModuleEntry32;
 CurProcId: DWORD;
 CurProcFN, CurProcFP, ProcFNP: String;
begin
 Result := false;
 HdlProc := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
 if HdlProc <> INVALID_HANDLE_VALUE then begin
  ProcEntry.dwSize := SizeOf(ProcEntry);
  if Process32First(HdlProc, ProcEntry) then begin
   CurProcId := GetCurrentProcessId();
   CurProcFN := LowerCase(Trim(ExtractFileName(ParamStr(0))));
   CurProcFP := LowerCase(Trim(ExtractFilePath(ParamStr(0))));
   repeat
    if ProcEntry.th32ProcessID <> CurProcId then begin
     SetString(ProcFNP, ProcEntry.szExeFile, MAX_PATH);
     if CurProcFN = LowerCase(Trim(ProcFNP)) then begin
      HdlMod := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, ProcEntry.th32ProcessID);
      if HdlProc <> INVALID_HANDLE_VALUE then begin
       ModEntry.dwSize := SizeOf(MODULEENTRY32);
       if Module32First(HdlMod, ModEntry) then
        repeat
         SetString(ProcFNP, ModEntry.szExePath, MAX_PATH);
         ModEntry.dwSize := SizeOf(MODULEENTRY32);
         Result := CurProcFP + CurProcFN = LowerCase(Trim(ProcFNP))
        until Result or not Module32Next(HdlMod, ModEntry);
       CloseHandle(HdlMod)
      end
     end
    end
   until Result or not Process32Next(HdlProc, ProcEntry);
   CloseHandle(HdlProc)
  end
 end
end;
// -----------------------------------------------------------------------------

{ GetFailureByNumber
  Gets failures of tool by its number.
}
function GetFailureByNumber(var DiskSpecs: TVolSpecs; num: Integer): String;
var Msg: String; begin
 Msg := PadLeft(IntToStr(num), 4, '0') + ': ';
 case num of
 01 : Result := Msg + 'The configuration file of tool wasn''t found.';
 02 : Result := Msg + 'Failed write data to the volume of source disk.';
 03 : Result := Msg + 'Failed write data to the volume of target disk.';
 04 : Result := Msg + 'Failed to read source disk data.';
 05 : Result := Msg + 'Failed to read target disk data.';
 06 : Result := Msg + 'Different total number of clusters of source and target disks.';
 07 : Result := Msg + 'Different number of sectors per cluster of source and target disks.';
 08 : Result := Msg + 'Different number of bytes per sector of source and target disks.';
 09 : Result := Msg + 'The attempt to lock source disk has failed.';
 10 : Result := Msg + 'The attempt to lock target disk has failed.';
 11 : Result := Msg + 'The read of source or target disk has failed.';
 12 : Result := Msg + 'Couldn''t delete file of disk zeroing routine.';
 13 : Result := Msg + 'Geometries of source & target disks differ' +
                      ' or target disk is unallocated.';
 14 : Result := Msg + 'Initialization of disk failed';
 15 : Result := Msg + 'Deletion of target disk system failed';
 16 : Result := Msg + 'Restoring of target MBR failed.';
 17 : Result := Msg + 'Initialization of defraggler unit failed.';
 18 : Result := Msg + 'The source have not free space for optimization of persistent data.';
 19 : Result := Msg + 'The tool library "CloperF" wasn''t found.';
 20 : Result := Msg + 'Initialization of free space patching failed.';
 21 : Result := Msg + 'Failed to find disks or partition with specified serial numbers.';
 22 : Result := Msg + 'Other working instances of this tool must be closed to run task.';
 23 : Result := Msg + 'Failed to assign letter to source disk, it may be is offline.';
 24 : Result := Msg + 'Failed to assign letter to target disk, it may be is offline.';
 25 : Result := Msg + 'Overflow, decrease memory use limit for current data set.';
 else
  raise Exception.Create('Unknown failure #' + IntToStr(num))
 end;
 DiskSpecs.ErrorCode := num
end;

function GetFailureByNumber(num: Integer): String;
var
 DiskSpecs: TVolSpecs;
begin
 Result := GetFailureByNumber(DiskSpecs, num);
end;

{ InfoToLog
  Returns log message string by its number.
}
function InfoToLog(num: Integer; const vals: array of const;
                bAlign: Boolean = true): String;
begin
 case num of
 01: Result := Format('The size of partition #%d is less than %d MB, skipped', vals);
 02: Result := Format('The USN journal of partition #%d is inactive, skipped', vals);
 03: Result := Format('The file system of partition #%d is not NTFS, skipped', vals);
 end;
 if bAlign then Result := '<align>' + Result
end;

{ PrintFailure
  Prints failures of tool by its number.
}
function PrintFailure(var DiskSpecs: TVolSpecs; num: Integer; Msg: String = ''): Boolean;
begin
 if Msg = '' then Msg := GetFailureByNumber(num) else
  Msg := GetFailureByNumber(num) + ' ' + Msg;
 Result := true; DoMessageOutput(mFailure, Msg)
end;

function PrintFailure(num: Integer; Msg: String = ''): Boolean;
begin
 if Msg = '' then Msg := GetFailureByNumber(num) else
  Msg := GetFailureByNumber(num) + ' ' + Msg;
 Result := true; DoMessageOutput(mFailure, Msg)
end;

procedure IntArrPer(i, j : Integer; var ptr : Pointer);
var
 ar : PIntegerDynArray; k : Integer;
begin
 ar := PIntegerDynArray(ptr); k := ar^[I]; ar^[I] := ar^[J]; ar^[J] := k
end;

function IntArrCmp(I, J : Integer; ptr : Pointer) : Boolean;
var
 ar : PIntegerDynArray;
begin
 ar := PIntegerDynArray(ptr); Result := ar^[I] < ar^[J]
end;

// Quick sort with minor modifications.
procedure QuickSort(A: Pointer; L, R: Integer;
                                Cmp: TCmpFunc = nil; Per: TPerProc = nil);
var
 I, J, P: Integer;
begin
 if @Cmp = nil then Cmp := @IntArrCmp; if @Per = nil then Per := @IntArrPer;
 repeat
  I := L; J := R; P := (L + R) shr 1;
  repeat
   while (I < R) and (I <> P) and Cmp(I, P, A) do Inc(I);
   while (L < J) and (J <> P) and Cmp(P, J, A) do Dec(J);
   if I <= J then begin
    if I <> J then Per(I, J, A);
    if P = I then P := J else if P = J then P := I;
    Dec(J); Inc(I)
   end
  until I > J;
  if L < J then QuickSort(A, L, J, Cmp, Per); L := I
 until I >= R
end;

function BinSchInt(iar: TIntegerDynArray; val: Integer;
               var ind: Integer; bi: Integer = -1; ei: Integer = -1): Boolean;
var
 i, j, k: Integer;
begin
 Result := false; i := Length(iar);
 if (-1 < bi) and (bi < i) then j := bi else j := 0;
 if (-1 < ei) and (ei < i) then i := ei else Dec(i); k := i; ind := k;
 while 1 < k do begin
  if ind < j then Inc(ind, k) else if i < ind then Dec(ind, k);
  k := (k + k mod 2) div 2;
  if val < iar[ind] then Dec(ind, k) else if iar[ind] < val then Inc(ind, k) else
   break
 end;
 k := 1;
 repeat
  if (j <= ind + k) and (ind + k <= i) then
   if iar[ind + k] = val then Result := true else if iar[ind + k] < val then
    break;
  Dec(k)
 until (ind + k < 0) or (k < -1);
 ind := ind + k + 1
end;

// Functions for TExtent type & for its derivatives.
function LBE(Et: PExtent): UInt64; begin
 if Et = nil then Result := 0 else Result := Et.Lcn.QuadPart
end;
function LEE(Et: PExtent): UInt64; begin
 if Et = nil then Result := 0 else Result := LBE(Et) + Et.Vcs - 1
end;

function JoinExtents(var Res: TExtents;
                        O, N: Pointer; PG: TGetExtent; PL: TExtsRange): Integer;
var
  oL, nL, oI, nI: Integer; oB, nB: Boolean; Eo, En: PExtent; F: TExtents;

 function L(P: Pointer; var h: Integer; var b: Boolean): Integer;
 begin
  try Result := PL(P, h) except Result := -1; h := -1 end; b := -1 < h
 end;
 function G(P: Pointer; i: Integer; var b: Boolean): PExtent;
 begin
  if b then try Result := PG(P, i) except Result := nil; b := false end else
   Result := nil
 end;
 procedure Ad(R: PExtent); begin
  if R <> nil then
   if(0 < Result) and (LBE(R) <= LBE(@F[Result - 1]) + F[Result - 1].Vcs) then
   begin
    if LEE(@F[Result - 1]) < LEE(R) then
     Inc(F[Result - 1].Vcs, LEE(R) - LEE(@F[Result - 1]))
   end else begin
    SetLength(F, Result + 1); F[Result].Lcn := R.Lcn; F[Result].Vcs := R.Vcs;
    Inc(Result)
   end
 end;
 procedure Ic(L: Integer; var i: Integer; var b: Boolean); begin
  if i < L then Inc(i) else b := false
 end;
begin
 Result := 0; SetLength(F, 0);
 oI := L(O, oL, oB); Eo := G(O, oI, oB); nI := L(N, nL, nB); En := G(N, nI, nB);
 while oB or nB do begin
  if Eo = nil then begin Ad(En); Ic(nL, nI, nB) end
  else
   if En = nil then begin Ad(Eo); Ic(oL, oI, oB) end
   else
    if (LBE(Eo) = LBE(En)) and (LEE(Eo) = LEE(En)) then begin
     Ad(Eo); Ic(oL, oI, oB); Ic(nL, nI, nB)
    end else
     if LBE(Eo) <= LBE(En) then begin Ad(Eo); Ic(oL, oI, oB) end
     else
      if LBE(En) <= LBE(Eo) then begin Ad(En); Ic(nL, nI, nB) end;
  Eo := G(O, oI, oB); En := G(N, nI, nB)
 end;
 SetLength(Res, Result); for nI := 0 to Result - 1 do Res[nI] := F[nI]
end;

procedure AddDelayedLogMsgs(LogMsg: String);
var
 i: Integer;
begin
 i := Length(DelayedLogMsgs); SetLength(DelayedLogMsgs, i + 1);
 DelayedLogMsgs[i] := LogMsg
end;

function CrtcArr(c: array of Extended): TExtendedDynArray;
var
 i: Integer;
begin
 SetLength(Result, Length(c)); for i := 0 to Length(c) - 1 do Result[i] := c[i]
end;

// Excludes volume identifier inside of file path string:
function PathInsideVolume(FilePath: String; bFolder: Boolean = false): String;
begin
 Result := '';
 if bFolder then begin
  if not DirectoryExists(FilePath) then exit
 end else
  if not FileExists(FilePath) then exit;

 Result := ExcludeTrailingBackslash(ExtractFilePath(FilePath));
 while DirectoryExists(LeftStr(Result, LastDelimiter('\', Result))) do
  Result := LeftStr(Result, LastDelimiter('\', Result) - 1);
 Result := ReplaceStr(FilePath, IncludeTrailingBackslash(Result), '')
end;

function GetDrvLtr(Drive : String) : String; begin
 // Format('\\.\%s:', [Disk])
 Result := '\\.\' + ReplaceStr(ReplaceStr(Drive, '\\.\', ''), '\\?\', '');
 if Length(ReplaceStr(ReplaceStr(ReplaceStr(ReplaceStr(Drive, '\', ''), '.',
    ''), '?', ''), ':', '')) = 1 then
    // if Pos('physicaldrive', LowerCase(Result)) = 0 then
  Result := ReplaceStr(ReplaceStr(Result, ':\', ''), ':', '') + ':';
end;
function GetDrvLbl(Drive : String) : PChar; begin
 Result := PChar(GetDrvLtr(Drive))
end;

// Converts strings from format like this:
// \\?\Volume{c4ee0265-bada-11dd-9cd5-806e6f6e6963}\
// to
// Volume{c4ee0265-bada-11dd-9cd5-806e6f6e6963}
function ExtractVolumeIdString(VolID : String) : String;
begin
 if 3 < Length(VolID) then
  if (VolID[1] + VolID[4] = '\\') and
     (Pos(VolID[2], '?\') * Pos(VolID[3], '?.') > 0) then
   VolID := Copy(VolID, 5, Length(VolID) - 4);
 Result := ExcludeTrailingPathDelimiter(VolID);
end;

function GetFileHandle(Drive: String; Access, Share : DWORD; var hFile: THandle;
                    FileFlag: DWORD = 0) : Boolean;
begin
 hFile := CreateFile(GetDrvLbl(Drive), Access, Share, nil, OPEN_EXISTING,
                     FileFlag,
                     0);
 Result := hFile <> INVALID_HANDLE_VALUE;
end;
function GetFileHandle(DriveNumber: Byte; Access, Share : DWORD; var hFile: THandle) : Boolean;
begin
 hFile := CreateFile(PChar('\\.\PhysicalDrive' + IntToStr(DriveNumber)),
                     Access, Share, nil, OPEN_EXISTING,
                     FILE_ATTRIBUTE_NORMAL,
                     0);
 Result := hFile <> INVALID_HANDLE_VALUE;
end;
function GetFileHandle(DriveNumber, PartitionNumber: Byte;
                       Access, Share : DWORD; var hFile: THandle) : Boolean;
begin
 // http://stackoverflow.com/questions/29628081/get-volume-guid-from-partition-number-in-windows
 // -->> '\\.\Harddisk[DriveNumber - 1]Partition[PartitionNumber]'
 hFile := CreateFile(PChar('\\.\Harddisk' + IntToStr(DriveNumber - 1)
                            + 'Partition' + IntToStr(PartitionNumber)),
                     Access, Share, nil, OPEN_EXISTING,
                     FILE_FLAG_NO_BUFFERING or FILE_FLAG_WRITE_THROUGH,
                     0);
 Result := hFile <> INVALID_HANDLE_VALUE;
end;

// --------- 2 overloaded functions of DeviceIoControl for internal use --------
{ DeviceIoControl
  Overloaded function incapsulates obtaining handle and call of parent function.
}
function IoDeviceControl(SID: String; ContCode: DWORD;
                         Access: DWORD = GENERIC_READ or GENERIC_WRITE;
                         Share: DWORD = FILE_SHARE_READ or FILE_SHARE_WRITE;
                         CrDsp: DWORD = OPEN_EXISTING;
                         Flags: DWORD = FILE_ATTRIBUTE_NORMAL): Boolean;
var
 Handle: THandle; jk: Cardinal;
begin
 Handle := CreateFile(PChar(SID), Access, Share, nil, CrDsp, Flags, 0);
 Result := Handle <> INVALID_HANDLE_VALUE;
 if Result then begin
  Result := DeviceIoControl(Handle, ContCode, nil, 0, nil, 0, jk, nil);
  if not Result then
   DoOutputSysError(true);
  CloseHandle(Handle)
 end else
  DoOutputSysError(true)
end;

{ DeviceIoControl
  Overloaded function incapsulates call of parent function.
}
function IoDeviceControl(Handle: THandle; ContCode: DWORD): Boolean;
var
 jk: Cardinal;
begin
 Result := DeviceIoControl(Handle, ContCode, nil, 0, nil, 0, jk, nil);
 if not Result then
  DoOutputSysError(true)
end;
// --------- ---------------------------------------------------------- --------

// Multiplies integers as long, splits product to low and high DWORD:
function __Mul(a, b: DWORD; var HiDWORD: DWORD): DWORD; // Result = LoDWORD
{$IFDEF WIN64}
 var
  aux_hi, aux_lo : UInt64;
 begin
  aux_lo := UInt64(a) * UInt64(b);
  aux_hi := aux_lo shr 32;
  HiDWORD := DWORD(aux_hi);
  Result := DWORD(aux_lo);
{$ELSE}
 asm
  mul edx
  mov [ecx],edx
{$ENDIF}
end;

function CTL_CODE(DeviceType, Funct, Method, Access : DWORD) : DWORD;
begin
 Result := (DeviceType shl 16) or (Access shl 14) or (Funct shl 2) or Method;
end;

{ FlipHighLow
  Procedure to work around reverse order of High/Low part in LARGE_INTEGER after
  call of DeviceIoControl with control code FSCTL_GET_RETRIEVAL_POINTERS.
}
procedure FlipHighLow(var li : LARGE_INTEGER);
var
 il : LARGE_INTEGER;
begin
 if(li.LowPart = 0)and(li.HighPart = -1)then
  li.QuadPart := -1
 else begin
  il.LowPart := li.HighPart; il.HighPart := li.LowPart; li := il
 end
end;

procedure SetLargeInteger(var li: LARGE_INTEGER; HhPt: Longint; LwPt: DWORD);
begin
 li.LowPart := LwPt; li.HighPart := HhPt;
 if li.LowPart <> LwPt then
  raise Exception.Create('Assigned unexpected value to LARGE_INTEGER.LowPart')
end;

{$region 'ReadSectors, WriteSectors'}
// Original source in russian:
// Kerk, http://kladovka.net.ru/ ,
// http://citforum.ru/programming/delphi/disk_editor/
// Code was modified for local use.

function ReadSectors(hFile: THandle; StartingSector, SectorCount: DWORD;
                     Buffer: Pointer; BytesPerSector: DWORD = 512): DWORD;
var
 br, TmpLo, TmpHi: DWORD;
begin
 TmpLo := __Mul(StartingSector, BytesPerSector, TmpHi);
 if SetFilePointer(hFile, TmpLo, @TmpHi, FILE_BEGIN) = TmpLo then
 begin
  SectorCount := SectorCount * BytesPerSector;
  if ReadFile(hFile, Buffer^, SectorCount, br, nil) then Result := br;
 end else
  Result := 0
end;
function ReadSectors(hFile: THandle; var Buffer: TByteDynArray;
                     StartingSector, SectorCount: DWORD;
                     BytesPerSector: DWORD = 512): DWORD;
var
 i : Integer;
begin
 if Length(Buffer) <> SectorCount * BytesPerSector then
  SetLength(Buffer, SectorCount * BytesPerSector);
 for i := 0 to Length(Buffer) - 1 do Buffer[i] := 0;
 Result := ReadSectors(hFile, StartingSector, SectorCount, @Buffer[0], BytesPerSector);
end;
function ReadSectors(Drive: String; StartingSector, SectorCount: DWORD;
                     Buffer: Pointer; BytesPerSector: DWORD = 512): DWORD;
var
 hFile: THandle;
begin
 if GetFileHandle(Drive, GENERIC_READ, FILE_SHARE_READ, hFile) then
 begin
  Result := ReadSectors(hFile, StartingSector, SectorCount, Buffer, BytesPerSector);
  CloseHandle(hFile)
 end else Result := 0;
end;
function ReadSectors(DriveNumber: Byte; StartingSector, SectorCount: DWORD;
                     Buffer: Pointer; BytesPerSector: DWORD = 512): DWORD;
begin
 Result := ReadSectors('\\.\PhysicalDrive' + IntToStr(DriveNumber),
                       StartingSector, SectorCount, Buffer, BytesPerSector);
end;

function WriteSectors(hFile: THandle; StartingSector, SectorCount: DWORD;
                      Buffer: Pointer; BytesPerSector: DWORD = 512): DWORD;
var
 bw, TmpLo, TmpHi: DWORD;
begin
 Result := 0; TmpLo := __Mul(StartingSector, BytesPerSector, TmpHi);
 if SetFilePointer(hFile, TmpLo, @TmpHi, FILE_BEGIN) = TmpLo then
 begin
  SectorCount := SectorCount * BytesPerSector;
  if WriteFile(hFile, Buffer^, SectorCount, bw, nil) then Result := bw;
 end;
end;
{$endregion}

function IsNtfsCompressed(const FileName: string): Boolean;
var
 Attr: DWORD;
begin
 Attr := GetFileAttributes(PChar(FileName));
 Result := 0 < Attr and FILE_ATTRIBUTE_COMPRESSED
end;

{ NtfsSetCompression
  Checks the file is NTFS compressed, compresses it if it is not.
}
function NtfsSetCompression(const FileName: string;
 const State: Short = COMPRESSION_FORMAT_LZNT1): Byte;
var
 Handle: THandle;
 BytesReturned: DWORD;
 Buffer: Short;
 Attr: DWORD;
begin
 Result := 0;
 if bCompressData then
 begin
  Attr := GetFileAttributes(PChar(FileName));
  if Attr and FILE_ATTRIBUTE_DIRECTORY > 0 then exit;
  if Attr and FILE_ATTRIBUTE_COMPRESSED > 0 then begin Result := 1; exit end;

  Handle := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,
            FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);

  if Handle = INVALID_HANDLE_VALUE then
   Result := 4
  else
   try
    Buffer := State;
    if DeviceIoControl(Handle, FSCTL_SET_COMPRESSION,
      @Buffer, SizeOf(Short), nil, 0, BytesReturned, nil) then
     Result := 2
    else
     Result := 3;
    FlushFileBuffers(Handle)
   finally
    CloseHandle(Handle)
   end
 end
end;

procedure InitializeObjectAttributes(var InitializedAttributes: TObjectAttributes;
                                     ObjectName: PUnicodeString;
                                     Attributes: ULONG;
                                     RootDirectory: THandle;
                                     SecurityDescriptor: Pointer;
                                     SecurityQualityOfService : Pointer);
begin
 InitializedAttributes.Length := SizeOf(TObjectAttributes);
 InitializedAttributes.RootDirectory := RootDirectory;
 InitializedAttributes.Attributes := Attributes;
 InitializedAttributes.ObjectName := ObjectName;
 InitializedAttributes.SecurityDescriptor := SecurityDescriptor;
 InitializedAttributes.SecurityQualityOfService := SecurityQualityOfService;
end;

{ DeleteFileNT
  Deletes file using ntdll APIs.
 (x86 target only, in x64 ntdll API silently fails & not used because of it)
}
function DeleteFileNT(wsFileName: WideString) : Boolean;
var
 ObjectAttributes: TObjectAttributes;
 UnicodeString: UNICODE_STRING;
begin
 RtlInitUnicodeString(@UnicodeString, @wsFileName);
 RtlDosPathNameToNtPathName_U(@wsFileName[1], UnicodeString, nil, nil);
 InitializeObjectAttributes(ObjectAttributes, @UnicodeString, $40, 0, nil, nil);
 Result := NtDeleteFile(@ObjectAttributes) <> $C0000001; // pass on the NTSTATUS
end;

{ DeleteFileNT32
  Deletes file using ntdll (x86) or kernel32 (x64) APIs.
}
function DeleteFileNT32(wsFileName: WideString) : Boolean;
begin
 SetFileAttributes(PWChar(wsFileName), FILE_ATTRIBUTE_NORMAL);
 if {$IFDEF WIN64}
     DeleteFile(wsFileName)
    {$ELSE}
     DeleteFileNT(wsFileName)
    {$ENDIF} then
  Result := not FileExists(wsFileName)
 else
  Result := false
end;

{ DeleteDirectory
  Recursively cleans files & subfolders, deletes folder.
}
function DeleteDirectory(PathName: string; bReportFiles: Boolean = false) : TDelResult;
var
 F : TSearchRec;
begin
 Result := drEmptyAbsent;
 if DirectoryExists(PathName) then
 begin
  PathName := ExcludeTrailingBackslash(PathName);
  if FindFirst(PathName + '\*', faAnyFile, F) = 0 then begin
   try
    repeat
     if (F.Attr and faDirectory <> 0) then begin
      if (F.Name <> '.') and (F.Name <> '..') then begin
        if Result = drError then
         DeleteDirectory(PathName + '\' + F.Name)
        else
         Result := DeleteDirectory(PathName + '\' + F.Name)
      end;
     end else
      if DeleteFileNT32(PathName + '\' + F.Name) then Result := drDone else begin
       Result := drError;
       if bReportFiles and (@fReportTestingEvent <> nil) then
        fReportTestingEvent(teDelFail,
                            PathInsideVolume(PathName + '\' + F.Name, true));
       break
      end
    until (Result = drError) or (FindNext(F) <> 0)
   finally
    FindClose(F);
   end;
   if Result <> drError then begin
    SetFileAttributes(PWChar(PathName), FILE_ATTRIBUTE_DIRECTORY or
           FILE_ATTRIBUTE_NORMAL);
    if RemoveDir(PathName) then Result := drDone else
     Result := drError;
    if DirectoryExists(PathName) then
     Result := drError
   end
  end
 end
end;

{ CleanUpDirectory
  Recursively cleans files & subfolders in directory.
}
function CleanUpDirectory(Dir: string; const Msk: string; bReportFiles: Boolean = false) : TDelResult;
var
 F : TSearchRec; hDLL: HMODULE;
begin
 Result := drEmptyAbsent;
 if DirectoryExists(Dir) then
 begin
  hDLL := LoadCloperF;
  Dir := ExcludeTrailingBackslash(Dir);
  if FindFirst(Dir + '\*', faAnyFile, F) = 0 then
   try
    repeat
     if MaskFits(Msk, F.Name, hDLL) then
      if (F.Attr and faDirectory <> 0) then begin
       if (F.Name <> '.') and (F.Name <> '..') then begin
        if Result = drError then
         DeleteDirectory(Dir + '\' + F.Name)
        else begin
         Result := DeleteDirectory(Dir + '\' + F.Name);
         if (Result = drError) and bReportFiles and (@fReportTestingEvent <> nil) then
          fReportTestingEvent(teDelFail,
                              PathInsideVolume(Dir + '\' + F.Name, true))
        end
       end
      end else begin
       if DeleteFileNT32(Dir + '\' + F.Name) then Result := drDone else begin
        Result := drError;
        if bReportFiles and (@fReportTestingEvent <> nil) then
         fReportTestingEvent(teDelFail, PathInsideVolume(Dir + '\' + F.Name));
        break
       end
      end
    until FindNext(F) <> 0
   finally
    FindClose(F)
   end;
  FreeLibrary(hDLL)
 end
end;

{ GetOSMajorVersion
 Obtains integer value of current OS version as
                                  Integer( 'dwMajorVersion' + 'dwMinorVersion' )
 for comparison. Expected results:
 4x = Windows NT 4
 50 = Windows 2000
 51 = Windows XP_86
 52 = Windows 2003 , Windows XP_64
 60 = Windows Vista
 61 = Windows 7
}
function GetOSMajorMinorVersion : Integer;
var
  Data: TOSVersionInfo;
begin
 FillChar(Data, SizeOf(Data), 0); //#1 Adding this might solve the whole problem -JJS 10/12/2005
 Data.dwOSVersionInfoSize := SizeOf(Data);
 GetVersionEx(Data);
 Result := StrToInt(IntToStr(Data.dwMajorVersion) +
        IntToStr(Data.dwMinorVersion))
end;

function GetWin32_OSName: String;
begin
 Result := TOSVersion.Name;
 if TOSVersion.Architecture in [arIntelX86, arARM32] then
  Result := Result + ' x86'
 else
  Result := Result + ' x64'
end;

function GetRAMsize(MemUnit : TMemUnit = muMB) : UInt64;
var
 Memory: TMemoryStatusEx;
begin
 Memory.dwLength := SizeOf( Memory );
 GlobalMemoryStatusEx( Memory );
 Result := Memory.ullTotalPhys;
 case MemUnit of
   muKB: Result := Result div 1024;
   muMB: Result := Result div 1024 div 1024;
   muGB: Result := Result div 1024 div 1024 div 1024;
 end;
end;

{ AnyKey (2)
  Checks user input to console and exits. Returns symbol when the specified
  button was pressed.
}
function AnyKey(const SchKeys : array of Char) : Char;
label
 exi;
var
 StOrg, StCur: TKeyboardState; i: Integer; Hdl: THandle; nStdHandle: THandle;

 // Original: https://theroadtodelphi.com/
 // Minor modifications.
 // Detect when a key was pressed in the console window
 function KeyPressed : Boolean;
 var
  lpNumberOfEvents     : DWORD;
  lpBuffer             : TInputRecord;
  lpNumberOfEventsRead : DWORD;
 begin
  Result := false;
  lpNumberOfEvents := 0;
  GetNumberOfConsoleInputEvents(nStdHandle, lpNumberOfEvents);
  if lpNumberOfEvents <> 0 then begin
   PeekConsoleInput(nStdHandle, lpBuffer, 1, lpNumberOfEventsRead);
   if lpNumberOfEventsRead <> 0 then begin
    if lpBuffer.EventType = KEY_EVENT then begin
     if lpBuffer.Event.KeyEvent.bKeyDown then
      Result := true
     else
      FlushConsoleInputBuffer(nStdHandle)
     end else
      FlushConsoleInputBuffer(nStdHandle)
    end
  end
 end;
begin
 Result := #0;
 nStdHandle := GetStdHandle(STD_INPUT_HANDLE);
 FlushConsoleInputBuffer(nStdHandle);
 Hdl := GetConsoleWindow;

 Sleep(125);
 FlushConsoleInputBuffer(nStdHandle);
 repeat
  Sleep(25);
  if Hdl = GetForegroundWindow then begin
   for i := 0 to Length(SchKeys) - 1 do
    if GetAsyncKeyState(Ord(SchKeys[i])) <> 0 then begin
     Result := SchKeys[i];
     goto exi;
    end;
   if KeyPressed then goto exi;
  end
 until false;
exi:
 FlushConsoleInputBuffer(nStdHandle);
 Sleep(125)
end;

function AnyKeyToExit(Submit : Char = #0) : Boolean;
begin
 Result := Submit <> #0;
 if Result then begin
  Submit := UpperCase(Submit)[1];
  WriteLn('Press the key "' + Submit + '" to continue or any other key to exit...');
  Result := AnyKey([Submit]) = Submit
 end else
  if bPauseAtEnd then begin
   WriteLn('Press any key to exit...');
   AnyKey([])
  end
end;

initialization
 OSMajorMinorVersion := GetOSMajorMinorVersion;
end.
