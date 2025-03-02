{ cloper_scrn
  The unit of Cloning Optimizer (Cloper) tool.

  The file contains functionality to perform screen output.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit cloper_scrn;

interface
uses
 Types;

procedure PrintShortHeader(bShowAuthor : Boolean = true);
procedure ModifyCaption(bSet : Boolean);
function  PrintHeader(Typ : Byte = 0): TStringDynArray;
          // PrintOptTskInfo initialises DisksStyle.
function PrintOptTskInfo(Serials: String; SrcDskInfo, TgtDskInfo: Integer): Boolean;
procedure PrintDefaultOutput;
procedure PrintHelp(sHelp : String);

implementation
uses
 SysUtils, StrUtils, cloper_base, cloper_echo, cloper_conf, ConTools,
 cloper_typs, cloper_into, Windows, DskTools, StrTools;

procedure PrintShortHeader(bShowAuthor : Boolean = true);
begin
 if bShowAuthor then Writeln;
 Writeln('Cloper - Cloning Optimizer ' + ToolPlatform + ', v' + GetToolVersionData(tvdVersion));
 if bShowAuthor then
  Writeln('Copyright (C) ' + GetToolVersionData(tvdDates) + ' ' +
     GetToolVersionData(tvdAuthor));
 Writeln
end;

var
 sOrigCap : String = '';
procedure ModifyCaption(bSet : Boolean);
var
 Hdl : THandle;
begin
 Hdl := GetConsoleWindow;
 if bSet then begin
  if sOrigCap = '' then
   sOrigCap := GetWinCaption(Hdl);
  SetWinCaption(Hdl, 'cloper v' + GetToolVersionData(tvdVersion) + '  (c) ' +
     GetToolVersionData(tvdAuthor));
 end else
  SetWinCaption(Hdl, sOrigCap);
end;

function PrintHeader(Typ : Byte = 0): TStringDynArray;
const
 Header : array[0..2] of TCRTColumn = ((Align:crtLeft ;TextColor:Word(ccBlack)),
                                       (Align:crtRight;TextColor:Word(ccBlack)),
                                       (Align:crtRight;TextColor:Word(ccBlack)));
var
 HeadStr : TStringDynArray;
 CRTGrid : TCRTGrid; i, ot : Integer; bsm : Boolean;
begin
 CRTGrid := TCRTGrid.Create(0, Word(ccDarkGray), true, false, Word(ccBlack), 79);
 CRTGrid.AddRows(Header);
 HeadStr := SplitString(' Cloper - Cloning Optimizer ' + ToolPlatform + ', v%s'
    + #1 + 'Copyright (C) %s ' + GetToolVersionData(tvdAuthor) + ' ' + #1 +
    'GNU General Public License ', #1);
 for i := 0 to 1 do
  HeadStr[i] := Format(HeadStr[i], [GetToolVersionData(TTlVnData(i))]);

 Result := CRTGrid.DrawAlignedRows(HeadStr, SilentMode or (Typ = 2));
 bsm := SilentMode; SilentMode := true; ot := OutType; OutType := Integer(oLogFile);
 if Typ < 2 then begin
  for i := 0 to Length(Result) - 1 do
   if Typ = 0 then
    OutputMessage(mLogNoDate, Result[i])
   else
    OutputMessage(mEcoNoDtHP, Result[i]);
  SetLength(Result, 0)
 end;
 SilentMode := bsm; OutType := ot
end;

function PrintOptTskInfo(Serials: String; SrcDskInfo, TgtDskInfo: Integer): Boolean;
var
 sDskID, sDskStyle: String; TgDskStyle: Byte; Atmps: Cardinal;
begin
 OutputMessage(mLogNoDate, 'Guest:<align>OS ' + GetWin32_OSName + ', RAM ' + Format('%u', [GetRAMsize]) + ' MB');
 Serials := UpperCase(Serials);
 if 0 < SrcDskInfo then begin
  PartInfoAction(piaGetIDtype, SrcDskInfo, sDskID, DisksStyle);// checked earlier
  case DisksStyle of
  0: sDskStyle := 'RAW'; 1: sDskStyle := 'GPT'; 2: sDskStyle := 'MBR';
  end;
 end else
  sDskStyle := '';
 OutputMessage(mLogNoDate, 'Disks:<align>' + SelStr(0 < SrcDskInfo,
                 'Style %s, volume''s s/n "%s"', [sDskStyle, Serials], 'n/a'
 ));
 OutputMessage(mLogNoDate, 'Source:<align>' +
            SelNSt(SrcDskInfo, '', 'Number ', ', signature $' + sDskID, 'n/a'));
 if 0 < TgtDskInfo then begin
  Atmps := 0;
  repeat
   Sleep(250); Inc(Atmps, 250);
   PartInfoAction(piaGetIDtype, TgtDskInfo, sDskID, TgDskStyle);
   Result := DisksStyle = TgDskStyle;
  until Result or (7500 < Atmps)
 end;
 OutputMessage(mLogNoDate, 'Target:<align>' +
            SelNSt(TgtDskInfo, '', 'Number ', ', signature $' + sDskID, 'n/a'));
 OutputMessage(mLogNoDate, PadRight('', 79, #$2500));
 if not Result then EchoFailure(13)
end;

procedure PrintDefaultOutput;
var
 str : String; LStr : Integer;
begin
 PrintShortHeader;
 str := 'usage: ' + ExtractFileName(ParamStr(0)) + ' ';
 WriteLn(str + '/[getconfig|gc][:short]');
 LStr := Length(str);
 WriteLn(PadLeft('', LStr, ' ') + '/[getserials|gs]:<drive letter>');
 WriteLn(PadLeft('', LStr, ' ') + '/[serials|s]:<serials>');
 WriteLn(PadLeft('', LStr, ' ') + '/[serials|s]:<serials> /[showdiskdirs|sdd]:<folders filter>');
 WriteLn(PadLeft('', LStr, ' ') + '/[serials|s]:<serials> /[showdiskfiles|sdf]:<files filter>');
 WriteLn(PadLeft('', LStr, ' ') + '/[signdisk|sd]:<new signature>');
 WriteLn(PadLeft('', LStr, ' ') + '/[help|h][:<section>]');
 WriteLn('where'); LStr := Length('where');
 WriteLn(PadLeft('', LStr, ' ') + '/serials or /s & <serials>          - '
                                     + 'without other arguments launches');
 WriteLn(PadLeft('', LStr + 38, ' ') + 'optimization of the virtual disk');
 WriteLn(PadLeft('', LStr + 38, ' ') + 'with given serials of volumes.');
 WriteLn;
 WriteLn(PadLeft('', LStr, ' ') + '/getconfig or /gc & "short"         - '
                                     + 'create new default .cfg file,');
 WriteLn(PadLeft('', LStr + 38, ' ') + 'the suffix "short" to drop comments.');
 WriteLn(PadLeft('', LStr, ' ') + '/getserials or /gs & <drive letter> - '
                                     + 'get serials of all volumes on disk,');
 WriteLn(PadLeft('', LStr + 38, ' ') + 'containing volume with drive letter.');
 WriteLn(PadLeft('', LStr, ' ') + '/showdiskdirs or /sdd & <filter>    - '
                                     + 'show all folders matching filter.');
 WriteLn(PadLeft('', LStr, ' ') + '/showdiskfiles or /sdf & <filter>   - '
                                     + 'show all files matching filter.');
 WriteLn(PadLeft('', LStr, ' ') + '/signdisk or /sd & <new signature>  - '
                                     + 'modify MBR/GPT signature of disk.');
 WriteLn(PadLeft('', LStr, ' ') + '/help or /h & <section>             - '
                                     + 'show help, optional suffix <section>');
 WriteLn(PadLeft('', LStr + 38, ' ') + 'to print corresponding topic.');
 WriteLn;
 AnyKeyToExit
end;


procedure PrintHelp(sHelp : String);
type
  THSctn = (hsNon = 0, hsGnl = 1, hsEul = 2, hsBeg = 3,
            hsDtl = 4, hsCfg = 5, hsSrl = 6, hsSpl = 7, hsExt = 8);
const
 NumIntScn = 7;
var
 HelpScns : THelpScns;

 function GetHSctn(sSctn : String; var ind : Integer) : THSctn;
 var
  i : Integer;
 begin
  if sSctn <> #0 then sSctn := LowerCase(Trim(sSctn));
  if(sSctn =    '') or (sSctn = '1') then Result := hsGnl else
   if(sSctn = 'eula') or (sSctn = '2') then Result := hsEul else
    if(sSctn = 'startup') or (sSctn = '3') then Result := hsBeg else
     if(sSctn = 'dtl') or (sSctn = '4') then Result := hsDtl else
      if(sSctn = 'cfg') or (sSctn = '5') then Result := hsCfg else
       if(sSctn = 'srl') or (sSctn = '6') then Result := hsSrl else
        if(sSctn = 'spl') or (sSctn = '7') then Result := hsSpl else
         if (Length(sSctn) = 1)
         and(0 < Pos(sSctn, '123456789'))
         and(NumIntScn < StrToInt(sSctn))
         and(StrToInt(sSctn) < NumIntScn + Length(HelpScns) + 1) then begin
          ind := StrToInt(sSctn) - NumIntScn - 1;
          Result := hsExt
         end else begin
          for i := 0 to Length(HelpScns) - 1 do
           if HelpScns[i].SID = sSctn then begin
            ind := i;
            Result := hsExt;
            exit
           end;
          Result := hsNon
         end
 end;
 procedure EchoMsg(str : String; Indent : Byte = 0);
 begin
  writeln(PadLeft('', Indent, ' ') + str);
 end;
 procedure EchoHeader(str: String; bUnLn: Boolean = true);
 begin
  if bUnLn then begin
   ClrScr; PrintHeader(1); WriteLn
  end else begin
   WriteLn; EchoMsg(PadRight('', 79, #$2500))
  end;
  EchoMsg(str);
  if bUnLn then EchoMsg(PadRight('', 79, #$2500));
  WriteLn
 end;
 function HSInfo(var ind : Integer) : THSctn;
 var
  Scs : String; i : Integer; Ccs : array of Char;
 begin
  Scs := ''; for i := 1 to NumIntScn do Scs := Scs + IntToStr(i);
  EchoHeader('Available help sections:', false);
  EchoMsg(PadLeft('', 4, ' ') + '1. <empty>  -  general information;');
  EchoMsg(PadLeft('', 4, ' ') + '2. eula     -  end user license agreement;');
  EchoMsg(PadLeft('', 4, ' ') + '3. startup  -  configuring system for VMware Player;');
  EchoMsg(PadLeft('', 4, ' ') + '4. dtl      -  details about optimization activities;');
  EchoMsg(PadLeft('', 4, ' ') + '5. cfg      -  fields & sections of configuration file;');
  EchoMsg(PadLeft('', 4, ' ') + '6. srl      -  serial numbers of disk volumes;');
  EchoMsg(PadLeft('', 4, ' ') + '7. spl      -  special calls of tool;');
  for i := 0 to Length(HelpScns) - 1 do begin
   EchoMsg(PadLeft('', 4, ' ') + IntToStr(NumIntScn + 1 + i) + '. ' +
        HelpScns[i].SID + '      -  ' + LowerCase(HelpScns[i].Name) + ';');
   Scs := Scs + IntToStr(NumIntScn + 1 + i);
  end;
  SetLength(Ccs, Length(Scs));
  for i := 0 to Length(Scs) - 1 do Ccs[i] := Scs[i + 1];
  EchoMsg('Press any key to exit or type the number of help section to show topic...');
  GotoXY(1, 0);
  Result := GetHSctn(AnyKey(Ccs), ind)
 end;
 function GnlInfo(var ind : Integer) : THSctn;
 begin
  EchoHeader('General information:');
  EchoMsg('The tool for optimization of virtual disk snapshots.');
  WriteLn;
  EchoMsg('The program is designed to do file size minimization of virtual disk snapshots.');
  EchoMsg('It performs size minimization or optimization process by comparison of the data');
  EchoMsg('of 2 mounted virtual disks in undoable mode, which are the two instances of the');
  EchoMsg('same virtual disk having different data corresponding to different time stamps.');
  EchoMsg('The data of selected source disk is relocated to minimize size of modified part');
  EchoMsg('of disk, after it the free space of the source disk is zeroed and the optimized');
  EchoMsg('result is obtained by writing of source sectors with found modified data to the');
  EchoMsg('target disk. After completion the 2nd target disk contains optimized snapshot.');
  WriteLn;
  EchoMsg('To be able to perform optimization task the guest service OS must enable access');
  EchoMsg('to target & source disks using Windows API functionality, the active OS user of');
  EchoMsg('running task must have administrative privileges. The tool tasks can be started');
  EchoMsg('using Windows OS versions starting with XP SP2 x86/x64. It''s recommended to use');
  EchoMsg('older OS versions to have minimum background data modifications by system.');
  WriteLn;
  EchoMsg('The current version of tool requires the target & source disks to be identical,');
  EchoMsg('that is, the quantity of disk volumes, their sizes and serial numbers should be');
  EchoMsg('the same. It supports only NTFS file system of volumes. The task mode using USN');
  EchoMsg('queries also requires USN NTFS journal to have active state.');
  Result := HSInfo(ind);
 end;
 function EulaInf(var ind : Integer) : THSctn;
 begin
  EchoHeader('End User License Agreement:');
  EchoMsg('This program is free software: you can redistribute it and/or modify');
  EchoMsg('it under the terms of the GNU General Public License as published by');
  EchoMsg('the Free Software Foundation, either version 3 of the License, or');
  EchoMsg('(at your option) any later version.');
  WriteLn;
  EchoMsg('This program is distributed in the hope that it will be useful,');
  EchoMsg('but WITHOUT ANY WARRANTY; without even the implied warranty of');
  EchoMsg('MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the');
  EchoMsg('GNU General Public License for more details.');
  WriteLn;
  EchoMsg('You should have received a copy of the GNU General Public License');
  EchoMsg('along with this program.  If not, see <http://www.gnu.org/licenses/>.');
  WriteLn;
  EchoMsg('To show help section use their short codes after ":" in the command line.');
  Result := HSInfo(ind);
 end;
 function BegInfo(var ind : Integer) : THSctn;
 begin
  EchoHeader('Help section: configuring system on the example of VMware Player.');
  EchoMsg('The tool supports all Windows systems since Windows XP SP2 and can be');
  EchoMsg('used with guest VMs running x86 & x64 OS. The x64 version of the tool');
  EchoMsg('can be used only on x64 platforms. To be able to run tasks, the guest');
  EchoMsg('VM user must have administrative privileges. The host system used for');
  EchoMsg('optimization task must enable mounting of 2 snapshots of virtual disk');
  EchoMsg('and allow to work with their data using Windows API. This description');
  EchoMsg('is given for VMware Player.');
  WriteLn;
  EchoMsg('Typical steps to run optimization task:');
  EchoMsg('1. Select VM for this task & open it''s folder in the explorer;');
  EchoMsg('2. Create subfolders Source & Target, copy .vmdk files into each one;');
  EchoMsg('   The .vmdk files are persistent part of undoable disks and must be');
  EchoMsg('   compatible with snapshots (ie, they must belong to the same VM).');
  EchoMsg('3. Copy snapshot for optimization into Source subfolder;');
  EchoMsg('4. Open .vmx file of the VM with text editor and add 2 disks:');
  EchoMsg('         scsi0:1.deviceType = "scsi-hardDisk"');
  EchoMsg('         scsi0:1.present = "TRUE"');
  EchoMsg('         scsi0:1.fileName = ".\Source\<Disk name>.vmdk"');
  EchoMsg('         scsi0:1.mode = "undoable"');
  EchoMsg('         scsi0:1.redo = ".\Source\<Snapshot name>.REDO"');
  EchoMsg('         scsi0:2.deviceType = "scsi-hardDisk"');
  EchoMsg('         scsi0:2.present = "TRUE"');
  EchoMsg('         scsi0:2.fileName = ".\Target\<Disk name>.vmdk"');
  EchoMsg('         scsi0:2.mode = "undoable"');
  EchoMsg('         scsi0:2.redo = ""');
  EchoMsg('   The name of target snapshot will be filled automatically later by');
  EchoMsg('   VM Player. The source disk is recommended to add 1st to have it''s');
  EchoMsg('   identifier unchanged and to have target bootable after completion.');
  EchoMsg('5. Save edits and start VM;');
  EchoMsg('6. In the command line of started VM run "diskmgmt.msc";');
  EchoMsg('7. Check the source disk is present and has at least one volume label');
  EchoMsg('   (for instance "L");');
  EchoMsg('8. Copy tool into arbitrary folder of guest VM (it can not be started');
  EchoMsg('   from processed disks, the volume of any other disk);');
  EchoMsg('9. Run tool "cloper /gs:L>>Serials.txt" to write serials into file;');
  EchoMsg('A. Run tool "cloper /gc" to create default .cfg file;');
  EchoMsg('B. Copy the stored serials string from file and run optimization task');
  EchoMsg('   by the command: "cloper /s:<Serials string>" ');
  EchoMsg('C. After task completion turn off the VM. The autocreated snapshot of');
  EchoMsg('   target disk will contain optimized snapshot.');
  WriteLn;
  EchoMsg('To run task in silent mode open .cfg file and change value "RunMode"');
  EchoMsg('from "ECHO" to "SILENT". The batch file command to start task can be:');
  WriteLn;
  EchoMsg('  @echo off');
  EchoMsg('  for /F "eol=; tokens=1,2 delims==" %%i in (');
  EchoMsg('   ''<Tool path>\cloper /s:<Serials string>''');
  EchoMsg('  ) do if "%%j"=="PASS" (goto :MyPassLabel) else (goto :MyFailLabel)');
  Result := HSInfo(ind)
 end;
 function DtlInfo(var ind : Integer) : THSctn;
 begin
  EchoHeader('Help section: Details and remarks on optimization process.');
  EchoMsg('Depending on size of persistent or existing data on source & target');
  EchoMsg('disks and depending on host system performance the all optimization');
  EchoMsg('process can take significant amount of time (several hours).');
  WriteLn;
  EchoMsg('The tool performs low level activities with data. It is recommended');
  EchoMsg('to do reserve copy of virtual disk snapshot before use it in tool.');
  WriteLn;
  EchoMsg('After completion of optimization sequence, the free space of source');
  EchoMsg('disk will be initialized by data bytes. The size of the source disk');
  EchoMsg('snapshot will reach size of free space in the persistent version of');
  EchoMsg('disk.');
  WriteLn;
  EchoMsg('During activities with data the API functions can fail, the failure');
  EchoMsg('details are printed as error messages. The interruption of the task');
  EchoMsg('happens only in the case of severe unrecoverable errors.');
  WriteLn;
  EchoMsg('The tool writes screen output only in the ECHO run mode, the SILENT');
  EchoMsg('mode is designed for use inside automation scripts. The only screen');
  EchoMsg('output in the latter case is PASS or FAIL.');
  Result := HSInfo(ind);
 end;
 function CfgInfo(var ind : Integer) : THSctn;
 begin
  EchoHeader('Help section: Fields & sections of configuration file.');
  EchoMsg('Sections of configuration file:');
  EchoMsg('1. CommonSettings - fields to define run mode & log output parameters;', 2);
  EchoMsg('2. VolumeCleanup - search filters for files & folders to delete before task;', 2);
  EchoMsg('3. Optimization - fields defining optimization parameters;', 2);
  EchoMsg('4. Cloning - fields for data transfer parameters & task finalization;', 2);
  EchoMsg('5. Testing - fields to write lists of problematic data to separate text files', 2);
  WriteLn;
  EchoMsg('Fields of configuration file:');
  EchoMsg('1. Section "CommonSettings":', 2);
  EchoMsg('RunMode            - SILENT for auto-scripts, default ECHO to show messages;', 3);
  EchoMsg('PauseAtEnd         - pause tasks with screen echo until user input;', 3);
  EchoMsg('IdSourceLssTarget  - the source disk is plugged first and has smaller number', 3);
  EchoMsg(                      'than target disk (YES/NOT, default YES). This order is', 24);
  EchoMsg(                      'needed to keep disk signature and bootable OS at end;', 24);
  EchoMsg('UseUsnQueries      - query NTFS USN journal to get volume items (YES) or get', 3);
  EchoMsg(                      'items using plain scan (NOT), default NOT;', 24);
  EchoMsg('MinVolumeSize      - minimum volume size in MBytes  (0..1000, default 750);', 3);
  EchoMsg('LogFile            - write all messages to .log file (YES/NOT, default NOT);', 3);
  EchoMsg('LogLowPriErrors    - log low priority errors (YES/NOT, default NOT);', 3);
  EchoMsg('LogLowPriSysErrors - log low priority system errors (YES/NOT, default NOT);', 3);
  EchoMsg('LogLowPriMessages  - log low priority messages (YES/NOT, default NOT);', 3);
  EchoMsg('ProcessPriority    - the process priority of running tool', 3);
  EchoMsg(                      '(NORMAL, HIGH or REALTIME, default HIGH);', 24);
  EchoMsg('EchoLowPriMessages - print on screen low priority messages;', 3);
  EchoMsg('TimeStamp          - creation date time of this file, format YYMMDDHHMMSS;', 3);
  EchoMsg('2. Section "VolumeCleanup":', 2);
  EchoMsg('Folders[1,2..] - paths to folders to delete before optimization;', 3);
  EchoMsg('Files[1,2..]   - paths to files to delete before optimization;', 3);
  EchoMsg('3. Section "Optimization":', 2);
  EchoMsg('MatchBlocks             - confront source files with matched target data', 3);
  EchoMsg(                          '(YES/NOT, default YES);', 29);
  EchoMsg('ModificationThreshold   - the modifications threshold for coinciding files', 3);
  EchoMsg(                          'in percents (0..75%, default 48);', 29);
  EchoMsg('MemoryUseLimit          - upper size limit for the data used to search fits', 3);
  EchoMsg(                          '(100..1500 MB, default 900 MB);', 29);
  EchoMsg('ClusterWise             - fit clusters (YES) or sectors (NOT), default YES;', 3);
  EchoMsg('MoveToBlocks            - populate all areas confronting target segments by', 3);
  EchoMsg(                          'source files (YES/NOT, default YES);', 29);
  EchoMsg('MoveNewData             - defragment new data (YES/NOT, default YES);', 3);
  EchoMsg('ConsolidateNewData      - consolidate all new data (YES/NOT, default YES);', 3);
  EchoMsg('PatchFreeSpace          - patch free space of source by target data', 3);
  EchoMsg(                          '(YES/NOT, default YES);', 29);
  EchoMsg('CompressData            - compress files using NTFS (YES/NOT, default YES);', 3);
  EchoMsg('NtfsCompressionType     - DEFAULT or LZNT1 type, DEFAULT is default value;', 3);
  EchoMsg('SkipCompressionForFiles - mask to skip files compression, e.g: *.cab|*.mp4;', 3);
  EchoMsg('ReportFoundSkipFiles    - report uncompressed files (YES/NOT, default YES);', 3);
  EchoMsg('4. Section "Cloning":', 2);
  EchoMsg('BufferSizeKB       - read buffer in KB, default 5120 KB, maximum 10240 KB;', 3);
  EchoMsg('ActivateTargetDisk - activate target, offline source (YES/NOT, default YES);', 3);
  EchoMsg('AssignLetters      - C..Z, assign labels to active volumes of online disk;', 3);
  EchoMsg('RestoreLetters     - set original labels to online disk after task end', 3);
  EchoMsg('(YES/NOT, default NOT);', 24);
  EchoMsg('5. Section "Testing":', 2);
  EchoMsg('ReportFiles    - write lists of problematic files (YES/NOT, default NOT);', 3);
  EchoMsg('EndUserLetters - (C,D,..,Z) labels of target volumes used by the end user VM', 3);
  Result := HSInfo(ind);
 end;
 function SrlInfo(var ind : Integer) : THSctn;
 begin
  EchoHeader('Help section: Serial numbers of disk volumes.');
  EchoMsg('The string of serial numbers is used for identification of disks,');
  EchoMsg('which must be processed. The serials string should contain serial');
  EchoMsg('numbers of all volumes on disk in hexadecimal format delimited by');
  EchoMsg('"-" symbol.');
  WriteLn;
  EchoMsg('The string sample for disk with 2 volumes: 22D5-EA43-E6E8-9614');
  Result := HSInfo(ind)
 end;
 function SplInfo(var ind : Integer) : THSctn;
 begin
  EchoHeader('Help section: Special calls of tool.');
  EchoMsg('The special calls of the tool is set to perform auxiliary tasks.');
  WriteLn;
  EchoMsg('Allowed next calls:');
  EchoMsg(' 1. To create file with new settings start it with GetSerials or');
  EchoMsg('    GC. The suffix ":short" allows to drop comments in new file.');
  EchoMsg(' 2. To obtain serial numbers of selected disk start it with keys');
  EchoMsg('    GetSerials or GS. The disk must have no less than one volume');
  EchoMsg('    label. This label is used as suffix ":<Label symbol>".');
  EchoMsg(' 3. To show selections of files and folders call it with serials');
  EchoMsg('    and ShowDiskFiles (SDF) or ShowDiskDirs (SDD).');
  EchoMsg('    Example: cloper /S:2A1B-4754-521C-5E30 /SDF:...\*.cab');
  EchoMsg(' 4. To change signature of disk call it with key SignDisk or SD,');
  EchoMsg('    the suffix after ":" must contain valid MBR/GPT value of ID.');
  EchoMsg('    The start without ID suffix shows information about attached');
  EchoMsg('    disks.');
  EchoMsg(' 5. To show help topics run program with key "H", to show single');
  EchoMsg('    selected help section use it''s short code after ":".');
  Result := HSInfo(ind)
 end;
 function ExtInfo(var ind : Integer) : THSctn;
 var
  Txt : TStringDynArray; i : Integer;
 begin
  EchoHeader('Help section: ' + HelpScns[ind].Name + '.');
  Txt := WriteExHelp(HelpScns[ind].SID);
  for i := 0 to Length(Txt) - 1 do EchoMsg(Txt[i]);
  Result := HSInfo(ind)
 end;
var HSctn : THSctn; ind : Integer; bow : Boolean; begin
 ShowHideCaret(shcHide);
 HelpScns := HelpSections;
 bow := IsProcessInsideWindowContainer([]);
 if bow then begin
  ModifyCaption(true);
  ShowWindow(GetConsoleWindow, SW_SHOWMAXIMIZED);
 end;

 HSctn := GetHSctn(sHelp, ind);
 repeat
  case HSctn of
   hsGnl: HSctn := GnlInfo(ind); hsEul: HSctn := EulaInf(ind);
   hsBeg: HSctn := BegInfo(ind); hsDtl: HSctn := DtlInfo(ind);
   hsCfg: HSctn := CfgInfo(ind); hsSrl: HSctn := SrlInfo(ind);
   hsSpl: HSctn := SplInfo(ind); hsExt: HSctn := ExtInfo(ind);
  end
 until HSctn = hsNon;

 ShowHideCaret(shcRestore);
 if bow then
  ModifyCaption(false)
end;

end.
