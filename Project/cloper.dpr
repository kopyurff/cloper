{ cloper
  The Cloning Optimizer (Cloper) project file.

  The project unit contains calls of tool tasks according given parameterization.

* * * * * * * * * * * * *                             * * * * * * * * * * * * *
*
* Cloper - Cloning Optimizer, v1.12
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
* * * * * * * * * * * * * End User License Agreement: * * * * * * * * * * * * *
*
*    This program is free software: you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation, either version 3 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
* * * * * * * * * * * * *                             * * * * * * * * * * * * *
}
program cloper;
{$APPTYPE CONSOLE}
{$R *.res}
{$R *.dres}

uses
  System.SysUtils,
  StrUtils,
  System.Types,
  Windows,
  StrTools in 'Lib\StrTools.pas',
  ConTools in 'Lib\ConTools.pas',
  Defragger in 'Lib\Defragger.pas',
  DskTools in 'Lib\DskTools.pas',
  VolFiles in 'Lib\VolFiles.pas',
  MftQueryUsn in 'Lib\MftQueryUsn.pas',
  VolTools in 'Lib\VolTools.pas',
  cloper_funs in 'cloper_funs.pas',
  cloper_typs in 'cloper_typs.pas',
  cloper_base in 'cloper_base.pas',
  cloper_clon in 'cloper_clon.pas',
  cloper_topt in 'cloper_topt.pas',
  cloper_conf in 'cloper_conf.pas',
  cloper_zero in 'cloper_zero.pas',
  cloper_echo in 'cloper_echo.pas',
  cloper_oper in 'cloper_oper.pas',
  cloper_scrn in 'cloper_scrn.pas',
  cloper_into in 'cloper_into.pas',
  cloper_hash in 'cloper_hash.pas',
  cloper_byte in 'cloper_byte.pas',
  cloper_spec in 'cloper_spec.pas';

function StartedToDoOptimization(Serials : String) : Boolean;
label
 exi;
var
 Config : String; NumPart, NumDskSrc, NumDskTgt : Integer;
 dtco : TDateTime;
begin
 Result := false;
 if ReadParam('Config', Config) then
  CfgFile := Config;

 bOptTask := true;
 if CheckCfgExists then begin
  ReadCommonCfgValues;
  DefineTimeShift
 end else begin
  EchoFailure(1); exit
 end;
 if CheckOtherInstancesRun then begin
  EchoFailure(22); exit
 end;
 ModifyCaption(true);
 PrintHeader;
 OutputMessage(mLogNoDate, ' ');
 dtco := Now;
 bUseUsnQueries := ReadCfgValue('CommonSettings', 'UseUsnQueries', 'not') = 'yes';

 NumPart := GetVolumeNumbersBySerials(Serials, NumDskSrc, SrcVolSpec.VolumeNumber,
                                               NumDskTgt, TgtVolSpec.VolumeNumber,
                                               []);
 if NumPart < 0 then
  if SearchResolveSignatureCollision(Serials) then begin
   NumPart := GetVolumeNumbersBySerials(Serials, NumDskSrc, SrcVolSpec.VolumeNumber,
                                                 NumDskTgt, TgtVolSpec.VolumeNumber,
                                                 []);
   OutputMessage(mLogNoDate, ' ')
  end;
 OutputMessage(mLogHighPri, 'Started cloning optimization task:');

 if not PrintOptTskInfo(Serials, NumDskSrc, NumDskTgt) then
  exit;
 if NumPart < 0 then if EchoFailure(21) then
  exit;

 if not CleanupsAndOptimizations(NumDskSrc, NumDskTgt, NumPart) then
  goto exi;

 if not CloneDisk(NumDskSrc, NumDskTgt) then
  goto exi;

 if bRestoreLetters then
  if GetAssignLetters(false)[0] <> '' then
   if bActivateTgtDsk then
    if DiskLabels(NumDskTgt, TgtLabels, dlSet, TgtLabels, true) then
     OutputMessage(mEcoNoDtHP, '<align>Restored volume labels of target disk')
    else
     EchoFailure(24)
   else
    if DiskLabels(NumDskSrc, SrcLabels, dlSet, SrcLabels, true) then
     OutputMessage(mEcoNoDtHP, '<align>Restored volume labels of source disk')
    else
     EchoFailure(23)
  else
   OutputMessage(mEcoNoDtHP, '<align>There is not %s volume labels to restore...',
                             [SelStr(bActivateTgtDsk, 'target', 'source')])
 else
  if GetAssignLetters[0] <> '' then
   if DiskLabels(NumDskTgt, SrcLabels, dlSet, TgtLabels, true) then
    OutputMessage(mEcoNoDtHP, '<align>Assigned volume labels to target disk')
   else
    EchoFailure(24);

 RemoveEmptyTestingReports;
 OutputMessage(mLogNoDate, PadRight('', 79, #$2500));
 OutputMessage(mLogHighPri,
               'The tool completed all tasks in %s', [GetDHMS(Now - dtco)]);
 OutputMessage(mLogNoDate, ' ');

 if not SilentMode then AnyKeyToExit;
 Result := true;
exi:
 ModifyCaption(false)
end;

var
 Serials, DskVolLab, SchPath, NewID, sShort, sHelp: String;
 bResult: Boolean;
begin
 if not InitCloperFLib then if EchoFailure(19) then exit;

 if ReadParam('Serials', Serials) or ReadParam('s', Serials) then begin
  if ReadParam('ShowDiskDirs', SchPath) or ReadParam('sdd', SchPath) then
   ShowDiskItems(true, SchPath, Serials)
  else
   if ReadParam('ShowDiskFiles', SchPath) or ReadParam('sdf', SchPath) then
    ShowDiskItems(false, SchPath, Serials)
   else begin
    bResult := StartedToDoOptimization(Serials);
    if SilentMode then
     if bResult then
      writeln('CLOPER=PASS')
     else
      writeln('CLOPER=FAIL')
   end
 end else
  if ReadParam('GetConfig', sShort) or ReadParam('gc', sShort) then
   WriteNewCfgFile(sShort = 'short')
  else
   if ReadParam('GetSerials', DskVolLab) or ReadParam('gs', DskVolLab) then
    writeln('Serials: ' + GetDiskSerials(DskVolLab))
   else
    if ReadParam('SignDisk', NewID) or ReadParam('sd', NewID) then
     SignDisk(NewID)
    else
     if ReadParam('Help', sHelp) or ReadParam('h', sHelp) then
      PrintHelp(sHelp)
      else
       PrintDefaultOutput;
 DeleteCloperFLib
end.
