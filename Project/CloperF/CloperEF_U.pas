unit CloperEF_U;

interface

type
 THelpSctn = record
   SID, Name: String;
 end;
 THelpScns = array of THelpSctn;

function HelpSections : THelpScns;
function WriteHelp(const sect : PWideChar) : PWideChar;

function MaskFits(const Mask, StrToChk: PWideChar) : Boolean;
function SubStrFittingMask(const Mask, StrToChk: PWideChar;
                             var SubStrPos: Integer): PWideChar;

function FindPathOnDisk(const SchPth: PWideChar; bAnyCase: Boolean = true) : PWideChar;
function FindFileOnDisk(const SchPth: PWideChar; bAnyCase: Boolean = true) : PWideChar;

function FindPathsOnDisk(const SchPth: PWideChar; bAnyCase: Boolean = true): PWideChar;
function FindFilesOnDisk(const SchPth: PWideChar; bAnyCase: Boolean = true): PWideChar;
function FindPathsToFiles(const SchPth: PWideChar; bAnyCase: Boolean = true): PWideChar;

function FindPathToFile(const SchPth: PWideChar; bAnyCase: Boolean = true): PWideChar;

implementation
uses
 StrUtils, StrUtilZ, DiscFuns;

function HelpSections : THelpScns;
begin
 SetLength(Result, 2);
 Result[0].SID := 'fol';
 Result[0].Name := 'The paths specifications for volumes';
 Result[1].SID := 'msk';
 Result[1].Name := 'The search mask'
end;

function WriteHelp(const sect : PWideChar) : PWideChar;
var
 sResult : String;

 procedure EchoMsg(str : String = ''; Indent : Byte = 0);
 begin
  if sResult <> '' then sResult := sResult + '|';
  sResult := sResult + LeftPad('', Indent, ' ') + str
 end;
 procedure MskInfo;
 begin
  EchoMsg('The search mask is used to search values by their string names.');
  EchoMsg;
  EchoMsg('The mask directives precedes mask string and may be the next:');
  EchoMsg(' 0. "$digit1..digit2$" - expression to specify position of substring.');
  EchoMsg('     Example: $11..17$ - search substring between positions 10 & 18;');
  EchoMsg(' 1. "_"                - case insensitive comparison;');
  EchoMsg(' 2. "[" & "]"          - embracing brackets to trim strings before checks.');
  EchoMsg('                         To trim & any case use "_[" at the left of mask.');
  EchoMsg;
  EchoMsg('Mask wildcards:');
  EchoMsg(' 5. "#"                - any digital substring;');
  EchoMsg(' 6. "?"                - any symbol should be present at position;');
  EchoMsg(' 7. "*"                - arbitrary substring with any symbols & length.');
  EchoMsg;
  EchoMsg('Derived wildcards:');
  EchoMsg(' 8. "#[Syms1#Syms2#..]           - digits with user specified format;');
  EchoMsg('                                   Syms - custom delimiters between digits;');
  EchoMsg(' 9. "#"[Syms1"#"Syms1"#..^|F^|D^|T] - dates with specified format ');
  EchoMsg('     Example: “#[#-#-# #:#^|dd-mm-yy hh:nn^|-^|:]”.');
  EchoMsg;
  EchoMsg('Notes:');
  EchoMsg('    - use "*" before constant digital substring to separate wildcard "#" and');
  EchoMsg('      constant digital substring.');
  EchoMsg('      Example: mask “#*96” fits string "3.141596";');
  EchoMsg('    - empty string fits "*" mask;');
  EchoMsg('    - the escape caret symbol "^" before wildcard symbol allows to use it as');
  EchoMsg('      usual symbol, the double "^^" corresponds to one "^" in the mask.');
  EchoMsg;
  EchoMsg('Mask wildcards may go in arbitrary order in string, they may reside between');
  EchoMsg('constant substrings in arbitrary positions, example: "*Hello?world*".');
 end;
 procedure FldInfo;
 begin
  EchoMsg('The tool uses path filters to find & delete disk items before each');
  EchoMsg('launch of optimization task. The filters are to be specified using');
  EchoMsg('fields "Folders#" & "Files#" of configuration file, where symbol #');
  EchoMsg('denotes number of filter and must start from ''1''.');
  EchoMsg;
  EchoMsg('The filters are used to search items on each volume of source disk,');
  EchoMsg('in the case if volume has NTFS file system and active USN journal.');
  EchoMsg;
  EchoMsg('The preliminary probe of filter result can be checked with mounted');
  EchoMsg('source and target disk by using special call with next parameters:');
  EchoMsg('> cloper /S:<Serial numbers> /SDD:<Directories search filter>');
  EchoMsg('> cloper /S:<Serial numbers> /SDF:<Files search filter>');
  EchoMsg;
  EchoMsg('The filter''s format and notes:');
  EchoMsg;
  EchoMsg('The tool doesn''t use volume labels, the path must start with root');
  EchoMsg('folder name.');
  EchoMsg('Example: Program Files (x86)\Common Files\Services');
  EchoMsg;
  EchoMsg('Any substring of paths may specify exact name of item by constant');
  EchoMsg('string or may be the search mask to select several volume items.');
  EchoMsg('Example: Program Files*\Common Files\Services');
  EchoMsg;
  EchoMsg('Use <RootPath>\...\<SubPath> to search all subitems.');
  EchoMsg('Example: Program Files (x86)\...\Services');
  EchoMsg('         will search "Services" items in all subfolders.');
 end;
begin
 if sect = 'msk' then
  MskInfo
 else
  if sect = 'fol' then
   FldInfo;

 Result := PWideChar(sResult)
end;

procedure RemCWC(var Mask : String); begin
 Mask := ReplaceStr(Mask, #1 + '###', #1 + '#');
 Mask := ReplaceStr(Mask, #1 + '##' , #1 + '#');
 Mask := ReplaceStr(Mask, #1 + '***', #1 + '*');
 Mask := ReplaceStr(Mask, #1 + '**' , #1 + '*');
 Mask := ReplaceStr(Mask, #1 + '???', #1 + '?');
 Mask := ReplaceStr(Mask, #1 + '??' , #1 + '?')
end;

function MaskFits(const Mask, StrToChk : PWideChar) : Boolean;
var
 _Mask : String;
begin
 if IsMask(Mask) then _Mask := Mask else _Mask := PrepareMask(Mask, []);
 RemCWC(_Mask);
 Result := StrUtilZ.MaskFits(_Mask, StrToChk)
end;

function SubStrFittingMask(const Mask, StrToChk: PWideChar;
                             var SubStrPos: Integer): PWideChar;
var
 _Mask : String;
begin
 if IsMask(Mask) then _Mask := Mask else _Mask := PrepareMask(Mask, []);
 RemCWC(_Mask);
 Result := PWideChar(StrUtilZ.SubStrFittingMask(_Mask, StrToChk, SubStrPos))
end;

function FindPathOnDisk(const SchPth : PWideChar;
                               bAnyCase : Boolean = true) : PWideChar;
begin
 Result := PWideChar(DiscFuns.FindPathOnDisk(SchPth, bAnyCase))
end;

function FindPathsOnDisk(const SchPth : PWideChar;
                             bAnyCase : Boolean = true) : PWideChar;
begin
 Result := PWideChar(JoinArray(DiscFuns.FindPathsOnDisk(SchPth, bAnyCase), '|'))
end;

function FindFileOnDisk(const SchPth : PWideChar;
                            bAnyCase : Boolean = true) : PWideChar;
begin
 Result := PWideChar(DiscFuns.FindFileOnDisk(SchPth, bAnyCase));
end;

function FindFilesOnDisk(const SchPth : PWideChar;
                             bAnyCase : Boolean = true) : PWideChar;
begin
 Result := PWideChar(JoinArray(DiscFuns.FindFilesOnDisk(SchPth, bAnyCase), '|'))
end;

function FindPathsToFiles(const SchPth : PWideChar;
                             bAnyCase : Boolean = true) : PWideChar;
begin
 Result := PWideChar(JoinArray(DiscFuns.FindPathsToFiles(SchPth, bAnyCase), '|'))
end;

function FindPathToFile(const SchPth : PWideChar;
                            bAnyCase : Boolean = true) : PWideChar;
begin
 Result := PWideChar(DiscFuns.FindPathToFile(SchPth, bAnyCase))
end;

end.
