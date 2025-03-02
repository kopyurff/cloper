{ cloper_into
  The unit of Cloning Optimizer (Cloper) tool.

  The file encapsulates routines to call exported functions of CloperF library.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit cloper_into;

interface
uses
 Types;

type
 THelpSctn = record
   SID, Name: String;
 end;
 THelpScns = array of THelpSctn;

function InitCloperFLib : Boolean;
function LoadCloperF: HMODULE;
procedure DeleteCloperFLib;
function WriteExHelp(sect : String) : TStringDynArray;
function HelpSections : THelpScns;

function MaskFits(const Mask, StrToChk : String; hDLL : HMODULE = 0) : Boolean;
function SubStrFittingMask(const Mask, StrToChk: String;
                             var SubStrPos: Integer; hDLL : HMODULE = 0): String;

function FindPathsOnDisk(const SchPth: String;
                             bAnyCase: Boolean = true) : TStringDynArray;
function FindPathOnDisk(const SchPth: String;
                            bAnyCase: Boolean = true) : String;
function FindFileOnDisk(const SchPth: String;
                            bAnyCase: Boolean = true) : String;
function FindFilesOnDisk(const SchPth: String;
                             bAnyCase: Boolean = true) : TStringDynArray;
function FindPathToFile(const SchPth: String;
                            bAnyCase: Boolean = true) : String;
function FindPathsToFiles(const SchPth: String;
                              bAnyCase: Boolean = true) : TStringDynArray;

implementation
uses
 Windows, SysUtils, Classes, StrUtils, cloper_base;

const
 DllFilMask = 'CloperFx' + {$IFDEF WIN32}'86'{$ELSE}'64'{$ENDIF} + '#%d.dll';
 DllResName = 'RCloperF_x' + {$IFDEF WIN32}'86'{$ELSE}'64'{$ENDIF};
var
 DllFilName: String = '';

{ SaveResourceLibrary
  Saves library to file, tries to delete it if exists with purpose to check lock.
}
function SaveResourceLibrary(const ADllResName: string; bDel: Boolean = true): Boolean;
 function DeleteSavedLibrary: Boolean;
 var
  path: String; i: Integer;
 begin
  Result := true;
  path := GetEnvironmentVariable('TEMP');
  if path = '' then path := ExtractFilePath(ParamStr(0));
  path := IncludeTrailingPathDelimiter(path);
  i := 1;
  while FileExists(Format(path + DllFilMask, [i])) do begin
   DeleteFileNT32(Format(path + DllFilMask, [i]));
   Inc(i);
  end;
  i := 1;
  while FileExists(Format(path + DllFilMask, [i])) do Inc(i);
  DllFilName := Format(path + DllFilMask, [i]);
  Result := false
 end;
var
 oRs: TResourceStream;
begin
 if bDel or (DllFilName = '') then if DeleteSavedLibrary then exit;
 Result := false;
 if FileExists(DllFilName) then
  Result := true
 else
  if FindResource(hInstance, PWideChar(ADllResName), RT_RCDATA) > 0 then begin
   oRs := TResourceStream.Create(hInstance, ADllResName, RT_RCDATA);
   try
    oRs.SaveToFile(DllFilName); Sleep(10);
    Result := true
   finally
    oRs.Free
   end
  end
end;

function InitCloperFLib: Boolean;
var
 hDLL : HMODULE;
begin
 if SaveResourceLibrary(DllResName) then begin
  hDLL := LoadLibrary(PWideChar(DllFilName));
  Result := hDLL <> 0;
  if Result then FreeLibrary(hDLL) else begin
   SaveResourceLibrary(DllResName, true);
   hDLL := LoadLibrary(PWideChar(DllFilName));
   Result := hDLL <> 0;
   if Result then FreeLibrary(hDLL)
  end
 end else
  Result := false
end;

function LoadCloperF: HMODULE;
begin
 if SaveResourceLibrary(DllResName, false) then begin
  Result := LoadLibrary(PWideChar(DllFilName));
 end else
  Result := 0;
 Assert(Result <> 0, 'Failed to load library "' + DllFilName + '"')
end;

procedure DeleteCloperFLib;
begin
 DeleteFileNT32(DllFilName)
end;

function GetString(ExStr: String): String;
var
 i : Integer;
begin
 Result := ''; for i := 0 to Length(ExStr) - 1 do Result := Result + ExStr.Chars[i]
end;

function GetArray(str : PWideChar) : TStringDynArray;
var
 i : Integer;
begin
 Result := SplitString(ReplaceStr(GetString(str), '^|', #1), '|');
 for i := 0 to Length(Result) - 1 do
  Result[i] := ReplaceStr(Result[i], #1, '|');
 if Length(Result) = 0 then begin
  SetLength(Result, 1); Result[0] := ''
 end;
end;

function HelpSections : THelpScns;
var
 hDLL : HMODULE;
 HelpSections : function() : THelpScns;
 HelpScns : THelpScns;
 i : Integer;
begin
 hDLL := LoadCloperF;
 @HelpSections := GetProcAddress(hDLL, 'HelpSections');
 if Assigned(@HelpSections) then
  HelpScns := HelpSections;
 SetLength(Result, Length(HelpScns));
 for i := 0 to Length(HelpScns) - 1 do begin
  Result[i].SID := GetString(HelpScns[i].SID);
  Result[i].Name := GetString(HelpScns[i].Name)
 end;
 SetLength(HelpScns, 0);
 FreeLibrary(hDLL)
end;

function WriteExHelp(sect : String) : TStringDynArray;
var
 hDLL : HMODULE;
 WriteHelp : function(sect : PWideChar) : PWideChar;
begin
 hDLL := LoadCloperF;
 @WriteHelp := GetProcAddress(hDLL, 'WriteHelp');
 if Assigned(@WriteHelp) then
  Result := GetArray(WriteHelp(PWideChar(sect)))
 else begin
  SetLength(Result, 1); Result[0] := ''
 end;
 FreeLibrary(hDLL)
end;

function MaskFits(const Mask, StrToChk : String; hDLL : HMODULE = 0) : Boolean;
var
 MaskFitsEx : function(const _Mask, _StrToChk : PWideChar) : Boolean;
 bDLL: Boolean;
begin
 bDLL := hDLL = 0;
 if bDLL then hDLL := LoadCloperF;
 @MaskFitsEx := GetProcAddress(hDLL, 'MaskFits');
 if Assigned(@MaskFitsEx) then
  Result := MaskFitsEx(PWideChar(Mask), PWideChar(StrToChk))
 else
  Result := false;
 if bDLL then FreeLibrary(hDLL)
end;

function SubStrFittingMask(const Mask, StrToChk: String;
                             var SubStrPos: Integer; hDLL : HMODULE = 0): String;
var
 SubStrFittingMaskEx : function(const _Mask, _StrToChk : PWideChar;
                                  var _SubStrPos: Integer) : PWideChar;
 bDLL: Boolean;
begin
 bDLL := hDLL = 0;
 if bDLL then hDLL := LoadCloperF;
 @SubStrFittingMaskEx := GetProcAddress(hDLL, 'SubStrFittingMask');
 if Assigned(@SubStrFittingMaskEx) then
  Result := GetString(
           SubStrFittingMaskEx(PWideChar(Mask), PWideChar(StrToChk), SubStrPos))
 else
  Result := '';
 if bDLL then FreeLibrary(hDLL)
end;

function FindPathOnDisk(const SchPth : String;
                              bAnyCase : Boolean = true) : String;
var
 hDLL : HMODULE;
 FindPathOnDiskEx : function(const _SchPth: PWideChar;
                                 _bAnyCase: Boolean = true): PWideChar;
begin
 hDLL := LoadCloperF;
 @FindPathOnDiskEx := GetProcAddress(hDLL, 'FindPathOnDisk');
 if Assigned(@FindPathOnDiskEx) then
  Result := GetString(FindPathOnDiskEx(PWideChar(SchPth), bAnyCase))
 else
  Result := '';
 FreeLibrary(hDLL)
end;

function FindPathsOnDisk(const SchPth : String;
                               bAnyCase : Boolean = true) : TStringDynArray;
var
 hDLL : HMODULE;
 FindPathsOnDiskEx : function(const _SchPth : PWideChar;
                                    _bAnyCase : Boolean = true) : PWideChar;
begin
 hDLL := LoadCloperF;
 @FindPathsOnDiskEx := GetProcAddress(hDLL, 'FindPathsOnDisk');
 if Assigned(@FindPathsOnDiskEx) then
  Result := GetArray(FindPathsOnDiskEx(PWideChar(SchPth), bAnyCase))
 else
  Result := GetArray('');
 FreeLibrary(hDLL)
end;

function FindFileOnDisk(const SchPth : String;
                              bAnyCase : Boolean = true) : String;
var
 hDLL : HMODULE;
 FindFileOnDiskEx : function(const _SchPth: PWideChar;
                                 _bAnyCase: Boolean = true): PWideChar;
begin
 hDLL := LoadCloperF;
 @FindFileOnDiskEx := GetProcAddress(hDLL, 'FindFileOnDisk');
 if Assigned(@FindFileOnDiskEx) then
  Result := GetString(FindFileOnDiskEx(PWideChar(SchPth), bAnyCase))
 else
  Result := '';
 FreeLibrary(hDLL)
end;

function FindFilesOnDisk(const SchPth : String;
                               bAnyCase : Boolean = true) : TStringDynArray;
var
 hDLL : HMODULE;
 FindFilesOnDiskEx : function(const _SchPth: PWideChar;
                                  _bAnyCase: Boolean = true): PWideChar;
begin
 hDLL := LoadCloperF;
 @FindFilesOnDiskEx := GetProcAddress(hDLL, 'FindFilesOnDisk');
 if Assigned(@FindFilesOnDiskEx) then
  Result := GetArray(FindFilesOnDiskEx(PWideChar(SchPth), bAnyCase))
 else
  Result := GetArray('');
 FreeLibrary(hDLL)
end;

function FindPathToFile(const SchPth : String;
                            bAnyCase : Boolean = true) : String;
var
 hDLL : HMODULE;
 FindPathToFileEx : function(const _SchPth: PWideChar;
                                 _bAnyCase: Boolean = true): PWideChar;
begin
 hDLL := LoadCloperF;
 @FindPathToFileEx := GetProcAddress(hDLL, 'FindPathToFile');
 if Assigned(@FindPathToFileEx) then
  Result := GetString(FindPathToFileEx(PWideChar(SchPth), bAnyCase))
 else
  Result := '';
 FreeLibrary(hDLL)
end;

function FindPathsToFiles(const SchPth : String;
                              bAnyCase : Boolean = true) : TStringDynArray;
var
 hDLL : HMODULE;
 FindPathsToFilesEx : function(const _SchPth: PWideChar;
                                   _bAnyCase: Boolean = true): PWideChar;
begin
 hDLL := LoadCloperF;
 @FindPathsToFilesEx := GetProcAddress(hDLL, 'FindPathsToFiles');
 if Assigned(@FindPathsToFilesEx) then
  Result := GetArray(FindPathsToFilesEx(PWideChar(SchPth), bAnyCase))
 else
  Result := GetArray('');
 FreeLibrary(hDLL)
end;

end.
