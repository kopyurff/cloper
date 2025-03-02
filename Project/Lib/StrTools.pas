{ StrTools
  The unit of Cloning Optimizer (Cloper) tool.

  The file contains miscellaneous functionality related to work with strings.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit StrTools;

interface
uses
 Types;

function GetDHMS(dt: Double; var D, H, M, S : Integer) : String; overload;
function GetDHMS(dt: Double) : String; overload;
function SSeconds(dt: TDateTime; db: TDateTime = 0) : String;

function FormatDT(dt: TDateTime; bShort : Boolean = true) : String;

function PadRight(const aString:String;aCharCount:Integer;aChar:Char):String; overload;
function PadRight(const aString:String;aCharCount:Integer;aChar:String):String; overload;
function PadLeft (const aString:String;aCharCount:Integer;aChar:Char):String; overload;
function PadLeft (const aString:String;aCharCount:Integer;aChar:String):String; overload;
function PadLap  (const aString:String;aCharCount:Integer;aChar:Char):String;

function BoolToYN(const vBool: Boolean; StrRep: Byte = 0): String;
function SelStr(vBool: Boolean; StrTrue, StrFals: String): String; overload;
function SelStr(vBool: Boolean; StrTrue: String;
           const vals: array of const; StrFals: String): String; overload;
function SelNSt(Num: Integer; sNum: String;
             Prefix: String = ''; Suffix: String = ''; sNum0: String = ''): String;

function GetAlignedArray(aString: String; aWidth: Integer; LRM: Byte): TStringDynArray;

function StrToUInt64(StrValue: String; sDelim: String = '';
                                       bRaise: Boolean = false) : UInt64;

function GetDigitFromString(Str: String; Low, High, Default: Double) : Double; overload;
function GetDigitFromString(Str: String; Low, High, Default: Integer) : Integer; overload;

function GuidToStr(const Guid: TGUID) : String;
function StrToGuid(const sGuid: String) : TGuid;

function HashCodesOfStrings(Str: String; var HCS: TCardinalDynArray;
                      bCheckAdd: Boolean = true;
                     bRetResult: Boolean = true): Boolean;

implementation
uses
 SysUtils, StrUtils, Windows;

// Date time, full: YYYYMMDDHHMMSS.NNN , short: YYMMDDHHMMSS
function FormatDT(dt: TDateTime; bShort : Boolean = true) : String;
var
  fs: TFormatSettings;
  s : String;
begin
  GetLocaleFormatSettings(TLanguages.UserDefaultLocale, fs);
  if bShort then
   fs.ShortDateFormat := 'HH:MM:SS'
  else
   fs.ShortDateFormat := 'YYYYMMDDHHMMSS.';
  Result := DateToStr(dt, fs);
  if bShort then
   exit;

  dt := (dt - Trunc(dt)) * 86400;
  s := IntToStr(Trunc((dt - Trunc(dt)) * 1000));
  while Length(s) < 3 do
   s := '0' + s;
  Result := Result + s;
end;
function GetDHMS(dt: Double; var D, H, M, S : Integer) : String;
 procedure DHMS(V : Integer; S : String);
 begin
 if V <> 0 then begin
  if Result <> '' then Result := ' ' + Result; Result := IntToStr(V) + S + Result
 end;
 end;
begin
 D := Trunc(dt); dt := dt - D; S := Round(86400 * dt);
 H := S div 3600; S := S - 3600 * H; M := S div 60; S := S - 60 * M;
 Result := '';
 DHMS(S, ' sec'); DHMS(M, ' min'); DHMS(H, ' hrs'); DHMS(D, ' days');
end;

function GetDHMS(dt : Double) : String;
var D, H, M, S : Integer; begin
 Result := GetDHMS(dt, D, H, M, S);
end;

function SSeconds(dt: TDateTime; db: TDateTime = 0) : String;
begin
 if db = 0 then
  Result := IntToStr(Round(dt * 86400))
 else
  Result := IntToStr(Round((dt  - db) * 86400))
end;

{ PadRight (2) & PadLeft (2)
  Performs right/left padding of argument string by specified symbol;
}
function PadLeft(const aString: string; aCharCount: integer; aChar: char): string;
begin
 Result := StringOfChar(aChar, aCharCount - length(aString)) + aString;
end;
function PadLeft(const aString: string; aCharCount: integer; aChar: string): string;
begin
 if Length(aChar) = 0 then Result := aString else
  Result := StringOfChar(aChar[1], aCharCount - length(aString)) + aString;
end;
function PadRight(const aString: string; aCharCount: integer; aChar: char): string;
begin
 Result := aString + StringOfChar(aChar, aCharCount - length(aString));
end;
function PadRight(const aString: string; aCharCount: integer; aChar: string): string;
begin
 if Length(aChar) = 0 then Result := aString else
  Result := aString + StringOfChar(aChar[1], aCharCount - length(aString));
end;
function PadLap(const aString: string; aCharCount: integer; aChar: char): string;
var
 C: Integer;
begin
 C := (aCharCount - Length(aString)) div 2;
 if aCharCount = 0 then Result := aString else
  Result := StringOfChar(aChar, C) + aString + StringOfChar(aChar, C);
 Result := PadRight(Result, aCharCount, aChar)
end;

function BoolToYN(const vBool: Boolean; StrRep: Byte = 0): String;
begin
 if vBool then
  case StrRep of 0: Result := 'YES'; 1: Result := 'yes'; 2: Result := 'Yes' end
 else
  case StrRep of 0: Result := 'NOT'; 1: Result := 'not'; 2: Result := 'Not' end
end;

function SelStr(vBool: Boolean; StrTrue, StrFals: String): String;
begin
 if vBool then Result := StrTrue else Result := StrFals
end;

function SelStr(vBool: Boolean; StrTrue: String;
           const vals: array of const; StrFals: String): String;
begin
 if vBool then Result := Format(StrTrue, vals) else Result := StrFals
end;

function SelNSt(Num: Integer; sNum: String;
             Prefix: String = ''; Suffix: String = ''; sNum0: String = ''): String;
begin
 if Num <= 0 then Result := sNum0 else begin
  if sNum <> '' then begin
   if 1 < Num then sNum := sNum + 's'; sNum := ' ' + sNum
  end;
  Result := Prefix + IntToStr(Num) + sNum + Suffix
 end
end;

{ GetAlignedArray
  Splits string into dynamic array with substrings shorter than given length.
  Performs alignment (LRM): 0 - Left, 1 - Right, 2 - Middle.
}
function GetAlignedArray(aString: String; aWidth: Integer; LRM: Byte): TStringDynArray;
var
 i, j: Integer; s: String;
begin
 SetLength(Result, 1); Result[0] := ''; i := 0; aString := Trim(aString);
 for j := 1 to Length(aString) do begin
  s := s + aString[j];
  if aString[j] = ' ' then begin
   if (Result[i] = '') or (Length(Result[i]) + Length(s) < aWidth) then
    Result[i] := Result[i] + s
   else begin
    Inc(i); SetLength(Result, i + 1); Result[i] := s
   end;
   s := ''
  end
 end;
 Result[i] := Result[i] + s;
 for i := 0 to Length(Result) - 1 do begin
  Result[i] := Trim(Result[i]);
  case LRM of
  0: Result[i] := PadLeft(Result[i], aWidth, ' ');
  1: Result[i] := PadRight(Result[i], aWidth, ' ');
  end
 end;
 if LRM = 2 then
  for i := 0 to Length(Result) - 1 do
   for j := Length(Result[i]) to aWidth - 2 do
    if j mod 2 = 1 then Result[i] := ' ' + Result[i] else
     Result[i] := Result[i] + ' '
end;

{ StrToUInt64
  Performs conversions of String to UInt64. Added support of formatting for
  internal use.
}
function StrToUInt64(StrValue : String; sDelim : String = '';
                                        bRaise : Boolean = false) : UInt64;
label
 fai;
var
 Base, Digit: Integer;
 NextValue: UInt64;
 i: Integer; c: Char;
begin
 StrValue := Trim(UpperCase(StrValue));
 if sDelim <> '' then StrValue := ReplaceStr(StrValue, sDelim, '');

 if(StrValue = '') or (StrValue[1] = '-') then goto fai;
 Base := 10;
 if StrValue[1] = '$' then begin
  Base := 16; StrValue := RightStr(StrValue, Length(StrValue) - 1);
 end else // Expected '$' prefix, but ok:
  for c in ['A', 'B', 'C', 'D', 'E', 'F'] do
   if 0 < Pos(c, StrValue) then begin
    Base := 16;
    break
   end;
 if(Base = 16)and(16 < Length(StrValue))then goto fai;
 if(Base = 10)and(20 < Length(StrValue))then goto fai;

 Result := 0;
 for i := 1 to Length(StrValue) do begin
  if 0 < Pos(StrValue[i], '0123456789') then
   Digit := Ord(StrValue[i]) - Ord('0')
  else if (Base = 16) and (StrValue[i] >= 'A') and (StrValue[i] <= 'F') then
   Digit := (Ord(StrValue[i]) - Ord('A')) + 10
  else
   goto fai;

  NextValue := (Result * Base) + Digit;
  if NextValue < Result then goto fai;
  Result := NextValue
 end;
 exit;
fai:
 if bRaise then
  raise EConvertError.Create('String with invalid UInt64 value')
 else
  Result := High(UInt64);
end;

function GuidToStr(const Guid: TGUID) : String;
begin
 with Guid do
  Result := ReplaceStr(Format('%8x-%4x-%4x-%4x-%6x',
      [D1, D2, D3, D4[0] shl 8 + D4[1], UInt64(D4[2]) shl 40 + UInt64(D4[3])
      shl 32 + D4[4] shl 24 + D4[5] shl 16 + D4[6] shl 8 + D4[7]]), ' ', '0')
end;
function StrToGuid(const sGuid: String) : TGuid;
var
 ar: TStringDynArray; D4L : UInt16; D4R : UInt64;
 bytes : array[0..7] of Byte; i : Integer;
begin
 ar := SplitString(sGuid, '-'); if Length(ar) <> 5 then exit;
 with Result do begin
  D1 := StrToInt('$' + ar[0]);
  D2 := StrToInt('$' + ar[1]); D4L := StrToInt  ('$' + ar[3]);
  D3 := StrToInt('$' + ar[2]); D4R := StrToInt64('$' + ar[4]);
  CopyMemory(@bytes, @D4L, 2); for i := 0 to 1 do D4[i] := bytes[1 - i];
  CopyMemory(@bytes, @D4R, 6); for i := 2 to 7 do D4[i] := bytes[7 - i];
 end;
end;

function GetDigitFromString(Str: String; Low, High, Default: Double) : Double;
var
 i: Integer;
begin
 Result := Default;
 for i := 1 to Length(str) do if Pos(str[i], '-.0123456789') = 0 then exit;
 if str <> '' then begin
  Default := StrToFloat(str);
  if (Low <= Default) and (Default <= High) then Result := Default
 end;
end;
function GetDigitFromString(Str: String; Low, High, Default: Integer) : Integer;
begin
 Result := Round(GetDigitFromString(Str, 1.0 * Low, High, Default))
end;

{ HashCodesOfStrings
  Fills cardinal array by hash code value of string in ascending order. Returns
  bRetResult if the string was already checked earlier. The parameter bCheckAdd
  is to add checked string if it was not found.
}
function HashCodesOfStrings(Str: String; var HCS: TCardinalDynArray;
                      bCheckAdd: Boolean = true;
                     bRetResult: Boolean = true): Boolean;
 function HashOf(const Key: string): Cardinal; // copy IniFiles.TStringHash.HashOf
 var
  I: Integer;
 begin
  Result := 0;
  for I := 0 to Key.Length - 1 do
   Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor
      Ord(Key.Chars[I])
 end;
 function BinSchHCS(sc: Cardinal; var ix: Integer): Boolean;
 var
  i, j: Integer;
 begin
  Result := false; i := Length(HCS) - 1; j := i; ix := i;
  while i > 1 do begin
   if ix < 0 then ix := ix + i else if j < ix then ix := ix - i;
   i := (i + i mod 2) div 2;
   if sc < HCS[ix] then ix := ix - i else
    if HCS[ix] < sc then ix := ix + i else break
  end;
  i := 1;
  repeat
   if (0 <= ix + i) and (ix + i <= j) then
    if HCS[ix + i] = sc then Result := true else if HCS[ix + i] < sc then break;
   Dec(i)
  until (ix + i < 0) or (i < -1);
  ix := ix + i + 1
 end;
var
 sc: Cardinal; i, j, k: Integer;
begin
 sc := HashOf(Str);
 if BinSchHCS(sc, i) then begin Result := bRetResult; exit end else
  Result := not bRetResult;

 if bCheckAdd then begin
  j := Length(HCS); SetLength(HCS, j + 1);
  for k := j downto i + 1 do HCS[k] := HCS[k - 1]; HCS[i] := sc
 end
end;

end.
