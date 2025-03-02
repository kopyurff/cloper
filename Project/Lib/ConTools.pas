{ ConTools
  The unit of Cloning Optimizer (Cloper) tool.

  Contains auxiliary functional extensions for console window output and several
  classes to perform formatted output:
  1. TCrtcProgress class contains functionality for the output of task progress;
  2. TCRTGrid class contains functionality for data vizualization as data grid;
  3. TWeighUp auxiliary class for modifications of progress indicator behavior.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit ConTools;

interface
uses
 Types, cloper_base;

procedure CrtcTextColor(Color: Byte);
procedure CrtcTextBackground(Color: Byte);
procedure CrtcDeleteLine(bMoveUp : Boolean = true);
procedure GotoXY(aX, aY: integer);

procedure ClrScr;

type
 TConsoleColor = (
    ccBlack = 0,
    ccBlue = 1,
    ccGreen = 2,
    ccCyan = 3,
    ccRed = 4,
    ccMagenta = 5,
    ccBrown = 6,
    ccLightGray = 7,
    ccDarkGray = 8,
    ccLightBlue = 9,
    ccLightGreen = 10,
    ccLightCyan = 11,
    ccLightRed = 12,
    ccLightMagenta = 13,
    ccYellow = 14,
    ccWhite = 15);
 TShowHideCaret = (shcShow = 0, shcHide = 1, shcRestore = 2);
 TEchoOfSilentTask = procedure(CurPerc: UInt64; ShortName: String);
 TWUMode = (wuNone = 0, wuAvrg = 1, wuPshEnd = 2);

procedure ShowHideCaret(Show: TShowHideCaret = shcShow);

const
 CrtTxtColorCategory = $B;
 CrtTxtColorSubCategory = $A;
 CrtTxtColorSub2Category = $6;
 CrtTxtColorWarning = $E;
 CrtTxtColorError = $C;
 CrtTxtColorLPError = $1C; // == $C
 CrtTxtColorEcho = $3;
 CrtTxtColorUserError = $D;

type
 { TCrtcProgress
   Container class for subtask progress visualisation.
 }
 TCrtcProgress = class
  private
   const Width = 79;
   const Height = 299;
   class var X, Y, dY : Integer;
   class var Header, Process : String;
   class var Prefix, Suffix : TStringDynArray;
   class var FBegValue, FEndValue : UInt64;
   class var CurPerc : Integer;
   class var BegDtTm, EchDtTm, FTimeShift : TDateTime;
   class var FInitialialized : Boolean;
   class var CrtTxtColor : Byte;
  public
   class procedure Initialize(_Head, _ProcName: String;
    _Pref, _Suff: array of String; _EndVal: UInt64; _BegVal: UInt64 = 0;
    _Category: String = ''; _CrtTxtColor : Byte = CrtTxtColorCategory);
   class procedure Echo(_CurVal: UInt64; _Vals : array of Extended;
                          bWait: Boolean = true);
   class procedure Summary(_Head: String; _Vals: array of Extended;
      bPASS: Boolean = true);
   class procedure Message(MsgType : TMsgType; Message_ : String);
   class function OutSysError(Sever: Boolean; SubMessage_: String = '';
      ErrorCode: DWORD = 0): Integer;
   class procedure Finalize(bPASS : Boolean = true);
   // The 3 proc & funs below is to do corrections of summary reports:
   class procedure InsertSummaryString(idx : Integer; _Pref, _Suff : String);
   class function ReplaceSummaryString(idx: Integer; _Pref, _Suff: String): Boolean;
   class function RemoveSummaryStrings(const idx: array of Integer): Boolean;

  public
   class var WeighUp: TWUMode;
   class var AuxCI : array[1..7] of UInt64;
   class var AuxCountD1, AuxCountD2, AuxCountD3: Double;
   class var EchoOfSilentTask: TEchoOfSilentTask;
  public
   class property Initialialized : Boolean read FInitialialized;
   class property TimeShift : TDateTime read FTimeShift write FTimeShift;
   class property BegValue : UInt64 read FBegValue;
   class property EndValue : UInt64 read FEndValue;
 end;
 TCRTAlign = (crtLeft = 0, crtCenter = 1, crtRight = 2);
 TCRTColumn = record
   Align : TCRTAlign;
   TextColor : Byte;
 end;
 { TCRTGrid
   The class to draw text into grid structure, uses pseudographics if specified.
 }
 TCRTGrid = class
  private
   FIndent, FWidth : Byte;
   FLinColor, FBkgColor : Word;
   FHasBorders, FHasBrdrIns : Boolean;
   CRTColumns, CRTRows : array of TCRTColumn;
  const
   LineWidth = 80;
   LeftTopCorner = #$250C;
   RightTopCorner = #$2510;
   HorizontalLine = #$2500;
   VerticalLine = #$2502;
   TopColumn = #$252C;
   BottomColumn = #$2534;
   LeftRow = #$251C;
   RightRow = #$2524;
   LeftBottomCorner = #$2514;
   RightBottomCorner = #$2518;
  public
   constructor Create(Indent: Byte; BkgColor: Word = $FF;
                  HasBorders: Boolean = false; HasBrdrIns: Boolean = false;
                   LineColor: Word = 0; RowWidth : Byte = 0);
   procedure AddColumns(CRTColumnStyles : array of TCRTColumn);
   procedure AddRows(CRTRowStyles : array of TCRTColumn);
   function DrawAlignedRows(Rows: array of String; OnlyGet : Boolean) : TStringDynArray;
   function DrawGrid(Columns: array of TStringDynArray;
       Header : String = ''; HeadTxtColor: Word = $FF; HeadAlign: TCRTAlign = crtCenter;
       Bottom : String = ''; BotmTxtColor: Word = $FF; BotmAlign: TCRTAlign = crtCenter;
       OnlyGet: Boolean = false) : TStringDynArray;
 end;

 { TWeighUp - auxiliary class of TCrtcProgress. Contains functionality to slow
   down the progress indicator of the process, which has fast progress at it's
   begin. It also averages value of progress indicator during subtask.
   The method ScaleDwn allows to split range of values due to same reason. The
   lower values of indicator correspond to the change of indicator by one, the
   higher subranges correspond to changes multiplied by 2.
 }
 TWeighUp = class
  private
   class var ValBeg, ValEnd, ValPrv, ValWup: UInt64;
   class var TimeSpan, TimeGain: Double;
   class var Started, dtPrv: TDateTime;
   class var aspr: TIntegerDynArray;
   class var FTimeShift : TDateTime;
  public
   class procedure Initialize(AValBeg, AValEnd: UInt64);
   class function Progress(PshMod: TWUMode; CurVal: UInt64;
                           ATimeShift: TDateTime = 0): UInt64;
   class function ScaleDwn(iVal: UInt64; bCnt: Boolean = false): UInt64;
 end;

 TCrtTxtColor = (ctcCategory = CrtTxtColorCategory,
                 ctcSubCategory = CrtTxtColorSubCategory,
                 ctcWarning = CrtTxtColorWarning,
                 ctcError = CrtTxtColorError);
var
  CrtTxtColorDefault : Word;
  CrtBkgColorDefault : Word;

implementation
uses
  System.SysUtils, Windows, StrTools;

var
  UpperLeft : TCoord = (X:0; Y:0);
  hCon : Integer;
  LastMode : Word;         { Current text mode }
  LastTextColor : Byte;
  LastBckgColor : Byte;
  TextAttr : Byte;         { Current text attribute }

function WhereX: integer;
var
 ScrBufInfo: TConsoleScreenBufferInfo;
begin
 GetConsoleScreenBufferInfo(hCon, ScrBufInfo);
 Result := ScrBufInfo.dwCursorPosition.X;
end;

function WhereY: integer;
var
 ScrBufInfo: TConsoleScreenBufferInfo;
begin
 GetConsoleScreenBufferInfo(hCon, ScrBufInfo);
 Result := ScrBufInfo.dwCursorPosition.Y;
end;

procedure GotoXY(aX, aY: integer);
var
  aCoord: TCoord;
begin
  aCoord.X := aX;
  aCoord.Y := aY;
  SetConsoleCursorPosition(hCon, aCoord);
end;

procedure CrtcDeleteLine(bMoveUp : Boolean = true);
var
 SourceScreenRect: TSmallRect;
 Coord: TCoord;
 CI: TCharinfo;
 dwSize, dwCount: DWORD;
 CBI: TConsoleScreenBufferInfo;
begin
 GetConsoleScreenBufferInfo(hCon, CBI);
 SourceScreenRect := CBI.srWindow;
 SourceScreenRect.Top := WhereY + CBI.srWindow.Top;
 CI.AsciiChar := ' ';
 CI.Attributes := TextAttr;
 Coord.X := SourceScreenRect.Left;
 Coord.Y := SourceScreenRect.Top - 1;
 dwSize := SourceScreenRect.Right - SourceScreenRect.Left + 1;
 ScrollConsoleScreenBuffer(hCon, SourceScreenRect, nil, Coord, CI);
 FillConsoleOutputAttribute(hCon, TextAttr, dwSize, Coord, dwCount);
 if bMoveUp then
  GotoXY(WhereX, WhereY - 1);
end;

procedure CrtcTextColor(Color: Byte);
begin
 if LastTextColor <> Color then LastTextColor := Color else exit;
 LastMode := TextAttr;
 TextAttr := (Color and $0F) or (TextAttr and $F0);
 SetConsoleTextAttribute(hCon, TextAttr);
end;

procedure CrtcTextBackground(Color: Byte);
begin
 if LastBckgColor <> Color then LastBckgColor := Color else exit;
 LastMode := TextAttr;
 TextAttr := (Color shl 4) or (TextAttr and $0F);
 SetConsoleTextAttribute(hCon, TextAttr);
end;

function GetAttr: word;
var
 ScrBufInfo: TConsoleScreenBufferInfo;
begin
 GetConsoleScreenBufferInfo(hCon, ScrBufInfo);
 Result := ScrBufInfo.wAttributes;
end;

procedure ClrScr;
var
 fill: Cardinal;
 ScrBufInfo: TConsoleScreenBufferInfo;
begin
 GetConsoleScreenBufferInfo(hCon, ScrBufInfo);
 fill := ScrBufInfo.dwSize.X * ScrBufInfo.dwSize.Y;
 FillConsoleOutputCharacter(hCon, ' ', fill, UpperLeft, fill);
 FillConsoleOutputAttribute(hCon, ScrBufInfo.wAttributes, fill,
   UpperLeft, fill);
 GotoXY(0, 0);
end;

// Allows to restore one previous visibility state of caret:
var
 ShowHideCaretState : Boolean = true;

procedure ShowHideCaret(Show: TShowHideCaret = shcShow);
var
 CCI: TConsoleCursorInfo;
 bShow : Boolean;
begin
 if Show in [shcShow, shcHide] then begin
  GetConsoleCursorInfo(hCon, CCI);
  ShowHideCaretState := CCI.bVisible;
  bShow := Show = shcShow
 end;
 if bShow = ShowHideCaretState then
  exit;
 if Show = shcRestore then
  CCI.bVisible := ShowHideCaretState
 else
  CCI.bVisible := bShow;
 CCI.dwSize := 1;
 SetConsoleCursorInfo(hCon, CCI)
end;

constructor TCRTGrid.Create(Indent: Byte; BkgColor: Word = $FF;
      HasBorders: Boolean = false; HasBrdrIns: Boolean = false;
      LineColor: Word = 0; RowWidth : Byte = 0);
begin
  FIndent := Indent;
  FLinColor := LineColor;
  FBkgColor := BkgColor;
  FHasBorders := HasBorders;
  FHasBrdrIns := HasBrdrIns;
  if FIndent + RowWidth < LineWidth then FWidth := RowWidth else FWidth := 0;
end;

procedure TCRTGrid.AddColumns(CRTColumnStyles : array of TCRTColumn);
var
 i, j : Integer;
begin
 i := Length(CRTColumnStyles);
 Assert((Length(CRTColumns) = 0) and (Length(CRTRows) = 0),
    'The grid can contain row descriptors or column descriptors.');
 SetLength(CRTColumns, i);
 for j := 0 to i - 1 do
  CRTColumns[j] := CRTColumnStyles[j];
end;
procedure TCRTGrid.AddRows(CRTRowStyles : array of TCRTColumn);
var
 i, j : Integer;
begin
 i := Length(CRTRowStyles);
 Assert((Length(CRTColumns) = 0) and (Length(CRTRows) = 0),
    'The grid can contain row descriptors or column descriptors.');
 SetLength(CRTRows, i);
 for j := 0 to i - 1 do
  CRTRows[j] := CRTRowStyles[j];
end;

function TCRTGrid.DrawGrid( Columns: array of TStringDynArray;
      Header: String = ''; HeadTxtColor: Word = $FF; HeadAlign : TCRTAlign = crtCenter;
      Bottom: String = ''; BotmTxtColor: Word = $FF; BotmAlign : TCRTAlign = crtCenter;
      OnlyGet : Boolean = false) : TStringDynArray;
var
 cw : TIntegerDynArray;
 w : Integer;
 procedure Wrt(str : String; bOut : Boolean = true); var i : Integer; begin
  i := Length(Result) - 1; Result[i] := Result[i] + str;
  if bOut and not OnlyGet then Write(str);
 end;
 procedure WrtLn(str : String); var i : Integer; begin
  Wrt(str, false);
  if not OnlyGet then WriteLn(str);
  i := Length(Result); SetLength(Result, i + 1);
 end;
 procedure CrctWdth(str : String);
 var
  i, j : Integer;
 begin
  i := Length(cw) - 1;
  if Length(str) > w then begin
   j := Length(str) - w; cw[i] := cw[i] + j; w := w + j
  end
 end;
 procedure ErsEndLn;
 var lw : Int8;
 begin
  if FBkgColor <> $FF then CrtcTextBackground(CrtBkgColorDefault);
  if FLinColor <> $FF then CrtcTextColor     (CrtTxtColorDefault);
  lw := LineWidth - FIndent - w;
  if FHasBorders then lw := lw - 2;
  if FHasBrdrIns then lw := lw - Length(cw);
  Dec(lw);
  WrtLn(PadRight('', lw, ' ')); Wrt(PadRight('', FIndent, ' '))
 end;
 procedure DrwBdrLine(Bdr : Boolean; Left, Col, Right : Char);
 var
  i, j : Integer;
 begin
  if Bdr or FHasBrdrIns then
   if FBkgColor = $FF then CrtcTextBackground(CrtBkgColorDefault) else
    CrtcTextBackground(FBkgColor);

  if FHasBorders then begin
   if Bdr or FHasBrdrIns then begin
    if FLinColor = $FF then CrtcTextColor(CrtTxtColorDefault) else
     CrtcTextColor(FLinColor);
    Wrt(Left);
    if 0 < Length(CRTColumns) then begin
     i := Length(CRTColumns) - 1;
     for j := 0 to i - 1 do
      Wrt(PadLeft(Col, cw[j] + 1, HorizontalLine));
     Wrt(PadLeft(Right, cw[i] + 1, HorizontalLine))
    end else
     Wrt(PadLeft(Right, w + 1, HorizontalLine));
    Bdr := true
   end
  end else if Bdr then
   Wrt(PadRight('', w + 2, ' '));
  if Bdr then ErsEndLn
 end;
 procedure DrwHedBtm(str: String; Clr: Word; Align: TCRTAlign;
    LeftCorner, Column, RightCorner: Char; Top: Boolean);
 var
  i : Integer;
 begin
  if Str <> '' then begin
   if Top then
    DrwBdrLine(true, LeftTopCorner, HorizontalLine, RightTopCorner)
   else
    DrwBdrLine(false, LeftRow, BottomColumn, RightRow);

   if FBkgColor = $FF then CrtcTextBackground(CrtBkgColorDefault) else
    CrtcTextBackground(FBkgColor);
   if FHasBorders then begin
    if FLinColor = $FF then CrtcTextColor(CrtTxtColorDefault) else
     CrtcTextColor(FLinColor);
    Wrt(VerticalLine)
   end else Wrt(' ');
   if FBkgColor <> $FF then CrtcTextBackground(FBkgColor);
   if Clr <> $FF then CrtcTextColor(Clr) else CrtcTextColor(CrtTxtColorDefault);
   case Align of
   crtLeft  : Wrt(PadRight(Str, w, ' '));
   crtCenter:
   begin
    for i := Length(Str) + 1 to w do
     if i mod 2 = 0 then Str := ' ' + Str else Str := Str + ' ';
    Wrt(Str)
   end;
   crtRight : Wrt(PadLeft(Str, w, ' '));
   end;

   if FHasBorders then begin
    if FLinColor = $FF then CrtcTextColor(CrtTxtColorDefault) else
     CrtcTextColor(FLinColor);
    Wrt(VerticalLine)
   end else Wrt(' ');
   ErsEndLn;

   if Top then
    DrwBdrLine(false, LeftRow, TopColumn, RightRow)
   else
    DrwBdrLine(true, LeftBottomCorner, HorizontalLine, RightBottomCorner);
  end else if FHasBrdrIns then
   DrwBdrLine(true, LeftCorner, Column, RightCorner)
  else
   DrwBdrLine(true, LeftCorner, HorizontalLine, RightCorner)
 end;
var
 i, j, k, x, y : Integer;
begin
 if 0 = Length(Columns) then
  raise Exception.Create('The grid must contain at least one column.');
 if 0 < Length(CRTRows) * Length(CRTColumns) then
  raise Exception.Create('The grid can contain only row descriptors or only column descriptors.');
 if(0 < Length(CRTColumns))and(Length(Columns) <> Length(CRTColumns))then
  raise Exception.Create('The number of column descriptors doesn''t coincide with number of columns.');
 if(0 < Length(CRTRows))and(1 < Length(Columns))then
  raise Exception.Create('The grid with rows descriptors can contain only one column.');
 if(0 < Length(CRTRows))and(Length(Columns[0]) <> Length(CRTRows))then
  raise Exception.Create('The number of rows descriptors doesn''t coincide with number of rows');
 if FHasBrdrIns then FHasBorders := true;
 SetLength(Result, 1);

 if 0 < Length(CRTColumns) then begin
  SetLength(cw, Length(CRTColumns));
  for i := 0 to Length(Columns[0]) - 1 do
   for j := 0 to Length(CRTColumns) - 1 do begin
    k := Length(Columns[j][i]); if cw[j] < k then cw[j] := k;
   end;
 end else begin
  SetLength(cw, 1);
  for i := 0 to Length(Columns[0]) - 1 do begin
    k := Length(Columns[0][i]); if cw[0] < k then cw[0] := k;
  end;
 end;
 w := Length(cw) - 1;
 for i := 0 to Length(cw) - 1 do w := w + cw[i];
 if (w < FWidth) then begin
  j := FWidth;
  if FHasBorders then j := j - 2;
  if FHasBrdrIns then j := j - Length(cw);
  for i := 0 to Length(cw) - 1 do cw[i] := cw[i] + (j - w) div Length(cw);
  w := j;
 end;

 CrctWdth(Header); CrctWdth(Bottom);
 for i := 0 to Length(Columns[0]) - 1 do
  if 0 < Length(CRTColumns) then
   for j := 0 to Length(CRTColumns) - 1 do
    case CRTColumns[j].Align of
    crtLeft  : Columns[j][i] := PadRight(Columns[j][i], cw[j], ' ');
    crtCenter: for k := Length(Columns[j][i]) + 1 to cw[j] do
        if k mod 2 = 0 then
         Columns[j][i] := ' ' + Columns[j][i]
        else
         Columns[j][i] := Columns[j][i] + ' ';
    crtRight : Columns[j][i] := PadLeft(Columns[j][i], cw[j], ' ');
    end
  else
    case CRTRows[i].Align of
    crtLeft  : Columns[0][i] := PadRight(Columns[0][i], cw[0], ' ');
    crtCenter: for k := Length(Columns[0][i]) + 1 to cw[0] do
        if k mod 2 = 0 then
         Columns[0][i] := ' ' + Columns[0][i]
        else
         Columns[0][i] := Columns[0][i] + ' ';
    crtRight : Columns[0][i] := PadLeft(Columns[0][i], cw[0], ' ');
    end;

 if FBkgColor = $FF then CrtcTextBackground(CrtBkgColorDefault) else
  CrtcTextBackground(FBkgColor);

 if not OnlyGet then
  GotoXY(WhereX + FIndent, WhereY);
 DrwHedBtm(Header, HeadTxtColor, HeadAlign, LeftTopCorner, TopColumn,
    RightTopCorner, true);
 x := WhereX + FIndent;
 y := WhereY;
 for i := 0 to Length(Columns[0]) - 1 do begin
  if FBkgColor = $FF then CrtcTextBackground(CrtBkgColorDefault) else
   CrtcTextBackground(FBkgColor);
  if FLinColor = $FF then CrtcTextColor(CrtTxtColorDefault) else
   CrtcTextColor(FLinColor);
  if FHasBorders then Wrt(VerticalLine) else Wrt(' ');
  if 0 < Length(CRTColumns) then begin
   k := Length(CRTColumns) - 1;
   for j := 0 to k do begin
    if CRTColumns[j].TextColor = $FF then CrtcTextColor(CrtTxtColorDefault) else
     CrtcTextColor(CRTColumns[j].TextColor);
    Wrt(Columns[j][i]);
    if FHasBrdrIns or (j = k) then
     if FLinColor = $FF then CrtcTextColor(CrtTxtColorDefault) else
      CrtcTextColor(FLinColor);
    case j < k of
    true : if FHasBrdrIns then Wrt(VerticalLine) else Wrt(' ');
    false: if FHasBorders then Wrt(VerticalLine) else Wrt(' ');
    end
   end
  end else begin
   if CRTRows[i].TextColor = $FF then CrtcTextColor(CrtTxtColorDefault) else
    CrtcTextColor(CRTRows[i].TextColor);
   Wrt(Columns[0][i]);
   if FLinColor = $FF then CrtcTextColor(CrtTxtColorDefault) else
    CrtcTextColor(FLinColor);
   if FHasBorders then Wrt(VerticalLine) else Wrt(' ');
  end;
  ErsEndLn;
  Inc(y)
 end;

 DrwHedBtm(Bottom, BotmTxtColor, BotmAlign, LeftBottomCorner, BottomColumn,
    RightBottomCorner, false);
 if not OnlyGet then
 //GotoXY(FIndent + w + Length(cw) - 1, y) // Caret to the end of grid.
  GotoXY(0, y + 1) // Caret to next string below grid.
end;
function TCRTGrid.DrawAlignedRows(Rows: array of String; OnlyGet : Boolean) : TStringDynArray;
var Columns : TStringDynArray; i : Integer; begin
 SetLength(Columns, Length(Rows));
 for i := 0 to Length(Rows) - 1 do Columns[i] := Rows[i];
 Result := DrawGrid( [Columns], '', $FF, crtCenter, '', $FF, crtCenter, OnlyGet)
end;

class procedure TCrtcProgress.Initialize(_Head, _ProcName: String;
  _Pref, _Suff: array of String; _EndVal: UInt64; _BegVal: UInt64 = 0;
  _Category: String = ''; _CrtTxtColor : Byte = CrtTxtColorCategory);
var
 i : Integer;
begin
 Header := _Head; Process := _ProcName; EchDtTm := 0;
 CrtTxtColor := _CrtTxtColor;
 if not SilentMode then CrtcTextColor(CrtTxtColor);
 TWeighUp.Initialize(_BegVal, _EndVal); WeighUp := wuNone;

 if _Category = '' then
  DoMessageOutput(mLogHighPri, 'Started ' + _Head + '...')
 else
  DoMessageOutput(mLogHighPri, _Category);
 if not SilentMode then
  CrtcTextColor(CrtTxtColorDefault);
 X := WhereX;
 Y := WhereY; dY := 0;
 Assert(Length(_Pref) = Length(_Suff),
   'The number of prefixes & suffixes of progress CRT messages must be the same');
 SetLength(Prefix, Length(_Pref));
 SetLength(Suffix, Length(_Suff));
 for i := 0 to Length(_Pref) - 1 do begin
  Prefix[i] := Trim(_Pref[i]); Suffix[i] := Trim(_Suff[i])
 end;
 FBegValue := _BegVal; FEndValue := _EndVal; CurPerc := -1;
 for i := 1 to Length(AuxCI) do
  AuxCI[i] := 0;
 AuxCountD1 := 0; AuxCountD2 := 0; AuxCountD3 := 0;
 FInitialialized := true; FTimeShift := 0; Inc(Y);
 if not SilentMode then begin
  GotoXY(X, Y);
  ShowHideCaret(shcHide)
 end;
 BegDtTm := Now;
end;

class procedure TCrtcProgress.Echo(_CurVal: UInt64; _Vals : array of Extended;
                                     bWait: Boolean = true);
const
 CRTColumnStyles : array[0..2] of TCRTColumn = ((Align:crtLeft  ;TextColor:$1F),
                                                (Align:crtRight ;TextColor:$1A),
                                                (Align:crtLeft  ;TextColor:$1F));
var
 i, j, _Y : Integer; Str : String; PercCur: Integer;
 cols : array[0..2] of TStringDynArray;
 CWL, CWM, CWR : Integer;
 LtM, LbM : Byte;
 CRTGrid : TCRTGrid;
begin
 if FEndValue <= FBegValue then PercCur := 100 else
  PercCur := 100 * _CurVal div (FEndValue - FBegValue);
 if bWait then
  if PercCur = CurPerc then begin
   if (Now - EchDtTm) * 86400 < 30 then exit
  end else
   if (Now - EchDtTm) * 86400 <  1 then exit;
 CurPerc := PercCur;
 if FBegValue < FEndValue then
  if WeighUp in [wuAvrg, wuPshEnd] then
   PercCur := 100 * TWeighUp.Progress(WeighUp, _CurVal, FTimeShift) div
                                              (FEndValue - FBegValue);
 EchDtTm := Now;
 if SilentMode then begin
  if @EchoOfSilentTask <> nil then EchoOfSilentTask(PercCur, Header);
  exit
 end;

 if Process <> '' then Str := 'The ' + Process else Str := 'The process';
 Str := Str + ' is completed by';
 SetLength(cols[0], 1); cols[0][0] := ' ' + Str;
 SetLength(cols[1], 1); cols[1][0] := ' ' + IntToStr(PercCur);
 SetLength(cols[2], 1); cols[2][0] := '%';
 for i := 0 to Length(_Vals) - 1 do begin
  j := Length(cols[0]);
  SetLength(cols[0], j + 1); cols[0][j] := ' ' + Prefix[i];
  SetLength(cols[1], j + 1);
   cols[1][j] := ' ' + IntToStr(Round(_Vals[i]));
  SetLength(cols[2], j + 1); cols[2][j] := Suffix[i];
 end;
 Inc(j);
 SetLength(cols[0], j + 1); cols[0][j] := ' Spent time';
 SetLength(cols[1], j + 1);
  cols[1][j] := ' ' + SSeconds(Now, BegDtTm);
 SetLength(cols[2], j + 1); cols[2][j] := 'sec';
 if PercCur > 0 then begin
  Inc(j);
  SetLength(cols[0], j + 1); cols[0][j] := ' Expected left time';
  SetLength(cols[1], j + 1);
  cols[1][j] := ' ' + IntToStr(Round((100 - PercCur)
                            * (Now - BegDtTm - FTimeShift) * 86400
      / PercCur));
  SetLength(cols[2], j + 1); cols[2][j] := 'sec'
 end;
 LtM := TextAttr;
 LbM := LastMode;
 CRTGrid := TCRTGrid.Create(3, $3, true, true, Word(ccDarkGray));
 CRTGrid.AddColumns(CRTColumnStyles);

 Str := PadRight('', Width, ' ');
 FlushConsoleInputBuffer(hCon);
 Sleep(1);
 GotoXY(0, Y);
 repeat
  for i := 0 to Length(cols[0]) do Writeln(Str);
  Sleep(1);
  _Y := WhereY;
  if _Y = Y + Length(cols[0]) + 1 then break;
  Y := _Y - Length(cols[0]) - 1;
  GotoXY(0, Y);
  Sleep(1);
 until false;

 GotoXY(0, Y + 1);
 if dY = 0 then begin
  dY := Length(CRTGrid.DrawGrid([cols[0], cols[1], cols[2]], UpperCase(' ' + Header), $1, crtLeft));
  i := dY - Length(cols[0])
 end else begin
  dY := Length(CRTGrid.DrawGrid([cols[0], cols[1], cols[2]], UpperCase(' ' + Header), $1, crtLeft));
  i := dY - Length(cols[0]) - 1;
 end;
 if Height <= Y + dY then
  Y := Height - dY;
 FlushConsoleInputBuffer(hCon);
 Sleep(1);
 for i := 0 to 2 do SetLength(cols[0], 0);
end;

class procedure TCrtcProgress.InsertSummaryString(idx: Integer; _Pref, _Suff: String);
var
 i, j : Integer;
begin
 j := Length(Prefix);
 SetLength(Prefix, j + 1); SetLength(Suffix, j + 1);
 if (idx < 0) or (j <= idx) then begin
  Prefix[j] := _Pref;
  Suffix[j] := _Suff
 end else begin
  for i := j downto idx + 1 do begin
   Prefix[i] := Prefix[i - 1];
   Suffix[i] := Suffix[i - 1]
  end;
  Prefix[idx] := _Pref;
  Suffix[idx] := _Suff
 end
end;

class function TCrtcProgress.ReplaceSummaryString(idx: Integer;
  _Pref, _Suff: String): Boolean;
begin
 Result := false;
 if (idx < 0) or (Length(Prefix) <= idx) then exit;
 Prefix[idx] := _Pref;
 Suffix[idx] := _Suff; Result := true;
end;

// Receives array of current indexes of strings in arbitrary order.
class function TCrtcProgress.RemoveSummaryStrings(const idx: array of Integer): Boolean;
var
 i, j, k, m : Integer;
begin
 Result := false; i := Length(Prefix); j := 0;
 for k := 0 to Length(idx) - 1 do
  if j < idx[k] then j := idx[k] else if (idx[k] < 0) or (i <= idx[k]) then exit;
 repeat
  for k := j + 1 to i - 1 do begin
   Prefix[k - 1] := Prefix[k];
   Suffix[k - 1] := Suffix[k]
  end;
  Dec(i); SetLength(Prefix, i); SetLength(Suffix, i);
  k := -1;
  for m := 0 to Length(idx) - 1 do if (k < idx[m]) and (idx[m] < j) then
   k := idx[m];
  j := k
 until j < 0;
 Result := true
end;

// Deletes rows with progress of the running process, prints message & increases
// Y to print later other rows below.
class procedure TCrtcProgress.Message(MsgType : TMsgType; Message_ : String);
var
 i, _Y : Integer; Spc : String;
begin
 if IsDoMessageOutput(MsgType, [oScreen]) then begin
  Spc := PadRight('', 79, ' ');
  _Y := WhereY;
  FlushConsoleInputBuffer(hCon);
  Sleep(1);
  GotoXY(0, Y);
  repeat
   for i := 0 to Length(Prefix) + 7 do Writeln(Spc);
   Sleep(1);
   _Y := WhereY;
   if _Y = Y + Length(Prefix) + 8 then break;
   Y := _Y - Length(Prefix) - 8;
   GotoXY(0, Y);
   Sleep(1);
  until false;
  GotoXY(0, Y - 1);
  CurPerc := -1
 end;
 Y := Y + DoMessageOutput(MsgType, Message_)
end;

class function TCrtcProgress.OutSysError(Sever: Boolean; SubMessage_: String = '';
  ErrorCode: DWORD = 0) : Integer;
var
 MsgType : TMsgType; Msg : String;
begin
 if Sever then MsgType := mSystemErr else MsgType := mSysLowPrE;
 if ErrorCode = NO_ERROR then
  ErrorCode := GetLastError();
 if SubMessage_ = '' then Msg := '' else Msg := '[' + SubMessage_ + '] ';

 TCrtcProgress.Message(MsgType, Msg + 'SYSTEM ERROR #' + IntToStr(ErrorCode) +
    ': ' + SysErrorMessage(ErrorCode));
 Result := ErrorCode;
end;

class procedure TCrtcProgress.Summary(_Head: String; _Vals: array of Extended;
  bPASS: Boolean = true);
var
 i : Integer; Msg : String;
begin
 if IsDoMessageOutput(mLogHighPri, [oScreen]) then begin
  i := WhereY;
  Msg := PadRight('', 79, ' ');
  while Y < i do begin
   GotoXY(0, i);
   Writeln(Msg);
   Dec(i)
  end;
  GotoXY(0, Y - 1)
 end;
 DoMessageOutput(mLogHighPri, _Head);
 for i := 0 to Length(Prefix) - 1 do
  DoMessageOutput(mLogNoDate, '<align>' + Prefix[i] + ' ' +
        IntToStr(Trunc(_Vals[i])) + ' ' + Suffix[i]);
 Y := WhereY;
 TCrtcProgress.Finalize(bPASS)
end;

class procedure TCrtcProgress.Finalize(bPASS : Boolean = true);
var
 i, _Y : Integer; Msg : String;
 LtM : Byte;
begin
 if not FInitialialized then exit;

 if IsDoMessageOutput(mLogHighPri, [oScreen]) then begin
  i := WhereY;
  Msg := PadRight('', 79, ' ');
  while Y < i do begin
   GotoXY(0, i);
   Writeln(Msg);
   Dec(i)
  end;
  GotoXY(0, Y);
  LtM := TextAttr;
  CrtcTextColor(CrtTxtColor);
 end;
 if Process <> '' then Msg := 'The ' + Process else Msg := 'The process';
 if bPASS then
  DoMessageOutput(mLogHighPri, Msg + ' was completed in ' +
     SSeconds(Now, BegDtTm) + ' sec')
 else
  DoMessageOutput(mLogHighPri, Msg + ' failed after ' +
     SSeconds(Now, BegDtTm) + ' sec');
 if not SilentMode then begin
  CrtcTextColor(LtM);
  ShowHideCaret(shcRestore);
 end;
 SetLength(Prefix, 0); Header  := '';
 SetLength(Suffix, 0); Process := ''; FInitialialized := false
end;

class procedure TWeighUp.Initialize(AValBeg, AValEnd: UInt64);
begin
 ValBeg := AValBeg; ValEnd := AValEnd; ValPrv := AValEnd;
 Started := 0; ValWup := 0; TimeSpan := 0; TimeGain := 0; FTimeShift := 0;
end;

class function TWeighUp.Progress(PshMod: TWUMode; CurVal: UInt64;
                                 ATimeShift: TDateTime = 0): UInt64;
var
 dtNow: TDateTime; SpanTime, GainTime: Double; Incr: Integer; LR, LW: UInt64;
 procedure _Progress(ValCur: UInt64);
 begin
  dtPrv := dtNow; ValPrv := CurVal; Result := ValCur
 end;
begin

 if CurVal = ValPrv then
  Result := ValWup
 else begin
  dtNow := Now;
  if FTimeShift < ATimeShift then begin
   dtNow := dtNow - ATimeShift; FTimeShift := ATimeShift
  end else if 0 < FTimeShift then
   dtNow := dtNow - FTimeShift;

  if (Started = 0) or (ValEnd = ValBeg) then begin
   Started := dtNow; _Progress(CurVal)
  end else if TimeSpan = 0 then begin
   TimeSpan := dtNow - dtPrv; ValWup := CurVal; _Progress(CurVal)
  end else if ValWup = ValEnd then
   Result := ValWup
  else begin
   SpanTime := dtNow - dtPrv;
   if TimeSpan < SpanTime then begin
    GainTime := SpanTime - TimeSpan;
    if TimeGain < GainTime then
     TimeGain := GainTime;
    TimeSpan := SpanTime
   end;
   if PshMod = wuPshEnd then begin
    SpanTime := (TimeSpan + SpanTime) / 2;
    GainTime := TimeGain
   end else
    GainTime := TimeGain / 2;
   SpanTime := SpanTime + (ValEnd - ValWup) * GainTime / (ValEnd - ValBeg);
   LR := ValEnd - CurVal; LW := ValEnd - ValWup;
   SpanTime := SpanTime * LR * LR / LW / LW;
   SpanTime := SpanTime / (CurVal - ValPrv);

   if SpanTime <= dtNow - dtPrv then begin
    if SpanTime = 0 then Incr := 0 else Incr := Trunc((dtNow - dtPrv)/SpanTime);

    if (Incr <= 0) or (ValEnd < ValWup + Incr) then
     ValWup := ValEnd
    else
     Inc(ValWup, Incr);
   _Progress(ValWup)
   end else begin ValPrv := CurVal; Result := ValWup end
  end
 end
end;

class function TWeighUp.ScaleDwn(iVal: UInt64; bCnt: Boolean = false): UInt64;
const
  F = 1;
 function GetAspr: Integer;
 var
  i, j: Integer;
 begin
  SetLength(aspr, 0); i := 1; j := 1; Result := 1;
  while i + j shl F < iVal do begin j := j shl F; Inc(i, j); Inc(Result) end;
  SetLength(aspr, Result + 1); aspr[0] := iVal - i;
  for i := 1 to Result do begin aspr[i] := j; j := j shr F end
 end;
var
 i, j, k, m: Integer;
begin
 i := Length(aspr);
 if bCnt then begin
  Result := 0; i := GetAspr; j := 0;
  for k := 0 to i do begin Inc(Result, aspr[k] shl j); Inc(j, F) end;
 end else if 0 < i then begin
  Result := 0; Dec(i); j := 0; k := 0; m := 0;
  while j + aspr[k] <= iVal do begin
   Inc(Result, aspr[k] shl m); Inc(j, aspr[k]); Inc(k); Inc(m, F)
  end;
  Inc(Result, (iVal - j) shl m)
 end
end;

procedure Init;
var
 CBI: TConsoleScreenBufferInfo;
begin
 GetConsoleScreenBufferInfo(hCon, CBI);
 TextAttr := CBI.wAttributes;
 LastMode := $00;
 CrtTxtColorDefault := TextAttr;
 CrtBkgColorDefault := $00;
end;

initialization
 hCon := GetStdHandle(STD_OUTPUT_HANDLE);
 Init;
 TCrtcProgress.FInitialialized := false;
 LastTextColor := $FF;
// CrtcTextBackground(20);
end.
