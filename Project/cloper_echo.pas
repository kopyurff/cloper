{ cloper_echo
  The unit of Cloning Optimizer (Cloper) tool.

  The file contains high level functionality to perform output to CRT and to
  .log file.
*
*                                           Copyright (C) 2015-2017 Anton Kopiev
*                                                     GNU General Public License
*
}
unit cloper_echo;

interface
uses
 Windows, cloper_base, ConTools;

type
  TMsgCategory = (mcCat = CrtTxtColorCategory,
    mcSubCat = CrtTxtColorSubCategory,
    mcSub2Cat = CrtTxtColorSub2Category,
    mcWarning = CrtTxtColorWarning,
    mcError = CrtTxtColorError,
    mcLowPriError = CrtTxtColorLPError,
    mcEcho = CrtTxtColorEcho,
    mcUserError = CrtTxtColorUserError,
    mcPlain = 0);

procedure OutputMessage(MsgType: TMsgType; Message_: String;
                        MsgCat: TMsgCategory = mcPlain); overload;
procedure OutputMessage(MsgType: TMsgType; Message_: String;
                        const vals: array of const;
                        MsgCat: TMsgCategory = mcPlain); overload;
function OutputSysError(Sever: Boolean; SubMessage_: String = '';
  ErrorCode: DWORD = 0) : Integer;

function EchoFailure(num: Integer; Msg: String = ''): Boolean; overload;
function EchoFailure(num: Integer; Msg: String; const vals: array of const): Boolean; overload;

implementation
uses
 SysUtils;

procedure OutputMessage(MsgType: TMsgType;
  Message_: String; MsgCat: TMsgCategory = mcPlain);
begin
 case MsgType of
 mFailure..mSystemErr:
  MsgCat := mcError;
 mLowPriErr, mSysLowPrE:
  MsgCat := mcLowPriError;
 mEcoHighPri..mEcoNoDtLP:
  MsgCat := mcEcho;
 mWarning:
  MsgCat := mcWarning;
 mUserError:
  MsgCat := mcUserError;
 end;

 if MsgCat <> mcPlain then CrtcTextColor(Byte(MsgCat));

 if TCrtcProgress.Initialialized then
  TCrtcProgress.Message(MsgType, Message_)
 else
  DoMessageOutput(MsgType, Message_);

 if MsgCat <> mcPlain then CrtcTextColor(CrtTxtColorDefault)
end;

procedure OutputMessage(MsgType: TMsgType;
  Message_: String; const vals: array of const; MsgCat: TMsgCategory = mcPlain);
begin
 OutputMessage(MsgType, Format(Message_, vals), MsgCat)
end;

function OutputSysError(Sever: Boolean; SubMessage_: String = '';
  ErrorCode: DWORD = 0) : Integer;
var
 ResMsg: String;
begin
 if TCrtcProgress.Initialialized then
  Result := TCrtcProgress.OutSysError(Sever, SubMessage_, ErrorCode)
 else begin
  OutputMessage(GetSysErrorForOutput(Sever, ResMsg, ErrorCode,
      SubMessage_), ResMsg);
  Result := ErrorCode
 end
end;

{ EchoFailure
  Prints failures of tool by its number.
}
function EchoFailure(num: Integer; Msg: String = ''): Boolean;
begin
 if Msg = '' then Msg := GetFailureByNumber(num) else
  Msg := GetFailureByNumber(num) + ' ' + Msg;

 Result := true; OutputMessage(mFailure, Msg, mcError)
end;

function EchoFailure(num: Integer; Msg: String; const vals: array of const): Boolean;
begin
 Result := EchoFailure(num, Format(Msg, vals))
end;

end.
