unit LogConfig;

interface

uses
  System.Classes, Winapi.Windows, Vcl.Forms, Vcl.Mask,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.Controls, Vcl.CheckLst,
  LoggerInterface, LoggerEx, JvExStdCtrls, JvEdit, JvValidateEdit;

type
  TLogConfigFrm = class(TForm)
    EventsList: TCheckListBox;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    ApplyBtn: TBitBtn;
    Label3: TLabel;
    Label4: TLabel;
    AutoFlushTimeOutEdit: TJvValidateEdit;
    MaxMBSizeEdit: TJvValidateEdit;
    ArchiveAfterDaysEdit: TJvValidateEdit;
    procedure EventsListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ApplyBtnClick(Sender: TObject);
  private
    fLogger : ILoggerEx;
  public
    constructor Create(AOwner : TComponent; const ALogger : ILoggerEx); reintroduce;
    destructor Destroy; override;
    procedure GetConfig(var AConfig: TLogConfig);
    procedure SetConfig(AConfig: TLogConfig);
  end;

implementation
uses System.SysUtils, lgui.consts, SynCommons2;

{$R *.dfm}

procedure TLogConfigFrm.ApplyBtnClick(Sender: TObject);
var
vConfig: TLogConfig;
vStore : ILoggingConfStoreAccess;
begin
  try
    if Supports(fLogger, ILoggingConfStoreAccess, vStore) then
    begin
      GetConfig(vConfig);
      FLogger.SetConfig(@vConfig);
      vStore.SaveLoggerConfig(FLogger);
      vStore := nil;
    end;
  finally
    self.Close;
  end;
end;

constructor TLogConfigFrm.Create(AOwner: TComponent; const ALogger: ILoggerEx);
var
E: TLogInfo;
vConfig: TLogConfig;
vClassConfig : TLogClassConfig;
begin
  inherited Create(AOwner);
  fLogger := ALogger;
  fLogger.GetClassConfig(@vClassConfig);
  for E := succ(lNone) to high(E) do
  begin
    if (E in vClassConfig.EnabledLevels) then
      EventsList.Items.AddObject(C_EventCaption[E], pointer(ord(E)));
  end;
  fLogger.GetConfig(@vConfig);
  SetConfig(vConfig);
  ApplyBtn.Enabled := Supports(fLogger, ILoggingConfStoreAccess);
end;

destructor TLogConfigFrm.Destroy;
begin
  fLogger := nil;
  inherited;
end;

procedure TLogConfigFrm.EventsListDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var E: TSynLogInfo;
begin
  if Index<0 then
    exit;
  E := TSynLogInfo(EventsList.Items.Objects[Index]);
  with EventsList.Canvas do begin
    Brush.Color := LOG_COLORS[false,E];
    Font.Color  := LOG_COLORS[true,E];
    TextRect(Rect,Rect.Left+4,Rect.Top,C_EventCaption[TLogInfo(E)]);
  end;
end;

procedure TLogConfigFrm.GetConfig(var AConfig: TLogConfig);
var i: integer;
begin
  integer(AConfig.Levels) := 0;
  for i := 0 to EventsList.Count-1 do
    if EventsList.Checked[i] then
      Include(AConfig.Levels, TLogInfo(EventsList.Items.Objects[i]));
  AConfig.ArhiveBeforeDelete := CheckBox1.Checked;
  AConfig.ArchiveAfterDays := ArchiveAfterDaysEdit.Value;
  AConfig.AutoFlushTimeOut := 60 * AutoFlushTimeOutEdit.Value;
  AConfig.MaxMBSize := MaxMBSizeEdit.Value;
end;

procedure TLogConfigFrm.SetConfig(AConfig: TLogConfig);
var i: integer;
begin
  for i := 0 to EventsList.Count-1 do
    EventsList.Checked[i] := (TLogInfo(EventsList.Items.Objects[i]) in AConfig.Levels);
  CheckBox1.Checked := AConfig.ArhiveBeforeDelete;
  ArchiveAfterDaysEdit.Value := IntToStr(AConfig.ArchiveAfterDays);
  AutoFlushTimeOutEdit.Value := IntToStr(AConfig.AutoFlushTimeOut div 60);
  MaxMBSizeEdit.Value := IntToStr(AConfig.MaxMBSize);
end;

end.
