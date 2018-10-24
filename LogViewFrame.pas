unit LogViewFrame;

interface

uses  Vcl.Forms, SynCommons2, Vcl.Menus, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls,
  Vcl.Controls, Vcl.CheckLst, Vcl.ExtCtrls, System.Classes, Winapi.Windows,
  System.SysUtils, LoggerEx;

type
  TLogViewFm = class(TFrame)
    Splitter2: TSplitter;
    PanelLeft: TPanel;
    BtnOpen: TButton;
    EventsList: TCheckListBox;
    EditSearch: TEdit;
    BtnSearch: TButton;
    BtnStats: TButton;
    MemoBottom: TMemo;
    List: TDrawGrid;
    OpenDialog: TOpenDialog;
    GridPopupMenu: TPopupMenu;
    RefreshMnu: TMenuItem;
    SettingsBtn: TButton;
    CompressAndSaveMnuItem: TMenuItem;
    procedure EventsListClickCheck(Sender: TObject);
    procedure BtnSearchClick(Sender: TObject);

    procedure ListClick(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure BtnStatsClick(Sender: TObject);
    procedure BtnOpenClick(Sender: TObject);
    procedure EventsListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure EventsListDblClick(Sender: TObject);
    procedure RefreshMnuClick(Sender: TObject);
    procedure SettingsBtnClick(Sender: TObject);
    procedure CompressAndSaveMnuItemClick(Sender: TObject);
  private
    procedure LoadLog;
  protected
    FLogger : ILoggerEx;
    FLog: TSynLogFile;
    FTxt: TMemoryMapText;
    FLogSelected: TIntegerDynArray;
    FLogSelectedCount: integer;
    FLogUncompressed: TMemoryStream;
    procedure SetLogFileName(const Value: TFileName);
    procedure SetListItem(Index: integer; const search: RawUTF8='');
  public
    procedure Load(const ALogger : ILoggerEx);
    property LogFileName: TFileName write SetLogFileName;
    destructor Destroy; override;
    procedure FormKeyDown(var Key: Word;
      Shift: TShiftState);
  end;

implementation
uses lgui.consts, LoggerInterface, LogConfig, ArchForm, Vcl.Graphics;
{$R *.dfm}


procedure TLogViewFm.Load(const ALogger : ILoggerEx);
var
vClassConfig : TLogClassConfig;
begin
  FLogger := ALogger;
  fLogger.GetClassConfig(@vClassConfig);
  OpenDialog.DefaultExt := '.'+vClassConfig.FileExtension;
  OpenDialog.Filter := Format('log files|*.%s;*.%s',[vClassConfig.FileExtension, vClassConfig.ArhiveExtension]);
  LoadLog;
  //inherited Show
end;

destructor TLogViewFm.Destroy;
begin
  FLog.Free;
  if assigned(FLogUncompressed) then
    FreeAndNil(FLogUncompressed);
  FTxt.Free;
  FLogger := nil;
  inherited Destroy;
end;

procedure TLogViewFm.SetLogFileName(const Value: TFileName);
var E: TLogInfo;
evidx : integer;
UnArchFrm : TArchFrm;
vClassConfig : TLogClassConfig;
begin
  CompressAndSaveMnuItem.Enabled := false;
  FreeAndNil(FLog);
  FreeAndNil(FTxt);
  FreeAndNil(FLogUncompressed);
  Finalize(FLogSelected);
  FLogSelectedCount := 0;
  Screen.Cursor := crHourGlass;
  try
    List.RowCount := 0;
    fLogger.GetClassConfig(@vClassConfig);
    if SameText(ExtractFileExt(Value),format('.%s',[vClassConfig.ArhiveExtension])) then
    begin
      FLogUncompressed := TMemoryStream.Create;
      UnArchFrm := TArchFrm.Create(self, mUnCompress, FLogUncompressed, Value, '');
      try
       if (UnArchFrm.ShowModal = mrOk) then
       begin
        FLog := TSynLogFile.Create(FLogUncompressed.Memory, FLogUncompressed.Size);
        FLog.FileName := UnArchFrm.ItemName;
       end else
         exit;
      finally
        FreeAndNil(UnArchFrm);
      end;
    end else
      FLog := TSynLogFile.Create(Value);
    EventsList.Items.BeginUpdate;
    EventsList.Items.Clear;
    if FLog.Count=0 then
    begin // if not a TSynLog file -> open as plain text
      FreeAndNil(FLog);
      if FLogUncompressed<>nil then
        FTxt := TMemoryMapText.Create(FLogUncompressed.Memory,FLogUncompressed.Size)
      else
        FTxt := TMemoryMapText.Create(Value);
      if FTxt.Count=0 then
        FreeAndNil(FTxt)
      else
      begin
        List.ColCount := 1;
        List.ColWidths[0] := 2000;
      end;
    end else
    begin
      List.ColCount := 3;
      CompressAndSaveMnuItem.Enabled := not assigned(FLogUncompressed)
      and FileExists(FLog.FileName);
      List.ColWidths[0] := 70;
      List.ColWidths[1] := 60;
      List.ColWidths[2] := 2000;
      SetLength(FLogSelected,FLog.Count);
      for E := succ(lNone) to high(E) do
      begin
        if TSynLogInfo(E) in FLog.EventLevelUsed then
        begin
          evidx := EventsList.Items.AddObject(C_EventCaption[E],pointer(ord(E)));
          EventsList.Checked[evidx] := E in vClassConfig.DefaultLevelsView;
        end;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  EventsList.Items.EndUpdate;
  EventsList.Height := 8+EventsList.Count*EventsList.ItemHeight;
  BtnStats.Top := EventsList.Top+EventsList.Height+32;
  EventsList.Enabled := FLog<>nil;
  BtnStats.Enabled := FLog<>nil;
  //SettingsBtn.Enabled := FLog<>nil;
  BtnSearch.Enabled := (FLog<>nil) or (FTxt<>nil);
  EditSearch.Enabled := (FLog<>nil) or (FTxt<>nil);
  List.Visible := (FLog<>nil) or (FTxt<>nil);
  List.Enabled := List.Visible;
  EventsListClickCheck(nil);
end;

procedure TLogViewFm.LoadLog;
begin
  FLogger.Flush(true);
  LogFileName := FLogger.FileName;
end;

procedure TLogViewFm.CompressAndSaveMnuItemClick(Sender: TObject);
var
SaveDialog : TSaveDialog;
ArhFrm : TArchFrm;  
vClassConfig : TLogClassConfig;
begin
  if not FileExists(FLog.FileName) then
    exit;
  SaveDialog := TSaveDialog.Create(nil);
  try   
    fLogger.GetClassConfig(@vClassConfig);
    SaveDialog.FileName := ChangeFileExt(ExtractFileName(FLog.FileName),'');
    SaveDialog.DefaultExt := Format('Compressed log files|*.%s',[vClassConfig.ArhiveExtension]);
    SaveDialog.Filter :=Format('Compressed log files|*.%s',[vClassConfig.ArhiveExtension]);
    SaveDialog.InitialDir := FLogger.ArchivePath;
    if SaveDialog.Execute then
    begin
       ArhFrm := TArchFrm.Create(self, mCompress, FLogUncompressed, FLog.FileName, SaveDialog.FileName);
      try
        ArhFrm.ShowModal;
      finally
        FreeAndNil(ArhFrm);
      end;
    end;
  finally
    FreeAndNil(SaveDialog);
  end;
end;

procedure TLogViewFm.RefreshMnuClick(Sender: TObject);
begin
  LoadLog;
end;

procedure TLogViewFm.EventsListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
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

procedure TLogViewFm.EventsListClickCheck(Sender: TObject);
var i, ndx: integer;
    Sets: TSynLogInfos;
begin
  FLogSelectedCount := 0;
  if FLog<>nil then
  begin
    ndx := List.Row;
    if ndx>=0 then
      ndx := FLogSelected[ndx];
    integer(Sets) := 0;
    for i := 0 to EventsList.Count-1 do
      if EventsList.Checked[i] then
        Include(Sets,TSynLogInfo(EventsList.Items.Objects[i]));
    if integer(Sets)<>0 then
      for i := 0 to FLog.Count-1 do
      if FLog.EventLevel[i] in Sets then
      begin
        FLogSelected[FLogSelectedCount] := i;
        inc(FLogSelectedCount);
      end;
    if ndx>=0 then
      ndx := IntegerScanIndex(pointer(FLogSelected),FLogSelectedCount,ndx);
    List.RowCount := FLogSelectedCount;
  end else
  begin
    ndx := -1;
    if FTxt<>nil then
      List.RowCount := FTxt.Count
    else
      List.RowCount := 0;
  end;
  SetListItem(ndx);
  if List.Visible then
  begin
    //List.SetFocus;
    List.Repaint;
    ListClick(nil);
  end;
end;

procedure TLogViewFm.EventsListDblClick(Sender: TObject);
var i: integer;
    E: TSynLogInfo;
begin
  if FLog=nil then
    exit;
  i := EventsList.ItemIndex;
  if i<0 then
    exit;
  E := TSynLogInfo(EventsList.Items.Objects[i]);
  // search from next item
  for i := List.Row+1 to FLogSelectedCount-1 do
    if FLog.EventLevel[FLogSelected[i]]=E then
    begin
      SetListItem(i);
      exit;
    end;
  // search from beginning
  for i := 0 to List.Row-1 do
    if FLog.EventLevel[FLogSelected[i]]=E then
    begin
      SetListItem(i);
      exit;
    end;
end;

procedure TLogViewFm.ListDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
type TListCol = (colDate, colEvent, colText);
var txt: string;
    b: boolean;
    Index: integer;
begin
  with List.Canvas do
  begin
    if FLog<>nil then
    begin
      Brush.Style := bsClear;
      if cardinal(ARow)<cardinal(FLogSelectedCount) then
      begin
        Index := FLogSelected[ARow];
        b := (gdFocused in State) or (gdSelected in State);
        if b then
          Brush.Color := clBlack else
          Brush.Color := LOG_COLORS[b,FLog.EventLevel[Index]];
        Font.Color  := LOG_COLORS[not b,FLog.EventLevel[Index]];
        FillRect(Rect);
        case TListCol(ACol) of
        colDate:
          txt := TimeToStr(FLog.EventDateTime(Index));
        colEvent:
          txt := C_EventCaption[TLogInfo(FLog.EventLevel[Index])];
        colText:
          txt := {UTF8ToString(}StringReplaceAll(Copy(FLog.Lines[Index],24,400),#9,'   '){)};
          //fLog.Lines[Index];
        end;
        TextOut(Rect.Left+4,Rect.Top,txt);
      end else
      begin
        Brush.Color := clLtGray;
        FillRect(Rect);
      end;
    end else
    begin
      if FTxt<>nil then
        TextRect(Rect,Rect.Left+4,Rect.Top,FTxt.Strings[ARow])
      else
        FillRect(Rect);
    end;
  end;
end;

procedure TLogViewFm.BtnSearchClick(Sender: TObject);
var s: RawUTF8;
    ndx, i: integer;
begin
  s := UpperCase(EditSearch.Text);
  //s := UpperCase(StringToUTF8(EditSearch.Text));
  //s := EditSearch.Text;
  //s := StringToUTF8(EditSearch.Text);
  Screen.Cursor := crHourGlass;
  try
    ndx := List.Row;
    if FTxt<>nil then
    begin
      // search from next item
      for i := ndx+1 to FTxt.Count-1 do
        if FTxt.LineContains(s,i) then
        begin
          SetListItem(i,s);
          exit;
        end;
      // not found -> search from beginning
      for i := 0 to ndx-1 do
        if FTxt.LineContains(s,i) then
        begin
          SetListItem(i,s);
          exit;
        end;
    end else
    begin
      // search from next item
      for i := ndx+1 to FLogSelectedCount-1 do
        if FLog.LineContains(s,FLogSelected[i]) then
        begin
          SetListItem(i,s);
          exit;
        end;
      // not found -> search from beginning
      for i := 0 to ndx-1 do
        if FLog.LineContains(s,FLogSelected[i]) then
        begin
          SetListItem(i,s);
          exit;
        end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TLogViewFm.FormKeyDown(var Key: Word;
  Shift: TShiftState);
begin
  if (Shift=[]) and (Key=VK_F3) then
  begin
    BtnSearchClick(nil);
    List.SetFocus;
  end else
  if (Shift=[ssCtrl]) and (Key=ord('F')) then
  begin
    EditSearch.SetFocus;
  end;
end;

procedure TLogViewFm.ListClick(Sender: TObject);
var i: integer;
begin
  i := List.Row;
  if cardinal(i)>=cardinal(FLogSelectedCount) then
  begin
    if FTxt<>nil then
      MemoBottom.Text := UTF8ToUnicodeString(FTxt.Strings[i])
    else
      MemoBottom.Text := ''
  end else
  begin
    MemoBottom.Text := UTF8ToUnicodeString(FLog.Strings[FLogSelected[i]]);
    (*Format('%s %s %s',[FormatDateTime('dd.mm.yy hh:mm:ss:zzz',FLog.EventDateTime(FLogSelected[i])),
    C_EventCaption[TLogInfo(FLog.EventLevel[FLogSelected[i]])],
    {UTF8ToString(}StringReplaceAll(Copy(FLog.Lines[FLogSelected[i]],24,400),#9,'   '){)} ]);//FLog.Strings[FLogSelected[i]];    *)
  end;
end;

procedure TLogViewFm.ListDblClick(Sender: TObject);
var i, Level: integer;
begin
  i := List.Row;
  if (FLog<>Nil) and (cardinal(i)<=cardinal(FLogSelectedCount)) then
  begin
    Level := 0;
    case FLog.EventLevel[FLogSelected[i]] of
      sllEnter: // retrieve corresponding Leave event
        repeat
          inc(i);
          if i>=FLogSelectedCount then
            exit;
          case FLog.EventLevel[FLogSelected[i]] of
          sllEnter: Inc(Level);
          sllLeave: if Level=0 then begin
            SetListItem(i);
            exit;
          end else Dec(Level);
          end;
        until false;
      sllLeave: // retrieve corresponding Enter event
        repeat
          dec(i);
          if i<0 then
            exit;
          case FLog.EventLevel[FLogSelected[i]] of
          sllLeave: Inc(Level);
          sllEnter: if Level=0 then begin
            SetListItem(i);
            exit;
          end else Dec(Level);
          end;
        until false;
    end;
  end;
end;

resourcestring
  sEnterAddress = 'Enter an hexadecimal address:';
  sStats = #13#10'Log'#13#10'---'#13#10#13#10'Name: %s'#13#10'Size: %s'#13#10#13#10+
    'Executable'#13#10'----------'#13#10#13#10'Name: %s%s'#13#10'Version: %s'#13#10+
    'Date: %s'#13#10#13#10'Host'#13#10'----'#13#10#13#10'Computer: %s'#13#10+
    'User: %s'#13#10'CPU: %s'#13#10'OS: Windows %s (service pack %d)'#13#10+
    'Wow64: %d'#13#10#13#10'Log content'#13#10'-----------'#13#10#13#10+
    'Log started at: %s'#13#10'Events count: %d'#13#10'Methods count: %d'#13#10+
    'Time elapsed: %s'#13#10#13#10'Per event stats'#13#10'---------------'#13#10#13#10;

procedure TLogViewFm.BtnStatsClick(Sender: TObject);
var M: TMemo;
    F: TForm;
    s: string;
    sets: array[TSynLogInfo] of integer;
    i: integer;
begin
  F := TForm.Create(Application);
  try
    F.Caption := {FMainCaption+}BtnStats.Caption;
    if Screen.Fonts.IndexOf('Consolas')>=0 then
      F.Font.Name := 'Consolas'
    else
      F.Font.Name := 'Courier New';
    F.Position := poScreenCenter;
    F.Width := 700;
    F.Height := 600;
    M := TMemo.Create(F);
    M.Parent := F;
    M.Align := alClient;
    M.ScrollBars := ssVertical;
    M.WordWrap := true;
    M.ReadOnly := true;
    if FLog<>nil then
      with FLog do
      begin
        if InstanceName<>'' then
          s := ' / '+UTF8ToString(InstanceName);
        s := format(sStats,
          [FileName,Ansi7ToString(KB(Map.Size)),
           UTF8ToString(ExecutableName),s,Ansi7ToString(ExecutableVersion),
           DateTimeToStr(ExecutableDate),{UTF8ToString(}ComputerHost{)},
           {UTF8ToString(}RunningUser{)},Ansi7ToString(CPU),
           GetCaptionFromEnum(TypeInfo(TWindowsVersion),ord(OS)),ServicePack,
           Integer(Wow64),DateTimeToStr(StartDateTime),Count,LogProcCount,
           FormatDateTime('hh:mm:ss',EventDateTime(Count-1)-StartDateTime)]);
        fillchar(sets,sizeof(sets),0);
        for i := 0 to Count-1 do
          inc(sets[EventLevel[i]]);
        for i := 0 to EventsList.Count-1 do
          s := s+EventsList.Items[i]+': '+
            IntToStr(sets[TSynLogInfo(EventsList.Items.Objects[i])])+#13#10;
      end;
    M.Text := s;
    F.ShowModal;
  finally
    F.Free;
  end;
end;


procedure TLogViewFm.SettingsBtnClick(Sender: TObject);
var frm : TLogConfigFrm;   
begin
  frm := TLogConfigFrm.Create(self, fLogger);
  try
    frm.ShowModal;
  finally
    FreeAndNil(frm);
  end;
end;

procedure TLogViewFm.BtnOpenClick(Sender: TObject);
begin
  OpenDialog.InitialDir := ExtractFilePath(Application.ExeName) + 'log';
  if OpenDialog.Execute then
    LogFileName := OpenDialog.FileName;
end;

procedure TLogViewFm.SetListItem(Index: integer; const search: RawUTF8='');
var i: integer;
    s,ss: string;
begin
  if Index<0 then
    MemoBottom.Text := '' else
  begin
    List.Row := Index;
    if FTxt<>nil then
      s := FTxt.Strings[Index]
    else
      s := FLog.Strings[FLogSelected[Index]];
    MemoBottom.Text := s;
    if search<>'' then
    begin
      ss := UTF8ToString(search);
      i := Pos(ss, System.SysUtils.UpperCase(s));
      if i>0 then
      begin
        MemoBottom.SelStart := i-1;
        MemoBottom.SelLength := length(ss);
      end;
    end;
  end;
end;

end.
