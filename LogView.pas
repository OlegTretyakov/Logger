unit LogView;

interface

uses
  System.Classes, Vcl.Controls, Vcl.Forms,
  Vcl.ComCtrls, FormsControllerInterface, LoggerInterface;

type
  TLogViewFrm = class(TForm,
                    IShowMdiForm,
                    IFormToFront,
                    IFormProps,
                    IFormOnDestroyEvent)
    PageControl1: TPageControl;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
   private
    fOnDestroy : TNotifyEvent;
    {IShowMdiForm}
    procedure IShowMdiForm.Show = IShowMdiForm_Show;
    procedure IShowMdiForm_Show; stdcall;
    {IFormToFront}
    procedure IFormToFront.BringToFront = IFormToFront_BringToFront;
    procedure IFormToFront_BringToFront; stdcall;
    {IFormProps}
    function IFormProps.GetHeight = IFormProps_GetHeight;
    function IFormProps.GetTop = IFormProps_GetTop;
    procedure IFormProps.SetTop = IFormProps_SetTop;
    function IFormProps_GetHeight : integer; stdcall;
    function IFormProps_GetTop : integer; stdcall;
    procedure IFormProps_SetTop(const Value : Integer);stdcall;
    {IFormOnDestroyEvent}
    procedure SetOnDestroy(const AEvent : TNotifyEvent);stdcall;
    function GetOnDestroy : TNotifyEvent;stdcall;
   public
    constructor Create(AOwner : TComponent; const ALoggers : ILoggers); reintroduce;
    destructor Destroy;override;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;




implementation
uses System.SysUtils, LoggerEx, LogViewFrame;

{$R *.dfm}

procedure LogViewModal(AOwner: TComponent; const ALoggers : ILoggers); stdcall;
var vFrm : TLogViewFrm;
begin
  vFrm := TLogViewFrm.Create(AOwner, ALoggers);
  try
    vFrm.FormStyle := fsNormal;
    vFrm.Visible := false;
    vFrm.ShowModal;
  finally
    FreeAndNil(vFrm);
  end;
end; exports LogViewModal;

function CreateLogViewFormInstance(AOwner: TComponent; const ALoggers : ILoggers):TComponent;stdcall;
begin
  result := TLogViewFrm.Create(AOwner, ALoggers);
end; exports CreateLogViewFormInstance;
     
{ TMainLogView }

procedure TLogViewFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TLogViewFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i : integer;
begin
  if PageControl1.PageCount < 1 then
  exit;
  i := 0;
  while (i < PageControl1.ActivePage.ControlCount)
    and not (PageControl1.ActivePage.Controls[i] is TLogViewFm) do
    inc(i);
  if (i < PageControl1.ActivePage.ControlCount) then
    TLogViewFm(PageControl1.ActivePage.Controls[i]).FormKeyDown(Key, Shift);
end;

function TLogViewFrm.IFormProps_GetHeight: integer;
begin
  result := Self.Height;
end;

function TLogViewFrm.IFormProps_GetTop: integer;
begin
  Result := Self.Top;
end;

procedure TLogViewFrm.IFormProps_SetTop(const Value: Integer);
begin
  Self.Top := Value;
end;

procedure TLogViewFrm.SetOnDestroy(const AEvent: TNotifyEvent);
begin
  fOnDestroy := AEvent;
end;

function TLogViewFrm.GetOnDestroy: TNotifyEvent;
begin
  result := fOnDestroy;
end;

procedure TLogViewFrm.IShowMdiForm_Show;
var
i : word;
begin
  i := 0;
  while i < Screen.CustomFormCount do
  begin
    if (Screen.CustomForms[i] <> Application.MainForm)
    and  (Screen.CustomForms[i].WindowState = wsMaximized) then
       Screen.CustomForms[i].WindowState := wsNormal;
    inc(i);
  end;
  self.FormStyle := fsMDIChild;
  self.Visible := true;
  self.OnClose := self.FormClose;
  self.Show;
end;

procedure TLogViewFrm.IFormToFront_BringToFront;
var
i : word;
begin
  i := 0;
  while i < Screen.CustomFormCount do
  begin
    if (Screen.CustomForms[i] <> Application.MainForm)
    and  (Screen.CustomForms[i].WindowState = wsMaximized) then
       Screen.CustomForms[i].WindowState := wsNormal;
    inc(i);
  end;
  BringToFront;
end;

constructor TLogViewFrm.Create(AOwner: TComponent; const ALoggers : ILoggers);
var
i : integer;
t : TTabSheet;
vLogger : ILoggerEx;
vClassConfig : TLogClassConfig;
f : TLogViewFm;
begin
  inherited Create(AOwner);
  fOnDestroy := nil;
  i := 0;
  while i < ALoggers.Count  do
  begin
    if ALoggers.Item[i, ILoggerEx, vLogger] then
    begin
      t := TTabSheet.Create(PageControl1);
      t.Name := format('TabLog%d',[i]);
      vLogger.GetClassConfig(@vClassConfig);
      t.Caption := vClassConfig.ModuleName;
      t.PageControl := PageControl1;
      t.Align := alClient;
      t.Visible := true;
      f := TLogViewFm.Create(t);
      f.Parent := t;
      f.Visible := true;
      f.Align := alClient;
      f.Load(vLogger);
      f.Show;
    end;
    vLogger := nil;
    inc(i);
  end;
  if (PageControl1.PageCount > 0) then
    PageControl1.ActivePageIndex := 0;
end;

destructor TLogViewFrm.Destroy;
begin
  if Assigned(fOnDestroy) then
    fOnDestroy(Self);
  inherited Destroy;
end;

end.
