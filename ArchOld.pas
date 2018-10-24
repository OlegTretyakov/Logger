unit ArchOld;


interface

uses Vcl.StdCtrls, Vcl.Buttons, Vcl.Controls, Vcl.ComCtrls, System.Classes,
  Winapi.Messages, Vcl.Forms, LoggerInterface;


  const
  LOGGERS_MAX = WM_USER+ 451;
  LOGGERS_PRG_UPDATE = LOGGERS_MAX + 1;
  FILES_MAX = LOGGERS_PRG_UPDATE + 1;
  FILES_PRG_UPDATE = FILES_MAX + 1;
  PROC_MAX = FILES_PRG_UPDATE + 1;
  PROC_PRG_UPDATE = PROC_MAX + 1;
  ARCH_WINDOW_CLOSE = PROC_PRG_UPDATE + 1;
type
  TArchOldLogsFrm = class(TForm)
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    ProgressBar3: TProgressBar;
    CancelBtn: TBitBtn;
    Label2: TLabel;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    fLoggers : ILoggers;
    FUserCanceled,
    FThreadActive : boolean;
    procedure OnLoggersMax(var Message : TMessage); message LOGGERS_MAX;
    procedure OnLoggersProgerss(var Message : TMessage); message LOGGERS_PRG_UPDATE;
    procedure OnFilesMax(var Message : TMessage); message FILES_MAX;
    procedure OnFilesProgerss(var Message : TMessage); message FILES_PRG_UPDATE;
    procedure OnProcessingMax(var Message : TMessage); message PROC_MAX;
    procedure OnProcessingProgerss(var Message : TMessage); message PROC_PRG_UPDATE;
    procedure OnCloseForm(var Message : TMessage); message ARCH_WINDOW_CLOSE;
  public
    constructor Create(AOwner: TComponent; const ALoggers : ILoggers); reintroduce;
    destructor Destroy; override;
  end;


implementation


{$R *.dfm}
uses
System.SysUtils,
System.DateUtils,
SynCommons2,
//sevenzip_16_04,
sevenzip2,
Winapi.Windows, LoggerEx;

const
cWaitTimeOut = 1000;//mSec

procedure ArchOldFiles(AOwner: TComponent; const ALoggers : ILoggers); stdcall;
var vArchOldLogsFrm : TArchOldLogsFrm;
begin
  vArchOldLogsFrm := TArchOldLogsFrm.Create(AOwner, ALoggers);
  try
    vArchOldLogsFrm.ShowModal;
  finally
    FreeAndNil(vArchOldLogsFrm);
  end;
end; exports ArchOldFiles;

type
  TCallBackParams = record
    FormHandle,
    CallBackEvent : THandle;
    Result : HRESULT;
    StrMessage : WideString;
  end;   
  pCallBackParams = ^TCallBackParams;
  TLogsArchThread = class (TThread)
   private
    fFormHandle : THandle;
    fLoggers : ILoggers;
   protected
    procedure Execute; override;
   public
    constructor Create(AFormHandle : THandle; const ALoggers : ILoggers); reintroduce;
    destructor Destroy; override;
  end;


function ProgressCallback(sender: Pointer; total: boolean; value: int64): HRESULT; stdcall;
var vCallBackParams : pCallBackParams;
begin
  vCallBackParams := pCallBackParams(sender);
  Result := E_ABORT;
  vCallBackParams^.Result := E_ABORT;

  if total then
    PostMessage(vCallBackParams^.FormHandle, PROC_MAX, Integer(sender), Integer(value))
  else
    PostMessage(vCallBackParams^.FormHandle, PROC_PRG_UPDATE, Integer(sender), Integer(value));

  if (WaitForSingleObject(vCallBackParams.CallBackEvent, cWaitTimeOut) = WAIT_OBJECT_0) then
    Result := vCallBackParams^.Result;
end;

{ TLogsArchThread }


constructor TLogsArchThread.Create(AFormHandle : THandle; const ALoggers : ILoggers);
begin
 FreeOnTerminate := true;
 fFormHandle := AFormHandle;
 fLoggers := ALoggers;
 inherited Create(false);
end;

destructor TLogsArchThread.Destroy;
begin
  fLoggers := nil;
  inherited;
end;

procedure TLogsArchThread.Execute;
var
vLoggerIdx, vFileIdx : integer;
vCallBackParams : pCallBackParams;
vFilesList : TStringList;
vArch: I7zOutArchive;
vLogger : ILoggerEx;
vConfig: TLogConfig;
vClassConfig : TLogClassConfig;
aTime: TDateTime;
Y,M,D: word;
vArchPath, vArchName: TFileName;
tmp: array[0..7] of AnsiChar;
begin
  New(vCallBackParams);
  vCallBackParams.FormHandle := fFormHandle;
  vCallBackParams.CallBackEvent := CreateEvent(nil, false, false, nil);
  vArch := CreateOutArchive(CLSID_CFormat7z);
  SetCompressionLevel(vArch, 5);
  SetMultiThreading(vArch, TThread.ProcessorCount);
  SevenZipSetCompressionMethod(vArch, m7BZip2);
  vArch.SetProgressCallback(vCallBackParams, ProgressCallback);
  vFilesList := TStringList.Create;
  try
    vLoggerIdx := 0;
    vCallBackParams.Result := S_OK;
    PostMessage(fFormHandle, LOGGERS_MAX, Integer(vCallBackParams), fLoggers.Count);
    if (WaitForSingleObject(vCallBackParams.CallBackEvent, cWaitTimeOut) = WAIT_TIMEOUT)
    or (vCallBackParams.Result = E_ABORT) then
      Terminate;
    while (not Terminated) and (vLoggerIdx < fLoggers.Count) do
    begin
      vLogger := nil;
      if fLoggers.Item[vLoggerIdx, ILoggerEx, vLogger] then
      begin
        vLogger.GetConfig(@vConfig);
        vLogger.GetClassConfig(@vClassConfig);
        vCallBackParams.StrMessage := vClassConfig.ModuleName;
        vCallBackParams.Result := S_OK;
        PostMessage(fFormHandle, LOGGERS_PRG_UPDATE, Integer(vCallBackParams), vLoggerIdx);
        if (WaitForSingleObject(vCallBackParams.CallBackEvent, cWaitTimeOut) = WAIT_TIMEOUT)
        or (vCallBackParams.Result = E_ABORT) then
          Terminate;
        vLogger.Flush(true);
        vLogger.FindOldFiles(vFilesList);
        vFileIdx := 0;
        vCallBackParams.Result := S_OK;
        PostMessage(fFormHandle, FILES_MAX, Integer(vCallBackParams), vFilesList.Count);
        if (WaitForSingleObject(vCallBackParams.CallBackEvent, cWaitTimeOut) = WAIT_TIMEOUT)
        or (vCallBackParams.Result = E_ABORT) then
        begin
          Terminate;
          break;
        end;
        while (not Terminated) and (vFileIdx < vFilesList.Count) do
        begin
          vCallBackParams.StrMessage := vFilesList[vFileIdx];
          vCallBackParams.Result := S_OK;
          PostMessage(fFormHandle, FILES_PRG_UPDATE, Integer(vCallBackParams), vFileIdx);
          if (WaitForSingleObject(vCallBackParams.CallBackEvent, cWaitTimeOut) = WAIT_TIMEOUT)
          or (vCallBackParams.Result = E_ABORT) then
          begin
            Terminate;
            break;
          end;
          if not vConfig.ArhiveBeforeDelete then
          begin
            vCallBackParams.Result := S_OK;
            PostMessage(fFormHandle, FILES_PRG_UPDATE, Integer(vCallBackParams), 0);
            if (WaitForSingleObject(vCallBackParams.CallBackEvent, cWaitTimeOut) = WAIT_TIMEOUT)
            or (vCallBackParams.Result = E_ABORT) then
            begin
              Terminate;
              break;
            end;
            vCallBackParams.Result := S_OK;
            PostMessage(fFormHandle, PROC_MAX, Integer(vCallBackParams), 1);
            if (WaitForSingleObject(vCallBackParams.CallBackEvent, cWaitTimeOut) = WAIT_TIMEOUT)
            or (vCallBackParams.Result = E_ABORT) then
            begin
              Terminate;
              break;
            end;

            DeleteFile(pwideChar(vFilesList[vFileIdx]));
            vCallBackParams.Result := S_OK;
            PostMessage(fFormHandle, PROC_PRG_UPDATE, Integer(vCallBackParams), 1);
            if (WaitForSingleObject(vCallBackParams.CallBackEvent, cWaitTimeOut) = WAIT_TIMEOUT)
            or (vCallBackParams.Result = E_ABORT) then
            begin
              Terminate;
              break;
            end;
          end else
          begin
            aTime := FileAgeToDateTime(vFilesList[vFileIdx]);
            DecodeDate(aTime,Y,M,D);
            tmp[0] := '\';
            YearToPChar(Y,@tmp[1]);
            PWord(@tmp[5])^ := TwoDigitLookupW[M];
            vArchPath := vLogger.ArchivePath+Ansi7ToString(tmp,7);
            vArchName := Format('%s\%s',[vArchPath,
              ChangeFileExt(ExtractFileName(vFilesList[vFileIdx]), '.'+vClassConfig.ArhiveExtension)]);
            if DirectoryExists(vArchPath)  or ForceDirectories(vArchPath)  then
            begin
              vArch.AddFile(vFilesList[vFileIdx], ExtractFileName(vFilesList[vFileIdx]));
              try
                vArch.SaveToFile(vArchName);
                vArch.ClearBatch;
                if (vCallBackParams.Result = S_OK)  then
                  DeleteFile(pwideChar(vFilesList[vFileIdx]))
                else
                begin
                  Terminate;
                  break;
                end;
              except
                Terminate;
                break;
              end;
            end;
          end;
          inc(vFileIdx);
          vCallBackParams.Result := S_OK;
          PostMessage(fFormHandle, FILES_PRG_UPDATE, Integer(vCallBackParams), vFileIdx);
          if (WaitForSingleObject(vCallBackParams.CallBackEvent, cWaitTimeOut) = WAIT_TIMEOUT)
          or (vCallBackParams.Result = E_ABORT) then
          begin
            Terminate;
            break;
          end;
        end;
      end;
      inc(vLoggerIdx);
      vCallBackParams.Result := S_OK;
      PostMessage(fFormHandle, LOGGERS_PRG_UPDATE, Integer(vCallBackParams), vLoggerIdx);
      if (WaitForSingleObject(vCallBackParams.CallBackEvent, cWaitTimeOut) = WAIT_TIMEOUT)
      or (vCallBackParams.Result = E_ABORT) then
      begin
        Terminate;
        break;
      end;
    end;
  finally   
    CloseHandle(vCallBackParams.CallBackEvent);
    Dispose(vCallBackParams);
    FreeAndNil(vFilesList);
    if not Terminated then
      PostMessage(fFormHandle, ARCH_WINDOW_CLOSE, 0, mrOk)
    else
      PostMessage(fFormHandle, ARCH_WINDOW_CLOSE, 0, mrAbort);
  end;
end;

procedure TArchOldLogsFrm.CancelBtnClick(Sender: TObject);
var
vCloseTime : TDateTime;
begin
  FUserCanceled := true;
  vCloseTime := now;
  while FThreadActive do
  begin
    Sleep(10);
    if SecondsBetween(now, vCloseTime) > 10 then
      break;
    Application.ProcessMessages;
  end;
  self.ModalResult := mrCancel;
end;

constructor TArchOldLogsFrm.Create(AOwner: TComponent;
  const ALoggers: ILoggers);
begin
  inherited Create(AOwner);
  fLoggers := ALoggers;
  FThreadActive := False;
  FUserCanceled := false;
end;

destructor TArchOldLogsFrm.Destroy;
begin
  fLoggers := nil;
  inherited;
end;

procedure TArchOldLogsFrm.FormShow(Sender: TObject);
begin
  FThreadActive := True;
  TLogsArchThread.Create(self.Handle, fLoggers);
end;

procedure TArchOldLogsFrm.OnCloseForm(var Message: TMessage);
begin
  FThreadActive := False;
  self.ModalResult := Message.LParam;
end;

procedure TArchOldLogsFrm.OnFilesMax(var Message: TMessage);
var vCallBackParams : pCallBackParams;
begin
  vCallBackParams := pCallBackParams(Pointer(Message.WParam));
  ProgressBar2.Max := Message.LParam;
  if FUserCanceled then
    vCallBackParams^.Result := E_ABORT
  else
    vCallBackParams^.Result := S_OK;
  SetEvent(vCallBackParams^.CallBackEvent);
end;

procedure TArchOldLogsFrm.OnFilesProgerss(var Message: TMessage);
var vCallBackParams : pCallBackParams;
begin
  vCallBackParams := pCallBackParams(Pointer(Message.WParam));
  ProgressBar2.Position := Message.LParam;
  Label2.Caption := vCallBackParams^.StrMessage;
  if FUserCanceled then
    vCallBackParams^.Result := E_ABORT
  else
    vCallBackParams^.Result := S_OK;
  SetEvent(vCallBackParams^.CallBackEvent);
end;

procedure TArchOldLogsFrm.OnLoggersMax(var Message: TMessage);
var vCallBackParams : pCallBackParams;
begin
  vCallBackParams := pCallBackParams(Pointer(Message.WParam));
  ProgressBar1.Max := Message.LParam;
  if FUserCanceled then
    vCallBackParams^.Result := E_ABORT
  else
    vCallBackParams^.Result := S_OK;
  SetEvent(vCallBackParams^.CallBackEvent);
end;

procedure TArchOldLogsFrm.OnLoggersProgerss(var Message: TMessage);
var vCallBackParams : pCallBackParams;
begin
  vCallBackParams := pCallBackParams(Pointer(Message.WParam));
  ProgressBar1.Position := Message.LParam;
  Label1.Caption := vCallBackParams^.StrMessage;
  if FUserCanceled then
    vCallBackParams^.Result := E_ABORT
  else
    vCallBackParams^.Result := S_OK;
  SetEvent(vCallBackParams^.CallBackEvent);
end;

procedure TArchOldLogsFrm.OnProcessingMax(var Message: TMessage);
var vCallBackParams : pCallBackParams;
begin
  vCallBackParams := pCallBackParams(Pointer(Message.WParam));
  ProgressBar3.Max := Message.LParam;
  if FUserCanceled then
    vCallBackParams^.Result := E_ABORT
  else
    vCallBackParams^.Result := S_OK;
  SetEvent(vCallBackParams^.CallBackEvent);
end;

procedure TArchOldLogsFrm.OnProcessingProgerss(var Message: TMessage);
var vCallBackParams : pCallBackParams;
begin
  vCallBackParams := pCallBackParams(Pointer(Message.WParam));
  ProgressBar3.Position := Message.LParam;
  if FUserCanceled then
    vCallBackParams^.Result := E_ABORT
  else
    vCallBackParams^.Result := S_OK;
  SetEvent(vCallBackParams^.CallBackEvent);
end;

end.
