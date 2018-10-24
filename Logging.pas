unit Logging;

interface
  uses
  System.Classes, LoggerInterface, LoggerEx,
  SynCommons2, System.SysUtils, System.Contnrs, Winapi.Windows,
  Winapi.Messages;

  type

  TLogger = class(TObject, ILogger, ILoggerEx)
   private
    fLoggers : ILoggers;
    fLogger : TSynLog;
    fClassConfig : TLogClassConfig;
    fArchveBeforeDelete : boolean;
   protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
   public
    constructor Create(ClassConfig : pLogClassConfig; const ALoggers : ILoggers);
    destructor Destroy;override;
    {ILogger}
    procedure GetConfig(AConfig: pLogConfig); stdcall;
    procedure SetConfig(AConfig: pLogConfig); stdcall;
    procedure GetClassConfig(Config : pLogClassConfig);stdcall;
    procedure Log(Level: TLogInfo; const Text: String;
      Instance: TObject=nil); overload; stdcall;
    procedure Log(Level: TLogInfo; Instance: TObject); overload; stdcall;
    procedure Log(Level: TLogInfo=lTrace); overload;stdcall;
    function LevelEnabled(ALevel : TLogInfo):boolean; stdcall;
    {ILoggerEx}
    function GetLoggers : ILoggers; stdcall;
    procedure FindOldFiles(AFiles : TStringList);stdcall;
    procedure DeleteOldFiles; stdcall;
    procedure Flush(ForceDiskWrite: boolean); stdcall;
    function FileName : string; stdcall;
    function ArchivePath : string; stdcall;
    property Logger : TSynLog read fLogger;
    property ArchveBeforeDelete : boolean read fArchveBeforeDelete;
  end;

  TLoggers = class (TObject, IInterface, ILoggers)
   private
    fUiHandle : THandle;
    FWindowHandleMessageCloseForm: HWND;
    fFormInstance : TComponent;
    fList : TObjectList;
    fLogPath : TFileName;
    fStoreInterface :ILoggingConfStoreAccess;
    procedure MessageCloseFormHandler(var message : TMessage);
    procedure FreeFormLibrary;
    function FindByGUID(const IID: TGUID; out Obj):boolean;stdcall;
    function Get(AIndex : integer): TLogger;  
    function GetItem(Index : Word; const IID: TGUID; out obj):Boolean; stdcall;
    function ShowLogFrm(AOwner : TComponent):TComponent; stdcall;
    procedure ShowModalLogFrm(AOwner : TComponent); stdcall;
    procedure OnFormDestroy(Sender: TObject);
   protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;   
    procedure SetStoreInterface(const Value: ILoggingConfStoreAccess); stdcall;
   public 
    constructor Create;
    destructor Destroy;override;
    function Count : Word; stdcall;
    property Items[AIndex : integer]: TLogger read Get; default;
  end;




function Loggers : TLoggers;


implementation

uses
System.IniFiles, FormsControllerInterface;

var v_Loggers : TLoggers = nil;

procedure CreateLoggers;
begin
  if not assigned(v_Loggers) then
    v_Loggers := TLoggers.Create;
end;

function InitLogers(const StoreInterface: ILoggingConfStoreAccess; out Obj):boolean; stdcall;
begin
  CreateLoggers;

  Loggers.SetStoreInterface(StoreInterface);
  
  if not DirectoryExists(v_Loggers.fLogPath) and
  not ForceDirectories(v_Loggers.fLogPath) then
  begin
    Result := False;
    FreeAndNil(v_Loggers);
    Exit;
  end else
    Result := Supports(v_Loggers, ILoggers, Obj);
    
  if not Result then     
    FreeAndNil(v_Loggers);
end; exports InitLogers;

function Loggers : TLoggers;
begin
  result := v_Loggers;
end;

function GetLoggers(out Obj):Boolean;stdcall;
begin
  result := Supports(v_Loggers, ILoggers, Obj)
end; exports GetLoggers;

function AddLogger(AConfig : pLogClassConfig; out Obj):boolean; stdcall;
var
vLogger : TLogger;
vStore : ILoggingConfStoreAccess;
begin  
  result := assigned(v_Loggers);
  if not result then
    Exit;
  Result := v_Loggers.FindByGUID(AConfig.ConfigGUID, Obj);
  if Result then
    Exit;
  vLogger := TLogger.Create(AConfig, v_Loggers);

  if Supports(v_Loggers, ILoggingConfStoreAccess, vStore) then
  begin
    vStore.LoadLoggerConfig(vLogger);
    vStore := nil;
  end;
  result := Supports(vLogger, ILogger, Obj);
  if result then
  begin
    v_Loggers.fList.Add(vLogger);
    vLogger.Log(lInfo, Format('Log %s started ',[AConfig.ModuleName]));
  end else
    FreeAndNil(vLogger);   
end; exports AddLogger;

procedure AchLogs(AOwner : TComponent); stdcall;
var
vUiLib : HMODULE;
vLoadProc, vUnloadProc :procedure;
vArchOldFiles : procedure (AOwner: TComponent; const ALoggers : ILoggers); stdcall;
begin
  if not assigned(v_Loggers) then
    Exit;
  vUiLib := SafeLoadLibrary(ExtractFilePath(ParamStr(0)) + 'lgui.bpl',
        SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
  if vUiLib > 0 then
  try
      @vLoadProc := GetProcAddress(vUiLib, 'Initialize');
      @vUnloadProc := GetProcAddress(vUiLib, 'Finalize');
      if Assigned(vLoadProc) then
        vLoadProc;
      try
        @vArchOldFiles := GetProcAddress(vUiLib, 'ArchOldFiles');
        if Assigned(vArchOldFiles) then
          vArchOldFiles(AOwner, v_Loggers);
      finally
        if Assigned(vUnloadProc) then
          vUnloadProc;
      end;
  finally
    FreeLibrary(vUiLib);
  end;
end; exports AchLogs;

procedure FinalizeLoggers;
var
i : integer;
begin  
  if not assigned(v_Loggers) then
    Exit;
  i := 0;
  while i < v_Loggers.Count do
  begin
    v_Loggers[i].Flush(true);
    inc(i);
  end;
  FreeAndNil(v_Loggers);
end; 

function ShowLogFrm(AOwner : TComponent):TComponent; stdcall;
begin 
  if not assigned(v_Loggers) then
    Exit;
  Result := v_Loggers.ShowLogFrm(AOwner);
end; exports ShowLogFrm;

procedure ShowModalLogFrm(AOwner : TComponent); stdcall;
begin 
  if not assigned(v_Loggers) then
    Exit;
  v_Loggers.ShowModalLogFrm(AOwner);
end; exports ShowModalLogFrm;


function TLogger.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IsEqualGUID(IID, ILoggers)
  or IsEqualGUID(IID, ILoggingConfStoreAccess) then
  begin
    Result := fLoggers.QueryInterface(IID, Obj);
    Exit;
  end;
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TLogger._AddRef: Integer;
begin
  Result := -1;
end;

function TLogger._Release: Integer;
begin
  Result := -1;
end;

function TLogger.FileName: string;
begin
  Result := Self.fLogger.FileName;
end;

procedure TLogger.FindOldFiles(AFiles : TStringList);
var SR: TSearchRec;
    OldTime: integer;
    aOldLogFileName
    : TFileName;
begin
  if not assigned(AFiles) then
    exit;
  AFiles.BeginUpdate;
  try
    AFiles.Clear;
    if FindFirst(fLogger.GenericFamily.DestinationPath+'*'+fLogger.GenericFamily.DefaultExtension,faAnyFile,SR)=0 then
    try
      if fLogger.GenericFamily.ArchiveAfterDays<0 then
        fLogger.GenericFamily.ArchiveAfterDays := 0;
      OldTime := DateTimeToFileDate(Now - fLogger.GenericFamily.ArchiveAfterDays);
      repeat
        {$ifndef DELPHI5OROLDER}
        {$WARN SYMBOL_DEPRECATED OFF} // for SR.Time
        {$endif}
        if (SR.Name[1]='.') or (faDirectory and SR.Attr<>0) or
           (SR.Time>OldTime) then
          continue;
        {$ifndef DELPHI5OROLDER}
        {$WARN SYMBOL_DEPRECATED ON}
        {$endif}
        aOldLogFileName := fLogger.GenericFamily.DestinationPath+SR.Name;
        if (aOldLogFileName = fLogger.FileName) or
        not (FileExists(aOldLogFileName) ) then
          Continue;
        AFiles.Add(aOldLogFileName);
      until FindNext(SR)<>0;
    finally
      System.SysUtils.FindClose(SR);
    end;
  finally
    AFiles.EndUpdate;
  end;
end;

procedure TLogger.Flush(ForceDiskWrite: boolean);
begin
  fLogger.Flush(ForceDiskWrite);
end;

function TLogger.ArchivePath: string;
begin
  Result := Logger.GenericFamily.ArchivePath;
end;

constructor TLogger.Create(ClassConfig: pLogClassConfig; const ALoggers : ILoggers);
begin
  fLoggers := ALoggers;
  fClassConfig.Assign(ClassConfig);
  fLogger := TSynLog.Create(TSynLogFamily.Create);
  fLogger.GenericFamily.StackTraceLevel := 0;
  fLogger.GenericFamily.DefaultExtension := '.' + fClassConfig.FileExtension;
  fLogger.GenericFamily.Level := [sllInfo, sllEvent, sllDebug,
  {$IFDEF DEBUG}sllEnter, sllLeave, {$ENDIF} sllWarning, sllError, sllException, sllSQL];
  fLogger.GenericFamily.DestinationPath := v_Loggers.fLogPath + '\';
  fLogger.GenericFamily.ArchivePath := fLogger.GenericFamily.DestinationPath + 'Archive';
end;  

destructor TLogger.Destroy;
begin
  FreeAndNil(fLogger);
  fLoggers := nil;
  inherited Destroy;
end;

procedure TLogger.DeleteOldFiles;
var Files : TStringList;
i : integer;
begin
  Files := TStringList.Create;
  try
    FindOldFiles(Files);
    i := 0;
    while (i < Files.Count) do
    begin
      DeleteFile(PWideChar(Files[i]));
      inc(i);
    end;
  finally
    FreeAndNil(Files);
  end;
end;

procedure TLogger.GetClassConfig(Config: pLogClassConfig);
begin
  Config.Assign(@fClassConfig);
end;

procedure TLogger.GetConfig(AConfig: pLogConfig);
begin
  AConfig.Levels := TLogInfos(fLogger.GenericFamily.Level);
  AConfig.ArchiveAfterDays := fLogger.GenericFamily.ArchiveAfterDays;
  AConfig.ArhiveBeforeDelete := fArchveBeforeDelete;
  AConfig.MaxMBSize := fLogger.GenericFamily.MaxMBSize;
  AConfig.AutoFlushTimeOut := fLogger.GenericFamily.AutoFlushTimeOut;
end;

function TLogger.GetLoggers: ILoggers;
begin
  Result := fLoggers;
end;

procedure TLogger.Log(Level: TLogInfo; const Text: String;
  Instance: TObject);
begin
  fLogger.Log(TSynLogInfo(Level), Text, Instance);
end;

function TLogger.LevelEnabled(ALevel: TLogInfo): boolean;
begin
  Result := (TSynLogInfo(ALevel) in fLogger.GenericFamily.Level);
end;

procedure TLogger.Log(Level: TLogInfo);
begin
  fLogger.Log(TSynLogInfo(Level));
end;

procedure TLogger.Log(Level: TLogInfo; Instance: TObject);
begin
  fLogger.Log(TSynLogInfo(Level), Instance);
end;

procedure TLogger.SetConfig(AConfig: pLogConfig);
var
i : TLogInfo;
vLevelsText : String;
begin
  fLogger.GenericFamily.Level := TSynLogInfos(AConfig.Levels);
  fLogger.GenericFamily.ArchiveAfterDays := AConfig.ArchiveAfterDays;
  fArchveBeforeDelete := AConfig.ArhiveBeforeDelete;
  fLogger.GenericFamily.AutoFlushTimeOut := AConfig.AutoFlushTimeOut;
  fLogger.GenericFamily.MaxMBSize := AConfig.MaxMBSize;
  for I := low(TLogInfo) to high(TLogInfo) do
  begin
    if (i in AConfig.Levels) then
    begin
      if (vLevelsText <> '') then
        vLevelsText := vLevelsText +', '+ C_EventCaption[i]
      else
        vLevelsText := C_EventCaption[i];
    end;
  end;
  fLogger.Log(sllInfo, Format('Enabled levels: %s',[vLevelsText]));
  fLogger.Flush(true);
end;


{ TLoggers }

function TLoggers.Count: Word;
begin
  Result := fList.Count;
end;

constructor TLoggers.Create;
var f : TIniFile;
vLogConfigFile : string;
begin
  inherited Create;
  fUiHandle := 0;
  fFormInstance := nil;
  FWindowHandleMessageCloseForm := System.Classes.AllocateHWND(MessageCloseFormHandler);
  fStoreInterface := nil;
  fList := TObjectList.Create;
  vLogConfigFile := ExtractFilePath(ParamStr(0))+'Logger.ini';
  fLogPath := ExtractFilePath(ParamStr(0)) + 'Log';
  if FileExists(vLogConfigFile) then
  begin
    f := TIniFile.Create(vLogConfigFile);
    try
      if f.ValueExists('Log', 'LogPath') then
      begin
        fLogPath :=
        f.ReadString('Log', 'LogPath', ExtractFilePath(ParamStr(0)) + 'Log');
      end;
    finally
      FreeAndNil(f);
    end;
  end;
end;

destructor TLoggers.Destroy;
var
vFormOnDestroyEvent : IFormOnDestroyEvent;
begin
  if Supports(fFormInstance, IFormOnDestroyEvent, vFormOnDestroyEvent) then
  begin
    vFormOnDestroyEvent.OnDestroy := nil;
    vFormOnDestroyEvent := nil;
  end;
  fFormInstance := nil;
  FreeFormLibrary;
  System.Classes.DeallocateHWND(FWindowHandleMessageCloseForm);
  fStoreInterface := nil;
  fList.Clear;
  FreeAndNil(fList);
  inherited Destroy;
end;

function TLoggers.FindByGUID(const IID: TGUID; out Obj): boolean;
var
i : Word;
vConfig : pLogClassConfig;
begin
  Result := False;
  New(vConfig);
  try
    i := 0;
    while i < fList.Count do
    begin
      Items[i].GetClassConfig(vConfig);
      Result := IsEqualGUID(IID, vConfig.ConfigGUID)
      and GetItem(i, ILogger, Obj);
      if Result then
        Break;
      Inc(i);
    end;
  finally
    Dispose(vConfig);
  end;
end;

function TLoggers.Get(AIndex: integer): TLogger;
begin
  result := TLogger(fList.Items[AIndex]);
end;

function TLoggers.GetItem(Index: Word; const IID: TGUID; out obj): Boolean;
begin
  Result := (Index < fList.Count) and Supports(Self.Items[Index], IID, obj);
end;

function TLoggers.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := E_NOINTERFACE;
  if GetInterface(IID, Obj) then
    Result := S_OK
  else if IsEqualGUID(IID, ILoggingConfStoreAccess)
  and supports(fStoreInterface, ILoggingConfStoreAccess, Obj) then
    Result := S_OK;
end;

procedure TLoggers.SetStoreInterface(const Value: ILoggingConfStoreAccess);
begin
  if Assigned(fStoreInterface) then
  begin
    fStoreInterface.SaveLoggersConfig(self);
    fStoreInterface := nil;
  end;
  if Assigned(Value) then
  begin
    fStoreInterface := Value;
    fStoreInterface.LoadLoggersConfig(self);
  end;
end;

procedure TLoggers.MessageCloseFormHandler(var message: TMessage);
begin
  if message.Msg = WM_USER+1 then
    FreeFormLibrary;
end;

procedure TLoggers.FreeFormLibrary;
var
vUnloadProc :procedure;
begin
  if (fUiHandle > 0) then
  try
    @vUnloadProc := GetProcAddress(fUiHandle, 'Finalize');
    if Assigned(vUnloadProc) then
      vUnloadProc;
  finally
    FreeLibrary(fUiHandle);
    fUiHandle := 0;
  end;
end;

procedure TLoggers.OnFormDestroy(Sender: TObject);
begin
  fFormInstance := nil;
  Winapi.Windows.PostMessage(FWindowHandleMessageCloseForm, WM_USER+1, 0, 0);
end;

function TLoggers.ShowLogFrm(AOwner : TComponent):TComponent;
var
vCreateLogViewFormInstance : function (AOwner: TComponent; const ALoggers : ILoggers):TComponent;stdcall;
vInitLibProc, vUnloadProc :procedure;
vShowMdiForm : IShowMdiForm;
vFormToFront : IFormToFront;
vFormOnDestroyEvent : IFormOnDestroyEvent;
begin
  if Assigned(fFormInstance)
  and Supports(fFormInstance, IFormToFront, vFormToFront) then
  begin
    vFormToFront.BringToFront;
    Result := fFormInstance;
    Exit;
  end;
  fUiHandle := SafeLoadLibrary(ExtractFilePath(ParamStr(0)) + 'lgui.bpl',
        SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
  try
    if (fUiHandle = 0) then
      Exit;
    @vCreateLogViewFormInstance := GetProcAddress(fUiHandle, 'CreateLogViewFormInstance');
    if not Assigned(vCreateLogViewFormInstance) then
    begin
      FreeLibrary(fUiHandle);
      fUiHandle := 0;
      Exit;
    end;
    @vInitLibProc := GetProcAddress(fUiHandle, 'Initialize');
    if Assigned(vInitLibProc) then
       vInitLibProc;
    fFormInstance := vCreateLogViewFormInstance(AOwner, self);
    if Supports(fFormInstance, IFormOnDestroyEvent, vFormOnDestroyEvent) then
       vFormOnDestroyEvent.OnDestroy := OnFormDestroy
    else
      raise Exception.Create('Log form not supports destroy event interface');
    if Supports(fFormInstance, IShowMdiForm, vShowMdiForm) then
    begin
      vShowMdiForm.Show;
      Result := fFormInstance;
    end
    else
      raise Exception.Create('Log form not supports show interface');
  except
    if (fUiHandle > 0) then
    try
      if Assigned(fFormInstance) then
        FreeAndNil(fFormInstance);
      @vUnloadProc := GetProcAddress(fUiHandle, 'Finalize');
      if Assigned(vUnloadProc) then
          vUnloadProc;
    finally
      FreeLibrary(fUiHandle);
      fUiHandle := 0;
      fFormInstance := nil;
    end;
  end;
end;

procedure TLoggers.ShowModalLogFrm(AOwner : TComponent);
var
vUiLib : HMODULE;
vFormToFront : IFormToFront;
vLoadProc, vUnloadProc :procedure;
vLogViewModal : procedure (AOwner: TComponent; const ALoggers : ILoggers); stdcall;
begin
  if not assigned(v_Loggers) then
    Exit;
  if Assigned(fFormInstance)
  and Supports(fFormInstance, IFormToFront, vFormToFront) then
  begin
    vFormToFront.BringToFront;
    Exit;
  end;
  vUiLib := SafeLoadLibrary(ExtractFilePath(ParamStr(0)) + 'lgui.bpl',
        SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
  if vUiLib > 0 then
  try
      @vLoadProc := GetProcAddress(vUiLib, 'Initialize');
      @vUnloadProc := GetProcAddress(vUiLib, 'Finalize');
      if Assigned(vLoadProc) then
        vLoadProc;
      try
        @vLogViewModal := GetProcAddress(vUiLib, 'LogViewModal');
        if Assigned(vLogViewModal) then
          vLogViewModal(AOwner, self);
      finally
        if Assigned(vUnloadProc) then
          vUnloadProc;
      end;
  finally
    FreeLibrary(vUiLib);
  end;
end;

function TLoggers._AddRef: Integer;
begin
  Result := -1;
end;

function TLoggers._Release: Integer;
begin
  Result := -1;
end;

initialization
  {CreateLoggers;  }

finalization
  FinalizeLoggers;
end.
