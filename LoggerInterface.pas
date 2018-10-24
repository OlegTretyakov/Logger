unit LoggerInterface;

 interface
  uses
 {$IFDEF  VER320+}
  System.Classes;
  {$ELSE}
  Classes;
  {$ENDIF}

  const
  MsgLen = 100;
  type
  TLogInfo = (
    lNone, lInfo, lEvent, lDebug, lPortWrite, lPortRead, lTrace, lWarning, lError,
    lEnter, lLeave,
    lLastError, lException, lExceptionOS, lMemory, lStackTrace,
    lFail, lSQL, lCache, lResult, lDB, lHTTP, lClient, lServer,
    lServiceCall, lServiceReturn, lUserAuth, lCustom2);

  TLogInfos = set of TLogInfo;

  pLogConfig = ^TLogConfig;
  TLogConfig = record
    Levels: TLogInfos;
    ArhiveBeforeDelete : boolean;
    ArchiveAfterDays,
    MaxMBSize,
    AutoFlushTimeOut : Word;
    procedure Assign(ASource : pLogConfig);
  end;
  
  pLogClassConfig = ^TLogClassConfig;
  TLogClassConfig = record
   ConfigGUID : TGUID;
   EnabledLevels,
   DefaultLevelsView : TLogInfos;
   FileExtension,
   ArhiveExtension,
   ModuleName : String;
   procedure Assign(ASource : pLogClassConfig);
  end;

  ILogger = interface(IInterface)
    ['{1A2589E0-0BCA-45BC-BF5E-6B90EE658438}']
    procedure GetConfig(AConfig: pLogConfig); stdcall;
    procedure SetConfig(AConfig: pLogConfig); stdcall;
    procedure GetClassConfig(Config : pLogClassConfig);stdcall;
    /// call this method to add some information to the log at a specified level
    // - if Instance is set and Text is not '', it will log the corresponding
    // class name and address (to be used e.g. if you didn't call TSynLog.Enter()
    // method first)
    // - if Instance is set and Text is '', will behave the same as
    // Log(Level,Instance), i.e. write the Instance as JSON content
    procedure Log(Level: TLogInfo; const Text: String;
      Instance: TObject=nil); overload; stdcall;
    /// call this method to add the content of an object to the log at a
    // specified level
    // - TSynLog will write the class and hexa address - TSQLLog will write the
    // object JSON content
    procedure Log(Level: TLogInfo; Instance: TObject); overload; stdcall;
    /// call this method to add the caller address to the log at the specified level
    // - if the debugging info is available from TSynMapFile, will log the
    // unit name, associated symbol and source code line
    procedure Log(Level: TLogInfo=lTrace); overload; stdcall;
    function LevelEnabled(ALevel : TLogInfo):boolean; stdcall;
    procedure Flush(ForceDiskWrite: boolean); stdcall;
  end;


  ILoggers = interface;
  ILoggingConfStoreAccess = interface (IInterface)
  ['{6303826E-FDFC-4792-8D57-55E335AF844D}']
    procedure LoadLoggerConfig(const ALogger : ILogger); stdcall;
    procedure SaveLoggerConfig(const ALogger : ILogger); stdcall;
    procedure LoadLoggersConfig(const ALoggers : ILoggers); stdcall;
    procedure SaveLoggersConfig(const ALoggers : ILoggers); stdcall;
  end;

  ILoggers = interface(IInterface)
    ['{2AB87410-E63B-4E6B-9264-66BE61149CB4}']
    function Count : Word; stdcall;
    function GetItem(Index : Word; const IID: TGUID; out obj):Boolean; stdcall;
    function FindByGUID(const IID: TGUID; out Obj):boolean;stdcall;
    procedure SetStoreInterface(const Value: ILoggingConfStoreAccess); stdcall;
    property Item[Index : Word; const IID: TGUID; out obj]:Boolean read GetItem;
    function ShowLogFrm(AOwner : TComponent):TComponent; stdcall;
    procedure ShowModalLogFrm(AOwner : TComponent); stdcall;
  end;

  TLoggerFunct = record
     strict private
      fLibHandle : THandle;
      fLoggers : ILoggers;
      fInitLogers : function (const StoreInterface :ILoggingConfStoreAccess; out Obj):boolean; stdcall;
      fAddLogger : function (AConfig : pLogClassConfig; out Obj):boolean; stdcall;
      fAchLogs : procedure(AOwner : TComponent); stdcall;
      fShowLogFrm : function(AOwner : TComponent):TComponent; stdcall;
      fShowModalLogFrm : procedure(AOwner : TComponent); stdcall;
      function GetItem(Index : Word; out obj):Boolean; stdcall;
     public
      function Count : Word;
      function FindByGUID(const IID: TGUID; out Obj):boolean;
      property Item[Index : Word; out obj]:Boolean read GetItem; default;
      function AddLogger(AConfig : pLogClassConfig; out Obj):boolean;
      procedure AchLogs(AOwner : TComponent);
      function ShowLogFrm(AOwner : TComponent):TComponent;
      procedure ShowModalLogFrm(AOwner : TComponent);
      function Init(const StoreInterface :ILoggingConfStoreAccess=nil) : boolean;
      procedure SetStoreInterface(const Value: ILoggingConfStoreAccess);
      procedure FlushAll;
      procedure Done;
  end;
  pLoggerFunct = ^TLoggerFunct;
  const
  C_EventCaption: array[TLogInfo] of String = (
    'None', 'Info', 'Event', 'Debug', 'Port Write', 'Port Read', 'Trace', 'Warning', 'Error',
    'Enter', 'Leave',
    'LastError', 'Exception', 'ExceptionOS', 'Memory', 'StackTrace', 'Fail',
    'SQL', 'Cache', 'Result', 'DB', 'http', 'Client', 'Server',
    'ServiceCall', 'ServiceReturn', 'UserAuth', 'Custom2');



implementation
 uses
{$IFDEF  VER320+}
System.SysUtils, Winapi.Windows;
{$ELSE}
SysUtils, Windows;
{$ENDIF}

type
  TPackageLoad = procedure;
  TPackageUnload = procedure;
{ TLogConfig }

procedure TLogConfig.Assign(ASource: pLogConfig);
begin
  Self.Levels :=  ASource^.Levels;
  Self.ArhiveBeforeDelete :=  ASource^.ArhiveBeforeDelete;
  Self.ArchiveAfterDays :=  ASource^.ArchiveAfterDays;
  Self.MaxMBSize :=  ASource^.MaxMBSize;
  Self.AutoFlushTimeOut :=  ASource^.AutoFlushTimeOut;
end;

{ TLogClassConfig }

procedure TLogClassConfig.Assign(ASource: pLogClassConfig);
begin
  Self.ConfigGUID := ASource^.ConfigGUID;
  Self.EnabledLevels := ASource^.EnabledLevels;
  Self.DefaultLevelsView := ASource^.DefaultLevelsView;
  Self.FileExtension := ASource^.FileExtension;
  Self.ArhiveExtension := ASource^.ArhiveExtension;
  Self.ModuleName := ASource^.ModuleName;  
end;

function TLoggerFunct.Init(const StoreInterface :ILoggingConfStoreAccess=nil) : boolean;
var GetLoggers : function (out Obj):Boolean;stdcall;
vInitProc: TPackageLoad;
begin
  Result := False;

  if not FileExists('Logger.bpl') then
    Exit;

  fLibHandle := GetModuleHandle('Logger.bpl');
  if fLibHandle <> 0 then
  begin
    @GetLoggers := GetProcAddress(fLibHandle, 'GetLoggers');
    if Assigned(GetLoggers) and GetLoggers(fLoggers) then
    begin
      @fInitLogers := GetProcAddress(fLibHandle, 'InitLogers');
      @fAddLogger := GetProcAddress(fLibHandle, 'AddLogger');
      @fAchLogs := GetProcAddress(fLibHandle, 'AchLogs');
      @fShowLogFrm := GetProcAddress(fLibHandle, 'ShowLogFrm');
      @fShowModalLogFrm := GetProcAddress(fLibHandle, 'ShowModalLogFrm');
      Result := True;
      Exit;
    end else
    begin
      FreeLibrary(fLibHandle);
      fLibHandle := 0;
    end;
  end;
  if fLibHandle <> 0 then
    Exit;
  fLibHandle := SafeLoadLibrary('Logger.bpl',
          SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
  @vInitProc := GetProcAddress(fLibHandle, 'Initialize');
  @fInitLogers := GetProcAddress(fLibHandle, 'InitLogers');
  @fAddLogger := GetProcAddress(fLibHandle, 'AddLogger');
  @fAchLogs := GetProcAddress(fLibHandle, 'AchLogs');
  @fShowLogFrm := GetProcAddress(fLibHandle, 'ShowLogFrm');
  @fShowModalLogFrm := GetProcAddress(fLibHandle, 'ShowModalLogFrm');
  if Assigned(vInitProc)
  and Assigned(fInitLogers)
  and Assigned(fAddLogger)
  and Assigned(fAchLogs)
  and Assigned(fShowLogFrm)
  and Assigned(fShowModalLogFrm) then
  begin
    vInitProc;
    Result := fInitLogers(StoreInterface, fLoggers);
  end;

  if (not result) and (fLibHandle <> 0) then
  begin
    UnloadPackage(fLibHandle);
    fLibHandle := 0;
  end;
end; 

function TLoggerFunct.FindByGUID(const IID: TGUID; out Obj): boolean;
begin
  Result := Assigned(fLoggers) and fLoggers.FindByGUID(IID, Obj);
end;

procedure TLoggerFunct.FlushAll;
var
i : word;
vLogger : ILogger;
begin
  if Assigned(fLoggers) then
  begin
    i := 0;
    while i < fLoggers.Count do
    begin
      if fLoggers.Item[i, ILogger, vLogger] then
        vLogger.Flush(true);
      vLogger := nil;
      Inc(i);
    end;
  end;
end;

function TLoggerFunct.GetItem(Index: Word; out obj): Boolean;
begin
  Result := Assigned(fLoggers) and fLoggers.GetItem(Index, ILogger, Obj);
end;

procedure TLoggerFunct.AchLogs(AOwner: TComponent);
begin
  if Assigned(fAchLogs) then
     fAchLogs(AOwner);
end;

function TLoggerFunct.AddLogger(AConfig: pLogClassConfig; out Obj): boolean;
begin
  Result := Assigned(fAddLogger) and fAddLogger(AConfig, Obj);
end;

function TLoggerFunct.Count: Word;
begin
  if Assigned(fLoggers) then
    Result := fLoggers.Count
  else
    Result := 0;
end;

procedure TLoggerFunct.SetStoreInterface(const Value: ILoggingConfStoreAccess);
begin
  if Assigned(fLoggers) then
    fLoggers.SetStoreInterface(Value);
end;

function TLoggerFunct.ShowLogFrm(AOwner: TComponent):TComponent;
begin
  if Assigned(fShowLogFrm) then
    result := fShowLogFrm(AOwner)
  else
    Result := nil;
end;

procedure TLoggerFunct.ShowModalLogFrm(AOwner: TComponent);
begin
  if Assigned(fShowModalLogFrm) then
   fShowModalLogFrm(AOwner);
end;

procedure TLoggerFunct.Done;    
var
vFinalizeProc: TPackageUnload;
begin 
  fLoggers := nil;
  if (fLibHandle <> 0) then
  try
    @vFinalizeProc := GetProcAddress(fLibHandle, 'Finalize'); //Do not localize
    if Assigned(vFinalizeProc) then
      vFinalizeProc;
    FreeLibrary(fLibHandle);
  finally
    fLibHandle := 0;
  end;
end;



end.
