использование:

1. В секцию uses drp проекта exe-пускача добавить LoggerInit.pas

2. Реализуем подключение и инициализацию логгера в drp пускача
begin
  Application.Initialize;
  LoggerInit.Init; //грузим bpl логгера
  try
    ........
    Application.CreateForm......;
    .......
    Application.Run;
  finally
    LoggerInit.Done; //выгружаем bpl логгера
  end;
end.

3. Создаем инстанцию для модуля, события которого будем логировать.
Их может быть несколько, нужно чтобы они различались по GUID, расщирениям и именам.
unit MyProcUnit;
uses ... 

LoggerInterface, 
LoggerInit,
....

function InitLogger(AFunct : pLoggerFunct; out Obj):boolean;
var
vConfig : pLogClassConfig;
begin
  Result := False;
  if not Assigned(AFunct) then
    Exit;
  New(vConfig);
  try
    vConfig.ConfigGUID := StringToGUID('{8C634824-80BE-4DCB-AF68-4B0B05B29DF8}');
    vConfig.EnabledLevels := [lInfo, lEvent, lDebug, lTrace, lWarning, lError,
                             lException, lExceptionOS];
    vConfig.DefaultLevelsView := [lInfo, lEvent, lWarning, lError];
    vConfig.FileExtension := 'mylog';
    vConfig.ArhiveExtension := 'mylog7z';
    vConfig.ModuleName := 'Процесс чего-то там...';
    try
      Result := AFunct^.AddLogger(vConfig, Obj);
    except
      Result := false;
    end;
  finally
    Dispose(vConfig);
  end;
end;

var MyProcLogger : ILogger=nil;
comstructor TMyProcess.Create;
begin
  ...
  InitLogger(LoggerInit.vLoggerFunct, MyProcLogger);
  
end;

destructor TChamberProcess.Destroy;
begin
  .,...
  inherited Destroy;
  MyProcLogger := nil;
end;

4. Логируем.
procedure TMyProcess.CallOnSomeEvent;
begin
  ....
  MyProcLogger.Log(lEvent, 'SomeEvent свершилось!!!');
  ....
end;

5. Отображаем журнал событий 
unit uMainForm;


....
uses ... 
LoggerInit,
....


procedure TMainForm.OnLogShowAction(Sender : TObject);
begin
  LoggerInit.vLoggerFunct.ShowLogFrm(self);
  {MDI окно. В этот момент будет подгружена lgui.bpl
  По закрытию формы lgui.bpl будет выгружена}
end;

6. Архивируем логи.
procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ....
  LoggerInit.vLoggerFunct.AchLogs(self);
  {Модальное окно. В этот момент будет подгружена lgui.bpl
  и запускается поток упаковки. Упаковка выполняется посредством 7z.dll
  По завершению архивации lgui.bpl будет выгружена}
  
  ....
end;

7. Реализация хранилища конфигов.

unit uDataModule;

uses 
.....
LoggerInterface,
LoggerInit,
...

TMyDataModule = class(...., ILoggingConfStoreAccess)

 private
 .....
   {ILoggingConfStoreAccess}
   procedure LoadLoggerConfig(const ALogger : ILogger); stdcall;
   procedure SaveLoggerConfig(const ALogger : ILogger); stdcall;
   procedure LoadLoggersConfig(const ALoggers : ILoggers); stdcall;
   procedure SaveLoggersConfig(const ALoggers : ILoggers); stdcall;
  ...
  
end;


implementation


procedure TMyDataModule.LoadLoggerConfig(const ALogger : ILogger);
var
vQR : TvdQuery;
vStr : TStringList;
vIni : TMemIniFile;
begin
  try
    vStr := TStringList.Create;
    vIni := TMemIniFile.Create('');
    vQR := CreateReadQuery(nil, 'SELECT FILE FROM CONFIG WHERE TYPE = :TYPE');
    try
      vQR.ParamByName('TYPE').AsString := 'LogConfig';
      vQR.Open;
      vStr.Text := vQR.FieldByName('FILE').AsWideString;
      vQR.Close;

      if vStr.Count > 0 then
      begin
        vIni.SetStrings(vStr);
        IniRoutines.LoadLoggerConfig(vIni, ALogger);
      end;
    finally
      FreeAndNil(vStr);
      FreeAndNil(vIni);
      FreeAndNil(vQR);
    end;
  except
    on e : Exception do
      Log(lException, E);
  end;
end;

procedure TMyDataModule.LoadLoggersConfig(const ALoggers: ILoggers);
var
vQR : TvdQuery;
vStr : TStringList;
vIni : TMemIniFile;
begin
  try
    vStr := TStringList.Create;
    vIni := TMemIniFile.Create('');
    vQR := CreateReadQuery(nil, 'SELECT FILE FROM CONFIG WHERE TYPE = :TYPE');
    try
      vQR.ParamByName('TYPE').AsString := 'LogConfig';
      vQR.Open;
      vStr.Text := vQR.FieldByName('FILE').AsWideString;
      vQR.Close;
      if vStr.Count > 0 then
      begin
        vIni.SetStrings(vStr);
        IniRoutines.LoadLoggersConfig(vIni, ALoggers);
      end;
    finally
      FreeAndNil(vStr);
      FreeAndNil(vIni);
      FreeAndNil(vQR);
    end;
  except
    on e : Exception do
      Log(lException, E);
  end;
end;

procedure TMyDataModule.SaveLoggerConfig(const ALogger : ILogger);
var
vQR : TvdQuery;
vStr : TStringList;
vIni : TMemIniFile;
vUpdate : boolean;
begin
  try
    vUpdate := false;
    vStr := TStringList.Create;
    vIni := TMemIniFile.Create('');
    try
      vQR := CreateReadQuery(nil, 'SELECT FILE FROM CONFIG WHERE TYPE = :TYPE');
      try
        vQR.ParamByName('TYPE').AsString := 'LogConfig';
        vQR.Open;
        vStr.Text := vQR.FieldByName('FILE').AsWideString;
        if vStr.Count > 0 then
        begin
          vIni.SetStrings(vStr);
          vUpdate := True;
        end;
        vQR.Close;
      finally
        FreeAndNil(vQR);
      end;


      IniRoutines.SaveLoggerConfig(vIni, ALogger);
      vStr.Clear;
      vIni.GetStrings(vStr);

      if vUpdate then
        vQR := CreateWriteQuery(nil, 'UPDATE CONFIG SET FILE = :FILE WHERE TYPE = :TYPE')
      else
        vQR := CreateWriteQuery(nil, 'INSERT INTO CONFIG (TYPE, FILE) VALUES (:TYPE, :FILE)');
      try
        vQR.ParamByName('TYPE').AsString := 'LogConfig';
        vQR.ParamByName('FILE').AsWideMemo := vStr.Text;
        vQR.ExecSQL;
        Commit(vQR);
      finally
        FreeAndNil(vQR);
      end;
    finally
      FreeAndNil(vStr);
      FreeAndNil(vIni);
    end;
  except
    on e : Exception do
      Log(lException, E);
  end;
end; 

procedure TMyDataModule.SaveLoggersConfig(const ALoggers: ILoggers);
var
vQR : TvdQuery;
vStr : TStringList;
vIni : TMemIniFile;
vUpdate : boolean;
begin
  try
    vUpdate := false;
    vStr := TStringList.Create;
    vIni := TMemIniFile.Create('');
    try
      vQR := CreateReadQuery(nil, 'SELECT FILE FROM CONFIG WHERE TYPE = :TYPE');
      try
        vQR.ParamByName('TYPE').AsString := 'LogConfig';
        vQR.Open;
        vStr.Text := vQR.FieldByName('FILE').AsWideString;
        if vStr.Count > 0 then
        begin
          vIni.SetStrings(vStr);
          vUpdate := True;
        end;
        vQR.Close;
      finally
        FreeAndNil(vQR);
      end;

      IniRoutines.SaveLoggersConfig(vIni, ALoggers);

      vStr.Clear;
      vIni.GetStrings(vStr);


      if vUpdate then
        vQR := CreateWriteQuery(nil, 'UPDATE CONFIG SET FILE = :FILE WHERE TYPE = :TYPE')
      else
        vQR := CreateWriteQuery(nil, 'INSERT INTO CONFIG (TYPE, FILE) VALUES (:TYPE, :FILE)');

      try
        vQR.ParamByName('TYPE').AsString := 'LogConfig';
        vQR.ParamByName('FILE').AsWideMemo := vStr.Text;
        vQR.ExecSQL;
        Commit(vQR);
      finally
        FreeAndNil(vQR);
      end;
    finally
      FreeAndNil(vStr);
      FreeAndNil(vIni);
    end;
  except
    on e : Exception do
      Log(lException, E);
  end;
end;

function TMyDataModule.Connect:boolean;
begin
  {connect creation routine}
  ....
  result := ......
  .....
  if result then
    LoggerInit.vLoggerFunct.SetStoreInterface(self);  
end;