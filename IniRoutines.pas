unit IniRoutines;

interface
uses LoggerInterface
  {$IFDEF  VER320+}
  ,system.IniFiles
  {$ELSE}
  , IniFiles
  {$ENDIF}
  ;

procedure LoadLoggerConfig(ASource : TCustomIniFile; const ALogger : ILogger);
procedure LoadLoggersConfig(ASource : TCustomIniFile; const ALoggers: ILoggers);
procedure SaveLoggerConfig(ADest : TCustomIniFile; const ALogger : ILogger);
procedure SaveLoggersConfig(ADest : TCustomIniFile; const ALoggers: ILoggers);

implementation
uses
{$IFDEF  VER320+}
System.SysUtils;
{$ELSE}
SysUtils;
{$ENDIF}



procedure LoadLoggerConfig(ASource : TCustomIniFile; const ALogger : ILogger);
var
vClassConfig : pLogClassConfig;
vLogConfig : pLogConfig;
E: TLogInfo;
vGuidStr : string;
begin  
  New(vLogConfig);
  New(vClassConfig);
  try 
    ALogger.GetClassConfig(vClassConfig);
    vGuidStr := GUIDToString(vClassConfig.ConfigGUID);
    if ASource.SectionExists(vGuidStr) then
    begin
      for E := succ(lNone) to high(E) do
      begin
        if ASource.ReadBool(vGuidStr, C_EventCaption[E], false) then
          Include(vLogConfig.Levels, E)
        else
          Exclude(vLogConfig.Levels, E);
      end;
      vLogConfig.ArhiveBeforeDelete := ASource.ReadBool(vGuidStr, 'ArhiveBeforeDelete', false);
      vLogConfig.ArchiveAfterDays := ASource.ReadInteger(vGuidStr, 'ArchiveAfterDays', 7);
      vLogConfig.MaxMBSize := ASource.ReadInteger(vGuidStr, 'MaxMBSize', 250);
      vLogConfig.AutoFlushTimeOut := ASource.ReadInteger(vGuidStr, 'AutoFlushTimeOut', 5);
      ALogger.SetConfig(vLogConfig);
    end;
  finally
    Dispose(vClassConfig);
    Dispose(vLogConfig);
  end;
end;

procedure LoadLoggersConfig(ASource : TCustomIniFile; const ALoggers: ILoggers);
var
vLogger : ILogger;
i : Word;
begin
  i := 0;
  while i < ALoggers.Count do
  begin
    vLogger := nil;
    if ALoggers.Item[i, ILogger, vLogger] then
      LoadLoggerConfig(ASource, vLogger);
    Inc(i);
  end;
end;

procedure SaveLoggerConfig(ADest : TCustomIniFile; const ALogger : ILogger);
var
vLogConfig : pLogConfig;
vClassConfig : pLogClassConfig;
E: TLogInfo;
vGuidStr : string;
begin
  New(vLogConfig);
  New(vClassConfig);
  try
    ALogger.GetConfig(vLogConfig);
    ALogger.GetClassConfig(vClassConfig);
    vGuidStr := GUIDToString(vClassConfig.ConfigGUID);
    ADest.WriteString(vGuidStr, 'Description', vClassConfig.ModuleName);
    for E := succ(lNone) to high(E) do
      ADest.WriteBool(vGuidStr, C_EventCaption[E], (e in vLogConfig.Levels));

    ADest.WriteBool(vGuidStr, 'ArhiveBeforeDelete', vLogConfig.ArhiveBeforeDelete);
    ADest.WriteInteger(vGuidStr, 'ArchiveAfterDays', vLogConfig.ArchiveAfterDays);
    ADest.WriteInteger(vGuidStr, 'MaxMBSize', vLogConfig.MaxMBSize);
    ADest.WriteInteger(vGuidStr, 'AutoFlushTimeOut', vLogConfig.AutoFlushTimeOut);
  finally
    Dispose(vClassConfig);
    Dispose(vLogConfig);
  end;
end;    

procedure SaveLoggersConfig(ADest : TCustomIniFile; const ALoggers: ILoggers);
var
vLogger : ILogger;
i : Word;
begin
  i := 0;
  while i < ALoggers.Count do
  begin
    vLogger := nil;
    if ALoggers.Item[i, ILogger, vLogger] then
      SaveLoggerConfig(ADest, vLogger);
    Inc(i);
  end;
end;

end.
