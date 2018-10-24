unit LoggerEx;

interface
  uses System.Classes, LoggerInterface;
  type
  ILoggerEx = interface(ILogger)
    ['{8AAC303A-CCC9-4FD3-AE87-D098F18C0719}']
    procedure FindOldFiles(AFiles : TStringList);stdcall;
    procedure DeleteOldFiles; stdcall;
    function FileName : string; stdcall;
    function ArchivePath : string; stdcall;
  end;
implementation

end.
