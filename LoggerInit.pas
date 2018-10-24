unit LoggerInit;


interface

uses LoggerInterface;

var
vLoggerFunct : pLoggerFunct=nil;
procedure Init;
procedure Done;

implementation

procedure Init;
begin
  New(vLoggerFunct);
  try
    if not vLoggerFunct.Init then
    begin
      Dispose(vLoggerFunct);
      vLoggerFunct := nil;
    end;
  except
    Dispose(vLoggerFunct); 
    vLoggerFunct := nil;
  end;
end;

procedure Done;
begin
  try
    if Assigned(vLoggerFunct) then
    try
      vLoggerFunct.Done;
    finally
      Dispose(vLoggerFunct); 
      vLoggerFunct := nil;
    end;
  except
  end;
end;


end.
