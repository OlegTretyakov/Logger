unit ArchForm;


interface

uses Vcl.StdCtrls, Vcl.Buttons, System.Classes, Vcl.Controls, Vcl.ComCtrls,
  Winapi.Messages, Vcl.Forms, System.SysUtils;

  

  const
  START_PROCESS = WM_USER+ 451;
  SET_TOTAL = START_PROCESS + 1;
  WND_PRG_UPDATE = SET_TOTAL + 1;
  WND_CLOSE = WND_PRG_UPDATE + 1;
  
type
  TFrmMode = (mCompress, mUnCompress);
  TArchFrm = class(TForm)
    ProgressBar1: TProgressBar;
    CloseBth: TBitBtn;
    procedure FormShow(Sender: TObject);
  private
    fInFile, fOutFile, fItemName : TFileName;
    fMemory : TMemoryStream;
    fMode : TFrmMode;
    procedure Start(var Message : TMessage); message START_PROCESS;
    procedure OnSetTotal(var Message : TMessage); message SET_TOTAL;
    procedure UpdateProgress(var Message : TMessage); message WND_PRG_UPDATE;
    procedure OnCloseMessage(var Message : TMessage); message WND_CLOSE;
  public
    constructor Create(AOwner :TComponent; AMode : TFrmMode;
    AMemory : TMemoryStream;  const AInFile, AOutFile : TFileName); reintroduce;
    property ItemName : TFileName read fItemName;
  end;

implementation

uses
//sevenzip_16_04,
sevenzip2,
SelectUnArchItem,
Winapi.Windows;

{$R *.dfm}

const
cWaitTimeOut = 1000;//mSec

type
  TCallBackParams = record
    FormHandle,
    CallBackEvent : THandle;
    Result : HRESULT;
  end;
  pCallBackParams = ^TCallBackParams;
  TUnArchThread = class (TThread)
   private
     fCallBackParams : TCallBackParams;
     fInFile : TFileName;
     fMemory : TMemoryStream;
     fCallback : T7zProgressCallback;
     fItem: integer;
   protected
    procedure Execute; override;
   public
    constructor Create(AFormHandle : THandle; const AInFile : TFileName; const AItem: integer;
                AMemory : TMemoryStream; ACallback : T7zProgressCallback);
  end;

  TSingleArchThread = class (TThread)
   private 
     fCallBackParams : TCallBackParams;
     fInFile, fOutFile : TFileName;
     fCallback : T7zProgressCallback;
   protected
    procedure Execute; override;
   public
    constructor Create(AFormHandle : THandle; const AInFile, AOutFile : TFileName; ACallback : T7zProgressCallback);
  end;

function ProgressCallback(sender: Pointer; total: boolean; value: int64): HRESULT; stdcall;
var vCallBackParams : pCallBackParams;
begin
  vCallBackParams := pCallBackParams(sender);   
  Result := E_ABORT;
  if total then
  PostMessage(vCallBackParams^.FormHandle, SET_TOTAL, vCallBackParams^.CallBackEvent, Integer(value))
  else PostMessage(vCallBackParams^.FormHandle, WND_PRG_UPDATE, vCallBackParams^.CallBackEvent, Integer(value));

  if (WaitForSingleObject(vCallBackParams.CallBackEvent, cWaitTimeOut) = WAIT_OBJECT_0) then
      Result := S_OK;
  vCallBackParams^.Result := Result;
end;

procedure CloseForm(ACallBackParams : TCallBackParams);
begin
  PostMessage(ACallBackParams.FormHandle, WND_CLOSE, 0, mrOk);
end;

function CheckItemAvailable(const AInFile: TFileName; const Item : integer): boolean;
var
Arch: I7zInArchive;
begin
  Arch := CreateInArchive(CLSID_CFormat7z);
  Arch.OpenFile(AInFile);
  result := (Cardinal(Item)< Arch.NumberOfItems) and not (Arch.ItemIsFolder[Item]);
end;

function CheckItemsCount(const AInFile: TFileName): cardinal;
var
Arch: I7zInArchive;
begin
  Arch := CreateInArchive(CLSID_CFormat7z);
  Arch.OpenFile(AInFile);
  result := Arch.NumberOfItems;
end;

function GetItemName(const AInFile: TFileName; const Item : integer): string;
var
Arch: I7zInArchive;
begin
  Arch := CreateInArchive(CLSID_CFormat7z);
  Arch.OpenFile(AInFile);
  if (Cardinal(Item) < Arch.NumberOfItems) then
  result := Arch.ItemPath[Item] else result := EmptyStr;
end;

{ TUnArchThread }

constructor TUnArchThread.Create(AFormHandle : THandle; const AInFile: TFileName; const AItem: integer;
  AMemory: TMemoryStream; ACallback: T7zProgressCallback);
begin
  fCallBackParams.FormHandle := AFormHandle;
  fInFile := AInFile;
  fItem := AItem;
  fMemory := AMemory;
  fCallback := ACallback;
  FreeOnTerminate := true;
  inherited Create(false);
end;

procedure TUnArchThread.Execute;
var 
Arch: I7zInArchive;
begin
  fCallBackParams.CallBackEvent := CreateEvent(nil, false, false, nil);
  try
    Arch := CreateInArchive(CLSID_CFormat7z);
    Arch.SetProgressCallback(@fCallBackParams, fCallback);
    Arch.OpenFile(fInFile);
    if (Cardinal(fItem) < Arch.NumberOfItems) and not (Arch.ItemIsFolder[fItem]) then
    Arch.ExtractItem(fItem, fMemory, false);
  finally 
    CloseForm(fCallBackParams);
    CloseHandle(fCallBackParams.CallBackEvent);
  end;
end;


constructor TSingleArchThread.Create(AFormHandle : THandle; const AInFile, AOutFile: TFileName; ACallback : T7zProgressCallback);
begin
  fCallBackParams.FormHandle := AFormHandle;
  fInFile := AInFile;
  fOutFile := AOutFile;
  fCallback := ACallback;
  FreeOnTerminate := true;
  inherited Create(false);
end;

procedure TSingleArchThread.Execute;
var Arch: I7zOutArchive;
begin
  fCallBackParams.CallBackEvent := CreateEvent(nil, false, false, nil);
  try
    Arch := CreateOutArchive(CLSID_CFormat7z);
    if FileExists(fInFile)  then
      Arch.AddFile(fInFile, ExtractFileName(fInFile));
    SetCompressionLevel(Arch, 5);
    SetMultiThreading(Arch, TThread.ProcessorCount);
    SevenZipSetCompressionMethod(Arch, m7BZip2);
    Arch.SetProgressCallback(@fCallBackParams, fCallback);
    Arch.SaveToFile(fOutFile);
  finally
    CloseForm(fCallBackParams);
    CloseHandle(fCallBackParams.CallBackEvent); 
  end;
end;

constructor TArchFrm.Create(AOwner: TComponent; AMode : TFrmMode; AMemory : TMemoryStream;  const AInFile,
  AOutFile: TFileName);
begin
  inherited Create(AOwner);
  fMode := AMode;
  fMemory := AMemory;
  fInFile := AInFile;
  fOutFile := AOutFile;
end;

procedure TArchFrm.FormShow(Sender: TObject);
begin
  PostMessage(self.Handle, START_PROCESS, 0, 0);
end;

procedure TArchFrm.OnCloseMessage(var Message: TMessage);
begin
  self.ModalResult := Message.LParam;
end;

procedure TArchFrm.OnSetTotal(var Message: TMessage);
begin
  ProgressBar1.Max := Message.LParam; 
  SetEvent(Message.WParam);
end;

procedure TArchFrm.Start(var Message: TMessage);
var item, ItemsCount : integer;
ItemsFrm : TSelectUnArchItemFrm;
begin
  case fMode of
    mCompress:
    begin
      if (fOutFile <> EmptyStr) then
      TSingleArchThread.Create(self.Handle, fInFile, fOutFile, ProgressCallback);
      Self.Caption := Format('Сжатие файла %s -> %s',[fInFile, fOutFile]);
    end;
    mUnCompress:
    if assigned(fMemory) then
    begin
      ItemsCount := CheckItemsCount(fInFile);
      if (ItemsCount > 0) then
      begin
        if (ItemsCount = 1) and CheckItemAvailable(fInFile, 0) then
        item := 0
        else if (ItemsCount = 1) then
        begin
          self.Close;
          exit;
        end
        else
        begin
          ItemsFrm := TSelectUnArchItemFrm.Create(self, fInFile);
          try
            if (ItemsFrm.ShowModal = mrOk) then
            item := ItemsFrm.ItemIndex else
            begin
              self.Close;
              exit;
            end;
            if (item = -1) then
            begin
              self.Close;
              exit;
            end;
          finally
            FreeAndNil(ItemsFrm);
          end;
        end;
      end else
      begin
        self.Close;
        exit;
      end;
      fItemName := GetItemName(fInFile, item);
      TUnArchThread.Create(self.Handle, fInFile, item, fMemory, ProgressCallback);
      Self.Caption := Format('Распаковка файла %s',[fInFile]);
    end else self.Close;
  end;  
end;

procedure TArchFrm.UpdateProgress(var Message: TMessage);
begin
  ProgressBar1.Position := Message.LParam;  
  SetEvent(Message.WParam);
end;

end.
