unit SelectUnArchItem;


interface

uses
  System.SysUtils,System. Classes, Vcl.Forms, VirtualTrees, Vcl.Controls;

type
  TSelectUnArchItemFrm = class(TForm)
    UnArchItemsGrid: TVirtualStringTree;
    procedure FormShow(Sender: TObject);
    procedure UnArchItemsGridGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure UnArchItemsGridNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
  private
    fInFile : TFileName;
    fItemIndex: integer;
  public
    constructor Create(AOwner :TComponent; const AInFile : TFileName); reintroduce;
    property ItemIndex : integer read fItemIndex;
  end;

implementation
uses
sevenzip2;
//sevenzip_16_04;
{$R *.dfm}

type
TGridRec = record
  ItemIndex : Word;
  Size : LongWord;
  Path : String;
end;

pGridRec = ^TGridRec;

{ TArchItemsFrm }

constructor TSelectUnArchItemFrm.Create(AOwner: TComponent; const AInFile: TFileName);
begin
  inherited Create(AOwner);
  fInFile := AInFile;
  fItemIndex := -1;
  UnArchItemsGrid.NodeDataSize := SizeOf(TGridRec);
end;

procedure TSelectUnArchItemFrm.FormShow(Sender: TObject);
var
Arch: I7zInArchive;
i : integer;
vNewNode : PVirtualNode;
vNodeData: pGridRec;
begin
  i := 0;
  Arch := CreateInArchive(CLSID_CFormat7z);
  Arch.OpenFile(fInFile);
  UnArchItemsGrid.BeginUpdate;
  try
    UnArchItemsGrid.Clear;
    while (cardinal(i) < Arch.NumberOfItems) do
    begin
      if not Arch.ItemIsFolder[i] then
      begin
        vNewNode := UnArchItemsGrid.AddChild(UnArchItemsGrid.RootNode);
        if not (vsInitialized in vNewNode.States) then
        UnArchItemsGrid.ReinitNode(vNewNode, False);
        vNodeData := UnArchItemsGrid.GetNodeData(vNewNode);
        vNodeData.ItemIndex := i; 
        vNodeData.Size := Arch.ItemSize[i];
        vNodeData.Path := Arch.ItemPath[i];
      end;
      inc(i);
    end;
  finally
    UnArchItemsGrid.EndUpdate;
  end;
end;

procedure TSelectUnArchItemFrm.UnArchItemsGridGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var RecData: pGridRec;
begin
  RecData := Sender.GetNodeData(Node);
  case Column of
    0: CellText := RecData.Path;
    1: CellText := Format('%d',[RecData.Size]);
  end;
end;

procedure TSelectUnArchItemFrm.UnArchItemsGridNodeDblClick(
  Sender: TBaseVirtualTree; const HitInfo: THitInfo);
var RecData: pGridRec;
begin
  RecData := Sender.GetNodeData(Sender.FocusedNode);
  fItemIndex := RecData.ItemIndex;
  self.ModalResult := mrOk;
end;

end.
