unit CompareSelectFrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, LResources, DetailedList;

type

  TCompareOption = record
    IconName: String;
    Caption: String;
    Details: String;
    Note: String;
    ImageName: String;
  end;

  TCompareOptions = array of TCompareOption;

  { TCompareSelectFrame }

  TCompareSelectFrame = class(TFrame)
    CompareImage: TImage;
    HeaderPanel: TPanel;
    HeaderLabel: TLabel;      
    CompareList: TDetailedList;
    procedure FrameResize(Sender: TObject);
  private
    FOptions: TCompareOptions;
    function CreatePreviewImage(Sender: TDetailedListItem): TControl;
    procedure ItemMouseEnter(Sender: TObject);
    procedure ItemMouseLeave(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;

    procedure SetItems(const Headline: String; const Options: TCompareOptions;
                       DefaultSelection: Integer = 0; RightImages: Boolean = True);

    function SelectedIndex: Integer; inline;
  end;

function CompareOption(const ImageName: String; const Caption: String;
  const Details: String; const Note: String = ''; const IconName: String = ''): TCompareOption; inline;
implementation

function CompareOption(const ImageName: String; const Caption: String;
  const Details: String; const Note: String; const IconName: String
  ): TCompareOption;
begin
  Result.Caption := Caption;
  Result.Details := Details;
  Result.Note := Note;
  Result.ImageName := ImageName;
  Result.IconName := IconName;
end;

{$R *.lfm}

{ TCompareSelectFrame }

procedure TCompareSelectFrame.FrameResize(Sender: TObject);
begin
  CompareList.Width := Self.Width div 2 - 8;
end;

function TCompareSelectFrame.CreatePreviewImage(Sender: TDetailedListItem): TControl;
var
  Img: TImage;
begin
  Result := TImage.Create(Sender);
  Img := TImage(Result);
  Img.Width := 64;
  Img.Height := 64;
  Img.Enabled := False;
  Img.Stretch := True;
  Img.Proportional := True;
  Img.Center := True;
  Img.Picture.LoadFromLazarusResource(FOptions[Sender.Index].ImageName);
end;

procedure TCompareSelectFrame.ItemMouseEnter(Sender: TObject);
begin
  CompareImage.Picture.LoadFromLazarusResource(FOptions[TDetailedListItem(Sender).Index].ImageName);
end;

procedure TCompareSelectFrame.ItemMouseLeave(Sender: TObject);
begin
  if Assigned(CompareList.Selected) then
    CompareImage.Picture.LoadFromLazarusResource(FOptions[CompareList.Selected.Index].ImageName);

end;

constructor TCompareSelectFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  CompareList := TDetailedList.Create(Self);
  CompareList.Parent := Self;
  CompareList.Align:=alLeft;
  CompareList.Width := Self.Width div 2 - 8;
end;

procedure TCompareSelectFrame.SetItems(const Headline: String;
  const Options: TCompareOptions; DefaultSelection: Integer;
  RightImages: Boolean);
var
  Option: TCompareOption;
begin
  HeaderLabel.Caption := Headline;
  FOptions := Options; 
  CompareList.Clear;
  for Option in FOptions do
  begin
    With CompareList.Add(Option.Caption, Option.Details, Option.Note) do
    begin
      if not Option.IconName.IsEmpty then
        Icon.LoadFromLazarusResource(Option.IconName);
      if RightImages then
        OnCreateRightControl := @CreatePreviewImage;
      OnMouseEnter := @ItemMouseEnter;
      OnMouseLeave :=@ItemMouseLeave;
    end;
  end;
  CompareList.Selected := CompareList.Items[DefaultSelection];
  ItemMouseLeave(CompareList.Selected);
end;

function TCompareSelectFrame.SelectedIndex: Integer;
begin
  Result := -1;
  if Assigned(CompareList.Selected) then
    Result := CompareList.Selected.Index;
end;

end.

