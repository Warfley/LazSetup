unit DetailedList;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, StdCtrls, ExtCtrls, Generics.Collections, uDarkStyleParams;

type
  EInvalidIndexException = class(Exception);
  EItemAlreadyInListException = class(Exception);

  TDetailedList = class;
  TDetailedListItem = class;

  TCreateRightControlDelegate = function(Sender: TDetailedListItem): TControl of object;

  { TDetailedListItem }

  TDetailedListItem = class(TComponent)
  private type
    TItemBaseControl = class(TCustomControl)
    public
      property OnMouseEnter;
      property OnMouseLeave;
    end;
  procedure ItemClicked(Sender: TObject);
  procedure ItemMouseEntered(Sender: TObject);
  procedure ItemMouseLeft(Sender: TObject);
  private
    FOwner: TDetailedList;
    FIndex: Integer;
    // Base Elements
    FBaseControl: TItemBaseControl;
    FCaptionLabel: TLabel;
    FDetailsLabel: TLabel;
    FNoteLabel: TLabel;

    // Additional Elements
    FImage: TImage;
    FRightControl: TControl;
    FOnCreateRightControl: TCreateRightControlDelegate;

    // Publicly available property data
    FCheckable: Boolean;
    FChecked: Boolean;
    FBorderColor: TColor;
    FBorderWidth: Integer;
    FBackground: TColor;
    FHighlightBackground: TColor;
    FHighlightWidth: Integer;
    FCheckedColor: Integer;
    FCheckedWidth: Integer;

    FHighlighted: Boolean;

    // Events
    FOnClick: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;

    // Getter and Setters for properties
    function GetCaption: TCaption;
    function GetCaptionFont: TFont;
    function GetDetails: TCaption;
    function GetDetailsFont: TFont;
    function GetNote: TCaption;
    function GetNoteFont: TFont;
    function GetPicture: TPicture;
    procedure SetBorderWidth(AValue: Integer);
    procedure SetCaption(AValue: TCaption);
    procedure SetChecked(AValue: Boolean);
    procedure SetDetails(AValue: TCaption);
    procedure SetHighlightWidth(AValue: Integer);
    procedure SetNote(AValue: TCaption);
    procedure SetPicture(AValue: TPicture);    
    procedure SetCaptionFont(AValue: TFont);
    procedure SetDetailsFont(AValue: TFont);
    procedure SetNoteFont(AValue: TFont);   
    procedure SetCheckedWidth(AValue: Integer);

  private
    function GetIconSize: Integer;
    procedure ReAdjust;            
    procedure PaintBaseControl(Sender: TObject);
    procedure SetCheckable(AValue: Boolean);
    procedure SetIconSize(AValue: Integer);
    procedure SetOnCreateRightControl(AValue: TCreateRightControlDelegate);
  protected
    procedure UpdateRightControl; virtual;
    procedure UpdateCheckState(AValue: Boolean); virtual;
  public
    constructor Create(AList: TDetailedList);
    destructor Destroy; override;

    property OnCreateRightControl: TCreateRightControlDelegate read FOnCreateRightControl write SetOnCreateRightControl;
  published
    property Index: Integer read FIndex;
    property Caption: TCaption read GetCaption write SetCaption;
    property CaptionFont: TFont read GetCaptionFont write SetCaptionFont;
    property Details: TCaption read GetDetails write SetDetails;
    property DetailsFont: TFont read GetDetailsFont write SetDetailsFont;
    property Note: TCaption read GetNote write SetNote;
    property NoteFont: TFont read GetNoteFont write SetNoteFont;
    property Icon: TPicture read GetPicture write SetPicture;
    property IconSize: Integer read GetIconSize write SetIconSize;

    property Checked: Boolean read FChecked write SetChecked;
    property Checkable: Boolean read FCheckable write SetCheckable;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth;
    property Background: TColor read FBackground write FBackground;
    property HighlightBackground: TColor read FHighlightBackground write FHighlightBackground;
    property HighlightWidth: Integer read FHighlightWidth write SetHighlightWidth;
    property CheckedColor: TColor read FCheckedColor write FCheckedColor;  
    property CheckedWidth: Integer read FCheckedWidth write SetCheckedWidth;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

  TListKind = (lskButtons, lskList, lskMultiSelect);

  TDetailedList = class(TScrollingWinControl)
  private type
    TDetailedItemList = specialize TList<TDetailedListItem>;
  private
    FItems: TDetailedItemList;

    FItemHeight: Integer;
    FSpacing: Integer;  
    FEdgeRadius: Integer;
    FListKind: TListKind;
    FSelected: TDetailedListItem;
    FColumnWidth: Integer; 
    FFillLast: Boolean;
    FUpdateDepth: Integer;

    function GetCount: Integer;
    function GetItem(AIndex: Integer): TDetailedListItem;
    procedure SetColumnWidth(AValue: Integer);
    procedure SetEdgeRadius(AValue: Integer);
    procedure SetFillLast(AValue: Boolean);
    procedure SetItem(AIndex: Integer; AValue: TDetailedListItem);
    procedure SetItemHeight(AValue: Integer);
    procedure SetListKind(AValue: TListKind);
    procedure SetSelected(AValue: TDetailedListItem);
    procedure SetSpacing(AValue: Integer);
  protected

    procedure SelectionChanged(ChangedItem: TDetailedListItem; IsSelected: Boolean);

    procedure SwapItems(AFirst, ASecond: Integer);
    procedure AddItem(AItem: TDetailedListItem);
    procedure BoundsChanged; override;
    procedure DoOnResize; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    function Add(const ACaption: String; const ADetails: String;
      const ANote: String = ''; AIcon: TPicture = nil): TDetailedListItem;
    procedure Delete(AIndex: Integer);
    procedure Clear;
    procedure RefreshItems;
    function NumColumns: Integer; inline;

    procedure BeginUpdate;
    procedure EndUpdate;


    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TDetailedListItem read GetItem write SetItem;
  published
    property ItemHeight: Integer read FItemHeight write SetItemHeight;
    property Spacing: Integer read FSpacing write SetSpacing;
    property ListKind: TListKind read FListKind write SetListKind;
    property EdgeRadius: Integer read FEdgeRadius write SetEdgeRadius;
    property Selected: TDetailedListItem read FSelected write SetSelected;
    property ColumnWidth: Integer read FColumnWidth write SetColumnWidth;  
    property FillLast: Boolean read FFillLast write SetFillLast;
  end;

implementation

{ TDetailedListItem }

procedure TDetailedListItem.ItemMouseLeft(Sender: TObject);
var
  RelativePos: TPoint;
begin
  RelativePos := FBaseControl.ScreenToClient(Mouse.CursorPos);
  if (RelativePos.X <= 0) or (RelativePos.X >= FBaseControl.Width)
  or (RelativePos.Y <= 0) or (RelativePos.Y >= FBaseControl.Height) then
  begin
    FHighlighted := False;
    FBaseControl.Refresh;
    if Assigned(FOnMouseLeave) then
      FOnMouseLeave(Self);
  end;
end;

procedure TDetailedListItem.ItemMouseEntered(Sender: TObject);
begin
  FHighlighted := True; 
  FBaseControl.Refresh; 
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TDetailedListItem.ItemClicked(Sender: TObject);
begin
  FOwner.SelectionChanged(Self, not Checked);
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

function TDetailedListItem.GetCaption: TCaption;
begin
  Result := FCaptionLabel.Caption;
end;

function TDetailedListItem.GetCaptionFont: TFont;
begin
  Result := FCaptionLabel.Font;
end;

function TDetailedListItem.GetDetails: TCaption;
begin
  Result := FDetailsLabel.Caption;
end;

function TDetailedListItem.GetDetailsFont: TFont;
begin
  Result := FDetailsLabel.Font;
end;

function TDetailedListItem.GetNote: TCaption;
begin
  Result := FNoteLabel.Caption;
end;

function TDetailedListItem.GetNoteFont: TFont;
begin
  Result := FNoteLabel.Font;
end;

function TDetailedListItem.GetPicture: TPicture;
begin
  Result := FImage.Picture;
end;

procedure TDetailedListItem.SetBorderWidth(AValue: Integer);
begin
  if FBorderWidth=AValue then Exit;
  FBorderWidth:=AValue;
  ReAdjust;
end;

procedure TDetailedListItem.SetCaption(AValue: TCaption);
begin
  FCaptionLabel.Caption := AValue;
end;  

procedure TDetailedListItem.SetCaptionFont(AValue: TFont);
begin
  FCaptionLabel.Font := AValue;
end;

procedure TDetailedListItem.SetChecked(AValue: Boolean);
begin
  if FChecked=AValue then Exit;
  FOwner.SelectionChanged(Self, AValue);
end;

procedure TDetailedListItem.SetDetails(AValue: TCaption);
begin
  FDetailsLabel.Caption := AValue;
end;       

procedure TDetailedListItem.SetDetailsFont(AValue: TFont);
begin
  FDetailsLabel.Font := AValue;
end;

procedure TDetailedListItem.SetHighlightWidth(AValue: Integer);
begin
  if FHighlightWidth=AValue then Exit;
  FHighlightWidth:=AValue; 
  ReAdjust;
end;

procedure TDetailedListItem.SetNote(AValue: TCaption);
begin
  FNoteLabel.Caption := AValue;
  ReAdjust;
end; 

procedure TDetailedListItem.SetNoteFont(AValue: TFont);
begin
  FNoteLabel.Font := AValue;
end;

procedure TDetailedListItem.SetPicture(AValue: TPicture);
begin
  FImage.Picture := AValue;
  FImage.Show;
  ReAdjust;
end;    

procedure TDetailedListItem.SetCheckedWidth(AValue: Integer);
begin
  if FCheckedWidth=AValue then Exit;
  FCheckedWidth:=AValue;
  ReAdjust;
end;

function TDetailedListItem.GetIconSize: Integer;
begin
  Result := FImage.Width;
end;

procedure TDetailedListItem.PaintBaseControl(Sender: TObject);
begin
  with FBaseControl.Canvas do
  begin
    Brush.Style := bsSolid;
    Pen.Style := psSolid;
    if FHighlighted then
    begin
      Pen.Width := FHighlightWidth;
      Pen.Color := FBorderColor;
      Brush.Color := FHighlightBackground;
    end
    else
    begin
      Pen.Width := FBorderWidth;
      if FChecked then
        Pen.Width := FCheckedWidth;
      Pen.Color := FBorderColor;
      Brush.Color := FBackground;
    end;  
    if FChecked then
      Pen.Color := FCheckedColor;
    RoundRect(Pen.Width div 2, Pen.Width div 2,
              FBaseControl.Width - Pen.Width div 2,
              FBaseControl.Height - Pen.Width div 2,
              FOwner.EdgeRadius, FOwner.EdgeRadius);
  end;
end;

procedure TDetailedListItem.SetCheckable(AValue: Boolean);
begin
  if FCheckable=AValue then Exit;
  if not AValue then
    Checked := False; 
  FCheckable:=AValue;
end;

procedure TDetailedListItem.SetIconSize(AValue: Integer);
begin
  FImage.Width := AValue;
  FImage.Height := AValue;
end;

procedure TDetailedListItem.SetOnCreateRightControl(
  AValue: TCreateRightControlDelegate);
begin
  if FOnCreateRightControl=AValue then Exit;
  FOnCreateRightControl:=AValue;
  UpdateRightControl;
end;

procedure TDetailedListItem.ReAdjust;
var
  MaxBorderWidth: Integer;
  LeftAlign, TopAlign, RightAlign, BotAlign, NumColumns, ColumnIndex: Integer;
begin
  if FOwner.FUpdateDepth > 0 then
    Exit;
  NumColumns := FOwner.NumColumns;
  ColumnIndex := FIndex mod NumColumns;
  FBaseControl.Height := FOwner.ItemHeight;
  FBaseControl.Width := FOwner.ClientWidth div NumColumns - FOwner.ChildSizing.LeftRightSpacing * 2;
  if FOwner.FillLast and (FIndex = FOwner.Count - 1) then
    FBaseControl.Width := (NumColumns-ColumnIndex) * FOwner.ClientWidth div NumColumns - FOwner.ChildSizing.LeftRightSpacing * 2;
  FBaseControl.Left := ColumnIndex * (FBaseControl.Width + FOwner.Spacing) + FOwner.ChildSizing.LeftRightSpacing;
  FBaseControl.Top := (FIndex div NumColumns) * (FOwner.ItemHeight + FOwner.Spacing) + FOwner.ChildSizing.TopBottomSpacing;
  MaxBorderWidth := FHighlightWidth;
  if FBorderWidth > MaxBorderWidth then
    MaxBorderWidth := FBorderWidth;
  if FCheckedWidth > MaxBorderWidth then
    MaxBorderWidth := FCheckedWidth;
  LeftAlign := MaxBorderWidth + FOwner.EdgeRadius div 2;
  TopAlign := MaxBorderWidth + 4;
  RightAlign := FBaseControl.Width - LeftAlign;
  BotAlign := FBaseControl.Height - MaxBorderWidth - 2;
  // Draw picture if set
  if FImage.Picture.Width > 0 then
  begin
    FImage.Left := LeftAlign;
    //FImage.Height := FBaseControl.Height - 2 * TopAlign;
    //FImage.Width := FImage.Height;
    FImage.Top := FBaseControl.Height div 2 - FImage.Height div 2;
    LeftAlign += FImage.Width + 4;
  end;
  // Right control if set
  if Assigned(FRightControl) then
  begin
    FRightControl.Left := RightAlign - FRightControl.Width;
    RightAlign -= FRightControl.Width + 4;
    // Centered vertically
    FRightControl.Top := FBaseControl.Height div 2 - FRightControl.Height div 2;
  end;
  // Caption label: On top left (right of picture)
  FCaptionLabel.Left := LeftAlign;
  FCaptionLabel.Top := TopAlign;
  FCaptionLabel.Height:=FCaptionLabel.Canvas.TextHeight(FCaptionLabel.Caption);
  FCaptionLabel.Width := RightAlign - LeftAlign;
  TopAlign += FCaptionLabel.Height + 4;

  // Note label: bottom left (right of picture
  FNoteLabel.Left := LeftAlign;
  FNoteLabel.Height:=FNoteLabel.Canvas.TextHeight(FNoteLabel.Caption);
  FNoteLabel.Width := RightAlign - LeftAlign;
  FNoteLabel.Top := BotAlign - FNoteLabel.Height;
  BotAlign -= FNoteLabel.Height - 4;

  // Description label: in between Caption and Note
  FDetailsLabel.AutoSize := False;
  FDetailsLabel.WordWrap := True;   
  FDetailsLabel.Width := RightAlign - LeftAlign;
  FDetailsLabel.Height := BotAlign - TopAlign;
  // Position must be set after width or height because otherwise it will go wrong
  FDetailsLabel.Top := TopAlign;
  FDetailsLabel.Left := LeftAlign;

  FBaseControl.Refresh;
end;

procedure TDetailedListItem.UpdateRightControl;
begin
  FreeAndNil(FRightControl);
  if Assigned(OnCreateRightControl) then
    FRightControl := OnCreateRightControl(Self)
  else if FOwner.ListKind = lskList then
  begin
    FRightControl := TRadioButton.Create(FBaseControl);
    TRadioButton(FRightControl).Checked := FChecked;
  FRightControl.OnClick := @ItemClicked;
  end
  else if FOwner.ListKind = lskMultiSelect then
  begin
    FRightControl := TCheckBox.Create(FBaseControl);
    TCheckBox(FRightControl).Checked := FChecked;   
    FRightControl.OnClick := @ItemClicked;
  end;

  if Assigned(FRightControl) then
    FRightControl.Parent := FBaseControl;

  ReAdjust;
end;

procedure TDetailedListItem.UpdateCheckState(AValue: Boolean);
var
  OldOnClick: TNotifyEvent;
begin
  FChecked := AValue;
  if Assigned(FRightControl) then
  begin
    OldOnClick := FRightControl.OnClick;   
    FRightControl.OnClick := nil;

    if FRightControl is TRadioButton then
      TRadioButton(FRightControl).Checked := AValue
    else if FRightControl is TCheckBox then
      TCheckBox(FRightControl).Checked := AValue;

    FRightControl.OnClick := OldOnClick;
  end;
end;

constructor TDetailedListItem.Create(AList: TDetailedList);
begin
  inherited Create(AList);

  FOwner := AList;
  FIndex := -1;

  FBaseControl := TItemBaseControl.Create(Self);
  FBaseControl.OnPaint :=@PaintBaseControl;
  FBaseControl.Parent := FOwner;
  FBaseControl.OnMouseEnter :=@ItemMouseEntered;
  FBaseControl.OnMouseLeave :=@ItemMouseLeft;
  FBaseControl.OnClick :=@ItemClicked;

  FCaptionLabel := TLabel.Create(FBaseControl);
  FCaptionLabel.Parent := FBaseControl;
  FCaptionLabel.Font.Color := clBtnText;
  FCaptionLabel.Font.Bold := True;
  FCaptionLabel.Anchors := [];
  FCaptionLabel.AutoSize := False;
  FCaptionLabel.Enabled:=False; // Dont catch mouse events

  FDetailsLabel := TLabel.Create(FBaseControl);
  FDetailsLabel.Parent := FBaseControl;  
  FDetailsLabel.Font.Color := clBtnText;
  FDetailsLabel.WordWrap := True;
  FDetailsLabel.Anchors := [];
  FDetailsLabel.AutoSize := False;
  FDetailsLabel.Enabled:=False; // Dont catch mouse events

  FNoteLabel := TLabel.Create(FBaseControl);
  FNoteLabel.Parent := FBaseControl;
  FNoteLabel.Font.Size := Round(FCaptionLabel.Font.Size * 0.75);
  FNoteLabel.Font.Color := clGrayText; 
  FNoteLabel.Anchors := [];  
  FNoteLabel.AutoSize := False;
  FNoteLabel.Enabled:=False; // Dont catch mouse events

  FImage := TImage.Create(FBaseControl);
  FImage.Parent := FBaseControl;
  FImage.Proportional := True;
  FImage.Stretch := True;
  FImage.Center := True;
  FImage.Width := 32;
  FImage.Height := 32;
  FImage.Enabled:=False; // Dont catch mouse events
  FImage.Hide;

  FCheckable := True;
  FChecked := False;
  // TODO: find a system color that this check is not required
  if IsDarkModeEnabled then
    FBorderColor := clHotLight
  else
    FBorderColor := TColor($DEDEDE);
  FBorderWidth := 2;
  FBackground := clDefault;
  FHighlightBackground := clWindow;
  FHighlightWidth := 4;
  FCheckedColor := clHighlight; 
  FCheckedWidth := 2;

  FHighlighted := False;

  UpdateRightControl;

  FOwner.AddItem(Self);
end;

destructor TDetailedListItem.Destroy;
begin
  inherited Destroy;
end;

{ TDetailedList }

procedure TDetailedList.SetItemHeight(AValue: Integer);
begin
  if FItemHeight=AValue then Exit;
  FItemHeight:=AValue;
  RefreshItems;
end;

procedure TDetailedList.SetListKind(AValue: TListKind);
begin
  if FListKind=AValue then Exit;
  FListKind:=AValue;
  RefreshItems;
end;

procedure TDetailedList.SetSelected(AValue: TDetailedListItem);
var
  Item: TDetailedListItem;
begin
  if FSelected=AValue then Exit;
  if Assigned(AValue) then
    SelectionChanged(AValue, True)
  else
    for Item in FItems do
      Item.Checked := False;
end;

procedure TDetailedList.SetSpacing(AValue: Integer);
begin
  if FSpacing=AValue then Exit;
  FSpacing:=AValue; 
  RefreshItems;
end;

function TDetailedList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TDetailedList.GetItem(AIndex: Integer): TDetailedListItem;
begin
  if (AIndex < 0) or (AIndex >= FItems.Count) then
    raise EInvalidIndexException.Create('Index not in list');
  Result := FItems[AIndex];
end;

procedure TDetailedList.SetColumnWidth(AValue: Integer);
begin
  if FColumnWidth=AValue then Exit;
  FColumnWidth:=AValue;
  RefreshItems;
end;

procedure TDetailedList.SetEdgeRadius(AValue: Integer);
begin
  if FEdgeRadius=AValue then Exit;
  FEdgeRadius:=AValue;
  RefreshItems;
end;

procedure TDetailedList.SetFillLast(AValue: Boolean);
begin
  if FFillLast=AValue then Exit;
  FFillLast:=AValue;
  RefreshItems;
end;

procedure TDetailedList.SetItem(AIndex: Integer; AValue: TDetailedListItem);
begin
  if (AIndex < 0) or (AIndex >= FItems.Count) then
    raise EInvalidIndexException.Create('Index not in list');
  if FItems[AIndex] = AValue then
    Exit;
  if AValue.FIndex < 0 then // New item just replace the old
  begin
    FItems[AIndex].Free;
    FItems[AIndex] := AValue;
    AValue.ReAdjust;
  end
  else // Is already in this list -> perform a swap
    SwapItems(AValue.FIndex, AIndex);
end;

procedure TDetailedList.SelectionChanged(ChangedItem: TDetailedListItem;
  IsSelected: Boolean);
begin
  if Assigned(ChangedItem) and not ChangedItem.Checkable then
    Exit;
  if FListKind = lskButtons then Exit;

  if (FListKind = lskList) and Assigned(FSelected) and IsSelected then
  begin // Deselect old
    FSelected.UpdateCheckState(False);
    FSelected.FBaseControl.Invalidate;
    FSelected := nil;
  end;

  // Select or unselect new
  if not Assigned(FSelected) and IsSelected then
    FSelected := ChangedItem
  else if not IsSelected and (ChangedItem = FSelected) then
    FSelected := nil;

  if Assigned(ChangedItem) then
  begin
    ChangedItem.UpdateCheckState(IsSelected);
    ChangedItem.FBaseControl.Invalidate;
  end;
end;

procedure TDetailedList.SwapItems(AFirst, ASecond: Integer);
var
  tmp: TDetailedListItem;
begin
  // Use Items for read access to trigger exceptions if out of bounds index
  // Use FItems for write access to avoid recursive call to this swap
  tmp := Items[AFirst];
  FItems[AFirst] := Items[ASecond];
  FItems[ASecond] := tmp;
  // Update indices
  FItems[AFirst].FIndex := AFirst;
  FItems[ASecond].FIndex := ASecond;
  // And redraw
  FItems[AFirst].ReAdjust;
  FItems[ASecond].ReAdjust;
end;

procedure TDetailedList.AddItem(AItem: TDetailedListItem);
begin
  if AItem.FIndex >= 0 then
    raise EItemAlreadyInListException.Create('Trying to add duplicate Item');
  AItem.FIndex := FItems.Add(AItem);
  AItem.ReAdjust;
end;

procedure TDetailedList.BoundsChanged;
begin
  RefreshItems;
  inherited BoundsChanged;
end;

procedure TDetailedList.DoOnResize;
begin
  RefreshItems;
  inherited DoOnResize;
end;

constructor TDetailedList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FItems := TDetailedItemList.Create;

  FItemHeight := 80;
  FSpacing := 16;
  FSelected := nil;
  FEdgeRadius := 16;
  FColumnWidth := -1;
  FListKind := lskList;
  AutoScroll := True;
  VertScrollBar.Smooth := True;
  FFillLast := True;
end;

destructor TDetailedList.Destroy;
begin
  FItems.Free; // the contents of the list will be freed by the component/owner principle
  inherited Destroy;
end;

function TDetailedList.Add(const ACaption: String; const ADetails: String;
  const ANote: String; AIcon: TPicture): TDetailedListItem;
begin
  Result := TDetailedListItem.Create(Self);
  Result.Caption := ACaption;
  Result.Details := ADetails;
  Result.Note := ANote;
  Result.Icon := AIcon;
end;

procedure TDetailedList.Delete(AIndex: Integer);
var
  i: Integer;
begin
  // Use setter for AIndex range checks
  if Selected = Items[AIndex] then
  begin
    Selected := nil;
    for i:=0 to FItems.Count - 1 do
      if FItems[i].Checked then
      begin
        FSelected := FItems[i];
        Break;
      end;
  end;
  Items[AIndex].Free;
  FItems.Delete(AIndex);
  for i:=0 to FItems.Count -1 do
  begin
    FItems[i].FIndex := i;
    FItems[i].ReAdjust;
  end;
end;

procedure TDetailedList.Clear;
var
  Item: TDetailedListItem;
begin
  for Item in FItems do
    Item.Free;
  FItems.Clear;
  FSelected := nil;
end;

procedure TDetailedList.RefreshItems;
var
  i: Integer;
begin
  if (FUpdateDepth > 0) or not Assigned(FItems) then
    Exit;
  for i:=0 to FItems.Count - 1 do
    FItems[i].ReAdjust;
end;

function TDetailedList.NumColumns: Integer;
begin
  Result := ClientWidth div FColumnWidth;
  if Result < 1 then
    Result := 1;
end;

procedure TDetailedList.BeginUpdate;
begin
  Inc(FUpdateDepth);
end;

procedure TDetailedList.EndUpdate;
begin
  if FUpdateDepth > 0 then
    Dec(FUpdateDepth);
  if FUpdateDepth = 0 then
    RefreshItems;
end;

end.

