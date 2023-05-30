unit SetupMainFrm;

{$mode objfpc}{$H+}
{$ModeSwitch arrayoperators}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, DetailedList, gitlabfetch, process, RegExpr, LResources,
  LazarusPackageManager, FileUtil, UIFlows;

type
  { TSetupMainForm }

  TSetupMainForm = class(TForm)
    CloseButton: TButton;
    MessageMemo: TMemo;
    MessageButtonPenal: TPanel;
    MessageHeaderLabel: TLabel;
    MessageOKButton: TButton;
    MessageCancelButton: TButton;
    MessagePanel: TPanel;
    NextButton: TBitBtn;
    RunLazarusButton: TBitBtn;
    FooterPanel: TPanel;
    SelectLazarusDirDialog: TSelectDirectoryDialog;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MessageCancelButtonClick(Sender: TObject);
    procedure MessageOKButtonClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure RunLazarusButtonClick(Sender: TObject);
  private
    FLazarusDir: String;   
    FGitPath: String;
    FPackageManager: TLazarusPackageManager;

    FChangedLazVersion: Boolean;
    FChangedTheme: Boolean;
    FChangedLayout: Boolean;
    FChangedScheme: Boolean;
    FChangedPackages: Boolean;

    FMainMenu: TDetailedList;
    FSelectVersionItem: TDetailedListItem;
    FSelectLayoutItem: TDetailedListItem;
    FSelectThemeItem: TDetailedListItem;
    FSelectSchemesItem: TDetailedListItem;
    FManagePackagesItem: TDetailedListItem;

    FCurrentFlow: TUIFlow;

    FRebuildFlow: TRebuildLazarusFlow;
    FChangeVersionFlow: TChangeVersionFlow;
    FChangeLayoutFlow: TChangeLayoutFlow;
    FChangeThemeFlow: TChangeThemeFlow;

    FMessageResult: TModalResult;

    function IsLazarusDir(const DirName: String; out Version: TBuildVersion): Boolean;
    procedure OnFlowCanceld(Sender: TObject);
    procedure OnFlowFinished(Sender: TObject; RequiresRebuild: TRebuildMode);
    procedure OnStepActivated(Sender: TObject);
    procedure OnStepFinished(Sender: TObject);
    procedure ReloadData;

    procedure SelectLayoutClick(Sender: TObject);
    procedure SelectVersionClick(Sender: TObject);

    function ShowFormMessage(const Headline: String; const Message: String;
      AColor: TColor = clDefault; CancelButton: Boolean = False): TModalResult;
    procedure CloseFormMessage(MR: TModalResult);

    procedure StartFlow(AFLow: TUIFlow);

    procedure UpdateButtons;
  public

  end;

var
  SetupMainForm: TSetupMainForm;

implementation

{$R *.lfm}

{ TSetupMainForm }

function TSetupMainForm.ShowFormMessage(const Headline: String;
  const Message: String; AColor: TColor; CancelButton: Boolean): TModalResult;
begin
  MessageCancelButton.Visible := CancelButton;
  MessageMemo.Text := Message;
  MessageHeaderLabel.Caption := Headline;
  MessagePanel.Color := AColor;
  FooterPanel.Enabled := False;
  FMainMenu.Enabled := False;

  MessagePanel.Show;
  Application.ProcessMessages;
  MessagePanel.BringToFront;
  MessageMemo.Color := AColor;
  while MessagePanel.Visible do
  begin
    Application.ProcessMessages;
    Sleep(10);
  end;
  Result := FMessageResult;
end;

procedure TSetupMainForm.CloseFormMessage(MR: TModalResult);
begin
  MessagePanel.Hide;
  FooterPanel.Enabled := True;
  FMainMenu.Enabled := True;
  FMessageResult := MR;
end;

procedure TSetupMainForm.StartFlow(AFLow: TUIFlow);
begin
  if Assigned(AFLow) then
  begin
    FMainMenu.Hide;
    RunLazarusButton.Hide;
    NextButton.Show;
    FCurrentFlow := AFLow;
    FCurrentFlow.Reset;
    FCurrentFlow.NextStep;
  end
  else
  begin
    FMainMenu.Show;
    RunLazarusButton.Show;
    NextButton.Hide;  
    FCurrentFlow := nil;
    UpdateButtons;
  end;
end;

procedure TSetupMainForm.UpdateButtons;
begin
  FMainMenu.BeginUpdate;
  try
    FSelectVersionItem.Caption := 'Lazarus ' + FChangeVersionFlow.SelectedVersion.Version.ToString;
    if FChangedLazVersion then
    begin
      FSelectVersionItem.Note := 'Changed: Requires rebuild';
      FSelectVersionItem.NoteFont.Bold := True;
    end
    else
      if FChangeVersionFlow.OnLatestVersion then
        FSelectVersionItem.Note := 'Latest Version'
      else
      begin
        FSelectVersionItem.Note := 'Update Available';
        FSelectVersionItem.NoteFont.Bold := True;
      end;

    if FChangeLayoutFlow.DockedEnabled then
    begin
      FSelectLayoutItem.Icon.LoadFromLazarusResource('Docked');
      FSelectLayoutItem.NoteFont.Bold := FChangedLayout;
      if FChangedLayout then
        FSelectLayoutItem.Note := 'Changed: Docked (requires rebuild)'
      else
        FSelectLayoutItem.Note := 'Selected: Docked';
    end
    else
    begin                    
      FSelectLayoutItem.Icon.LoadFromLazarusResource('Floating');
      FSelectLayoutItem.NoteFont.Bold := FChangedLayout;
      if FChangedLayout then
        FSelectLayoutItem.Note := 'Changed: Floating (requires rebuild)'
      else
        FSelectLayoutItem.Note := 'Selected: Floating';
    end;

    case FChangeThemeFlow.DarkModeStyle of
    dmsBright:
    begin
      FSelectThemeItem.Icon.LoadFromLazarusResource('BrightTheme');
      FSelectThemeItem.NoteFont.Bold := FChangedTheme;
      if FChangedTheme then
        FSelectThemeItem.Note := 'Changed: Bright (requires rebuild)'
      else
        FSelectThemeItem.Note := 'Selected: Bright';
    end;
    dmsSystem:
    begin
      FSelectThemeItem.Icon.LoadFromLazarusResource('DarkBright');
      FSelectThemeItem.NoteFont.Bold := FChangedTheme;
      if FChangedTheme then
        FSelectThemeItem.Note := 'Changed: System (requires rebuild)'
      else
        FSelectThemeItem.Note := 'Selected: System';
    end;
    dmsDark:
    begin
      FSelectThemeItem.Icon.LoadFromLazarusResource('DarkTheme');
      FSelectThemeItem.NoteFont.Bold := FChangedTheme;
      if FChangedTheme then
        FSelectThemeItem.Note := 'Changed: Dark (requires rebuild)'
      else
        FSelectThemeItem.Note := 'Selected: Dark';
    end;
    end;
  finally
    FMainMenu.EndUpdate;
  end;
end;

procedure TSetupMainForm.FormCreate(Sender: TObject);
begin
  FMainMenu := TDetailedList.Create(Self);
  FMainMenu.Parent := Self;
  FMainMenu.Align := alClient;
  FMainMenu.ListKind := lskButtons;
  FMainMenu.ChildSizing.TopBottomSpacing := 8;
  FMainMenu.ChildSizing.LeftRightSpacing := 8;

  FSelectVersionItem := FMainMenu.Add('Lazarus', 'Switch Version');
  FSelectVersionItem.Icon.Assign(RunLazarusButton.Glyph);
  FSelectVersionItem.OnClick :=@SelectVersionClick;

  FSelectLayoutItem := FMainMenu.Add('IDE Layout', 'Select window layout', 'Current: Floating');
  FSelectLayoutItem.OnClick :=@SelectLayoutClick;

  FSelectThemeItem := FMainMenu.Add('IDE Theme', 'Select between Bright and Dark modes', 'Current: Bright');

  FSelectSchemesItem := FMainMenu.Add('Editor Colorscheme', 'Change the Editors color theme', 'Current: Default');

  FManagePackagesItem := FMainMenu.Add('Manage Packages', 'Install or remove additional packages');
  FManagePackagesItem.Icon.LoadFromLazarusResource('pkg_add_200');


  FRebuildFlow := TRebuildLazarusFlow.Create(Self, @OnStepActivated, @OnStepFinished,
                                             @OnFlowFinished, @OnFlowCanceld, @ShowFormMessage);
  FChangeVersionFlow := TChangeVersionFlow.Create(Self, @OnStepActivated, @OnStepFinished,
                                                  @OnFlowFinished, @OnFlowCanceld, @ShowFormMessage);
  FChangeLayoutFlow := TChangeLayoutFlow.Create(Self, @OnStepActivated, @OnStepFinished,
                                                @OnFlowFinished, @OnFlowCanceld, @ShowFormMessage);
  FChangeThemeFlow := TChangeThemeFlow.Create(Self, @OnStepActivated, @OnStepFinished,
                                              @OnFlowFinished, @OnFlowCanceld, @ShowFormMessage);


  if not FindInPath('git', FGitPath) then
    FGitPath := '';

  ReloadData;
end;

procedure TSetupMainForm.CloseButtonClick(Sender: TObject);
begin
  if not Assigned(FCurrentFlow) then
    Close
  else
    FCurrentFlow.Cancel;
end;

procedure TSetupMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FPackageManager) then
    FPackageManager.Free;
end;

procedure TSetupMainForm.FormResize(Sender: TObject);
begin
  MessagePanel.Width := ClientWidth;
  MessagePanel.Top := ClientHeight div 2 - MessagePanel.Height div 2;
  FMainMenu.ColumnWidth := ClientWidth div 2 - 32;
  if FMainMenu.ColumnWidth < 200 then
    FMainMenu.ColumnWidth := 200;
end;

procedure TSetupMainForm.ReloadData;
var
  LazVersion: TBuildVersion;
  AvailableVersions: TBranchVersions;
begin
  if (ParamCount > 0) and IsLazarusDir(ParamStr(1), LazVersion) then
    FLazarusDir := ParamStr(1)
  else if IsLazarusDir(ExtractFileDir(ParamStr(0)), LazVersion) then
  FLazarusDir := ExtractFileDir(ParamStr(0))
  else if IsLazarusDir(GetCurrentDir, LazVersion) then
    FLazarusDir := GetCurrentDir
  else if IsLazarusDir(GetEnvironmentVariable('LAZDIR'), LazVersion) then
    FLazarusDir := GetEnvironmentVariable('LAZDIR')
  else repeat
    if not SelectLazarusDirDialog.Execute then
    begin
      Close;
      Exit;
    end;
    FLazarusDir := SelectLazarusDirDialog.FileName;
  until IsLazarusDir(FLazarusDir, LazVersion);

  AvailableVersions := GetTagVersions(LazarusProjectID, LazarusTagPrefix);

  if Assigned(FPackageManager) then
    FPackageManager.Free;
  FPackageManager := TLazarusPackageManager.Create(FLazarusDir);
  FPackageManager.Reload;

  FRebuildFlow.Setup(FPackageManager);
  FChangeVersionFlow.Setup(FLazarusDir, FGitPath, LazVersion, AvailableVersions);
  FChangeLayoutFlow.Setup(FPackageManager);
  FChangeThemeFlow.Setup(FPackageManager);

  UpdateButtons;
end;

procedure TSetupMainForm.MessageCancelButtonClick(Sender: TObject);
begin
  CloseFormMessage(mrCancel);
end;

procedure TSetupMainForm.MessageOKButtonClick(Sender: TObject);
begin
  CloseFormMessage(mrOK);
end;

procedure TSetupMainForm.NextButtonClick(Sender: TObject);
begin
  if Assigned(FCurrentFlow) then
    FCurrentFlow.NextStep;
end;

procedure TSetupMainForm.RunLazarusButtonClick(Sender: TObject);
var
  Proc: TProcess;
  i: Integer;
begin
  if FRebuildFlow.RebuildMode = rbmNone then
  begin
    Proc := TProcess.Create(nil);
    try
      Proc.Executable := IncludeTrailingPathDelimiter(FLazarusDir) + 'startlazarus'{$IfDef WINDOWS}+ '.exe'{$EndIf};
      Proc.CurrentDirectory := FLazarusDir;
      for i:=1 to GetEnvironmentVariableCount do
        Proc.Environment.Add(GetEnvironmentString(i));
      Proc.Execute;
    finally
      Proc.Free;
    end;
    Close;
  end
  else if not Assigned(FCurrentFlow) then
    StartFlow(FRebuildFlow);
end;

function TSetupMainForm.IsLazarusDir(const DirName: String; out
  Version: TBuildVersion): Boolean;
var
  Lazbuild, LazVersionOut, LazVersionString: String;
  LazVersionOutLines: TStringArray;
  VersionRegex: TRegExpr;
begin
  Result := False;
  Lazbuild := LazbuildPath(DirName);
  if not FileExists(Lazbuild) then
    Exit;
  if not RunCommand(Lazbuild, ['-v'], LazVersionOut, [poNoConsole]) then
    Exit;
  LazVersionOutLines := LazVersionOut.Trim.Split([LineEnding]);
  LazVersionString := LazVersionOutLines[High(LazVersionOutLines)];

  VersionRegex := TRegexpr.Create('(\d+).(\d+).(\d+)');
  try
    if not VersionRegex.Exec(LazVersionString) then
      Exit;
    Version := BuildVersion(StrToInt(VersionRegex.Match[1]),
                            StrToInt(VersionRegex.Match[2]),
                            StrToInt(VersionRegex.Match[3]));
  finally
    VersionRegex.Free;
  end;
  Result := True;
end;

procedure TSetupMainForm.OnFlowCanceld(Sender: TObject);
begin
  StartFlow(nil);
end;

procedure TSetupMainForm.OnFlowFinished(Sender: TObject;
  RequiresRebuild: TRebuildMode);
begin
  FRebuildFlow.SetRebuildMode(RequiresRebuild);
  if FRebuildFlow.RebuildMode = rbmNone then
  begin
    RunLazarusButton.Caption := 'Run';
    FChangedLazVersion := False;
    FChangedTheme := False;
    FChangedLayout := False;
    FChangedScheme := False;
    FChangedPackages := False;
    UpdateButtons;
  end
  else
    RunLazarusButton.Caption := 'Build';
  StartFlow(nil);
end;

procedure TSetupMainForm.OnStepActivated(Sender: TObject);
begin
  NextButton.Enabled := False;
end;

procedure TSetupMainForm.OnStepFinished(Sender: TObject);
begin
  NextButton.Enabled := True;
end;

procedure TSetupMainForm.SelectLayoutClick(Sender: TObject);
begin
  StartFlow(FChangeLayoutFlow);
end;

procedure TSetupMainForm.SelectVersionClick(Sender: TObject);
begin
  StartFlow(FChangeVersionFlow);
end;

initialization
  {$I ../images/images.lrs}

end.

