unit UIFlows;

{$mode ObjFPC}{$H+}
{$ModeSwitch arrayoperators}

interface

uses
  Classes, SysUtils, Graphics, Forms, Controls, GitlabFetch, AsyncProgress,
  LazarusPackageManager, Progressfrm, CompareSelectFrm, FileUtil, LazFileUtils;

type
  
  TRebuildMode = (rbmNone, rbmLazarus, rbmAll);

  TShowMessageDelegate = function(const Headline: String;
                                  const Message: String;
                                  MessageColor: TColor = clDefault;
                                  CancelButton: Boolean = False): TModalResult of object;

  TFlowFinishedEvent = procedure(Sender: TObject; RequiresRebuild: TRebuildMode) of Object;

  { TUIFlow }

  TUIFlow = class(TComponent)
  private
    FParent: TWinControl;
    FOnStepActive: TNotifyEvent;
    FOnStepFinished: TNotifyEvent;
    FOnFinished: TFlowFinishedEvent;
    FOnCancel: TNotifyEvent;

    FShowMessageDelegate: TShowMessageDelegate;
    FCurrentView: TFrame;
  protected
    procedure SetupView(View: TFrame);
    procedure SwitchView(NewView: TFrame);
    procedure StepFinished;

    function DoStep: Boolean; virtual; abstract;
    procedure DoCancel; virtual; abstract;
    function Finished: Boolean; virtual; abstract; 
    function RebuildResult: TRebuildMode; virtual; abstract;
    function AutoNextStep: Boolean; virtual;

    function MessageDialog(const Headline: String;
                           const Message: String;
                           MessageColor: TColor = clDefault;
                           CancelButton: Boolean = False): TModalResult;
  public
    constructor Create(AParent: TWinControl;
                       const AOnStepActive: TNotifyEvent;
                       const AOnStepFinished: TNotifyEvent;
                       const AOnFinished: TFlowFinishedEvent;
                       const AOnCancel: TNotifyEvent;
                       const AShowMessageDelegate: TShowMessageDelegate);

    procedure NextStep;
    procedure Cancel;
    procedure Reset; virtual;

    property OnStepActive: TNotifyEvent read FOnStepActive write FOnStepActive;
    property OnStepFinished: TNotifyEvent read FOnStepFinished write FOnStepFinished;
    property OnFinished: TFlowFinishedEvent read FOnFinished write FOnFinished;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;

    property ShowMessageDelegate: TShowMessageDelegate read FShowMessageDelegate write FShowMessageDelegate;
  end;

  { TRebuildLazarusFlow }

  TRebuildLazarusFlow = class(TUIFlow)
  private type
    TFlowStep = (rbsPreStart, rbsRebuild, rbsDone);
  private
    FCurrentStep: TFlowStep;
    FRebuildMode: TRebuildMode;
    FPackageManager: TLazarusPackageManager;

    FProgressFrame: TProgressFrame;

    procedure OnProgressError(Sender: TObject; const Output: String);
    procedure OnProgressFinished(Sender: TObject);

  protected
    function DoStep: Boolean; override;
    procedure DoCancel; override;
    function Finished: Boolean; override;
    function RebuildResult: TRebuildMode; override;

  public
    constructor Create(AParent: TWinControl; const AOnStepActive: TNotifyEvent;
      const AOnStepFinished: TNotifyEvent;
      const AOnFinished: TFlowFinishedEvent; const AOnCancel: TNotifyEvent;
      const AShowMessageDelegate: TShowMessageDelegate);
    destructor Destroy; override;

    procedure SetRebuildMode(NewRebuildMode: TRebuildMode);

    procedure Setup(PackageManager: TLazarusPackageManager);
    procedure Reset; override;

    property RebuildMode: TRebuildMode read FRebuildMode;
  end;

  { TChangeVersionFlow }

  TChangeVersionFlow = class(TUIFlow)
  private type
    TFlowStep = (cvsPreStart, cvsSelectVersion, cvsCreateGit, cvsSwitchBranch, cvsHardReset, cvsDone);

  private
    FCurrentStep: TFlowStep;
    FAllVersions: TBranchVersions;
    FCurrentIndex: Integer;
    FLazarusDir: String;
    FGitPath: String;

    FSelectFrame: TCompareSelectFrame;
    FProgressFrame: TProgressFrame;
    function GetSelectedVersion: TBranchVersion;

    procedure OnProgressError(Sender: TObject; const Output: String);
    procedure OnProgressFinished(Sender: TObject);
                    
    procedure CreateGitStage(const NewVersion: TBranchVersion);
    procedure HardResetStage(const NewVersion: TBranchVersion);
    procedure SwitchBranchStage(const NewVersion: TBranchVersion);

    procedure RefreshList;

  protected
    function DoStep: Boolean; override;
    procedure DoCancel; override;
    function Finished: Boolean; override;
    function RebuildResult: TRebuildMode; override;

  public
    constructor Create(AParent: TWinControl; const AOnStepActive: TNotifyEvent;
      const AOnStepFinished: TNotifyEvent;
      const AOnFinished: TFlowFinishedEvent; const AOnCancel: TNotifyEvent;
      const AShowMessageDelegate: TShowMessageDelegate);
    destructor Destroy; override;

    procedure Setup(const LazarusDir: string; const GitPath: String;
      CurrentVersion: TBuildVersion; AvailableVersions: TBranchVersions);
    procedure Reset; override;

    function OnLatestVersion: Boolean;

    property SelectedVersion: TBranchVersion read GetSelectedVersion;
  end;

  { TChangeLayoutFlow }

  TChangeLayoutFlow = class(TUIFlow)
  private type
    TFlowStep = (clsPreStart, clsSelectLayout, clsUninstall, clsCompilePackages, clsInstall, clsDone);

  private
    FCurrentStep: TFlowStep;
    FPackageManager: TLazarusPackageManager;
    FIsDocked: Boolean;

    FSelectFrame: TCompareSelectFrame;
    FProgressFrame: TProgressFrame;

    procedure OnProgressError(Sender: TObject; const Output: String);
    procedure OnProgressFinished(Sender: TObject);

    function CompileStage(const PackageNames: array of String): Boolean;
    function InstallGlobalPackages(const PackageNames: array of String): Boolean;
  protected
    function DoStep: Boolean; override;
    procedure DoCancel; override;
    function Finished: Boolean; override;
    function RebuildResult: TRebuildMode; override;
    function AutoNextStep: Boolean; override;

  public
    constructor Create(AParent: TWinControl; const AOnStepActive: TNotifyEvent;
      const AOnStepFinished: TNotifyEvent;
      const AOnFinished: TFlowFinishedEvent; const AOnCancel: TNotifyEvent;
      const AShowMessageDelegate: TShowMessageDelegate);
    destructor Destroy; override;

    procedure Setup(PackageManager: TLazarusPackageManager);
    procedure Reset; override;

    property DockedEnabled: Boolean read FIsDocked;
  end;

  TDarkModeStyle = (dmsBright, dmsSystem, dmsDark);

  { TChangeThemeFlow }

  TChangeThemeFlow = class(TUIFlow)
  private type
    TFlowStep = (ctsPreStart, {TODO: Fill me} ctsDone);

  private
    FCurrentStep: TFlowStep;
    FPackageManager: TLazarusPackageManager;
    FDarkMode: TDarkModeStyle;

    FSelectFrame: TCompareSelectFrame;
    FProgressFrame: TProgressFrame;

    procedure OnProgressError(Sender: TObject; const Output: String);
    procedure OnProgressFinished(Sender: TObject);
                                                            
    function DownloadStage: Boolean;
    function CompileStage: Boolean;
    function InstallPackages: Boolean;

    function ReadDarkModeState: TDarkModeStyle;
  protected
    function DoStep: Boolean; override;
    procedure DoCancel; override;
    function Finished: Boolean; override;
    function RebuildResult: TRebuildMode; override;
    function AutoNextStep: Boolean; override;

  public
    constructor Create(AParent: TWinControl; const AOnStepActive: TNotifyEvent;
      const AOnStepFinished: TNotifyEvent;
      const AOnFinished: TFlowFinishedEvent; const AOnCancel: TNotifyEvent;
      const AShowMessageDelegate: TShowMessageDelegate);
    destructor Destroy; override;

    procedure Setup(PackageManager: TLazarusPackageManager);
    procedure Reset; override;

    property DarkModeStyle: TDarkModeStyle read FDarkMode;
  end;

implementation

{ TUIFlow }

procedure TUIFlow.SetupView(View: TFrame);
begin
  View.Parent := FParent;
  View.Align := alClient;
  View.Hide;
end;

procedure TUIFlow.SwitchView(NewView: TFrame);
begin
  if Assigned(FCurrentView) then
    FCurrentView.Hide;
  FCurrentView := NewView;
  NewView.Show;
end;

procedure TUIFlow.StepFinished;
begin
  if Assigned(FOnStepFinished) then
    FOnStepFinished(Self);
end;

function TUIFlow.AutoNextStep: Boolean;
begin
  Result := False;
end;

function TUIFlow.MessageDialog(const Headline: String; const Message: String;
  MessageColor: TColor; CancelButton: Boolean): TModalResult;
begin
  if Assigned(FCurrentView) then
  begin
    FCurrentView.SendToBack;  
    FCurrentView.Enabled := False;
  end;
  Result := FShowMessageDelegate(Headline, Message, MessageColor, CancelButton);
  if Assigned(FCurrentView) then
    FCurrentView.Enabled := True;
end;

constructor TUIFlow.Create(AParent: TWinControl;
  const AOnStepActive: TNotifyEvent; const AOnStepFinished: TNotifyEvent;
  const AOnFinished: TFlowFinishedEvent; const AOnCancel: TNotifyEvent;
  const AShowMessageDelegate: TShowMessageDelegate);
begin
  inherited Create(AParent);
  FParent := AParent;
  FOnStepActive := AOnStepActive;
  FOnStepFinished := AOnStepFinished;
  FOnFinished := AOnFinished;
  FOnCancel := AOnCancel;
  FShowMessageDelegate := AShowMessageDelegate;
end;

procedure TUIFlow.NextStep;
var
  StepDidFinish: Boolean;
begin
  repeat
    if Finished then
    begin   
      if Assigned(FCurrentView) then
        FCurrentView.Hide;
      FCurrentView := nil;
      if Assigned(FOnFinished) then
        FOnFinished(Self, RebuildResult);
      Exit;
    end;

    if Assigned(FOnStepActive) then
      FOnStepActive(Self);
    StepDidFinish := DoStep;
    if StepDidFinish then
      StepFinished;
  until not StepDidFinish or not AutoNextStep;
end;

procedure TUIFlow.Cancel;
begin
  DoCancel;        
  if Assigned(FCurrentView) then
    FCurrentView.Hide;
  FCurrentView := nil;
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

procedure TUIFlow.Reset;
begin
  if Assigned(FCurrentView) then
    FCurrentView.Hide;
  FCurrentView := nil;
end;


{ TRebuildLazarusFlow }

procedure TRebuildLazarusFlow.OnProgressError(Sender: TObject;
  const Output: String);
begin
  MessageDialog('Error Building Lazarus', Output, clRed);
end;

procedure TRebuildLazarusFlow.OnProgressFinished(Sender: TObject);
begin
  FRebuildMode := rbmNone;
  FCurrentStep := rbsDone;
  StepFinished;
end;

function TRebuildLazarusFlow.DoStep: Boolean;
var
  Jobs: array of TAsyncProgress;
begin
  Result := False;
  case FCurrentStep of
  rbsPreStart:
  begin
    Jobs := [];
    if FRebuildMode = rbmAll then
      Jobs := [
        FPackageManager.RebuildLazbuildJob
      ];
    Jobs += [FPackageManager.RebuildLazarusJob];

    SwitchView(FProgressFrame);
    FProgressFrame.ClearJobs(True);
    FProgressFrame.ExecuteJobs('Building Lazarus', Jobs, @OnProgressFinished,
                               @OnProgressError);
    FCurrentStep := rbsRebuild;
  end
  else raise Exception.Create('This should never be reached');
  end;
end;

procedure TRebuildLazarusFlow.DoCancel;
begin
  FProgressFrame.ClearJobs(True);
end;

function TRebuildLazarusFlow.Finished: Boolean;
begin
  Result := (FRebuildMode = rbmNone) or (FCurrentStep = rbsDone);
end;

function TRebuildLazarusFlow.RebuildResult: TRebuildMode;
begin
  Result := rbmNone;
end;

constructor TRebuildLazarusFlow.Create(AParent: TWinControl;
  const AOnStepActive: TNotifyEvent; const AOnStepFinished: TNotifyEvent;
  const AOnFinished: TFlowFinishedEvent; const AOnCancel: TNotifyEvent;
  const AShowMessageDelegate: TShowMessageDelegate);
begin
  inherited Create(AParent, AOnStepActive, AOnStepFinished, AOnFinished, AOnCancel, AShowMessageDelegate);

  FProgressFrame := TProgressFrame.Create(Self);
  SetupView(FProgressFrame);
end;

destructor TRebuildLazarusFlow.Destroy;
begin
  FProgressFrame.ClearJobs(True);
  inherited Destroy;
end;

procedure TRebuildLazarusFlow.SetRebuildMode(NewRebuildMode: TRebuildMode);
begin
  if NewRebuildMode > FRebuildMode then
    FRebuildMode := NewRebuildMode;
end;

procedure TRebuildLazarusFlow.Setup(PackageManager: TLazarusPackageManager);
begin
  FPackageManager := PackageManager;
end;

procedure TRebuildLazarusFlow.Reset;
begin
  FCurrentStep := rbsPreStart;
  FProgressFrame.ClearJobs(True);
  inherited Reset;
end;

{ TChangeVersionFlow }

procedure TChangeVersionFlow.OnProgressError(Sender: TObject;
  const Output: String);
var
  NewVersion: TBranchVersion;
begin
  if (FCurrentStep = cvsCreateGit) and (FProgressFrame.CurrentJob < 4) then
  begin
    MessageDialog('Error creating git repository', 'Git encountered the following error: ' +
                  LineEnding + Output + LineEnding + LineEnding + 'Process will be reverted', clRed);
    DeleteDirectory(IncludeTrailingPathDelimiter(FLazarusDir) + '.git', False);
  end
  else if FCurrentStep = cvsHardReset then
  begin
    MessageDialog('Error resetting branch', 'Git encountered the following error: ' +
                  LineEnding + Output + LineEnding + LineEnding + 'You are on your own', clRed);
  end
  else
  begin
    if MessageDialog('Error switching branch', 'Git encountered the following error: ' +
                     LineEnding + Output + LineEnding + LineEnding +
                     'Trying to reset to new branch (changes will be lost).', clRed, True) = mrOK then
    begin
      NewVersion := FAllVersions[FSelectFrame.SelectedIndex];
      HardResetStage(NewVersion);
    end;
  end;
end;

procedure TChangeVersionFlow.OnProgressFinished(Sender: TObject);
begin
  FCurrentIndex := FSelectFrame.SelectedIndex;
  FCurrentStep := cvsDone;
  StepFinished;
end;

function TChangeVersionFlow.GetSelectedVersion: TBranchVersion;
begin
  Result := FAllVersions[FCurrentIndex];
end;

procedure TChangeVersionFlow.CreateGitStage(const NewVersion: TBranchVersion);
var
  Title: String;
begin
  if NewVersion.Version < SelectedVersion.Version then
    Title := 'Downgrading to ' + NewVersion.Version.ToString
  else
    Title := 'Upgrading to ' + NewVersion.Version.ToString;

  SwitchView(FProgressFrame);
  FProgressFrame.ClearJobs(True);
  FProgressFrame.ExecuteJobs(Title, [
      TAsyncProcessExecution.Create('Initializing git', FGitPath, ['-C' , FLazarusDir, 'init']),
      TAsyncProcessExecution.Create('Setting origin', FGitPath, ['-C' , FLazarusDir, 'remote', 'add', 'origin', LazarusCloneUrl]),
      TAsyncProcessExecution.Create('Fetching Repository', FGitPath, ['-C' , FLazarusDir, 'fetch', '-v', '--all'], @PercentParser),
      TAsyncProcessExecution.Create('Switching branch', FGitPath, ['-C' , FLazarusDir, 'reset', '--hard', NewVersion.BranchName], @PercentParser),
      TAsyncProcessExecution.Create('Switching branch', FGitPath, ['-C' , FLazarusDir, 'checkout', NewVersion.BranchName], @PercentParser)
    ],
    @OnProgressFinished, @OnProgressError
  );
  FCurrentStep := cvsCreateGit;
end;

procedure TChangeVersionFlow.HardResetStage(const NewVersion: TBranchVersion);
begin
  SwitchView(FProgressFrame);
  FProgressFrame.ClearJobs(True);
  FProgressFrame.ExecuteJobs('Resetting Branch', [
      TAsyncProcessExecution.Create('Resetting branch', FGitPath, ['-C' , FLazarusDir, 'reset', '--hard', NewVersion.BranchName], @PercentParser),
      TAsyncProcessExecution.Create('Switching branch', FGitPath, ['-C' , FLazarusDir, 'checkout', NewVersion.BranchName], @PercentParser)
    ],
    @OnProgressFinished, @OnProgressError
  );
  FCurrentStep := cvsHardReset;
end;

procedure TChangeVersionFlow.SwitchBranchStage(const NewVersion: TBranchVersion);
var
  Title: String;
begin
  if NewVersion.Version < SelectedVersion.Version then
    Title := 'Downgrading to ' + NewVersion.Version.ToString
  else
    Title := 'Upgrading to ' + NewVersion.Version.ToString;

  SwitchView(FProgressFrame);
  FProgressFrame.ClearJobs(True);
  FProgressFrame.ExecuteJobs(Title, [
      TAsyncProcessExecution.Create('Fetching Repository', FGitPath, ['-C' , FLazarusDir, 'fetch', '-v', '--all'], @PercentParser),
      TAsyncProcessExecution.Create('Switching branch', FGitPath, ['-C' , FLazarusDir, 'checkout', NewVersion.BranchName], @PercentParser)
    ],
    @OnProgressFinished, @OnProgressError
  );
  FCurrentStep := cvsSwitchBranch;
end;

procedure TChangeVersionFlow.RefreshList;
var
  Options: TCompareOptions;
  i: Integer;
  DetailsStr, NoteStr: String;
begin
  for i:=0 to Length(FAllVersions) - 1 do
    if (FAllVersions[i].BranchName = 'None') and (i <> FCurrentIndex) then
    begin
      Delete(FAllVersions, i, 1);
      if i < FCurrentIndex then
        Dec(FCurrentIndex);
      Break;
    end;
  Options := [];
  SetLength(Options, Length(FAllVersions));
  for i:=0 to Length(FAllVersions) - 1 do
  begin
    if FAllVersions[i].BranchName = 'None' then
      DetailsStr := 'Locally installed version'
    else
      DetailsStr := 'Git branch: ' + FAllVersions[i].BranchName;
    if FAllVersions[i].Version < SelectedVersion.Version then
      NoteStr := 'Downgrade'
    else if SelectedVersion.Version < FAllVersions[i].Version then
      NoteStr := 'Upgrade'
    else
      NoteStr := 'Currently Installed';

    Options[i] := CompareOption('cheetah', FAllVersions[i].Version.ToString,
                                DetailsStr, NoteStr);
  end;
  FSelectFrame.SetItems('Lazarus Version', Options, FCurrentIndex, False);
end;

function TChangeVersionFlow.DoStep: Boolean;
var
  NewVersion: TBranchVersion;
begin
  Result := FCurrentStep = cvsPreStart;
  case FCurrentStep of
  cvsPreStart:
  begin
    SwitchView(FSelectFrame);
    FSelectFrame.CompareList.Items[FCurrentIndex].Checked := True;
    FCurrentStep := cvsSelectVersion;
  end;
  cvsSelectVersion:
  begin
    if not Assigned(FSelectFrame.CompareList.Selected) then
      Exit(True);  // Nothing selected -> Do nothing
    if FSelectFrame.SelectedIndex = FCurrentIndex then
      Exit(True); // Should be covered by Finished

    NewVersion := FAllVersions[FSelectFrame.SelectedIndex];

    if not DirectoryExists(IncludeTrailingPathDelimiter(FLazarusDir) + '.git') then
      if MessageDialog('Not a git Repository',
                       'Your lazarus installation is currently not a git repository. ' +
                       'The updater will attempt to create a git repository, ' +
                       'any changes to your lazarus sources will be lost.' +
                       LineEnding + LineEnding + 'Do you agree?',
                       TColor($0080FF), True) <> mrOK then
        Exit(True)
      else
      begin
        CreateGitStage(NewVersion);
        Exit;
      end;
    SwitchBranchStage(NewVersion);
  end;
  else raise Exception.Create('This should never be reached');
  end;
end;

procedure TChangeVersionFlow.DoCancel;
begin
  if FCurrentStep > cvsSelectVersion then
    FProgressFrame.ClearJobs(True);
end;

function TChangeVersionFlow.Finished: Boolean;
begin
  Result := ((FCurrentStep = cvsSelectVersion) and (FSelectFrame.SelectedIndex = FCurrentIndex))
         or (FCurrentStep = cvsDone);
end;

function TChangeVersionFlow.RebuildResult: TRebuildMode;
begin
  Result := rbmNone;
  if Finished then
    Result := rbmAll;
end;

constructor TChangeVersionFlow.Create(AParent: TWinControl;
  const AOnStepActive: TNotifyEvent; const AOnStepFinished: TNotifyEvent;
  const AOnFinished: TFlowFinishedEvent; const AOnCancel: TNotifyEvent;
  const AShowMessageDelegate: TShowMessageDelegate);
begin
  inherited Create(AParent, AOnStepActive, AOnStepFinished, AOnFinished, AOnCancel, AShowMessageDelegate);

  FProgressFrame := TProgressFrame.Create(Self);
  SetupView(FProgressFrame);
  FSelectFrame := TCompareSelectFrame.Create(Self);
  SetupView(FSelectFrame);
end;

destructor TChangeVersionFlow.Destroy;
begin
  FProgressFrame.ClearJobs(True);
  inherited Destroy;
end;

procedure TChangeVersionFlow.Setup(const LazarusDir: string; const GitPath: String;
  CurrentVersion: TBuildVersion; AvailableVersions: TBranchVersions);
var
  LocalVersion: TBranchVersion;
begin            
  FLazarusDir := LazarusDir;
  FGitPath := GitPath;
  FCurrentStep := cvsPreStart;

  FAllVersions := AvailableVersions;

  LocalVersion := Default(TBranchVersion);
  LocalVersion.BranchName := 'None';
  LocalVersion.Version := CurrentVersion;
  FCurrentIndex := FindOrInsertSorted(LocalVersion, FAllVersions);
end;

procedure TChangeVersionFlow.Reset;
begin
  RefreshList;
  FCurrentStep := cvsPreStart;
  FProgressFrame.ClearJobs(True);
  inherited Reset;
end;

function TChangeVersionFlow.OnLatestVersion: Boolean;
begin
  Result := FCurrentIndex < 0;
end;


{ TChangeLayoutFlow }

procedure TChangeLayoutFlow.OnProgressError(Sender: TObject;
  const Output: String);
begin
  MessageDialog('Error building packages', Output, clRed);
end;

procedure TChangeLayoutFlow.OnProgressFinished(Sender: TObject);
begin
  StepFinished;
end;

function TChangeLayoutFlow.CompileStage(const PackageNames: Array of String): Boolean;
var
  Packages: array of TPackageInfo;
  Jobs: array of TAsyncProgress;
  i: Integer;
begin
  Result := False;
  Jobs := [];
  Packages := [];
  SetLength(Packages, Length(PackageNames));
  for i:=0 to Length(PackageNames) - 1 do
    if not FPackageManager.Packages.TryGetValue(PackageNames[i].ToLower, Packages[i]) then
    begin
      MessageDialog('Package not found', 'Global package ' +
                    PackageNames[i] + ' was not found in Lazarus global links', clRed);
      Exit;
    end;
  SetLength(Jobs, Length(Packages));
  for i:=0 to Length(Packages) - 1 do
    Jobs[i] := FPackageManager.CompilePackageJob(Packages[i]);

  SwitchView(FProgressFrame);
  FProgressFrame.ClearJobs(True);
  FProgressFrame.ExecuteJobs('Building Packages', Jobs, @OnProgressFinished, @OnProgressError);
  FCurrentStep := clsCompilePackages;
  Result := True;
end;

function TChangeLayoutFlow.InstallGlobalPackages(
  const PackageNames: array of String): Boolean;
var
  Packages: array of TPackageInfo;
  i: Integer;
begin
  Result := False;
  Packages := [];
  SetLength(Packages, Length(PackageNames));
  for i:=0 to Length(PackageNames) - 1 do
    if not FPackageManager.Packages.TryGetValue(PackageNames[i].ToLower, Packages[i]) then
    begin
      MessageDialog('Package not found', 'Global package ' +
                    PackageNames[i] + ' was not found in Lazarus global links', clRed);
      Exit;
    end;
  for i:=0 to Length(Packages) - 1 do
  begin
    if not Packages[i].Registered then
      FPackageManager.RegisterPackage(Packages[i].PackageFile);
    if Packages[i].Installable and not Packages[i].Installed then
      FPackageManager.InstallPackage(Packages[i].PackageFile);
  end;
  Result := True;
end;

function TChangeLayoutFlow.DoStep: Boolean;
begin
  Result := True;
  case FCurrentStep of
  clsPreStart:
  begin
    SwitchView(FSelectFrame);
    FSelectFrame.CompareList.Items[ord(FIsDocked)].Checked := True;
    FCurrentStep := clsSelectLayout;
  end;
  clsSelectLayout:
  begin
    if not Assigned(FSelectFrame.CompareList.Selected) then
      Exit;  // Nothing selected -> Do nothing
    if FSelectFrame.SelectedIndex = ord(FIsDocked) then
      Exit; // Should be covered by Finished

    if FIsDocked then // Remove docking
      FCurrentStep := clsUninstall
    else
      Result := CompileStage(['anchordocking', 'anchordockingdsgn', 'dockedformeditor']);
  end;
  clsUninstall:
  begin
    FPackageManager.UninstallPackage('anchordockingdsgn');
    FPackageManager.UninstallPackage('dockedformeditor');
    FIsDocked := False;
    FCurrentStep := clsDone;
  end;
  clsCompilePackages: FCurrentStep := clsInstall;
  clsInstall:
  begin
    Result := InstallGlobalPackages(['anchordocking', 'anchordockingdsgn', 'dockedformeditor']);
    FIsDocked := True;
    FCurrentStep := clsDone;
  end;
  else raise Exception.Create('This should never be reached');
  end;
end;

procedure TChangeLayoutFlow.DoCancel;
begin
  FProgressFrame.ClearJobs(True);
end;

function TChangeLayoutFlow.Finished: Boolean;
begin
  Result := ((FCurrentStep = clsSelectLayout) and (FSelectFrame.SelectedIndex = Ord(FIsDocked)))
         or (FCurrentStep = clsDone);
end;

function TChangeLayoutFlow.RebuildResult: TRebuildMode;
begin
  Result := rbmNone;
  if FCurrentStep = clsDone then
    Result := rbmLazarus;
end;

function TChangeLayoutFlow.AutoNextStep: Boolean;
begin
  Result := FCurrentStep in [clsUninstall, clsInstall, clsDone];
end;

constructor TChangeLayoutFlow.Create(AParent: TWinControl;
  const AOnStepActive: TNotifyEvent; const AOnStepFinished: TNotifyEvent;
  const AOnFinished: TFlowFinishedEvent; const AOnCancel: TNotifyEvent;
  const AShowMessageDelegate: TShowMessageDelegate);
begin
  inherited Create(AParent, AOnStepActive, AOnStepFinished, AOnFinished, AOnCancel, AShowMessageDelegate);

  FProgressFrame := TProgressFrame.Create(Self);
  SetupView(FProgressFrame);
  FSelectFrame := TCompareSelectFrame.Create(Self);
  SetupView(FSelectFrame);

  FSelectFrame.SetItems('IDE Layout', [
    CompareOption('Floating', 'Floating IDE', 'IDE consisting of seperate floating windows'),
    CompareOption('Docked', 'Docked IDE', 'IDE consists of only one window with different docked sections')
  ]);
end;

destructor TChangeLayoutFlow.Destroy;
begin
  FProgressFrame.ClearJobs(True);
  inherited Destroy;
end;

procedure TChangeLayoutFlow.Setup(PackageManager: TLazarusPackageManager);
var
  Package: TPackageInfo;
begin
  FPackageManager := PackageManager;
  if FPackageManager.Packages.TryGetValue('anchordockingdsgn', Package) then
    FIsDocked := Package.Installed
  else
    FIsDocked := False;
  FCurrentStep := clsPreStart;
end;

procedure TChangeLayoutFlow.Reset;
begin
  FProgressFrame.ClearJobs(True);
  FCurrentStep := clsPreStart;
  inherited Reset;
end; 

{ TChangeThemeFlow }

procedure TChangeThemeFlow.OnProgressError(Sender: TObject; const Output: String
  );
begin

end;

procedure TChangeThemeFlow.OnProgressFinished(Sender: TObject);
begin

end;

function TChangeThemeFlow.DownloadStage: Boolean;
begin
  Result := True;
end;

function TChangeThemeFlow.CompileStage: Boolean;
begin
  Result := True;
end;

function TChangeThemeFlow.InstallPackages: Boolean;
begin
  Result := True;
end;

function TChangeThemeFlow.ReadDarkModeState: TDarkModeStyle;
var
  Package: TPackageInfo;
  DsgnFile, FileContents: String;
begin
  Result := dmsBright;
  if not FPackageManager.Packages.TryGetValue('metadarkstyledsgn', Package) then
    Exit;
  if not Package.Installed then
    Exit;
  DsgnFile := ExtractFilePath(Package.PackageFile) + 'src' + PathDelim + 'registermetadarkstyledsgn.pas';
  if not FileExists(DsgnFile) then
    Exit;
  FileContents := ReadFileToString(DsgnFile);
  if FileContents.Contains('AppModeOpt2PreferredAppMode(MetaDarkStyleDSGNOpt.AppMode)') then
    Result := dmsSystem
  else if FileContents.Contains('AppModeOpt2PreferredAppMode(amOptForceDark)') then
    Result := dmsDark;
end;

function TChangeThemeFlow.DoStep: Boolean;
begin
  Result := True;
end;

procedure TChangeThemeFlow.DoCancel;
begin

end;

function TChangeThemeFlow.Finished: Boolean;
begin
  Result := True;
end;

function TChangeThemeFlow.RebuildResult: TRebuildMode;
begin
  Result := rbmNone;
end;

function TChangeThemeFlow.AutoNextStep: Boolean;
begin
  Result := True;
end;

constructor TChangeThemeFlow.Create(AParent: TWinControl;
  const AOnStepActive: TNotifyEvent; const AOnStepFinished: TNotifyEvent;
  const AOnFinished: TFlowFinishedEvent; const AOnCancel: TNotifyEvent;
  const AShowMessageDelegate: TShowMessageDelegate);
begin
  inherited Create(AParent, AOnStepActive, AOnStepFinished, AOnFinished, AOnCancel, AShowMessageDelegate);

  FProgressFrame := TProgressFrame.Create(Self);
  SetupView(FProgressFrame);
  FSelectFrame := TCompareSelectFrame.Create(Self);
  SetupView(FSelectFrame);

  FSelectFrame.SetItems('IDE Theme', [
    CompareOption('Docked', 'Bright Theme', 'Default bright theme', '', 'BrightTheme'),
    CompareOption('DockedBrightDark', 'System Default', 'Use the theme configured in Windows color settings', 'Requires metadarkstyle package', 'DarkBright'),
    CompareOption('DockedDark', 'DarkTheme', 'Always uses the dark theme', 'Requires metadarkstyle package', 'DarkTheme')
  ]);

end;

destructor TChangeThemeFlow.Destroy;
begin
  FProgressFrame.ClearJobs(True);
  inherited Destroy;
end;

procedure TChangeThemeFlow.Setup(PackageManager: TLazarusPackageManager);
begin
  FPackageManager := PackageManager;
  FDarkMode := ReadDarkmodeState;
end;

procedure TChangeThemeFlow.Reset;
begin
  inherited Reset;
end;

end.

