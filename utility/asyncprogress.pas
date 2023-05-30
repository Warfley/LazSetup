unit AsyncProgress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, fphttpclient, process, Zipper, FileUtil;

type
  EThreadTerminated = class(Exception);

  TProgressEvent = procedure(Sender: TObject; Progress: Double) of object;
  TStringMessageEvent = procedure(Sender: TObject; const Output: String) of object;
  TOutputHandler = function(const Output: String; PreviousProgress: Double): Double;
  TOutputHandlerMethod = function(const Output: String; PreviousProgress: Double): Double of object;

  { TAsyncProgress }

  TAsyncProgress = class(TThread)
  private type
    PProgressData = ^TProgressData;
    TProgressData = record
      Sender: TObject;
      Progress: Double;
      Event: TProgressEvent;
    end;

    PStringMessageData = ^TStringMessageData;
    TStringMessageData = record
      Sender: TObject;
      Message: String;
      Event: TStringMessageEvent;
    end;

    PNotifyData = ^TNotifyData;
    TNotifyData = record
      Sender: TObject;
      Message: String;
      Event: TNotifyEvent;
    end;
  private
    FOnProgress: TProgressEvent;
    FOnOutput: TStringMessageEvent;
    FOnError: TStringMessageEvent;
    FOnSuccess: TNotifyEvent;
    FTaskName: String;

    procedure MainThreadProgress(Data: IntPtr);
    procedure MainThreadStringMessage(Data: IntPtr);
    procedure MainThreadNotify(Data: IntPtr);
  protected
    procedure Success; virtual;
    procedure Error(const AMessage: String);
    procedure Progress(Progress: Double); virtual;
    procedure Output(const AOutput: String);
  public
    constructor Create(const ATaskName: String);

    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnOutput: TStringMessageEvent read FOnOutput write FOnOutput;
    property OnError: TStringMessageEvent read FOnError write FOnError;
    property OnSuccess: TNotifyEvent read FOnSuccess write FOnSuccess;
    property TaskName: String read FTaskName;
  end;

  { TAsyncDownload }

  TAsyncDownload = class(TAsyncProgress)
  private
    FURL: String;
    FTarget: String;
    FGuessedSize: SizeInt;
    procedure DataReceived(Sender: TObject; const ContentLength,
      CurrentPos: Int64);
  protected
    procedure Execute; override;
  public
    constructor Create(const ATaskName: String; const AURL: String;
      const ATarget: String; AGuessedSize: SizeInt);
  end;

  { TAsyncProcessExecution }

  TAsyncProcessExecution = class(TAsyncProgress)
  private
    FExecutable: String;
    FArguments: array of String;
    FOutputParser: TOutputHandler;
    FOutputParserMethod: TOutputHandlerMethod;
    FEnvironment: TStringList;

    function ParseOutput(const OutputData: String; LastProgress: Double): Double;
  protected
    procedure Execute; override;
  public                      
    constructor Create(const ATaskName: String; const AExecutable:
                       String; const AArguments: array of String);
    constructor Create(const ATaskName: String; const AExecutable:
                       String; const AArguments: array of String;
                       const AOutputParser: TOutputHandler);
    constructor Create(const ATaskName: String; const AExecutable:
                       String; const AArguments: array of String;
                       const AOutputParser: TOutputHandlerMethod);

    destructor Destroy; override;

    procedure AddToPath(const NewPath: String);

    property Environment: TStringList read FEnvironment;
  end;

  { TAsyncUnzip }

  TAsyncUnzip = class(TAsyncProgress)
  private
    FZipArchive: String;
    FTargetDir: String;
    procedure ZipProgress(Sender: TObject; const ATotPos, ATotSize: Int64);
  protected
    procedure Execute; override;
  public
    constructor Create(const ATaskName: String; const AZipArchive: String;
                       const ATargetDir: String);
  end;

  { TAsyncZipCleanup }

  TAsyncZipCleanup = class(TAsyncProgress)
  private
    FZipArchive: String;
    FTargetDir: String;
  protected
    procedure Execute; override;
  public
    constructor Create(const ATaskName: String; const AZipArchive: String;
                       const ATargetDir: String);
  end;

function PercentParser(const AOutput: String; LastProgress: Double): Double;
implementation

function PercentParser(const AOutput: String; LastProgress: Double): Double;
var
  LastPercent, PercentStart: SizeInt;
  Progress: LongInt;
begin
  LastPercent := AOutput.LastIndexOf('%') + 1;
  if LastPercent = 0 then
    Exit(LastProgress);
  PercentStart := LastPercent - 1;
  while (PercentStart > 0) and (AOutput[PercentStart] in ['0'..'9']) do
    Dec(PercentStart);
  Progress := StrToInt(AOutput.Substring(PercentStart, LastPercent-PercentStart-1));
  Result := Progress / 100;
end;

{ TAsyncProgress }

procedure TAsyncProgress.MainThreadProgress(Data: IntPtr);
var
  ProgressData: PProgressData;
begin
  ProgressData := PProgressData(Data);
  if Assigned(ProgressData^.Event) then
    ProgressData^.Event(ProgressData^.Sender, ProgressData^.Progress);
  Dispose(ProgressData);
end;

procedure TAsyncProgress.MainThreadStringMessage(Data: IntPtr);
var
  MessageData: PStringMessageData;
begin
  MessageData := PStringMessageData(Data);
  if Assigned(MessageData^.Event) then
    MessageData^.Event(MessageData^.Sender, MessageData^.Message);  
  Dispose(MessageData);
end;

procedure TAsyncProgress.MainThreadNotify(Data: IntPtr);
var
  Notification: PNotifyData;
begin
  Notification := PNotifyData(Data);
  if Assigned(Notification^.Event) then
    Notification^.Event(Notification^.Sender);
  Dispose(Notification);
end;

procedure TAsyncProgress.Success;
var
  Data: PNotifyData;
begin
  if not Assigned(FOnSuccess) then
    Exit;
  New(Data);
  Data^.Sender := Self;
  Data^.Event := FOnSuccess;
  Application.QueueAsyncCall(@MainThreadNotify, IntPtr(Data));
end;

procedure TAsyncProgress.Error(const AMessage: String);
var
  Data: PStringMessageData;
begin
  if not Assigned(FOnError) then
    Exit;
  New(Data);
  Data^.Sender := Self;
  Data^.Event := FOnError;
  Data^.Message := AMessage;
  UniqueString(Data^.Message);
  Application.QueueAsyncCall(@MainThreadStringMessage, IntPtr(Data));
end;

procedure TAsyncProgress.Progress(Progress: Double);
var
  Data: PProgressData;
begin
  if not Assigned(FOnProgress) then
    Exit;
  New(Data);
  Data^.Sender := Self;
  Data^.Event := FOnProgress;
  Data^.Progress := Progress;
  Application.QueueAsyncCall(@MainThreadProgress, IntPtr(Data));
end;

procedure TAsyncProgress.Output(const AOutput: String);
var
  Data: PStringMessageData;
begin
  if not Assigned(FOnOutput) then
    Exit;
  New(Data);
  Data^.Sender := Self;
  Data^.Event := FOnOutput;
  Data^.Message := AOutput;
  UniqueString(Data^.Message);
  Application.QueueAsyncCall(@MainThreadStringMessage, IntPtr(Data));
end;

constructor TAsyncProgress.Create(const ATaskName: String);
begin
  inherited Create(True);
  FTaskName := ATaskName;
  UniqueString(FTaskName);
end;  

{ TAsyncDownload }

procedure TAsyncDownload.DataReceived(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
begin
  if Terminated then
    TFPHTTPClient(Sender).Terminate;
  if ContentLength >= CurrentPos then
    Progress(CurrentPos/ContentLength)
  else if FGuessedSize >= CurrentPos then
    Progress(CurrentPos/FGuessedSize)
  else
    Progress(-1);
end;

procedure TAsyncDownload.Execute;
var
  http: TFPHTTPClient;
begin
  if Terminated then
    Exit;
  http := TFPHTTPClient.Create(nil);
  try
    http.OnDataReceived :=@DataReceived;
    http.AllowRedirect := True;
    try
      ForceDirectories(ExtractFileDir(FTarget));
      Output('Start downloading ' + FURL);
      http.Get(FURL, FTarget);
    except on E: Exception do
      Error(E.Message);
    end;
  finally
    http.Free;
  end;
  Output('Download successful to ' + FTarget);
  Success;
end;

constructor TAsyncDownload.Create(const ATaskName: String; const AURL: String; const ATarget: String;
  AGuessedSize: SizeInt);
begin
  inherited Create(ATaskName);
  FURL := AURL;
  UniqueString(FURL);
  FTarget := ATarget;
  UniqueString(FTarget);
  FGuessedSize := AGuessedSize;
end;

{ TAsyncProcessExecution }

function TAsyncProcessExecution.ParseOutput(const OutputData: String;
  LastProgress: Double): Double;
begin
  Result := LastProgress;
  if Assigned(FOutputParser) then
    Result := FOutputParser(OutputData, LastProgress)
  else if Assigned(FOutputParserMethod) then
    Result := FOutputParserMethod(OutputData, LastProgress);
end;

procedure TAsyncProcessExecution.Execute;
var
  Proc: TProcess;
  buff: String = '';
  GitOut: String = '';
  Arg: String;
  LastProgress: Double;
begin  
  if Terminated then
    Exit;
  LastProgress := -1;
  Proc := TProcess.Create(nil);
  try
    proc.Executable := FExecutable;
    Proc.Environment.Assign(FEnvironment);
    for Arg in FArguments do
      Proc.Parameters.Add(Arg);
    Proc.Parameters.Delimiter := ' ';
    Output('Executing %s %s'.Format([Proc.Executable, Proc.Parameters.DelimitedText]));
    Proc.Options := Proc.Options + [poUsePipes, poNoConsole];

    Proc.Execute;
    while Proc.Running do
    begin
      if Terminated then
      begin
        Proc.Terminate(1);
        Exit;
      end;
      if Proc.Output.NumBytesAvailable > 0 then
      begin
        SetLength(buff, Proc.Output.NumBytesAvailable);
        Proc.Output.Read(buff[1], buff.Length);
        LastProgress := ParseOutput(buff, LastProgress);
        Progress(LastProgress);
        Output(buff);
        GitOut += buff;
      end
      else if Proc.Stderr.NumBytesAvailable > 0 then
      begin
        SetLength(buff, Proc.Stderr.NumBytesAvailable);
        Proc.Stderr.Read(buff[1], buff.Length);
        LastProgress := ParseOutput(buff, LastProgress);
        Progress(LastProgress);
        Output(buff); 
        GitOut += buff;
      end
      else
        Sleep(10);
    end;
    // Fetch last output from pipes
    if Proc.Output.NumBytesAvailable > 0 then
    begin
      SetLength(buff, Proc.Output.NumBytesAvailable);
      Proc.Output.Read(buff[1], buff.Length);
      LastProgress := ParseOutput(buff, LastProgress);
      Progress(LastProgress);
      Output(buff);
      GitOut += buff;
    end;
    if Proc.Stderr.NumBytesAvailable > 0 then
    begin
      SetLength(buff, Proc.Stderr.NumBytesAvailable);
      Proc.Stderr.Read(buff[1], buff.Length);
      LastProgress := ParseOutput(buff, LastProgress);
      Progress(LastProgress);
      Output(buff);
      GitOut += buff;
    end;
    if Proc.ExitCode = 0 then
    begin
      Progress(1);
      Success;
    end
    else
      Error(GitOut);
  finally
    Proc.Free;
  end;
end;

constructor TAsyncProcessExecution.Create(const ATaskName: String;
  const AExecutable: String; const AArguments: array of String);
var
  i: Integer;
  Env: String;
begin
  Inherited Create(ATaskName);
  FEnvironment := TStringList.Create;
  for i := 1 to GetEnvironmentVariableCount do
  begin
    Env := GetEnvironmentString(i);
    // Not sure if GetEnvironmentString may be shared, better to unique it
    UniqueString(Env);
    FEnvironment.Add(Env);
  end;
  FExecutable := AExecutable;
  UniqueString(FExecutable);
  SetLength(FArguments, Length(AArguments));
  for i:=0 to Length(AArguments) - 1 do
  begin
    FArguments[i] := AArguments[i];
    UniqueString(FArguments[i]);
  end;
end;

constructor TAsyncProcessExecution.Create(const ATaskName: String;
  const AExecutable: String; const AArguments: array of String;
  const AOutputParser: TOutputHandler);
begin
  Create(ATaskName, AExecutable, AArguments);
  FOutputParser := AOutputParser;
end;

constructor TAsyncProcessExecution.Create(const ATaskName: String;
  const AExecutable: String; const AArguments: array of String;
  const AOutputParser: TOutputHandlerMethod);
begin
  Create(ATaskName, AExecutable, AArguments);
  FOutputParserMethod := AOutputParser;
end;

destructor TAsyncProcessExecution.Destroy;
begin
  FEnvironment.Free;
  inherited Destroy;
end;

procedure TAsyncProcessExecution.AddToPath(const NewPath: String);
begin
  // Add to front of PATH so it has highest priority
  {$IfDef WINDOWS}
  FEnvironment.Values['Path'] := NewPath + ';' + FEnvironment.Values['Path'];
  {$Else}
  FEnvironment.Values['PATH'] := NewPath + ':' + FEnvironment.Values['PATH'];
  {$EndIf}
end;

{ TAsyncUnzip }

procedure TAsyncUnzip.ZipProgress(Sender: TObject; const ATotPos,
  ATotSize: Int64);
begin
  if Terminated then
    TUnZipper(Sender).Terminate;
  Progress(ATotPos / ATotSize);
end;

procedure TAsyncUnzip.Execute;
var
  ZipFile: TUnZipper;
begin    
  if Terminated then
    Exit;
  try
    ForceDirectories(FTargetDir);
    ZipFile := TUnZipper.Create;
    try
      Output('Unzipping ' + FZipArchive + ' to ' + FTargetDir);
      ZipFile.OutputPath := FTargetDir;
      ZipFile.FileName := FZipArchive;
      ZipFile.OnProgressEx :=@ZipProgress;
      ZipFile.UnZipAllFiles;
    finally
      ZipFile.Free;
    end;
  except on E: Exception do
    Error(E.Message);
  end;
  Output('All files unzipped');
  Success;
end;

constructor TAsyncUnzip.Create(const ATaskName: String;
  const AZipArchive: String; const ATargetDir: String);
begin
  inherited Create(ATaskName);
  FZipArchive := AZipArchive;
  UniqueString(FTargetDir);
  FTargetDir := ATargetDir;
  UniqueString(FTargetDir);
end;  

{ TAsyncZipCleanup }

procedure TAsyncZipCleanup.Execute;
var
  ZipFile: TUnZipper;
  SubDirName, Path, RelFileName, NewFileName: String;
  ToMove: TStringList;
  i: Integer;
begin
  if Terminated then
    Exit;
  try
    // gitlab zip files have a root directory that has the name of the commit
    // and a checksum. First we need to move all the contents from that directoy
    // to the target directory
    ZipFile := TUnZipper.Create;
    try
      ZipFile.FileName := FZipArchive;
      ZipFile.Examine;
      SubDirName := ZipFile.Entries[0].ArchiveFileName;
      SetLength(SubDirName, SubDirName.Length - 1);
    finally
      ZipFile.Free;
    end;
    Output('Moving extracted files to target directory');
    ToMove := TStringList.Create;
    try
      FindAllDirectories(ToMove, IncludeTrailingPathDelimiter(FTargetDir) + SubDirName, False);
      FindAllFiles(ToMove, IncludeTrailingPathDelimiter(FTargetDir) + SubDirName, '*', False);
      for i:=0 to ToMove.Count - 1 do
      begin
        RelFileName := ExtractRelativePath(IncludeTrailingPathDelimiter(FTargetDir) + SubDirName, ToMove[i]);
        NewFileName := IncludeTrailingPathDelimiter(FTargetDir) + RelFileName.Substring(SubDirName.Length + 1);
        RenameFile(ToMove[i], NewFileName);
        Progress((i + 1) / (ToMove.Count + 1));
      end;
    finally
      ToMove.Free;
    end;
    Output('Removing archive and extraction artifacts');
    DeleteFile(FZipArchive);
    RmDir(IncludeTrailingPathDelimiter(FTargetDir) + SubDirName);
  except on E: Exception do
    Error(E.Message);
  end;
  Success;
end;

constructor TAsyncZipCleanup.Create(const ATaskName: String;
  const AZipArchive: String; const ATargetDir: String);
begin
  inherited Create(ATaskName);
  FZipArchive := AZipArchive;
  UniqueString(FTargetDir);
  FTargetDir := ATargetDir;
  UniqueString(FTargetDir);
end;

end.

