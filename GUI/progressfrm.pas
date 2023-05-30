unit Progressfrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, AsyncProgress;

type

  { TProgressFrame }

  TProgressFrame = class(TFrame)
    HeadlineLabel: TLabel;
    LastOutputLabel: TLabel;
    Label2: TLabel;
    OutputReportMemo: TMemo;
    Panel1: TPanel;
    JobProgressBar: TProgressBar;
    StepNumberLabel: TLabel;
    StepNameLabel: TLabel;
    procedure FrameResize(Sender: TObject);
    procedure LastOutputLabelClick(Sender: TObject);
  private
    FOnJobsFinished: TNotifyEvent;
    FOnJobError: TStringMessageEvent;
    FJobs: Array of TAsyncProgress;
    FCurrentJob: Integer;

    procedure JobError(Sender: TObject; const Output: String);
    procedure JobOutput(Sender: TObject; const Output: String);
    procedure JobProgress(Sender: TObject; Progress: Double);
    procedure JobSuccess(Sender: TObject);
    procedure NextJob;     
    procedure KillJobs;
  public
    procedure ExecuteJobs(const TaskName: String;
      const AJobs: array of TAsyncProgress; AOnFinish: TNotifyEvent;
  AOnError: TStringMessageEvent);

    procedure ClearJobs(FreeJobs: Boolean);

    property Jobs: specialize TArray<TAsyncProgress> read FJobs;
    property CurrentJob: Integer read FCurrentJob;
  end;

implementation

{$R *.lfm}

{ TProgressFrame }

procedure TProgressFrame.FrameResize(Sender: TObject);
begin
  if OutputReportMemo.Visible then
    Panel1.Height := ClientHeight - JobProgressBar.Top - JobProgressBar.Height - 32
  else
    Panel1.Height := LastOutputLabel.Height;
  JobProgressBar.Width := ClientWidth - JobProgressBar.Left * 2;
end;

procedure TProgressFrame.LastOutputLabelClick(Sender: TObject);
begin
  OutputReportMemo.Visible := not OutputReportMemo.Visible;
  FrameResize(Self);
end;

procedure TProgressFrame.NextJob;
var
  ToExecute: TAsyncProgress;
begin
  Inc(FCurrentJob);
  if FCurrentJob >= Length(FJobs) then
  begin
    if Assigned(FOnJobsFinished) then
      FOnJobsFinished(Self);
    Exit;
  end;
  JobProgressBar.Position := 0;
  ToExecute := FJobs[FCurrentJob];
  ToExecute.OnError := @JobError;
  ToExecute.OnOutput := @JobOutput;
  ToExecute.OnProgress := @JobProgress;
  ToExecute.OnSuccess := @JobSuccess;
  StepNumberLabel.Caption := '%d/%d'.Format([FCurrentJob + 1, Length(FJobs)]);
  StepNameLabel.Caption := ToExecute.TaskName;
  JobOutput(ToExecute, 'Starting next Job: ' + ToExecute.TaskName);
  ToExecute.Start;
end;

procedure TProgressFrame.JobSuccess(Sender: TObject);
begin
  JobProgressBar.Position := 100;
  JobProgressBar.Refresh;
  NextJob;
end;

procedure TProgressFrame.JobProgress(Sender: TObject; Progress: Double);
begin
  if Progress < 0 then
    JobProgressBar.Style := pbstMarquee
  else
  begin
    JobProgressBar.Style := pbstNormal;    
    JobProgressBar.Position := Round(Progress * 100);
  end;
end;

procedure TProgressFrame.JobOutput(Sender: TObject; const Output: String);
begin
  OutputReportMemo.Lines.Add(Output);
  LastOutputLabel.Caption := Output;
end;

procedure TProgressFrame.JobError(Sender: TObject; const Output: String);
begin
  OutputReportMemo.Lines.Add('Error during Job Execution');
  LastOutputLabel.Caption := 'Error during Job Execution';
  OutputReportMemo.Lines.Add(Output);
  FOnJobError(Self, Output);
end;

procedure TProgressFrame.ExecuteJobs(const TaskName: String;
                              const AJobs: array of TAsyncProgress;
                              AOnFinish: TNotifyEvent;
                              AOnError: TStringMessageEvent);
var
  i: Integer;
begin
  SetLength(FJobs, Length(AJobs));
  for i:=0 to Length(AJobs)-1 do
    FJobs[i] := AJobs[i];
  FOnJobError := AOnError;
  FOnJobsFinished := AOnFinish;
  FCurrentJob := -1;
  OutputReportMemo.Clear;
  LastOutputLabel.Caption := '';
  HeadlineLabel.Caption := TaskName;
  NextJob;
end;

procedure TProgressFrame.ClearJobs(FreeJobs: Boolean);
var
  AJob: TAsyncProgress;
begin
  KillJobs;
  if FreeJobs then
    for AJob in FJobs do
    begin
      if not AJob.Finished then
        raise Exception.Create('Jobs not finished');
      AJob.Free;
    end;
  FJobs := nil;
end;

procedure TProgressFrame.KillJobs;
var
  i: Integer;
  Job: TAsyncProgress;
begin
  for i:=FCurrentJob to Length(FJobs) - 1 do
  begin
    FJobs[i].Terminate;
    if FJobs[i].Suspended then
    begin
      FJobs[i].Start;
    end;
  end;
  for Job in FJobs do
    while not Job.Finished do
    begin
      Application.ProcessMessages;
    end;
end;

end.

