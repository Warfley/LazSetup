program SetupWizzard;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset   
  uDarkStyleParams, uMetaDarkStyle, uDarkStyleSchemes, Forms, SetupMainFrm,
  CompareSelectFrm, DetailedList, Progressfrm, AsyncProgress, GitlabFetch,
  LazarusPackageManager, UIFlows
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  PreferredAppMode:=pamAllowDark;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TSetupMainForm, SetupMainForm);
  Application.Run;
end.

