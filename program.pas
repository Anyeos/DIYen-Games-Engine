program diyendev;

{$mode objfpc}{$H+}

uses
  diyen, main;

{$R *.res}

procedure AudioPost2(udata: Pointer; stream: PUInt8; len: Integer);
begin

end;


begin
  diyen_main(TProgram.Create);
end.

