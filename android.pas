library diyendev;
{$mode objfpc}{$H+}
{$LINKLIB c}

uses
  log, jni, diyen, main;


function SDL_main(argc: longint; argv: array of PChar) : longint;
begin
  diyen_main(TProgram.Create);
  SDL_main := 0;
end;

exports SDL_main name 'SDL_main';

end.


