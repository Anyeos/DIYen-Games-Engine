unit diyenzhelper;
{$mode objfpc}{$H+}

{** Parte de DIYen **}
{
  Este es un proceso que se queda ejecutándose siempre para ayudar a dibujar
  los elementos que no son procesos pero usan Z.

  FIXME: No se puede usar porque let_me_alone lo elimina y no se puedo volver
         a crear desde la función let_me_alone (produce un error).

  Por ahora dejemos que el programador tenga al menos un proceso con Z lo
  suficientemente alto.
}

interface

uses
  Classes, SysUtils, diyen;

type
  TdiyenZHelper = class(TProcess)
    procedure start; override;
    procedure main; override;
    function process_type: word; override;
  end;



implementation

function TdiyenZHelper.process_type: word;
begin
  Result := PTYPE_HELPER;
end;
procedure TdiyenZHelper.start;
begin
  z := 32767;
  graph := 0;
end;
procedure TdiyenZHelper.main;
begin
end;


end.

