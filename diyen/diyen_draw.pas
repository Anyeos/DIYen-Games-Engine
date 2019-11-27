(*
  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

*)

unit diyen_draw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  SDL2;

const
  DIYEN_LMAX_DRAWINGS     = 128;

type

  Tdrawtype = (DRAW_LINE := 1,
              DRAW_RECTANGLE,
              DRAW_RECTANGLE_FILL,
              DRAW_POINT);

  PDraw = ^TDraw;
  TDraw = class
      dtype : Tdrawtype; // tipo de primitiva que se analizará para dibujar

      x0, y0  : integer;
      xw, yh  : integer;
      px, py: PInteger;
      pw, ph: PInteger;

      alpha     : byte;
      color     : TSDL_Color;
      blend     : TSDL_BlendMode;

      visible   : Boolean;
      procedure Draw;
  end;


  // Lista de primitivas gráficas
  TdiyenDrawList = class
  private
    vi_uborrado: smallint;
    vu_borrados: array of byte;

    function Read( index: byte ): TDraw;
  public
    vmax: smallint;

    drawing: array of PDraw;

    constructor Create;
    destructor Destroy; override;

    function draw( cdtype: Tdrawtype; cx0, cy0: integer; cxw, cyh: integer; ccolor: TSDL_Color; calpha: byte; cblend: TSDL_BlendMode ): byte;
    function add_draw( drawp: PDraw ): byte;
    procedure del_draw( drawid: byte );

    property Item[index: byte]: TDraw read Read; default;
  end;


  // Funciones tipo DIV
  function TD( drawid: byte ): TDraw;
  function draw( dtype: Tdrawtype; x0, y0: integer; Xw, Yh: integer; color: TSDL_Color; alpha: byte; blend: TSDL_BlendMode ): byte;
  function draw_int( dtype: Tdrawtype; x0, y0: PInteger; Xw, Yh: integer; color: TSDL_Color; alpha: byte; blend: TSDL_BlendMode ): byte;
  procedure move_draw( drawid: byte; x, y: integer );
  procedure delete_draw( drawid: byte );

var
  diyen_drawings_list: TdiyenDrawList;

implementation
uses diyen;



procedure TDraw.Draw;
var
  Rect: TSDL_Rect;
begin
  if not visible then Exit;
  SDL_SetRenderDrawColor(gInternal.renderer, color.r, color.g, color.b, alpha);
  SDL_SetRenderDrawBlendMode(gInternal.renderer, blend);

  case dtype of
  DRAW_LINE: SDL_RenderDrawLine(gInternal.renderer, px^, py^, pw^, ph^);
  DRAW_RECTANGLE:
    begin
      Rect.x := px^;
      Rect.y := py^;
      Rect.w := pw^;
      Rect.h := ph^;
      SDL_RenderDrawRect(gInternal.renderer, @Rect);
    end;
  DRAW_RECTANGLE_FILL:
    begin
      Rect.x := px^;
      Rect.y := py^;
      Rect.w := pw^;
      Rect.h := ph^;
      SDL_RenderFillRect(gInternal.renderer, @Rect);
    end;
  otherwise
    SDL_RenderDrawPoint(gInternal.renderer, x0, y0);
  end;
end;

constructor TdiyenDrawList.Create();
var
  i : byte;
begin
  SetLength(drawing, DIYEN_LMAX_DRAWINGS+1);
  SetLength(vu_borrados, DIYEN_LMAX_DRAWINGS+1);
  for i := 0 to DIYEN_LMAX_DRAWINGS do drawing[i] := nil;
  vmax := 0;
  vi_uborrado := 0;
end;
destructor TdiyenDrawList.Destroy();
var
  i : byte;
begin
  for i:=vmax downto 0 do
    if (TDraw(drawing[i]) <> nil) then TDraw(drawing[i]).Destroy;
  SetLength(drawing, 0);
  SetLength(vu_borrados, 0);
end;

function TdiyenDrawList.Read( index: byte ): TDraw;
begin
  Result := TDraw(drawing[index]);
end;


function TdiyenDrawList.add_draw( drawp: PDraw ): byte;
var
  iborrado : byte;
begin
  if vi_uborrado > 0 then
  begin
    // Cual esta borrado?
    iborrado := vu_borrados[vi_uborrado];
    Dec(vi_uborrado); // Ya no esta mas borrado
    drawing[iborrado] := drawp;
    Result := iborrado; // Este es el indice
  end else
  begin
    Inc(vmax);
    drawing[vmax] := drawp;
    Result := vmax;
  end;
end;

procedure TdiyenDrawList.del_draw( drawid: byte );
var
  i : byte;
begin
  if ( drawid > 0 ) then
  begin
    if (TDraw(drawing[drawid]) <> nil) then TDraw(drawing[drawid]).Destroy;
    drawing[drawid] := nil;

    // Era el ultimo?
    if drawid = vmax then
    begin
      // Entonces ahora ya no (esta borrado)
      Dec(vmax);
    end else
    begin // No era el ultimo
      Inc(vi_uborrado); // Entonces se agrega a los borrados
      vu_borrados[vi_uborrado] := drawid; // Y fue este
    end;
  end else
  // Borrar todos (drawid := 0)
  begin
    for i := vmax downto 0 do
    begin
      if (TDraw(drawing[i]) <> nil) then TDraw(drawing[i]).Destroy;
      drawing[i] := nil;
    end;
    vmax := 0;
    vi_uborrado := 0;
  end;
end;

function TdiyenDrawList.draw(
  cdtype: Tdrawtype;
  cx0, cy0: integer;
  cxw, cyh: integer;
  ccolor: TSDL_Color;
  calpha: byte;
  cblend: TSDL_BlendMode
  ): byte;
var
  vdraw : TDraw;
begin
  vdraw := TDraw.Create;
  vdraw.dtype := cdtype;
  vdraw.x0 := cx0; vdraw.px := @vdraw.x0;
  vdraw.y0 := cy0; vdraw.py := @vdraw.y0;
  vdraw.xw := cxw; vdraw.pw := @vdraw.xw;
  vdraw.yh := cyh; vdraw.ph := @vdraw.yh;
  vdraw.color := ccolor;
  vdraw.alpha := calpha;
  vdraw.blend := cblend;
  vdraw.visible := true;

  Result := add_draw(PDraw(vdraw));
end;





function draw( dtype: Tdrawtype; x0, y0: integer; Xw, Yh: integer; color: TSDL_Color; alpha: byte; blend: TSDL_BlendMode ): byte;
begin
  Result := diyen_drawings_list.draw(dtype, x0, y0, Xw, Yh, color, alpha, blend);
end;
function draw_int( dtype: Tdrawtype; x0, y0: PInteger; Xw, Yh: integer; color: TSDL_Color; alpha: byte; blend: TSDL_BlendMode ): byte;
var
  vdraw : TDraw;
begin
  Result := diyen_drawings_list.draw(dtype, 0, 0, Xw, Yh, color, alpha, blend);
  vdraw := TDraw(diyen_drawings_list[Result]);
  vdraw.px:=x0; vdraw.py:=y0;
end;


procedure move_draw( drawid: byte; x, y: integer );
begin
  TDraw(diyen_drawings_list.drawing[drawid]).x0 := x;
  TDraw(diyen_drawings_list.drawing[drawid]).y0 := y;
end;

procedure delete_draw( drawid: byte );
begin
  diyen_drawings_list.del_draw(drawid);
end;


// Propia de DIYen
// Devuelve el tipo draw que le corresponde al ID
// Sirve para cambiar parámetros del draw
function TD( drawid: byte ): TDraw;
begin
  Result := TDraw(diyen_drawings_list.drawing[drawid]);
end;

end.

