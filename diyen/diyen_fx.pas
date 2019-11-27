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

unit diyen_fx;

(* ***********************
  Diyen FX engine

  El motor de efectos especiales.
  Bueno, más que motor, son estrategias usando los gráficos que ya tenemos.
  Aplicando la creatividad se logran efectos visuales interesantes.

* ************************)

{$mode objfpc}{$H+}

interface

uses
  SDL2,
  Classes, SysUtils;


type
  TFader  = record
    tocolor : TSDL_Color;
    //toalpha : byte;
    color : TSDL_Color;
    alpha : byte;
    speed : byte;
    curtain : PSDL_Texture;
    pause : Boolean;
  end;

  PFXparams = ^TFXparams;
  TFXparams = record
    n : integer; // Numero de conteo por el que va

    x : integer;
    y : integer;
    angle : integer;
    size_x : integer;
    size_y : integer;

    file_ : word;
    graph : word;
    alpha : smallint;
    blend : TSDL_BlendMode;
    color : TSDL_Color;
    flip  : byte;

    count : word; // How much times to execute

    // Source rectangle:
    // Se usa para elegir que pedazo del gráfico origen dibujar
    // si no se establece (default) dibuja todo.
    // Esto es útil por ejemplo a la hora de "crear" animaciones
    // a partir de un solo mapa donde se colocan todos los sprites
    // a la vez. En lugar de usar la variable graph.
    {$ifndef FX_NO_RECT_SRC}
    Rsrc : TSDL_Rect;
    {$endif}

    // A user data pointer
    data : Pointer;
    // User integer vars
    user1, user2, user3: PInteger;
  end;

  TFXfunction = procedure(var fx: TFXparams);
  PFX = ^TFX;
  TFX = record
    count : smallint;
    fxfunction : TFXfunction;
    data : Pointer;
    user1, user2, user3: integer;
  end;

procedure clear_screen(r, g, b, alpha : Uint8);
procedure clear_screen(r, g, b: Uint8);
procedure clear_screen();
procedure clear_color(r, g, b, alpha : Uint8);
//xput
procedure put( fileID, graphID : word; x, y: integer);
procedure put_screen ( fileID, graphID: word );
//get_pixel
//get_point
//get_real_point
// DIV compatible fade():
// usar un promedio de los 3 valores (r, g, b) y obtener
// el alpha en base a ese promedio mientras van alcanzando
// el valor pedido.
procedure fade(r, g, b, vel: byte);
procedure fade_off;
procedure fade_on;

//procedure fx_add(process: TProcess; fxfunc: TFXfunction; count: word; data: Pointer);

  {$ifndef NO_FX_ENGINE}
var
  fading            : Boolean = False;
  {$endif}

implementation
uses diyen_map, diyen;


(*
  ------------------
  DIV like functions
  ------------------
*)
procedure clear_screen(r, g, b, alpha: UInt8);
begin
  gInternal.clearcolor.r := r;
  gInternal.clearcolor.g := g;
  gInternal.clearcolor.b := b;
  gInternal.clearcolor.unused := alpha;

  {$ifndef NO_BACKGROUND_GRAPH}
  {$ifdef KEEP_SURFACE}
  //SDL_FreeSurface(gInternal.sbackground);
  {$endif}
  if (gInternal.background <> nil) then SDL_DestroyTexture(gInternal.background);
  //gInternal.sbackground   := SDL_CreateRGBSurface(0, gInternal.vp.w, gInternal.vp.h, 32, RMASK,GMASK,BMASK,AMASK);
  //SDL_FillRect(gInternal.sbackground, nil, $ff000000);
  //gInternal.background    := SDL_CreateTextureFromSurface(gInternal.renderer, gInternal.sbackground);
  gInternal.background := SDL_CreateTexture( gInternal.renderer, PIXEL_FORMAT, SDL_TEXTUREACCESS_TARGET, regions.region[0].w, regions.region[0].h );
  if SDL_SetRenderTarget( gInternal.renderer, gInternal.background ) = 0 then
  begin
  {$ifndef KEEP_SURFACE}
    //SDL_FreeSurface(gInternal.sbackground);
  {$endif}
  {$endif} // NO_BACKGROUND_GRAPH

    SDL_SetRenderDrawColor(gInternal.renderer, gInternal.clearcolor.r, gInternal.clearcolor.g, gInternal.clearcolor.b, gInternal.clearcolor.unused);
    SDL_RenderClear(gInternal.renderer);

  {$ifndef NO_BACKGROUND_GRAPH}
    SDL_SetRenderTarget(gInternal.renderer,nil);
  end;
  {$endif}
end;
procedure clear_screen();
begin
  clear_screen(0,0,0,255);
end;
procedure clear_screen(r, g, b: Uint8);
begin
  clear_screen(r, g, b, 255);
end;

//procedure xput
procedure put( fileID, graphID : word; x, y: integer; blend: TSDL_BlendMode);
var
  map: TMap;
  dstrect: TSDL_Rect;
begin
  map := TMap(TFPG(diyen_fpg_list.fpg[fileID]).maps[graphID]);
  {$ifndef NO_BACKGROUND_GRAPH}

  //gInternal.background^.access := 3(*SDL_TEXTUREACCESS_TARGET*);
  if SDL_SetRenderTarget( gInternal.renderer, gInternal.background ) = 0 then
  begin
    SDL_SetTextureBlendMode(map.texture, blend);
    SDL_SetTextureAlphaMod(map.texture, 255);
    SDL_SetTextureColorMod(map.texture, 255, 255, 255);

    dstrect.x := x;
    dstrect.y := y;
    dstrect.w := map.width;
    dstrect.h := map.height;

    SDL_RenderCopy( gInternal.renderer,
                    map.texture,
                    nil, @dstrect);

    SDL_SetRenderTarget( gInternal.renderer, nil );
  end;
  {$endif}
end;

procedure put( fileID, graphID: word; x, y: int);
begin
  put(fileID, graphID, x, y, BLENDMODE_NONE);
end;

procedure put_screen ( fileID, graphID: word; blend: TSDL_BlendMode );
var
  x, y : int;
  map: TMap;
begin
  map := TMap(TFPG(diyen_fpg_list.fpg[fileID]).maps[graphID]);
  x := abs((regions.region[0].w div 2) - (map.cpoint[0].x));
  y := abs((regions.region[0].h div 2) - (map.cpoint[0].y));
  put(fileID, graphID, x, y, blend);
end;

procedure put_screen( fileID, graphID: word );
begin
  put_screen(fileID, graphID, BLENDMODE_BLEND);
end;

//procedure get_pixel
//procedure get_point
//procedure get_real_point
procedure fade(r, g, b, vel: byte);
begin
  {$ifndef NO_FX_ENGINE}
  fading := True;
  gInternal.fader.tocolor.r := r;
  gInternal.fader.tocolor.g := g;
  gInternal.fader.tocolor.b := b;
  gInternal.fader.speed := vel;
  gInternal.fader.pause := False;
  {$endif}
end;
procedure fade_off;
begin
  fade(0, 0, 0, 5);
  {$ifndef NO_FX_ENGINE}
  gInternal.fader.pause := True;
  {$endif}
end;

procedure fade_on;
begin
  fade(100, 100, 100, 5);
end;
//procedure map_get_pixel
//procedure map_put_pixel
//procedure new_map
//procedure map_clear
//procedure map_put
//procedure map_xput

// Función que sólo limpia la pantalla con un color en el momento que
// es llamada.
procedure clear_color(r, g, b, alpha: Uint8);
begin
  SDL_SetRenderDrawColor(gInternal.renderer, r, g, b, alpha);
  SDL_RenderClear(gInternal.renderer);
end;



end.

