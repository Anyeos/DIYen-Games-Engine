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
unit diyen_tilemaps;

{$mode objfpc}{$H+}

interface

uses
  SDL2, diyen,
  Classes, SysUtils;

const
  DIYEN_LMAX_TILEMAPS  = 6;

type
  TTile = class(TProcess)
    tc : word;
    tr : word;
    executed : boolean;
  end;

  PTileMap = ^TTileMap;
  TTileMap = record
    // read only
      tileset : array of TTile;
      tiles   : int;
      drawed  : boolean;
    // read write
      x, y    : int;
      z       : smallint;
      resolution: int;
      ctype     : Tctype;
      cnumber   : set of Tcnumber;
      region    : byte;

      tile_width, tile_height : int;
      columns, rows : int;

      tile    : array of array of byte;

      populatevars : boolean; // Verdadero para enviar a los procesos tile
      // Pueden ser enviadas a los procesos tile las siguientes variables
      alpha     : byte;
      color     : TSDL_Color;
      blend     : TSDL_BlendMode;
      flip      : byte;

      angle     : int;
      size_x    : smallint;
      size_y    : smallint;

      // Start working from... to...:
      from_column, to_column: int;
      from_row, to_row: int;
  end;

  TdiyenTileMapList = class
    private
      function Read( index: byte )  : TTileMap;
    public
      alist : array of byte;
      alist_max : integer;
      procedure start_tilemap( index: byte; twidth, theight: integer; columns, rows: integer);
      procedure stop_tilemap( index: byte );
      //procedure reset_tileset( index: byte );
      procedure free_tileset( index: byte );
      procedure SetDefaultValues( tm: PTileMap );
      procedure TAdd( tile: TTile; index: byte );
      constructor Create();
      destructor Destroy(); override;
      property Item[index: byte]: TTileMap read Read; default;
  end;


procedure start_tilemap( index: byte; twidth, theight: integer; columns, rows: integer);
procedure start_tilemap( index: byte );
procedure stop_tilemap( index: byte );
//procedure reset_tileset( index: byte );
//procedure free_tileset( index: byte );
procedure TAdd( tile: TTile; index: byte );


var
  // Lists
  diyen_tilemap_list : TdiyenTileMapList;
  tilemap : array of TTileMap;


implementation

(* ***********************
  Diyen Tilemaps

  Sistema de mosaicos

* ************************)

procedure start_tilemap( index: byte; twidth, theight: integer; columns, rows: integer);
begin
  diyen_tilemap_list.start_tilemap(
  index,
  twidth, theight,
  columns, rows);
end;
procedure start_tilemap(index: byte);
begin
  diyen_tilemap_list.start_tilemap(
  index,
  tilemap[index].tile_width, tilemap[index].tile_height,
  tilemap[index].columns, tilemap[index].rows);
end;

procedure stop_tilemap( index: byte );
begin
  diyen_tilemap_list.stop_tilemap(index);
end;

(*procedure reset_tileset( index: byte );
begin
  diyen_tilemap_list.reset_tileset(index);
end;*)

procedure free_tileset( index: byte );
begin
  diyen_tilemap_list.free_tileset(index);
end;

procedure TAdd( tile: TTile; index: byte );
begin
  diyen_tilemap_list.TAdd(tile, index);
end;


constructor TdiyenTilemapList.Create();
var
  n : integer;
  tm : PTileMap;
begin
  SetLength(tilemap, DIYEN_LMAX_TILEMAPS+1);
  alist_max:=-1;

  // Initialize variables
  for n := 0 to DIYEN_LMAX_TILEMAPS do
  begin
    tm := @tilemap[n];
    SetDefaultValues(tm);
    tm^.columns := 0;
    tm^.rows    := 0;
    tm^.tile_width  := 0;
    tm^.tile_height := 0;
    tm^.tiles := 0;
  end;
end;

destructor TdiyenTilemapList.Destroy();
var
  n : integer;
begin
  for n := 0 to DIYEN_LMAX_TILEMAPS do
  begin
    free_tileset(n);
    if (tilemap[n].columns > 0) and (tilemap[n].rows > 0) then
      SetLength(tilemap[n].tile, 0);
  end;
  SetLength(tilemap, 0);
  SetLength(alist, 0);
end;

function TdiyenTilemapList.Read( index: byte )  : TTileMap;
begin
  Result := tilemap[index];
end;

procedure TdiyenTilemapList.SetDefaultValues( tm: PTileMap );
begin
  tm^.populatevars := False;
  tm^.alpha := 255;
  tm^.blend := DIYEN_DEFAULT_BLENDMODE;
  tm^.angle := 0;
  tm^.color.r := 255;
  tm^.color.g := 255;
  tm^.color.b := 255;
  tm^.drawed := false;
  tm^.flip := 0;
  tm^.size_x := 100;
  tm^.size_y := 100;
  tm^.resolution := 1;
  tm^.x := 0;
  tm^.y := 0;
  tm^.z := 0;
end;

// Arranca los tilemaps (como start_scroll es a los scrolls)
procedure TdiyenTilemapList.start_tilemap( index: byte;
  twidth, theight: integer;
  columns, rows: integer);
var
  tm : PTileMap;
begin
  Inc(alist_max);
  SetLength(alist, alist_max+1);
  alist[alist_max] := index;

  tm := @tilemap[index];
  free_tileset(index);

  SetDefaultValues(tm);

  tm^.tile_width    := twidth;
  tm^.tile_height   := theight;
  tm^.columns       := columns;
  tm^.rows          := rows;

  tm^.from_column := 0;
  tm^.to_column   := tm^.columns-1;
  tm^.from_row  := 0;
  tm^.to_row    := tm^.rows-1;


  SetLength(tm^.tile, columns, rows);
end;

procedure TdiyenTilemapList.stop_tilemap( index: byte );
var
  sn : byte;
  n : byte;
  tm : PTileMap;
begin
  for sn := 0 to alist_max do
    if alist[sn] = index then
    begin
      tm := @tilemap[alist[sn]];
      SetLength(tm^.tile, 0);
      free_tileset(index);

      if (alist_max > 0) then
      for n := sn to alist_max-1 do
        alist[n] := alist[n+1];

      Dec(alist_max);
      break;
    end;

  SetLength(alist, alist_max+1);
end;

(*procedure TdiyenTilemapList.reset_tileset( index: byte );
var
  //n : integer;
  tm : PTileMap;
begin
  tm := @tilemap[index];
  tm^.tiles := 0;
  SetLength(tm^.tileset, 0);
end;*)

procedure TdiyenTilemapList.free_tileset( index: byte );
var
  n : integer;
  tile : TTile;
  tm : PTileMap;
begin
  tm := @tilemap[index];
  if tm^.tiles <= 0 then Exit;

  for n:=0 to tm^.tiles-1 do
  begin
    tile := tm^.tileset[n];
    if (tile <> nil) then
    begin
      tile.Free;
      tm^.tileset[n] := nil;
    end;
  end;

  tm^.tiles := 0;
  SetLength(tm^.tileset, 0);
end;

procedure TdiyenTilemapList.TAdd( tile: TTile; index: byte );
var
  tm : PTileMap;
begin
  tm := @tilemap[index];
  //Result := tm^.tiles;
  Inc(tm^.tiles);
  SetLength(tm^.tileset, tm^.tiles);
  // FIXME: Se puede asignar así o se genera una copia de la clase?
  tm^.tileset[tm^.tiles-1] := tile;

  diyen_process_list.ProcessInit(tile, DIYEN_LMAX_PROCESS+tm^.tiles, 0, nil);
  tile.Internal.status := STATUS_RUNNING;
  tile.start;
end;


end.

