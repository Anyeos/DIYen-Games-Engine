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
unit diyen_map;

{$mode objfpc}{$H+}

interface

uses
  SDL2, SDL2_image, sfile,
  Classes, SysUtils;

const
  DIYEN_LMAX_MAPS      = 999;
  DIYEN_LMAX_BOUNDS    = 128;
  DIYEN_LMAX_CPOINTS   = 999;
  DIYEN_LMAX_FPGS      = 99;

type

// Bound types
// They are used to collision and hit detection
Tbound_type = ( BOUND_NONE := 0,
                BOUND_AA_SQUARE,
                BOUND_AA_RECT,
                BOUND_CIRCLE,
                BOUND_POINT,
                BOUND_RECTANGLE,
                BOUND_ELLIPSE);

// Bounding structure
// The boundings are always related to the map (bitmap surface)
// (x, y) = (0,0) means center of the bitmap.
// For CIRCULARS
// x, y = center
// w = radius1
// h = radius2
// For RECTANGULARS
// x, y --------- x+w, y
//    |              |
// x, y+h ------- x+w, y+h
PBound = ^TBound;
TBound = record
  t  : Tbound_type; // Bound type
  x  : integer;
  y  : integer;
  fx : integer;
  fy : integer;
  w  : integer;
  h  : integer;
end;

TListBounds = record
  first : byte; // Start checking from this index
  last  : byte; // Finish checking in this index
  bounds : array of TBound;
end;

TPoint = record
  x : integer;
  y : integer;
end;

// The map
// A graphic container class
PMap = ^TMap;
TMap = class
  public
    cpoint : array of TSDL_Point;
    col_bounds: TListBounds;
    hit_bounds: TListBounds;
    name    : string;
    texture : PSDL_Texture;
    surface : PSDL_Surface;
    width   : integer;
    height  : integer;
    procedure set_coltype( coltype : Tbound_type; number : word );
    procedure set_coltype( coltype : Tbound_type );
    procedure set_name( nname : string );
    function cpoints_load( filename: string ): Boolean;
    function col_bounds_load( filename: string ): Boolean;
    function hit_bounds_load( filename: string ): Boolean;
    constructor Create(filename: string);
    constructor Create(basedir, filename: string; ismap : boolean);
    destructor Destroy(); override;
  protected

  published

end;

// FPG
// A static list of maps
PFPG = ^TFPG;
TFPG = class
  private
    vmax : word;

  public
    maps : array[1..DIYEN_LMAX_MAPS] of PMap;
    function map_load_image ( basedir, filename : string ) : word;
    function map_load_image ( filename : string; index: word ) : boolean;
    function map_load_image ( basedir, filename : string; index: word ) : boolean;
    procedure map_free ( index: word );
    constructor Create();
    destructor Destroy(); override;
  protected

  published

end;

// A list of diyenFPG
TdiyenFPGList = class
  private
    function load_dir_images (dir, ext: string; digits: byte): word;
  public
    fpg: array of PFPG;
    procedure new_fpg( n: word );
    function add_fpg(): word;
    procedure free_fpg( index: word );
    procedure unload_map( fileID, graphID: word );
    procedure unload_fpg( fileID: word );
    function load_from_image_list ( basedir, fname: string; digits: byte ): word;
    function load_from_image_list ( basedir, fname: string; digits: byte; auto: Boolean; ext: string ): word;
    function save_to_image_list ( fpgn: word; fname: string; digits: byte ): word;
    constructor Create();
    destructor Destroy(); override;
  private
    vi_uborrado: smallint;
    vu_borrados: array of word;
    vmax: word;
end;


// Funciones tipo DIV
function load_map_image( filename: string ): word;
function load_map_image( filename: string; index : word ): boolean;
procedure unload_map( mapid: word );
function load_fpg_list( basename: string ): word;
function load_fpg_png_list( basename: string ): word;
procedure unload_fpg( fpgid: word );

procedure get_point( fileID, graphID, pointID: word; X, Y: pint);
procedure get_point_rotated( fileID, graphID, pointID: word; X, Y: pint; angle: integer );
function get_map( fileID, graphID: word ): TMap;

var
  // Lista
  diyen_fpg_list  : TdiyenFPGList;

implementation
uses diyen;

(*
========================
TMap
========================
*)
// set_coltype
// Automatically set the bound of the map for collision detection based in
// the coltype choosen.
procedure TMap.set_coltype( coltype : Tbound_type; number : word );
var
  bound : PBound;
begin
  col_bounds.first:=number;
  col_bounds.last:=number;
  bound := @(col_bounds.bounds[number]);
  bound^.t := BOUND_NONE;

  case coltype of
    BOUND_AA_SQUARE:
    begin
      bound^.t := BOUND_AA_SQUARE;
      bound^.w := Trunc(sqrt(width*width+height*height));
      bound^.x := (0 - bound^.w) div 2;
      bound^.y := (0 - bound^.w) div 2;
    end;
    BOUND_AA_RECT:
    begin
      bound^.t := BOUND_AA_RECT;
      bound^.w := width;
      bound^.h := height;
      bound^.x := (0 - bound^.w) div 2;
      bound^.y := (0 - bound^.h) div 2;
    end;
    BOUND_RECTANGLE:
    begin
      bound^.t := BOUND_RECTANGLE;
      bound^.w := width;
      bound^.h := height;
      bound^.x := (0 - bound^.w) div 2;
      bound^.y := (0 - bound^.h) div 2;
    end;
    BOUND_CIRCLE:
    begin
      bound^.t := BOUND_CIRCLE;
      //bound^.w := Trunc(sqrt(width*width+height*height)) div 2;
      bound^.w := greater(width, height) div 2;
      bound^.x := 0;
      bound^.y := 0;
    end;
  end;
end;
procedure TMap.set_coltype( coltype : Tbound_type );
begin
  set_coltype( coltype, 0 );
end;

// Set the name of the map
procedure TMap.set_name( nname : string );
begin
  name := nname;
end;

// Para obtener los valores del archivo de puntos
procedure ObtenerPointVals(val : PChar; valx, valy: PPChar);
var
  vals : PChar;
begin
  vals := strscan(val, ',');
  valx^ := StrAlloc(vals-val+1);
  strlcopy(valx^, val, vals-val);

  valy^ := StrAlloc(StrLen(vals));
  strlcopy(valy^, vals+1, StrLen(vals));
end;

// Para obtener los valores del archivo de bounds
procedure ObtenerBoundVals(val : PChar; valt, valx, valy, valw, valh: PPChar);
var
  vals : PChar;
begin
  vals := strscan(val, '=');
  valt^ := StrAlloc(vals-val+1);
  strlcopy(valt^, val, vals-val);

  val := strscan(vals+1, ',');
  valx^ := StrAlloc(val-vals);
  strlcopy(valx^, vals+1, val-vals-1);

  vals := strscan(val+1, ' ');
  valy^ := StrAlloc(vals-val);
  strlcopy(valy^, val+1, vals-val-1);

  val := strscan(vals+1, 'x');
  valw^ := StrAlloc(val-vals);
  strlcopy(valw^, vals+1, val-vals-1);

  valh^ := StrAlloc(StrLen(val));
  strlcopy(valh^, val+1, StrLen(val));
end;

// Load the cpoints (control points)
function TMap.cpoints_load( filename: string ): Boolean;
var
  Archivo : TStringList;
  valx, valy : PChar;
  val : PChar;
  l : integer;
  stream : TStream;
begin
  Result := false;
  // cpoints[0] debe ser siempre el medio de la imagen real
  // no se permite modificar porque de esto depende que funcione
  // el sistema de detección de colisión.
  // Solucionado: Debe ser el editor quien muestre los boundings
  // con el offset del cpoint[0] aplicado y ya todo funcionará.

  // Valor por defecto
  cpoint[0].x:=width div 2;
  cpoint[0].y:=height div 2;

  Archivo := TStringList.Create;
  try
    stream := SLoadFromFile(filename);
    Archivo.Clear;
    Archivo.LoadFromStream(stream);
    stream.Free;
    Result := true;
  except
  end;

  for l:=0 to DIYEN_LMAX_CPOINTS do
  begin
    if (l < Archivo.Count) then
    begin
      val := StrAlloc(255);
      StrPCopy(val, Archivo[l]);
      ObtenerPointVals(val, @valx, @valy);
      cpoint[l].x := StrToIntDef(valx, 0);
      cpoint[l].y := StrToIntDef(valy, 0);
      StrDispose(valx);
      StrDispose(valy);
      StrDispose(val);
    end else
    begin
      if (l>0) then
      begin
        cpoint[l].x := 0;
        cpoint[l].y := 0;
      end;
    end;
  end;

  Archivo.Free;
end;

// Devuelve desde una cadena el BOUND que corresponde
function StrToBounds( s : string ) : Tbound_type;
begin
  Result := BOUND_NONE;
  case s of
    'FIXCUAD':    Result := BOUND_AA_SQUARE; // Obsolete
    'FIXRECT':    Result := BOUND_AA_RECT; // Obsolete
    'AA_SQUARE':  Result := BOUND_AA_SQUARE;
    'AA_RECT':    Result := BOUND_AA_RECT;
    'CIRCLE':     Result := BOUND_CIRCLE;
    'RECTANGLE':  Result := BOUND_RECTANGLE;
    'POINT':      Result := BOUND_POINT;
    'NONE':       Result := BOUND_NONE;
  end;
end;

procedure diyen_boundpreflip(bound : PBound);
begin
  case bound^.t of
    BOUND_AA_SQUARE:
    begin
      bound^.h := bound^.w;
      bound^.fx:= -(bound^.x+bound^.w);
      bound^.fy:= -(bound^.y+bound^.h);
    end;
    BOUND_CIRCLE:
    begin
      bound^.h := bound^.w;
      bound^.fx:= -(bound^.x);
      bound^.fy:= -(bound^.y);
    end;
    BOUND_POINT:
    begin
      bound^.w := 0;
      bound^.h := 0;
      bound^.fx:= -(bound^.x);
      bound^.fy:= -(bound^.y);
    end;
  otherwise
    bound^.fx:= -(bound^.x+bound^.w);
    bound^.fy:= -(bound^.y+bound^.h);
  end;
end;

// Load the collision bounds from a file
function TMap.col_bounds_load( filename: string ): Boolean;
var
  Archivo : TStringList;
  valt          : PChar;
  valx, valy    : PChar;
  valw, valh    : PChar;
  val : PChar;
  l : integer;
  stream : TStream;
begin
  Result := false;
  col_bounds.first := 0;
  col_bounds.last  := 0;

  Archivo := TStringList.Create;
  try
    stream := SLoadFromFile(filename);
    Archivo.Clear;
    Archivo.LoadFromStream(stream);
    stream.Free;
    col_bounds.first := 1;
    Result := true;
  except
  end;

  if (Archivo.Count > 0) then
    col_bounds.last  := byte(Archivo.Count-1)
  else
    col_bounds.last  := 0;

  for l:=0 to DIYEN_LMAX_BOUNDS do
  begin
    if (l < Archivo.Count) then
    begin
      val := StrAlloc(255);
      StrPCopy(val, Archivo[l]);
      ObtenerBoundVals(val, @valt, @valx, @valy, @valw, @valh);
      col_bounds.bounds[l].t := StrToBounds(valt);
      col_bounds.bounds[l].x := StrToIntDef(valx, 0);
      col_bounds.bounds[l].y := StrToIntDef(valy, 0);
      col_bounds.bounds[l].w := StrToIntDef(valw, 0);
      col_bounds.bounds[l].h := StrToIntDef(valh, 0);
      StrDispose(valt);
      StrDispose(valx);
      StrDispose(valy);
      StrDispose(valw);
      StrDispose(valh);
      StrDispose(val);
      diyen_boundpreflip(@col_bounds.bounds[l]);
    end else
    begin
      col_bounds.bounds[l].t := BOUND_NONE;
      col_bounds.bounds[l].x := 0;
      col_bounds.bounds[l].y := 0;
      col_bounds.bounds[l].fx := 0;
      col_bounds.bounds[l].fy := 0;
      col_bounds.bounds[l].w := 0;
      col_bounds.bounds[l].h := 0;
    end;
  end;
  Archivo.Free;
  //set_coltype(BOUND_CIRCLE);
end;
// Load the hit bounds from a file
function TMap.hit_bounds_load( filename: string ): Boolean;
var
  Archivo : TStringList;
  valt          : PChar;
  valx, valy    : PChar;
  valw, valh    : PChar;
  val : PChar;
  l : integer;
  stream : TStream;
begin
  Result := false;
  hit_bounds.first := 0;
  hit_bounds.last  := 0;

  Archivo := TStringList.Create;
  try
    stream := SLoadFromFile(filename);
    Archivo.Clear;
    Archivo.LoadFromStream(stream);
    stream.Free;
    hit_bounds.first := 1;
    Result := true;
  except
  end;

    if (Archivo.Count > 0) then
      hit_bounds.last  := byte(Archivo.Count-1)
    else
      hit_bounds.last  := 0;

  for l:=0 to DIYEN_LMAX_BOUNDS do
  begin
    if (l < Archivo.Count) then
    begin
      val := StrAlloc(255);
      StrPCopy(val, Archivo[l]);
      ObtenerBoundVals(val, @valt, @valx, @valy, @valw, @valh);
      hit_bounds.bounds[l].t := StrToBounds(valt);
      hit_bounds.bounds[l].x := StrToIntDef(valx, 0);
      hit_bounds.bounds[l].y := StrToIntDef(valy, 0);
      hit_bounds.bounds[l].w := StrToIntDef(valw, 0);
      hit_bounds.bounds[l].h := StrToIntDef(valh, 0);
      StrDispose(valt);
      StrDispose(valx);
      StrDispose(valy);
      StrDispose(valw);
      StrDispose(valh);
      StrDispose(val);
      diyen_boundpreflip(@hit_bounds.bounds[l]);
    end else
    begin
      hit_bounds.bounds[l].t := BOUND_NONE;
      hit_bounds.bounds[l].x := 0;
      hit_bounds.bounds[l].y := 0;
      hit_bounds.bounds[l].fx := 0;
      hit_bounds.bounds[l].fy := 0;
      hit_bounds.bounds[l].w := 0;
      hit_bounds.bounds[l].h := 0;
    end;
  end;

  Archivo.Free;
end;
// Creates a map
constructor TMap.Create( basedir, filename: string; ismap : boolean );
begin
  SetLength(cpoint, DIYEN_LMAX_CPOINTS+1);
  SetLength(col_bounds.bounds, DIYEN_LMAX_BOUNDS+1);
  SetLength(hit_bounds.bounds, DIYEN_LMAX_BOUNDS+1);

  width  := -1;
  height := -1;
  if (not ismap) then
  begin
    surface := IMG_Load(PChar(basedir+DirectorySeparator+filename));
    if (surface <> nil) then
    begin
      name := filename;
      width := surface^.w;
      height := surface^.h;
      texture := SDL_CreateTextureFromSurface(gInternal.renderer, surface);
      {$ifndef KEEP_SURFACE}
      SDL_FreeSurface(surface);
      {$endif}
    end else
    begin
      if filename <> '' then
        SDL_Log(PChar('Error: Cannot load image: '+filename+' from '+basedir));
    end;
  end;
end;
constructor TMap.Create( filename: string );
var
  basedir: string;
begin
  basedir := ExtractFileDir(filename);
  Create(basedir, filename, true);
end;
// Free the mem
destructor TMap.Destroy( );
begin
  SetLength(cpoint, 0);
  SetLength(col_bounds.bounds, 0);
  SetLength(hit_bounds.bounds, 0);

  if texture <> nil then SDL_DestroyTexture(texture);
  {$ifdef KEEP_SURFACE}
  if surface <> nil then SDL_FreeSurface(surface);
  {$endif}
end;

(*
  TdiyenFPG

  A FPG emulation. It does not use or support actually FPG files.
  It is for emulates the use of "file" local variable.
  It actually loads a map in a map list and do the neccesary initialization
  for that.
  In a future maybe DIYen will have some support for FPG format file.
*)

// Carga un mapa automáticamente
function TFPG.map_load_image ( basedir, filename : string ) : word;
var
  Map: TMap;
  basename: string;
  bfilename: string;
begin
  Map := TMap.Create(basedir, filename, false);
  Result := 0;
  if (Map.width > 0) then
  begin
    //basedir := ExtractFileDir(filename);
    basename := ExtractFileName(filename);
    bfilename :=  basedir+DirectorySeparator+'collision'+
                  DirectorySeparator+basename;

    // Try to load cpoints
    if not Map.cpoints_load( filename+'.pt' ) then
      Map.cpoints_load( bfilename+'.pt' );
    // Try to load bounds
    if not Map.col_bounds_load( filename+'.cb' ) then
      Map.col_bounds_load( bfilename+'.cb' );
    if not Map.hit_bounds_load( filename+'.hb' ) then
      Map.hit_bounds_load( bfilename+'.hb' );

    Inc(vmax);//max:=max+1;
    map_free(vmax);
    maps[vmax] := Pointer(Map);
    Result := vmax;
  end else Map.Destroy;
end;
// Carga una imagen como mapa manualmente y devuelve verdadero si se pudo cargar sobre ese índice
function TFPG.map_load_image ( basedir, filename : string; index: word ) : boolean;
var
  Map: TMap;
begin
  Map := TMap.Create(basedir, filename, false);
  Map.name:= filename;

  Result := false;
  if (Map.width > 0) then
  begin
    if (maps[index] = nil) then
    begin
      // Try to load cpoints
      if not Map.cpoints_load( basedir+DirectorySeparator+filename+'.pt' ) then
        Map.cpoints_load( basedir+DirectorySeparator+'collision'+DirectorySeparator+filename+'.pt' );
      // Try to load bounds
      if not Map.col_bounds_load( basedir+DirectorySeparator+filename+'.cb' ) then
        Map.col_bounds_load( basedir+DirectorySeparator+'collision'+DirectorySeparator+filename+'.cb' );
      if not Map.hit_bounds_load( basedir+DirectorySeparator+filename+'.hb' ) then
        Map.hit_bounds_load( basedir+DirectorySeparator+'collision'+DirectorySeparator+filename+'.hb' );
      map_free(index);
      maps[index] := Pointer(Map);
      Result := true;
    end else Map.Destroy;
  end else Map.Destroy;
end;
//
function TFPG.map_load_image ( filename: string; index: word ) : boolean;
var
  basedir: string;
begin
  basedir := ExtractFileDir(filename);
  filename := ExtractFileName(filename);
  Result := map_load_image(basedir, filename, index);
end;

procedure TFPG.map_free ( index: word );
begin
  //if (maps[index] <> nil) then TMap(maps[index]).Destroy;
  TMap(maps[index]).Free;
end;
constructor TFPG.Create();
var
  i : word;
begin
  for i := 1 to DIYEN_LMAX_MAPS do maps[i] := nil;
  vmax := 0;
end;
destructor TFPG.Destroy();
var
  i : word;
begin
  for i:=vmax downto 1 do map_free(i);
end;

(*
  TdiyenFPGList
  It is the list of all TdiyenFPG in the engine.
*)
procedure TdiyenFPGList.new_fpg( n : word);
var
  lFPG : TFPG;
begin
  lFPG := TFPG.Create();
  fpg[n] := Pointer(lFPG);
end;

function TdiyenFPGList.add_fpg(): word;
var
  iborrado : word;
begin

  if vi_uborrado > 0 then
  begin
    // Cual esta borrado?
    iborrado := vu_borrados[vi_uborrado];
    vi_uborrado := vi_uborrado - 1; // Ya no esta mas borrado
    new_fpg(iborrado);
    Result := iborrado; // Este es el indice
  end else
  begin
    Inc(vmax);
    new_fpg(vmax);
    Result := vmax;
  end;

end;


// Carga un directorio con imágenes con el formato de nombre de archivo:
// [000-999]nombre.ext
// La cantidad de dígitos depende de "digits".
// Un buen nombre para el archivo sería: 01 - Nave Jugador.png
// lo que lo haría cargar en el índice 1 del FPG
// Esta función devuelve el indice del FPG que se creó
function TdiyenFPGList.load_dir_images ( dir, ext: string; digits : byte ) : word;
var
  strn : string;
  n, nmax : integer;
  fResult : TSearchRec;
begin
  Result := add_fpg();

  nmax := Trunc(pow(10, digits))-1;
  for n := 1 to nmax do
  begin
    strn := '';
    FmtStr(strn, '%.'+IntToStr(digits)+'d', [n]);
    if (FindFirst (dir+DirectorySeparator+strn+'*.'+ext, 0, fResult) = 0) then
      TFPG(fpg[Result]).map_load_image(dir+DirectorySeparator+fResult.Name, n);
    FindClose(fResult);
  end;
end;
// Esta función sirve para poder usar una lista de imagenes bajo
// sistemas android ya que la función anterior "load_dir_images" requeriría
// de algunas estrategias para ser útil.
// Devuelve el indice del fpg creado o 0 si no se creó ninguno.

// Sólo carga de la lista
function TdiyenFPGList.load_from_image_list ( basedir, fname: string; digits : byte ) : word;
begin
  Result := load_from_image_list( basedir, fname, digits, false, '' );
end;

// Carga desde un directorio y autocrea el archivo si no se encontró,
// de lo contrario carga directo desde el archivo.
function TdiyenFPGList.load_from_image_list ( basedir, fname: string; digits : byte; auto: Boolean; ext: string ) : word;
var
  n, nmax : integer;
  Name : string;
  flist : TStringList;
  stream : TStream;
  //idx : integer;
begin
  flist := TStringList.Create;
  try
    stream := SLoadFromFile(fname);
    flist.LoadFromStream(stream);
    stream.Free;
    Result := add_fpg();
    nmax := Trunc(pow(10, digits))-1;
    for n := 1 to nmax do
    begin
      if (n <= flist.Count) then
        Name := flist[n-1]
      else
        Name := '';
      TFPG(fpg[Result]).map_load_image(basedir+DirectorySeparator+Name, n);
    end;
  except
    Result := 0;
    if (auto) then
    begin
      Result := load_dir_images(basedir, ext, digits);
      save_to_image_list(Result, fname, digits);
    end;
  end;

  flist.Free;
end;
// Esta función es la contraparte de la anterior y sirve para generar un archivo
// con la lista de imágenes png. Es útil para guardar una lista en la PC así luego
// podemos usar esa lista para android.
// Devuelve el indice del fpg salvado o 0 si no se salvó nada.
function TdiyenFPGList.save_to_image_list ( fpgn: word; fname: string; digits : byte ) : word;
var
  n, nmax : integer;
  Map : TMap;
  flist : TStringList;
  stream : TStream;
begin
  flist := TStringList.Create;
  nmax := Trunc(pow(10, digits))-1;
  for n := 1 to nmax do
  begin
    Map := TMap(TFPG(fpg[fpgn]).maps[n]);
    if (Map <> nil) then
      flist.Add(Map.name)
    else
      flist.Add('');
  end;

  try
    stream := SSaveToFile(fname);
    flist.SaveToStream(stream);
    stream.Free;
    Result := fpgn;
  except
    Result := 0;
  end;

  flist.Free;
end;

procedure TdiyenFPGList.free_fpg ( index: word );
begin
  if (TFPG(fpg[index]) <> nil) then TFPG(fpg[index]).Destroy;
end;
procedure TdiyenFPGList.unload_map( fileID, graphID: word );
begin
  TFPG(fpg[fileID]).map_free(graphID);
end;
procedure TdiyenFPGList.unload_fpg( fileID: word );
begin
  free_fpg(fileID);
  fpg[fileID] := nil;

  // Era el ultimo?
  if fileID = vmax then
  begin
    // Entonces ahora ya no (esta borrado)
    Dec(vmax);
  end else
  begin // No era el ultimo
    Inc(vi_uborrado); // Entonces se agrega a los borrados
    vu_borrados[vi_uborrado] := fileID; // Y fue este
  end;
end;
constructor TdiyenFPGList.Create();
var
  i : word;
begin
  SetLength(fpg, DIYEN_LMAX_FPGS+1);
  SetLength(vu_borrados, DIYEN_LMAX_FPGS+1);
  for i := 0 to DIYEN_LMAX_FPGS do fpg[i] := nil;
  vmax := 0;
  vi_uborrado:=0;
end;
destructor TdiyenFPGList.Destroy();
var
  i : word;
begin
  for i:=vmax downto 1 do free_fpg(i);
  SetLength(fpg, 0);
  SetLength(vu_borrados, 0);
end;


// Load a image as map into memory. The image as map in diyen can be any graphic
// file but it can have associated three more files:
// filename.ext.pt  - Points file
// filename.ext.cb  - Collision bounds definition file
// filename.ext.hb  - Hit bounds definition file
function load_map_image( filename: string ): word;
var
  basedir: String;
begin
  basedir := ExtractFileDir(filename);
  filename := ExtractFileName(filename);
  Result := TFPG(diyen_fpg_list.fpg[0]).map_load_image(basedir, filename);
end;
function load_map_image( filename: string; index : word ): boolean;
begin
  Result := TFPG(diyen_fpg_list.fpg[0]).map_load_image(filename, index);
end;
procedure unload_map( mapid : word );
begin
  TFPG(diyen_fpg_list.fpg[0]).map_free(mapid);
end;

function load_fpg_list( basename: string ): word;
begin
  Result := diyen_fpg_list.load_from_image_list(basename, basename+'.fpl', 2);
end;
function load_fpg_png_list( basename: string ): word;
begin
  Result := diyen_fpg_list.load_from_image_list(basename, basename+'.fpl', 2, true, 'png');
end;
procedure unload_fpg( fpgid : word );
begin
  diyen_fpg_list.unload_fpg(fpgid);
end;



procedure get_point( fileID, graphID, pointID : word; X, Y: pint);
var
  point : PSDL_Point;
begin
  point := @TMap(TFPG(diyen_fpg_list.fpg[fileID]).maps[graphID]).cpoint[pointID];
  X^ := point^.x;
  Y^ := point^.y;
end;
procedure get_point_rotated( fileID, graphID, pointID : word; X, Y: pint; angle: integer );
var
  point : PSDL_Point;
  cosb: float;
  senb: float;
  px, py: integer;
begin
  point := @TMap(TFPG(diyen_fpg_list.fpg[fileID]).maps[graphID]).cpoint[pointID];
  px := point^.x;
  py := point^.y;
  cosb := dcos(angle);
  senb := dsin(angle);
  X^ := Trunc((px)*cosb-(py)*senb);
  Y^ := Trunc((py)*cosb+(px)*senb);
end;


function get_map( fileID, graphID: word ): TMap;
begin
  Result := TMap(TFPG(diyen_fpg_list.fpg[fileID]).maps[graphID]);
end;

end.

