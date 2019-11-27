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
unit diyen_text;

{$mode objfpc}{$H+}

interface

uses
  SDL2, SDL2_image, diyen, utf8utils,
  Classes, SysUtils, Variants;

const
  DIYEN_LMAX_FONTS     = 128;
  DIYEN_LMAX_TEXTS     = 254;

type
  // Un objeto fuente
  PFont = ^TFont;
  TFont = class
    glyphs_s  : PSDL_Surface; // Surface con todos los caracteres
    // No conviene usar texturas para almacenar los caracteres
    // ya que no todos los render soportan texturas tan gigantes.
    //glyphs    : PSDL_Texture; // Textura con todos los caracteres
    name: string;
    w:    word; // Ancho de un glyph
    h:    word; // Alto de un glyph
    procedure load_from_image( filename: string );
    constructor Create( filename: string );
    destructor Destroy(); override;
    function calc_text_width( text: string; offset: smallint ): integer;
    function calc_text_height( text: string ): integer;
    function TextRender( text: string; offset: smallint ): PSDL_Texture;
    //procedure TextRenderEx( texture: PSDL_Texture; text: string; angle, size: int );
  end;

  Ttexttype = (TT_SSTRING := 0,
              TT_STRING,
              TT_INT,
              TT_FLOAT,
              TT_VAR);

  Ttextalign = (TA_TOP_LEFT := 0,
                TA_TOP,
                TA_TOP_RIGHT,
                TA_CENTER_LEFT,
                TA_CENTER,
                TA_CENTER_RIGHT,
                TA_BOTTOM_LEFT,
                TA_BOTTOM,
                TA_BOTTOM_RIGHT);

  // Un objeto texto
  PText = ^TText;
  TText = class
    ttype : Ttexttype; // tipo de texto que se analizará para renderizar

    // Todos estos valores pueden ser cambiados
    font  : PFont; // Fuente que usará este texto.

    x, y  : smallint; // Ubicación de este objeto texto en pantalla.
    size  : smallint; // Porcentaje en tamaño del texto.
    angle : integer;

    align : Ttextalign;
    offset : smallint; // Separación entre caracteres

    texto   : string; // Texto en formato string
    ptexto  : pointer; // Puntero de donde obtener el texto.

    ttex : PSDL_Texture; // El texto ya escrito para renderizar.
    alpha     : smallint;
    color     : TSDL_Color;
    blend     : TSDL_BlendMode;
    flip      : byte;

    alx, aly  : integer; // Ubicación para alinear la textura (esquina superior izquierda)
    w, h      : integer; // Ancho y alto obtenidos
    redraw : Boolean; // Si hay que redibujar el texto
    center : TSDL_Point; // Lugar del centro de rotación

    procedure SetDefaults();
    constructor Create( stexto : string );
    constructor Create( pstexto : pstring );
    constructor Create( itexto  : pinteger );
    constructor Create( ftexto  : pfloat );
    constructor Create( vtexto  : pvariant );
    procedure SetFont( nfont : PFont );
    destructor Destroy(); override;
    procedure RenderText;

    private
      // Estas son usadas para verificar si se requiere redibujar o
      // recalcular algo. Obtenemos un poco más de velocidad si no
      // calculamos cosas innecesarias.
      sant   : string; // Cadena anterior
      iant   : integer; // Entero anterior
      fant   : float; // Float anterior
      vant   : variant; // Var anterior
      aalign : TTextAlign; // Alineación anterior
      asize  : smallint; // Tamaño anterior
  end;

  // Lista de fuentes
  TdiyenFontList = class
  private
    vi_uborrado: smallint;
    vu_borrados: array of byte;
    vmax: byte;

  public
    fuente: array of Pointer;

    constructor Create();
    destructor Destroy(); override;
    function load_font_image( filename: string ): byte;
    function add_font( pfont: Pointer ): byte;
    //procedure unload_font( fontid: byte );
    procedure del_font( fontid: byte );
  end;

  // Lista de textos
  TdiyenTextList = class
  private
    vi_uborrado: smallint;
    vu_borrados: array of byte;

    function Read( index: byte ): TText;
    //procedure Write( index: byte; ttexto: TText );
  public
    vmax: byte;

    texto: array of PText;
    drawed  : Boolean;

    constructor Create();
    destructor Destroy(); override;

    function write_text(fontid: byte; x, y: integer; alignment: TTextAlign; stexto  : string ):   byte;
    function write_text(fontid: byte; x, y: integer; alignment: TTextAlign; pstexto : pstring ):  byte;
    function write_text(fontid: byte; x, y: integer; alignment: TTextAlign; itexto  : pinteger ): byte;
    function write_text(fontid: byte; x, y: integer; alignment: TTextAlign; ftexto  : pfloat ):   byte;
    function write_text(fontid: byte; x, y: integer; alignment: TTextAlign; vtexto  : pvariant ): byte;
    function add_text( ptext: Pointer ): byte;
    procedure del_text( textid: byte );

    property Item[index: byte]: TText read Read; default;
  end;


  // Funciones tipo DIV
  function load_fnt_image( filename: string ): byte;
  procedure unload_fnt( fontid: byte );
  function write( fontid: byte; x, y: integer; alignment: TTextAlign; stexto: string ): byte;
  function write_string( fontid: byte; x, y: integer; alignment: TTextAlign; pstexto: pstring ): byte;
  function write_int( fontid: byte; x, y: integer; alignment: TTextAlign; itexto: pinteger ): byte;
  function write_float( fontid: byte; x, y: integer; alignment: TTextAlign; ftexto: pfloat ): byte;
  function write_var( fontid: byte; x, y: integer; alignment: TTextAlign; vtexto: pvariant ): byte;
  procedure settext_font( textid: byte; fontid: byte );
  procedure settext_angle( textid: byte; nangle: integer );
  procedure settext_size( textid: byte; nsize: smallint );
  procedure settext_alpha( textid: byte; nalpha: word );
  procedure settext_blend( textid: byte; nblend: TSDL_BlendMode );
  procedure settext_flip( textid: byte; nflip: byte );
  procedure settext_color( textid: byte; r, g, b: byte );
  procedure settext_align( textid: byte; nalign: TTextAlign );
  procedure settext_offset( textid: byte; noffset: smallint );
  procedure move_text( textid: byte; x, y: integer );
  procedure delete_text( textid: byte );
  function TT( textid: byte ): TText;


var

  {$ifdef TEXT_USE_Z}
  text_z            : smallint = -256;
  {$endif}


  // Listas
  diyen_fonts_list : TdiyenFontList;
  diyen_texts_list : TdiyenTextList;

implementation
(*
********************
  Manejo de Textos
********************
*)
procedure TFont.load_from_image( filename: string );
//var
    //rect: TSDL_Rect;
    //pixform : TSDL_PixelFormat;
begin
  glyphs_s := IMG_Load(PChar(filename));
  name := filename;
  if (glyphs_s = nil) then
  begin
    // FIXME: Acá podemos crear una tipografía por defecto de alguna manera
    // Quizá usando un bitmap embebido en el código
    say(SDL_GetError());
    glyphs_s := SDL_CreateRGBSurface(0, 1200, 12, 32, RMASK,GMASK,BMASK, AMASK);
    //rect.w := glyphs_s^.w;
    //rect.h := glyphs_s^.h;
    //pixform.BitsPerPixel := 32;
    //SDL_FillRect(glyphs_s, @rect, SDL_MapRGBA(@pixform, 0, 0, 0, 255));
    name := 'default';
  end;

  w    := word(glyphs_s^.w div 111);
  h    := word(glyphs_s^.h);

  // No conviene usar texturas para las fuentes porque casi todas las imagenes de
  // fuente tienen un ancho mayor a lo que algunas tarjetas de video soportan
  //glyphs := SDL_CreateTextureFromSurface(gInternal.renderer, glyphs_s);
end;
constructor TFont.Create( filename: string );
begin
  load_from_image(filename);
end;
destructor TFont.Destroy();
begin
  if (glyphs_s <> nil) then SDL_FreeSurface(glyphs_s);
  //if (glyphs <> nil) then SDL_DestroyTexture(glyphs);
end;


function TFont.TextRender( text: string; offset: smallint ): PSDL_Texture;
var
  // Para analizar y graficar la cadena
  n : integer;
  //pa : integer;
  xt : integer;
  c : char;
  wc : string; // WideChar (cuando nos toca Ñ á y esas cosas)
  cn : shortint; // Valor para ubicar el caracter

  t_width, t_height : integer; // Tamaño esperado del texto

  //alx, aly : integer; // Punto de partida para alineación
  srcrect, dstrect : TSDL_Rect;

  outsurf : PSDL_Surface;
  texture : PSDL_Texture;
  //rendtex : boolean = false;
begin
  Result := nil;

  t_width := calc_text_width(text, offset);
  t_height:= calc_text_height(text);

  // Si soporta renderer a textura entonces trabajará todo con una textura
  // de lo contrario trabajará sólo con la superficie.
  // No podemos usar texturas como origen de las fuentes de
  // texto porque no todas las tarjetas de video soportan texturas
  // tan gigantes (las fuentes suelen usar imagenes de más de 2000 pixeles
  // de ancho).
  (*rendtex :=
  (gInternal.render_info.flags and SDL_RENDERER_TARGETTEXTURE)
  = SDL_RENDERER_TARGETTEXTURE;*)

  (*if (rendtex) then
  begin
    // Una superficie vacía no permite crear una textura
    //texture := SDL_CreateTextureFromSurface(gInternal.renderer, outsurf);
    texture := SDL_CreateTexture(gInternal.renderer,
    SDL_PIXELFORMAT_BGRA8888,
    SDL_TEXTUREACCESS_STATIC,
    t_width, t_height);
    //SDL_FreeSurface(outsurf);
    SDL_SetRenderTarget(gInternal.renderer, texture);
  end else*)
    // Creamos la superficie a usar.
    // Nos va a servir para crear la textura.
    outsurf := SDL_CreateRGBSurface(0, t_width, t_height, 32, RMASK,GMASK,BMASK,AMASK);
    {$ifdef DEBUG_TEXT}
    SDL_FillRect(outsurf, nil, $7f00ff00);
    {$endif}


  n := 1; xt := 0;
  while n <= Length(text) do
  begin
    c := text[n];
    Inc(n);

    if (c = #195) or (c = #194) then
    begin
      wc := c+text[n];
      Continue;
    end else
    begin
      case wc of
      'º' : cn := 95;
      'á' : cn := 96;
      'é' : cn := 97;
      'í' : cn := 98;
      'ó' : cn := 99;
      'ú' : cn := 100;
      'ñ' : cn := 101;
      'Ñ' : cn := 102;
      '¿' : cn := 103;
      '¡' : cn := 104;
      'Á' : cn := 105;
      'É' : cn := 106;
      'Í' : cn := 107;
      'Ó' : cn := 108;
      'Ú' : cn := 109;
      'ü' : cn := 110;
      'Ü' : cn := 111;
      otherwise
        cn := Ord(c)-32;
      end;
      wc := '';
    end;




    srcrect.x:=cn*w;
    srcrect.y:=0;
    srcrect.w:=w;
    srcrect.h:=h;

    dstrect.x:=xt;
    dstrect.y:=0;
    dstrect.w:=w;
    dstrect.h:=h;

    (*if (rendtex) then
    begin
      //SDL_RenderCopy(gInternal.renderer, glyphs, @srcrect, @dstrect);
    end else*)
    begin
      SDL_UpperBlit(glyphs_s, @srcrect, outsurf, @dstrect);
    end;

    Inc(xt, w+offset);
  end;

  (*if (rendtex) then
  begin
    // Desactivamos el renderizado a una textura, volvemos a lo normal.
    SDL_SetRenderTarget(gInternal.renderer, nil);
  end else*)
  begin
    // Copiamos a textura y liberamos la superficie usada
    texture := SDL_CreateTextureFromSurface(gInternal.renderer, outsurf);
    SDL_FreeSurface(outsurf);
  end;

  Result := texture;
end;

function TFont.calc_text_width( text: string; offset: smallint ): int;
begin
  if (offset = 0) then
    Result := UTF8Length(text)*w
  else
    Result := UTF8Length(text)*(w+offset)-offset;
end;

function TFont.calc_text_height( text: string ): int;
begin
  Result := h;
end;


// Renderiza el texto en la pantalla
// No distingue capa ni nada, sólo renderiza cuando es llamada.
// Pero sí detecta el tipo, obtiene la textura con el texto si es necesario
// y lo alinea en pantalla.
procedure TText.RenderText;
var
  dstrect : TSDL_Rect;
  {$ifdef USE_SCALER}{$ifndef USE_SCALER_SDL}
  ccenter : TSDL_Point;
  {$endif}{$endif}
begin
  case ttype of
    // Estáticos, no hay cambios, no se debe analizar nada
    TT_SSTRING:;
    // Pueden haber cambios, hay que analizar si requiere regraficar el texto
    TT_STRING: // Puntero a cadena
    begin
      if (sant <> string(ptexto^)) then
      begin
      sant := string(ptexto^);
      texto := sant; // No nos olvidemos del texto para graficar!
      redraw := true;
      end;
    end;
    TT_INT: // Puntero a entero
    begin
      if (iant <> integer(ptexto^)) then
      begin
      iant := integer(ptexto^);
      texto := IntToStr(iant); // No nos olvidemos del texto para graficar!
      redraw := true;
      end;
    end;
    TT_FLOAT: // Puntero a float
    begin
      if (fant <> float(ptexto^)) then
      begin
      fant := float(ptexto^);
      texto := FloatToStr(fant); // No nos olvidemos del texto para graficar!
      redraw := true;
      end;
    end;
    TT_VAR: // Puntero a variant?
    begin
      if (VarToStr(vant) <> String(ptexto^)) then
      begin
      vant := Variant(ptexto^);
      texto := VarToStr(vant); // Como la pasamos acá?
      redraw := true;
      end;
    end;
  end;

  // Hay que regraficar el texto
  if (redraw) then
  begin
    if (ttex <> nil) then SDL_DestroyTexture(ttex);
    ttex := font^.TextRender(texto, offset);
  end;

  if ttex = nil then Exit;

  // Si cambió el tamaño se recalcula
  if (size <> asize) or redraw then
  begin
    asize := size;

    w := ttex^.w * size div 100;
    h := ttex^.h * size div 100;

    redraw := true;
  end;

  // Si cambió la alineación hay que volver a calcularla
  if (align <> aalign) or redraw then
  begin
    aalign := align;
    case align of
      TA_TOP_LEFT:
        begin
        alx := 0;
        aly := 0;
        end;
      TA_TOP:
        begin
        alx := -w div 2;
        aly := 0;
        end;
      TA_TOP_RIGHT:
        begin
        alx := -w;
        aly := 0;
        end;
      TA_CENTER_LEFT:
        begin
        alx := 0;
        aly := -h div 2;
        end;
      TA_CENTER:
        begin
        alx := -w div 2;
        aly := -h div 2;
        end;
      TA_CENTER_RIGHT:
        begin
        alx := -w;
        aly := -h div 2;
        end;
      TA_BOTTOM_LEFT:
        begin
        alx := 0;
        aly := -h;
        end;
      TA_BOTTOM:
        begin
        alx := -w div 2;
        aly := -h;
        end;
      TA_BOTTOM_RIGHT:
        begin
        alx := -w;
        aly := -h;
        end;
      otherwise
        alx := 0;
        aly := 0;
    end;
    center.x := -alx;
    center.y := -aly;
  end;


  redraw := false;

  dstrect.x := x+alx;
  dstrect.y := y+aly;
  dstrect.w := w;
  dstrect.h := h;

  SDL_SetTextureAlphaMod(ttex, byte(alpha));
  SDL_SetTextureColorMod(ttex, color.r, color.g, color.b);
  SDL_SetTextureBlendMode(ttex, blend);

  {$ifdef USE_SCALER}
  {$ifndef USE_SCALER_SDL}
  ccenter := center;
  if (scaler.enabled) then
    scaler.scaleit(@dstrect, @ccenter);
  {$endif}
  {$endif}

  // Put the graphic
  if ((angle mod 360000) = 0) and (flip = SDL_FLIP_NONE) then
    SDL_RenderCopy( gInternal.renderer,
                    ttex,
                    nil, @dstrect)
  else
    begin
      SDL_RenderCopyEx( gInternal.renderer,
                        ttex,
                        nil, @dstrect,
                        angle/1000,
                        {$ifdef USE_SCALER}
                        {$ifndef USE_SCALER_SDL}
                        @ccenter,
                        {$else}
                        @center,
                        {$endif}
                        {$else}
                        @center,
                        {$endif}
                        flip);
    end;
end;

procedure TText.SetDefaults();
begin
  blend := DIYEN_DEFAULT_BLENDMODE;
  angle := 0;
  alpha := 255;
  size  := 100;
  x := 0;
  y := 0;
  flip := SDL_FLIP_NONE;
  color.r := 255;
  color.g := 255;
  color.b := 255;
  align := TA_TOP_LEFT;

  redraw := true;
end;

procedure TText.SetFont( nfont : PFont );
begin
  font := nfont;
  redraw := true;
end;

constructor TText.Create( stexto : string );
begin
  ttype   := TT_SSTRING;
  texto   := stexto;
  redraw  := true;
  SetDefaults();
end;

constructor TText.Create( pstexto : pstring );
begin
  ttype  :=TT_STRING;
  ptexto :=pointer(pstexto);
  sant   := '';
  redraw :=true;
  SetDefaults();
end;


constructor TText.Create( itexto  : pinteger );
begin
  ttype  :=TT_INT;
  ptexto :=pointer(itexto);
  iant   :=integer(itexto^)+1;
  redraw :=true;
  SetDefaults();
end;

constructor TText.Create( ftexto  : pfloat );
begin
  ttype  :=TT_FLOAT;
  ptexto :=pointer(ftexto);
  fant   :=float(ftexto^)+1;
  redraw :=true;
  SetDefaults();
end;

constructor TText.Create( vtexto  : pvariant );
begin
  ttype  :=TT_VAR;
  ptexto :=pointer(vtexto);
  redraw :=true;
  SetDefaults();
end;

destructor TText.Destroy();
begin
  if (ttex <> nil) then SDL_DestroyTexture(ttex);
end;

(* ******** Lista de fuentes y textos ******* *)

constructor TdiyenFontList.Create();
var
  i : byte;
begin
  SetLength(fuente, DIYEN_LMAX_FONTS+1);
  SetLength(vu_borrados, DIYEN_LMAX_FONTS+1);
  for i := 0 to DIYEN_LMAX_FONTS do fuente[i] := nil;
  vmax := 0;
  vi_uborrado:=0;
end;

destructor TdiyenFontList.Destroy();
var
  i : byte;
begin
  for i:=vmax downto 0 do
    if (TFont(fuente[i]) <> nil) then TFont(fuente[i]).Destroy;
  SetLength(fuente, 0);
  SetLength(vu_borrados, 0);
end;

function TdiyenFontList.load_font_image( filename: string ): byte;
var
  fontt : TFont;
begin
  fontt := TFont.Create(filename);
  Result := add_font(Pointer(fontt));
end;

function TdiyenFontList.add_font( pfont: Pointer ): byte;
var
  iborrado : byte;
begin
  if vi_uborrado > 0 then
  begin
    // Cual esta borrado?
    iborrado := vu_borrados[vi_uborrado];
    Dec(vi_uborrado); // Ya no esta mas borrado
    fuente[iborrado] := pfont;
    Result := iborrado; // Este es el indice
  end else
  begin
    Inc(vmax);
    fuente[vmax] := pfont;
    Result := vmax;
  end;
end;

procedure TdiyenFontList.del_font( fontid: byte );
begin
  if fontid <= 0 then Exit;

  if (TFont(fuente[fontid]) <> nil) then TFont(fuente[fontid]).Destroy;
  fuente[fontid] := nil;

  // Era el ultimo?
  if fontid = vmax then
  begin
    // Entonces ahora ya no (esta borrado)
    Dec(vmax);
  end else
  begin // No era el ultimo
    Inc(vi_uborrado); // Entonces se agrega a los borrados
    vu_borrados[vi_uborrado] := fontid; // Y fue este
  end;
end;



constructor TdiyenTextList.Create();
var
  i : byte;
begin
  SetLength(texto, DIYEN_LMAX_TEXTS+1);
  SetLength(vu_borrados, DIYEN_LMAX_TEXTS+1);
  for i := 0 to DIYEN_LMAX_TEXTS do texto[i] := nil;
  vmax := 0;
  vi_uborrado:=0;
end;

destructor TdiyenTextList.Destroy();
var
  i : byte;
begin
  for i:=vmax downto 0 do
    if (TText(texto[i]) <> nil) then TText(texto[i]).Destroy;
  SetLength(texto, 0);
  SetLength(vu_borrados, 0);
end;


function TdiyenTextList.Read( index: byte ): TText;
begin
  Result := TText(texto[index]);
end;

(*
procedure TdiyenTextList.Write( index: byte; ttexto: TText );
begin
  texto[index] := Pointer(ttexto);
end;*)


function TdiyenTextList.add_text( ptext: Pointer ): byte;
var
  iborrado : byte;
begin
  if vi_uborrado > 0 then
  begin
    // Cual esta borrado?
    iborrado := vu_borrados[vi_uborrado];
    Dec(vi_uborrado); // Ya no esta mas borrado
    texto[iborrado] := ptext;
    Result := iborrado; // Este es el indice
  end else
  begin
    Inc(vmax);
    texto[vmax] := ptext;
    Result := vmax;
  end;
end;

procedure TdiyenTextList.del_text( textid: byte );
var
  i : byte;
begin
  if ( textid > 0 ) then
  begin
    if (TText(texto[textid]) <> nil) then TText(texto[textid]).Destroy;
    texto[textid] := nil;

    // Era el ultimo?
    if textid = vmax then
    begin
      // Entonces ahora ya no (esta borrado)
      Dec(vmax);
    end else
    begin // No era el ultimo
      Inc(vi_uborrado); // Entonces se agrega a los borrados
      vu_borrados[vi_uborrado] := textid; // Y fue este
    end;
  end else
  // Borrar todos los textos (textid := 0)
  begin
    for i := vmax downto 0 do
    begin
      if (TText(texto[i]) <> nil) then TText(texto[i]).Destroy;
      texto[i] := nil;
    end;
    vmax := 0;
    vi_uborrado := 0;
  end;
end;

procedure SetTextParams(pttexto: PText; fontid: byte; x, y: int; alignment: TTextAlign );
begin
  pttexto^.font := @TFont(diyen_fonts_list.fuente[fontid]);
  pttexto^.x := x;
  pttexto^.y := y;
  pttexto^.align := alignment;
end;

function TdiyenTextList.write_text(
  fontid: byte;
  x, y: int;
  alignment: TTextAlign;
  stexto  : string ):   byte;
var
  ttexto : TText;
begin
  ttexto := TText.Create(stexto);
  ttexto.font := @TFont(diyen_fonts_list.fuente[fontid]);
  ttexto.x := x;
  ttexto.y := y;
  ttexto.align := alignment;

  Result := add_text(Pointer(ttexto));
end;

function TdiyenTextList.write_text(
  fontid: byte;
  x, y: int;
  alignment: TTextAlign;
  pstexto  : pstring ):   byte;
var
  ttexto : TText;
begin
  ttexto := TText.Create(pstexto);
  ttexto.font := @TFont(diyen_fonts_list.fuente[fontid]);
  ttexto.x := x;
  ttexto.y := y;
  ttexto.align := alignment;

  Result := add_text(Pointer(ttexto));
end;

function TdiyenTextList.write_text(
  fontid: byte;
  x, y: int;
  alignment: TTextAlign;
  itexto  : pinteger ):   byte;
var
  ttexto : TText;
begin
  ttexto := TText.Create(itexto);
  ttexto.font := @TFont(diyen_fonts_list.fuente[fontid]);
  ttexto.x := x;
  ttexto.y := y;
  ttexto.align := alignment;

  Result := add_text(Pointer(ttexto));
end;

function TdiyenTextList.write_text(
  fontid: byte;
  x, y: int;
  alignment: TTextAlign;
  ftexto  : pfloat ):   byte;
var
  ttexto : TText;
begin
  ttexto := TText.Create(ftexto);
  ttexto.font := @TFont(diyen_fonts_list.fuente[fontid]);
  ttexto.x := x;
  ttexto.y := y;
  ttexto.align := alignment;

  Result := add_text(Pointer(ttexto));
end;

function TdiyenTextList.write_text(
  fontid: byte;
  x, y: int;
  alignment: TTextAlign;
  vtexto  : pvariant ):   byte;
var
  ttexto : TText;
begin
  ttexto := TText.Create(vtexto);
  ttexto.font := @TFont(diyen_fonts_list.fuente[fontid]);
  ttexto.x := x;
  ttexto.y := y;
  ttexto.align := alignment;

  Result := add_text(Pointer(ttexto));
end;

(*
DIV like para textos
*)

function load_fnt_image( filename: string ): byte;
begin
  Result := diyen_fonts_list.load_font_image(filename);
end;

procedure unload_fnt( fontid: byte );
begin
  diyen_fonts_list.del_font(fontid);
end;

// Propia de DIYen
// Devuelve el tipo fuente que le corresponde al ID
//function TF( fontid: byte ): TFont;
//begin
  //Result := diyen_fonts_list[fontid];
//end;


function write(
  fontid: byte;
  x, y: int;
  alignment: TTextAlign;
  stexto  : string ): byte;
begin
  Result := diyen_texts_list.write_text(fontid,x,y,alignment,stexto);
end;
function write_string(
  fontid: byte;
  x, y: int;
  alignment: TTextAlign;
  pstexto  : pstring ): byte;
begin
  Result := diyen_texts_list.write_text(fontid,x,y,alignment,pstexto);
end;
function write_int(
  fontid: byte;
  x, y: int;
  alignment: TTextAlign;
  itexto  : pinteger ): byte;
begin
  Result := diyen_texts_list.write_text(fontid,x,y,alignment,itexto);
end;
function write_float(
  fontid: byte;
  x, y: int;
  alignment: TTextAlign;
  ftexto  : pfloat ): byte;
begin
  Result := diyen_texts_list.write_text(fontid,x,y,alignment,ftexto);
end;
function write_var(
  fontid: byte;
  x, y: int;
  alignment: TTextAlign;
  vtexto  : pvariant ): byte;
begin
  Result := diyen_texts_list.write_text(fontid,x,y,alignment,vtexto);
end;

procedure settext_font( textid: byte; fontid: byte );
begin
  TText(diyen_texts_list.texto[textid]).SetFont(@TFont(diyen_fonts_list.fuente[fontid]));
end;

procedure settext_size( textid: byte; nsize: smallint );
begin
  TText(diyen_texts_list.texto[textid]).size:=nsize;
end;

procedure settext_angle( textid: byte; nangle: integer );
begin
  TText(diyen_texts_list.texto[textid]).angle:=nangle;
end;

procedure settext_alpha( textid: byte; nalpha: word );
begin
  TText(diyen_texts_list.texto[textid]).alpha := nalpha;
end;

procedure settext_blend( textid: byte; nblend: TSDL_BlendMode );
begin
  TText(diyen_texts_list.texto[textid]).blend:=nblend;
end;

procedure settext_flip( textid: byte; nflip: byte );
begin
  TText(diyen_texts_list.texto[textid]).flip:=nflip;
end;

procedure settext_color( textid: byte; r, g, b: byte );
begin
  TText(diyen_texts_list.texto[textid]).color.r:=r;
  TText(diyen_texts_list.texto[textid]).color.g:=g;
  TText(diyen_texts_list.texto[textid]).color.b:=b;
end;

procedure settext_align( textid: byte; nalign: TTextAlign );
begin
  TText(diyen_texts_list.texto[textid]).align:=nalign;
end;

procedure settext_offset( textid: byte; noffset: smallint );
begin
  TText(diyen_texts_list.texto[textid]).offset:=noffset;
  // Esta modificación necesita redibujarse.
  TText(diyen_texts_list.texto[textid]).redraw := true;
end;

procedure move_text( textid: byte; x, y: int );
begin
  TText(diyen_texts_list.texto[textid]).x := x;
  TText(diyen_texts_list.texto[textid]).y := y;
end;

procedure delete_text( textid: byte );
begin
  diyen_texts_list.del_text(textid);
end;

// Propia de DIYen
// Devuelve el tipo texto que le corresponde al ID
// Sirve para cambiar parámetros del texto
function TT( textid: byte ): TText;
begin
  Result := TText(diyen_texts_list.texto[textid]);
end;

end.

