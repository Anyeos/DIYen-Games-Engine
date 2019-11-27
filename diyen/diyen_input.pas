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
unit diyen_input;

{$mode objfpc}{$H+}

interface

uses
  SDL2, diyen_sstruct, diyen_map, diyen,
  Classes, SysUtils, utf8utils, math;

(*
  Diyen input system

  El sistema de entrada de diyen provee de funciones prácticas para
  la abstracción de dispositivos de entrada (teclado, joystick,
  mouse, etc).

  Además de las funciones estándares de DIV, provee de otras más
  que permiten cierta abstracción con los dispositivos de entrada.
  Y a su vez facilitan su configuración y uso en una forma más
  universal:

  La función pushed(jugador, boton)
  Retorna 1 si está presionada una tecla digital o 0 si no está presionada.
  Pero también retorna un valor distinto de cero representando la variación
  en el eje del joystick.

  También puede ser usada para los menús, usando BUTTON_UP, BUTTON_DOWN,
  BUTTON_LEFT, BUTTON_RIGHT, BUTTON_OK y BUTTON_CANCEL como segundo parámetro
  y como primer parámetro cero ya que siempre en los menús se utiliza
  el joystick que primero presione una tecla como dispositivo principal.


*)


const
// Entradas genéricas, no configurables, usadas en menús
BTMENU_UP     = 201;
BTMENU_DOWN   = 202;
BTMENU_LEFT   = 203;
BTMENU_RIGHT  = 204;
BTMENU_OK     = 205; // Presiona la tecla de aceptación
BTMENU_CANCEL = 206; // Presiona la tecla de cancelar o salir
BTMENU_BACK   = 207; // Tecla de volver
BTMENU_FS     = 208; // Tecla de cambiar el modo pantalla completa o ventana
BTMENU_DEL    = 209; // Presiona la tecla de borrar

BUTTON_MOUSE_LEFT   = 101;
BUTTON_MOUSE_MIDDLE = 102;
BUTTON_MOUSE_RIGHT  = 103;
MOUSE_STATE_MOTION  = 1<<(7-1);

// Valor máximo de mapeo de teclas (0 es la primer tecla)
DIYEN_MAX_MAPPINGS = 22;
// Valor máximo de jugadores a configurar (0 es el primero)
DIYEN_MAX_DEVICES = 3;


// Constantes para el uso de mapeos de entradas
// Estas constantes sirven para almacenar a qué botón o control pertenecen
// Serían las teclas configurables de tu juego

// Digitales
// Digital pad
BUTTON_DUP		= 1;
BUTTON_DDOWN	= 2;
BUTTON_DLEFT	= 3;
BUTTON_DRIGHT	= 4;

// Botones de lucha
// Fight buttons
BUTTON_A	= 5;
BUTTON_B	= 6;
BUTTON_X	= 7;
BUTTON_Y	= 8;

// Botones traseros y secundarios
// Back buttons and secondary
BUTTON_L1	= 9;
BUTTON_R1	= 10;
BUTTON_L2	= 11;
BUTTON_R2	= 12;
BUTTON_L3	= 13;
BUTTON_R3	= 14;

// Start
BUTTON_BACK 	= 15;
BUTTON_START	= 16;

// Analógicos
// Analog pad1
BUTTON_AX	= 17;
BUTTON_AY	= 18;

// Analog pad2
BUTTON_A2X	= 19;
BUTTON_A2Y	= 20;

// Analog trigger
BUTTON_ATLZ = 21;
BUTTON_ATRZ = 22;


// Fin constantes fijas de mapeos
DIYEN_LMAX_TOUCHS = 32; // Elementos de joystick virtual

// DIV const for key()
_ESC         = SDL_SCANCODE_ESCAPE;
_1           = SDL_SCANCODE_1;
_2           = SDL_SCANCODE_2;
_3           = SDL_SCANCODE_3;
_4           = SDL_SCANCODE_4;
_5           = SDL_SCANCODE_5;
_6           = SDL_SCANCODE_6;
_7           = SDL_SCANCODE_7;
_8           = SDL_SCANCODE_8;
_9           = SDL_SCANCODE_9;
_0           = SDL_SCANCODE_0;
_MINUS       = SDL_SCANCODE_MINUS;
_PLUS        = SDLK_PLUS;
_BACKSPACE   = SDL_SCANCODE_BACKSPACE;
_TAB         = SDL_SCANCODE_TAB;
_Q           = SDL_SCANCODE_Q;
_W           = SDL_SCANCODE_W;
_E           = SDL_SCANCODE_E;
_R           = SDL_SCANCODE_R;
_T           = SDL_SCANCODE_T;
_Y           = SDL_SCANCODE_Y;
_U           = SDL_SCANCODE_U;
_I           = SDL_SCANCODE_I;
_O           = SDL_SCANCODE_O;
_P           = SDL_SCANCODE_P;
_L_BRACKET   = SDL_SCANCODE_LEFTBRACKET;
_R_BRACKET   = SDL_SCANCODE_RIGHTBRACKET;
_ENTER       = SDL_SCANCODE_RETURN;
_C_ENTER     = SDL_SCANCODE_KP_ENTER;
_CONTROL     = SDL_SCANCODE_LCTRL;
_A           = SDL_SCANCODE_A;
_S           = SDL_SCANCODE_S;
_D           = SDL_SCANCODE_D;
_F           = SDL_SCANCODE_F;
_G           = SDL_SCANCODE_G;
_H           = SDL_SCANCODE_H;
_J           = SDL_SCANCODE_J;
_K           = SDL_SCANCODE_K;
_L           = SDL_SCANCODE_L;
_SEMICOLON   = SDL_SCANCODE_SEMICOLON;
_APOSTROPHE  = SDL_SCANCODE_APOSTROPHE;
_WAVE        = SDL_SCANCODE_GRAVE;
_L_SHIFT     = SDL_SCANCODE_LSHIFT;
_BACKSLASH   = SDL_SCANCODE_BACKSLASH;
_Z           = SDL_SCANCODE_Z;
_X           = SDL_SCANCODE_X;
_C           = SDL_SCANCODE_C;
_V           = SDL_SCANCODE_V;
_B           = SDL_SCANCODE_B;
_N           = SDL_SCANCODE_N;
_M           = SDL_SCANCODE_M;
_COMMA       = SDL_SCANCODE_COMMA;
_POINT       = SDL_SCANCODE_PERIOD;
_SLASH       = SDL_SCANCODE_SLASH;
_C_BACKSLASH = SDL_SCANCODE_KP_DIVIDE;
_R_SHIFT     = SDL_SCANCODE_RSHIFT;
_C_ASTERISK  = SDL_SCANCODE_KP_MULTIPLY;
_PRN_SCR     = SDL_SCANCODE_PRINTSCREEN;
_ALT         = SDL_SCANCODE_LALT;
_SPACE       = SDL_SCANCODE_SPACE;
_CAPS_LOCK   = SDL_SCANCODE_CAPSLOCK;
_F1          = SDL_SCANCODE_F1;
_F2          = SDL_SCANCODE_F2;
_F3          = SDL_SCANCODE_F3;
_F4          = SDL_SCANCODE_F4;
_F5          = SDL_SCANCODE_F5;
_F6          = SDL_SCANCODE_F6;
_F7          = SDL_SCANCODE_F7;
_F8          = SDL_SCANCODE_F8;
_F9          = SDL_SCANCODE_F9;
_F10         = SDL_SCANCODE_F10;
_NUM_LOCK    = SDL_SCANCODE_NUMLOCKCLEAR;
_SCROLL_LOCK = SDL_SCANCODE_SCROLLLOCK;
_HOME        = SDL_SCANCODE_HOME;
_C_HOME      = SDL_SCANCODE_KP_7;
_UP          = SDL_SCANCODE_UP;
_C_UP        = SDL_SCANCODE_KP_8;
_PGUP        = SDL_SCANCODE_PAGEUP;
_C_PGUP      = SDL_SCANCODE_KP_3;
_C_MINUS     = SDL_SCANCODE_KP_MINUS;
_LEFT        = SDL_SCANCODE_LEFT;
_C_LEFT      = SDL_SCANCODE_KP_4;
_C_CENTER    = SDL_SCANCODE_KP_5;
_RIGHT       = SDL_SCANCODE_RIGHT;
_C_RIGHT     = SDL_SCANCODE_KP_6;
_C_PLUS      = SDL_SCANCODE_KP_PLUS;
_END         = SDL_SCANCODE_END;
_C_END       = SDL_SCANCODE_KP_1;
_DOWN        = SDL_SCANCODE_DOWN;
_C_DOWN      = SDL_SCANCODE_KP_2;
_PGDN        = SDL_SCANCODE_PAGEDOWN;
_C_PGDN      = SDL_SCANCODE_KP_9;
_INS         = SDL_SCANCODE_INSERT;
_C_INS       = SDL_SCANCODE_KP_0;
_DEL         = SDL_SCANCODE_DELETE;
_C_DEL       = SDL_SCANCODE_KP_DECIMAL;
_F11         = SDL_SCANCODE_F11;
_F12         = SDL_SCANCODE_F12;
_LESS        = SDLK_LESS;
_EQUALS      = SDL_SCANCODE_EQUALS;
_GREATER     = SDLK_GREATER;
_ASTERISK    = SDLK_ASTERISK;
_R_ALT       = SDL_SCANCODE_RALT;
_R_CONTROL   = SDL_SCANCODE_RCTRL;
_L_ALT       = SDL_SCANCODE_LALT;
_L_CONTROL   = SDL_SCANCODE_LCTRL;
_MENU        = SDL_SCANCODE_MENU;
_L_WINDOWS   = SDL_SCANCODE_LGUI;
_R_WINDOWS   = SDL_SCANCODE_RGUI;

(* TODO
STAT_RSHIFT  = ;
STAT_LSHIFT  = ;
STAT_CTRL    = ;
STAT_ALT     = ;
STAT_RCTRL   = ;
STAT_LCTRL   = ;
STAT_RALT    = ;
STAT_LALT    = ;
STAT_NUM     = ;
STAT_CAPS    = ;
STAT_SHIFT   = ;
*)

// Remapeo a los menús

// Para la OUYA
JOYBTMENU_UP      = 11;
JOYBTMENU_DOWN    = 12;
JOYBTMENU_LEFT    = 13;
JOYBTMENU_RIGHT   = 14;
JOYBTMENU_OK      = 0;
JOYBTMENU_CANCEL  = 5; // Menu
JOYBTMENU_BACK    = 1;
JOYBTMENU_DEL     = 3;
JOYBTMENU_FS      = 254; // Disabled

type
  TKeyButton = record
    k : word; // El valor que la función key() espera
  end;

  TJoyButton = record
    d : word; // El dispositivo de joystick que le corresponde
    j : word; // El número de botón o eje del joystick
  end;

  TInputConfig = record
    key0 : array[1..DIYEN_MAX_MAPPINGS] of TKeyButton;
    key1 : array[1..DIYEN_MAX_MAPPINGS] of TKeyButton;
    joy0 : array[1..DIYEN_MAX_MAPPINGS] of TJoyButton;
    joy1 : array[1..DIYEN_MAX_MAPPINGS] of TJoyButton;
  end;

  TdiyenInputConfig = class(TReadableConfig)
  private
    function DevToString(dev: integer): String;
    function MappingValueString(index: integer; mapping: integer): String;
  public
    map : array[0..DIYEN_MAX_DEVICES] of TInputConfig;
    procedure Save();
    procedure Load();
  end;

  TMouse = record
    X, Y : Integer;
    Xrel, Yrel : Integer;
    state : integer;
    touchid : integer;
  end;

  TTouchType =
  (TOUCH_DIGITAL := 0,
   TOUCH_TOGGLE,
   TOUCH_ANALOGXY,
   TOUCH_ANALOGZ);

  PTouchGraph = ^TTouchGraph;
  TTouchGraph = record
    fileID  : word;
    graphID : word;
    alpha   : byte;
    blend   : TSDL_BlendMode;
    color   : TSDL_Color;
  end;

  PTouch = ^TTouch;
  TTouch = class
    ttype : TTouchType;
    button : byte; // index of button emulation (BUTTON_A, BUTTON_B, BUTTON_AX, etc)
    //button2 : byte; // Mostly for analog, if button = BUTTON_AX, then button2 must be BUTTON_AY.
    device : byte; // index of device emulation (0 for first player, 1 for second...)

    dx, dy : single; // Delta of analog stick normalized (-1..1)

    area: TSDL_Rect; // Rectangle detection area

    pushed : boolean; // it is pushed
    overed : boolean; // it is overed

    tgraph_default : TTouchGraph; // Normal unpushed graph
    tgraph_over    : TTouchGraph; // Mouse over graph
    tgraph_pushed  : TTouchGraph; // Pushed graph
    tgraph_stick   : TTouchGraph; // Graph of the stick if it is TOUCH_ANALOG

    procedure RenderTouch;
  end;

  // Lista de elementos touch (joystick virtual)
  TdiyenTouchList = class
  private
    vi_uborrado: smallint;
    vu_borrados: array of byte;

    function Read( index: byte ): TTouch;
  public
    touch_map : array[0..DIYEN_MAX_DEVICES] of array[1..DIYEN_MAX_MAPPINGS] of smallint;

    vmax: smallint;

    touch: array of PTouch;

    constructor Create;
    destructor Destroy; override;

    function add_touch( touchp: PTouch ): byte;
    procedure del_touch( touchid: byte );

    property Item[index: byte]: TTouch read Read; default;
  end;


procedure diyen_input_init();
procedure diyen_input_finish();
procedure input_touch_unpush();
function input_touch_over(x, y: integer): integer;
function input_touch_over(x, y: integer; touchid: integer): integer;
procedure input_finger_touch(X, Y: integer; id: byte);

// DIV like input functions
function joy_number() : integer;
function joy_getbutton( device, button : integer ) : boolean;
function joy_getaxis( device, button : integer ) : integer;
function key( scancode : integer ) : boolean;

// DIYEN input functions
function pushedm(input : integer) : boolean;
function pushed(num, input : integer) : integer;

function touched(num, input : integer) : integer;
function FingersActive: integer;
function touch(kind: TTouchType; device, button: byte; x, y, w, h: integer; fileID, graphID, graphIDstick: word): byte;
function touch(kind: TTouchType; device, button: byte; x, y, w, h: integer; fileID, graphID: word): byte;
function TTT( touchid: byte ): TTouch;
procedure move_touch( touchid: byte; x, y: integer );
procedure delete_touch( touchid: byte );

var
  // Keys from SDL
  input_KeysState   : PUInt8; // Array for compare if key is pressed
  input_KeysNum     : PInt;   // Number of keys pressed at one time
  // Joysticks
  input_joysticks           : array of PSDL_Joystick;
  input_joysticks_len        : word;
  //input_joystick : integer = -1;
  joy_axis_menu : integer = 0;
  touch_axis_menu : integer = BUTTON_AX;

  // La variable de configuración del mapeo de entrada
  input_config : TdiyenInputConfig;

  {$ifndef DISABLE_MOUSE}
  mouse : TMouse;
  {$endif}

  finger : array of TMouse;
  input_touchdevice : TSDL_TouchID;
  FingersTotal : word = 0;

  diyen_touchs_list : TdiyenTouchList;


  MappingToString: Array[1..DIYEN_MAX_MAPPINGS] of String =
  ('BUTTON_DUP',
  'BUTTON_DDOWN',
  'BUTTON_DLEFT',
  'BUTTON_DRIGHT',
  'BUTTON_A',
  'BUTTON_B',
  'BUTTON_X',
  'BUTTON_Y',
  'BUTTON_L1',
  'BUTTON_R1',
  'BUTTON_L2',
  'BUTTON_R2',
  'BUTTON_L3',
  'BUTTON_R3',
  'BUTTON_BACK',
  'BUTTON_START',
  'BUTTON_AX',
  'BUTTON_AY',
  'BUTTON_A2X',
  'BUTTON_A2Y',
  'BUTTON_ATLZ',
  'BUTTON_ATRZ');

implementation


(* ***********************
  Diyen input system

  El sistema de entrada de diyen provee de funciones prácticas para
  la utilización de dispositivos de entrada (teclado, joystick,
  mouse, etc).

  Además de las funciones estándares de DIV, provee de otras más
  que permiten cierta abstracción con los dispositivos de entrada.
  Y a su vez facilitan su configuración y uso en una forma más
  universal.
* ************************)



(*
  DIV like functions
*)
function key( scancode : integer ) : boolean;
begin
  Result := input_KeysState[scancode] = 1;
end;

function joy_getbutton( device, button : integer ) : boolean;
begin
  Result := false;
  if (device >= 0) and (device < input_joysticks_len) then
    Result := SDL_JoystickGetButton(input_joysticks[device], button) = 1;
end;

function joy_getaxis( device, button : integer ) : integer;
begin
  Result := 0;
  if (device >= 0) and (device < input_joysticks_len) then
    Result := SDL_JoystickGetAxis(input_joysticks[device], button);
end;

function joy_getball( device, ball : integer; dx, dy : pinteger ) : integer;
begin
  Result := 0;
  if (device >= 0) and (device < input_joysticks_len) then
    Result := SDL_JoystickGetBall(input_joysticks[device], ball, dx, dy);
end;

function joy_gethat( device, hat : integer ) : byte;
begin
  Result := 0;
  if (device >= 0) and (device < input_joysticks_len) then
    Result := SDL_JoystickGetHat(input_joysticks[device], hat);
end;

function joy_number() : integer;
begin
  Result := input_joysticks_len;
end;

function joy_name( device : integer ) : string;
begin
  if (device >= 0) and (device < input_joysticks_len) then
    Result := SDL_JoystickName(input_joysticks[device]);
  //Result := SDL_JoystickNameForIndex(device);
end;

(*
* Funciones de entrada exclusivas de diyen
*)

// Busca el primer joystick que se va a usar como principal
function diyen_input_search_active_joystick : integer;
var
	d   : integer;
	k		: integer;
begin
	Result := -1;
	for d:=0 to input_joysticks_len-1 do
	begin
		for k:=0 to 32 do
		begin
			if joy_getbutton(d,k) then
			begin
			Result := d;
      //SDL_Log(PChar('Active input device: '+IntToStr(Result)));
			Exit;
			end;
		end;
	end;
end;


// Si el punto está sobre un elemento del joystick virtual
// devuelve el id de ese elemento
function input_touch_over(x, y: integer; touchid: integer): integer;
var
  i : integer;
begin
  Result := -1;
  if touchid < 0 then
  begin
    for i := 1 to diyen_touchs_list.vmax do
    with diyen_touchs_list[i] do
    if diyen_touchs_list[i] <> nil then
      // Detectamos si el punto está incluido en el rectángulo
      if
      (x > area.x) and
      (x < (area.x+area.w)) and
      (y > area.y) and
      (y < (area.y+area.h)) then
      begin
        Result := i;
        break;
      end;
  end else
  begin
    with diyen_touchs_list[touchid] do
    if diyen_touchs_list[touchid] <> nil then
      // Detectamos si el punto está incluido en el rectángulo
      if
      (x > area.x) and
      (x < (area.x+area.w)) and
      (y > area.y) and
      (y < (area.y+area.h)) then
      begin
        Result := touchid;
      end;
  end;
end;
function input_touch_over(x, y: integer): integer;
begin
  Result := input_touch_over(x, y, -1);
end;

procedure input_finger_touch(X, Y: integer; id: byte);
var
  jx, jy : integer;
begin
      with diyen_touchs_list[id] do
      if diyen_touchs_list[id] <> nil then
      begin

          if ttype = TOUCH_ANALOGXY then
          begin
            if pushed then
            begin
              dx := ((X-area.x - area.w div 2) / (area.w div 2));
              dy := ((Y-area.y - area.h div 2) / (area.h div 2));
            end else
            begin
              dx := 0;
              dy := 0;
            end;
            (*if dx > 0.8 then dx := 0.8
            else if dx < -0.8 then dx := -0.8;
            if dy > 0.8 then dy := 0.8
            else if dy < -0.8 then dy := -0.8;*)

            // 25% más así el stick no se tiene que pegar al borde del
            // area de movimiento.
            jx := Trunc(dx*40960);
            jy := Trunc(dy*40960);

            if jx > 32767 then jx := 32767
            else if jx < -32768 then jx := -32768;
            if jy > 32767 then jy := 32767
            else if jy < -32768 then jy := -32768;
            //say('jx, jy '+IntToStr(jx)+','+IntToStr(jy));


            diyen_touchs_list.touch_map[device][button]   := jx;
            diyen_touchs_list.touch_map[device][button+1] := jy;
          end else
          if pushed then
          // FIXME: A veces el analógico va a entregar 1 por sólo pulsar
          //        sobre él.
            diyen_touchs_list.touch_map[device][button] := 1 // DOWN
          else
            diyen_touchs_list.touch_map[device][button] := 0; // UP
      end;
end;

procedure input_touch_unpush();
var
  i : integer;
begin
  for i := 1 to diyen_touchs_list.vmax do
  with diyen_touchs_list[i] do
  if diyen_touchs_list[i] <> nil then
  begin
    pushed:=false;
    dx := 0;
    dy := 0;
    diyen_touchs_list.touch_map[device][button] := 0;
    if ttype = TOUCH_ANALOGXY then
      diyen_touchs_list.touch_map[device][button+1] := 0;
  end;
end;

function touched(num, input: integer): integer;
begin
  Result := diyen_touchs_list.touch_map[num][input];
end;

// Static unmodifiable keys for menus
function pushedm(input : integer) : boolean;
var
  active_joystick : integer = -1;
  active_touch : integer = 0;
  ax : integer = 0;
  ay : integer = 0;
begin
  Result := false;

  if {(input_joystick < 0) and} (input_joysticks_len > 0) then
    active_joystick := diyen_input_search_active_joystick;

		case input of
			BTMENU_UP:
      begin
      {$ifndef DISABLE_TOUCH}
      if touch_axis_menu > -1 then
      begin
        //ax := diyen_touchs_list.touch_map[active_touch][touch_axis_menu];
        ay := diyen_touchs_list.touch_map[active_touch][touch_axis_menu+1];
        if ay < -16000 then
        begin
          Result := True;
          Exit;
        end;
      end;{$endif}
      if (input_joysticks_len > 0) and (joy_axis_menu > -1) then
      begin
        //ax := joy_getaxis(active_joystick, joy_axis_menu);
        ay := joy_getaxis(active_joystick, joy_axis_menu+1);
        if ay < -16000 then
        begin
          Result := True;
          Exit;
        end;
      end;
      Result := (key(_UP) or
          {$ifndef DISABLE_TOUCH}Boolean(diyen_touchs_list.touch_map[active_touch][BUTTON_DUP]) or{$endif}
					((input_joysticks_len > 0) and
					joy_getbutton(active_joystick,JOYBTMENU_UP)));
      end;
      BTMENU_DOWN:
      begin
      {$ifndef DISABLE_TOUCH}
      if touch_axis_menu > -1 then
      begin
        //ax := diyen_touchs_list.touch_map[active_touch][touch_axis_menu];
        ay := diyen_touchs_list.touch_map[active_touch][touch_axis_menu+1];
        if ay > 16000 then
        begin
          Result := True;
          Exit;
        end;
      end; {$endif}
      if (input_joysticks_len > 0) and (joy_axis_menu > -1) then
      begin
        //ax := joy_getaxis(active_joystick, joy_axis_menu);
        ay := joy_getaxis(active_joystick, joy_axis_menu+1);
        if ay > 16000 then
        begin
          Result := True;
          Exit;
        end;
      end;
      Result := (key(_DOWN) or
          {$ifndef DISABLE_TOUCH}Boolean(diyen_touchs_list.touch_map[active_touch][BUTTON_DDOWN]) or{$endif}
					((input_joysticks_len > 0) and
					joy_getbutton(active_joystick,JOYBTMENU_DOWN)));
      end;
      BTMENU_LEFT:
      begin
      {$ifndef DISABLE_TOUCH}
      if touch_axis_menu > -1 then
      begin
        ax := diyen_touchs_list.touch_map[active_touch][touch_axis_menu];
        //ay := diyen_touchs_list.touch_map[active_touch][touch_axis_menu+1];
        if ax < -16000 then
        begin
          Result := True;
          Exit;
        end;
      end; {$endif}
      if (input_joysticks_len > 0) and (joy_axis_menu > -1) then
      begin
        ax := joy_getaxis(active_joystick, joy_axis_menu);
        //ay := joy_getaxis(active_joystick, joy_axis_menu+1);
        if ax < -16000 then
        begin
          Result := True;
          Exit;
        end;
      end;
      Result :=  (key(_LEFT) or
          {$ifndef DISABLE_TOUCH}Boolean(diyen_touchs_list.touch_map[active_touch][BUTTON_DLEFT]) or{$endif}
					((input_joysticks_len > 0) and
					joy_getbutton(active_joystick,JOYBTMENU_LEFT)));
      end;
			BTMENU_RIGHT:
      begin
      {$ifndef DISABLE_TOUCH}
      if touch_axis_menu > -1 then
      begin
        ax := diyen_touchs_list.touch_map[active_touch][touch_axis_menu];
        //ay := diyen_touchs_list.touch_map[active_touch][touch_axis_menu+1];
        if ax > 16000 then
        begin
          Result := True;
          Exit;
        end;
      end; {$endif}
      if (input_joysticks_len > 0) and (joy_axis_menu > -1) then
      begin
        ax := joy_getaxis(active_joystick, joy_axis_menu);
        //ay := joy_getaxis(active_joystick, joy_axis_menu+1);
        if ax > 16000 then
        begin
          Result := True;
          Exit;
        end;
      end;
			Result :=  (key(_RIGHT) or
          {$ifndef DISABLE_TOUCH}Boolean(diyen_touchs_list.touch_map[active_touch][BUTTON_DRIGHT]) or{$endif}
					((input_joysticks_len > 0) and
					joy_getbutton(active_joystick,JOYBTMENU_RIGHT)));
      end;
      BTMENU_OK:
			Result :=  (key(_ENTER) or
        {$ifndef DISABLE_TOUCH}Boolean(diyen_touchs_list.touch_map[active_touch][BUTTON_A]) or{$endif}
				((input_joysticks_len > 0) and (
				joy_getbutton(active_joystick,JOYBTMENU_OK))));

			BTMENU_CANCEL:
			Result :=  (key(_ESC) or
          {$ifndef DISABLE_TOUCH}Boolean(diyen_touchs_list.touch_map[active_touch][BUTTON_BACK]) or{$endif}
					((input_joysticks_len > 0) and
					joy_getbutton(active_joystick,JOYBTMENU_CANCEL)));

      BTMENU_BACK:
			Result :=  (key(_ESC) or
          {$ifndef DISABLE_TOUCH}Boolean(diyen_touchs_list.touch_map[active_touch][BUTTON_BACK]) or{$endif}
					((input_joysticks_len > 0) and
					joy_getbutton(active_joystick,JOYBTMENU_BACK)));

      BTMENU_DEL:
  		Result :=  (key(_BACKSPACE) or
          {$ifndef DISABLE_TOUCH}Boolean(diyen_touchs_list.touch_map[active_touch][BUTTON_Y]) or{$endif}
  				((input_joysticks_len > 0) and
  				joy_getbutton(active_joystick,JOYBTMENU_DEL)));

      BTMENU_FS:
  		Result :=  (key(_F10) or
  				((input_joysticks_len > 0) and
  				joy_getbutton(active_joystick,JOYBTMENU_FS)));
		end;
end;


// Return 1 if the key is pressed / pushed
// num is the configured/mapped number (it is equivalent to player number)
// If it is digital returns 0 if unpressed and 1 if pressed.
// if it is analog returns the axis value.
function pushed(num, input : integer) : integer;
begin
	if (input < 17) then // Digital
  begin
		Result :=
      {$ifndef DISABLE_TOUCH}touched(num, input) or{$endif}
      Integer(
			key(input_config.map[num].key0[input].k) or
			key(input_config.map[num].key1[input].k) or
			((input_joysticks_len > 0) and (
			joy_getbutton(input_config.map[num].joy0[input].d, input_config.map[num].joy0[input].j) or
			joy_getbutton(input_config.map[num].joy1[input].d, input_config.map[num].joy1[input].j))));
	end else
  if (input < 23) then // Analog
  begin
		Result :=
        {$ifndef DISABLE_TOUCH}touched(num, input) or{$endif}
        (Integer(input_joysticks_len > 0) * (
				joy_getaxis(input_config.map[num].joy0[input].d, input_config.map[num].joy0[input].j)+
				joy_getaxis(input_config.map[num].joy1[input].d, input_config.map[num].joy1[input].j)));
  {$ifndef DISABLE_MOUSE}
	end else
  if (input < 200) then // Mouse
  begin
    Result := 1<<(input-101) and (mouse.state);
  {$endif}
  end else // Menu buttons
    Result := Integer(pushedm(input));

end;


function FingersActive: integer;
begin
  Result := SDL_GetNumTouchFingers(input_touchdevice);
end;



// Elementos touch (joystick virtual sobre pantalla)
constructor TdiyenTouchList.Create();
var
  i, b : byte;
begin
  SetLength(touch, DIYEN_LMAX_TOUCHS+1);
  SetLength(vu_borrados, DIYEN_LMAX_TOUCHS+1);
  for i := 0 to DIYEN_LMAX_TOUCHS do touch[i] := nil;
  vmax := 0;
  vi_uborrado := 0;

  for i := 0 to DIYEN_MAX_DEVICES do
    for b := 1 to DIYEN_MAX_MAPPINGS do
      touch_map[i][b] := 0;
end;
destructor TdiyenTouchList.Destroy();
var
  i : byte;
begin
  for i:=vmax downto 0 do
    if (touch[i] <> nil) then TTouch(touch[i]).Destroy;
  SetLength(touch, 0);
  SetLength(vu_borrados, 0);
end;

function TdiyenTouchList.Read( index: byte ): TTouch;
begin
  Result := TTouch(touch[index]);
end;


function TdiyenTouchList.add_touch( touchp: PTouch ): byte;
var
  iborrado : byte;
begin
  if vi_uborrado > 0 then
  begin
    // Cual esta borrado?
    iborrado := vu_borrados[vi_uborrado];
    Dec(vi_uborrado); // Ya no esta mas borrado
    touch[iborrado] := touchp;
    Result := iborrado; // Este es el indice
  end else
  begin
    Inc(vmax);
    touch[vmax] := touchp;
    Result := vmax;
  end;
end;

procedure TdiyenTouchList.del_touch( touchid: byte );
var
  i : byte;
begin
  if ( touchid > 0 ) then
  begin
    if (touch[touchid] <> nil) then TTouch(touch[touchid]).Destroy;
    touch[touchid] := nil;

    // Era el ultimo?
    if touchid = vmax then
    begin
      // Entonces ahora ya no (esta borrado)
      Dec(vmax);
    end else
    begin // No era el ultimo
      Inc(vi_uborrado); // Entonces se agrega a los borrados
      vu_borrados[vi_uborrado] := touchid; // Y fue este
    end;
  end else
  // Borrar todos (touchid := 0)
  begin
    for i := vmax downto 0 do
    begin
      if (touch[i] <> nil) then TTouch(touch[i]).Destroy;
      touch[i] := nil;
    end;
    vmax := 0;
    vi_uborrado := 0;
  end;
end;


procedure TTouch.RenderTouch;
var
  map : TMap = nil;
  map_stick : TMap = nil;
  dstrect : TSDL_Rect;
  tgraph : PTouchGraph;
begin
  if pushed and (tgraph_pushed.graphID > 0) then
    tgraph := @tgraph_pushed
  else
  if overed and (tgraph_over.graphID > 0) then
    tgraph := @tgraph_over
  else
    tgraph := @tgraph_default;


  if tgraph^.graphID > 0 then
    map := TMap(TFPG(diyen_fpg_list.fpg[tgraph^.fileID]).maps[tgraph^.graphID]);

  // Dibujamos el fondo
  if map <> nil then
  begin
    dstrect.x := area.x;
    dstrect.y := area.y;
    dstrect.w := map.width;
    dstrect.h := map.height;

    SDL_SetTextureAlphaMod(map.texture, tgraph^.alpha);
    SDL_SetTextureColorMod(map.texture, tgraph^.color.r, tgraph^.color.g, tgraph^.color.b);
    SDL_SetTextureBlendMode(map.texture, tgraph^.blend);
    SDL_RenderCopy( gInternal.renderer,
                    map.texture,
                    nil, @dstrect );
  end;

  // Dibujamos el stick
  if ttype = TOUCH_ANALOGXY then
    if tgraph_stick.graphID > 0 then
    begin
      map_stick := TMap(TFPG(diyen_fpg_list.fpg[tgraph_stick.fileID]).maps[tgraph_stick.graphID]);
      dstrect.x := area.x+area.w div 2+Trunc(dx*area.w) div 2-map_stick.width div 2;
      dstrect.y := area.y+area.h div 2+Trunc(dy*area.h) div 2-map_stick.width div 2;
      dstrect.w := map_stick.width;
      dstrect.h := map_stick.height;

      SDL_SetTextureAlphaMod(map_stick.texture, tgraph_stick.alpha);
      SDL_SetTextureColorMod(map_stick.texture, tgraph_stick.color.r, tgraph_stick.color.g, tgraph_stick.color.b);
      SDL_SetTextureBlendMode(map_stick.texture, tgraph_stick.blend);
      SDL_RenderCopy( gInternal.renderer,
                      map_stick.texture,
                      nil, @dstrect );
    end;
end;


function touch(kind: TTouchType; device, button: byte; x, y, w, h: integer; fileID, graphID, graphIDstick: word): byte;
var
  TouchC : TTouch;
  graph : TTouchGraph;
begin
  TouchC := TTouch.Create;

  TouchC.ttype := kind;
  TouchC.device:=device;
  TouchC.button:=button;
  TouchC.area.x := x;
  TouchC.area.y := y;
  TouchC.area.w := w;
  TouchC.area.h := h;

  graph.fileID := fileID;
  graph.graphID := graphID;
  graph.blend := BLENDMODE_BLEND;
  graph.color.r := 255;
  graph.color.g := 255;
  graph.color.b := 255;
  graph.alpha := 255;
  TouchC.tgraph_default := graph;

  graph.fileID := fileID;
  graph.graphID := graphID;
  graph.blend := BLENDMODE_BLEND;
  graph.color.r := 200;
  graph.color.g := 200;
  graph.color.b := 200;
  graph.alpha := 255;
  TouchC.tgraph_over := graph;


  graph.fileID := fileID;
  graph.graphID := graphID;
  graph.blend := BLENDMODE_ADD;
  graph.color.r := 255;
  graph.color.g := 255;
  graph.color.b := 255;
  graph.alpha := 255;
  TouchC.tgraph_pushed := graph;

  graph.fileID := fileID;
  graph.graphID := graphIDstick;
  graph.blend := BLENDMODE_BLEND;
  graph.color.r := 255;
  graph.color.g := 255;
  graph.color.b := 255;
  graph.alpha := 255;
  TouchC.tgraph_stick := graph;

  Result := diyen_touchs_list.add_touch(PTouch(TouchC));
end;
function touch(kind: TTouchType; device, button: byte; x, y, w, h: integer; fileID, graphID: word): byte;
begin
  Result := touch(kind, device, button, x, y, w, h, fileID, graphID, 0);
end;

function TTT( touchid: byte ): TTouch;
begin
  Result := TTouch(diyen_touchs_list[touchid]);
end;

procedure move_touch( touchid: byte; x, y: integer );
var
  touch : TTouch;
begin
  touch := TTouch(diyen_touchs_list[touchid]);
  touch.area.x := x;
  touch.area.y := y;
end;

procedure delete_touch( touchid: byte );
var
  dev, but : integer;
begin
  if touchid > 0 then
  with TTouch(diyen_touchs_list[touchid]) do
  // Desactivamos la pulsación del botón
    diyen_touchs_list.touch_map[device][button] := 0
  else
    // Acá hay que desactivar todas las pulsaciones ya que borraremos todos
    for dev := 0 to DIYEN_MAX_DEVICES do
      for but := 1 to DIYEN_MAX_MAPPINGS do
        diyen_touchs_list.touch_map[dev][but] := 0;

  diyen_touchs_list.del_touch(touchid);
end;




// Genera una configuración por defecto
procedure diyen_input_default(def_dev : integer);
var
	m, n : integer;
begin
  with input_config do
  for n:=0 to DIYEN_MAX_DEVICES do
  begin
    //Reset all for this device
    for m:=1 to DIYEN_MAX_MAPPINGS do
    begin
  	map[n].key0[m].k := 0;
  	map[n].key1[m].k := 0;
  	map[n].joy0[m].d := n;
  	map[n].joy0[m].j := 254;
  	map[n].joy1[m].d := n;
  	map[n].joy1[m].j := 254;
  	//map[n].joy2[m].d := 0;
  	//map[n].joy2[m].j := 254;
    end;
  def_dev := n;

  if (n = 0) then
  begin
    map[n].key0[BUTTON_A].k	:= _n;
  	map[n].key1[BUTTON_A].k	:= _n;
    map[n].key0[BUTTON_B].k	:= _k;
  	map[n].key1[BUTTON_B].k	:= _k;
    map[n].key0[BUTTON_X].k	:= _h;
  	map[n].key1[BUTTON_X].k	:= _h;
  	map[n].key0[BUTTON_Y].k	:= _u;
  	map[n].key1[BUTTON_Y].k	:= _u;

  	map[n].key0[BUTTON_L1].k  := _L_SHIFT;
    map[n].key0[BUTTON_R1].k  := _L_ALT;

    map[n].key0[BUTTON_L3].k  := _ENTER;

  	map[n].key0[BUTTON_DUP].k     := _s;
  	map[n].key0[BUTTON_DDOWN].k   := _x;
  	map[n].key0[BUTTON_DLEFT].k   := _z;
  	map[n].key0[BUTTON_DRIGHT].k  := _c;
  end;
  if (n = 1) then
  begin
    map[n].key0[BUTTON_B].k	:= _C_RIGHT;
  	map[n].key1[BUTTON_B].k	:= _C_RIGHT;
  	map[n].key0[BUTTON_A].k	:= _C_DOWN;
  	map[n].key1[BUTTON_A].k	:= _C_DOWN;
    map[n].key0[BUTTON_X].k	:= _C_LEFT;
  	map[n].key1[BUTTON_X].k	:= _C_LEFT;
  	map[n].key0[BUTTON_Y].k	:= _C_UP;
  	map[n].key1[BUTTON_Y].k	:= _C_UP;


  	map[n].key0[BUTTON_L1].k      := _C_HOME;
    map[n].key0[BUTTON_R1].k      := _C_PGDN;
  	map[n].key0[BUTTON_DUP].k     := _up;
  	map[n].key0[BUTTON_DDOWN].k   := _down;
  	map[n].key0[BUTTON_DLEFT].k   := _left;
  	map[n].key0[BUTTON_DRIGHT].k  := _right;
  end;


  // Defaults para la OUYA
  map[n].joy0[BUTTON_A].d	:= def_dev;
  map[n].joy0[BUTTON_A].j	:= 0;
  map[n].joy0[BUTTON_B].d	:= def_dev;
	map[n].joy0[BUTTON_B].j	:= 1;
  map[n].joy0[BUTTON_X].d	:= def_dev;
  map[n].joy0[BUTTON_X].j	:= 2;
  map[n].joy0[BUTTON_Y].d	:= def_dev;
	map[n].joy0[BUTTON_Y].j	:= 3;

  map[n].joy0[BUTTON_L1].d	:= def_dev;
	map[n].joy0[BUTTON_L1].j	:= 9;

  map[n].joy0[BUTTON_R1].d	:= def_dev;
	map[n].joy0[BUTTON_R1].j	:= 10;

  map[n].joy0[BUTTON_L3].d	:= def_dev;
	map[n].joy0[BUTTON_L3].j	:= 7;
  map[n].joy0[BUTTON_R3].d	:= def_dev;
	map[n].joy0[BUTTON_R3].j	:= 8;

	map[n].joy0[BUTTON_DUP].d		:= def_dev;
	map[n].joy0[BUTTON_DUP].j		:= 11;
	map[n].joy0[BUTTON_DDOWN].d	:= def_dev;
	map[n].joy0[BUTTON_DDOWN].j	:= 12;
	map[n].joy0[BUTTON_DLEFT].d	:= def_dev;
	map[n].joy0[BUTTON_DLEFT].j	:= 13;
	map[n].joy0[BUTTON_DRIGHT].d	:= def_dev;
	map[n].joy0[BUTTON_DRIGHT].j	:= 14;

  // FIXME: En Android el acelerómetro funciona como ejes de joystick!
  map[n].joy0[BUTTON_AX].d		  := def_dev;
	map[n].joy0[BUTTON_AX].j		  := 0;
	map[n].joy0[BUTTON_AY].d		  := def_dev;
	map[n].joy0[BUTTON_AY].j		  := 1;

  // FIXME: En Android estos ejes también funcionan pero no sé de donde provienen.
  (*map[n].joy0[BUTTON_A2X].d		  := def_dev;
	map[n].joy0[BUTTON_A2X].j		  := 2;
	map[n].joy0[BUTTON_A2Y].d		  := def_dev;
	map[n].joy0[BUTTON_A2Y].j		  := 3;*)
	end;
end;


//
(*function JoystickGUIDtoString( jguid: TSDL_JoystickGUID ): String;
var
  n : integer;
begin
  Result := '';
  for n:=0 to 15 do
    Result := Result + Char(jguid.data[n]);
end;*)

// Loads all the joystick mapping from a file DB
// El formato es el de SDL_gamecontroller
procedure LoadMappingsFromFile( filedb: String );
begin

end;

function TdiyenInputConfig.DevToString(dev: integer): String;
begin
  Result:='Player '+IntToStr(dev);
end;

function TdiyenInputConfig.MappingValueString(index: integer; mapping: integer): String;
begin
  Result := '';
  case index of
    0: Result:=Result+'key0-'+MappingToString[mapping];
    1: Result:=Result+'key1-'+MappingToString[mapping];

    2: Result:=Result+'joy0-'+MappingToString[mapping]+'-dev';
    3: Result:=Result+'joy0-'+MappingToString[mapping];

    4: Result:=Result+'joy1-'+MappingToString[mapping]+'-dev';
    5: Result:=Result+'joy1-'+MappingToString[mapping];
  end;
end;

procedure TdiyenInputConfig.Save();
var
  n, m: integer;
begin
  for n:=0 to DIYEN_MAX_DEVICES do
    for m:=1 to DIYEN_MAX_MAPPINGS do
    begin
      if map[n].key0[m].k <> 0 then
        Int[DevToString(n), MappingValueString(0, m)] := map[n].key0[m].k;
      if map[n].key1[m].k <> 0 then
        Int[DevToString(n), MappingValueString(1, m)] := map[n].key1[m].k;

      if map[n].joy0[m].j <> 254 then
      begin
        Int[DevToString(n), MappingValueString(2, m)] := map[n].joy0[m].d;
        Int[DevToString(n), MappingValueString(3, m)] := map[n].joy0[m].j;
      end;
      if map[n].joy1[m].j <> 254 then
      begin
        Int[DevToString(n), MappingValueString(4, m)] := map[n].joy1[m].d;
        Int[DevToString(n), MappingValueString(5, m)] := map[n].joy1[m].j;
      end;
    end;

  inherited Save('input.map');
end;

procedure TdiyenInputConfig.Load();
var
  n, m: integer;
  //StringToMapping: TStringList;
begin
  inherited Load('input.map');

  (*StringToMapping := TStringList.Create;
  for m := 1 to DIYEN_MAX_MAPPINGS do
    StringToMapping.Add(MappingToString[m]);*)

  for n:=0 to DIYEN_MAX_DEVICES do
    for m:=1 to DIYEN_MAX_MAPPINGS do
    begin
      // Reset to no active values
    	map[n].key0[m].k := 0;
    	map[n].key1[m].k := 0;
    	map[n].joy0[m].d := n;
    	map[n].joy0[m].j := 254;
    	map[n].joy1[m].d := n;
    	map[n].joy1[m].j := 254;

      if Int[DevToString(n), MappingValueString(0, m)] <> 0 then
        map[n].key0[m].k := Int[DevToString(n), MappingValueString(0, m)];
      if Int[DevToString(n), MappingValueString(1, m)] <> 0 then
        map[n].key1[m].k := Int[DevToString(n), MappingValueString(1, m)];

      if Int[DevToString(n), MappingValueString(3, m)] <> 254 then
      begin
        map[n].joy0[m].d := Int[DevToString(n), MappingValueString(2, m)];
        map[n].joy0[m].j := Int[DevToString(n), MappingValueString(3, m)];
      end;

      if Int[DevToString(n), MappingValueString(5, m)] <> 254 then
      begin
        map[n].joy1[m].d := Int[DevToString(n), MappingValueString(4, m)];
        map[n].joy1[m].j := Int[DevToString(n), MappingValueString(5, m)];
      end;
    end;

  //StringToMapping.Free;
end;

// Inicializa la entrada para diyen
procedure diyen_input_init();
var
  n : integer;
  //ctrl : PSDL_GameController;
begin
  //SDL_InitSubSystem(SDL_INIT_JOYSTICK);
  input_config := TdiyenInputConfig.Create;

  input_joysticks_len := SDL_NumJoysticks();
  SetLength(input_joysticks, input_joysticks_len);
  for n := 0 to input_joysticks_len-1 do
  begin
    //if (SDL_IsGameController(n) then
    //begin
    //  ctrl := SDL_GameControllerOpen(n);
    //  input_joy[n] := SDL_GameControllerGetJoystick(ctrl);
    //end else
      {$ifdef ANDROID_OLD}
      // La última versión de SDLActivity.java entrega la lista
      // invertida alegando a una compatibilidad con XBox 360.
      //input_joy[input_joylen-1-n] := SDL_JoystickOpen(n);
      {$else}
      input_joysticks[n] := SDL_JoystickOpen(n);
      //say('Input joystick device '+IntToStr(n)+' GUID: '+JoystickGUIDtoString(SDL_JoystickGetDeviceGUID(n)));
      {$endif}
  end;

  //say('Num joysticks: '+IntToStr(SDL_NumJoysticks()));
  FingersTotal := 5;
  SetLength(finger, FingersTotal);
  for n := 0 to FingersTotal-1 do
  begin
    FillByte(finger[n], SizeOf(TMouse), 0);
  end;
  input_touchdevice := SDL_GetTouchDevice(0);

  diyen_touchs_list := TdiyenTouchList.Create;

  diyen_input_default(0); // Genera la configuración por defecto
  //input_config.Load();
end;
procedure diyen_input_finish();
begin
  SetLength(input_joysticks, 0);

  input_config.Save();
  input_config.Free;

  diyen_touchs_list.Free;

  SetLength(finger, 0);
end;

end.

