(*
  DIYEN MAIN INCLUDE FILE

*)

const

  // Maximums
  // LMAX_PROCESS debe dejar "espacio" si deseamos usar TILEMAPS.
  //DIYEN_LMAX_PROCESS   = 32767;
  DIYEN_LMAX_PROCESS   = 4096;
  //DIYEN_LMAX_PROCESS   = 8192;
  //DIYEN_LMAX_PROCESS   = 2048;
  DIYEN_LMAX_REGIONS   = 31;
  DIYEN_LMAX_SCROLLS   = 9;

  DIYEN_TABLES_PRECISION  = 360;
  DIYEN_ANG_STEP          = 360000 div DIYEN_TABLES_PRECISION;

  DIYEN_DEFAULT_BLENDMODE = SDL_BLENDMODE_BLEND;

  // graph_mode
  // Color depths constants:
  (*MODE_8BITS    = 8;
  MODE_8BPP     = 8;
  MODE_16BITS   = 16;
  MODE_16BPP    = 16;
  // Render flags constants:
  MODE_WINDOW         = $000000;
  MODE_2XSCALE        = $000100;
  MODE_FULLSCREEN     = $000200;
  MODE_DOUBLEBUFFER   = $000400;
  MODE_HARDWARE       = $000800;
  MODE_MODAL          = $001000;
  MODE_FRAMELESS      = $002000;*)

  // OS id
  OS_WIN32      = 0;
  OS_LINUX      = 1;
  OS_BEOS       = 2;
  OS_MACOS      = 3;
  OS_GP32       = 4;
  OS_DC         = 5;
  OS_BSD        = 6;
  OS_GP2X       = 7;
  OS_GP2X_WIZ   = 8;
  OS_CAANOO     = 9;
  OS_DINGUX_A320= 10;
  OS_WII        = 1000;
  OS_PSP        = 1001;
  OS_IOS        = 1002;
  OS_ANDROID    = 1003;


  ONE_SECOND = (1000 / MSecsPerDay);


  // Modos blend
  BLENDMODE_NONE  = SDL_BLENDMODE_NONE;
  BLENDMODE_BLEND = SDL_BLENDMODE_BLEND;
  BLENDMODE_ADD   = SDL_BLENDMODE_ADD;
  BLENDMODE_MOD   = SDL_BLENDMODE_MOD;

  // Flips
  FLIP_NONE       = SDL_FLIP_NONE;
  FLIP_HORIZONTAL = SDL_FLIP_HORIZONTAL;
  FLIP_VERTICAL   = SDL_FLIP_VERTICAL;

  // Bit Masks
  {$ifndef ANDROID}
  // Estas son las máscaras para x86
  // Aparentemente lo más común es ARGB (en little endian)
  AMASK = $ff000000;
  RMASK = $00ff0000;
  GMASK = $0000ff00;
  BMASK = $000000ff;
  PIXEL_FORMAT = SDL_PIXELFORMAT_ARGB8888;
  {$else}
  AMASK = $ff000000;
  RMASK = $000000ff;
  GMASK = $0000ff00;
  BMASK = $00ff0000;
  PIXEL_FORMAT = SDL_PIXELFORMAT_ABGR8888;
  {$endif}

type

  // DIV types compatibility
  int = integer;


type
  // Para configurar los hints
  Thint = (HINT_ACCEL,    // Use hardware 3D acceleration
           HINT_DRIVER,   // Set the driver to use
           HINT_SHADERS,  // Use OpenGL Shaders
           HINT_QUALITY,  // Enable better quality
           HINT_VSYNC,    // Sync with vertical retrace
           HINT_GRAB,     // Grab the keyboard
           HINT_MINIMIZE, // Minimize if focus loss
           HINT_IOSTIMER,  // Disable the idle timer in iOS
           HINT_IOS_ORIENT,// Orientation in iOS: 'LandscapeLeft', 'LandscapeRight', 'Portrait', 'PortraitUpsideDown'
           HINT_XINPUT,   // Use xtimer input
           HINT_ALLOWJOYBG,  // Allow joystick background events
           HINT_TIMERRES,  // up to Windows 7 autoset system timer resolution
           HINT_ACCELASJOY, // List real joysticks along with the accelerometer as if it were a 3 axis joystick (the default)
           HINT_REMOTEASJOY, // TV Remote (AndroidTv) as Joystick (default '1')
           HINT_TOUCHMOUSE, // Touch events will generate mouse events (default '1')
           HINT_ASEPARATEMT, // Android separate mouse and touch events (default '0')
           HINT_GLESDRIVER // Load OpenGL ES library using the default library names (default '0')
           );

  // De uso general
  //TPWdiyenList = array of word;
  //TPBdiyenList = array of byte;

const
  Thinttostr: array[THint] of string = (
                SDL_HINT_FRAMEBUFFER_ACCELERATION,
                SDL_HINT_RENDER_DRIVER,
                SDL_HINT_RENDER_OPENGL_SHADERS,
                SDL_HINT_RENDER_SCALE_QUALITY,
                SDL_HINT_RENDER_VSYNC,
                SDL_HINT_GRAB_KEYBOARD,
                SDL_HINT_VIDEO_MINIMIZE_ON_FOCUS_LOSS,
                SDL_HINT_IDLE_TIMER_DISABLED,
                SDL_HINT_ORIENTATIONS,
                SDL_HINT_XINPUT_ENABLED,
                SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS,
                SDL_HINT_TIMER_RESOLUTION,
                (* Habría que actualizar las unit SDL para pascal *)
                'SDL_ACCELEROMETER_AS_JOYSTICK',
                'SDL_TV_REMOTE_AS_JOYSTICK',
                'SDL_TOUCH_MOUSE_EVENTS',
                'SDL_ANDROID_SEPARATE_MOUSE_AND_TOUCH',
                'SDL_OPENGL_ES_DRIVER'
                );

type
  // Status used in local STATUS
  Tpstatus = (
  STATUS_UNSTARTED := 0,
  STATUS_STARTED,
  STATUS_RUNNING,
  STATUS_SLEEPING,
  STATUS_FROZEN,
  STATUS_KILL,
  STATUS_DEAD);

  // Signals to send
  Tpsignal = (
  S_KILL := 0,
  S_WAKEUP,
  S_SLEEP,
  S_FREEZE);

  // TScaler
  // Un escalador matemático (porque el escalado gráfico se hace siempre)
  // Este tipo posee unas funciones prácticas para el escalado
  // Nota: SDL2 hace exactamente lo mismo, trae un escalador matemático.
  {$ifdef USE_SCALER}
  TScaler = class
    private
      venabled : boolean;
      procedure Enable( activar: boolean );
    public
      {$ifdef USE_SCALER_SDL}
      sdl_scale : float;
      {$else}
      scale   : integer;
      dstrect : TSDL_Rect;
      procedure scaleit(orig: PSDL_Rect; center: PSDL_Point);
      {$endif}

      procedure init( ow, oh, sw, sh : integer );
      property enabled: boolean read venabled write Enable;
  end;
  {$endif}
    TSinglePoint = record
      x, y : single;
    end;

    TdiyenGlobalInternal = record
      // for private use (only system)
      // touching this vars can crash the engine
      expected_fps         : word;
      expected_skip        : word;

      title             : String;

      window            : PSDL_Window;
      renderer          : PSDL_Renderer;
      render_info       : TSDL_RendererInfo;
      lscreen           : TSDL_Rect;

      {$ifndef NO_BACKGROUND_GRAPH}
      //sbackground       : PSDL_Surface;
      background        : PSDL_Texture;
      {$endif}
      clearcolor        : TSDL_Color;

      program_next_tick: longint;

      {$ifdef USE_PRIORITY}
      priority_ordered : boolean;
      priority_min     : smallint;
      {$endif}

      {$ifndef NO_FX_ENGINE}
      fader          : TFader;
      {$endif}

      frames         : integer;
      frames_skip    : word;
      frames_1second : longint;
      last_tick      : longint;
      fullscreen     : boolean;

      //modeW, modeH  : integer;
      DisplayBounds : TSDL_Rect;
      vp  : TSDL_Rect; // ViewPort
      //scale    : TSinglePoint;
      //nofs_scale : TSinglePoint; // Escala al no estar en fullscreen
      scaling : Boolean;
      //fsTexture     : PSDL_Texture;


      {$ifdef DIYEN_PAUSE}
      processing : boolean;
      {$endif}

      {$ifdef ZLAYER}
      zlayer_auto      : boolean;
      {$ifdef ZLAYER_RUNTIME}
      use_zlayer       : boolean;
      {$endif}
      {$endif}
    end;


// Global system vars
var
  // You can use this vars
  // Estas ya están definidas en free pascal
  //argc              : int;
  //argv              : array of string;

  exit_status       : int = 0;

  fps               : int = 0;

  //graph_mode        : word = 0;
  //restore_type      : word = 0;

  Timer             : array[0..9] of integer = (0,0,0,0,0,0,0,0,0,0);


  // diyen engine features
  collision_z       : word = 65535; // Diferencia en ±Z para colisionar

  os_id             : word = 0;

  software_mode     : boolean = false;
  force_accel       : boolean = false;

  {$ifdef USE_SCALER}
  scaler            : TScaler;
  {$endif}


type

  // Region
  // Las regiones son una parte rectangular de la pantalla donde aparecerán los
  // gráficos. Fuera de esa región no se verán.
  // Sirve para definir tipo ventanas a donde limitar la aparición de gráficos
  // Se definen dos esquinas, la superior izquierda, y la inferior derecha.

  // En DIYEN se podrían poner rotadas, escaladas y espejadas pero preferí
  // utilizar la función RenderClipRect de SDL para simular una región en lugar
  // de una textura como destino.
  // De todos modos dejo el código comentado por si se quiere implementar esa
  // funcionalidad más adelante.
  // Nota: No sé si todos los chips de video la soporten.
  PRegion = ^TRegion;
  TRegion = record
    x, y : integer;
    w, h : integer;
    //texture : PSDL_Texture;
    //angle : integer;
    //flip : byte;
    //stretch : boolean;
  end;

  // Scroll
  // Los scrolls son regiones con graficos que se desplazan dando un efecto
  // de como si el escenario fuera mucho más grande que la pantalla.
  // En realidad permite eso, hacer escenarios más grandes que la pantalla
  // y que puedan ser visualizados como si fuera una cámara que sólo filma
  // en el tamaño de la pantalla (se puede usar una región).
  Tscrolllock = ( SN_NONE := 0,
                  SN_H,
                  SN_V,
                  SN_BH,
                  SN_BV,
                  SN_B2H,
                  SN_B2V );

  TSetscrolllock = set of Tscrolllock;

  PScroll = ^TScroll;
  TScroll = record
      x0, y0 : int;
      x1, y1 : int;
      x2, y2 : int;
      z      : smallint;
      camera : word;
      ratio, ratio2 : int;
      speed  : word;
      region1 : shortint;
      region2 : shortint;

      sregion : byte;
      lock    : TSetScrollLock;
      file_   : word;

      graph0  : word;
      alpha0  : byte;
      color0  : TSDL_Color;
      blend0  : TSDL_BlendMode;
      flip0   : byte;

      graph1  : word;
      alpha1  : byte;
      color1  : TSDL_Color;
      blend1  : TSDL_BlendMode;
      flip1   : byte;

      graph2  : word;
      alpha2  : byte;
      color2  : TSDL_Color;
      blend2  : TSDL_BlendMode;
      flip2   : byte;

      drawed  : boolean;
  end;

  // tipo ctype (para no equivocarse)
  // Coordinatetype modes are used to set the type of coordinatesytem
  // to be used, by assigning them to the local variable ctype.
  Tctype = (C_SCREEN := 0, C_SCROLL);
  //C_SCREEN  = 0; // - The screen's top left corner, coordinate (0,0).
  //C_SCROLL  = 1; // - The foreground graphic of the scroll in which the process is shown.
  //C_M7      = 2;

  // tipo cnumber
  Tcnumber = (C_0 := 1, C_1, C_2, C_3, C_4, C_5, C_6, C_7, C_8, C_9);


//{$i diyen_ptypes.inc}
//var
    //PTYPE_ALL : Set of TProcessType;
(*const
    PTYPE_ALL = $ffffffff;
    PTYPE_UNDEFINED = 0;
    PTYPE_PROGRAM = 1;
    PTYPE_HELPER = 1;*)

type
  {$ifdef ZLAYER}
  TZprocess = record
    Zold: smallint;
    nZ: word;
  end;
  {$endif}


  TProcess = class;
  PProcess = ^TProcess;

  //TPPdiyenList = array of Pointer;
  TdiyenProcessInternal = record
    //pthread: PSDL_Thread;
    params        : pointer;
    //signal        : Tpsignal;
    status        : Tpstatus;
    frame_percent : word;
    process_id    : word;
    bigson        : word;
    id_scan       : word;
    id_scan_last  : word;
    class_scan_last : TClass;
    scan_collision_list : array of word;
    //col_filter    : longword;

    {$ifdef ZLAYER}
    // Para saber que quiere cambiar de capa
    Zl              : TZprocess;
    {$endif}
    //real_p        : TSDL_Point;
    //center        : TSDL_Point;
    //dstrect       : TSDL_Rect;
    {$ifndef NO_FX_ENGINE}
    fx_max          : word;
    fxlist          : array of TFX;
    {$endif}
    //process_next_tick: longint;
  end;

  // The process type
  // The first form of something processable in the game
  // it is like the process in DIV
  TProcess = class
  public
    constructor Create;
    constructor Create( Father: TProcess );
    destructor Destroy; override;

    procedure start; virtual;                    // Override this to put your initialization
    procedure start( params: pointer ); virtual;

    {$ifdef USE_THINK}
    procedure think; virtual; // Override this function and put your code there
    think_time: int; // Time to sync by the calling to think (in milliseconds)
    {$endif}
    procedure loop; virtual; // Override this function and put your code there

    procedure col(other: TProcess; nbound, nobound: word); virtual; // Collide with

    {$ifndef NO_HIT_COL}
    procedure hit(other: TProcess; nbound, nobound: word); virtual; // Hit with
    {$endif}
    {$ifdef HIT_HIT}
    procedure hithit(other: TProcess; nbound, nobound: word); virtual; // Hit with other hit
    {$endif}

    procedure died; virtual; // Override this if you want to take
                                     // an action when your process is terminated

    //function process_type: word; virtual; // Override this one and define a
                                          // unique ID for your kind of process (See diyen_ptypes.inc)
  public
  // DIV compatible LOCAL vars
    father    : word;
    son       : word;
    bigbro    : word;
    //smallbro  : word;

    //flags     : word; // ?

    alpha     : smallint;
    color     : TSDL_Color;
    blend     : TSDL_BlendMode;
    flip      : byte;

    resolution: int;
    x         : int;
    y         : int;
    z         : smallint;
    angle     : int;
    size_x    : smallint;
    size_y    : smallint;

    graph     : word;
    file_     : word; // It cannot be simply "file" because that is a Internal word in pascal
    //xgraph    : Pointer;

    {$ifdef USE_PRIORITY}
    priority  : shortint;
    {$endif}
    ctype     : Tctype;
    cnumber   : set of Tcnumber;
    region    : byte;


    //nosetvars: boolean; // Usada por si se necesita establecer algunas variables
                        // al momento de crear el proceso y que no sean
                        // establecidas en su rutina de arranque (.start).

    //{$i diyen_locals.inc}

    //function ProcessAdd( process: TProcess) : int;
    function PAdd( process: TProcess ) : word;
    function PAdd( process: TProcess; params: pointer ) : word;
    procedure frame(percent: int);
    procedure frame;
    //procedure dExit( message : string );
    //procedure dExit( message : string; code : int );
    //procedure dExit( code : int );
    //procedure dExit;

    function running: boolean;
    function sleeping: boolean;
    function frozen: boolean;

    procedure kill;
    procedure kill( pid: word );
    function collision( p : int ) : TProcess;
    function collision( pclass : TClass ) : TProcess;
    //function pcollision( p : int ) : TProcess;
    function advance( distance: int ) : boolean;
    function advance( dangle, distance: int ) : boolean;
    //function get_angle( pid : word ) : int;
    function get_angle( process: TProcess ): int;
    //function get_dist( pid : word ) : int;
    function get_dist( process: TProcess ): int;
    function get_distx( ang, dist : int ) : int;
    function get_disty( ang, dist : int ) : int;
    //function get_id( pt : int ) : int;
    procedure let_me_alone();
    procedure signal( p: int; sig: TPSignal; all: boolean );
    procedure signal( sig: TPSignal; all : boolean );
    procedure signal( p: int; sig: TPSignal);

    function get_col_bound(num: word): TBound;
    function get_hit_bound(num: word): TBound;

    {$ifndef NO_FX_ENGINE}
    procedure fx_add(fxfunc: TFXfunction; count: word; data: Pointer);
    procedure fx_add(fxfunc: TFXfunction; data: Pointer);
    procedure fx_add(fxfunc: TFXfunction);
    procedure fx_del();
    procedure fx_del(index: word);
    function particle_spawn(particleZ: smallint; particlenum: word): word;
    function particle_spawn(particleZ, particleSize: smallint; particlenum: word): word;
    {$endif}


  private
    function getsize : smallint;
    procedure putsize ( newsize : smallint );
    function GetFullScreen : boolean;
    procedure SetFullScreen ( fs : boolean );
    //function GetClearColor: TSDL_Color;
    //procedure SetClearColor( ccolor : TSDL_Color );
    {$ifdef DIYEN_PAUSE}
    function GetProcessing : boolean;
    procedure SetProcessing ( process : boolean );
    {$endif}
  public
    Internal      : TdiyenProcessInternal; // Internal diyen use only
    property id : word read Internal.process_id; // self index
    property size : smallint read getsize write putsize;
    property full_screen : boolean read GetFullScreen write SetFullScreen;
    //property clear_color : TSDL_Color read GetClearColor write SetClearColor;
    {$ifdef DIYEN_PAUSE}
    property processing : boolean read GetProcessing write SetProcessing;
    {$endif}
  end;

  TMainProgram = class(TProcess)
    procedure main; virtual; // Override this function and put your code there
  end;

  {$ifdef ZLAYER}
  TZLayer = record
    max : word;
    p   : array of word;
  end;
  {$endif}


  // FIXME: Habría que pensar en un mejor uso para Item.
  TdiyenRegionList = class
    private
      function Read( index: byte )  : PSDL_Rect;
    public
      region : array of TRegion;
      clip_region : array of TSDL_Rect;

      //alist : array of byte;
      //alist_max : smallint;
      procedure define_region( num: byte; x, y, ancho, alto : integer );
      procedure define_region( num: byte; x, y, ancho, alto, angulo : integer; flip : byte; estirada : boolean );
      function out_region(x, y, res : integer; num : byte) : boolean;
      constructor Create();
      destructor Destroy(); override;
      property Item[index: byte]: PSDL_Rect read Read; default;
  end;


  TdiyenScrollList = class
    private
      //scrolls : array of TScroll;
      function Read( index: word )  : TScroll;
      //procedure Write( index: word; scroll: TScroll );
    public
      alist : array of byte;
      alist_max : smallint;
      procedure start_scroll( scrollnumber: byte; fileID, graphID, backgroundgraphID, background2graphID: word; regionnumber: byte; lockindicator: TSetScrollLock );
      procedure start_scroll( scrollnumber: byte; fileID, graphID, backgroundgraphID: word; regionnumber: byte; lockindicator: TSetScrollLock );
      procedure stop_scroll( scrollnumber: byte );
      procedure move_scroll( scrollnumber: byte );
      constructor Create();
      destructor Destroy(); override;
      property Item[index: word]: TScroll read Read; default;
  end;


  // Lista de procesos
  TdiyenPList = class
    private
      vmax                : word; // Valor maximo alcanzado en la lista.
      vindice_uborrado    : word; // Indice de ultimo borrado
      vultimos_borrados   : array of word;  // Para agilizar la creacion de nuevos elementos.
      startl              : array of word;// Para .start
      startl_max          : word;
      endedl              : array of word;// Para pasar a ended
      endedl_max          : word;
      killl               : array of word;// Para quitar
      killl_max           : word;
      {$if not defined(ZLAYER) or defined(ZLAYER_RUNTIME)}
      zlist               : array of word; // Usada para modo computado
      zlmax               : word;
      zlmax_fixed         : word;
      {$endif}
      {$ifdef ZLAYER}
      zlayer              : array of TZLayer; // Usada para modo capas de Z
      zlayer_max          : smallint; // Valor máximo de indice de capa Z
      {$endif}
      {$ifdef USE_PRIORITY}
      //priosome_deleted    : boolean;
      priomax             : word;
      priolist            : TPWdiyenList;
      {$endif}
      function Read( index: word )  : TProcess; // Lee el elemento a partir del indice
      //procedure Write( index: word; process : TProcess ); // Escribe/Reemplaza el elemento a partir del indice
      procedure ProgramStart(main: TMainProgram);
      procedure ProcessEnd( process: TProcess );
    public
      procedure ProcessInit(process: TProcess; id, father: word; params: Pointer);
    public
      velementos          : array of Pointer;
      constructor Create;
      destructor Destroy; override;
      function Next( index: Pword ) : TProcess; // Asigna el siguiente indice valido o cero si se llego al final y devuelve el elemento.
      function Add( process: TProcess; father: word; params: pointer )  : word; // Añade y devuelve el indice
      procedure Del( index: word );  // Borra un elemento a partir del indice
      property Max : word read vmax write vmax; // Maximo indice alcanzado.
      property Item[index: word]: TProcess read Read; default;
    end;

  //procedure ProgramStart(main: TMainProgram);
var
  // for private use (only system)
  // modifying the content of this vars can crash the engine
  gInternal : TdiyenGlobalInternal;

  // Lista de Scrolls
  diyen_scroll_list : TdiyenScrollList;
  scroll : array of TScroll;

  // Regions
  regions : TdiyenRegionList;

  // Control de procesos
  diyen_process_list  : TdiyenPList;


  diyen_sin_table: array[0..DIYEN_TABLES_PRECISION] of float;
  diyen_cos_table: array[0..DIYEN_TABLES_PRECISION] of float;
  diyen_tan_table: array[0..DIYEN_TABLES_PRECISION] of float;
  //diyen_tan_table: array[0..DIYEN_TABLES_PRECISION] of float;

  // for get_id()
  //diyen_ptype_scan : array[1..32] of TPPdiyenList;

  {$ifdef ANDROID}
  background : boolean = false;
  {$endif}

// Principales
procedure diyen_main(mainprogram: TMainProgram);
//procedure diyen_create_trig_tables();
function greater( number1, number2 : integer ): integer;
function bestratio( number1, number2 : integer ): integer;
function minmax( number, min, max: integer ): integer;
function rand(i, f: integer): integer;

// Compatibility declarations (DIV like functions)
procedure dExit( message: string );
procedure dExit( message: string; code: int );
procedure dExit( code: int );
procedure dExit;

function set_mode(width, height, lwidth, lheight : int) : boolean;
function set_mode(width, height : int) : boolean;
function set_mode_auto(width, height : int) : boolean;
function set_hint(hint: Thint; value: string): boolean;
procedure set_title( title: string );
procedure set_fps( fps, skip: int);
procedure set_fps( fps: int );
procedure say( msj: string );
function PAdd( process: TProcess; father: word ): word;
function TP( pid: word ): TProcess;
function exists( var pid: word ): boolean;
procedure define_region( num, x, y, ancho, alto: int );
function out_region(x, y, res: int; num: byte) : boolean;
function out_region(pid: word; num: byte) : boolean;
function SREG( num: byte ): TSDL_Rect;
procedure start_scroll( scrollnumber, fileID, graphID, background1graphID, background2graphID, regionnumber: int; lockindicator : TSetScrollLock );
procedure start_scroll( scrollnumber, fileID, graphID, backgroundgraphID, regionnumber: int; lockindicator : TSetScrollLock );
procedure move_scroll ( scrollnumber: int );


function dcos( ang: int ): single;
function dsin( ang: int ): single;
function dtan( ang: int ): single;
function datan( y, x: int ): int;
function pow( base: single; power: int ): single;

procedure advance( var x, y: int; angle, distance: int );
function fget_dist( pa_x, pa_y, pb_x, pb_y: int ): int;
function fget_angle( pa_x, pa_y, pb_x, pb_y: int ): int;
function near_angle( oangle, dangle, increment: int ): int;

procedure stop_scroll( scrollnumber: byte );


// Graphic FXs and manipulation
//map_get_pixel
//map_put_pixel
//new_map
//map_clear
//map_put
//map_xput




(*
INT get_point ( <INT fileID> , <INT graphID> , <INT controlpointID> , <INT POINTER x>, <INT POINTER y>)
INT blur ( <INT fileID> , <INT graphID> , <BYTE mode> )
INT fpg_add ( <INT destFileID> , <INT destGraphID> , <INT origFileID> , <INT origGraphID> )

Get_glyph()
Get_id()
Get_point()
Get_screen()
Get_text_color()
Get_timer()
Getenv()

*)

{$ifdef ZLAYER}
procedure start_zlayer(zmax : integer);
procedure stop_zlayer();
procedure zlayer_automax( maxz: integer );
{$endif}

{$ifndef NO_FX_ENGINE}
procedure fx_run(p: TProcess; params: PFXparams; num: word; sc: PScroll);
{$endif}


