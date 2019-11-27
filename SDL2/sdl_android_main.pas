{***
  It appears to be impossible for Pascal syntaxis to mix two units to make
  a library like you can do in C or C++.

  The SDL_Main function declaration never can be satisfied (otherwise you
  mix it in only one file). Or you get errors in the main project file
  related to "BEGIN" expected but "INTERFACE" found. So that don't let you
  mix this two units.

  So, the only solution is to copy this file to your project file and adapt it
  to make it work.
  COPY ONLY the portions between the $IFDEF (you will need only that).
  DONT FORGET to "uses jni;"
***}

unit sdl_android_main;
{$LINKLIB c}

interface

uses jni;

const
{$IFDEF ANDROID}
  android_sdl_LibName = 'libSDL2.so';

// Called before SDL_main() to initialize JNI bindings in SDL library
procedure SDL_Android_Init(env : PJNIEnv; cls: jclass); cdecl; external android_sdl_LibName name 'SDL_Android_Init';
procedure SDL_SetMainReady(); cdecl; external android_sdl_LibName name 'SDL_SetMainReady';
function SDL_strdup(const str: PChar): PChar; cdecl; external android_sdl_LibName name 'SDL_strdup';
{$ENDIF}

implementation

{$IFDEF ANDROID}
// Start up the SDL app
//void Java_org_libsdl_app_SDLActivity_nativeInit(JNIEnv* env, jclass cls, jobject obj)
procedure Java_org_libsdl_app_SDLActivity_nativeInit(env : PJNIEnv; cls: jclass; obj: jobject);
var
  status : longint;
  argv: array[0..1] of PChar;
begin
    // This interface could expand with ABI negotiation, calbacks, etc.
    SDL_Android_Init(env, cls);

    SDL_SetMainReady();

    // Run the application code!
    argv[0] := SDL_strdup('SDL_app');
    argv[1] := '';
    status := SDL_main(1, argv);

    // Do not issue an exit or the whole application will terminate instead of just the SDL thread
    //exit(status);
end;

exports Java_org_libsdl_app_SDLActivity_nativeInit name 'Java_org_libsdl_app_SDLActivity_nativeInit';
{$ENDIF}



begin
end.
