{*/from SDL_log.h
	Copyright (C) 2013 Anyeos
(*
  Simple DirectMedia Layer
  Copyright (C) 1997-2013 Sam Lantinga <slouken@libsdl.org>

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
**}

(**
 *  \file SDL_log.h
 *
 *  Simple log messages with categories and priorities.
 *
 *  By default logs are quiet, but if you're debugging SDL you might want:
 *
 *      SDL_LogSetAllPriority(SDL_LOG_PRIORITY_WARN);
 *
 *  Here's where the messages go on different platforms:
 *      Windows: debug output stream
 *      Android: log output
 *      Others: standard error output (stderr)
 *)

const
(**
 *  \brief The maximum size of a log message
 *
 *  Messages longer than the maximum size will be truncated
 *)
SDL_MAX_LOG_MESSAGE=4096;

(**
 *  \brief The predefined log categories
 *
 *  By default the application category is enabled at the INFO level,
 *  the assert category is enabled at the WARN level, test is enabled
 *  at the VERBOSE level and all other categories are enabled at the
 *  CRITICAL level.
 *)

    SDL_LOG_CATEGORY_APPLICATION=1;
    SDL_LOG_CATEGORY_ERROR=2;
    SDL_LOG_CATEGORY_ASSERT=3;
    SDL_LOG_CATEGORY_SYSTEM=4;
    SDL_LOG_CATEGORY_AUDIO=5;
    SDL_LOG_CATEGORY_VIDEO=6;
    SDL_LOG_CATEGORY_RENDER=7;
    SDL_LOG_CATEGORY_INPUT=8;
    SDL_LOG_CATEGORY_TEST=9;

    {* Reserved for future SDL library use *}
    SDL_LOG_CATEGORY_RESERVED1=10;
    SDL_LOG_CATEGORY_RESERVED2=11;
    SDL_LOG_CATEGORY_RESERVED3=12;
    SDL_LOG_CATEGORY_RESERVED4=13;
    SDL_LOG_CATEGORY_RESERVED5=14;
    SDL_LOG_CATEGORY_RESERVED6=15;
    SDL_LOG_CATEGORY_RESERVED7=16;
    SDL_LOG_CATEGORY_RESERVED8=17;
    SDL_LOG_CATEGORY_RESERVED9=18;
    SDL_LOG_CATEGORY_RESERVED10=19;

    (* Beyond this point is reserved for application use, e.g.
       enum {
           MYAPP_CATEGORY_AWESOME1 = SDL_LOG_CATEGORY_CUSTOM,
           MYAPP_CATEGORY_AWESOME2,
           MYAPP_CATEGORY_AWESOME3,
           ...
       };
     *)
    SDL_LOG_CATEGORY_CUSTOM=20;

(**
 *  \brief The predefined log priorities
 *)
type 
	TSDL_LogPriority = (
    SDL_LOG_PRIORITY_VERBOSE = 1,
    SDL_LOG_PRIORITY_DEBUG,
    SDL_LOG_PRIORITY_INFO,
    SDL_LOG_PRIORITY_WARN,
    SDL_LOG_PRIORITY_ERROR,
    SDL_LOG_PRIORITY_CRITICAL,
    SDL_NUM_LOG_PRIORITIES);


(**
 *  \brief Set the priority of all log categories
 *)
//extern DECLSPEC void SDLCALL SDL_LogSetAllPriority(SDL_LogPriority priority);
procedure SDL_LogSetAllPriority(priority: TSDL_LogPriority); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogSetAllPriority' {$ENDIF} {$ENDIF};

(**
 *  \brief Set the priority of a particular log category
 *)
//extern DECLSPEC void SDLCALL SDL_LogSetPriority(int category,
//                                                SDL_LogPriority priority);
procedure SDL_LogSetPriority(category: SInt32; priority: TSDL_LogPriority); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogSetPriority' {$ENDIF} {$ENDIF};

(**
 *  \brief Get the priority of a particular log category
 *)
//extern DECLSPEC SDL_LogPriority SDLCALL SDL_LogGetPriority(int category);
function SDL_LogGetPriority( category: SInt32 ): TSDL_LogPriority; cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogGetPriority' {$ENDIF} {$ENDIF};

(**
 *  \brief Reset all priorities to default.
 *
 *  \note This is called in SDL_Quit().
 *)
//extern DECLSPEC void SDLCALL SDL_LogResetPriorities(void);
procedure SDL_LogResetPriorities(); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogResetPriorities' {$ENDIF} {$ENDIF};

(**
 *  \brief Log a message with SDL_LOG_CATEGORY_APPLICATION and SDL_LOG_PRIORITY_INFO
 *)
//extern DECLSPEC void SDLCALL SDL_Log(const char *fmt, ...);
procedure SDL_Log( const fmt: PChar ); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_Log' {$ENDIF} {$ENDIF};

(**
 *  \brief Log a message with SDL_LOG_PRIORITY_VERBOSE
 *)
//extern DECLSPEC void SDLCALL SDL_LogVerbose(int category, const char *fmt, ...);
procedure SDL_LogVerbose(category: SInt32; const fmt: PChar); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogVerbose' {$ENDIF} {$ENDIF};

(**
 *  \brief Log a message with SDL_LOG_PRIORITY_DEBUG
 *)
//extern DECLSPEC void SDLCALL SDL_LogDebug(int category, const char *fmt, ...);
procedure SDL_LogDebug(category: SInt32; const fmt: PChar); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogDebug' {$ENDIF} {$ENDIF};

(**
 *  \brief Log a message with SDL_LOG_PRIORITY_INFO
 *)
//extern DECLSPEC void SDLCALL SDL_LogInfo(int category, const char *fmt, ...);
procedure SDL_LogInfo(category: SInt32; const fmt: PChar); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogInfo' {$ENDIF} {$ENDIF};

(**
 *  \brief Log a message with SDL_LOG_PRIORITY_WARN
 *)
//extern DECLSPEC void SDLCALL SDL_LogWarn(int category, const char *fmt, ...);
procedure SDL_LogWarn(category: SInt32; const fmt: PChar); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogWarn' {$ENDIF} {$ENDIF};

(**
 *  \brief Log a message with SDL_LOG_PRIORITY_ERROR
 *)
//extern DECLSPEC void SDLCALL SDL_LogError(int category, const char *fmt, ...);
procedure SDL_LogError(category: SInt32; const fmt: PChar); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogError' {$ENDIF} {$ENDIF};

(**
 *  \brief Log a message with SDL_LOG_PRIORITY_CRITICAL
 *)
//extern DECLSPEC void SDLCALL SDL_LogCritical(int category, const char *fmt, ...);
procedure SDL_LogCritical(category: SInt32; const fmt: PChar); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogCritical' {$ENDIF} {$ENDIF};

(**
 *  \brief Log a message with the specified category and priority.
 *)
//extern DECLSPEC void SDLCALL SDL_LogMessage(int category,
//                                            SDL_LogPriority priority,
//                                            const char *fmt, ...);
procedure SDL_LogMessage(category: SInt32; priority: TSDL_LogPriority; const fmt: PChar); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogMessage' {$ENDIF} {$ENDIF};

(**
 *  \brief Log a message with the specified category and priority.
 *)
//extern DECLSPEC void SDLCALL SDL_LogMessageV(int category,
//                                             SDL_LogPriority priority,
//                                             const char *fmt, va_list ap);
//procedure SDL_LogMessageV(category: SInt32; priority: TSDL_LogPriority; const fmt: PChar; ap: va_list); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogMessageV' {$ENDIF} {$ENDIF};

(**
 *  \brief The prototype for the log output function
 *)
//typedef void (*SDL_LogOutputFunction)(void *userdata, int category, SDL_LogPriority priority, const char *message);
type
TSDL_LogOutputFunction = procedure(userdata: Pointer; category: SInt32; priority: TSDL_LogPriority; const message: PChar);

(**
 *  \brief Get the current log output function.
 *)
//extern DECLSPEC void SDLCALL SDL_LogGetOutputFunction(SDL_LogOutputFunction *callback, void **userdata);
procedure SDL_LogGetOutputFunction( callback: TSDL_LogOutputFunction; userdata: PPointer); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogGetOutputFunction' {$ENDIF} {$ENDIF};

(**
 *  \brief This function allows you to replace the default log output
 *         function with one of your own.
 *)
//extern DECLSPEC void SDLCALL SDL_LogSetOutputFunction(SDL_LogOutputFunction callback, void *userdata);
procedure SDL_LogSetOutputFunction( callback: TSDL_LogOutputFunction; userdata: Pointer); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogSetOutputFunction' {$ENDIF} {$ENDIF};

