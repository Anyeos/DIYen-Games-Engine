{// from "SDL_hints.h"
 /*
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
*/

/**
 *  \file SDL_hints.h
 *
 *  Official documentation for SDL configuration variables
 *
 *  This file contains functions to set and get configuration hints,
 *  as well as listing each of them alphabetically.
 *
 *  The convention for naming hints is SDL_HINT_X, where "SDL_X" is
 *  the environment variable that can be used to override the default.
 *
 *  In general these hints are just that - they may or may not be
 *  supported or applicable on any given platform, but they provide
 *  a way for an application or user to give the library a hint as
 *  to how they would like the library to work.
 */
}


(**
 *  \brief  A variable controlling how 3D acceleration is used to accelerate the SDL screen surface.
 *
 *  SDL can try to accelerate the SDL screen surface by using streaming
 *  textures with a 3D rendering engine.  This variable controls whether and
 *  how this is done.
 *
 *  This variable can be set to the following values:
 *    "0"       - Disable 3D acceleration
 *    "1"       - Enable 3D acceleration, using the default renderer.
 *    "X"       - Enable 3D acceleration, using X where X is one of the valid rendering drivers.  (e.g. "direct3d", "opengl", etc.)
 *
 *  By default SDL tries to make a best guess for each platform whether
 *  to use acceleration or not.
 *)
const
SDL_HINT_FRAMEBUFFER_ACCELERATION  = 'SDL_FRAMEBUFFER_ACCELERATION';

{/**
 *  \brief  A variable specifying which render driver to use.
 *
 *  If the application doesn't pick a specific renderer to use, this variable
 *  specifies the name of the preferred renderer.  If the preferred renderer
 *  can't be initialized, the normal default renderer is used.
 *
 *  This variable is case insensitive and can be set to the following values:
 *    "direct3d"
 *    "opengl"
 *    "opengles2"
 *    "opengles"
 *    "software"
 *
 *  The default varies by platform, but it's the first one in the list that
 *  is available on the current platform.
 */}
SDL_HINT_RENDER_DRIVER = 'SDL_RENDER_DRIVER';

{/**
 *  \brief  A variable controlling whether the OpenGL render driver uses shaders if they are available.
 *
 *  This variable can be set to the following values:
 *    "0"       - Disable shaders
 *    "1"       - Enable shaders
 *
 *  By default shaders are used if OpenGL supports them.
 */}
SDL_HINT_RENDER_OPENGL_SHADERS = 'SDL_RENDER_OPENGL_SHADERS';

{/**
 *  \brief  A variable controlling the scaling quality
 *
 *  This variable can be set to the following values:
 *    "0" or "nearest" - Nearest pixel sampling
 *    "1" or "linear"  - Linear filtering (supported by OpenGL and Direct3D)
 *    "2" or "best"    - Currently this is the same as "linear"
 *
 *  By default nearest pixel sampling is used
 */}
SDL_HINT_RENDER_SCALE_QUALITY = 'SDL_RENDER_SCALE_QUALITY';

{/**
 *  \brief  A variable controlling whether updates to the SDL screen surface should be synchronized with the vertical refresh, to avoid tearing.
 *
 *  This variable can be set to the following values:
 *    "0"       - Disable vsync
 *    "1"       - Enable vsync
 *
 *  By default SDL does not sync screen surface updates with vertical refresh.
 */}
SDL_HINT_RENDER_VSYNC = 'SDL_RENDER_VSYNC';

{/**
 *  \brief  A variable controlling whether the X11 VidMode extension should be used.
 *
 *  This variable can be set to the following values:
 *    "0"       - Disable XVidMode
 *    "1"       - Enable XVidMode
 *
 *  By default SDL will use XVidMode if it is available.
 */}
SDL_HINT_VIDEO_X11_XVIDMODE = 'SDL_VIDEO_X11_XVIDMODE';

{/**
 *  \brief  A variable controlling whether the X11 Xinerama extension should be used.
 *
 *  This variable can be set to the following values:
 *    "0"       - Disable Xinerama
 *    "1"       - Enable Xinerama
 *
 *  By default SDL will use Xinerama if it is available.
 */}
SDL_HINT_VIDEO_X11_XINERAMA = 'SDL_VIDEO_X11_XINERAMA';

{/**
 *  \brief  A variable controlling whether the X11 XRandR extension should be used.
 *
 *  This variable can be set to the following values:
 *    "0"       - Disable XRandR
 *    "1"       - Enable XRandR
 *
 *  By default SDL will not use XRandR because of window manager issues.
 */}
SDL_HINT_VIDEO_X11_XRANDR = 'SDL_VIDEO_X11_XRANDR';

{/**
 *  \brief  A variable controlling whether grabbing input grabs the keyboard
 *
 *  This variable can be set to the following values:
 *    "0"       - Grab will affect only the mouse
 *    "1"       - Grab will affect mouse and keyboard
 *
 *  By default SDL will not grab the keyboard so system shortcuts still work.
 */}
SDL_HINT_GRAB_KEYBOARD = 'SDL_GRAB_KEYBOARD';

{/**
 *  \brief Minimize your SDL_Window if it loses key focus when in Fullscreen mode. Defaults to true.
 *
 */}
SDL_HINT_VIDEO_MINIMIZE_ON_FOCUS_LOSS = 'SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS';


{/**
 *  \brief  A variable controlling whether the idle timer is disabled on iOS.
 *
 *  When an iOS app does not receive touches for some time, the screen is
 *  dimmed automatically. For games where the accelerometer is the only input
 *  this is problematic. This functionality can be disabled by setting this
 *  hint.
 *
 *  This variable can be set to the following values:
 *    "0"       - Enable idle timer
 *    "1"       - Disable idle timer
 */}
SDL_HINT_IDLE_TIMER_DISABLED = 'SDL_IOS_IDLE_TIMER_DISABLED';

{/**
 *  \brief  A variable controlling which orientations are allowed on iOS.
 *
 *  In some circumstances it is necessary to be able to explicitly control
 *  which UI orientations are allowed.
 *
 *  This variable is a space delimited list of the following values:
 *    "LandscapeLeft", "LandscapeRight", "Portrait" "PortraitUpsideDown"
 */}
SDL_HINT_ORIENTATIONS = 'SDL_IOS_ORIENTATIONS';


{/**
 *  \brief  A variable that lets you disable the detection and use of Xinput gamepad devices
 *
 *  The variable can be set to the following values:
 *    "0"       - Disable XInput timer (only uses direct input)
 *    "1"       - Enable XInput timer (the default)
 */}
SDL_HINT_XINPUT_ENABLED = 'SDL_XINPUT_ENABLED';


{/**
 *  \brief  A variable that lets you manually hint extra gamecontroller db entries
 *
 *  The variable should be newline delimited rows of gamecontroller config data, see SDL_gamecontroller.h
 *
 *  This hint must be set before calling SDL_Init(SDL_INIT_GAMECONTROLLER)
 *  You can update mappings after the system is initialized with SDL_GameControllerMappingForGUID() and SDL_GameControllerAddMapping()
 */}
SDL_HINT_GAMECONTROLLERCONFIG = 'SDL_GAMECONTROLLERCONFIG';


{/**
 *  \brief  A variable that lets you enable joystick (and gamecontroller) events even when your app is in the background.
 *
 *  The variable can be set to the following values:
 *    "0"       - Disable joystick & gamecontroller input events when the
 *                application is in the background.
 *    "1"       - Enable joystick & gamecontroller input events when the
 *                application is in the backgroumd.
 *
 *  The default value is "0".  This hint may be set at any time.
 */}
SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS = 'SDL_JOYSTICK_ALLOW_BACKGROUND_EVENTS';


{/**
 *  \brief If set to 0 then never set the top most bit on a SDL Window, even if the video mode expects it.
 *      This is a debugging aid for developers and not expected to be used by end users. The default is "1"
 *
 *  This variable can be set to the following values:
 *    "0"       - don't allow topmost
 *    "1"       - allow topmost
 */}
SDL_HINT_ALLOW_TOPMOST = 'SDL_ALLOW_TOPMOST';


{/**
 *  \brief A variable that controls the timer resolution, in milliseconds.
 *
 *  The higher resolution the timer, the more frequently the CPU services
 *  timer interrupts, and the more precise delays are, but this takes up
 *  power and CPU time.  This hint is only used on Windows 7 and earlier.
 *
 *  See this blog post for more information:
 *  http://randomascii.wordpress.com/2013/07/08/windows-timer-resolution-megawatts-wasted/
 *
 *  If this variable is set to "0", the system timer resolution is not set.
 *
 *  The default value is "1". This hint may be set at any time.
 */}
SDL_HINT_TIMER_RESOLUTION = 'SDL_TIMER_RESOLUTION';



{/**
 *  \brief  An enumeration of hint priorities
 */}
type
  SDL_HintPriority = (SDL_HINT_DEFAULT, SDL_HINT_NORMAL, SDL_HINT_OVERRIDE);


{/**
 *  \brief Set a hint with a specific priority
 *
 *  The priority controls the behavior when setting a hint that already
 *  has a value.  Hints will replace existing hints of their priority and
 *  lower.  Environment variables are considered to have override priority.
 *
 *  \return SDL_TRUE if the hint was set, SDL_FALSE otherwise
 */}
function SDL_SetHintWithPriority( const name: PChar; const value: PChar; priority: SDL_HintPriority) : boolean; cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SetHintWithPriority' {$ENDIF} {$ENDIF};

// extern DECLSPEC SDL_bool SDLCALL SDL_SetHintWithPriority(const char *name,
//                                                         const char *value,
//                                                         SDL_HintPriority priority);
{/**
 *  \brief Set a hint with normal priority
 *
 *  \return SDL_TRUE if the hint was set, SDL_FALSE otherwise
 */}
 function SDL_SetHint( const name: PChar; const value: PChar) : boolean; cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SetHint' {$ENDIF} {$ENDIF};

 //extern DECLSPEC SDL_bool SDLCALL SDL_SetHint(const char *name,
  //                                           const char *value);

{/**
 *  \brief Get a hint
 *
 *  \return The string value of a hint variable.
 */}
function SDL_GetHint( const name: PChar): PChar; cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetHint' {$ENDIF} {$ENDIF};
 //extern DECLSPEC const char * SDLCALL SDL_GetHint(const char *name);


{/**
 *  \brief Add a function to watch a particular hint
 *
 *  \param name The hint to watch
 *  \param callback The function to call when the hint value changes
 *  \param userdata A pointer to pass to the callback function
 */}
type
TSDL_HintCallback = procedure(userdata: Pointer; const name: PChar; const oldValue: PChar; const newValue: PChar);
procedure SDL_AddHintCallback(const name: PChar; callback: TSDL_HintCallback; userdata: Pointer); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_AddHintCallback' {$ENDIF} {$ENDIF};
 //typedef void (*SDL_HintCallback)(void *userdata, const char *name, const char *oldValue, const char *newValue);
//extern DECLSPEC void SDLCALL SDL_AddHintCallback(const char *name,
//                                                 SDL_HintCallback callback,
//                                                 void *userdata);

{/**
 *  \brief Remove a function watching a particular hint
 *
 *  \param name The hint being watched
 *  \param callback The function being called when the hint value changes
 *  \param userdata A pointer being passed to the callback function
 */}
procedure SDL_DelHintCallback(const name: PChar; callback: TSDL_HintCallback; userdata: Pointer); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_DelHintCallback' {$ENDIF} {$ENDIF};
//extern DECLSPEC void SDLCALL SDL_DelHintCallback(const char *name,
//                                                 SDL_HintCallback callback,
//                                                 void *userdata);

{/**
 *  \brief  Clear all hints
 *
 *  This function is called during SDL_Quit() to free stored hints.
 */}
//extern DECLSPEC void SDLCALL SDL_ClearHints(void);
procedure SDL_ClearHints(); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_ClearHints' {$ENDIF} {$ENDIF};

