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
unit diyen_sound;

{$mode objfpc}{$H+}

interface

uses
  diyen, SDL2, SDL2_mixer,
  Classes, SysUtils;

const
  DIYEN_LMAX_MUSIC     = 32;
  DIYEN_LMAX_SOUNDS    = 128;


type
// Modo de sonido
Tsoundmode = (MODE_MONO := 0,
              MODE_STEREO,
              MODE_UNKNOWN := 99);


// Lista de sonidos
TdiyenSoundsList = class
  sound: array of PMix_Chunk;
  constructor Create();
  destructor Destroy(); override;
public
  function load_sound( filename: string ): word;
  procedure unload_sound( soundid: word );
  function set_volume( channel: smallint; volume: smallint ): byte;
  function play_sound( soundid: word; loops: smallint; channel: smallint ): byte;
  function play_sound_position( soundid: word; loops: smallint; channel: smallint; angle: int; distance: byte ): byte;
  procedure stop_sound( channel: smallint );
  function is_playing_sound( channel: smallint ): byte;
  function set_panning( channel, lpan, rpan: byte ): Boolean;
  function set_distance( channel, distance: byte ): Boolean;
  function set_position( channel: byte; ang: smallint; dist: byte ): Boolean;
private
  vi_uborrado: integer;
  vu_borrados: array of word;
  vmax: word;
end;


// Lista de musica / song
TdiyenMusicList = class
  music: array of PMix_Music;
  constructor Create();
  destructor Destroy(); override;
public
  function load_song( filename: string ): byte;
  procedure unload_song( songid: byte );
  function set_song_volume( volume: smallint ): byte;
  function play_song( songid: byte; loops: smallint ): boolean;
  procedure stop_song();
  procedure pause_song();
  procedure resume_song();
  function is_playing_song(): boolean;
  function fade_music_off( milliseconds: int ): Boolean;
  function fade_music_on( songid: byte; loops: smallint; ms: int ): Boolean;
private
  vi_uborrado: smallint;
  vu_borrados: array of byte;
  vmax: byte;
end;


// Funciones del sistema de sonido
function sound_init(): int;
function sound_reserve_channels( num: int ): int;

// Song - Music
function load_song( filename: string ): byte;
procedure unload_song( songid: byte );
function set_song_volume( volume: smallint ): byte;
function play_song( songid: byte; loops: smallint ): boolean;
procedure stop_song();
procedure pause_song();
procedure resume_song();
function is_playing_song(): boolean;
function fade_music_off( milliseconds: int ): Boolean;
function fade_music_on( songid: byte; loops: smallint; ms: int ): Boolean;
// Sound - FX
function load_sound( filename: string ): word;
function load_wav( filename: string ): word;
function load_ogg( filename: string ): word;
procedure unload_sound( soundid: word );
function set_volume( channel: smallint; volume: smallint ): byte;
function play_sound( soundid: word; loops: smallint; channel: smallint ): byte;
function play_sound( soundid: word ): byte;
function play_sound_position( soundid: word; loops: smallint; channel: smallint; angle: int; distance: byte ): byte;
function play_sound_position( soundid: word; angle: int; distance: byte ): byte;
function play_wav( soundid: word; loops: smallint; channel: smallint ): byte;
function play_ogg( soundid: word; loops: smallint; channel: smallint ): byte;
procedure stop_sound( channel: smallint );
procedure stop_wav( channel: smallint );
procedure stop_ogg( channel: smallint );
function is_playing_sound( channel: smallint ): byte;
function set_panning( channel, lpan, rpan: byte ): Boolean;
function set_distance( channel, distance: byte ): Boolean;
function set_position( channel: byte; ang: smallint; dist: byte ): Boolean;
//change_channel
//change_sound

var
  // Global
  sound_mode      : Tsoundmode = MODE_STEREO;
  sound_freq      : word = 22050;
  sound_format    : UInt16 = AUDIO_S16;
  sound_channels  : byte = 20;
  sound_chunksize : word = 1024;


  // Listas
  diyen_music_list : TdiyenMusicList;
  diyen_sounds_list : TdiyenSoundsList;

implementation

(********************************
  Funciones del sistema de sonido
*********************************)

// Inicializa el audio con la configuración establecida
// DIV LIKE sound functions
function sound_init(): int;
var
  channels : integer;

begin
  (*if (sound_freq > 22050) then
    sound_freq := 44100
  else if (sound_freq > 11025) then
    sound_freq := 22050
  else
    sound_freq := 11025;*)

  channels := 1+Ord(sound_mode);

  Result := Mix_OpenAudio(sound_freq, sound_format, channels, sound_chunksize);
  Mix_QuerySpec(@sound_freq, @sound_format, @channels);

  if channels = 2 then
    sound_mode := MODE_STEREO
  else
  if channels = 1 then
    sound_mode := MODE_MONO
  else
    sound_mode := MODE_UNKNOWN;

  Mix_AllocateChannels(sound_channels);
end;

function sound_reserve_channels( num: int ): int;
begin
  Result := Mix_ReserveChannels(num);
end;

function load_song( filename: string ): byte;
begin
  Result := diyen_music_list.load_song(filename);
end;

procedure unload_song( songid: byte );
begin
  diyen_music_list.unload_song(songid);
end;

function set_song_volume( volume: smallint ): byte;
begin
  Result := diyen_music_list.set_song_volume( volume * 128 div 100 );
end;

function play_song( songid: byte; loops: smallint ): boolean;
begin
  Result := diyen_music_list.play_song(songid, loops);
end;

procedure stop_song();
begin
  diyen_music_list.stop_song();
end;

procedure pause_song;
begin
  diyen_music_list.pause_song;
end;

procedure resume_song();
begin
  diyen_music_list.resume_song();
end;

function is_playing_song(): boolean;
begin
  Result := diyen_music_list.is_playing_song();
end;

function fade_music_off( milliseconds: int ): Boolean;
begin
  Result := diyen_music_list.fade_music_off( milliseconds );
end;

function fade_music_on( songid: byte; loops: smallint; ms: int ): Boolean;
begin
  Result := diyen_music_list.fade_music_on(songid, loops, ms);
end;

function load_sound( filename: string ): word;
begin
  Result := diyen_sounds_list.load_sound( filename );
end;

function load_wav( filename: string ): word;
begin
  Result := load_sound( filename );
end;

function load_ogg( filename: string ): word;
begin
  Result := load_sound( filename );
end;

procedure unload_sound( soundid: word );
begin
  diyen_sounds_list.unload_sound(soundid);
end;

function set_volume( channel: smallint; volume: smallint ): byte;
begin
  Result := diyen_sounds_list.set_volume( channel, volume * 128 div 100 );
end;

function play_sound( soundid: word; loops: smallint; channel: smallint ): byte;
begin
  Result := diyen_sounds_list.play_sound( soundid, loops, channel );
end;
function play_sound( soundid: word ): byte;
begin
  Result := diyen_sounds_list.play_sound( soundid, 0, -1 );
end;

function play_sound_position( soundid: word; loops: smallint; channel: smallint; angle: int; distance: byte ): byte;
begin
  Result := diyen_sounds_list.play_sound_position( soundid, loops, channel, angle, distance );
end;
function play_sound_position( soundid: word; angle: int; distance: byte ): byte;
begin
  Result := diyen_sounds_list.play_sound_position( soundid, 0, -1, angle, distance );
end;


function play_wav( soundid: word; loops: smallint; channel: smallint ): byte;
begin
  Result := play_sound(soundid,loops,channel);
end;

function play_ogg( soundid: word; loops: smallint; channel: smallint ): byte;
begin
  Result := play_sound(soundid,loops,channel);
end;

procedure stop_sound( channel: smallint );
begin
  diyen_sounds_list.stop_sound(channel);
end;

procedure stop_wav( channel: smallint );
begin
  stop_sound(channel);
end;

procedure stop_ogg( channel: smallint );
begin
  stop_sound(channel);
end;

function is_playing_sound( channel: smallint ): byte;
begin
  Result := diyen_sounds_list.is_playing_sound(channel);
end;

function set_panning( channel, lpan, rpan: byte ): Boolean;
begin
  Result := diyen_sounds_list.set_panning(channel, lpan, rpan);
end;

function set_distance( channel, distance: byte ): Boolean;
begin
  Result := diyen_sounds_list.set_distance(channel, distance);
end;

function set_position( channel: byte; ang: smallint; dist: byte ): Boolean;
begin
  Result := diyen_sounds_list.set_position(channel, ang, dist);
end;

(*
 Funciones para música
*)
constructor TdiyenMusicList.Create();
var
  i : byte;
begin
  SetLength(music, DIYEN_LMAX_MUSIC+1);
  SetLength(vu_borrados, DIYEN_LMAX_MUSIC+1);
  for i := 0 to DIYEN_LMAX_MUSIC do music[i] := nil;
  vmax := 0;
end;
destructor TdiyenMusicList.Destroy();
var
  i : byte;
begin
  for i:=vmax downto 0 do
    if (music[i] <> nil) then Mix_FreeMusic(music[i]);
  SetLength(music, 0);
  SetLength(vu_borrados, 0);
end;

function TdiyenMusicList.load_song( filename: string ): byte;
var
  iborrado : integer;
begin
  if vi_uborrado > 0 then
  begin
    // Cual esta borrado?
    iborrado := vu_borrados[vi_uborrado];
    Dec(vi_uborrado); // Ya no esta mas borrado
    music[iborrado] := Mix_LoadMUS( PChar(filename) );
    Result := iborrado; // Este es el indice
  end else
  // Procedimiento normal
  begin
    Inc(vmax);
    music[vmax] := Mix_LoadMUS( PChar(filename) );
    Result := vmax;
  end;
end;

procedure TdiyenMusicList.unload_song( songid: byte );
begin
  if music[songid] <> nil then
    Mix_FreeMusic(music[songid]);
  music[songid] := nil;

  // Era el ultimo?
  if songid = vmax then
  begin
    // Entonces ahora ya no (esta borrado)
    Dec(vmax);
  end else
  begin // No era el ultimo
    Inc(vi_uborrado); // Entonces se agrega a los borrados
    vu_borrados[vi_uborrado] := songid; // Y fue este
  end;
end;

function TdiyenMusicList.set_song_volume( volume: smallint ): byte;
begin
  Result := byte(Mix_VolumeMusic( volume ));
end;

function TdiyenMusicList.play_song( songid: byte; loops: smallint ): boolean;
begin
  Result := (Mix_PlayMusic(music[songid], loops) < 0);
end;

procedure TdiyenMusicList.stop_song();
begin
  Mix_HaltMusic();
end;

procedure TdiyenMusicList.pause_song();
begin
  Mix_PauseMusic;
end;

procedure TdiyenMusicList.resume_song();
begin
  Mix_ResumeMusic;
end;

function TdiyenMusicList.is_playing_song(): boolean;
begin
  Result := Boolean(Mix_PlayingMusic);
end;

function TdiyenMusicList.fade_music_off( milliseconds: int ): Boolean;
begin
  Result := Boolean(Mix_FadeOutMusic( milliseconds ));
end;

function TdiyenMusicList.fade_music_on( songid: byte; loops: smallint; ms: int ): Boolean;
begin
  Result := Boolean(Mix_FadeInMusic( music[songid], loops, ms ));
end;

(*
  Sonidos
*)
constructor TdiyenSoundsList.Create();
var
  i : word;
begin
  SetLength(sound, DIYEN_LMAX_SOUNDS+1);
  SetLength(vu_borrados, DIYEN_LMAX_SOUNDS+1);
  for i := 0 to DIYEN_LMAX_SOUNDS do sound[i] := nil;
  vmax := 0;
end;
destructor TdiyenSoundsList.Destroy();
var
  i : word;
begin
  for i:=vmax downto 0 do
    if (sound[i] <> nil) then Mix_FreeChunk(sound[i]);
  SetLength(sound, 0);
  SetLength(vu_borrados, 0);
end;

function TdiyenSoundsList.load_sound( filename: string ): word;
var
    iborrado : integer;
begin
    if vi_uborrado > 0 then
    begin
      // Cual esta borrado?
      iborrado := vu_borrados[vi_uborrado];
      Dec(vi_uborrado); // Ya no esta mas borrado
      sound[iborrado] := Mix_LoadWAV( PChar(filename) );
      Result := iborrado; // Este es el indice
    end else
    // Procedimiento normal
    begin
      Inc(vmax);
      sound[vmax] := Mix_LoadWAV( PChar(filename) );
      Result := vmax;
    end;
end;

procedure TdiyenSoundsList.unload_sound( soundid: word );
begin
    if sound[soundid] <> nil then
      Mix_FreeChunk(sound[soundid]);
    sound[soundid] := nil;

    // Era el ultimo?
    if soundid = vmax then
    begin
      // Entonces ya no es más el último porque está borrado
      Dec(vmax);
    end else
    begin // No era el ultimo
      Inc(vi_uborrado); // Entonces se agrega a los borrados
      vu_borrados[vi_uborrado] := soundid; // Y fue este
    end;
end;

function TdiyenSoundsList.set_volume( channel: smallint; volume: smallint ): byte;
begin
  Result := Mix_Volume(channel, volume);
end;

function TdiyenSoundsList.play_sound(
  soundid: word;
  loops: smallint;
  channel: smallint ): byte;
begin
  Result := byte(Mix_PlayChannel(channel, sound[soundid], loops));
  //Mix_SetPosition(Result, 0, 0);
end;

function TdiyenSoundsList.play_sound_position(
  soundid: word;
  loops: smallint;
  channel: smallint;
  angle: int;
  distance: byte ): byte;
begin
  Result := byte(Mix_PlayChannel(channel, sound[soundid], loops));
  Mix_SetPosition(Result, angle div 1000, distance);
end;

procedure TdiyenSoundsList.stop_sound( channel: smallint );
begin
  Mix_HaltChannel(channel);
end;

function TdiyenSoundsList.is_playing_sound( channel: smallint ): byte;
begin
  Result := Mix_Playing( channel );
end;

function TdiyenSoundsList.set_panning( channel, lpan, rpan: byte ): Boolean;
begin
  Result := (Mix_SetPanning(channel, lpan, rpan) = 0);
end;

function TdiyenSoundsList.set_distance( channel, distance: byte ): Boolean;
begin
  Result := (Mix_SetDistance(channel, distance) = 0);
end;

function TdiyenSoundsList.set_position( channel: byte; ang: smallint; dist: byte ): Boolean;
begin
  Result := (Mix_SetPosition(channel, ang, dist) = 0);
end;



end.

