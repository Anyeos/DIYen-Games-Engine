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
unit diyen_sound_fx;

{$mode objfpc}{$H+}

interface

uses
  diyen, diyen_sound, SDL2, SDL2_mixer,
  Classes, SysUtils;

type
  TDiyenSoundFX = record
    L: smallint;
    R: smallint;
  end;

procedure sound_fx_init();

var
  sound_fx : TDiyenSoundFX;

implementation


function lowpass_filter( Frequency: word ): smallint;
var
  RC: Integer;
  DT: Integer;
begin
  RC := Trunc(1000 / (Frequency * 2 * Pi));
  dt := 1000 div sound_freq;
  Result := dt div (RC+dt);
end;


procedure sound_fx_post(udata: Pointer; stream: PUInt8; len: Integer);
var
  i : word;
  valLcur: SmallInt = 0;
  valRcur: SmallInt = 0;
  valL: Integer = 0;
  valR: Integer = 0;
  samples : Integer = 0;
begin
  i := 0;
  repeat
    valLcur := SInt16(stream^);
    Inc(stream);
    valLcur := SInt16(valLcur + stream^ * 256);
    valL += valLcur;

    Inc(stream);
    valRcur := SInt16(stream^);
    Inc(stream);
    valRcur := SInt16(valRcur + stream^ * 256);
    valR += valRcur;

    Inc(stream);
    Inc(i,4);
    Inc(samples);
  until i >= len;

  sound_fx.L := valL div samples;
  sound_fx.R := valR div samples;
end;
procedure sound_fx_init();
begin
  sound_fx.R := 0;
  sound_fx.L := 0;
  Mix_SetPostMix(TMix_Func(@sound_fx_post), nil);
end;

end.

