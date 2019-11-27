unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL2, SDL2_mixer,
  diyen, diyen_input, diyen_sound, diyen_map, diyen_text, diyen_draw,
  diyen_particles;

type

  // declare
  TProgram = class(TMainProgram)
  procedure main; override;
  end;



implementation
uses diyen_sound_fx;

// program
procedure TProgram.main;
begin
  set_fps(60, 10);
  //set_hint(HINT_VSYNC, '1');
  set_mode_auto(1024, 600);
  {$ifdef ANDROID}
  full_screen:=true;
  {$endif}

  // Sonido
  sound_channels:=10;
  sound_freq:=22050;
  sound_mode:=MODE_STEREO;
  sound_init();

  set_volume(-1, 70);
  //set_song_volume(Config.volmusica);

  sound_fx_init();

  repeat


  frame;
  //clear_color(100, 100, 12);
  until (exit_status <> 0)
end;

end.

