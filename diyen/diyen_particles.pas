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
unit diyen_particles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL2, diyen, diyen_map, diyen_fx, sfile,
  inifiles, strutils;

type
  (*
  kind of particles:

  alphastatic : alpha-blended billboard
  static : additive-blended billboard
  spark : additive blended, stretched (based on velocity)
  beam : a beam particle, drawn from position to position + velocity
  rain : a rain particle, alpha-blended spart that will cause splash effect on impact
  snow: alpha blended, velocity jitters in realtime
  bubble: alpha-blended
  smoke: alpha-blended billboard
  *)
  TParticleKind =
    (
    PARTICLE_ALPHASTATIC := 0,
    PARTICLE_STATIC,
    PARTICLE_SPARK,
    PARTICLE_EXPLOSION,
    PARTICLE_BEAM,
    PARTICLE_CHAIN,
    PARTICLE_RAIN,
    PARTICLE_SNOW,
    PARTICLE_SMOKE);

  (*
  TODO:
    - Permitir agregar efectos al usuario (fx_add)




  Idea:
    Kind define el funcionamiento. qué comportamiento
    adoptará en el código principal. Como por ejemplo en una chispa la velocity
    no es el desplazamiento sino la velocidad con la que la chispa se abre.

    Para que el efecto sea más "lindo" usaremos un arreglo por cada elemento
    creado que contendrá las coordenadas de cada elemento. Siendo un elemento
    una serie de valores únicos que se manipulan en la función FX.

    Chispa es una partícula que se abre en forma de flor.
    Beam es una partícula que se estira velocity (sirve para hacer rayos).
    Chain es una partícula que se repite de un punto a otro (la quería usar para
    hacer elementos largos como por ejemplo una cadena del arma de un guerrero
    visto desde arriba, pero capaz eso se hace más fácil con fx_add).
    Rain debería ser una serie de elementos que se distribuyen como lluvia.
    Snow parecido a rain pero con los elementos zigzagueando.
    Smoke gira, static no gira.

    La explosión usa graph min y max para ejecutar la serie de animación desde el
    min hasta el max.

    El resto de los kind mantienen una similitud.

    Tendrían que tener gravity_x, gravity_y para poder "atraer" las partículas
    hacia ese lugar. O gravity, gravity_angle para elegir la dirección del vector
    gravedad.
    Por ejemplo, con eso podríamos simular fuegos artificiales, o explosiones
    en juegos tipo plataformas.
  *)

type
  TParticleData = record
    //name: String; // Only for reference
    kind: TParticleKind;

    count: word; // How many of elements

    //Generic blend is set by kind, but with this parm it cound be changed after kind is defined.
    // none: no blending,
    // blend : alpha blending,
    // add: additive blending,
    // mod: color modulation
    blend: TSDL_BlendMode;

    //Sets a color for particles.
    //On each particle spawn, it’s color is linearly randomized betwen two given colors.
    color_min, color_max : TSDL_Color;

    // Sets a graph of particle from fpg.
    // Randomized linearly on each particle spawn.
    graph_min, graph_max : word;

    // Opacity of particles, 255 is opaque, 0 is transparent. Randomized.
    // Could be more than 255 (to simulate fade delay).
    // "alpha_rate" is how huch alpha to throw away per frame,
    // once particle gets alpha 0 (full transparence), it gets removed.
    alpha_min, alpha_max, alpha_rate : word;

    // Size of particle in percent, randomized.
    size_min, size_max : int;

    // This will make particle grow or diminish over time.
    // "sizeincrease" is to how much units to add or subtract per frame.
    sizeincrease : int;

    // Particle time-to-live in frames, randomized.
    time_min, time_max : int;

    // Particle gravity modifier, 1 is full gravity, 0.5 is half etc., negative values are supported (particle go up).
    //gravity : int;

    // Particle friction while moving, good option for smoke emitters.
    // A value of 0 means no friction, negative values will do acceleration.
    friction : int;

    // How much movement to apply
    velocity_x, velocity_y : int;

    // How much to offset the particle (-jitter to +jitter).
    jitter_x, jitter_y : word;

    // How much to jitter the velocity.
    veljitter_x, veljitter_y : word;

    // Velotity applied to angle (angle += anglevel)
    anglevel_min, anglevel_max : int;

    frame : word; // Porcentaje de frame para ejecutar
  end;

  PParticleElement = ^TParticleElement;
  TParticleElement = record
    x, y: int;
    size: int;
    angle: int;
    anglevel: int;
    velocity_x, velocity_y: int;
    color: TSDL_Color;
  end;

  //PParticle = ^TParticle;
  TParticle = class(TProcess)
    private
      alphavar: smallint;
      time_alive: int;
      //drawed: Boolean; // FIXME: En realidad habría que corregir el engine
                       //        para que dibuje el primer fotograma

    public
      particledata : TParticleData;
      elements: array of TParticleElement;
      procedure start; override;
      procedure loop; override;
  end;

  TDiyenParticleList = class
    private
      fMax: word;
      procedure AddParticlesFromFile( fname: String );
    public
      particle: array of TParticleData;
      constructor Create(basename: string);
      destructor Destroy; override;
  end;


  procedure particles_load( basename: string );
  procedure particles_unload();
  function particle_spawn2(x, y: int; angle: int; z, size: smallint; resolution: int; ctype: Tctype; particlenum: word): word;

var
  particles_fpg: int;
  diyen_particles_list : TDiyenParticleList = nil;

implementation


// Acá se hace la magia
procedure particle_fx(var fx: TFXparams);
var
  p: TParticle;
  elem: PParticleElement;
  friction, sizeincrease : int;
  dx1, dy1, dx2, dy2, dist: int;
begin
  p := TParticle(fx.data);
  friction := p.particledata.friction;
  sizeincrease := p.particledata.sizeincrease;
  elem := PParticleElement(@p.elements[fx.n]);

  {$ifdef DIYEN_PAUSE}
  if gInternal.processing then
  begin
  {$endif}
  if elem^.anglevel <> 0 then
    elem^.angle += elem^.anglevel;

  if elem^.velocity_x <> 0 then
  begin
    //advance(elem^.x, elem^.y, elem^.angle, elem^.velocity);
    elem^.x := elem^.x + elem^.velocity_x;

    if abs(elem^.velocity_x) < abs(friction) then
      elem^.velocity_x := 0;

    if elem^.velocity_x > 0 then
      elem^.velocity_x -= friction
    else
    if elem^.velocity_x < 0 then
      elem^.velocity_x += friction;
  end;

  if elem^.velocity_y <> 0 then
  begin
    //advance(elem^.x, elem^.y, elem^.angle, elem^.velocity);
    elem^.y := elem^.y + elem^.velocity_y;

    if abs(elem^.velocity_y) < abs(friction) then
      elem^.velocity_y := 0;

    if elem^.velocity_y > 0 then
      elem^.velocity_y -= friction
    else
    if elem^.velocity_y < 0 then
      elem^.velocity_y += friction;
  end;

  if sizeincrease <> 0 then
    elem^.size += sizeincrease;
  {$ifdef DIYEN_PAUSE}
  end;
  {$endif}

  fx.color := elem^.color;
  fx.x := p.x + elem^.x;
  fx.y := p.y + elem^.y;

  if p.particledata.kind = PARTICLE_SPARK then
  begin
    //fx.size_x := Trunc(dcos(elem^.angle)*(elem^.velocity_x) / p.resolution *100 {* p.size_x});
    //fx.size_y := Trunc(dsin(elem^.angle)*(elem^.velocity_y) / p.resolution *100 {* p.size_y});
    dx1 := elem^.x;
    dy1 := elem^.y;
    dx2 := dx1 + elem^.velocity_x;
    dy2 := dy1 + elem^.velocity_y;
    dist := fget_dist(dx1, dy1,
                      dx2, dy2);
    fx.size_x := Trunc(dist / p.resolution * p.size_x);
    fx.size_y := p.size_y;
    elem^.angle := fget_angle(dx1, dy1, dx2, dy2);
  end
  else
  begin
    fx.size_x := (elem^.size * p.size_x) div 100;
    fx.size_y := (elem^.size * p.size_y) div 100;
  end;

  if (p.particledata.kind = PARTICLE_SPARK) then
    fx.angle := elem^.angle
  else
  if (p.particledata.kind <> PARTICLE_STATIC) and
    (p.particledata.kind <> PARTICLE_ALPHASTATIC) then
    fx.angle := (p.angle + elem^.angle) mod 360000
  else
    fx.angle := p.angle;
end;

procedure TParticle.start;
var
  (*colorHex : int;
  colorHex_min : int;
  colorHex_max : int;*)
  n: int;
  e: PParticleElement;
begin
  blend := particledata.blend;

  with particledata do
  begin
    time_alive := rand(time_min, time_max);

    if kind = PARTICLE_EXPLOSION then
      graph := graph_min
    else
      graph := rand(graph_min, graph_max);

    alphavar := rand(alpha_min, alpha_max);

    if alphavar > 255 then
      alpha := 255
    else
      alpha := alphavar;

    (*colorHex_min :=
    color_min.r*0x10000+
    color_min.g*0x100+
    color_min.b*0x1;

    colorHex_max :=
    color_max.r+
    color_max.g*256+
    color_max.b*256*256;

    colorHex := rand(colorHex_min, colorHex_max);
    color.r := (colorHex and 0xff) div 0x10000;
    color.g := (colorHex and 0xff00) div 0x100;
    color.b := (colorHex and 0xff0000) div 0x1;*)

    (*color.r := rand(color_min.r, color_max.r);
    color.g := rand(color_min.g, color_max.g);
    color.b := rand(color_min.b, color_max.b);

    size := rand(size_min, size_max);*)

    // Color y tamaño aplica en la FX
    SetLength(elements, count+1); // El índice 0 no se usa
    for n := 1 to count do
    begin
      e := PParticleElement(@elements[n]);

      e^.x := rand(-jitter_x, jitter_x);
      e^.y := rand(-jitter_y, jitter_y);

      e^.color.r := rand(color_min.r, color_max.r);
      e^.color.g := rand(color_min.g, color_max.g);
      e^.color.b := rand(color_min.b, color_max.b);

      e^.size := rand(size_min, size_max);

      e^.velocity_x := velocity_x+rand(-veljitter_x,+veljitter_x);
      e^.velocity_y := velocity_y+rand(-veljitter_y,+veljitter_y);

      e^.anglevel := rand(anglevel_min,anglevel_max);
    end;

    file_ := particles_fpg;

    //drawed := false; // Give time to draw first frame
    fx_add(TFXfunction(@particle_fx), count, Pointer(self));
  end;

  frame(particledata.frame);
end;
procedure TParticle.loop;
begin
  frame(particledata.frame);

  //if drawed then
  with particledata do
  begin
    if alphavar > 0 then alphavar -= alpha_rate;
    if alphavar < 0 then alphavar := 0;

    if alphavar > 255 then
      alpha := 255
    else
      alpha := alphavar;

    if kind = PARTICLE_EXPLOSION then
      if graph < graph_max then Inc(graph);

    if time_alive > 0 then Dec(time_alive);
    if time_alive = 0 then kill;
  end;

  if alpha <= 0 then kill;
  //drawed := true;
end;

function StrToParticleKind( value: String ): TParticleKind;
begin
  if SameText( value, 'ALPHASTATIC' ) then
    Result := PARTICLE_ALPHASTATIC
  else
  if SameText( value, 'STATIC' ) then
    Result := PARTICLE_STATIC
  else
  if SameText( value, 'SPARK' ) then
    Result := PARTICLE_SPARK
  else
  if SameText( value, 'EXPLOSION' ) then
    Result := PARTICLE_EXPLOSION
  else
  if SameText( value, 'BEAM' ) then
    Result := PARTICLE_BEAM
  else
  if SameText( value, 'CHAIN' ) then
    Result := PARTICLE_CHAIN
  else
  if SameText( value, 'RAIN' ) then
    Result := PARTICLE_RAIN
  else
  if SameText( value, 'SNOW' ) then
    Result := PARTICLE_SNOW
  else
  if SameText( value, 'SMOKE' ) then
    Result := PARTICLE_SMOKE
  else
    Result := PARTICLE_STATIC;
end;

function IntToSDLColor( value: Integer ): TSDL_Color;
begin
  Result.b:=value and $ff;
  Result.g:=(value and $ff00) div $100;
  Result.r:=(value and $ff0000) div $10000;
end;

function StrToSDLBlend( value: String ): TSDL_BlendMode;
begin
  if SameText( value, 'BLEND' ) then
    Result := SDL_BLENDMODE_BLEND
  else
  if SameText( value, 'ADD' ) then
    Result := SDL_BLENDMODE_ADD
  else
  if SameText( value, 'MOD' ) then
    Result := SDL_BLENDMODE_MOD
  else
  if SameText( value, 'NONE' ) then
    Result := SDL_BLENDMODE_NONE
  else
    Result := $FF;
end;

procedure TDiyenParticleList.AddParticlesFromFile( fname: String );
var
  ParticleFile: TIniFile;
  Sections: TStrings = nil;
  Section: String;
  n, i : integer;
  strtmp: String;
  blendtmp : TSDL_BlendMode;

  Stream: TStream;
begin
  try
  Stream := SLoadFromFile(fname);
  ParticleFile := TIniFile.Create(Stream);
  Sections := TStringList.Create;
  ParticleFile.ReadSections(Sections);
  for n := 0 to Sections.Count-1 do
    begin
      Section := Sections[n];
      i := fMax; Inc(fMax);
      SetLength(particle, fMax);
      with particle[i] do
      begin
        kind := StrToParticleKind(ParticleFile.ReadString(Section, 'kind', 'ALPHASTATIC'));
        // Sets blend from kind
        case kind of
        PARTICLE_ALPHASTATIC: blend := SDL_BLENDMODE_BLEND;
        PARTICLE_STATIC: blend := SDL_BLENDMODE_ADD;
        PARTICLE_SPARK: blend := SDL_BLENDMODE_ADD;
        PARTICLE_EXPLOSION: blend := SDL_BLENDMODE_ADD;
        PARTICLE_BEAM: blend := SDL_BLENDMODE_ADD;
        PARTICLE_CHAIN: blend := SDL_BLENDMODE_BLEND;
        PARTICLE_RAIN: blend := SDL_BLENDMODE_BLEND;
        PARTICLE_SNOW: blend := SDL_BLENDMODE_BLEND;
        PARTICLE_SMOKE: blend := SDL_BLENDMODE_BLEND;
        end;

        count := ParticleFile.ReadInteger(Section, 'count', 1);

        //Generic blend is set by kind,
        //but with this parm it could be changed after kind is defined.
        // none: no blending,
        // blend : alpha blending,
        // add: additive blending,
        // mod: color modulation
        blendtmp := StrToSDLBlend(ParticleFile.ReadString(Section, 'blend', ''));
        if blendtmp <> $FF then blend := blendtmp;

        //Sets a color for particles.
        //On each particle spawn, it’s color is linearly randomized betwen two given colors.
        strtmp := ParticleFile.ReadString(Section, 'color', '$ffffff $ffffff');
        color_min := IntToSDLColor(StrToIntDef(ExtractWord(1, strtmp, [' ']), $ffffff));
        color_max := IntToSDLColor(StrToIntDef(ExtractWord(2, strtmp, [' ']), $ffffff));

        // Sets a graph of particle from fpg.
        // Randomized linearly on each particle spawn.
        strtmp := ParticleFile.ReadString(Section, 'graph', '0 0');
        graph_min := StrToIntDef(ExtractWord(1, strtmp, [' ']), 0);
        graph_max := StrToIntDef(ExtractWord(2, strtmp, [' ']), graph_min);

        // Opacity of particles, 255 is opaque, 0 is transparent. Randomized.
        // Could be more than 255 (to simulate fade delay).
        // "alpha_rate" is how huch alpha to throw away per frame,
        // once particle gets alpha 0 (full transparence), it gets removed.
        strtmp := ParticleFile.ReadString(Section, 'alpha', '255 255 10');
        alpha_min := StrToIntDef(ExtractWord(1, strtmp, [' ']), 255);
        alpha_max := StrToIntDef(ExtractWord(2, strtmp, [' ']), alpha_min);
        alpha_rate := StrToIntDef(ExtractWord(3, strtmp, [' ']), 10);

        // Size of particle in percent, randomized.
        strtmp := ParticleFile.ReadString(Section, 'size', '100 100');
        size_min := StrToIntDef(ExtractWord(1, strtmp, [' ']), 100);
        size_max := StrToIntDef(ExtractWord(2, strtmp, [' ']), size_min);

        // This will make particle grow or diminish over time.
        // "sizeincrease" is to how much units to add or subtract per frame.
        sizeincrease := ParticleFile.ReadInteger(Section, 'sizeincrease', 0);

        // Particle time-to-live in frames, randomized.
        strtmp := ParticleFile.ReadString(Section, 'time', '-1 -1');
        time_min := StrToIntDef(ExtractWord(1, strtmp, [' ']), -1);
        time_max := StrToIntDef(ExtractWord(2, strtmp, [' ']), time_min);

        // Particle gravity modifier, 1 is full gravity, 0.5 is half etc., negative values are supported (particle go up).
        //gravity : int;

        // Particle friction while moving, good option for smoke emitters.
        // A value of 0 means no friction, negative values will do acceleration.
        friction := ParticleFile.ReadInteger(Section, 'friction', 0);

        // How much to move. Negative values goes backward.
        strtmp := ParticleFile.ReadString(Section, 'velocity', '0 0');
        velocity_x := StrToIntDef(ExtractWord(1, strtmp, [' ']), 0);
        velocity_y := StrToIntDef(ExtractWord(2, strtmp, [' ']), velocity_x);

        // How much to offset the particle (-jitter to +jitter).
        strtmp := ParticleFile.ReadString(Section, 'jitter', '0 0');
        jitter_x := StrToIntDef(ExtractWord(1, strtmp, [' ']), 0);
        jitter_y := StrToIntDef(ExtractWord(2, strtmp, [' ']), 0);

        // How much to jitter the velocity.
        strtmp := ParticleFile.ReadString(Section, 'veljitter', '0 0');
        veljitter_x := StrToIntDef(ExtractWord(1, strtmp, [' ']), 0);
        veljitter_y := StrToIntDef(ExtractWord(2, strtmp, [' ']), veljitter_x);

        // Velotity applied to the angle (angle += anglevel)
        strtmp := ParticleFile.ReadString(Section, 'anglevel', '0 0');
        anglevel_min := StrToIntDef(ExtractWord(1, strtmp, [' ']), 0);
        anglevel_max := StrToIntDef(ExtractWord(2, strtmp, [' ']), anglevel_min);

        frame := ParticleFile.ReadInteger(Section, 'frame', 100);
      end;
    end;
    Sections.Free;
    ParticleFile.Free;
    Stream.Free;
  except
    SDL_Log(PChar('Error: Cannot load particles file: '+fname));
  end;
end;

constructor TDiyenParticleList.Create(basename: string);
begin
  fMax := 0;
  AddParticlesFromFile(basename+'.particles');
end;

destructor TDiyenParticleList.Destroy;
begin
  SetLength(particle, 0);
  inherited Destroy;
end;

procedure particles_load( basename: string );
begin
  particles_fpg := load_fpg_list(basename);
  if diyen_particles_list = nil then
    diyen_particles_list := TDiyenParticleList.Create(basename)
  else
    diyen_particles_list.AddParticlesFromFile(basename);
end;

procedure particles_unload;
begin
  if diyen_particles_list <> nil then
  begin
    diyen_particles_list.Destroy;
    diyen_particles_list := nil;
  end;
end;


function particle_spawn2(x, y: int; angle: int; z, size: smallint; resolution: int; ctype: Tctype; particlenum: word): word;
var
  Particle: TParticle;
begin
  Particle := TParticle.Create;
  Result := PAdd(Particle, 0);

  with diyen_particles_list.particle[particlenum] do
  begin
    Particle.x := x;
    Particle.y := y;
    Particle.z := z;
    Particle.angle := angle;
    Particle.size := size;
    Particle.resolution := resolution;
    Particle.ctype := ctype;

    Particle.particledata.kind := kind;
    Particle.particledata.count := count;
    Particle.particledata.blend := blend;
    Particle.particledata.color_min := color_min;
    Particle.particledata.color_max := color_max;
    Particle.particledata.graph_min := graph_min;
    Particle.particledata.graph_max := graph_max;
    Particle.particledata.alpha_min := alpha_min;
    Particle.particledata.alpha_max := alpha_max;
    Particle.particledata.alpha_rate := alpha_rate;
    Particle.particledata.size_min := size_min;
    Particle.particledata.size_max := size_max;
    Particle.particledata.sizeincrease := sizeincrease;
    Particle.particledata.time_min := time_min;
    Particle.particledata.time_max := time_max;
    Particle.particledata.friction := friction;
    Particle.particledata.velocity_x := velocity_x;
    Particle.particledata.velocity_y := velocity_y;
    Particle.particledata.jitter_x := jitter_x;
    Particle.particledata.jitter_y := jitter_y;
    Particle.particledata.veljitter_x := veljitter_x;
    Particle.particledata.veljitter_y := veljitter_y;
    Particle.particledata.anglevel_min := anglevel_min;
    Particle.particledata.anglevel_max := anglevel_max;
    Particle.particledata.frame := frame;
  end;
end;

end.

