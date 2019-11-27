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


  DIYen Collision detection system
  --------------------------------

  It is a complex arithmetic part of the engine
  It can combine some shapes with some others

  El sistema de colisión de diyen funciona comprobando figuras.
  Las imágenes (tipos map) tienen asignada una lista de "boundings" para
  colisión y otra para hit (impacto, mas que nada para usar en juegos
  de lucha). Entonces el sistema de colisión de diyen compara la
  superposición de esas figuras.
  Se pueden usar rectángulos, cuadrados, círculos y puntos.
  Existen también dos tipos de poligonos de 4 lados especiales: el
  cuadrado alineado a ejes y el rectángulo alineado a ejes.
  Que se caracterizan por estar alineados al eje de las coordenadas y jamás rotar.
  Estos dos sirven para dos propósitos:
    - El cuadrado alineado a ejes, sirve para detectar rápidamente colisión.
      Y es el que conviene poner siempre como el primero. Util para detectar
      si es posible que haya una colisión más detallada.
    - Y el rectángulo alineado a ejes, sirve para los juegos tipo de lucha.
      Es casi tan rápido como el cuadrado pero tiene forma rectangular y no
      requiere de cálculos de rotación.

  Ninguno de esos dos sirve para detectar colisiones con figuras rotadas.
  Pero como los juegos de lucha no suelen rotar los personajes (que yo
  sepa ninguno lo hace), entonces el rectángulo alineado a ejes es una muy
  buena opción para esos casos.

  El cuadrado alineado a ejes se usa en figuras que rotan como primer detección
  para determinar si proceder a detectar el resto de las figuras.


   Opciones al compilador:
  -dNO_HIT_COL para desactivar colisión de un hit contra un col de otro proceso.
  -dHIT_HIT para detectar colisión entre hits de procesos.

*)

unit diyen_collision;

{$mode objfpc}{$H+}

interface

uses
  diyen, SDL2,
  Classes, SysUtils;

procedure diyen_colandhit_detection( process: TProcess );
function diyen_bound_aa_with(a1, aL1, a2, aL2 : integer): boolean;
function diyen_bound_rect_rect(
  x1, y1, w1, h1, x2, y2, w2, h2 : integer;
  angle1, angle2 : integer;
  c1x, c1y, c2x, c2y : integer ) : boolean;
//function diyen_bound_cuad_rect(
//  x1, y1, sizex1, sizey1, x2, y2, sizex2, sizey2 : integer;
//  angle1, angle2 : integer;
//  bound, obound: PBound) : boolean;
//function diyen_bound_rect_cuad(
//  x1, y1, sizex1, sizey1, x2, y2, sizex2, sizey2 : integer;
//  angle1, angle2 : integer;
//  bound, obound: PBound) : boolean;
procedure diyen_bound_aa_vals(a, aL: pinteger; size, c, L: integer);

implementation
uses diyen_map;

(*
  TODO:
  Optimizar todas las funciones en algunos parámetros
    En lugar de pasarle un tipo record directamente podría ser más rápido
    que le pasara un puntero a dicho record (no haría una copia local del
    mismo).
  Añadir el tipo de colisión "punto" (BOUND_POINT). Que será muy útil para
  juegos tipo plataformas por ejemplo. Y es más rápido que usar figuras.

  Revisar los cálculos de fijos. Y si hay alguna combinación que aún
  no sea considerada. Especialmente revisar las de "otherwise" o mejor
  quitar esa posibilidad.

*)


{$ifdef DIYEN_DEBUG}
procedure diyen_debug_collision(
  x1, y1, sizex1, sizey1,
  x2, y2, sizex2, sizey2: integer;
  angle1, angle2 : integer;
  bound, obound: TBound);
var
  rect, drect : TSDL_Rect;
  r : integer;
  ang : float;
  x, y, xant, yant : integer;

  P1x, P2x, P3x, P4x : integer;
  P1y, P2y, P3y, P4y : integer;

  R1x, R2x, R3x, R4x : integer;
  R1y, R2y, R3y, R4y : integer;

  cosb, senb : float;
  Ox, Oy : integer;

  Pang, px, py : integer;
  ppx, ppy : integer;

  rect2 : TSDL_Rect;
  cosb2, senb2 : float;
  O2x, O2y : integer;

  Xmin, Xmax : integer;
  Ymin, Ymax : integer;
begin

  rect.x:=x1;
  rect.y:=y1;
  rect.w:=(sizex1) div 100+5;
  rect.h:=(sizey1) div 100+5;
  SDL_SetRenderDrawColor(gInternal.renderer, 200, 200, 0, 200);
  SDL_RenderDrawPoint(gInternal.renderer, rect.x    , rect.y);
  SDL_RenderDrawPoint(gInternal.renderer, rect.x+rect.w , rect.y);
  SDL_RenderDrawPoint(gInternal.renderer, rect.x-rect.w , rect.y);
  SDL_RenderDrawPoint(gInternal.renderer, rect.x    , rect.y+rect.h);
  SDL_RenderDrawPoint(gInternal.renderer, rect.x    , rect.y-rect.h);

  rect.x:=x2;
  rect.y:=y2;
  rect.w:=(sizex2) div 100+5;
  rect.h:=(sizey2) div 100+5;
  SDL_RenderDrawPoint(gInternal.renderer, rect.x    , rect.y);
  SDL_RenderDrawPoint(gInternal.renderer, rect.x+rect.w , rect.y);
  SDL_RenderDrawPoint(gInternal.renderer, rect.x-rect.w , rect.y);
  SDL_RenderDrawPoint(gInternal.renderer, rect.x    , rect.y+rect.h);
  SDL_RenderDrawPoint(gInternal.renderer, rect.x    , rect.y-rect.h);


  SDL_SetRenderDrawColor(gInternal.renderer, 150, 150, 150, 200);
  SDL_RenderDrawLine(gInternal.renderer, x1, y1, x2, y2);

  case bound.t of
    BOUND_AA_SQUARE:
    begin
      r := greater(sizex1, sizey1);
      rect.x:=x1+(bound.x*r) div 100;
      rect.y:=y1+(bound.y*r) div 100;
      rect.w := (bound.w*r) div 100;
      rect.h := rect.w;
      SDL_SetRenderDrawColor(gInternal.renderer, 0, 255, 0, 100);
      SDL_RenderDrawRect(gInternal.renderer, @rect);
    end;
    BOUND_AA_RECT:
    begin
      rect.x:=x1+(bound.x*sizex1) div 100;
      rect.y:=y1+(bound.y*sizey1) div 100;
      rect.w:=(bound.w*sizex1) div 100;
      rect.h:=(bound.h*sizey1) div 100;
      SDL_SetRenderDrawColor(gInternal.renderer, 0, 255, 0, 100);
      SDL_RenderDrawRect(gInternal.renderer, @rect);
    end;
    BOUND_RECTANGLE:
    begin
      // Con referencia punto de origen en el centro de la imagen
      // y el centro de rotación también es el centro de la imagen
      cosb := dcos(angle1);
      senb := dsin(angle1);

      //Ox := (bound.x*sizex1) div 100;
      //Oy := (bound.y*sizey1) div 100;

      //rect.x:=x1+Trunc(Ox*cosb-Oy*senb);
      //rect.y:=y1+Trunc(Oy*cosb+Ox*senb);
      rect.x:=(bound.x*sizex1) div 100;
      rect.y:=(bound.y*sizey1) div 100;
      rect.w:=rect.x+(bound.w*sizex1) div 100;
      rect.h:=rect.y+(bound.h*sizey1) div 100;

      //P1x := rect.w;
      //P1y := rect.h;

      //P2x := -P1x; P2y := P1y;
      //P3x := -P1x; P3y := -P1y;
      //P4x := P1x; P4y := -P1y;
      P1x := rect.x;  P1y := rect.y;
      P2x := rect.w;  P2y := rect.y;
      P3x := rect.w;  P3y := rect.h;
      P4x := rect.x;  P4y := rect.h;

      R1x := Trunc(P1x*cosb-P1y*senb);  R1y := Trunc(P1y*cosb+P1x*senb);
      R1x := R1x+x1;                    R1y := R1y+y1;
      R2x := Trunc(P2x*cosb-P2y*senb);  R2y := Trunc(P2y*cosb+P2x*senb);
      R2x := R2x+x1;                    R2y := R2y+y1;
      R3x := Trunc(P3x*cosb-P3y*senb);  R3y := Trunc(P3y*cosb+P3x*senb);
      R3x := R3x+x1;                    R3y := R3y+y1;
      R4x := Trunc(P4x*cosb-P4y*senb);  R4y := Trunc(P4y*cosb+P4x*senb);
      R4x := R4x+x1;                    R4y := R4y+y1;

      //SDL_SetRenderDrawColor(gInternal.renderer, 255, 0, 0, 100);
      //SDL_RenderDrawRect(gInternal.renderer, @rect);

      SDL_SetRenderDrawColor(gInternal.renderer, 0, 255, 0, 100);
      SDL_RenderDrawLine(gInternal.renderer, R1x, R1y, R2x, R2y);
      SDL_RenderDrawLine(gInternal.renderer, R2x, R2y, R3x, R3y);
      SDL_RenderDrawLine(gInternal.renderer, R3x, R3y, R4x, R4y);
      SDL_RenderDrawLine(gInternal.renderer, R4x, R4y, R1x, R1y);



      //SDL_RenderDrawLine(gInternal.renderer, P1x, P1y, P2x, P2y);
      //SDL_RenderDrawLine(gInternal.renderer, P2x, P2y, P3x, P3y);
      //SDL_RenderDrawLine(gInternal.renderer, P3x, P3y, P4x, P4y);
      //SDL_RenderDrawLine(gInternal.renderer, P4x, P4y, P1x, P1y);
      //x1:=x1+(bound.x*sizex1) div 100;
      //y1:=y1+(bound.y*sizey1) div 100;
      //xL1:=x1+(bound.w*sizex1) div 100;
      //yL1:=y1+(bound.h*sizey1) div 100;
      //if (y2-y1) <> 0 then
      //  Pang := Trunc(arctan((x2-x1) div (y2-y1))*1000)
      //else
      //  Pang := 90000;


      (*
      rect.x := x1+ Ox+rect.w div 2;
      rect.y := y1+ Oy+rect.h div 2;
      drect.x := x1+ Ox;
      drect.y := y1+ Oy;
      drect.w := rect.w;
      drect.h := rect.h;

      SDL_SetRenderDrawColor(gInternal.renderer, 255, 0, 100, 100);
      SDL_RenderDrawRect(gInternal.renderer, @drect);


      cosb := dcos(-angle1);
      senb := dsin(-angle1);
      px := rect.x+Trunc((rect.x-x2)*cosb-(rect.y-y2)*senb);
      py := rect.y+Trunc((rect.y-y2)*cosb+(rect.x-x2)*senb);
      SDL_RenderDrawLine(gInternal.renderer, x1, y1, px, py);
      *)


      //if (px < rect.x-rect.w div 2) then px := rect.x-rect.w div 2;
      //if (px > rect.x+rect.w div 2) then px := rect.x+rect.w div 2;
      //if (py < rect.y-rect.h div 2) then py := rect.y-rect.h div 2;
      //if (py > rect.y+rect.h div 2) then py := rect.y+rect.h div 2;

      //SDL_SetRenderDrawColor(gInternal.renderer, 255, 100, 0, 100);
      //SDL_RenderDrawPoint(gInternal.renderer, px, py);
      //SDL_RenderDrawLine(gInternal.renderer, rect.x, rect.y, px, py);


      //cosb := dcos(angle1);
      //senb := dsin(angle1);

      //px := rect.x-px; py := rect.y-py;
      //ppx := rect.x+Trunc(px*cosb-py*senb); ppy := rect.y+Trunc(py*cosb+px*senb);

      //SDL_SetRenderDrawColor(gInternal.renderer, 255, 0, 0, 100);
      //SDL_RenderDrawLine(gInternal.renderer, rect.x, rect.y, ppx, ppy);



      (*
      cosb2 := dcos(-angle1+angle2);
      senb2 := dsin(-angle1+angle2);

      O2x := (obound.x*sizex2) div 100;
      O2y := (obound.y*sizey2) div 100;

      rect2.x:=px+Trunc(O2x*cosb2-O2y*senb2);
      rect2.y:=py+Trunc(O2y*cosb2+O2x*senb2);
      rect2.w:=(obound.w*sizex2) div 100;
      rect2.h:=(obound.h*sizey2) div 100;

      P1x := 0;         P1y := 0;
      P2x := rect2.w;   P2y := 0;
      P3x := rect2.w;   P3y := rect2.h;
      P4x := 0;         P4y := rect2.h;

      R1x := Trunc(P1x*cosb2-P1y*senb2); R1y := Trunc(P1y*cosb2+P1x*senb2);
      R1x := R1x+rect2.x; R1y := R1y+rect2.y;
      R2x := Trunc(P2x*cosb2-P2y*senb2); R2y := Trunc(P2y*cosb2+P2x*senb2);
      R2x := R2x+rect2.x; R2y := R2y+rect2.y;
      R3x := Trunc(P3x*cosb2-P3y*senb2); R3y := Trunc(P3y*cosb2+P3x*senb2);
      R3x := R3x+rect2.x; R3y := R3y+rect2.y;
      R4x := Trunc(P4x*cosb2-P4y*senb2); R4y := Trunc(P4y*cosb2+P4x*senb2);
      R4x := R4x+rect2.x; R4y := R4y+rect2.y;

      SDL_SetRenderDrawColor(gInternal.renderer, 255, 100, 200, 100);
      // Rectangulo que forma
      SDL_RenderDrawLine(gInternal.renderer, R1x, R1y, R2x, R2y);
      SDL_RenderDrawLine(gInternal.renderer, R2x, R2y, R3x, R3y);
      SDL_RenderDrawLine(gInternal.renderer, R3x, R3y, R4x, R4y);
      SDL_RenderDrawLine(gInternal.renderer, R4x, R4y, R1x, R1y);

      // Puntos proyectados
      SDL_RenderDrawPoint(gInternal.renderer, R1x, y1);
      SDL_RenderDrawPoint(gInternal.renderer, x1, R1y);
      SDL_RenderDrawPoint(gInternal.renderer, R2x, y1);
      SDL_RenderDrawPoint(gInternal.renderer, x1, R2y);
      SDL_RenderDrawPoint(gInternal.renderer, R3x, y1);
      SDL_RenderDrawPoint(gInternal.renderer, x1, R3y);
      SDL_RenderDrawPoint(gInternal.renderer, R4x, y1);
      SDL_RenderDrawPoint(gInternal.renderer, x1, R4y);

      // Buscamos maximo y minimo
      Xmin := R1x;
      if (R2x < Xmin) then Xmin := R2x;
      if (R3x < Xmin) then Xmin := R3x;
      if (R4x < Xmin) then Xmin := R4x;
      Xmax := R4x;
      if (R3x > Xmax) then Xmax := R3x;
      if (R2x > Xmax) then Xmax := R2x;
      if (R1x > Xmax) then Xmax := R1x;

      if not diyen_bound_aa_with(Xmin, Xmax, drect.x, drect.x+drect.w) then
      begin
        SDL_SetRenderDrawColor(gInternal.renderer, 255, 255, 200, 100);
        SDL_RenderDrawLine(gInternal.renderer, drect.x, y1, drect.x+drect.w, y1);
        SDL_SetRenderDrawColor(gInternal.renderer, 100, 200, 200, 100);
        SDL_RenderDrawLine(gInternal.renderer, Xmin, y1, Xmax, y1);
      end;

      Ymin := R1y;
      if (R2y < Ymin) then Ymin := R2y;
      if (R3y < Ymin) then Ymin := R3y;
      if (R4y < Ymin) then Ymin := R4y;
      Ymax := R4y;
      if (R3y > Ymax) then Ymax := R3y;
      if (R2y > Ymax) then Ymax := R2y;
      if (R1y > Ymax) then Ymax := R1y;

      if not diyen_bound_aa_with(Ymin, Ymax, drect.y, drect.y+drect.h) then
      begin
        SDL_SetRenderDrawColor(gInternal.renderer, 255, 255, 200, 100);
        SDL_RenderDrawLine(gInternal.renderer, x1, drect.y, x1, drect.y+drect.h);
        SDL_SetRenderDrawColor(gInternal.renderer, 100, 200, 200, 100);
        SDL_RenderDrawLine(gInternal.renderer, x1, Ymin, x1, Ymax);
      end;
      *)


      (*r:=abs(bound.w*greater(sizex1, sizey1)) div 100;
      rect.x:=x1+(bound.x*sizex1) div 100;
      rect.y:=y1+(bound.y*sizey1) div 100;
      rect.w:=r;
      rect.h:=r;
      SDL_SetRenderDrawColor(gInternal.renderer, 200, 250, 100, 100);
      SDL_RenderDrawPoint(gInternal.renderer, rect.x, rect.y);

      ang := 0;
      xant := rect.x;
      yant := rect.y;
      while ang <= pi*2 do
      begin
        x := rect.x+Trunc(sin(ang)*r);
        y := rect.y+Trunc(cos(ang)*r);
        SDL_RenderDrawLine(gInternal.renderer, xant, yant, x, y);
        xant := x; yant := y;
        ang := ang + pi / 8;
      end;*)
    end;

    otherwise
    begin
      r:=abs(bound.w*greater(sizex1, sizey1)) div 100;
      cosb := dcos(angle1);
      senb := dsin(angle1);

      Ox := (bound.x*sizex1) div 100;
      Oy := (bound.y*sizey1) div 100;
      rect.x:=x1+Trunc(Ox*cosb-Oy*senb);
      rect.y:=y1+Trunc(Oy*cosb+Ox*senb);
      //rect.x:=x1+(bound.x*sizex1) div 100;
      //rect.y:=y1+(bound.y*sizey1) div 100;
      rect.w:=r;
      rect.h:=r;
      SDL_SetRenderDrawColor(gInternal.renderer, 0, 255, 0, 100);
      SDL_RenderDrawPoint(gInternal.renderer, rect.x, rect.y);

      ang := 0;
      xant := rect.x;
      yant := rect.y;
      while ang <= pi*2 do
      begin
        x := rect.x+Trunc(sin(ang)*r);
        y := rect.y+Trunc(cos(ang)*r);
        SDL_RenderDrawLine(gInternal.renderer, xant, yant, x, y);
        xant := x; yant := y;
        ang := ang + pi / 8;
      end;
      (*rect.x := rect.x-rect.w;
      rect.y := rect.y-rect.h;
      rect.w:=r*2;
      rect.h:=r*2;
      SDL_RenderDrawRect(gInternal.renderer, @rect);*)
    end;
  end;

  case obound.t of
    BOUND_AA_SQUARE:
    begin
      r := greater(sizex2, sizey2);
      rect.x:=x2+(obound.x*r) div 100;
      rect.y:=y2+(obound.y*r) div 100;
      rect.w:=(obound.w*r) div 100;
      rect.h:=rect.w;
      SDL_SetRenderDrawColor(gInternal.renderer, 0, 255, 255, 100);
      SDL_RenderDrawRect(gInternal.renderer, @rect);
    end;
    BOUND_AA_RECT:
    begin
      rect.x:=x2+(obound.x*sizex2) div 100;
      rect.y:=y2+(obound.y*sizey2) div 100;
      rect.w:=(obound.w*sizex2) div 100;
      rect.h:=(obound.h*sizey2) div 100;
      SDL_SetRenderDrawColor(gInternal.renderer, 0, 255, 255, 100);
      SDL_RenderDrawRect(gInternal.renderer, @rect);
    end;
    BOUND_RECTANGLE:
    begin
      cosb := dcos(angle2);
      senb := dsin(angle2);

      rect.x:=(obound.x*sizex2) div 100;
      rect.y:=(obound.y*sizey2) div 100;
      rect.w:=rect.x+(obound.w*sizex2) div 100;
      rect.h:=rect.y+(obound.h*sizey2) div 100;

      P1x := rect.x;  P1y := rect.y;
      P2x := rect.w;  P2y := rect.y;
      P3x := rect.w;  P3y := rect.h;
      P4x := rect.x;  P4y := rect.h;

      R1x := Trunc(P1x*cosb-P1y*senb);  R1y := Trunc(P1y*cosb+P1x*senb);
      R1x := R1x+x2;                    R1y := R1y+y2;
      R2x := Trunc(P2x*cosb-P2y*senb);  R2y := Trunc(P2y*cosb+P2x*senb);
      R2x := R2x+x2;                    R2y := R2y+y2;
      R3x := Trunc(P3x*cosb-P3y*senb);  R3y := Trunc(P3y*cosb+P3x*senb);
      R3x := R3x+x2;                    R3y := R3y+y2;
      R4x := Trunc(P4x*cosb-P4y*senb);  R4y := Trunc(P4y*cosb+P4x*senb);
      R4x := R4x+x2;                    R4y := R4y+y2;

      (*Ox := (obound.x*sizex2) div 100;
      Oy := (obound.y*sizey2) div 100;

      rect.x:=x2+Trunc(Ox*cosb-Oy*senb);
      rect.y:=y2+Trunc(Oy*cosb+Ox*senb);
      rect.w:=(obound.w*sizex2) div 100;
      rect.h:=(obound.h*sizey2) div 100;*)

      //P1x := rect.w;
      //P1y := rect.h;

      //P2x := -P1x; P2y := P1y;
      //P3x := -P1x; P3y := -P1y;
      //P4x := P1x; P4y := -P1y;
      (*P1x := 0;       P1y := 0;
      P2x := rect.w;  P2y := 0;
      P3x := rect.w;  P3y := rect.h;
      P4x := 0;       P4y := rect.h;

      R1x := Trunc(P1x*cosb-P1y*senb); R1y := Trunc(P1y*cosb+P1x*senb);
      R1x := R1x+rect.x; R1y := R1y+rect.y;
      R2x := Trunc(P2x*cosb-P2y*senb); R2y := Trunc(P2y*cosb+P2x*senb);
      R2x := R2x+rect.x; R2y := R2y+rect.y;
      R3x := Trunc(P3x*cosb-P3y*senb); R3y := Trunc(P3y*cosb+P3x*senb);
      R3x := R3x+rect.x; R3y := R3y+rect.y;
      R4x := Trunc(P4x*cosb-P4y*senb); R4y := Trunc(P4y*cosb+P4x*senb);
      R4x := R4x+rect.x; R4y := R4y+rect.y;*)

      //SDL_SetRenderDrawColor(gInternal.renderer, 255, 0, 0, 100);
      //SDL_RenderDrawRect(gInternal.renderer, @rect);

      SDL_SetRenderDrawColor(gInternal.renderer, 0, 255, 255, 100);
      SDL_RenderDrawLine(gInternal.renderer, R1x, R1y, R2x, R2y);
      SDL_RenderDrawLine(gInternal.renderer, R2x, R2y, R3x, R3y);
      SDL_RenderDrawLine(gInternal.renderer, R3x, R3y, R4x, R4y);
      SDL_RenderDrawLine(gInternal.renderer, R4x, R4y, R1x, R1y);
    end;

    otherwise
    begin
      r:=abs(obound.w*greater(sizex2, sizey2)) div 100;
      cosb := dcos(angle2);
      senb := dsin(angle2);

      Ox := (obound.x*sizex2) div 100;
      Oy := (obound.y*sizey2) div 100;
      rect.x:=x2+Trunc(Ox*cosb-Oy*senb);
      rect.y:=y2+Trunc(Oy*cosb+Ox*senb);
      //rect.x:=x2+(obound.x*sizex2) div 100;
      //rect.y:=y2+(obound.y*sizey2) div 100;
      rect.w:=r;
      rect.h:=r;
      SDL_SetRenderDrawColor(gInternal.renderer, 0, 255, 255, 100);
      SDL_RenderDrawPoint(gInternal.renderer, rect.x, rect.y);

      ang := 0;
      xant := rect.x;
      yant := rect.y;
      while ang <= pi*2 do
      begin
        x := rect.x+Trunc(sin(ang)*r);
        y := rect.y+Trunc(cos(ang)*r);
        SDL_RenderDrawLine(gInternal.renderer, xant, yant, x, y);
        xant := x; yant := y;
        ang := ang + pi / 8;
      end;
      (*rect.x := rect.x-rect.w;
      rect.y := rect.y-rect.h;
      rect.w:=r*2;
      rect.h:=r*2;
      SDL_RenderDrawRect(gInternal.renderer, @rect);*)
    end;
  end;

  SDL_SetRenderDrawColor(gInternal.renderer, 0, 0, 0, 255);
end;

(*procedure diyen_debug_collision(
  x1, y1, sizex1, sizey1,
  x2, y2, sizex2, sizey2: integer;
  angle1, angle2 : integer;
  bound, obound: TBound);
begin
  diyen_debug_collision(
  x1, y1, sizex1, sizey1,
  x2, y2, sizex2, sizey2,
  angle1, angle2,
  bound, obound,
  0, 0);
end;*)

{$endif}

procedure diyen_bound_circle_vals(x, y: pinteger; a, aL, b, bL: integer);
begin
  if (x^ < a) then x^ := a;
  if (x^ > aL) then x^ := aL;
  if (y^ < b) then y^ := b;
  if (y^ > bL) then y^ := bL;
end;

function diyen_bound_circle_with(x1, y1, r1, x2, y2, r2 : integer ): integer;
var
  a, b : integer;
  rd   : integer;
begin
  x1 := (x2-x1);   a := x1*x1;
  y1 := (y2-y1);   b := y1*y1;
  rd := (r1+r2);
  Result := (a+b)-rd*rd;
end;

function diyen_bound_circle_p(
  x, y, r,
  x2, y2, sizex2, sizey2, angle2: integer;
  obound: PBound): integer;
var
  r1, r2 : integer;
  px, py : integer;
  xL2, yL2 : integer;
  w, h : integer;
  Ox, Oy : integer;
  cosb, senb : float;
begin
  case obound^.t of
    BOUND_CIRCLE: // Circulo con circulo
    begin
      r2 := abs(obound^.w*greater(sizex2, sizey2)) div 100;

      cosb := dcos(angle2);
      senb := dsin(angle2);

      Ox := (obound^.x*sizex2) div 100;
      Oy := (obound^.y*sizey2) div 100;
      x2:=x2+Trunc(Ox*cosb-Oy*senb);
      y2:=y2+Trunc(Oy*cosb+Ox*senb);

      Result := diyen_bound_circle_with(x, y, r, x2, y2, r2);
    end;
    BOUND_AA_SQUARE: // Circulo con cuadrado alineado a los ejes
    begin
      r2 := greater(sizex2, sizey2);
      diyen_bound_aa_vals(@x2, @xL2, r2, obound^.x, obound^.w);
      diyen_bound_aa_vals(@y2, @yL2, r2, obound^.y, obound^.w);

      px := x; py := y;
      diyen_bound_circle_vals(@px, @py, x2, xL2, y2, yL2);
      //SDL_SetRenderDrawColor(gInternal.renderer, 0, 100, 200, 100);
      //SDL_RenderDrawLine(gInternal.renderer, px, py, x, y);
      //SDL_SetRenderDrawColor(gInternal.renderer, 0, 0, 0, 0);

      Result := diyen_bound_circle_with(x, y, r, px, py, 0);
    end;
    BOUND_AA_RECT: // Circulo con rectangulo alineado a los ejes
    begin
      diyen_bound_aa_vals(@x2, @xL2, sizex2, obound^.x, obound^.w);
      diyen_bound_aa_vals(@y2, @yL2, sizey2, obound^.y, obound^.h);

      px := x; py := y;
      diyen_bound_circle_vals(@px, @py, x2, xL2, y2, yL2);
      Result := diyen_bound_circle_with(x, y, r, px, py, 0);
    end;
    BOUND_RECTANGLE: // Circulo con rectangulo
    begin
      // Rotamos el punto del centro del círculo
      // con respecto al centro de la imagen del rectángulo
      cosb := dcos(-angle2);
      senb := dsin(-angle2);
      px := x2+Trunc((x-x2)*cosb-(y-y2)*senb);
      py := y2+Trunc((y-y2)*cosb+(x-x2)*senb);

      // El círculo desplazado según rotaciones
      x := px; y := py;

      // Acomodamos el rectángulo
      x2:=x2+(obound^.x*sizex2) div 100;
      y2:=y2+(obound^.y*sizey2) div 100;
      w := (obound^.w*sizex2) div 100;
      h := (obound^.h*sizey2) div 100;
      xL2:=x2+w;
      yL2:=y2+h;

      diyen_bound_circle_vals(@px, @py, x2, xL2, y2, yL2);

      Result := diyen_bound_circle_with(x, y, r, px, py, 0);
    end;
    otherwise // Por defecto usa el método de círculo
      begin
      r1 := greater(sizex2, sizey2);
      r2 := abs(obound^.w*r1) div 100;

      cosb := dcos(angle2);
      senb := dsin(angle2);

      Ox := (obound^.x*sizex2) div 100;
      Oy := (obound^.y*sizey2) div 100;
      x2:=x2+Trunc(Ox*cosb-Oy*senb);
      y2:=y2+Trunc(Oy*cosb+Ox*senb);

      Result := diyen_bound_circle_with(x, y, r, x2, y2, r2);
    end;
  end;
end;

procedure diyen_bound_aa_vals(a, aL: pinteger; size, c, L: integer);
begin
  // c+a |------ L -----| aL (c+a+L)
  a^    := a^+(c*size) div 100;
  aL^   := a^+(L*size) div 100;
end;

function diyen_bound_aa_with(a1, aL1, a2, aL2 : integer): boolean;
begin
  //a1 |----------| aL1                    a1 |----------| aL1
  //                       a2 |----| aL2
  Result := (a1 > aL2) or (aL1 < a2);
end;


function diyen_bound_aacuad_p(
  x1, y1, sizex1, sizey1,
  x2, y2, sizex2, sizey2, angle2: integer;
  bound, obound: PBound): integer;
var
  xL1, xL2 : integer;
  yL1, yL2 : integer;
  r1, r2 : integer;
  px, py : integer;
  Ox, Oy : integer;
  cosb, senb : float;

  xb1, yb1, wb1, hb1 : integer;
  xb2, yb2, wb2, hb2 : integer;
begin
  Result := 0;
  case obound^.t of
    BOUND_AA_SQUARE: // Cuadrado alineado a ejes con cuadrado alineado a ejes
    begin
      r1 := greater(sizex1, sizey1);
      r2 := greater(sizex2, sizey2);
      diyen_bound_aa_vals(@x1, @xL1, r1, bound^.x, bound^.w);
      diyen_bound_aa_vals(@x2, @xL2, r2, obound^.x, obound^.w);
      if (diyen_bound_aa_with(x1, xL1, x2, xL2)) then Exit;

      diyen_bound_aa_vals(@y1, @yL1, r1, bound^.y, bound^.w);
      diyen_bound_aa_vals(@y2, @yL2, r2, obound^.y, obound^.w);
      if (diyen_bound_aa_with(y1, yL1, y2, yL2)) then Exit;

      Result := -1;
    end;
    BOUND_AA_RECT: // Cuadrado alineado a ejes con rectangulo alineado a ejes
    begin
      r1 := greater(sizex1, sizey1);
      diyen_bound_aa_vals(@x1, @xL1, r1, bound^.x, bound^.w);
      diyen_bound_aa_vals(@x2, @xL2, sizex2, obound^.x, obound^.w);
      if (diyen_bound_aa_with(x1, xL1, x2, xL2)) then Exit;

      diyen_bound_aa_vals(@y1, @yL1, r1, bound^.y, bound^.w);
      diyen_bound_aa_vals(@y2, @yL2, sizey2, obound^.y, obound^.h);
      if (diyen_bound_aa_with(y1, yL1, y2, yL2)) then Exit;

      Result := -1;
    end;
    BOUND_RECTANGLE: // Cuadrado fijo con rectangulo
    begin
      r1 := greater(sizex1, sizey1);
      xb1 := (bound^.x*r1) div 100;
      yb1 := (bound^.y*r1) div 100;
      wb1 := (bound^.w*r1) div 100;
      hb1 := (bound^.h*r1) div 100;

      xb2 := (obound^.x*sizex2) div 100;
      yb2 := (obound^.y*sizey2) div 100;
      wb2 := (obound^.w*sizex2) div 100;
      hb2 := (obound^.h*sizey2) div 100;

      // Primero comprobamos desde el primer rectángulo
      if diyen_bound_rect_rect(xb1, yb1, wb1, hb1,
          xb2, yb2, wb2, hb2,
          0, angle2,
          x1, y1, x2, y2) then Exit;
      // Ahora comprobamos desde el segundo
      if diyen_bound_rect_rect(xb2, yb2, wb2, hb2,
          xb1, yb1, wb1, hb1,
          angle2, 0,
          x2, y2, x1, y1) then Exit;

      Result := -1;
    end;
    BOUND_CIRCLE: // Cuadrado fijo con circulo
    begin
      r1 := greater(sizex1, sizey1);
      diyen_bound_aa_vals(@x1, @xL1, r1, bound^.x, bound^.w);
      diyen_bound_aa_vals(@y1, @yL1, r1, bound^.y, bound^.w);

      r2 := abs(obound^.w*greater(sizex2, sizey2)) div 100;
      cosb := dcos(angle2);
      senb := dsin(angle2);

      Ox := (obound^.x*sizex2) div 100;
      Oy := (obound^.y*sizey2) div 100;
      x2:=x2+Trunc(Ox*cosb-Oy*senb);
      y2:=y2+Trunc(Oy*cosb+Ox*senb);

      px := x2; py := y2;
      diyen_bound_circle_vals(@px, @py, x1, xL1, y1, yL1);
      Result := diyen_bound_circle_with(x2, y2, r2, px, py, 0);
    end;
    otherwise // Por defecto usa el método de círculo
      begin
      r1 := abs(obound^.w*greater(sizex1, sizey1)) div 100;
      r2 := greater(sizex1, sizey1);
      x1:=x1+(obound^.x*sizex1) div 100;
      y1:=y1+(obound^.y*sizey1) div 100;
      Result := diyen_bound_circle_with(x1, y1, r1, x2, y2, r2);
      end;
  end;
end;

function diyen_bound_aarect_p(
  x1, y1, sizex1, sizey1,
  x2, y2, sizex2, sizey2, angle2: integer;
  bound, obound: PBound): integer;
var
  xL1, xL2 : integer;
  yL1, yL2 : integer;
  r1, r2 : integer;
  px, py : integer;
  Ox, Oy : integer;
  xb1, yb1, wb1, hb1 : integer;
  xb2, yb2, wb2, hb2 : integer;
  senb, cosb : float;
begin
  Result := 0;
  case obound^.t of
    BOUND_AA_RECT: // Rectangulo alineado a ejes con rectangulo alineado a ejes
    begin
      diyen_bound_aa_vals(@x1, @xL1, sizex1, bound^.x, bound^.w);
      diyen_bound_aa_vals(@x2, @xL2, sizex2, obound^.x, obound^.w);
      if (diyen_bound_aa_with(x1, xL1, x2, xL2)) then Exit;

      diyen_bound_aa_vals(@y1, @yL1, sizey1, bound^.y, bound^.h);
      diyen_bound_aa_vals(@y2, @yL2, sizey2, obound^.y, obound^.h);
      if (diyen_bound_aa_with(y1, yL1, y2, yL2)) then Exit;

      Result := -1;
    end;
    BOUND_AA_SQUARE: // Rectangulo alineado a ejes con cuadrado alineado a ejes
    begin
      r2 := greater(sizex2, sizey2);

      diyen_bound_aa_vals(@x1, @xL1, sizex1, bound^.x, bound^.w);
      diyen_bound_aa_vals(@x2, @xL2, r2, obound^.x, obound^.w);
      if (diyen_bound_aa_with(x1, xL1, x2, xL2)) then Exit;

      diyen_bound_aa_vals(@y1, @yL1, sizey1, bound^.y, bound^.h);
      diyen_bound_aa_vals(@y2, @yL2, r2, obound^.y, obound^.w);
      if (diyen_bound_aa_with(y1, yL1, y2, yL2)) then Exit;

      Result := -1;
    end;
    BOUND_RECTANGLE: // Rectangulo fijo con rectangulo
    begin
      xb1 := (bound^.x*sizex1) div 100;
      yb1 := (bound^.y*sizey1) div 100;
      wb1 := (bound^.w*sizex1) div 100;
      hb1 := (bound^.h*sizey1) div 100;

      xb2 := (obound^.x*sizex2) div 100;
      yb2 := (obound^.y*sizey2) div 100;
      wb2 := (obound^.w*sizex2) div 100;
      hb2 := (obound^.h*sizey2) div 100;
      // Primero comprobamos desde el primer rectángulo
      if diyen_bound_rect_rect(xb1, yb1, wb1, hb1,
          xb2, yb2, wb2, hb2,
          0, angle2,
          x1, y1, x2, y2) then Exit;
      // Ahora comprobamos desde el segundo
      if diyen_bound_rect_rect(xb2, yb2, wb2, hb2,
          xb1, yb1, wb1, hb1,
          angle2, 0,
          x2, y2, x1, y1) then Exit;

      Result := -1;
    end;
    BOUND_CIRCLE: // Rectangulo fijo con circulo
    begin
      diyen_bound_aa_vals(@x1, @xL1, sizex1, bound^.x, bound^.w);
      diyen_bound_aa_vals(@y1, @yL1, sizey1, bound^.y, bound^.h);

      r2 := abs(obound^.w*greater(sizex2, sizey2)) div 100;
      cosb := dcos(angle2);
      senb := dsin(angle2);

      Ox := (obound^.x*sizex2) div 100;
      Oy := (obound^.y*sizey2) div 100;
      x2:=x2+Trunc(Ox*cosb-Oy*senb);
      y2:=y2+Trunc(Oy*cosb+Ox*senb);

      px := x2; py := y2;
      diyen_bound_circle_vals(@px, @py, x1, xL1, y1, yL1);
      Result := diyen_bound_circle_with(x2, y2, r2, px, py, 0);
    end;
    otherwise // Por defecto usa el metodo de circulo
      begin
      r1 := abs(obound^.w*greater(sizex1, sizey1)) div 100;
      r2 := greater(sizex1, sizey1);
      x1:=x1+(obound^.x*r2) div 100;
      y1:=y1+(obound^.y*r2) div 100;
      Result := diyen_bound_circle_with(x1, y1, r1, x2, y2, r2);
    end;
  end;
end;

// Rectangulo con Rectangulo
function diyen_bound_rect_rect(
  x1, y1, w1, h1, x2, y2, w2, h2 : integer;
  angle1, angle2 : integer;
  c1x, c1y, c2x, c2y : integer ) : boolean;
var
  x, y : integer;

  Xmin, Xmax, Ymin, Ymax : integer;

  cosb, senb : float;
  px, py : integer;

  P1x, P2x, P3x, P4x : integer;
  P1y, P2y, P3y, P4y : integer;

  R1x, R2x, R3x, R4x : integer;
  R1y, R2y, R3y, R4y : integer;

  //rect : TSDL_Rect;
begin
  Result := True;
  // Hay que hallar el punto con respecto al centro de su imagen
  // y del centro de la imagen del segundo rectángulo

  // +---+
  // |   |
  // |   |   |Imagen|
  // |   |
  // +---+

  // Primer punto con respecto a su propia imagen rotada
  cosb := dcos(angle2);
  senb := dsin(angle2);
  x := c2x+Trunc(x2*cosb-y2*senb);
  y := c2y+Trunc(y2*cosb+x2*senb);

  //SDL_SetRenderDrawColor(gInternal.renderer, 120, 0, 0, 128);
  //SDL_RenderDrawLine(gInternal.renderer, c2x, c2y, x, y);

  // Ahora rotamos nuevamente el punto pero con respecto al centro de la
  // otra imagen
  x := x-c1x;
  y := y-c1y;
  cosb := dcos(-angle1);
  senb := dsin(-angle1);
  px := c1x+Trunc(x*cosb-y*senb);
  py := c1y+Trunc(y*cosb+x*senb);
  //SDL_RenderDrawLine(gInternal.renderer, c2x, c2y, px, py);

      // Rotamos el segundo rectángulo
      // transportándolo con respecto al primero
      // Ya obtuvimos el punto para transportar anteriormente (px, py)
      cosb := dcos(angle2-angle1);
      senb := dsin(angle2-angle1);

      P1x := 0;    P1y := 0;
      P2x := w2;   P2y := 0;
      P3x := w2;   P3y := h2;
      P4x := 0;    P4y := h2;

      //R1x := px+Trunc(P1x*cosb-P1y*senb);
      //R1y := py+Trunc(P1y*cosb+P1x*senb);

      //R2x := px+Trunc(P2x*cosb-P2y*senb);
      //R2y := py+Trunc(P2y*cosb+P2x*senb);

      //R3x := px+Trunc(P3x*cosb-P3y*senb);
      //R3y := py+Trunc(P3y*cosb+P3x*senb);

      //R4x := px+Trunc(P4x*cosb-P4y*senb);
      //R4y := py+Trunc(P4y*cosb+P4x*senb);

      // Simplificado
      R1x := px;
      R1y := py;

      R2x := px+Trunc(P2x*cosb);
      R2y := py+Trunc(P2x*senb);

      R3x := px+Trunc(P3x*cosb-P3y*senb);
      R3y := py+Trunc(P3y*cosb+P3x*senb);

      R4x := px+Trunc(-P4y*senb);
      R4y := py+Trunc(P4y*cosb);


      //SDL_SetRenderDrawColor(gInternal.renderer, 100, 200, 200, 100);
      //SDL_RenderDrawLine(gInternal.renderer, R1x, R1y, R2x, R2y);
      //SDL_RenderDrawLine(gInternal.renderer, R2x, R2y, R3x, R3y);
      //SDL_RenderDrawLine(gInternal.renderer, R3x, R3y, R4x, R4y);
      //SDL_RenderDrawLine(gInternal.renderer, R4x, R4y, R1x, R1y);

      // Valores absolutos para el primer rectángulo
      x1 := x1 + c1x;
      y1 := y1 + c1y;

      // Puntos proyectados
      //SDL_SetRenderDrawColor(gInternal.renderer, 100, 200, 200, 100);
      //SDL_RenderDrawPoint(gInternal.renderer, R1x, y1);
      //SDL_RenderDrawPoint(gInternal.renderer, x1, R1y);
      //SDL_RenderDrawPoint(gInternal.renderer, R2x, y1);
      //SDL_RenderDrawPoint(gInternal.renderer, x1, R2y);
      //SDL_RenderDrawPoint(gInternal.renderer, R3x, y1);
      //SDL_RenderDrawPoint(gInternal.renderer, x1, R3y);
      //SDL_RenderDrawPoint(gInternal.renderer, R4x, y1);
      //SDL_RenderDrawPoint(gInternal.renderer, x1, R4y);
      //SDL_SetRenderDrawColor(gInternal.renderer, 0, 0, 0, 0);

      // Buscamos maximo y minimo
      Xmin := R1x;
      if (R2x < Xmin) then Xmin := R2x;
      if (R3x < Xmin) then Xmin := R3x;
      if (R4x < Xmin) then Xmin := R4x;
      Xmax := R4x;
      if (R3x > Xmax) then Xmax := R3x;
      if (R2x > Xmax) then Xmax := R2x;
      if (R1x > Xmax) then Xmax := R1x;

      //SDL_SetRenderDrawColor(gInternal.renderer, 120, 0, 0, 128);
      //SDL_RenderDrawPoint(gInternal.renderer, xmin, y1);
      //SDL_RenderDrawPoint(gInternal.renderer, xmax, y1);

      //SDL_SetRenderDrawColor(gInternal.renderer, 120, 120, 0, 128);
      ////SDL_RenderDrawLine(gInternal.renderer, x1, y1, x1+w1, y1+h1);
      //SDL_RenderDrawLine(gInternal.renderer, x1, y1, x1+w1, y1);
      //SDL_RenderDrawLine(gInternal.renderer, x1+w1, y1, x1+w1, y1+h1);
      //SDL_RenderDrawLine(gInternal.renderer, x1+w1, y1+h1, x1, y1+h1);
      //SDL_RenderDrawLine(gInternal.renderer, x1, y1+h1, x1, y1);

      //SDL_SetRenderDrawColor(gInternal.renderer, 0, 0, 0, 0);

      if diyen_bound_aa_with(Xmin, Xmax, x1, x1+w1) then Exit;

      Ymin := R1y;
      if (R2y < Ymin) then Ymin := R2y;
      if (R3y < Ymin) then Ymin := R3y;
      if (R4y < Ymin) then Ymin := R4y;
      Ymax := R4y;
      if (R3y > Ymax) then Ymax := R3y;
      if (R2y > Ymax) then Ymax := R2y;
      if (R1y > Ymax) then Ymax := R1y;

      //SDL_SetRenderDrawColor(gInternal.renderer, 120, 0, 0, 128);
      //SDL_RenderDrawPoint(gInternal.renderer, x1, ymin);
      //SDL_RenderDrawPoint(gInternal.renderer, x1, ymax);
      //SDL_SetRenderDrawColor(gInternal.renderer, 0, 0, 0, 0);

      if diyen_bound_aa_with(Ymin, Ymax, y1, y1+h1) then Exit;

      //SDL_SetRenderDrawColor(gInternal.renderer, 100, 200, 200, 100);
      //SDL_RenderDrawLine(gInternal.renderer, x1, Ymin, x1, Ymax);
      //SDL_SetRenderDrawColor(gInternal.renderer, 0, 0, 0, 255);

      Result := False;
end;


function diyen_bound_rect_p(
  x1, y1, sizex1, sizey1, x2, y2, sizex2, sizey2 : integer;
  angle1, angle2 : integer;
  bound, obound: PBound) : integer;
var
  r2 : integer;
  xL1, yL1, w, h : integer;
  Ox, Oy : integer;
  px, py : integer;
  cosb, senb : float;

  xb1, yb1, wb1, hb1 : integer;
  xb2, yb2, wb2, hb2 : integer;
begin
  Result := 0;

  case obound^.t of
    BOUND_RECTANGLE: // Rectangulo con rectangulo
    begin
      xb1 := (bound^.x*sizex1) div 100;
      yb1 := (bound^.y*sizey1) div 100;
      wb1 := (bound^.w*sizex1) div 100;
      hb1 := (bound^.h*sizey1) div 100;

      xb2 := (obound^.x*sizex2) div 100;
      yb2 := (obound^.y*sizey2) div 100;
      wb2 := (obound^.w*sizex2) div 100;
      hb2 := (obound^.h*sizey2) div 100;

      // Primero comprobamos desde el primer rectángulo
      if diyen_bound_rect_rect(xb1, yb1, wb1, hb1,
          xb2, yb2, wb2, hb2,
          angle1, angle2,
          x1, y1, x2, y2) then Exit;
      // Ahora comprobamos desde el segundo
      if diyen_bound_rect_rect(xb2, yb2, wb2, hb2,
          xb1, yb1, wb1, hb1,
          angle2, angle1,
          x2, y2, x1, y1) then Exit;

      Result := -1;
    end;
    BOUND_AA_RECT: // Rectangulo con rectangulo alineado a ejes
    begin
      xb1 := (bound^.x*sizex1) div 100;
      yb1 := (bound^.y*sizey1) div 100;
      wb1 := (bound^.w*sizex1) div 100;
      hb1 := (bound^.h*sizey1) div 100;

      xb2 := (obound^.x*sizex2) div 100;
      yb2 := (obound^.y*sizey2) div 100;
      wb2 := (obound^.w*sizex2) div 100;
      hb2 := (obound^.h*sizey2) div 100;

      // Primero comprobamos desde el primer rectángulo
      if diyen_bound_rect_rect(xb1, yb1, wb1, hb1,
          xb2, yb2, wb2, hb2,
          angle1, 0,
          x1, y1, x2, y2) then Exit;
      // Ahora comprobamos desde el segundo
      if diyen_bound_rect_rect(xb2, yb2, wb2, hb2,
          xb1, yb1, wb1, hb1,
          0, angle1,
          x2, y2, x1, y1) then Exit;

      Result := -1;
    end;
    BOUND_AA_SQUARE: // Rectangulo con cuadrado alineado a ejes
    begin
      xb1 := (bound^.x*sizex1) div 100;
      yb1 := (bound^.y*sizey1) div 100;
      wb1 := (bound^.w*sizex1) div 100;
      hb1 := (bound^.h*sizey1) div 100;

      r2  := greater(sizex2, sizey2);
      xb2 := (obound^.x*r2) div 100;
      yb2 := (obound^.y*r2) div 100;
      wb2 := (obound^.w*r2) div 100;
      hb2 := (obound^.h*r2) div 100;

      // Primero comprobamos desde el primer rectángulo
      if diyen_bound_rect_rect(xb1, yb1, wb1, hb1,
          xb2, yb2, wb2, hb2,
          angle1, angle2,
          x1, y1, x2, y2) then Exit;
      // Ahora comprobamos desde el segundo
      if diyen_bound_rect_rect(xb2, yb2, wb2, hb2,
          xb1, yb1, wb1, hb1,
          angle2, angle1,
          x2, y2, x1, y1) then Exit;

      Result := -1;
    end;
    BOUND_CIRCLE: // Rectangulo con círculo
    begin
      r2:=abs(obound^.w*greater(sizex2, sizey2)) div 100;

      cosb := dcos(angle2);
      senb := dsin(angle2);

      Ox := (obound^.x*sizex2) div 100;
      Oy := (obound^.y*sizey2) div 100;
      x2:=x2+Trunc(Ox*cosb-Oy*senb);
      y2:=y2+Trunc(Oy*cosb+Ox*senb);

      // Rotamos el punto del centro del círculo
      // con respecto al centro de la imagen del rectángulo
      cosb := dcos(-angle1);
      senb := dsin(-angle1);
      px := x1+Trunc((x2-x1)*cosb-(y2-y1)*senb);
      py := y1+Trunc((y2-y1)*cosb+(x2-x1)*senb);

      // El círculo desplazado según rotaciones
      x2 := px; y2 := py;

      // Acomodamos el rectángulo
      x1:=x1+(bound^.x*sizex1) div 100;
      y1:=y1+(bound^.y*sizey1) div 100;
      w := (bound^.w*sizex1) div 100;
      h := (bound^.h*sizey1) div 100;
      xL1:=x1+w;
      yL1:=y1+h;

      diyen_bound_circle_vals(@px, @py, x1, xL1, y1, yL1);

      Result := diyen_bound_circle_with(x2, y2, r2, px, py, 0);
    end;
  end;
end;

// Detect overlaping of two bounds and return true if they overlap or touch
function diyen_overlap_bounds(
  x1, y1, sizex1, sizey1, x2, y2, sizex2, sizey2 : integer;
  angle1, angle2 : integer;
  bound, obound: PBound): boolean;
var
  p : integer;
  x, y, r : integer;
  Ox, Oy : integer;
  cosb, senb : float;
begin
  case bound^.t of
    BOUND_CIRCLE: // Circulo con?
    begin
      r:=abs(bound^.w*greater(sizex1, sizey1)) div 100;

      cosb := dcos(angle1);
      senb := dsin(angle1);

      Ox := (bound^.x*sizex1) div 100;
      Oy := (bound^.y*sizey1) div 100;
      x:=x1+Trunc(Ox*cosb-Oy*senb);
      y:=y1+Trunc(Oy*cosb+Ox*senb);

      p := diyen_bound_circle_p(x, y, r, x2, y2, sizex2, sizey2, angle2, obound);
    end;
    BOUND_AA_SQUARE: // Cuadrado alineado a ejes con?
    begin
      p := diyen_bound_aacuad_p(x1, y1, sizex1, sizey1, x2, y2, sizex2, sizey2, angle2, bound, obound);
    end;
    BOUND_AA_RECT: // Rectángulo alineado a ejes con?
    begin
      p := diyen_bound_aarect_p(x1, y1, sizex1, sizey1, x2, y2, sizex2, sizey2, angle2, bound, obound);
    end;
    BOUND_RECTANGLE: // Rectángulo con?
    begin
      p := diyen_bound_rect_p(x1, y1, sizex1, sizey1, x2, y2, sizex2, sizey2, angle1, angle2, bound, obound);
    end;

    otherwise // Por defecto utiliza el método de círculo
    begin
      r:=abs(bound^.w*greater(sizex1, sizey1)) div 100;
      x:=x1+(bound^.x*sizex1) div 100; y:=y1+(bound^.y*sizey1) div 100;
      p := diyen_bound_circle_p(x, y, r, x2, y2, sizex2, sizey2, angle2, obound);
    end;
  end;
  Result := (p<0);

  {$ifdef DIYEN_DEBUG_COLS}
  if (Result) then
  diyen_debug_collision(
  x1, y1, sizex1, sizey1,
  x2, y2, sizex2, sizey2,
  angle1, angle2,
  bound, obound);
  {$endif}

end;

// Detect collision and hit filling the appropiate list
procedure diyen_colandhit_detection( process: TProcess );
var
  bound   : TBound;
  obound  : TBound;
  Map   : TMap;
  oMap  : TMap;
  n     : word;
  no    : word;

  pi    : word;
  i     : integer;
  other : TProcess;

  ncol : word = 0;
  //nhit : word = 0;

  x, y : integer;
  xo, yo : integer;

  //centro, ocentro : TSDL_Point;

  break_this : boolean;

  {$ifndef SCROLL_NO_COL}
  sc : PScroll;
  sn : pbyte;
  {$endif}

  {$ifdef DIYEN_DEBUG}
  //scrollx, scrolly : integer;
  reg : TSDL_Rect;
  {$endif}
begin
  process.Internal.id_scan:=0;
  process.Internal.scan_collision_list[1] := 0;
  if (process.graph <= 0) then Exit;
  if (process.alpha <= 0) then Exit;
  if (process.Internal.status <> STATUS_RUNNING) and
     (process.Internal.status <> STATUS_FROZEN) then Exit;
  Map := TMap(TFPG(diyen_fpg_list.fpg[process.file_]).maps[process.graph]);

  // Si hay algún tipo de colisión que detectar
  if (Map.col_bounds.bounds[0].t <> BOUND_NONE) or
     (Map.hit_bounds.bounds[0].t <> BOUND_NONE) then
  begin

  // Verificamos colisión con el graph0 de los scrolls
  {$ifndef SCROLL_NO_COL}
  {$ifdef ENABLE_TILEMAPS}
  if (process.Internal.process_id <= DIYEN_LMAX_PROCESS) then
  {$endif}
  for i := 0 to diyen_scroll_list.alist_max do
  begin
    sn := @diyen_scroll_list.alist[i];
    sc := @scroll[sn^];
    if (sc^.graph0 <= 0) then Continue;
    if (sc^.alpha0 <= 0) then Continue;
    oMap := TMap(TFPG(diyen_fpg_list.fpg[sc^.file_]).maps[sc^.graph0]);

    x := (process.x div process.resolution);
    y := (process.y div process.resolution);
    xo := sc^.x0;
    yo := sc^.y0;

    // Un map de scroll no necesita una colisión principal tipo FIXCUAD para
    // determinarse si colisiona. Pero el proceso sí.
    break_this := false;
    for no := oMap.col_bounds.first to oMap.col_bounds.last do
    begin
      if (oMap.col_bounds.bounds[no].t = BOUND_NONE) then Continue;

      // Aplicamos flip
      obound := oMap.col_bounds.bounds[no];
      if Boolean(sc^.flip0 and SDL_FLIP_HORIZONTAL) then  obound.x := obound.fx;
      if Boolean(sc^.flip0 and SDL_FLIP_VERTICAL) then    obound.y := obound.fy;

      // Aplicamos flip
      bound := Map.col_bounds.bounds[0];
      if Boolean(SDL_FLIP_HORIZONTAL and process.flip) then  bound.x := bound.fx;
      if Boolean(SDL_FLIP_VERTICAL and process.flip) then    bound.y := bound.fy;

      // Verificamos si alguna del scroll colisiona con la principal del proceso
      // Si es así comienza a analizar con las restantes del proceso.
      if diyen_overlap_bounds(
      x, y, process.size_x, process.size_y,
      xo, yo, 100, 100,
      process.angle, 0,
      @bound, @obound) then
      begin
        for n := Map.col_bounds.first to Map.col_bounds.last do
        begin
          // Aplicamos flip
          bound := Map.col_bounds.bounds[n];
          if Boolean(SDL_FLIP_HORIZONTAL and process.flip) then  bound.x := bound.fx;
          if Boolean(SDL_FLIP_VERTICAL and process.flip) then    bound.y := bound.fy;

          if diyen_overlap_bounds(
          x, y, process.size_x, process.size_y,
          xo, yo, 100, 100,
          process.angle, 0,
          @bound, @obound) then
          begin
            process.col(nil, n, no);
            {$ifdef DIYEN_DEBUG}
            diyen_debug_collision(
            x, y, process.size_x, process.size_y,
            xo, yo, 100, 100,
            process.angle, 0,
            bound, obound);
            {$endif}
            break_this := true;
            break;
          end;
        end;
      end;
      if (break_this) then break;
    end;
  end;
  {$endif}

  for pi := 1 to diyen_process_list.Max do
  begin
    other := TProcess(diyen_process_list[pi]);
    if (other = nil) then Continue;
    if (other.graph <=0) then Continue;
    if (other.alpha <=0) then Continue;
    if (other.Internal.process_id = process.Internal.process_id) then Continue;
    if (other.Internal.status <> STATUS_RUNNING) and
       (other.Internal.status <> STATUS_FROZEN) then Continue;
    // Sólo tipos que estén permitidos colisionar (dentro del valor posible)
    {$ifndef AUTO_PTYPE}
    (*if (other.process_type <= 32) and
       ((longword(1<<word(other.process_type)) and process.Internal.col_filter)
       <> longword(1<<word(other.process_type))) then Continue;*)
    {$else}
    (*if (other.process_type <= 32) and
       ((longword(1<<word(other.process_type)) and process.Internal.col_filter)
       <> longword(1<<word(other.process_type))) then Continue;*)
    {$endif}
    // Sólo elementos que estén dentro de la diferencia de Z aceptable
    if (abs(other.z-process.z) > collision_z) then Continue;

    oMap := TMap(TFPG(diyen_fpg_list.fpg[other.file_]).maps[other.graph]);

    // Aplicamos flip
    bound := Map.col_bounds.bounds[0];
    if Boolean(SDL_FLIP_HORIZONTAL and process.flip) then  bound.x := bound.fx;
    if Boolean(SDL_FLIP_VERTICAL and process.flip) then    bound.y := bound.fy;

    obound := oMap.col_bounds.bounds[0];
    if Boolean(SDL_FLIP_HORIZONTAL and other.flip) then  obound.x := obound.fx;
    if Boolean(SDL_FLIP_VERTICAL and other.flip) then    obound.y := obound.fy;

    // Solucionado: el cpoints[0] no se tiene que aplicar acá. Sino que
    // se tiene que aplicar en el editor de boundings.
    //centro.x :=   longint((Map.cpoints[0].x * process.size_x) div 100);
    //centro.y :=   longint((Map.cpoints[0].y * process.size_y) div 100);
    //ocentro.x :=  longint((oMap.cpoints[0].x * other.size_x) div 100);
    //ocentro.y :=  longint((oMap.cpoints[0].y * other.size_y) div 100);

    x := (process.x div process.resolution);
    y := (process.y div process.resolution);
    xo := (other.x div other.resolution);
    yo := (other.y div other.resolution);

    // Primero detectamos si hay colisión principal
    if (bound.t <> BOUND_NONE) and (obound.t <> BOUND_NONE) and
       (diyen_overlap_bounds(
       x, y, process.size_x, process.size_y,
       xo, yo, other.size_x, other.size_y,
       process.angle, other.angle,
       @bound, @obound)) then
    begin
      // Bound de colisión única
      if (Map.col_bounds.first = 1) and (Map.col_bounds.last = 0) then
      begin
        process.col(other, 0, 0);
        Inc(ncol);
        process.Internal.scan_collision_list[ncol] := other.Internal.process_id;
        {$ifdef DIYEN_DEBUG}
        sc := @scroll[0];
        reg := regions[sc^.sregion]^;
        diyen_debug_collision(
        x-sc^.x0+reg.x,
        y-sc^.y0+reg.y,
        process.size_x, process.size_y,
        xo-sc^.x0+reg.x,
        yo-sc^.y0+reg.y,
        other.size_x, other.size_y,
        process.angle, other.angle,
        bound, obound);
        {$endif}
      end else
      // Lista de bounds de colisión
      begin
        break_this := false;
        for no := oMap.col_bounds.first to oMap.col_bounds.last do
        begin
          for n := Map.col_bounds.first to Map.col_bounds.last do
          begin
            // Aplicamos flip
            bound := Map.col_bounds.bounds[n];
            if Boolean(SDL_FLIP_HORIZONTAL and process.flip) then  bound.x := bound.fx;
            if Boolean(SDL_FLIP_VERTICAL and process.flip) then    bound.y := bound.fy;

            obound := oMap.col_bounds.bounds[no];
            if Boolean(SDL_FLIP_HORIZONTAL and other.flip) then  obound.x := obound.fx;
            if Boolean(SDL_FLIP_VERTICAL and other.flip) then    obound.y := obound.fy;

            if diyen_overlap_bounds(
            x, y, process.size_x, process.size_y,
            xo, yo, other.size_x, other.size_y,
            process.angle, other.angle,
            @bound, @obound) then
            begin
              process.col(other, n, no);
              Inc(ncol);
              process.Internal.scan_collision_list[ncol] := other.Internal.process_id;
              {$ifdef DIYEN_DEBUG}
              sc := @scroll[0];
              reg := regions[sc^.sregion]^;
              diyen_debug_collision(
              x-sc^.x0+reg.x,
              y-sc^.y0+reg.y,
              process.size_x, process.size_y,
              xo-sc^.x0+reg.x,
              yo-sc^.y0+reg.y,
              other.size_x, other.size_y,
              process.angle, other.angle,
              bound, obound);
              {$endif}
              break_this := true;
              break;
            end;
          end;
          if (break_this) then break;
        end;
      end;
    end;

    // Hit
    // El hit es detectado desde el hit del originador con
    // algún bound de colisión del receptor.

    // Hit con col
    // Util para detectar cuando un personaje le da un golpe
    // al cuerpo del otro.
    {$ifndef NO_HIT_COL}
    bound := Map.hit_bounds.bounds[0];
    if Boolean(SDL_FLIP_HORIZONTAL and process.flip) then  bound.x := bound.fx;
    if Boolean(SDL_FLIP_VERTICAL and process.flip) then    bound.y := bound.fy;

    obound := oMap.col_bounds.bounds[0];
    if Boolean(SDL_FLIP_HORIZONTAL and other.flip) then  obound.x := obound.fx;
    if Boolean(SDL_FLIP_VERTICAL and other.flip) then    obound.y := obound.fy;

    // Primero detectamos si hay hit principal
    if (bound.t <> BOUND_NONE) and (obound.t <> BOUND_NONE) and
       (diyen_overlap_bounds(
       x, y, process.size_x, process.size_y,
       xo, yo, other.size_x, other.size_y,
       process.angle, other.angle,
       @bound, @obound)) then
    begin
      // Bound de hit única
      if (Map.hit_bounds.first = 1) and (Map.hit_bounds.last = 0) then
      begin
        process.hit(other, 0, 0);
        Inc(ncol);
        //process.Internal.scan_hit_list[nhit] := Pointer(other);
        {$ifdef DIYEN_DEBUG}
        diyen_debug_collision(
        x, y, process.size_x, process.size_y,
        xo, yo, other.size_x, other.size_y,
        process.angle, other.angle,
        bound, obound);
        {$endif}
      end else
      // Lista de bounds de colisión
      begin
        break_this := false;
        for no := oMap.col_bounds.first to oMap.col_bounds.last do
        begin
          for n := Map.hit_bounds.first to Map.hit_bounds.last do
          begin
            bound := Map.hit_bounds.bounds[n];
            if Boolean(SDL_FLIP_HORIZONTAL and process.flip) then  bound.x := bound.fx;
            if Boolean(SDL_FLIP_VERTICAL and process.flip) then    bound.y := bound.fy;

            obound := oMap.col_bounds.bounds[no];
            if Boolean(SDL_FLIP_HORIZONTAL and other.flip) then  obound.x := obound.fx;
            if Boolean(SDL_FLIP_VERTICAL and other.flip) then    obound.y := obound.fy;

            if diyen_overlap_bounds(
            x, y, process.size_x, process.size_y,
            xo, yo, other.size_x, other.size_y,
            process.angle, other.angle,
            @bound, @obound) then
            begin
              process.hit(other, n, no);
              Inc(ncol);
              //process.Internal.scan_hit_list[ncol] := Pointer(other);
              {$ifdef DIYEN_DEBUG}
              diyen_debug_collision(
              x, y, process.size_x, process.size_y,
              xo, yo, other.size_x, other.size_y,
              process.angle, other.angle,
              bound, obound);
              {$endif}
              break_this := true;
              break;
            end;
          end;
          if (break_this) then break;
        end;
      end;
    end;
    {$endif}

    // Hit con hit
    // Esto es útil para golpes que se chocan entre sí
    // Por ejemplo: los dos lanzan una patada a la vez y sólo chocan sus
    // pies (area de hit).
    {$ifdef HIT_HIT}
    bound := Map.hit_bounds.bounds[0];
    if Boolean(process.flip and SDL_FLIP_HORIZONTAL) then  bound.x := bound.fx;
    if Boolean(process.flip and SDL_FLIP_VERTICAL) then    bound.y := bound.fy;

    obound := oMap.hit_bounds.bounds[0];
    if Boolean(other.flip and SDL_FLIP_HORIZONTAL) then  obound.x := obound.fx;
    if Boolean(other.flip and SDL_FLIP_VERTICAL) then    obound.y := obound.fy;
    // Primero detectamos si hay hit principal
    if (bound.t <> BOUND_NONE) and (obound.t <> BOUND_NONE) and
       (diyen_overlap_bounds(
       x, y, process.size_x, process.size_y,
       xo, yo, other.size_x, other.size_y,
       process.angle, other.angle,
       @bound, @obound)) then
    begin
      // Bound de hit única
      if (Map.hit_bounds.first = 1) and (Map.hit_bounds.last = 0) then
      begin
        process.hithit(other, 0, 0);
        Inc(ncol);
        //process.Internal.scan_hit_list[ncol] := Pointer(other);
        {$ifdef DIYEN_DEBUG}
        diyen_debug_collision(
        x, y, process.size_x, process.size_y,
        xo, yo, other.size_x, other.size_y,
        process.angle, other.angle,
        bound, obound);
        {$endif}
      end else
      // Lista de bounds de hit
      begin
        break_this := false;
        for no := oMap.hit_bounds.first to oMap.hit_bounds.last do
        begin
          for n := Map.hit_bounds.first to Map.hit_bounds.last do
          begin
            bound := Map.hit_bounds.bounds[n];
            if Boolean(process.flip and SDL_FLIP_HORIZONTAL) then  bound.x := bound.fx;
            if Boolean(process.flip and SDL_FLIP_VERTICAL) then    bound.y := bound.fy;

            obound := oMap.hit_bounds.bounds[no];
            if Boolean(other.flip and SDL_FLIP_HORIZONTAL) then  obound.x := obound.fx;
            if Boolean(other.flip and SDL_FLIP_VERTICAL) then    obound.y := obound.fy;

            if diyen_overlap_bounds(
            x, y, process.size_x, process.size_y,
            xo, yo, other.size_x, other.size_y,
            process.angle, other.angle,
            @bound, @obound) then
            begin
              process.hithit(other, n, no);
              Inc(ncol);
              //process.Internal.scan_hit_list[ncol] := Pointer(other);
              {$ifdef DIYEN_DEBUG}
              diyen_debug_collision(
              x, y, process.size_x, process.size_y,
              xo, yo, other.size_x, other.size_y,
              process.angle, other.angle,
              bound, obound);
              {$endif}
              break_this := true;
              break;
            end;
          end;
          if (break_this) then break;
        end;
      end;
    end;
    {$endif}
  end;

  end;
  // TODO: Comprobar overflow
  //if (ncol <
    process.Internal.scan_collision_list[ncol+1] := 0;

end;


end.

