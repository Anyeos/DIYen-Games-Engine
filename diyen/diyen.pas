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



  DIYen Games Engine

  TODO:
   Parece que hay un bug grave de memoria. Aparentemente es un memory leak, ya
   que sólo sucede al cerrar el programa. No pude descubrir si fue del juego
   CCRobots en particular o si esta sucediendo dentro de DIYen.
   Sucede al ejecutar Halt(0) cuando se pretende cerrar el programa. Y es una
   excepción con un puntero entero.

   Mejorar el código de set_mode. <- Algo de eso ya está hecho

   Implementar Zoom para el scroll (así se tendrían pantallas con acercamiento).

   Acomodar para un modo OUYA.
   Corregir las asignaciones de teclas para los joysticks.
   Ver si se puede usar una manera genérica de generar mapeo de joystick tanto
   para PC como Android.


   Faltan implementar las funciones para trabajar sobre la pantalla entera.
   Por ejemplo se podrían aplicar efectos especiales a toda la escena.
   El fade es un ejemplo de algo así.
      Falta implementar fading.


   Algún tipo de impresión de registro de errores en lugar de usar say()
   Solución: Se podría usar SDL_Log, SDL_LogError, etc

   Falta el modo SCROLL por software.


   Mejorar el sistema de video: autodetectar límites de textura admitidos
   y utilizar un workaround en tal caso.

   Revisar use_priority (a ver si está hecho, no recuerdo)
   Revisar los COLMODE también (no sé si están terminados)

   Compatibilidad con coordenada tradicional, o sea, que la esquina 0,0 sea
   la inferior derecha (como los ejes cartesianos) y que Y al aumentar
   suba graficamente en lugar de bajar. <- Al pedo por ahora


   Preparar formatos propios de archivos. Hay que poner todo en una especie
   de paquete binario para poder salvarlo y cargarlo. Mejor que sean compatibles
   con el DIV. <- Compatibles con DIV para qué?

   Los gráficos puestos en pantalla principal deberán caer al fondo de todo
   sin importar el Z, pero también debería poderse poner gráficos al frente.
   O sea que las funciones podrían tener un parámetro extra para especificarlo.
   Estas funciones son put, xput, etc
   Podríamos tener entonces dos gráficos de pantalla: los del fondo, como
   tiene DIV, y los de frente, cuyo comportamiento sería similar a los del
   fondo sólo que estos se graficarán al frente.


   Revisar que no hayan errores con el uso de los punteros.
   Finalmente optimizar todo el código que se pueda para lograr más velocidad.

   Posibilidad de conseguir velocidad: Si ambos sizes son 100 entonces no
   sería necesario hacer las multiplicaciones con size.

   Implementar hotplug en el sistema de entrada.


   El modo de red:
   Por la red debería pasar la lista de procesos completa cada cierto tiempo.
   Podría ser configurable un intervalo de actualización desde el server a
   todos los clientes (el server haría eso).
   Del lado del cliente enviaría más seguido sólo los datos de un proceso.
   Cómo se podría implementar:
    En el lazo principal de DIYen el servidor, cuando sea el momento según
    el intervalo, enviará toda la lista de procesos.
    El cliente al recibir la lista deberá borrar toda su lista y crear una
    nueva con los datos que le entregó el servidor.
    El cliente siempre obedecerá al servidor de esa forma. Pero también actuará
    una vez recibidos los datos. Sólo que siempre que el servidor le envíe la
    lista, tendrá que obedecer a esa lista.
    De esa forma se ejecuta el juego igual pero será el servidor quien mande.
    Para que el cliente pueda crear cada vez los procesos, necesitará una lista
    de .Creates asociados a las clases correspondientes. Porque DIYen no tiene
    acceso a las clases que el desarrollador elabore. Sólo llega hasta TProcess.
    Entonces utilizará una lista de funciones que tendrán en su cuerpo la
    llamada a .Create correspondiente. Por ejemplo para la clase 'TJugador' la
    función tendrá "Result := TJugador.Create;" luego recibirá la data con su
    correspondiente procedure "receive(data: String);"

    Qué pasa para las clases con datos privados. Usaremos para eso un override
    de la función "function send: String;" y "procedure receive(data: String);".
    En la cual usarán alguna función de empaquetado que será la que usará el
    sistema de red. Donde empaquetará todo en una cadena.
    A partir de esas funciones es que construirá la cadena de red que deberá
    enviar a los clientes.
    La variable global "client" te dice el número de cliente que eres.
    Si es 0 es que eres el servidor, 1 el cliente numero 1, y así.
    Serviría para enviar los datos del cliente exclusivamente. Ejemplo:
    TJugador.loop;
    ...
      if client > 0 then clientsend;
    ...
    end;

    Esto hará que el cliente envíe los datos de ese proceso para que el servidor
    los procese.
    Entonces el servidor recibe el ID del proceso más los datos y los incorpora
    a su proceso local con ese ID.
    Esto sirve para que el cliente pueda controlar su personaje. Porque sino
    solo estaría viendo. Y para que el servidor no controle el personaje
    de algún jugador en red, debería llevar cuenta de los jugadores y sólo
    analizar conntroles en el equipo del jugador que corresponde.
    Pero todo eso lo deberá programar el desarrollador del juego. DIYen no se
    hace cargo de esa parte.

    Por supuesto que en este planteo no hay ninguna medida de seguridad
    y el cliente podrá enviar datos a cualquier PID y lo podrá controlar.
    Esa parte la pensaré luego.

    Para crear una partida se juega como normalmente, luego el cliente podrá
    ver su partida y unirse. Qué funciones se usarán y cómo todavía no pensé.



   Modos scroll de dos tipos:
    Modo DIV. Usando Z para la profundidad del scroll completo.
    Modo clásico. Donde el primer gráfico (frente) se ve delante
      de todos los otros gráficos sin importar su Z. Podría tener
      un ratio mayor.
      El segundo gráfico se ve detrás de todos los objetos. El ratio
      sería 1 a 1.
      Y el tercer gráfico se vería al fondo con un ratio menor para
      que se desplaze más lentamente.


   FEATURES / IDEAS:
      - Implementar zoom para los scrolls. Solamente haría zoom gráficamente
        pero no debería afectar en nada de lo demás (por ejemplo la detección
        de colisiones).

      - Incorporar la fnt_default en el código fuente.
        Incorporar los gráficos de onscreen joystick también en el fuente.

      - Crear funciones de FX útiles

      - Generador de sombra
        Con el engine FX actual se puede hacer girando la imagen, poniendola
        tipo escala de grises (supongo que será con r, g, b a 0) y desplazándola
        para que coincida con la imagen original. Esto daría una efecto de
        sombra en perspectiva. Bajándole alpha se hará semitransparente.

      - Asignación de texturizado a un texto.
        Que se pueda asignar un texturizado a un texto y aplicarle desplazamiento.
        Y sino fx_add a los textos? Por qué no?

      - Se podría hacer que se pueda elegir qué punto de control deberá seguir
        la función move_scroll. Así se pueden tener varios topes distintos de
        pantalla. La única utilidad que por ahora le veo es poder poner pantallas
        "ocultas" y destrancarlas si el usuario realiza cierta acción.
        O para obligar al usuario a ir por ciertos sitios donde se va
        desbloqueando la pantalla.
      - Incluir un motor de física.

      - Idea para hacer uso de .think en reemplazo o adición a .loop
        Que se estaría ejecutando sincronizado a un tiempo establecido en think_time
        De esta manera tendríamos una independencia de los fotogramas.
        Se puede hacer también dentro de cada .loop ya que .loop se ejecuta
        siempre por cada fotograma. Pero creo que es mejor que el código del
        juego sea lo más limpio posible.

      - Otra idea para hacer un sistema de esqueletos para animar piezas que
        componen al personaje. Se debería crear una FX para tal fin así sólo
        se procesa la imagen.
        Cómo funcionaría:
        En una función fx se tiene una estructura (record) que se cargó de
        un archivo.
        Que le indica cada pieza, su offset y el orden (cual primero y cual después).
        Entonces se pueden usar las únicas características que ya se tienen como
        el size, mover con x, y, color, tipo de blend, etc.
        Para eso se puede crear un programa editor de animaciones.
        Y cada animación puede estar en una estructura, o sea, un array de
        acciones. Y en el código diyen se pide ejecutar el número de acción y
        desde qué fotograma en todo caso.
        La función fx puede tener entrelazado así la animación se mantiene
        fluida siempre. Incluso cuando se pide cambiar de una a otra o cuando
        se pide tal fotograma de tal acción.



    // Modo Z
    // -dZLAYER
    // El modo Z permite definir qué algoritmo para el manejo de Z se quiere usar
    // Se disponen de dos algoritmos: El modo Z ordenado y el por capas (layer).
    // * El modo Z ordenado consiste en ordenar los procesos en función de su
    //  valor de Z.
    //  Luego el volcado en pantalla se hace ordenadamente (primero los Z más
    //  "profundos" y luego los de Z más "cercanos").
    //  Este algoritmo (ordenado) no necesita nada más para funcionar.
    //  Pero debido a que hace reordenamiento puede ser lento.
    // * El modo Z layer (capa) consiste en utilizar un arreglo de capas de Z.
    //  Luego se hace el volcado en pantalla de acuerdo a las capas: primero
    //  se vuelca la capa más "profunda" y así sucesivamente hasta llegar a la
    //  capa más "cercana".
    //  Este algoritmo necesita un valor de capas máximo.
    //  Ya que usa una capa por cada Z.
    //  De esa manera se puede saber cuántas capas se usarán.
    //  Estos valores normalmente se establecen al comienzo del programa y si
    //  algún proceso los intenta superar (porque nos equivocamos en su valor de
    //  Z) entonces el programa se sale con un error. Lo que sirve para evitar
    //  sobrecargar el sistema, ya que este algoritmo es más eficiente si
    //  tiene un número bajo de capas, de lo contrario podría llegar a ser
    //  más lento que el modo ordenado.

    //  También disponemos de un modo automático que aumenta el valor máximo
    //  de capas según la demanda.
    //  start_zlayer() sin parámetros usará el modo automático.

    //  Es una forma cómoda de manejarnos pero podríamos estar produciendo
    //  una cantidad excesiva de capas sin darnos cuenta (aunque esto es poco
    //  probable, sólo sucedería si usamos valores de Z demasiado distantes de
    //  un proceso a otro).

    //  -dZLAYER_RUNTIME permite usar ZLAYER en tiempo de ejecución.
    //  De lo contrario el modo ordenado nunca estará disponible.


    // Modos de colisión
    // -dCOLMODE_INDEPENDENT al compilador, de lo contrario se usará el inframe.
    **** NO IMPLEMENTADA ****

    // Los modos de colisión sólo seleccionan en qué parte detectar la colisión
    // La forma de computar colisión de una figura a otra sigue siendo la misma
    // pero cambia donde se hacen los análisis.
    // Hay dos modos:
    // * Inframe. Indica que los análisis de colisión se harán sólo en el "frame"
    //    de cada proceso. Es la más rápida y normalmente no se necesita más.
    // * Independiente. Indica que la colisión se hará en una parte exclusiva para
    //  tal fin. Este modo asegura que las colisiones sean detectadas en forma
    //  controlada. O sea, que en este modo es difícil que se pase por alto
    //  alguna colisión. Con este modo no se salteará ninguna colisión, en cambio
    //  en el modo anterior se corre un pequeño riesgo de perderse una colisión
    //  debido a que un proceso puede cambiar sus valores de x, y o el ángulo justo
    //  antes de que se compute la colisión del proceso siguiente.
    //  El modo Independiente es aconsejable para un juego que se requiera
    //  suma presición al detectar colisiones. Normalmente no es necesario.
    // Nota:  Ninguno de los modos de colisión utiliza algoritmo de predicción.
    //        Así que puede darse el caso de un objeto que atraviesa a otro sin
    //        chocarlo (porque va muy rápido). Eso también pasaba en DIV.


    // Prioridad
    // -dUSE_PRIORITY al compilador, de lo contrario no se usará.
    **** NO IMPLEMENTADA ****
    // En DIYEN la prioridad no está contemplada entonces se obtiene mayor
    // velocidad.
    // Pero se puede activar para que sea más parecido a DIV.
    // De todos modos no es necesario ya que en DIV la prioridad se usaba porque
    // habían procesos que se quedaban atrasados con respecto a otros.
    // Pero en DIYEN todos son procesados en forma secuencial y no se pueden
    // producir atrasos de unos a otros.
    // No obstante pueden haber situaciones donde se requiere especificar la
    // prioridad ya que algunos procesos tendrán sus valores cambiados antes
    // que otros.


    // Scroll
    // -dSCROLL_USE_Z al compilador para que utilice Z.
    // Si no se usa Z:
    //  el primer gráfico, el 0, aparecerá detrás de todos los procesos.
    //  el segundo gráfico, el 1, aparecerá detrás del primero.
    //  el tercer gráfico, el 2, aparecerá al frente de todos los procesos.
    // El algoritmo de utilización de Z de los scrolls en modo computado
    // no es tan eficiente por lo que es más rápido no usar Z en ese modo.

    // -dNO_MOVE_SCROLL al compilador para que diyen no llame automáticamente
    // a la función de movimiento de scroll. Por lo tanto deberás llamarla
    // manualmente desde el proceso cámara.

    // -dNO_REPEAT_SCROLL al compilador para que no se repita la textura
    // del fondo de los scrolls.
    // Actualmente DIYen para repetir la textura utiliza matemáticas dibujando
    // varias veces la misma textura hasta que completa el tamaño pedido (la
    // region).
    // Esta opción desactiva esa característica. Tal vez en algunos casos
    // desactivarla muestre un aumento de velocidad pero no creo que sea
    // sustancial ya que si la textura es lo suficientemente grande no se
    // graficará más veces. El problema sucede si las texturas son más
    // pequeñas que la región (donde deberá graficarse varias veces para poder
    // rellenar el espacio).

    // -dSCROLL_SOFTWARE al compilador para usar superficies de software en
    // los scrolls.
    // Utiliza el modo software para los scrolls permitiendo usar imágenes
    // de tamaños abismales.
    // Por defecto DIYen utiliza texturas de la tarjeta de video (característica
    // propia de SDL2) para todo, y los scrolls no son la excepción.
    // Esta opción activa el uso de superficies de software en los scrolls, pero
    // para que eso sea posible DIYen ahora deberá mantener las superficies
    // en los mapas, en lugar de liberarlas como hacía antes una vez que obtenía
    // la textura.
    // Esta funcionalidad, además de que puede alentizar el graficado de los
    // scrolls, también aumentará el uso de memoria.
    // Pero es necesaria para usar scrolls inmensos (más de 10000 pixeles de
    // ancho), ya que en modo textura no todos los chips gráficos soportan
    // texturas tan grandes, especialmente en dispositivos Android.
    // Conviene para juegos de plataforma donde suelen usarse mapas gigantezcos
    // en los scrolls (Al estilo Super Metroid o Super Mario Bros).

    // -dSCROLL_NO_COL al compilador para desactivar la detección de colisión
    // para el primer mapa de scroll.
    // Por defecto DIYen analiza el primer mapa (graph0) del scroll buscando
    // detectar colisión.
    // Este comportamiento permite definir las partes de un mapa en el scroll
    // que serán "chocables". De lo contrario habría que hacer ese tipo de
    // comprobaciones de otra manera.



    // Textos
    // -dTEXT_USE_Z al compilador para que utilice Z.
    // Si no se usa Z para los textos:
    // Se dibujarán todos los textos encima de todo (inclusive de los scrolls).

    // Scaler
    **** OBSOLETA ****
    // TODO: Revisar si tiene utilidad más allá de usar pantalla completa.
    // -dUSE_SCALER al compilador para activar el escaler matemático.
    // -dUSE_SCALER_SDL al compilador para hacer escalado con el de SDL.

    // Auto process_type
    **** OBSOLETA, YA NO IMPLEMENTADA ****
    // -dAUTO_PTYPE al compilador para activar el modo de process_type automático.
    // Este modo devuelve automáticamente un valor para process_type sin necesidad
    // de declarar nada manualmente. Pero sí se debe declarar para el programa
    // principal.

    // Varios
    // Opciones para manipular características variadas
    //
    // -dNO_CLEAR para que el engine no use SDL_RenderClear.
    // Esto podría solucionar algunos problemas visuales.
    // Lo normal según SDL2 es usarlo, pero si usamos un gráfico de fondo,
    // ya sea por un scroll, o porque está activo el uso de gráfico de fondo,
    // no será necesario limpiar la pantalla.

    // -dNO_BACKGROUND_GRAPH para desactivar el uso de gráfico de fondo.
    // Esto podría darnos un poquito más de velocidad si no lo necesitamos.
    // De lo contrario es bastante práctico tener la posibilidad de poder
    // dibujar en el fondo.

    // -dNO_FX_ENGINE
    // *** INCOMPLETO ***
    // Definiendo NO_FX_ENGINE desactivamos la funcionalidad de "efectos"
    // gráficos.
    // Si no desactivamos el gráfico de fondo
    // podremos graficar en el fondo con la función put().
    // -dFX_SOFTWARE
    // *** AÚN NO PROGRAMADO ***
    // Fixme: De verdad es necesario actualmente?
    // Activa el modo de procesado de FX por software. Muy lento
    // pero reproducible exactamente igual en todos los dispositivos.

    // -dDISABLE_TILEMAPS
    // Al compilador para desactivar el sistema de mapa de mosaicos.
    // Para poder usarlo debemos crear un proceso como hacemos habitualmente
    // pero éste será añadido al tileset en lugar de ser añadido a la lista de
    // procesos.
    // Como se usa:
    // 1) Se crean procesos como siempre pero se agregan con TAdd
    //    (en vez de PAdd).
    // 2) Se le da inicio al tilemap con start_tilemap(n).
    // 3) Se rellena el arreglo tilemap[n].tile[c][r] desde un archivo o
    //    manualmente.
    //
    // Se puede acceder a los tilemap creados con la variable tilemap[n].
    // Donde n es el número de tilemap.
    // Se le puede asignar un valor de Z diferente a cada tilemap creado.
    // Los procesos que son parte de un tilemap (añadidos con TAdd) funcionan
    // igual que cualquier proceso, excepto que:
    // - Las variables X, Y, Z, RESOLUTION, CTYPE, CNUMBER y REGION se
    //   heredaran del tilemap.
    // - X, Y serán las coordenadas de acuerdo al tile que en ese momento esté
    //   siendo procesado.
    // - Su process ID contendrá el valor del indice dentro del tileset sumado
    //   a la constante DIYEN_LMAX_PROCESS+1.
    // - La llamada a frame ignorará el porcentaje.
    //
    // Nota: TAdd crea una lista tileset dentro del tilemap en orden secuencial.
    //       Cada llamada a TAdd incrementa el índice. Así que el primer mosaico
    //       agregado tendrá índice 0, el segundo 1, y así sucesivamente.
    //
    // * Los tiles se deben programar como cualquier otro proceso.
    // * No olvidar rellenar el arreglo tilemap[x].tile[ancho][alto] con los
    //   valores de los tipos de mosaicos que graficarán el tilemap.
    //
    //


*)
unit diyen;
{$mode objfpc}{$H+}

interface

uses
  SDL2, SDL2_mixer, diyen_map, diyen_fx,
  Classes, SysUtils, math;

{$undef KEEP_SURFACE}
{$ifdef SCROLL_SOFTWARE} {$define KEEP_SURFACE} {$endif}
{$ifdef FX_SOFTWARE} {$define KEEP_SURFACE} {$endif}

{$i diyen.inc}

implementation
uses
  diyen_collision, diyen_input, diyen_sound,
  {$ifndef NO_FX_ENGINE}diyen_particles,{$endif}
  {$ifndef DISABLE_TILEMAPS}diyen_tilemaps, {$endif}
  diyen_text, diyen_draw;

(*
-------------------------------------------------------------------------------
DIYEN MAIN FUNCTIONS
*)
{$ifdef USE_DIYEN_CLIPRECT}
procedure diyen_ClipRect( rect: TSDL_Rect );
var
  fsx, fsy : single;
begin
  if (gInternal.scaling) then
  begin
    SDL_RenderGetScale(gInternal.renderer, @fsx, @fsy);
    rect.w := Trunc(rect.w*fsx);
    rect.h := Trunc(rect.h*fsy);
  end;
  SDL_RenderSetClipRect(gInternal.renderer, @rect);
end;
{$endif}

procedure diyen_scroll_putgraph2( sc : PScroll; reg : TSDL_Rect );
var
  dstrect : TSDL_Rect;
  sc_map : TMap;
  ax, ay : integer;
  ayini  : integer;
  {$ifdef SCROLL_SOFTWARE}
  dstsurf : PSDL_Surface;
  dsttex  : PSDL_Texture;
  {$endif}
begin
  if sc^.graph2 > 0 then
  begin
    {$ifdef SCROLL_SOFTWARE}
    dstsurf := SDL_CreateRGBSurface(0, regions.region[0].w, regions.region[0].h, 32, RMASK,GMASK,BMASK,AMASK);
    //dstsurf := SDL_CreateRGBSurface(0, regions.region[0].w, regions.region[0].h, 8, 0, 0, 0, 0);
    SDL_SetSurfaceBlendMode(dstsurf, SDL_BLENDMODE_NONE);
    //SDL_FillRect(dstsurf, nil, $aaaaffff);
    {$endif}

    sc_map := TMap(TFPG(diyen_fpg_list.fpg[sc^.file_]).maps[sc^.graph2]);
    {$ifndef NO_REPEAT_SCROLL}
    ax    :=  reg.x-((sc^.x2) mod sc_map.width+sc_map.width);
    ayini :=  reg.y-((sc^.y2) mod sc_map.height+sc_map.height);

    while (ax-reg.x <= reg.w) do
    begin
      ay := ayini;
      while (ay-reg.y <= reg.h) do
      begin
    {$else}
        ax := (sc.x2) mod (sc_map.width);
        ay := (sc.y2) mod (sc_map.height);
    {$endif}
        dstrect.x := ax;
        dstrect.y := ay;
        dstrect.w := sc_map.width;
        dstrect.h := sc_map.height;

        {$ifndef SCROLL_SOFTWARE}
        SDL_SetTextureAlphaMod(sc_map.texture, sc^.alpha2);
        SDL_SetTextureColorMod(sc_map.texture, sc^.color2.r, sc^.color2.g, sc^.color2.b);
        SDL_SetTextureBlendMode(sc_map.texture, sc^.blend2);
        {$endif}

        {$ifdef USE_SCALER}
        {$ifndef USE_SCALER_SDL}
        if (scaler.enabled) then
          scaler.scaleit(@dstrect, nil);
        {$endif}
        {$endif}

        {$ifndef SCROLL_SOFTWARE}
        // Put the scroll graph
        SDL_RenderCopy( gInternal.renderer,
                        sc_map.texture,
                        nil, @dstrect);
        {$else}
        SDL_UpperBlit(sc_map.surface, nil, dstsurf, @dstrect);
        {$endif}

    {$ifndef NO_REPEAT_SCROLL}
        Inc(ay, sc_map.height);
      end;
      Inc(ax, sc_map.width);
    end;
    {$endif}

    {$ifdef SCROLL_SOFTWARE}
    dstrect.x := 0;
    dstrect.y := 0;
    dstrect.w := dstsurf^.w;
    dstrect.h := dstsurf^.h;

    dsttex := SDL_CreateTextureFromSurface(gInternal.renderer, dstsurf);
    SDL_FreeSurface(dstsurf);

    SDL_SetTextureAlphaMod(dsttex, sc^.alpha2);
    SDL_SetTextureColorMod(dsttex, sc^.color2.r, sc^.color2.g, sc^.color2.b);
    SDL_SetTextureBlendMode(dsttex, sc^.blend2);

    SDL_RenderCopy( gInternal.renderer,
                    dsttex,
                    nil, @dstrect);
    SDL_DestroyTexture(dsttex);
    {$endif}
  end;
end;

procedure diyen_scroll_putgraph10( sc : PScroll; reg : TSDL_Rect );
var
  dstrect : TSDL_Rect;
  sc_map : TMap;
  ax, ay : integer;
  ayini  : integer;
  {$ifdef SCROLL_SOFTWARE}
  dstsurf : PSDL_Surface;
  dsttex  : PSDL_Texture;
  {$endif}
begin
  if sc^.graph1 > 0 then
  begin
    {$ifdef SCROLL_SOFTWARE}
    dstsurf := SDL_CreateRGBSurface(0, regions.region[0].w, regions.region[0].h, 32, RMASK,GMASK,BMASK,AMASK);
    //dstsurf := SDL_CreateRGBSurface(0, regions.region[0].w, regions.region[0].h, 8, 0, 0, 0, 0);
    SDL_SetSurfaceBlendMode(dstsurf, SDL_BLENDMODE_NONE);
    //SDL_FillRect(dstsurf, nil, $0);
    {$endif}

    sc_map := TMap(TFPG(diyen_fpg_list.fpg[sc^.file_]).maps[sc^.graph1]);
    {$ifndef NO_REPEAT_SCROLL}
    ax    :=  reg.x-((sc^.x1) mod sc_map.width+sc_map.width);
    ayini :=  reg.y-((sc^.y1) mod sc_map.height+sc_map.height);

    while (ax-reg.x <= reg.w) do
    begin
      ay := ayini;
      while (ay-reg.y <= reg.h) do
      begin
    {$else}
        ax := (sc.x1) mod (sc_map.width);
        ay := (sc.y1) mod (sc_map.height);
    {$endif}
        dstrect.x := ax;
        dstrect.y := ay;
        dstrect.w := sc_map.width;
        dstrect.h := sc_map.height;

        {$ifndef SCROLL_SOFTWARE}
        SDL_SetTextureAlphaMod(sc_map.texture, sc^.alpha1);
        SDL_SetTextureColorMod(sc_map.texture, sc^.color1.r, sc^.color1.g, sc^.color1.b);
        SDL_SetTextureBlendMode(sc_map.texture, sc^.blend1);
        {$endif}

        {$ifdef USE_SCALER}
        {$ifndef USE_SCALER_SDL}
        if (scaler.enabled) then
          scaler.scaleit(@dstrect, nil);
        {$endif}
        {$endif}

        {$ifndef SCROLL_SOFTWARE}
        // Put the scroll graph
        SDL_RenderCopy( gInternal.renderer,
                        sc_map.texture,
                        nil, @dstrect);
        {$else}
        SDL_UpperBlit(sc_map.surface, nil, dstsurf, @dstrect);
        {$endif}

    {$ifndef NO_REPEAT_SCROLL}
        Inc(ay, sc_map.height);
      end;
      Inc(ax, sc_map.width);
    end;
    {$endif}

    {$ifdef SCROLL_SOFTWARE}
    dstrect.x := 0;
    dstrect.y := 0;
    dstrect.w := dstsurf^.w;
    dstrect.h := dstsurf^.h;

    dsttex := SDL_CreateTextureFromSurface(gInternal.renderer, dstsurf);
    SDL_FreeSurface(dstsurf);

    SDL_SetTextureAlphaMod(dsttex, sc^.alpha1);
    SDL_SetTextureColorMod(dsttex, sc^.color1.r, sc^.color1.g, sc^.color1.b);
    SDL_SetTextureBlendMode(dsttex, sc^.blend1);

    SDL_RenderCopy( gInternal.renderer,
                    dsttex,
                    nil, @dstrect);
    SDL_DestroyTexture(dsttex);
    {$endif}
  end;

  if sc^.graph0 > 0 then
  begin
    {$ifdef SCROLL_SOFTWARE}
    dstsurf := SDL_CreateRGBSurface(0, regions.region[0].w, regions.region[0].h, 32, RMASK,GMASK,BMASK,AMASK);
    //dstsurf := SDL_CreateRGBSurface(0, regions.region[0].w, regions.region[0].h, 8, 0, 0, 0, 0);
    SDL_SetSurfaceBlendMode(dstsurf, SDL_BLENDMODE_NONE);
    //SDL_FillRect(dstsurf, nil, $aaaaffff);
    {$endif}

    sc_map := TMap(TFPG(diyen_fpg_list.fpg[sc^.file_]).maps[sc^.graph0]);
    {$ifndef NO_REPEAT_SCROLL}
    ax    :=  reg.x-((sc^.x0) mod sc_map.width+sc_map.width);
    ayini :=  reg.y-((sc^.y0) mod sc_map.height+sc_map.height);

    while (ax-reg.x <= reg.w) do
    begin
      ay := ayini;
      while (ay-reg.y <= reg.h) do
      begin
    {$else}
        ax := (sc.x0) mod (sc_map.width);
        ay := (sc.y0) mod (sc_map.height);
    {$endif}
        dstrect.x := ax;
        dstrect.y := ay;
        dstrect.w := sc_map.width;
        dstrect.h := sc_map.height;

        {$ifndef SCROLL_SOFTWARE}
        SDL_SetTextureAlphaMod(sc_map.texture, sc^.alpha0);
        SDL_SetTextureColorMod(sc_map.texture, sc^.color0.r, sc^.color0.g, sc^.color0.b);
        SDL_SetTextureBlendMode(sc_map.texture, sc^.blend0);
        {$endif}

        {$ifdef USE_SCALER}
        {$ifndef USE_SCALER_SDL}
        if (scaler.enabled) then
          scaler.scaleit(@dstrect, nil);
        {$endif}
        {$endif}

        {$ifndef SCROLL_SOFTWARE}
        // Put the scroll graph
        SDL_RenderCopy( gInternal.renderer,
                        sc_map.texture,
                        nil, @dstrect);
        {$else}
        SDL_UpperBlit(sc_map.surface, nil, dstsurf, @dstrect);
        {$endif}

    {$ifndef NO_REPEAT_SCROLL}
        Inc(ay, sc_map.height);
      end;
      Inc(ax, sc_map.width);
    end;
    {$endif}

    {$ifdef SCROLL_SOFTWARE}
    dstrect.x := 0;
    dstrect.y := 0;
    dstrect.w := dstsurf^.w;
    dstrect.h := dstsurf^.h;

    dsttex := SDL_CreateTextureFromSurface(gInternal.renderer, dstsurf);
    SDL_FreeSurface(dstsurf);

    SDL_SetTextureAlphaMod(dsttex, sc^.alpha0);
    SDL_SetTextureColorMod(dsttex, sc^.color0.r, sc^.color0.g, sc^.color0.b);
    SDL_SetTextureBlendMode(dsttex, sc^.blend0);

    SDL_RenderCopy( gInternal.renderer,
                    dsttex,
                    nil, @dstrect);
    SDL_DestroyTexture(dsttex);
    {$endif}
  end;
end;

procedure diyen_scroll_putgraph( sc : PScroll );
var
  reg    : TSDL_Rect;
begin
    reg := regions[sc^.sregion]^;

    diyen_scroll_putgraph2( sc, reg );
    diyen_scroll_putgraph10( sc, reg );

    sc^.drawed := true;
end;

// Pone los gráficos de todas las primitivas (draw)
procedure diyen_drawings_putgraph();
var
  i : smallint;
begin
  //if diyen_drawings_list.vmax > 0 then
    //SDL_RenderSetClipRect(gInternal.renderer, @regions.clip_region[0]);
  with diyen_drawings_list do
    for i := vmax downto 1 do
    begin
      if (drawing[i] <> nil) then
        TDraw(drawing[i]).Draw;
    end;
end;

// Pone los gráficos de todos los textos en pantalla
procedure diyen_texts_putgraph();
var
  i : smallint;
begin
  {$ifdef TEXT_USE_Z}
  if diyen_texts_list.vmax > 0 then
    SDL_RenderSetClipRect(gInternal.renderer, @regions.clip_region[0]);
  {$endif}
  with diyen_texts_list do
    for i := vmax downto 1 do
    begin
      if (texto[i] <> nil) then
        TText(texto[i]).RenderText;
    end;

  diyen_texts_list.drawed := true;
end;


// Pone los gráficos de los elementos touch en pantalla
procedure diyen_touchs_putgraph();
var
  i : smallint;
begin
  with diyen_touchs_list do
    for i := vmax downto 1 do
    begin
      if (touch[i] <> nil) then
        TTouch(touch[i]).RenderTouch;
    end;
end;


// Put the graph of the process
procedure diyen_process_putgraph( process: TProcess );
var
  dstrect : TSDL_Rect;
  process_map : TMap;
  center : TSDL_Point;

  {$ifdef DIYEN_DEBUG}
  x, y : integer;
  {$endif}

  n   : integer;
  sn  : byte;
  sc  : PScroll;
  reg : TSDL_Rect;

  {$ifndef NO_FX_ENGINE}
  FXparams : TFXparams;
  {$endif}

  //realx, realy : single;
  //difx, dify : integer;

begin
  //if (gInternal.renderer = nil) then Exit;
  if (process.Internal.frame_percent >= 100) then
    Dec(process.Internal.frame_percent,100);

  if process.graph > 0 then
  if process.alpha > 0 then
  begin
    process_map := TMap(TFPG(diyen_fpg_list.fpg[process.file_]).maps[process.graph]);


    if (process.ctype <> C_SCROLL) then
    begin
      // En qué región está
      if (process.region > 0) then
      {$ifdef USE_DIYEN_CLIPRECT}
        diyen_ClipRect(regions[process.region]^)
      {$else}
        SDL_RenderSetClipRect(gInternal.renderer, @regions.clip_region[process.region])
      {$endif}
      else
      {$ifdef USE_DIYEN_CLIPRECT}
        diyen_ClipRect(regions[0]^);
      {$else}
        SDL_RenderSetClipRect(gInternal.renderer, @regions.clip_region[0]);
      {$endif}

      {$ifndef NO_FX_ENGINE}
      if process.Internal.fx_max <= 0 then
      begin
      {$endif}
      center.x := longint((process_map.cpoint[0].x * process.size_x) div 100);
      center.y := longint((process_map.cpoint[0].y * process.size_y) div 100);
      //realx := (process.x / process.resolution);
      //realy := (process.y / process.resolution);
      dstrect.x := (process.x div process.resolution) - center.x;
      dstrect.y := (process.y div process.resolution) - center.y;
      //difx := Round(Frac(realx));
      //dify := Round(Frac(realy));

      // Ahorramos velocidad al aplicar tamaño
      if process.size_x <> 100 then
        dstrect.w := longint((process_map.width * process.size_x) div 100)
      else
        dstrect.w := longint(process_map.width);

      if process.size_y <> 100 then
        dstrect.h := longint((process_map.height * process.size_y) div 100)
      else
        dstrect.h := longint(process_map.height);

      {$ifdef USE_SCALER}
      {$ifndef USE_SCALER_SDL}
      if (scaler.enabled) then
        scaler.scaleit(@dstrect, @center);
      {$endif}
      {$endif}

      SDL_SetTextureAlphaMod(process_map.texture, byte(process.alpha));
      SDL_SetTextureColorMod(process_map.texture, process.color.r, process.color.g, process.color.b);
      SDL_SetTextureBlendMode(process_map.texture, process.blend);

      // Put the graphic
      if ((process.angle mod 360000) = 0) and (process.flip = SDL_FLIP_NONE) then
        SDL_RenderCopy( gInternal.renderer,
                        process_map.texture,
                        nil, @dstrect)
      else
        begin
          SDL_RenderCopyEx( gInternal.renderer,
                            process_map.texture,
                            nil, @dstrect,
                            process.angle/1000,
                            @center,
                            process.flip);
        end;
      {$ifndef NO_FX_ENGINE}
      end else
      begin
        with process.Internal do
        begin
          FXparams.x      := process.x;
          FXparams.y      := process.y;
          FXparams.size_x := process.size_x;
          FXparams.size_y := process.size_y;
          FXparams.angle  := process.angle;
          FXparams.file_  := process.file_;
          FXparams.graph  := process.graph;
          FXparams.alpha  := process.alpha;
          FXparams.color  := process.color;
          FXparams.blend  := process.blend;
          FXparams.flip   := process.flip;
          {$ifndef FX_NO_RECT_SRC}FXparams.Rsrc.w := 0;{$endif}
          fx_run(process, @FXparams, 0, nil);
        end;
      end;
      {$endif}

      // DEBUG
      {$ifdef DIYEN_DEBUG}
      SDL_SetRenderDrawColor(gInternal.renderer, 100, 100, 0, 100);
      x := process.x div process.resolution;
      y := process.y div process.resolution;

      SDL_RenderDrawPoint(gInternal.renderer, x       , y);
      SDL_RenderDrawPoint(gInternal.renderer, x+6     , y);
      SDL_RenderDrawPoint(gInternal.renderer, x-6     , y);
      SDL_RenderDrawPoint(gInternal.renderer, x       , y+6);
      SDL_RenderDrawPoint(gInternal.renderer, x       , y-6);

      //SDL_RenderDrawRect(gInternal.renderer, @dstrect);

      SDL_SetRenderDrawColor(gInternal.renderer, 0, 0, 0, 255);
      {$endif}
    end else
    // C_SCROLL
    begin
      for n := 0 to diyen_scroll_list.alist_max do
      begin
        sn := diyen_scroll_list.alist[n];
        if (process.cnumber <> []) and not (Tcnumber(sn) in process.cnumber) then Continue;
        sc := @scroll[sn];
        reg := regions[sc^.sregion]^;

        {$ifdef USE_DIYEN_CLIPRECT}
        diyen_ClipRect(regions[sc^.sregion]^);
        {$else}
        SDL_RenderSetClipRect(gInternal.renderer, @regions.clip_region[sc^.sregion]);
        {$endif}
        // Si se usa Z para el scroll
        {$ifdef SCROLL_USE_Z}
          if (not sc^.drawed) and (sc^.z <= process.z) then
          begin
            diyen_scroll_putgraph(sc);
          end;
        {$endif}

        {$ifndef NO_FX_ENGINE}
        if process.Internal.fx_max <= 0 then
        begin
        {$endif}
        //realx := (process.x / process.resolution);
        //realy := (process.y / process.resolution);
        //difx := Round(Frac(realx));
        //dify := Round(Frac(realy));
        center.x := longint((process_map.cpoint[0].x * process.size_x) div 100);
        center.y := longint((process_map.cpoint[0].y * process.size_y) div 100);
        dstrect.x := (process.x div process.resolution) + (reg.x-sc^.x0-center.x);
        dstrect.y := (process.y div process.resolution) + (reg.y-sc^.y0-center.y);

        (*dstrect.w := longint((process_map.width * process.size_x) div 100);
        dstrect.h := longint((process_map.height * process.size_y) div 100);*)
        // Ahorramos un poco de procesamiento
        if process.size_x <> 100 then
          dstrect.w := longint((process_map.width * process.size_x) div 100)
        else
          dstrect.w := longint(process_map.width);

        if process.size_y <> 100 then
          dstrect.h := longint((process_map.height * process.size_y) div 100)
        else
          dstrect.h := longint(process_map.height);


        {$ifdef USE_SCALER}
        {$ifndef USE_SCALER_SDL}
        if (scaler.enabled) then
          scaler.scaleit(@dstrect, @center);
        {$endif}
        {$endif}

        SDL_SetTextureAlphaMod(process_map.texture, byte(process.alpha));
        SDL_SetTextureColorMod(process_map.texture, process.color.r, process.color.g, process.color.b);
        SDL_SetTextureBlendMode(process_map.texture, process.blend);

        // Put the graphic
        if ((process.angle mod 360000) = 0) and (process.flip = SDL_FLIP_NONE) then
          SDL_RenderCopy( gInternal.renderer,
                          process_map.texture,
                          nil, @dstrect)
        else
          begin
            SDL_RenderCopyEx( gInternal.renderer,
                              process_map.texture,
                              nil, @dstrect,
                              process.angle/1000,
                              @center,
                              process.flip);
          end;
        {$ifndef NO_FX_ENGINE}
        end else
        begin
          with process.Internal do
          begin
            FXparams.x      := process.x;
            FXparams.y      := process.y;
            FXparams.size_x := process.size_x;
            FXparams.size_y := process.size_y;
            FXparams.angle  := process.angle;
            FXparams.file_  := process.file_;
            FXparams.graph  := process.graph;
            FXparams.alpha  := process.alpha;
            FXparams.color  := process.color;
            FXparams.blend  := process.blend;
            FXparams.flip   := process.flip;
            {$ifndef FX_NO_RECT_SRC}FXparams.Rsrc.w := 0;{$endif}
            fx_run(process, @FXparams, 0, sc);
          end;
        end;
        {$endif}
      end;
    end;
  end;
end;

{$ifndef DISABLE_TILEMAPS}
procedure diyen_tilemap_putgraph( tm : PTileMap );
var
  tile : TTile;
  tc, tr, tmax : word;
  tsetn : byte;
begin
  for tc := tm^.from_column to tm^.to_column do
  for tr := tm^.from_row to tm^.to_row do
  begin
    tsetn := tm^.tile[tc][tr];
    tile := tm^.tileset[tsetn];
    tile.tc := tc;
    tile.tr := tr;
    tile.resolution := tm^.resolution;
    tile.z := tm^.z;
    tile.ctype := tm^.ctype;
    tile.cnumber := tm^.cnumber;
    tile.region := tm^.region;
    tile.x := tm^.x + tm^.tile_width * tm^.resolution * tc;
    tile.y := tm^.y + tm^.tile_height * tm^.resolution * tr;

    if tm^.populatevars then
    begin
    tile.alpha := tm^.alpha;
    tile.color := tm^.color;
    tile.blend := tm^.blend;
    tile.flip  := tm^.flip;

    tile.angle   := tm^.angle;
    tile.size_x  := tm^.size_x;
    tile.size_y  := tm^.size_y;
    end;

    if tile.running and (tile.Internal.frame_percent < 100) then
    begin
    tile.loop;
    // No debe correr más de una vez
    // FIXME: Que hay de poder dibujar en cada tile por separado?
    // FIXME2!: Que hay de la detección de colisión con frame();??
    // La idea de poder dibujar por separado consiste en que
    // cada tile tiene una posición de X, Y distinta. Lo que
    // nos permitiría ejecutar dicho tile (que no ejecutaría solo uno, sino
    // el conjunto). Se pensaba que cambiando en general las variables
    // heredadas se podrían obtener distintos efectos.
    // Pero lo cierto es que aunque cambiemos esas variables no
    // serán tomadas en cuenta puesto que se volverían a su valor justo
    // antes de dibujar el mosaico. ^ mirar el código más arriba

    // Las únicas que nos podrían servir serían X, e Y. Para saber la ubicación
    // del tile en cuestión. Pero sólo veríamos la ubicación de ese único tile.
    // Una cosa que no podríamos hacer por ejemplo es crear un proceso
    // sobre todos los tiles de ese tipo. Sólo se crearía en uno.
    // Pero podemos usar una variedad de imágenes para simularlo.
    // Lo que no se puede tener es un tile con "vida" o más bien dinámico más
    // allá de su gráfico.

    // SOLUCIONADO:
    // Si queremos usar una variable para animar con graph, entonces podemos usar
    // una variable "executed" para saber si el mosaico ya fue ejecutado y no
    // incrementar el valor de una variable cada vez que ejecute el mismo tipo de
    // mosaico.
    // No olvidar colocar frame si queremos detectar colisión.
    tile.executed := True;
    end;
    tile.angle := tile.angle mod 360000; // Mantenemos el ángulo dentro de valores de ángulo

    diyen_process_putgraph(tile);
  end;
  tm^.drawed:=true;

  for tc := tm^.from_column to tm^.to_column do
  for tr := tm^.from_row to tm^.to_row do
  begin
    tsetn := tm^.tile[tc][tr];
    tile := tm^.tileset[tsetn];
    tile.executed := False;
  end;
  (*tmax := tm^.tiles-1;
  for tc := 0 to tmax do
  begin
    tile := tm^.tileset[tc];
    tile.executed := False;
  end;*)
end;
{$endif}


// Mover el scroll desde los procesos es un error
// ya que a veces moverá el scroll antes que dibuje
// el resto de los procesos y a veces lo moverá
// después produciendo un efecto de "retardo" en
// el "movimiento" de algunos procesos.
(*procedure diyen_move_scroll( pid : word );
var
  sc : TScroll;
  n : integer;
  sn : byte;
begin
  for n := 0 to diyen_scroll_list.alist_max do
  begin
    sn := diyen_scroll_list.alist[n];
    sc := scroll[sn];
    if (sc.camera = pid) then
      diyen_scroll_list.move_scroll(sn);
  end;
end;*)

(*function diyen_process_signals( process: TProcess ): Boolean;
var
  z : integer;
begin
  Result := false;

  case (process.Internal.signal) of
    S_KILL:
    begin
      diyen_process_list.Del(process.Internal.process_id);
      process.Internal.status := STATUS_DEAD;
    end;
    S_SLEEP:
    begin;
      process.Internal.status := STATUS_SLEEPING;
    end;
    S_FREEZE:
    begin;
      process.Internal.status := STATUS_FROZEN;
    end;
    S_WAKEUP:
    begin;
      (*
      if (process.Internal.status = STATUS_UNBORN) then
      begin
      process.start(process.Internal.params);
        // Como vamos a meter un nuevo proceso
        // verifica si hay que reordenar
        {$ifndef ZLAYER}
        //gInternal.aprocess_started := true;
        with diyen_process_list do
        if (zlmax > 0) and (zlist[zlmax-1] <> 0) then
          z := diyen_process_list[zlist[zlmax-1]].z
        else
          z := 32767;

        if (process.z >= z) then
          gInternal.Z_comp_ordered := false;

        process.Internal.zold := process.z;
        {$endif}
      end;
      // Lo agregamos a la lista
      Inc(zlmax);
      zlist[zlmax] := process.Internal.process_id;*)

      process.Internal.status := STATUS_RUNNING;
      Result := true;
    end;
    otherwise
    begin
      {
      if (process.z <> process.Internal.zold) then
      begin
        process.Internal.zold := process.z;
        gInternal.Z_comp_ordered := false;
        Result := true;
      end;
      }
    end;
  end;
    process.Internal.signal := S_NONE;
end;
*)





// Called for terminating the program
procedure diyen_main_loop_end();
begin
  {$ifdef USE_SCALER}
  scaler.Free;
  {$endif}

  diyen_process_list.Free;
  regions.Free;
  diyen_fpg_list.Free;

  diyen_scroll_list.Free;

  diyen_music_list.Free;
  diyen_sounds_list.Free;

  diyen_texts_list.Free;
  diyen_fonts_list.Free;

  diyen_drawings_list.Free;

  {$ifndef DISABLE_TILEMAPS}
  diyen_tilemap_list.Free;
  {$endif}

  diyen_input_finish();

  SDL_SetWindowFullscreen(gInternal.window, 0);
  //Mix_Quit;
  SDL_Quit;

  {$ifdef ANDROID}
  Halt(0); // Necesario para que Android cierre completamente todo
  {$endif}
end;

// Main loop called in the frame function from the PROGRAM
procedure diyen_main_loop();
var
  this_tick: longint = 0;

  evento         : TSDL_Event;  // Eventos varios, teclado, joystick, etc

  scent : integer;

  sc : PScroll;
  n : integer;
  sn : byte;

  {$ifndef DISABLE_TILEMAPS}
  tn  : integer;
  tmn : byte;
  tm  : PTileMap;
  {$endif}

  //zlmax : word;
  process: TProcess;
  i : integer;

  {$if not defined(ZLAYER) or defined(ZLAYER_RUNTIME)}
  o : integer;
  zant : int;
  process_ant : TProcess;

  zsig : int;
  process_sig : TProcess;
  movido : boolean;
  {$endif}

  {$ifdef ZLAYER}
  Z : int;
  {$endif}
begin
  // Aqui procesamos todo, absolutamente todo.
  // Podríamos decir que es la abstracción del lazo principal
  // del juego. Bah, demasiadas palabras jejeje.


  //{$ifndef DISABLE_TOUCH}input_touch_unpush();{$endif}
  {$ifndef DISABLE_MOUSE}mouse.state := mouse.state and not MOUSE_STATE_MOTION;{$endif}
  // Eventos
  while ( (exit_status = 0) and (SDL_PollEvent( @evento ) <> 0) ) do
  begin
    case evento.type_ of
    SDL_QUITEV : exit_status := 1;
    {$ifdef ANDROID}
    SDL_APP_WILLENTERBACKGROUND: background := true;
    SDL_APP_WILLENTERFOREGROUND: background := false;
    {$endif}

    {$ifndef DISABLE_MOUSE}
    SDL_MOUSEBUTTONDOWN:
    with evento.button do
    if which <> SDL_TOUCH_MOUSEID then
    begin
      //mouse[0].X := Trunc(x / gInternal.scale.x);
      //mouse[0].Y := Trunc(y / gInternal.scale.y);
      mouse.touchid := input_touch_over(x, y);
      if mouse.touchid < 0 then
      begin
        mouse.X := x;
        mouse.Y := y;
        mouse.state := mouse.state or (1<<(button-1));
      end else
      with diyen_touchs_list[mouse.touchid] do
      if diyen_touchs_list[mouse.touchid] <> nil then
      begin
        pushed := true;
        input_finger_touch(x, y, mouse.touchid);
      end;
    end;

    SDL_MOUSEBUTTONUP:
    with evento.button do
    if which <> SDL_TOUCH_MOUSEID then
    begin
      //mouse[0].X := Trunc(x / gInternal.scale.x);
      //mouse[0].Y := Trunc(y / gInternal.scale.y);
      if mouse.touchid < 0 then
      begin
        mouse.X := x;
        mouse.Y := y;
        mouse.state := mouse.state and not (1<<(button-1));
      end else
      with diyen_touchs_list[mouse.touchid] do
      if diyen_touchs_list[mouse.touchid] <> nil then
      begin
        pushed := false;
        overed := false;
        input_finger_touch(x, y, mouse.touchid);
        mouse.touchid:=-1;
      end;
    end;

    SDL_MOUSEMOTION:
    with evento.motion do
    if which <> SDL_TOUCH_MOUSEID then
    begin
      //mouse[0].X := Trunc(x / gInternal.scale.x);
      //mouse[0].Y := Trunc(y / gInternal.scale.y);

      if mouse.touchid >= 0 then
      // Si se salió del sitio
        if input_touch_over(x, y, mouse.touchid) < 0 then
        with diyen_touchs_list[mouse.touchid] do
          if diyen_touchs_list[mouse.touchid] <> nil then
          begin
            overed := false;
            pushed := false;
            diyen_touchs_list.touch_map[device][button] := 0;
            if ttype = TOUCH_ANALOGXY then
            begin
              dx := 0;
              dy := 0;
              diyen_touchs_list.touch_map[device][button]   := 0;
              diyen_touchs_list.touch_map[device][button+1] := 0;
            end;
          end;

      mouse.touchid := input_touch_over(x, y);

      if mouse.touchid < 0 then
      begin
        mouse.state := state or MOUSE_STATE_MOTION;

        mouse.X := x;
        mouse.Y := y;

        //mouse[0].Xrel:=Trunc(xrel / gInternal.scale.x);
        //mouse[0].Yrel:=Trunc(yrel / gInternal.scale.y);
        mouse.Xrel:=xrel;
        mouse.Yrel:=yrel;
      end else
      with diyen_touchs_list[mouse.touchid] do
      if diyen_touchs_list[mouse.touchid] <> nil then
      begin
        overed := true;
        if ttype = TOUCH_ANALOGXY then
          input_finger_touch(x, y, mouse.touchid);
      end;
    end;
    {$endif}

    {$ifndef DISABLE_TOUCH}
    SDL_FINGERDOWN:
    with evento.tfinger do
    begin
      if fingerId >= FingersTotal then
      begin
        Inc(FingersTotal);
        SetLength(finger, FingersTotal);
      end;
      finger[fingerId].X := Trunc(x*gInternal.lscreen.w);
      finger[fingerId].Y := Trunc(y*gInternal.lscreen.h);
      finger[fingerId].state := 1;

      finger[fingerId].touchid := input_touch_over(finger[fingerId].X, finger[fingerId].Y);
      if finger[fingerId].touchid < 0 then
      begin
        mouse.X := finger[fingerId].X;
        mouse.Y := finger[fingerId].Y;
        mouse.state := mouse.state or 1; // Mouse Left
      end else
      with diyen_touchs_list[finger[fingerId].touchid] do
      if diyen_touchs_list[finger[fingerId].touchid] <> nil then
      begin
        pushed := true;
        input_finger_touch(finger[fingerId].X, finger[fingerId].Y, finger[fingerId].touchid);
      end;
    end;

    SDL_FINGERUP:
    with evento.tfinger do
    begin
      if fingerId >= FingersTotal then
      begin
        Inc(FingersTotal);
        SetLength(finger, FingersTotal);
      end;

      finger[fingerId].X := Trunc(x*gInternal.lscreen.w);
      finger[fingerId].Y := Trunc(y*gInternal.lscreen.h);
      finger[fingerId].state := 0;

      if finger[fingerId].touchid < 0 then
      begin
        mouse.X := finger[fingerId].X;
        mouse.Y := finger[fingerId].Y;
        mouse.state := mouse.state and not 1;
      end else
      with diyen_touchs_list[finger[fingerId].touchid] do
      if diyen_touchs_list[finger[fingerId].touchid] <> nil then
      begin
        pushed := false;
        overed := false;
        input_finger_touch(finger[fingerId].X, finger[fingerId].Y, finger[fingerId].touchid);
        finger[fingerId].touchid:=-1;
      end;
    end;

    SDL_FINGERMOTION:
    with evento.tfinger do
    begin
      if fingerId >= FingersTotal then
      begin
        Inc(FingersTotal);
        SetLength(finger, FingersTotal);
      end;

      finger[fingerId].X := Trunc(x*gInternal.lscreen.w);
      finger[fingerId].Y := Trunc(y*gInternal.lscreen.h);

      if finger[fingerId].touchid >= 0 then
      // Si se salió del sitio
        if input_touch_over(finger[fingerId].X, finger[fingerId].Y, finger[fingerId].touchid) < 0 then
          with diyen_touchs_list[finger[fingerId].touchid] do
            if diyen_touchs_list[finger[fingerId].touchid] <> nil then
            begin
              overed := false;
              pushed := false;
              diyen_touchs_list.touch_map[device][button] := 0;
              if ttype = TOUCH_ANALOGXY then
              begin
                dx := 0;
                dy := 0;
                diyen_touchs_list.touch_map[device][button]   := 0;
                diyen_touchs_list.touch_map[device][button+1] := 0;
              end;
            end;

      finger[fingerId].touchid := input_touch_over(finger[fingerId].X, finger[fingerId].Y);

      if finger[fingerId].touchid < 0 then
      begin
        mouse.X := finger[fingerId].X;
        mouse.Y := finger[fingerId].Y;
        mouse.state := mouse.state or 1 or MOUSE_STATE_MOTION;
      end else
      with diyen_touchs_list[finger[fingerId].touchid] do
      if diyen_touchs_list[finger[fingerId].touchid] <> nil then
      begin
        overed := true;
        if ttype = TOUCH_ANALOGXY then
          input_finger_touch(finger[fingerId].X, finger[fingerId].Y, finger[fingerId].touchid);
      end;
    end;
    {$endif}

    end;
  end;
  input_KeysState := SDL_GetKeyboardState(input_KeysNum);

  // En Android el evento OnPause y OnResume deben
  // pausar y reanudar la aplicación.
  {$ifdef ANDROID}
  if background then
  begin
    Exit();
  end;
  {$endif}

  (* ======= A procesar los procesos ======= *)

  {$ifdef DIYEN_PAUSE}
  if gInternal.processing then
  begin
  {$endif}

  // =====================
  // Ejecución de procesos
  // =====================
  with diyen_process_list do
  begin
    i := 0;
    while i < startl_max do
    begin
      Inc(i);
      process := diyen_process_list[startl[i]];
      process.start(process.Internal.params);
      process.Internal.status := STATUS_STARTED;
      //process.Internal.status := STATUS_RUNNING;
      {$if defined(ZLAYER) and defined(ZLAYER_RUNTIME)}
      if not use_zlayer then
      begin
      {$endif}
      {$if not defined(ZLAYER) or defined(ZLAYER_RUNTIME)}
      // Agregamos a la lista Z (si estamos en modo COMP)
      Inc(zlmax);
      zlist[zlmax] := process.Internal.process_id;
      {$endif}
      {$if defined(ZLAYER) and defined(ZLAYER_RUNTIME)}
      end else
      begin
      {$endif}
      {$ifdef ZLAYER}
      process.Internal.Zl.Zold:=process.z;
      if gInternal.zlayer_auto then
      if zlayer_max < process.z then
        zlayer_automax(process.z);
      with zlayer[process.z] do
      begin
        Inc(max);
        p[max] := process.Internal.process_id;
        process.Internal.Zl.nZ := max;
      end;
      {$endif}
      {$if defined(ZLAYER) and defined(ZLAYER_RUNTIME)}
      end;
      {$endif}
    end;
  startl_max:=0;

  i := 0;
  while i < killl_max do
  begin
    Inc(i);
    process := diyen_process_list[killl[i]];
    // Finalmente lo borramos
    Del(process.Internal.process_id);
  end;
  killl_max:=0;

  i := 0;
  while i < endedl_max do
  begin
    Inc(i);
    process := diyen_process_list[endedl[i]];
    ProcessEnd(process);
    // Lo agregamos para borrar
    Inc(killl_max);
    killl[killl_max] := process.Internal.process_id;
  end;
  endedl_max:=0;
  end;

  {$if defined(ZLAYER) and defined(ZLAYER_RUNTIME)}
  if use_zlayer then
  begin
  {$endif}
  {$ifdef ZLAYER}
  // MODO Z en capas
  // Mantiene varias listas de procesos para cada valor de Z
  with diyen_process_list do
  begin
    i := 0;
    while i < vmax do
    begin
      Inc(i);

      process := diyen_process_list[i];

      // Quiere decir que no existe
      if (process = nil) then
        Continue;

      if process.Internal.status = STATUS_UNSTARTED then
        Continue;

      Z := process.z;

      if gInternal.zlayer_auto then
      if zlayer_max < process.z then
        zlayer_automax(process.z);

      if process.Internal.status = STATUS_KILL then
      begin
        // Lo agregamos a la lista para terminar
        Inc(endedl_max);
        endedl[endedl_max] := process.Internal.process_id;
        // Lo quitamos de la capa Z
        // En realidad eliminamos el último siempre, para eso lo
        // intercambiamos con éste y luego descontamos uno a máximo.
        with process.Internal.Zl do
        begin
          zlayer[Z].p[nZ] := zlayer[Z].p[zlayer[Z].max];
          // Actualizamos el nZ del proceso afectado en la capa anterior
          if zlayer[Z].max > 0 then
          begin
            diyen_process_list[zlayer[Z].p[nZ]].Internal.Zl.nZ := nZ;
            Dec(zlayer[Z].max);
          end;
        end;
        Continue;
      end;

      // Cambio de Z
      if Z <> process.Internal.Zl.Zold then
      begin
        // Cambiamos de Z entonces cambiamos de capa
        with process.Internal.Zl do
        begin
          // Lo eliminamos de la capa anterior
          zlayer[Zold].p[nZ] := zlayer[Zold].p[zlayer[Zold].max];
          // Actualizamos el nZ del proceso afectado en la capa anterior
          if zlayer[Zold].max > 0 then
          begin
            diyen_process_list[zlayer[Zold].p[nZ]].Internal.Zl.nZ := nZ;;
            Dec(zlayer[Zold].max);
          end;

          // Ahora lo agregamos a la nueva capa
          Inc(zlayer[Z].max);
          nZ := zlayer[Z].max;
          zlayer[Z].p[nZ] := i;
          Zold:=Z;
        end;
        Continue;
      end;

      // Durmiendo no procesa
      if (process.sleeping) then Continue;

      // Ejecuta
      if process.running and (process.Internal.frame_percent < 100) then
        process.loop;
    end;
  end;
  {$endif}
  {$if defined(ZLAYER) and defined(ZLAYER_RUNTIME)}
  end else
  begin
  {$endif}
  {$if not defined(ZLAYER) or defined(ZLAYER_RUNTIME)}
  // MODO Z Computado
  // Mantiene una lista de procesos ordenadas por su Z de mayor a menor.
  // Donde el mayor se dibuja antes que el menor para que el menor aparezca
  // sobre el mayor y de la sensación de estar más cerca.
  i := 0;
  diyen_process_list.zlmax_fixed := diyen_process_list.zlmax;
  with diyen_process_list do
    while i < zlmax_fixed do
    begin
      Inc(i);
      process := diyen_process_list[zlist[i]];

      // Quiere decir que está borrado
      // FIXME: Esto no debería pasar
      if (process = nil) then
      begin
        zlist[i] := 0;
        Continue;
      end;

      if process.Internal.status = STATUS_KILL then
      begin
        // Lo agregamos a la lista para terminar
        Inc(endedl_max);
        endedl[endedl_max] := process.Internal.process_id;
        zlist[i] := 0; // Acá ya no participa más
        Continue;
      end;


      // Durmiendo no procesa
      if (process.sleeping) then Continue;

      // Movemos hacia adelante
      o := i;
      if (o<zlmax_fixed) then
      begin
        movido := false;
        process_sig := diyen_process_list[diyen_process_list.zlist[o+1]];
        if process_sig <> nil then
          zsig := process_sig.z
        else
          zsig := -32768;

        while (process.z < zsig) and (o<zlmax_fixed) do
        begin
          diyen_process_list.zlist[o] := diyen_process_list.zlist[o+1];
          Inc(o);
          diyen_process_list.zlist[o] := process.Internal.process_id;
          movido := true;

          process_sig := diyen_process_list[diyen_process_list.zlist[o+1]];
          if process_sig <> nil then
            zsig := process_sig.z
          else
            zsig := -32768;
        end;

        if movido then
        begin
          Dec(i);
          Continue;
        end;
      end;

      // Ejecuta
      if process.running and (process.Internal.frame_percent < 100) then
        process.loop
      else
      if process.Internal.status = STATUS_STARTED then
        process.Internal.status := STATUS_RUNNING;

      // Lo movemos hacia atrás
      o := i;
      if (o>1) then
      begin
        process_ant := diyen_process_list[diyen_process_list.zlist[o-1]];
        if process_ant <> nil then
          zant := process_ant.z
        else
          zant := -32768;

        while (process.z > zant) and (o>1) do
        begin
          diyen_process_list.zlist[o] := diyen_process_list.zlist[o-1];
          Dec(o);
          diyen_process_list.zlist[o] := process.Internal.process_id;

          process_ant := diyen_process_list[diyen_process_list.zlist[o-1]];
          if process_ant <> nil then
            zant := process_ant.z
          else
            zant := -32768;
        end;
      end;
    end;
  {$endif}
  {$if defined(ZLAYER) and defined(ZLAYER_RUNTIME)}
  end;
  {$endif}
  {$ifdef DIYEN_PAUSE}
  end;
  {$endif}

  {$if defined(ZLAYER) and defined(ZLAYER_RUNTIME)}
  if not use_zlayer then
  begin
  {$endif}
  {$if not defined(ZLAYER) or defined(ZLAYER_RUNTIME)}
  // Limpiamos el final
  with diyen_process_list do
    while (zlmax > 0) and (zlist[zlmax] = 0) do
      Dec(zlmax);
  {$endif}
  {$if defined(ZLAYER) and defined(ZLAYER_RUNTIME)}
  end;
  {$endif}


  // =================
  // Control de tiempo
  // =================
  this_tick := SDL_GetTicks;
  if (gInternal.expected_fps > 0) then
  begin
    if (gInternal.program_next_tick >= this_tick) then
    begin
      SDL_Delay(gInternal.program_next_tick - this_tick);
      gInternal.frames_skip := 0;
    end else
    if gInternal.frames_skip < gInternal.expected_skip then
    begin
      Inc(gInternal.frames_skip, (this_tick - gInternal.program_next_tick) * gInternal.expected_fps div 1000);
    end;

    gInternal.program_next_tick := SDL_GetTicks + longword(1000 div gInternal.expected_fps);
  end;

  this_tick := SDL_GetTicks;
  if (this_tick >= gInternal.frames_1second) then
  begin
    fps := gInternal.frames;
    gInternal.frames := 0;
    //gInternal.frames_1second := Trunc(single(now)*10000000000)+(1000 div MSecsPerDay)*10000000000;
    gInternal.frames_1second := this_tick+1000;
    //say(FloatToStr(now));
    //say(FloatToStr(gInternal.frames_1second));
    //say(IntToStr(Trunc(single(now)*1000000000)));
    //say(IntToStr(fps));
  end else
    Inc(gInternal.frames);

  // Timers
  scent := (this_tick-gInternal.last_tick) div 10;
  //say(IntToStr(scent));
  Inc(Timer[0], scent);
  Inc(Timer[1], scent); Inc(Timer[2], scent); Inc(Timer[3], scent);
  Inc(Timer[4], scent); Inc(Timer[5], scent); Inc(Timer[6], scent);
  Inc(Timer[7], scent); Inc(Timer[8], scent); Inc(Timer[9], scent);
  //say(IntToStr(Timer[0]));
  gInternal.last_tick:=this_tick;


  if (gInternal.frames_skip > 0) then
  begin
    Dec(gInternal.frames_skip);
    if (gInternal.frames_skip mod gInternal.expected_skip) <> 0 then
      Exit;
  end;

  // ====================
  // Dibujado de Gráficos
  // ====================
  // Ejecución de procesos por un lado y gráficos aparte
  // para no generar artifacts visuales.

  (*if (gInternal.fullscreen) then
    SDL_SetRenderTarget(gInternal.renderer, gInternal.fsTexture);*)
  {$ifndef NO_CLEAR}
  SDL_SetRenderDrawColor(gInternal.renderer,
        gInternal.clearcolor.r,
        gInternal.clearcolor.g,
        gInternal.clearcolor.b, 255);
  SDL_RenderClear(gInternal.renderer);
  {$endif}

  {$ifndef NO_BACKGROUND_GRAPH}
  SDL_RenderSetClipRect(gInternal.renderer, @regions.clip_region[0]);
  SDL_RenderCopy(gInternal.renderer, gInternal.background, nil, @regions.region[0]);
  {$endif}


  {$ifndef SCROLL_USE_Z}
  // Scroll sin Z, graficamos los del fondo
  for n := 0 to diyen_scroll_list.alist_max do
  begin
      sn := diyen_scroll_list.alist[n];
      sc := @scroll[sn];

      {$ifndef NO_MOVE_SCROLL}
      // Movemos automáticamente el scroll a "camera"
      if exists(sc^.camera) then diyen_scroll_list.move_scroll(n);
      {$endif}

      {$ifdef USE_DIYEN_CLIPRECT}
      diyen_ClipRect(regions[sc^.sregion]^);
      {$else}
      SDL_RenderSetClipRect(gInternal.renderer, @regions.clip_region[sc^.sregion]);
      {$endif}
      diyen_scroll_putgraph10(sc, regions[sc^.sregion]^);
  end;
  {$endif}

  {$if defined(ZLAYER) and defined(ZLAYER_RUNTIME)}
  if not use_zlayer then
  begin
  {$endif}
  {$if not defined(ZLAYER) or defined(ZLAYER_RUNTIME)}
  i := 0;
  while i < diyen_process_list.zlmax_fixed do
  begin
      Inc(i);
      process := diyen_process_list[diyen_process_list.zlist[i]];
      if (process = nil) then break;
      {$ifndef DISABLE_TILEMAPS}
      {$ifndef TILEMAPS_NO_Z}
      for tn := 0 to diyen_tilemap_list.alist_max do
      begin
        tmn := diyen_tilemap_list.alist[tn];
        tm := @tilemap[tmn];

        if (not tm^.drawed) and (tm^.z >= process.z) then
        diyen_tilemap_putgraph(tm);
      end;
      {$endif}
      {$endif}

      // Fuentes usando Z
      {$ifdef TEXT_USE_Z}
      if (not diyen_texts_list.drawed) and (text_z >= process.z) then
          diyen_texts_putgraph();
      {$endif}

      if (process.sleeping) then Continue;

      // Mantenemos el ángulo dentro de valores de ángulo
      process.angle := process.angle mod 360000;
      diyen_process_putgraph(process);
  end;
  // FIN MODO Z COMPUTADO
  {$endif}
  {$if defined(ZLAYER) and defined(ZLAYER_RUNTIME)}
  end else
  begin
  {$endif}


  // Modo Z con capas
  {$if defined(ZLAYER)}
    with diyen_process_list do
    // Graficamos primero el Z más alto para que quede debajo
    for Z := zlayer_max downto 0 do
    begin
    {$ifndef DISABLE_TILEMAPS}
    {$ifndef TILEMAPS_NO_Z}
    for tn := 0 to diyen_tilemap_list.alist_max do
    begin
      tmn := diyen_tilemap_list.alist[tn];
      tm := @tilemap[tmn];

      if (not tm^.drawed) and (tm^.z >= Z) then
      diyen_tilemap_putgraph(tm);
    end;
    {$endif}
    {$endif}

      // Graficamos procesos
      for i := 1 to zlayer[Z].max do
      begin
      process := diyen_process_list[diyen_process_list.zlayer[Z].p[i]];

      if (process = nil) then // Esto no debería pasar
        break;

      if (process.sleeping) then Continue;

      // Mantenemos el ángulo dentro de valores de ángulo
      process.angle := process.angle mod 360000;
      diyen_process_putgraph(process);
      end;

    // Fuentes usando Z
    {$ifdef TEXT_USE_Z}
    if (not diyen_texts_list.drawed) and (text_z >= Z) then
        diyen_texts_putgraph();
    {$endif}
  end;
  {$endif}
  {$if defined(ZLAYER) and defined(ZLAYER_RUNTIME)}
  end;
  {$endif}

  {$ifdef SCROLL_USE_Z}
  // Grafica los scrolls que no se han graficado (porque están al fondo de todo)
  for n := 0 to diyen_scroll_list.alist_max do
  begin
    sn := @diyen_scroll_list.alist[n];
    sc := @scroll[sn^];
      if (not sc^.drawed) then
      begin
        {$ifdef USE_DIYEN_CLIPRECT}
        diyen_ClipRect(regions[sc^.sregion]^);
        {$else}
        SDL_RenderSetClipRect(gInternal.renderer, @regions.clip_region[sc^.sregion]);
        {$endif}
        diyen_scroll_putgraph(sc);
      end;
    sc^.drawed := false;
  end;
  {$else}
  // Scroll sin Z, dibujamos el del frente
  for n := 0 to diyen_scroll_list.alist_max do
  begin
    sn := diyen_scroll_list.alist[n];
    sc := @scroll[sn];
    {$ifdef USE_DIYEN_CLIPRECT}
    diyen_ClipRect(regions[sc^.sregion]^);
    {$else}
    SDL_RenderSetClipRect(gInternal.renderer, @regions.clip_region[sc^.sregion]);
    {$endif}
    diyen_scroll_putgraph2(sc, regions[sc^.sregion]^);
  end;
  {$endif}

  // Ya no están más dibujados los tilemaps.
  {$ifndef DISABLE_TILEMAPS}
  {$ifndef TILEMAPS_NO_Z}
  for tn := 0 to diyen_tilemap_list.alist_max do
  begin
    tmn := diyen_tilemap_list.alist[tn];
    tm := @tilemap[tmn];

      if (not tm^.drawed) then
      diyen_tilemap_putgraph(tm);

    tm^.drawed:=false;
  end;
  {$endif}
  {$endif}


  // Prioridad y colisión independiente
  // FIXME: No está nada bien

  // Si sólo usa prioridad
  {$ifdef USE_PRIORITY}
  begin
    pi := 0;
    with diyen_process_list do
    while pi < priomax do
    begin
      Inc(pi);
      if (not gInternal.prio_ordered) and (pi < priomax) then
      begin
        imayor := pi;
        // Aquí se ordena usando el método de ordenamiento por selección
        for po:=pi+1 to priomax do
        begin
          if (diyen_process_list[priolist[imayor]] <> nil) then
            zi := diyen_process_list[priolist[imayor]].priority else zi := -32768;
          if (diyen_process_list[priolist[po]] <> nil) then
            zo := diyen_process_list[priolist[po]].priority else zo := -32768;
          if (zo > zi) then imayor := po;
        end;
        // Intercambiamos
        itmp := priolist[pi];
        priolist[pi] := priolist[imayor];
        priolist[imayor] := itmp;
      end;

      // Obtenemos el proceso que le corresponde
      process := diyen_process_list[priolist[pi]];
      if (process = nil) then
      begin
        diyen_process_list.priomax:=pi-1;
        break;
      end;

      // Analisis de señales para establecer estado
      if diyen_process_signals(process) then Continue;

      if (process.ended) then
      begin
        diyen_process_list.Del(process.id);
        Dec(pi);
        Continue;
      end;

      if (process.sleeping or process.killed or process.dead) then Continue;

      // Procesa
      if process.running and (process.Internal.frame_percent < 100) then process.loop;
      process.angle := process.angle mod 360000; // Mantenemos el ángulo dentro de valores de ángulo

      {$ifndef NO_MOVE_SCROLL}
      diyen_move_scroll(process.id);
      {$endif}
    end;
    with diyen_process_list do
    if (prio_max > 0) then
    begin
      //if (not diyen_process_list.prio_some_deleted) then
      //begin
        gInternal.prio_min := diyen_process_list[priolist[priomax]].priority;
        gInternal.prio_ordered := true;
      //end else
      //  diyen_process_list.prio_some_deleted := false;
    end;
  end;
  {$endif}

  // Sólo colisión independiente
  {$ifdef COLMODE_INDEPENDENT}
  //if (col_mode = COLMODE_INDEPENDENT) then
    with diyen_process_list do
    for pi:=1 to Max do
    begin
      process := diyen_process_list[pi];
      if (process = nil) then Continue;

      diyen_colandhit_detection(process);
    end;
  {$endif}
  // FIN de procesar los procesos

  SDL_RenderSetClipRect(gInternal.renderer, @regions.clip_region[0]);
  {$ifdef TEXT_USE_Z}
  if (not diyen_texts_list.drawed) then
    diyen_texts_putgraph();
  diyen_texts_list.drawed := false;
  {$else}
  // Si no usa Z los dibuja arriba de todos los demás gráficos
  diyen_texts_putgraph();
  {$endif}

  diyen_drawings_putgraph();
  diyen_touchs_putgraph();

  {$ifdef DEBUG_MOUSE}
  SDL_SetRenderDrawColor(gInternal.renderer, 200, 150, 150, 200);
  SDL_RenderDrawLine(gInternal.renderer, 0, mouse.Y, gInternal.DisplayBounds.w, mouse.Y);
  SDL_RenderDrawLine(gInternal.renderer, mouse.X, 0, mouse.X, gInternal.DisplayBounds.h);
  if Boolean(mouse.state and SDL_BUTTON_LMASK) then
    say('mouse='+IntToStr(mouse.X)+','+IntToStr(mouse.Y));
  {$endif}

  // Render a pantalla
  (*if (gInternal.fullscreen) then
  begin
    SDL_SetRenderTarget(gInternal.renderer, nil);
    SDL_RenderCopy( gInternal.renderer,
                    gInternal.fsTexture,
                    nil, @gInternal.DisplayBounds);
  end;*)
  SDL_RenderPresent(gInternal.renderer);
end;


// Some maths
procedure diyen_create_trig_tables();
var
  ang : float;
  i : integer;
begin
  ang := 0; i := 0;
  while i <= (360000 div DIYEN_ANG_STEP) do
  begin
    diyen_sin_table[i] := sin(ang);
    diyen_cos_table[i] := cos(ang);
    diyen_tan_table[i] := tan(ang);
    ang := ang + DIYEN_ANG_STEP * pi / 180000;
    Inc(i);
  end;
end;


(*procedure diyen_build_arctan_table();
var
  i: integer;
begin
  SetLength(diyen_arctan_table, 231);
  for i:=1 to 115 do
    diyen_arctan_table[116-i] := ArcTan(1/i);
  for i:=116 to 230 do
    diyen_arctan_table[i] := ArcTan(i-114);
end;*)

// Start the diyen program
procedure diyen_main(mainprogram: TMainProgram);
var
  FontDef : TFont;
begin
  //SDL_Init(SDL_INIT_EVERYTHING);
  SDL_InitSubSystem(SDL_INIT_TIMER);
  SDL_InitSubSystem(SDL_INIT_EVENTS);
  SDL_InitSubSystem(SDL_INIT_VIDEO);
  SDL_InitSubSystem(SDL_INIT_AUDIO);
  SDL_InitSubSystem(SDL_INIT_JOYSTICK);
  SDL_InitSubSystem(SDL_INIT_HAPTIC);
  SDL_InitSubSystem(SDL_INIT_GAMECONTROLLER);
  diyen_input_init();
  Mix_Init(MIX_INIT_MOD or MIX_INIT_MODPLUG or MIX_INIT_MP3 or MIX_INIT_OGG);

  {$ifdef ANDROID}
  force_accel := true;
  {$endif}

  SDL_GetDisplayBounds(0, @gInternal.DisplayBounds);

  diyen_create_trig_tables();

  gInternal.program_next_tick := 0;
  gInternal.expected_fps := 30;

  {$ifdef USE_SCALER}
  scaler := TScaler.Create;
  {$endif}

  regions := TdiyenRegionList.Create();
  diyen_scroll_list := TdiyenScrollList.Create();

  diyen_fpg_list := TdiyenFPGList.Create;
  diyen_fpg_list.new_fpg(0); // El 0 siempre existe y es el principal

  diyen_music_list := TdiyenMusicList.Create;
  diyen_sounds_list := TdiyenSoundsList.Create;

  diyen_fonts_list := TdiyenFontList.Create;
  FontDef := TFont.Create('default_fnt.png');
  diyen_fonts_list.fuente[0] := Pointer(FontDef);

  diyen_texts_list := TdiyenTextList.Create;

  diyen_drawings_list := TdiyenDrawList.Create;

  {$ifndef DISABLE_TILEMAPS}
  diyen_tilemap_list := TdiyenTileMapList.Create();
  {$endif}

  // Para que no produzca error de acceso a memoria
  // si se llama primero a la función key o similar.
  input_KeysState := SDL_GetKeyboardState(input_KeysNum);
  SDL_PollEvent(nil);

  // Un random distinto cada vez
  RandSeed := Trunc(now());

  diyen_process_list := TDiyenPList.Create;
  diyen_process_list.ProgramStart(mainprogram);
  //ProgramStart(mainprogram);
end;


(*
  ------------------
  DIV like functions
  ------------------
*)

// Set the window title
procedure set_title( title: string );
begin
  gInternal.title := title;
  if gInternal.window <> nil then
    SDL_SetWindowTitle(gInternal.window, pchar(title));
end;

// Print in like log messages
procedure say( msj: string );
begin
  SDL_Log(PChar(msj));
end;

// Set the desired fps
procedure set_fps( fps: int; skip: int );
begin
  gInternal.expected_fps := fps;
  gInternal.expected_skip := skip;
end;
procedure set_fps( fps: int );
begin
  gInternal.expected_fps := fps;
  gInternal.expected_skip := 0;
end;

// Envía una señal a un proceso
(*procedure signal( p: int; sig : TPSignal );
var
  i : integer;
  proc : TProcess;
begin
  //TProcess(diyen_process_list[pid]).Internal.signal := sig;
  // Lista de tipos
  if (p > DIYEN_LMAX_PROCESS) then
  begin
    i := 0;
    repeat
    Inc(i);
    proc := diyen_process_list[i];
      if (proc <> nil) and (word(proc.ClassType) = longword(p-DIYEN_LMAX_PROCESS)) then
      begin
        proc.Internal.signal := sig;
      end;
    until (i > DIYEN_LMAX_PROCESS);
  end else
  if (p = DIYEN_LMAX_PROCESS) then
  // Special case, all types (use PTYPE_UNDEFINED as parameter)
  begin
    i := 0;
    repeat
    Inc(i);
    proc := diyen_process_list[i];
      if (proc <> nil) then
      begin
        proc.Internal.signal := sig;
      end;
    until (i > DIYEN_LMAX_PROCESS);
  end else
  // Unico proceso
  diyen_process_list[p].Internal.signal := sig;
end;*)
procedure TProcess.signal( p: int; sig : TPSignal; all : boolean );
var
  i : int;
  status : Tpstatus;
  process: TProcess = nil;
begin
  case sig of
  S_FREEZE:   status := STATUS_FROZEN;
  S_SLEEP:    status  := STATUS_SLEEPING;
  S_WAKEUP:   status := STATUS_RUNNING;
  S_KILL:     status := STATUS_KILL;
  end;

  {$ifdef IS_LIBRARY}
  process := _diyenprocesslist(p);
  {$else}
  process := diyen_process_list[p];
  {$endif}

  if all then
  begin
    for i := 1 to DIYEN_LMAX_PROCESS do
    begin // Todos excepto el proceso indicado
      {$ifdef IS_LIBRARY}
      process := _diyenprocesslist(i);
      {$else}
      process := diyen_process_list[i];
      {$endif}

      if (i <> p) and
      (process <> nil)
      then
      if
        (process.Internal.status <> STATUS_KILL) and
        (process.Internal.status <> STATUS_UNSTARTED)
      then
        process.Internal.status := status;
    end;
  end else
  // FIXME: No hay que condicionar nada en este caso?
    process.Internal.status := status; // Uno solo
end;
procedure TProcess.signal( sig: Tpsignal; all: boolean );
begin
  signal(Internal.process_id, sig, all);
end;
procedure TProcess.signal( p: int; sig: Tpsignal );
var
  process: TProcess;
begin
  {$ifdef IS_LIBRARY}
  process := _diyenprocesslist(p);
  {$else}
  process := diyen_process_list[p];
  {$endif}

  if
  (process.Internal.status <> STATUS_KILL) and
  (process.Internal.status <> STATUS_UNSTARTED)
  then
  case sig of
  S_FREEZE:   process.Internal.status := STATUS_FROZEN;
  S_SLEEP:    process.Internal.status := STATUS_SLEEPING;
  S_WAKEUP:   process.Internal.status := STATUS_RUNNING;
  S_KILL:     process.Internal.status := STATUS_KILL;
  end;
end;

// Set the video mode
// It don't use flags or colordepth because always is at maximum (32 bits OpenGL).
// Returns true if success or false if an error occured
// No se puede cambiar el modo de video en SDL una vez inciado sin perder las
// texturas (habría que implementar algo para recargar las texturas).
function set_mode(width, height, lwidth, lheight : integer) : boolean;
var
  flags: longint;
  //x, y: single;
  //scale: single;
  //swidth, sheight: single;

  n : integer;
  //wscal, hscal : single;
begin
  if (gInternal.window = nil) then
  begin
    flags := SDL_WINDOW_OPENGL;
    {$ifdef ANDROID}
    flags := flags or SDL_WINDOW_BORDERLESS;
    //flags := flags or SDL_WINDOW_MAXIMIZED;
    //gInternal.scaling := true;
    gInternal.fullscreen := true;
    {$endif}
    gInternal.window := SDL_CreateWindow(PChar(gInternal.title),
    SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED,
    width, height, flags);

    // Con la bandera no anda bien, así que es mejor establecer
    // el modo pantalla completa con SDL_SetWindowFullscreen
    if gInternal.fullscreen then
    begin
      if (not gInternal.scaling) then
        //flags := flags or SDL_WINDOW_FULLSCREEN_DESKTOP
        SDL_SetWindowFullscreen(gInternal.window, SDL_WINDOW_FULLSCREEN_DESKTOP)
      else
        //flags := flags or SDL_WINDOW_FULLSCREEN;
        SDL_SetWindowFullscreen(gInternal.window, SDL_WINDOW_FULLSCREEN);
    end;

    {$ifdef ANDROID}
      //SDL_SetWindowSize(gInternal.window, lwidth, lheight);
      //SDL_SetWindowSize(gInternal.window, lwidth, lheight);
    {$endif}
  end else
  begin
    SDL_SetWindowSize(gInternal.window, width, height);
  end;

  if (gInternal.renderer = nil) then
  begin
    //SDL_DestroyRenderer(gInternal.renderer);
    //flags := SDL_RENDERER_PRESENTVSYNC;
    flags := 0;
    if (force_accel) then
      flags := flags or SDL_RENDERER_ACCELERATED;

    if (software_mode) then
      flags := flags or SDL_RENDERER_SOFTWARE;

    gInternal.renderer := SDL_CreateRenderer(gInternal.window, -1,
      SDL_RENDERER_TARGETTEXTURE or flags)
  end;

  {$ifndef USE_SCALER}
  if (lwidth <> 0) or (lheight <> 0) then
  begin
    gInternal.lscreen.w := lwidth;
    gInternal.lscreen.h := lheight;
    SDL_RenderSetLogicalSize(gInternal.renderer, lwidth, lheight);

    {$ifdef ANDROID}
    SDL_Log(PChar('Screen Bounds: '+IntToStr(width)+'x'+IntToStr(height)));
    //SDL_Log('Screen Bounds: '+IntToStr(width)+'x'+IntToStr(height));
    {$endif}

    regions[0]^.x:=0;
    regions[0]^.y:=0;
    regions[0]^.w:=lwidth;
    regions[0]^.h:=lheight;


    (*x := 0; y := 0;
    if (lwidth > lheight) then
    begin
      scale := width / lwidth;
      y := (height - lheight*scale) / 2;
    end else
    begin
      scale := height / lheight;
      x := (width - lwidth*scale) / 2;
    end;

    swidth := lwidth*scale;
    sheight:= lheight*scale;*)


    //gInternal.vp.x := Trunc(x);
    //gInternal.vp.y := Trunc(y);
    //gInternal.vp.w := Trunc(swidth);
    //gInternal.vp.h := Trunc(sheight);
    // FIXME: Lo correcto sería calcular como hace SDL para poder centrar el viewport
    gInternal.vp.x := 0;
    gInternal.vp.y := 0;
    gInternal.vp.w := lwidth;
    gInternal.vp.h := lheight;

    {$ifdef ANDROID}
    //SDL_Log(PChar('Scale factor: '+FloatToStr(scale)));
    {$endif}

    //SDL_RenderSetScale(gInternal.renderer, scale, scale);
    SDL_RenderSetViewport(gInternal.renderer, @gInternal.vp);
    //SDL_RenderGetScale(gInternal.renderer, @gInternal.scale.x, @gInternal.scale.y);
    gInternal.scaling:=true;

    // ¿Y esas cuentas a continuación?
    // No entiendo qué hace el Scaler de SDL pero es un "desorden" total.
    // Aparentemente si el tamaño de destino es más chico que el origen,
    // algunas cuentas se invierten. No le encuentro la lógica a ese
    // comportamiento pero tendrá su razón de ser. Tal vez esa es la manera
    // más rápida de escalar? No sé.
    // Estas cuentas sirven para acomodarnos a ese "despelote" que hace SDL.

    //SDL_RenderGetScale(gInternal.renderer, @wscal, @hscal);
    for n := 0 to DIYEN_LMAX_REGIONS do
    begin
      with regions.clip_region[n] do
      begin
        x := regions.region[n].x;
        y := regions.region[n].y;
        w := regions.region[n].w;
        h := regions.region[n].h;

      //w := Trunc((w)*wscal);
      //h := Trunc((h)*hscal);
      end;
      (*if swidth >= lwidth then
      begin
        regions.clip_region[n].x := regions.region[n].x;
        regions.clip_region[n].w := Trunc((regions.region[n].w) * scale);
      end else
      begin
        regions.clip_region[n].x := regions.region[n].x+Trunc(x/scale);
        regions.clip_region[n].w := regions.region[n].w;
      end;

      if sheight >= lheight then
      begin
        regions.clip_region[n].y := regions.region[n].y;
        regions.clip_region[n].h := Trunc((regions.region[n].h) * scale);
      end else
      begin
        // ¿Qué pasa con SDL que trabaja con las X diferente a que con las Y
        // cuando se trata de escalar y del viewport? Es un BUG?
        regions.clip_region[n].y := regions.region[n].y+abs(Trunc(y/scale));
        regions.clip_region[n].h := regions.region[n].h;
      end;*)
    end;

  end else
  {$endif}
  begin
    lwidth  := width;
    lheight := height;
    gInternal.lscreen.w := lwidth;
    gInternal.lscreen.h := lheight;

    SDL_RenderSetLogicalSize(gInternal.renderer, lwidth, lheight);

    regions[0]^.x:=0;
    regions[0]^.y:=0;
    regions[0]^.w:=lwidth;
    regions[0]^.h:=lheight;

    gInternal.vp.x := 0;
    gInternal.vp.y := 0;
    gInternal.vp.w := lwidth;
    gInternal.vp.h := lheight;
    //SDL_RenderSetScale(gInternal.renderer, 1, 1);
    SDL_RenderSetViewport(gInternal.renderer, @gInternal.vp);
    //gInternal.scale.x := 1;
    //gInternal.scale.y := 1;
    gInternal.scaling:=false;

    for n := 0 to DIYEN_LMAX_REGIONS do
    begin
      regions.clip_region[n].x := regions.region[n].x;
      regions.clip_region[n].y := regions.region[n].y;
      regions.clip_region[n].w := regions.region[n].w;
      regions.clip_region[n].h := regions.region[n].h;
    end;
  end;

  Result := (gInternal.window <> nil) and (gInternal.renderer <> nil);

  if Result then
  begin
    SDL_GetRendererInfo(gInternal.renderer, @gInternal.render_info);
    // FIXME: No creo que gInternal.fullscreen aún esté seteada
    (*if not gInternal.scaling and gInternal.fullscreen then
    begin
      SDL_SetWindowFullscreen(gInternal.window, SDL_WINDOW_FULLSCREEN_DESKTOP);
      //SDL_RenderGetScale(gInternal.renderer, @gInternal.fs_scale, nil);
    end;*)

    // Limpiamos la pantalla
    //SDL_SetRenderDrawColor(gInternal.renderer, gInternal.clearcolor.r, gInternal.clearcolor.g, gInternal.clearcolor.b, 255);
    //SDL_RenderClear(gInternal.renderer);

    // Creamos el gráfico de fondo
    clear_screen();
  end;

end;

function set_mode(width, height : integer) : boolean;
begin
  Result := set_mode(width, height, 0, 0);
end;

// Establece el modo automáticamente ya sea que se compila en Android
// o en PC.
function set_mode_auto(width, height : integer) : boolean;
{$ifdef ANDROID}
var
  Swidth, Sheight : integer;
{$endif}
begin
  {$ifdef ANDROID}
    Swidth := gInternal.DisplayBounds.w;
    Sheight := gInternal.DisplayBounds.h;
    Result := set_mode(Swidth, Sheight, width, height);
  {$else}
    Result := set_mode(width, height, 0, 0);
  {$endif}
end;


function set_hint(hint: THint; value: string): Boolean;
var
  shint : string;
begin
  shint := THintToStr[hint];
  Result := SDL_SetHint(PChar(shint), PChar(value));
end;

// Returns a valid value for the function collision or hit
{$ifndef AUTO_PTYPE}
function ptype( pt : int ): int;
begin
  Result := int(word(pt)+DIYEN_LMAX_PROCESS);
end;
{$else}
function ptype( pt: TClass ): int;
begin
  //Result := int(word(byte(pt)+33)+DIYEN_LMAX_PROCESS);
  Result := int(word(pt)+DIYEN_LMAX_PROCESS);
end;
{$endif}
function ptypeall(): int;
begin
  Result := int(DIYEN_LMAX_PROCESS);
end;

// Returns a TProcess from the supplied process id
function TP( pid: word ): TProcess;
begin
  Result := TProcess(diyen_process_list.velementos[pid]);
end;

// exists
// Check if the process exists and return true if exists or false if not.
function exists( var pid: word ): boolean;
begin
  Result := (diyen_process_list[pid] <> nil);
  if Result then
    Result :=
    (TProcess(diyen_process_list[pid]).Internal.status <> STATUS_KILL) and
    (TProcess(diyen_process_list[pid]).Internal.status <> STATUS_DEAD)
  else
    pid := 0;
end;

// Define una región de pantalla como un recuadro donde se dibujarán
// sólo los procesos que pertenecen a esa región
procedure define_region( num, x, y, ancho, alto: int );
begin
  regions.define_region(byte(num), x, y, ancho, alto);
end;

// Verdadero si está fuera de la región
function out_region(x, y, res: int; num: byte) : boolean;
begin
  Result := regions.out_region(x, y, res, num);
end;
function out_region(pid: word; num: byte) : boolean;
var
  process : TProcess;
begin
  process := diyen_process_list[pid];
  Result := regions.out_region(process.x, process.y, process.resolution, num);
end;

function SREG( num: byte ): TSDL_Rect;
begin
  Result := regions[num]^;
end;


procedure start_scroll( scrollnumber,
  fileID, graphID, background1graphID, background2graphID,
  regionnumber: int;
  lockindicator : TSetScrollLock );
begin
  diyen_scroll_list.start_scroll(byte(scrollnumber), word(fileID), word(graphID), word(background1graphID), word(background2graphID), byte(regionnumber), lockindicator);
end;
procedure start_scroll( scrollnumber,
  fileID, graphID, backgroundgraphID,
  regionnumber: int;
  lockindicator : TSetScrollLock );
begin
  diyen_scroll_list.start_scroll(byte(scrollnumber), word(fileID), word(graphID), word(backgroundgraphID), byte(regionnumber), lockindicator);
end;

procedure move_scroll ( scrollnumber: int );
begin
  diyen_scroll_list.move_scroll(byte(scrollnumber));
end;

procedure stop_scroll( scrollnumber: byte );
begin
  diyen_scroll_list.stop_scroll(scrollnumber);
end;

function dsin( ang: integer ): single;
begin
  Result := diyen_sin_table[((360000+ang mod 360000) mod 360000) div DIYEN_ANG_STEP];
end;
function dcos( ang: integer ): single;
begin
  Result := diyen_cos_table[((360000+ang mod 360000) mod 360000) div DIYEN_ANG_STEP];
end;
function dtan( ang: integer ): single;
begin
  Result := diyen_tan_table[((360000+ang mod 360000) mod 360000) div DIYEN_ANG_STEP];
end;

// Devuelve el ángulo en formato DIV (0 a 360000) según las coordenadas dadas.
function datan( y, x: int ): int;
begin
  (*if abs(y) > 115 then y := 115;
  Result := diyen_arctan_table[Round(abs(y / x)*115)];*)
  Result := Trunc(ArcTan2(y, x)*180/pi)*1000;
  //say('y, x arctan(y/x): '+IntToStr(y)+','+IntToStr(x)+' '+IntToStr(Result));
end;

// First version of this function. Multiplies power times the base.
// This function does not support negative values for power.
function pow( base : single; power: integer ): single;
var
  t : integer;
begin
  Result := 1;
  for t:=1 to power do
    Result := Result * base;
end;



function fget_angle(pa_x, pa_y, pb_x, pb_y : int) : int;
var
  rx, ry : integer;
begin
  rx := pb_x-pa_x;
  ry := pb_y-pa_y;
    (*if rx <> 0 then
    begin
      if (rx > 0) then
        Result := Trunc(ArcTan(ry / rx)*(180000/pi))
      else
        Result := Trunc(ArcTan(ry / rx)*(180000/pi)) - 180000;

    end
    else
      Result := 90000;*)

    if rx <> 0 then
      Result := (Trunc(ArcTan2(ry, rx)*(180/pi))*1000)
    else
    if ry < 0 then
      Result := 270000
    else
    if ry > 0 then
      Result := 90000
    else
      Result := 0;
end;

// Devuelve el ángulo más próximo a dangle en incrementos inc
function near_angle( oangle, dangle, increment : int ) : int;
var
  dif : integer;
begin
  dangle := (dangle+360000) mod 360000;
  //dangle := dangle mod 360000;

  oangle := (oangle+360000) mod 360000;
  //oangle := oangle mod 360000;

  dif := (oangle-dangle);

  Result := oangle;
  if abs(dif) >= increment then
  begin
    if (dif+360000) mod 360000 > 180000 then
      Result := oangle+increment
    else
    //if (dif+360000) mod 360000 > 180000 then
      Result := oangle-increment;

    //if ((((oangle-dangle) mod 360000)+360000) mod 360000) > 180000 then
    //  Result := oangle+inc
    //else
    //  Result := oangle-inc;
  end;
end;


// Este algoritmo en la función es para poder calcular el cuadrado de
// números grandes. Porque realmente obtendríamos un resultado
// gigante. Ejemplo: el cuadrado de 10 mil es 100 millones.
// Una PC moderna puede calcularlo, pero un dispositivo Android
// de 32 bits no es capaz.
function fget_dist(pa_x, pa_y, pb_x, pb_y : int) : int;
var
  rx, ry : integer;
  //sqx, sqy : integer;
begin
  rx := pb_x-pa_x;
  ry := pb_y-pa_y;

  if (rx > 10000) or (rx < -10000) or
     (ry > 10000) or (ry < -10000) then
  begin
    rx := rx div 100;
    ry := ry div 100;
    Result := Trunc(sqrt(rx*rx+ry*ry)*100);
  end
  else
    Result := Trunc(sqrt(rx*rx+ry*ry));
end;


// Same function as advance but for use without a process class.
procedure advance( var x, y: int; angle, distance: int );
begin
  x := x+Trunc(dcos(angle)*distance);
  y := y+Trunc(dsin(angle)*distance);
end;


// Returns the greater number
function greater( number1, number2 : integer ): integer;
begin
  Result := number2;
  if (number1 > number2) then Result:=number1;
end;
function bestratio( number1, number2 : integer ): integer;
begin
  Result := number2 div number1;
  if (number1 > number2) then Result:=number1 div number2;
end;
function minmax( number, min, max: integer ): integer;
begin
  if (number > max) then number := max;
  if (number < min) then number := min;
  Result := number;
end;

(*
  Implementación de una función de generación de números
  pseudo aleatorios rápida. Ya que en juegos cualquier ms es útil.
  FIXME: Es esta la función más rápida para todas las arquitecturas?
*)
{disable range check error}
{$R-}
// LCG Random algorithm
function IM:cardinal;inline;
begin
  RandSeed := RandSeed * 134775813  + 1;
  Result := RandSeed;
end;
(*function LCGRandom: extended;inline;
begin
  Result := IM * 2.32830643653870e-10;
end;*)
function LCGRandom(const range:longint):longint;inline;
begin
  Result := IM * range shr 32;
end;
// Random function like DIV
// Atención: Tenemos una limitación de valor
// El valor entregado no puede ser mayor a 65536
function rand(i, f: integer): integer;
begin
  Result := i+( LCGRandom(f-i+1) );
end;
{$R+}

(*
  =======
  TScaler
  =======
*)

{$ifdef USE_SCALER}
procedure TScaler.Enable( activar: boolean );
var
  n : int;
begin
  if activar and not venabled then
  begin
    // escala las regiones que se usan en pantalla para clip
    {$ifndef USE_SCALER_SDL}
    for n := 0 to DIYEN_LMAX_REGIONS do
    begin
      regions.clip_regions[n].x := regions.regions[n].x;
      regions.clip_regions[n].y := regions.regions[n].y;
      regions.clip_regions[n].w := Trunc((regions.regions[n].w) * scale) >> 10;
      regions.clip_regions[n].h := Trunc((regions.regions[n].h) * scale) >> 10;
    end;
    {$else}
    for n := 0 to DIYEN_LMAX_REGIONS do
    begin
      regions.clip_regions[n].x := regions.regions[n].x;
      regions.clip_regions[n].y := regions.regions[n].y;
      regions.clip_regions[n].w := Trunc((regions.regions[n].w) * sdl_scale);
      regions.clip_regions[n].h := Trunc((regions.regions[n].h) * sdl_scale);
    end;
    SDL_RenderSetScale(gInternal.renderer, fscale, fscale);
    {$endif}
    venabled := true;
  end;
  if not activar and venabled then
  begin
    // Restablece las regiones de pantalla para clip
    for n := 0 to DIYEN_LMAX_REGIONS do
    begin
      regions.clip_regions[n].x := regions.regions[n].x;
      regions.clip_regions[n].y := regions.regions[n].y;
      regions.clip_regions[n].w := regions.regions[n].w;
      regions.clip_regions[n].h := regions.regions[n].h;
    end;
    {$ifdef USE_SCALER_SDL}
    SDL_RenderSetScale(gInternal.renderer, 1, 1);
    {$endif}
    venabled := false;
  end;

  gInternal.scaling := venabled;
end;


procedure TScaler.init( ow, oh, sw, sh : integer );
var
  x, y : single;
  w, h : single;
begin
  w := (ow/sw);
  h := (oh/sh);

  x := 0;
  y := 0;

  if (w > h) then
  begin
    w := h;
    x := ow / 2 - sw*h / 2;
  end;

  if (h > w) then
  begin
    h := w;
    y := oh / 2 - sh*w / 2;
  end;

  {$ifndef USE_SCALER_SDL}
  scale := Trunc(w*1024);

  dstrect.x := Trunc(x);
  dstrect.y := Trunc(y);
  dstrect.w := Trunc(sw*w);
  dstrect.h := Trunc(sh*w);
  {$else}
  fscale := w;
  SDL_RenderSetScale(gInternal.renderer, fscale, fscale);
  {$endif}
end;

{$ifndef USE_SCALER_SDL}
procedure TScaler.scaleit(orig: PSDL_Rect; center: PSDL_Point);
begin
  orig^.w := (orig^.w * scale) >> 10;
  orig^.h := (orig^.h * scale) >> 10;
  orig^.x := dstrect.x+ ( orig^.x * scale) div 1024;
  orig^.y := dstrect.y+ ( orig^.y * scale) div 1024;

  if (center <> nil) then
  begin
    center^.x := (center^.x * scale) >> 10;
    center^.y := (center^.y * scale) >> 10;
  end;
end;
{$endif}
{$endif}

(***************************************************************************
  =======
  Process
  =======
****************************************************************************)
procedure TMainProgram.main; begin exit_status := 1; end;
// Start
// It function serve to initialize variables of your process.
// It is called once per process creation
procedure TProcess.start( params: pointer ); begin start(); end;
procedure TProcess.start; begin end;
// Main execution loop - It must be overriden to write your own code for your process
{$ifdef USE_THINK}
procedure TProcess.think; begin kill; end;
procedure TProcess.loop; begin frame; end;
{$else}
{$ifndef LOOP_NO_KILL}
procedure TProcess.loop; begin kill; end;
{$else}
procedure TProcess.loop; begin frame; end;
{$endif}
{$endif}
procedure TProcess.col(other: TProcess; nbound, nobound: word); begin end;
{$ifndef NO_HIT_COL}
procedure TProcess.hit(other: TProcess; nbound, nobound: word ); begin end;
{$endif}
{$ifdef HIT_HIT}
procedure TProcess.hithit(other: TProcess; nbound, nobound: word ); begin end;
{$endif}
procedure TProcess.died; begin end;

// Add
// Add a new pocess to the game. The new process must be created with .Create
// Example: ProcessAdd(TShip.Create);
// Returns the id of the new created process or 0 if no process can be
// created.
(*function TProcess.ProcessAdd( process: TProcess) : integer;
begin
  Result := diyen_process_list.Add(process, Internal.process_id, nil);
end;*)
// Short version of the same function
function TProcess.PAdd( process: TProcess ): word;
begin
  Result := diyen_process_list.Add(process, Internal.process_id, nil);
end;
function TProcess.PAdd( process: TProcess; params: pointer ): word;
begin
  Result := diyen_process_list.Add(process, Internal.process_id, params);
end;
function PAdd( process: TProcess; father: word): word;
begin
  Result:= diyen_process_list.Add(process, father, nil);
end;


// Frame
// Establish the drawing of the process.
procedure TProcess.frame( percent: integer );
begin
  if (Internal.process_id = 0) then
  begin
    //if (exit_status = 0) then
    begin
      diyen_main_loop;
    end;
  Exit;
  end;

  Inc(Internal.frame_percent, percent);
  Internal.id_scan := 0;

  // Collision detection
  {$ifdef COLMODE_INDEPENDENT}
  {$else}
    diyen_colandhit_detection(self);
  {$endif}
end;
procedure TProcess.frame;
begin
  frame(100);
end;
// Exit
// The Exit function of pascal exits only from the procedure
// not from the entire program.
// So we need the Exit to exit from the entire program.
procedure dExit( code: integer );
begin
  Halt(code);
end;
procedure dExit;
begin
  SDL_Quit;
end;
procedure dExit(message: string; code: integer);
begin
  say(message);
  Halt(code);
end;
procedure dExit(message: string);
begin
  dExit(message, -1);
end;

// Status of the process
function TProcess.running: boolean;
begin
  Result := (Internal.status = STATUS_RUNNING);
end;
function TProcess.sleeping: boolean;
begin
  Result := (Internal.status = STATUS_SLEEPING);
end;
function TProcess.frozen: boolean;
begin
  Result := (Internal.status = STATUS_FROZEN);
end;


// Cierra todos los procesos menos este
procedure TProcess.let_me_alone();
var
  i : word;
  {$ifdef ZLAYER}
  Zi : word;
  {$endif}
begin
  for i:=1 to DIYEN_LMAX_PROCESS do
  begin
    if (i <> Internal.process_id) then begin
      TProcess(diyen_process_list[i]).Free;
      diyen_process_list.velementos[i] := nil;
    end;
  end;

  diyen_process_list.vmax := 0;
  diyen_process_list.vindice_uborrado:=0;
  diyen_process_list.startl_max := 0;
  diyen_process_list.endedl_max := 0;
  diyen_process_list.killl_max := 0;
  {$if not defined(ZLAYER) or defined(ZLAYER_RUNTIME)}
  if not (Self is TMainProgram) then
  begin
    diyen_process_list.zlmax := 1;
    diyen_process_list.zlist[1] := Internal.process_id;
  end else
  begin
    diyen_process_list.zlmax := 0;
  end;
  //gInternal.Z_comp_ordered := false;
  {$endif}
  {$ifdef ZLAYER}
  with diyen_process_list do
  begin
    for Zi := 0 to zlayer_max do
      zlayer[Zi].max := 0;

    if not (self is TMainProgram) then
    begin
      Internal.Zl.nZ := 1;
      zlayer[z].max := 1;
      zlayer[z].p[1] := Internal.process_id;
    end;
  end;
  {$endif}
end;

// Devuelve los bounds correspondientes al número.
// ATENCIÓN: No comprueba si el proceso tiene gráfico asignado.
// NOTA: Devuelve una copia, habría que pensar si conviene poder acceder
// para modificar el bound.
function TProcess.get_col_bound(num: word): TBound;
begin
  Result := TMap(TFPG(diyen_fpg_list.fpg[file_]).maps[graph]).col_bounds.bounds[num];
end;

function TProcess.get_hit_bound(num: word): TBound;
begin
  Result := TMap(TFPG(diyen_fpg_list.fpg[file_]).maps[graph]).hit_bounds.bounds[num];
end;

// Se finaliza
// You can call this to terminate your process.
procedure TProcess.kill;
begin
  Internal.status := STATUS_KILL;
end;
procedure TProcess.kill( pid: word );
begin
  signal(pid, S_KILL);
end;

// Devuelve la colisión con la clase de proceso
// Es como la función del mismo nombre en DIV
function TProcess.collision( pclass : TClass ) : TProcess;
var
  proc : TProcess;
begin
  if (Internal.class_scan_last <> pclass) then
    Internal.id_scan := 0;
  Internal.class_scan_last := pclass;
  Result := nil;

    repeat
    Inc(Internal.id_scan);
    proc := diyen_process_list[Internal.scan_collision_list[Internal.id_scan]];
      if (proc = nil) then
      begin
        Internal.id_scan:=0;
        Result := nil;
        break;
      end else
        Result := proc;
    until (proc is pclass);
end;


// Devuelve la colisión con el identificador de proceso
function TProcess.collision( p : int ) : TProcess;
var
  proc : TProcess;
begin
  if (Internal.id_scan_last <> p) then
    Internal.id_scan := 0;
  Internal.id_scan_last := p;
  Result := nil;

  // Type listing
  (*if (p > DIYEN_LMAX_PROCESS) then
  begin
    Internal.col_filter:=Internal.col_filter or longword(1<<word(p-DIYEN_LMAX_PROCESS));
    repeat
    Inc(Internal.id_scan);
    proc := diyen_process_list[Internal.scan_collision_list[Internal.id_scan]];
      if (proc = nil) then
      begin
        Internal.id_scan:=0;
        Result := nil;
        break;
      end else
        Result := proc;
    until (proc.process_type = word(p-DIYEN_LMAX_PROCESS));
  end else
  if (p = DIYEN_LMAX_PROCESS) then
  // Special case, all types (use PTYPE_UNDEFINED as parameter)
  begin
    Internal.col_filter := PTYPE_ALL;
    repeat
    Inc(Internal.id_scan);
    proc := diyen_process_list[Internal.scan_collision_list[Internal.id_scan]];
      if (proc = nil) then
      begin
        Internal.id_scan:=0;
        Result := nil;
        break;
      end else
      begin
        Result := proc;
        break;
      end;
    until (Internal.id_scan > (DIYEN_LMAX_PROCESS-1));
  end else*)
  // Explicit process id
    repeat
    Inc(Internal.id_scan);
    proc := diyen_process_list[Internal.scan_collision_list[Internal.id_scan]];
      if (proc = nil) then
      begin
        Internal.id_scan:=0;
        Result := nil;
        break;
      end else
        Result := proc;
    until (proc.Internal.process_id = p);
end;

(*function TProcess.collision( p : integer ) : TProcess;
begin
  Result := pp(collision( p ));
end;*)

function TProcess.advance( distance: integer ) : boolean;
begin
  Result := false;
  if (distance = 0) then Exit;
  //x := x+Trunc(cos(angle*2*pi / 360000)*distance);
  //y := y+Trunc(sin(angle*2*pi / 360000)*distance);
  x := x+Trunc(dcos(angle)*distance);
  y := y+Trunc(dsin(angle)*distance);
  Result := true;
end;

function TProcess.advance( dangle, distance: int ) : boolean;
begin
  Result := false;
  if (distance = 0) then Exit;
  x := x+Trunc(dcos(dangle)*distance);
  y := y+Trunc(dsin(dangle)*distance);
  Result := true;
end;

function TProcess.get_distx( ang, dist : integer ) : integer;
begin
  Result := Trunc(dcos(ang)*dist);
end;

function TProcess.get_disty( ang, dist : integer ) : integer;
begin
  Result := Trunc(dsin(ang)*dist);
end;

function TProcess.getsize : smallint;
begin
  Result := greater(size_x, size_y);
end;

procedure TProcess.putsize ( newsize : smallint );
begin
  size_x := newsize;
  size_y := newsize;
end;

function TProcess.GetFullScreen : Boolean;
begin
  Result := gInternal.fullscreen;
end;

procedure TProcess.SetFullScreen( fs : boolean );
begin
  // Se cambió a pantalla completa?
  if (fs) then
  begin
    if gInternal.window <> nil then
    begin
      {$ifdef ANDROID}
      (*
      //SDL_GetDisplayBounds(0, @gInternal.DisplayBounds);
      //SDL_GetWindowSize(Global.video.ventana, @BoundsRect.w, @BoundsRect.h);
      SDL_SetWindowSize(gInternal.window, gInternal.DisplayBounds.w, gInternal.DisplayBounds.h);
      {gInternal.fsTexture := SDL_CreateTexture(gInternal.renderer,
                              SDL_PIXELFORMAT_BGRA8888,
                              SDL_TEXTUREACCESS_TARGET,
                              gInternal.DisplayBounds.w, gInternal.DisplayBounds.h);}
      scaler.genrect(
            gInternal.DisplayBounds.w / regions[0]^.w,
            gInternal.DisplayBounds.h / regions[0]^.h,
            1);
      scaler.enabled:=true;
      //set_mode(gInternal.DisplayBounds.w, gInternal.DisplayBounds.h);*)
      {$else}

      {$endif}
      if (not gInternal.scaling) then
        SDL_SetWindowFullscreen(gInternal.window, SDL_WINDOW_FULLSCREEN_DESKTOP)
      else
        SDL_SetWindowFullscreen(gInternal.window, SDL_WINDOW_FULLSCREEN);

      SDL_RenderSetViewport(gInternal.renderer, @gInternal.vp);
      //gInternal.nofs_scale := gInternal.scale;
      //SDL_RenderGetScale(gInternal.renderer, @gInternal.scale.x, @gInternal.scale.y);
      (*gInternal.scale.x := gInternal.DisplayBounds.w / gInternal.vp.w;
      gInternal.scale.y := gInternal.DisplayBounds.h / gInternal.vp.h;*)
      //gInternal.vp.w := gInternal.DisplayBounds.w;
    end;
    gInternal.fullscreen := true;
  end else
  begin
    if gInternal.window <> nil then
    begin
      {$ifdef ANDROID}
      (*
      //SDL_GetDisplayBounds(0, @gInternal.DisplayBounds);
      //SDL_GetWindowSize(Global.video.ventana, @BoundsRect.w, @BoundsRect.h);
      SDL_SetWindowSize(gInternal.window, regions[0]^.w, regions[0]^.h);
      //set_mode(regions[0]^.w, regions[0]^.h);
      scaler.enabled:=false;
      {  if (gInternal.fsTexture <> nil) then
          SDL_DestroyTexture(gInternal.fsTexture);}*)
      {$else}

      {$endif}
      SDL_SetWindowFullscreen(gInternal.window, 0);
      SDL_RenderSetViewport(gInternal.renderer, @gInternal.vp);
      //gInternal.scale := gInternal.nofs_scale;
    end;
    gInternal.fullscreen := false;
  end;

  // Limpiamos la pantalla
  //SDL_SetRenderDrawColor(gInternal.renderer, gInternal.clearcolor.r, gInternal.clearcolor.g, gInternal.clearcolor.b, 255);
  //SDL_RenderClear(gInternal.renderer);
end;

(*function TProcess.GetClearColor: TSDL_Color;
begin
  Result := gInternal.clearcolor;
end;
procedure TProcess.SetClearColor( ccolor : TSDL_Color );
begin
  gInternal.clearcolor := ccolor;
  SDL_SetRenderDrawColor(gInternal.renderer, gInternal.clearcolor.r, gInternal.clearcolor.g, gInternal.clearcolor.b, 255);
  SDL_RenderClear(gInternal.renderer);
end;*)

(*function TProcess.get_angle( pid : integer ) : integer;
var
  process : TProcess;
begin
  process := diyen_process_list[pid];
  Result := fget_angle(x, y, process.x, process.y);
end;*)
function TProcess.get_angle( process : TProcess ) : integer;
begin
  Result := fget_angle(x, y, process.x, process.y);
end;

(*function TProcess.get_dist( pid : integer ) : integer;
var
  process : TProcess;
begin
  process := diyen_process_list[pid];
  Result := fget_dist(x, y, process.x, process.y);
end;*)
// Devuelve la distancia en pixeles del proceso que la llama
// al que se le pasa como parámetro.
// Es útil para manejarnos con valores inamobibles.
// Para obtener la distancia en valores de acuerdo al valor
// real de x, y se usa fget_dist
function TProcess.get_dist( process : TProcess ) : integer;
var
  rx: int;
  ry: int;
begin
  rx := process.x div process.resolution - x div resolution;
  ry := process.y div process.resolution - y div resolution;
  Result := Trunc(rx*rx+ry*ry);
  //Result := fget_dist(x, y, process.x, process.y);
end;

constructor TProcess.Create;
begin
  SetLength(Internal.scan_collision_list, DIYEN_LMAX_PROCESS+1);
end;
constructor TProcess.Create( Father: TProcess );
begin
  SetLength(Internal.scan_collision_list, DIYEN_LMAX_PROCESS+1);
  diyen_process_list.Add(self, father.Internal.process_id, nil);
end;

destructor TProcess.Destroy;
begin
  SetLength(Internal.scan_collision_list, 0);
  {$ifndef NO_FX_ENGINE}
  SetLength(Internal.fxlist, 0);
  {$endif}
  if (self is TMainProgram) then
    diyen_main_loop_end;

  inherited;
end;

(*
  ----------------------------------------------------------------------------
  TDiyenPList implementation

  A fast list implementation.
  Esta es una lista rápida especialmente pensada para obtener velocidad.
  Algunas funciones que pudieran parecer inseguras en realidad no lo son, ya
  que es necesario utilizar funciones a un nivel un poco más bajo para obtener
  los resultados esperados. De esta forma nos aseguramos de realizar sólo las
  comprobaciones justas y necesarias.
  De todos modos no tiene por qué fallar.
  ----------------------------------------------------------------------------
*)

constructor TDiyenPList.Create;
var
  i : word;
begin
  vmax := 0;
  vindice_uborrado := 0;
  SetLength(vultimos_borrados, DIYEN_LMAX_PROCESS+1);

  startl_max := 0;
  SetLength(startl, DIYEN_LMAX_PROCESS+1);
  endedl_max := 0;
  SetLength(endedl, DIYEN_LMAX_PROCESS+1);
  killl_max := 0;
  SetLength(killl, DIYEN_LMAX_PROCESS+1);

  {$if not defined(ZLAYER) or defined(ZLAYER_RUNTIME)}
  zlmax := 0;
  SetLength(zlist, DIYEN_LMAX_PROCESS+1);
  {$endif}

  {$ifdef ZLAYER}
  zlayer_max := -1; // Not initialized
  gInternal.zlayer_auto:=true;
  {$ifdef ZLAYER_RUNTIME}
  gInternal.use_zlayer:=false; // Not using zlayer
  {$endif}
  {$endif}

  {$ifdef USE_PRIORITY}
  SetLength(priolist, DIYEN_LMAX_PROCESS+1);
  priomax := 0;
  {$endif}

  SetLength(velementos, DIYEN_LMAX_PROCESS+1);

  // Inicializamos todos los valores con nil para marcarlos como que no existen
  for i:=0 to DIYEN_LMAX_PROCESS do
  begin
    velementos[i] := nil;
    {$if not defined(ZLAYER) or defined(ZLAYER_RUNTIME)}
    zlist[i] := 0;
    {$endif}
  end;
  //velementos[vmax] := nil;
end;

destructor TDiyenPList.Destroy;
var
  i : word;
begin
  // Destruimos/liberamos todos los process creados
  for i:=1 to DIYEN_LMAX_PROCESS do
  begin
    Del(i);
  end;

  SetLength(velementos, 0);
  SetLength(vultimos_borrados, 0);
  SetLength(startl, 0);
  SetLength(endedl, 0);
  SetLength(killl, 0);
  {$if not defined(ZLAYER) or defined(ZLAYER_RUNTIME)}
  SetLength(zlist, 0);
  {$endif}
  {$if defined(ZLAYER)}
  stop_zlayer();
  {$endif}

  {$ifdef USE_PRIORITY}
  SetLenght(priolist, 0);
  {$endif}
end;

// Arranca el programa principal
procedure TDiyenPList.ProgramStart(main: TMainProgram);
//procedure ProgramStart(main: TMainProgram);
begin
  (*if (process.process_type = PTYPE_UNDEFINED) then
  begin
    SDL_LogCritical(SDL_LOG_CATEGORY_ERROR, 'PROGRAM - process_type = PTYPE_UNDEFINED. Override the "process_type" function with "Result:=PTYPE_PROGRAM;".');
    Halt(SDL_LOG_CATEGORY_ERROR);
  end;*)

  main.flip  := SDL_FLIP_NONE;
  main.alpha := 255;
  main.blend := DIYEN_DEFAULT_BLENDMODE;
  main.color.r := 255;
  main.color.g := 255;
  main.color.b := 255;

  main.Internal.process_id := 0;

  main.resolution := 1;
  main.size    := 100;
  main.size_x  := 100;
  main.size_y  := 100;

  //process.Internal.signal    := S_NONE;
  main.Internal.status    := STATUS_RUNNING;

  {$ifdef DIYEN_PAUSE}
  gInternal.processing := true;
  {$endif}
  gInternal.title := 'DIYen';
  main.main;
  if (exit_status = 0) then exit_status := 1;
  main.Destroy;
end;


// Arranca un nuevo proceso, da valores iniciales.
procedure TDiyenPList.ProcessInit(process: TProcess; id, father: word; params: Pointer);
begin
  (*if (process.process_type = PTYPE_UNDEFINED) then
  begin
    SDL_LogCritical(SDL_LOG_CATEGORY_ERROR, 'process_type = PTYPE_UNDEFINED. Please, define your process type with overriding the "process_type" function.');
    Halt(SDL_LOG_CATEGORY_ERROR);
  end;*)

  //say('ClassName: '+process.ClassName+'; process_type: '+IntToStr(process.process_type));

  process.flip  := SDL_FLIP_NONE;
  process.alpha := 255;
  process.blend := DIYEN_DEFAULT_BLENDMODE;
  process.color.r := 255;
  process.color.g := 255;
  process.color.b := 255;
  {$ifndef ZLAYER}
  process.z     := -32767;
  {$else}
  process.z     := 0;
  {$endif}

  //process.Internal.col_filter := 0;

  process.Internal.process_id := id;

  //process.nosetvars := false;

  //process.Internal.smallbro :=
  //process.Internal.bigbro   :=
  //process.Internal.psmallbro :=
  //process.Internal.pbigbro   :=

  process.resolution := 1;
  //process.size    := 100;
  process.size_x  := 100;
  process.size_y  := 100;

  //process.Internal.signal    := S_WAKEUP;
  process.Internal.status    := STATUS_UNSTARTED;

  process.Internal.params   := params;

  process.father  := father;
  if (father > 0) then
  begin
    TP(process.father).son := id;

    //PrFather.Internal.smallson := id;
    if (TP(process.father).Internal.bigson <= 0) then
      TP(process.father).Internal.bigson := id;
    process.bigbro := TP(process.father).Internal.bigson;
  end;

  //process.Internal.pthread := SDL_CreateThread(@process_main, 'PROCESS', Pointer(process));
  //process.start; // Ahora arranca cuando se despierta por primera vez
end;

// Termina un proceso existente, lo finaliza y libera la memoria
procedure TDiyenPList.ProcessEnd(process: TProcess);
begin
  process.Internal.status := STATUS_DEAD;
  process.died;
end;

// Añade un proceso a la lista
function TDiyenPList.Add( process: TProcess; father: word; params: Pointer ): word;
var
  iborrado : word;
begin
  Result := 0; // Si no se pudo agregar devuelve 0
  // Primero nos fijamos si hay algun elemento borrado
  if vindice_uborrado > 0 then
  begin
    // Cual esta borrado?
    iborrado := vultimos_borrados[vindice_uborrado];
    Dec(vindice_uborrado); // Ya no esta mas borrado
    velementos[iborrado] := Pointer(process);
    ProcessInit(process, iborrado, father, params);
    Result := iborrado; // Este es el indice
  end else
  // No hay ningun elemento borrado entonces agregamos uno nuevo si
  // no estamos al tope de la lista
  if vmax < (DIYEN_LMAX_PROCESS-1) then // Uno menos asi se puede devolver nil en Next
  begin
    Inc(vmax);// vmax := vmax + 1; // Uno mas
    velementos[vmax] := Pointer(process);
    ProcessInit(process, vmax, father, params);
    Result := vmax; // Este es el indice
  end else
  begin
    // No pudimos agregarlo ya está repleta la lista (qué increíble)
    //raise
    SDL_Log(PChar('Error: Cannot create more processes. Limit ('+IntToStr(DIYEN_LMAX_PROCESS)+') reached!'));
    process.Destroy;
    Exit;
  end;

  // Lo agregamos a la lista para arrancar
  Inc(startl_max);
  startl[startl_max] := Result;

  // Si usamos priority
  {$ifdef USE_PRIORITY}
  if (pid <> 0) then
  begin
    Inc(priomax);
    priolist[priomax] := pid;
    (*if (process.priority >= gInternal.priority_min) then
      gInternal.priority_ordered := false
    else
      gInternal.priority_min := process.priority;*)
  end;
  {$endif}
end;

// Lo borra de la lista
procedure TDiyenPList.Del(index: word);
begin
  // Borramos el elemento pedido
  if (velementos[index] <> nil) then
  begin
    //ProcessEnd(TProcess(velementos[index]));
    TProcess(velementos[index]).Destroy;
    velementos[index] := nil;

      // Es el ultimo?
      if index = vmax then
      begin
      // Entonces ahora ya no es el ultimo (ya esta borrado)
      Dec(vmax); //vmax := vmax - 1;
        // Verificamos si los anteriores están borrados también
        // NOTA: No he notado que se gane mucha velocidad con esto.
        (*while (vindice_uborrado > 0) and
              (vultimos_borrados[vindice_uborrado] = vmax)
              do
        begin
          Dec(vindice_uborrado);
          Dec(vmax);
        end;*)
      end else
      begin
      // No es el ultimo entonces avisamos que lo borramos
      Inc(vindice_uborrado);
      vultimos_borrados[vindice_uborrado] := index; // Y fue este
      end;

    {$ifndef ZLAYER}
    // Hay que ordenar la lista sí o sí, no queda de otra.
    //gInternal.Z_comp_ordered := false;
    //some_deleted := true;
    {$endif}

    {$ifdef USE_PRIORITY}
    // Hay que ordenar la lista sí o sí, no queda de otra.
    gInternal.priority_ordered := false;
    //prio_some_deleted := true;
    {$endif}

  end;
end;

function TDiyenPList.Read(index: word): TProcess;
begin
  Result := TProcess(velementos[index]);
end;

(*procedure TDiyenPList.Write(index: word; process: TProcess);
begin
  velementos[index] := Pointer(process);
end;*)

// Devuelve el próximo process válido o nil si se llegó al final
function TDiyenPList.Next(index: Pword): TProcess;
begin
  repeat
  Inc(index^); //index^ := index^ + 1;
  Result := TProcess(velementos[index^]);
  until (index^ > vmax) or (velementos[index^] <> nil)
end;

(*
================
TdiyenRegionList
================

  Define la lista de regiones

*)
constructor TDiyenRegionList.Create();
begin
  SetLength(region, DIYEN_LMAX_REGIONS+1);
  SetLength(clip_region, DIYEN_LMAX_REGIONS+1);
  //for i := DIYEN_LMAX_REGIONS downto 0 do
    //regions[i].texture := nil;

  //alist_max := 0;
  //SetLength(alist, alist_max+1);

  // Definimos la región 0 por defecto
  //alist[0] := 0;
  // Se redefinirá automáticamente cada vez que
  // se cambie el modo de video
  region[0].w := 0;
  region[0].h := 0;
  region[0].x := 0;
  region[0].y := 0;
end;

destructor TdiyenRegionList.Destroy();
begin
  //for i := DIYEN_LMAX_REGIONS downto 0 do
    //if (regions[i].texture <> nil) then
      //SDL_DestroyTexture(regions[i].texture);
  SetLength(region, 0);
  SetLength(clip_region, 0);
  //SetLength(alist, 0);
end;

function TdiyenRegionList.Read( index: byte )  : PSDL_Rect;
begin
  Result := @region[index];
end;

// Con un algoritmo en el cual se utilicen texturas como destino para simular regiones,
// se podrían poner rotadas, espejadas o estiradas en pantalla.
// Por ahora preferí utilizar la función SetClip de SDL para simular una región ya
// que se obtiene exactamente el mismo resultado que en DIV.
// 20-11-2013: Igualmente, implementar lo anterior de rotado y demás sería incompatible
// con algunos dispositivos de video ya que no todos soportan render a textura según
// parece porque en mi tarjeta GT 640 no me funcionó?
// Aunque dicen por internet que supuestamente a partir de OpenGL 2 siempre
// se soporta render a textura.

procedure TdiyenRegionList.define_region( num: byte; x, y, ancho, alto, angulo : integer; flip : byte; estirada : boolean );
//var
  //l : integer;
  //inlist : boolean = false;
  //wscal: single = 1;
  //hscal: single = 1;
begin
  //if (num < 0) or (num > DIYEN_LMAX_REGIONS) then Exit;
  region[num].x := x;
  region[num].y := y;
  region[num].w := ancho;
  region[num].h := alto;

  //SDL_RenderGetScale(gInternal.renderer, @wscal, @hscal);
  clip_region[num].x := x;
  clip_region[num].y := y;
  clip_region[num].w := ancho;
  clip_region[num].h := alto;

  //regions[num].angle   := angulo;
  //regions[num].flip    := flip;
  //regions[num].stretch := estirada;

  // Check if already in list
  (*for l:=0 to alist_max do
    if (alist[l] = num) then
    begin
    inlist := true;
    break;
    end;
  if not inlist then
  begin
    Inc(alist_max);
    SetLength(alist, alist_max+1);
    alist[alist_max] := num;
    regions[num].texture :=
          SDL_CreateTexture(gInternal.renderer, SDL_PIXELFORMAT_ABGR8888,
          SDL_TEXTUREACCESS_STREAMING,
          regions[num].w, regions[num].h);
  end else
  begin

  end; *)
end;

procedure TdiyenRegionList.define_region( num: byte; x, y, ancho, alto : integer );
begin
  define_region(num, x, y, ancho, alto, 0, 0, false);
end;

function TdiyenRegionList.out_region(x, y, res : integer; num : byte) : boolean;
begin
  x := x div res;
  y := y div res;
  Result := true;

  if (x < region[num].x) then Exit;
  if (y < region[num].y) then Exit;
  if (x > (region[num].x+region[num].w)) then Exit;
  if (y > (region[num].y+region[num].h)) then Exit;

  Result := false;
end;

(*
================
TdiyenScrollList
================

  Define la lista de scrolls

*)
constructor TdiyenScrollList.Create();
begin
  SetLength(scroll, DIYEN_LMAX_SCROLLS+1);
  alist_max:=-1;
end;

destructor TdiyenScrollList.Destroy();
begin
  SetLength(scroll, 0);
  SetLength(alist, 0);
end;

function TdiyenScrollList.Read( index: word )  : TScroll;
begin
  Result := scroll[index];
end;

(*procedure TdiyenScrollList.Write(index: word; scroll: TScroll);
begin
  scrolls[index] := scroll;
end;*)


// Classic compliance start_scroll function
procedure TdiyenScrollList.start_scroll( scrollnumber : byte;
  fileID,
  graphID, backgroundgraphID, background2graphID : word;
  regionnumber : byte;
  lockindicator : TSetScrollLock );
var
  sc : PScroll;
  n : integer;
begin
  for n := 0 to alist_max do
    if alist[n] = scrollnumber then
    begin
      stop_scroll(scrollnumber);
      break;
    end;

  Inc(alist_max);
  SetLength(alist, alist_max+1);
  alist[alist_max] := scrollnumber;

  sc := @scroll[scrollnumber];
  sc^.file_    := fileID;

  sc^.graph0   := graphID;
  sc^.flip0    := SDL_FLIP_NONE;
  sc^.alpha0   := 255;
  sc^.blend0   := DIYEN_DEFAULT_BLENDMODE;
  sc^.color0.r := 255;
  sc^.color0.g := 255;
  sc^.color0.b := 255;
  sc^.x0       := 0;
  sc^.y0       := 0;

  sc^.graph1   := backgroundgraphID;
  sc^.ratio    := 200;
  sc^.flip1    := SDL_FLIP_NONE;
  sc^.alpha1   := 255;
  sc^.blend1   := DIYEN_DEFAULT_BLENDMODE;
  sc^.color1.r := 255;
  sc^.color1.g := 255;
  sc^.color1.b := 255;
  sc^.x1       := 0;
  sc^.y1       := 0;

  sc^.graph2   := background2graphID;
  sc^.ratio2   := 100;
  sc^.flip2    := SDL_FLIP_NONE;
  sc^.alpha2   := 255;
  sc^.blend2   := DIYEN_DEFAULT_BLENDMODE;
  sc^.color2.r := 255;
  sc^.color2.g := 255;
  sc^.color2.b := 255;
  sc^.x2       := 0;
  sc^.y2       := 0;

  sc^.sregion  := regionnumber;
  sc^.lock     := lockindicator;
  sc^.region1  := -1;
  sc^.region2  := -1;
  sc^.z        := 512;

end;


// DIV compliance start_scroll function
procedure TdiyenScrollList.start_scroll( scrollnumber : byte;
  fileID, graphID, backgroundgraphID : word;
  regionnumber : byte;
  lockindicator : TSetScrollLock );
begin
  start_scroll(scrollnumber,
  fileID, graphID, backgroundgraphID, 0,
  regionnumber, lockindicator );
end;

procedure TdiyenScrollList.stop_scroll( scrollnumber: byte );
var
  sn : byte;
  n : byte;
begin
  for sn := 0 to alist_max do
    if alist[sn] = scrollnumber then
    begin
      if (alist_max > 0) then
      for n := sn to alist_max-1 do
        alist[n] := alist[n+1];
      Dec(alist_max);
      break;
    end;

  SetLength(alist, alist_max+1);
end;

// DIV move_scroll compliant
procedure TdiyenScrollList.move_scroll ( scrollnumber : byte );
var
  sc : PScroll;
  cam : TProcess;
  reg, reg1, reg2 : TSDL_Rect;
  scamx, scamy : integer;
  camx, camy : integer;
  map : TMap;
  speedx, speedy : integer;
  cpoint : PSDL_Point;
begin
  sc := @scroll[scrollnumber];
  if (sc^.camera > 0) then
  begin
    cam := TP(sc^.camera);
    if (sc^.region1 > -1) then reg1 := SREG(sc^.region1);
    if (sc^.region2 > -1) then reg2 := SREG(sc^.region2);

    camx := (cam.x div cam.resolution);
    camy := (cam.y div cam.resolution);
    reg := SREG(sc^.sregion);

    // FIXME: por qué w := w + x??
    // FIXME: Creo que todo esto está mal planteado
    if (sc^.region1 > -1) or (sc^.region2 > -1) then
    begin
      reg1.w := reg1.w+reg1.x;
      reg1.h := reg1.h+reg1.y;

      scamx := camx + (reg.x-sc^.x0);
      scamy := camy + (reg.y-sc^.y0);

      if (sc^.speed <> 0) and (sc^.region1 > -1) then
      begin
        speedx := 0;
        if (scamx < reg1.x) then Dec(speedx, reg1.x-scamx);
        if (scamx > reg1.w) then Inc(speedx, scamx-reg1.w);
        if (speedx > sc^.speed) then speedx := sc^.speed;
        if (speedx < -sc^.speed) then speedx := -sc^.speed;
        Inc(sc^.x0, speedx);

        speedy := 0;
        if (scamy < reg1.y) then Dec(speedy, reg1.y-scamy);
        if (scamy > reg1.h) then Inc(speedy, scamy-reg1.h);
        if (speedy > sc^.speed) then speedy := sc^.speed;
        if (speedy < -sc^.speed) then speedy := -sc^.speed;
        Inc(sc^.y0, speedy);
      end;

      if  (sc^.region2 > -1) then
      begin
        reg2.w := reg2.w+reg2.x;
        reg2.h := reg2.h+reg2.y;

        if (scamx < reg2.x) then sc^.x0 := camx-reg2.x+reg.x;
        if (scamx > reg2.w) then sc^.x0 := camx-reg2.w+reg.x;
        if (scamy < reg2.y) then sc^.y0 := camy-reg2.y+reg.y;
        if (scamy > reg2.h) then sc^.y0 := camy-reg2.h+reg.y;
      end;
    end else
    begin
      sc^.x0 := camx - reg.w div 2;
      sc^.y0 := camy - reg.h div 2;
    end;

    if (sc^.graph0 > 0) then
    begin
      map := TMap(TFPG(diyen_fpg_list.fpg[sc^.file_]).maps[sc^.graph0]);
      cpoint := @map.cpoint[0];
      if not (SN_H in sc^.lock) then
      begin
        if (sc^.x0 < cpoint^.x) then sc^.x0 := cpoint^.x;
        if (sc^.x0 > cpoint^.x+map.width-reg.w ) then sc^.x0 := cpoint^.x+map.width-reg.w;
      end;
      if not (SN_V in sc^.lock) then
      begin
        if (sc^.y0 < cpoint^.y) then sc^.y0 := cpoint^.y;
        if (sc^.y0 > cpoint^.y+map.height-reg.h ) then sc^.y0 := cpoint^.y+map.height-reg.h;
      end;
    end;

    if (sc^.graph1 > 0) then
    begin
      sc^.x1 := (sc^.x0 * 100) div sc^.ratio;
      sc^.y1 := (sc^.y0 * 100) div sc^.ratio;

      map := TMap(TFPG(diyen_fpg_list.fpg[sc^.file_]).maps[sc^.graph1]);
      cpoint := @map.cpoint[0];

      if not (SN_BH in sc^.lock) then
      begin
        if (sc^.x1 < cpoint^.x) then sc^.x1 := cpoint^.x;
        if (sc^.x1 > cpoint^.x+map.width-reg.w ) then sc^.x1 := cpoint^.x+map.width-reg.w;
      end;
      if not (SN_BV in sc^.lock) then
      begin
        if (sc^.y1 < cpoint^.y) then sc^.y1 := cpoint^.y;
        if (sc^.y1 > cpoint^.y+map.height-reg.h ) then sc^.y1 := cpoint^.y+map.height-reg.h;
      end;
    end;

    if (sc^.graph2 > 0) then
    begin
      sc^.x2 := (sc^.x0 * 100) div sc^.ratio2;
      sc^.y2 := (sc^.y0 * 100) div sc^.ratio2;

      map := TMap(TFPG(diyen_fpg_list.fpg[sc^.file_]).maps[sc^.graph2]);
      cpoint := @map.cpoint[0];
      if not (SN_B2H in sc^.lock) then
      begin
        if (sc^.x2 < cpoint^.x) then sc^.x2 := cpoint^.x;
        if (sc^.x2 > cpoint^.x+map.width-reg.w ) then sc^.x2 := cpoint^.x+map.width-reg.w;
      end;
      if not (SN_B2V in sc^.lock) then
      begin
        if (sc^.y2 < cpoint^.y) then sc^.y2 := cpoint^.y;
        if (sc^.y2 > cpoint^.y+map.height-reg.h ) then sc^.y2 := cpoint^.y+map.height-reg.h;
      end;
    end;

  end;
end;


{$ifdef DIYEN_PAUSE}
function TProcess.GetProcessing(): Boolean;
begin
  Result := gInternal.processing;
end;

procedure TProcess.SetProcessing( process : boolean );
begin
  gInternal.processing:=process;
end;

{$endif}


{$ifdef ZLAYER}
procedure start_zlayer( zmax: integer );
var
  z, i: integer;
begin

  if zmax > 0 then
  begin
  gInternal.zlayer_auto := false;
    with diyen_process_list do
    begin
      zlayer_max:=zmax-1;
      SetLength(zlayer, zmax);
      for z := 0 to zlayer_max do
      begin
        SetLength(zlayer[z].p, DIYEN_LMAX_PROCESS+1);
        zlayer[z].max:=0;
        for i := 1 to DIYEN_LMAX_PROCESS do
          zlayer[z].p[i] := 0;
      end;
    end;
  end else
  begin
    gInternal.zlayer_auto := true;
  end;
  {$ifdef ZLAYER_RUNTIME}
  gInternal.use_zlayer:=true;
  {$endif}
end;
procedure stop_zlayer();
var
 z: integer;
begin
  with diyen_process_list do
  begin
    for z:=0 to zlayer_max do
      SetLength(zlayer[z].p, 0);
    zlayer_max:=-1;
    SetLength(zlayer, 0);
  end;
  {$ifdef ZLAYER_RUNTIME}
  gInternal.zlayer_auto := false;
  gInternal.use_zlayer:=false;
  {$endif}
end;
procedure zlayer_automax( maxz: integer );
var
 from : integer;
 z : integer;
 i : integer;
begin
  with diyen_process_list do
  begin
  from := zlayer_max+1;
  zlayer_max := zlayer_max+maxz+1;
    SetLength(zlayer, zlayer_max+1);
    for z := from to zlayer_max do
    begin
      SetLength(zlayer[z].p, DIYEN_LMAX_PROCESS+1);
      zlayer[z].max:=0;
      for i := 1 to DIYEN_LMAX_PROCESS do
        zlayer[z].p[i] := 0;
    end;
  end;
end;

{$endif}


{$ifndef NO_FX_ENGINE}
procedure TProcess.fx_add(fxfunc: TFXfunction; count: word; data: Pointer);
begin
  with Internal do
  begin
  Inc(fx_max);
  SetLength(fxlist, fx_max);
  if count = 0 then count := 1;
  fxlist[fx_max-1].count:=count;
  fxlist[fx_max-1].fxfunction:=fxfunc;
  fxlist[fx_max-1].data:=data;
  end;
end;
procedure TProcess.fx_add(fxfunc: TFXfunction; data: Pointer);
begin
  fx_add(fxfunc, 1, data);
end;
procedure TProcess.fx_add(fxfunc: TFXfunction);
begin
  fx_add(fxfunc, 1, nil);
end;

procedure TProcess.fx_del();
begin
    SetLength(Internal.fxlist, 0);
    Internal.fx_max := 0;
end;

procedure TProcess.fx_del(index: word);
begin
  with Internal do
  if index < fx_max then
  begin
  fxlist[index] := fxlist[fx_max-1];
  SetLength(fxlist, fx_max);
  Dec(fx_max);
  end;
end;

procedure fx_run(p: TProcess; params: PFXparams; num: word; sc: PScroll);
var
 fx: PFX;
 FXparams: TFXparams;

 map : TMap;
 dstrect : TSDL_Rect;
 center : TSDL_Point;

 reg: TSDL_Rect;

 PRsrc: PSDL_Rect = nil;
begin
  fx := PFX(@p.Internal.fxlist[num]);
  params^.data := fx^.data;
  params^.n := 0;
  params^.count := fx^.count;
  params^.user1 := @fx^.user1; params^.user2 := @fx^.user2; params^.user3 := @fx^.user3;
  while params^.n < params^.count do
  begin
    Inc(params^.n);
    fx^.fxfunction(params^);
    if p.Internal.fx_max > num+1 then
    begin
      (*FXparams.x      := params^.x;
      FXparams.y      := params^.y;
      FXparams.size_x := params^.size_x;
      FXparams.size_y := params^.size_y;
      FXparams.angle  := params^.angle;
      FXparams.file_  := params^.file_;
      FXparams.graph  := params^.graph;
      FXparams.alpha  := params^.alpha;
      FXparams.color  := params^.color;
      FXparams.blend  := params^.blend;
      FXparams.flip   := params^.flip;
      {$ifndef FX_NO_RECT_SRC}FXparams.Rsrc   := params^.Rsrc;{$endif}*)
      FXparams := params^;
      fx_run(p, @FXparams, num+1, sc);
    end else
    begin
      map := TMap(TFPG(diyen_fpg_list.fpg[params^.file_]).maps[params^.graph]);

      center.x := longint((map.cpoint[0].x * params^.size_x) div 100);
      center.y := longint((map.cpoint[0].y * params^.size_y) div 100);
      if sc = nil then
      begin
        dstrect.x := (params^.x div p.resolution) - center.x;
        dstrect.y := (params^.y div p.resolution) - center.y;
      end else
      begin // Estamos en un scroll
        reg := regions[sc^.sregion]^;

        dstrect.x := (params^.x div p.resolution) + (reg.x-sc^.x0-center.x);
        dstrect.y := (params^.y div p.resolution) + (reg.y-sc^.y0-center.y);
      end;



      {$ifndef FX_NO_RECT_SRC}
      if (params^.Rsrc.w > 0) and (params^.Rsrc.h > 0) then
      begin
        if params^.size_x <> 100 then
          dstrect.w := longint((params^.Rsrc.w * params^.size_x) div 100)
        else
          dstrect.w := longint(params^.Rsrc.w);

        if params^.size_y <> 100 then
          dstrect.h := longint((params^.Rsrc.h * params^.size_y) div 100)
        else
          dstrect.h := longint(params^.Rsrc.h);

        PRsrc := @params^.Rsrc;
      end else
      {$endif}
      begin
        // Ahorramos velocidad al aplicar tamaño
        if params^.size_x <> 100 then
          dstrect.w := longint((map.width * params^.size_x) div 100)
        else
          dstrect.w := longint(map.width);

        if params^.size_y <> 100 then
          dstrect.h := longint((map.height * params^.size_y) div 100)
        else
          dstrect.h := longint(map.height);
      end;

      SDL_SetTextureAlphaMod(map.texture, byte(params^.alpha));
      SDL_SetTextureColorMod(map.texture, params^.color.r, params^.color.g, params^.color.b);
      SDL_SetTextureBlendMode(map.texture, params^.blend);

      // Put the graphic
      if ((params^.angle mod 360000) = 0) and (params^.flip = SDL_FLIP_NONE) then
        SDL_RenderCopy( gInternal.renderer,
                        map.texture,
                        PRsrc, @dstrect)
      else
        begin
          SDL_RenderCopyEx( gInternal.renderer,
                            map.texture,
                            PRsrc, @dstrect,
                            params^.angle/1000,
                            @center,
                            params^.flip);
        end;
    end;
  end;
end;

function TProcess.particle_spawn(particleZ: smallint; particlenum: word): word;
begin
  Result := particle_spawn2(x, y, angle, particleZ, 100, resolution, ctype, particlenum);
end;
function TProcess.particle_spawn(particleZ, particleSize: smallint; particlenum: word): word;
begin
  Result := particle_spawn2(x, y, angle, particleZ, particleSize, resolution, ctype, particlenum);
end;

{$endif}


end.

