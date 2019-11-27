   Changelog básico:
   03-04-2019
    * 18:40  Tiraba error en la función TMix_Func y eso que probé de muchas
             maneras. Creo que estuve como 4 horas tratando de descubrir
             cómo es que daba ese error. Y era porque hacía checkeo de pila
             ( directiva -Ct ).
   02-04-2019
    * 00:46  Arreglos menores para Android.
             Incorporación de manejo de menú con el Stick (axis)
   29-03-2019
    * 01:33  Se arregló near_angle();
   28-03-2019
    * 00:30  Se modificó set_mode() para que funcione bien con la OUYA.
   20-03-2019
    * 17:00  Hace unas horas se terminó el "Kit de desarrollo".
    * 18:19  Se mejoró el algoritmo para frameskip.
   19-03-2019
    * 10:15  Se cambió start_tilemap. Ahora reinicializa los valores del
             tilemap excepto el ancho alto filas y columnas.
             Se hace así por una cuestión de comodidad.
    * 11:50  Mejorado de ReadableConfig a un formato tipo INI. Ahora sí es
             más "readable".
   18-03-2019
    * 18:06  Se eliminó free_tileset para uso del usuario (ahora se usa sólo
             internamente). Ya que sólo basta con stop_tilemap() para liberar
             todo.
   17-03-2019
    * 17:54  Corregido start_tilemap y similares que no liberaban los tileset.
   14-03-2019
    * 09:41  Implementado frame skip.
   13-03-2019
    * 02:07  Los días anteriores se corrigieron algunos detalles en particles.
   09-03-2019
    * 22:18  Se hizo uso de STATUS_STARTED para no perder el primer fotograma.
   08-03-1019
    * 01:05  Se creó una clase especial para los tiles (TTile) con un par
             de variables útiles.
   04-03-2019
    * 08:00  Se agregó from_..., to... como variables en el record tilemap
             para establecer desde donde hasta donde graficar y procesar los
             tilemaps. Esto nos permite tener miles de tilemaps fuera de
             pantalla sin tenerse que procesar.
   02-03-2019
    * 00:37  Sólo se cambió para usar clear por defecto.
   19-02-2019
    * 07:25  Se corrigió load_map_image que no extraía el filename.
   03-01-2019
    * 04:26 Arreglado diyen_particles para considerar pausa (processing).
    * 04:05 Reparado rand que nunca alcanzaba el valor máximo (si el mínimo)
   02-01-2019
    * 19:43 Casi listo el sistema de partículas (bastante lindo quedó).
   27-12-2018
    * 15:13 Corregido el Random
   21-12-2018
    * 02:58 Creada función random rápida.
            Avance en diyen_particles. Al menos ahora se pueden cargar y
            crear partículas. Faltan las fx_
   07-12-2018
    * 21:24 Establecidos valores por omisión para los DEFINE.
   26-11-2018
    * 14:59 Corregida move_scroll que seguía la cámara después de haber dibujado
            los scrolls (y tenía que seguirla antes de dibujar).
            Incorporado prototipo de start_scroll con background2graphID.
    * 09:31 Arreglado de la función fget_dist para Android.
    * 01:33 Terminado salvar y cargar mapeo de teclas.
            Ayer se terminó de acomodar fget_angle.
   22-11-2018
    * 04:03 Planteado correctamente del los touch en pantalla y terminado
            de los mismos.
    * 00:52 Se mejoró y se terminó de implementar el sistema de elementos
            touch para pantallas táctiles.
   21-11-2018
    * 09:03 Se reparó la función fget_angle()
            Se estableció TDraw y TMouse para usar PInteger e Integer.
            Se implementó Rsrc para los FXs con lo que se pueden tener
            control sobre el origen de la textura. Claro, está limitado el uso
            de colisiones, pero justamente sirve para hacer efectos visuales
            que no requieren colisión (por ejemplo: Barra de energía).
   20-11-2018
    * 23:22 Ayer se arreglaron las funciones de put, put_screen, clear_color...
            Se cambió "paused" por "processing" ya que define mejor lo que hace.
            Se cambió la nominación de FIXCUAD por AA_SQUARE y FIXRECT por
            AA_RECT que define mejor ese tipo de colisión.
   17-11-2018
    * 04:41 Arreglado de listas de texto y de draw (no estaba del todo
            contemplado cual sería el primer índice de la lista)
    * 02:18 Implementación básica (pero más que suficiente) de draw.
   16-11-2018
    * 23:26 Implementación de mouse (coordenadas y botones).
            Algunas correcciones al setear el modo de video (sobretodo
            con respecto a la escala).
    * 02:49 Se corrigió el seguimiento de cámara en scroll_move.
            el width estaba como witdh := witdh + x
            No sé por qué si en realidad el width es el width, nada tiene
            que ver con un desplazamiento, porque es un tamaño!
   15-11-2018
    * 22:44 Se está acomodando diyen_input
            Se hizo funcionar como App para Android.
            Se corrigió set_mode_auto para Android.
   14-11-2018
    * 22:46 Arreglado salida de error más legible al cargar mapa.
    * 18:55 Días atrás terminé el FX engine.
            Se solucionó el problema de desaparición de disparos en CoreSpace.
            Se debía a que el proceso era analizado para colisión apenas era
            incluido en la lista.
            Anteayer se introdujo algo de diyen_particles
            Ahora se está acomodando para Android
   10-11-2018
    * 02:34 Se terminó el modo Z con capas (ZLAYER).
   09-11-2018
    * 18:22 Se terminó de acomodar todo en archivos separados para simplificar
            el desarrollo.
            Se cambió "main" por "loop" en los procesos y se diferenció mejor
            TMainProgram para evitar confusiones.
   07-11-2018
    * 22:47 Replanteado todo el sistema con listas z (zlist).
    FIXME: En CoreSpace al disparar continuo se borra a veces un disparo.
   05-11-2018
    * 16:43 Corregido a usar los process.id en lugar de tclass para
            la función exists(), ya que se evita el uso de punteros que
            normalmente quedaban apuntando a nada (ej: cuando un proceso
            era eliminado no había forma de saberlo ya que nuestro puntero
            apuntaba a un área de memoria ilegal).
    * 02:17 Eliminación de ptype por completo. El mismo FPC permite
            usar una clase tipo variable para poder comparar la class.
    * 00:14 El error gráfico de dibujar con más alfa no era más que
            el start_scroll sin su stop_scroll correspondiente. Lo que
            dibujaba X veces lo mismo.
            Ahora la función start_scroll analiza si ese scroll ya
            fue iniciado y lo detiene automáticamente para volver
            a crearlo de nuevo.
   04-11-2018
    * Hay un error gráfico que hace dibujar cada vez con más ¿alpha?
      Desconozco la maldita causa pero es muy molesto.
      Sucede cuando no cierras el programa y vuelve a crear los
      procesos. No sé qué estará fallando, pero hay algo que "pinta"
      cada vez más fuerte las cosas.
    * Se cambiaron las funciones para eliminar el uso de ID.
      Ahora se usa directamente el tipo TProcess.
    * Ayer:
      - Arreglos en colisión (no contemplaba cuando un proceso estaba
        marcado para eliminar)
    * Hace unos días:
      - alpha ahora es smallint para permitir adquirir números negativos y
        poder poner los límites en el código.


   04-02-2017
    * Corregido para uso de Z computado. Antes no actualizaba la lista ordenada
      de Z para graficar aunque cambiábamos Z de un proceso.
   03-02-2017
    * Terminado el modo de tiles.
   01-02-2017
    * Comienzo de implementación de tiles (baldosas).
   31-01-2017
    * Corregido un range check error en stop_scroll
    * Arreglado de la forma de manejar el fondo de pantalla
   30-01-2017
    * Solo agregué la función move_text() ya que es práctica.
    * Cambié a SDL_WINDOW_FULLSCREEN_DESKTOP. Requiere al menos SDL 2.0.5.
    * Arreglo de funciones para graficado de fondo (put, put_screen).

   23-12-1013
    * Modificado el TSDL_RendererFlip por un simple valor de byte.
      Ya que el comportamiento de C es distinto al de Free Pascal, y
      los types enum del C se permiten manipular más libremente. En
      Pascal estamos forzados a sólo establecer valores que estén
      dentro del "set of" y no se puede convertir el tipo.
    * Implementado el modo de scroll por software.
    * Corregidas las regiones, especialmente move_scroll que consideraba
      el ancho y el alto como la otra esquina (no como ancho y alto).
      También se revisaron las clip_regions.
    * Implementado el "offset" en los textos.
   22-12-2013
    * Se actualizó a SDL 2.0.1 y se combinó el código de SDLActivity.java
    * Invertida la lista de joysticks ya que SDLActivity la tiene invertida
      alegando a que se debe a una compatibilidad con XBox 360.
      La OUYA invirtió los controles (no sé por qué) sin embargo
      en sus luces los indicaba correctamente. Al mismo tiempo que había
      creado un código erróneo para inventir la lista.

   21-12-2013
    * Plasmada la idea del motor de efectos especiales, pero aún no se ha
      creado código para ello.
   19-12-2013
    * Agregado vocales acentuadas mayúsculas y la U con diéresis a la tabla
      de caracteres.
    * Corregido error de overflow en el for de detección de colisión para los
      scrolls.
   15-12-2013
    * Arreglada un poco la documentación online.
    * Añadido análisis de colisión en el graph0 de los scrolls.
   14-12-2013
    * Arreglado parte del problema con los modos de video usando tamaños menores
      de pantalla de destino que de origen.
      Aún falla poniendo algunos tamaños. Pero creo que es suficiente para
      funcionar bien en Android.
   08-12-2013
    * Agregado el modo AUTO_PTYPE.
    * Actualizado, corregido y terminado el ejemplo RemioBroders.
   01-12-2013
    * Corregido un error al añadir un proceso que no reordenaba la lista.
      En lugar de comparar con el anterior añadido, comparaba con el anterior
      siendo analizado. Pero en realidad como se añade al final de la lista
      debía comprar con el anterior del final de la lista.
   29-11-2013
    * Faltaba stop_song() y fue creada.
    * Creando el ejemplo de Remio.
   28-11-2013
    * Después de sufrir las consecuencias del estrés, continúo con el proyecto.
    * Arreglado el problema de detección de colisión para rectángulos rotables.
    * Correcciones en calculos de colision de rectángulo con círculo.
    * Se corrigió un error en las regions_clip al setear el modo de video
      (no sumaba el desplazamiento X).
    * Realizado algunas correcciones: limpiado de código con variables sin uso.
    * Corregido error al poner gráfico de scroll del frente.
   20-11-2013
    * Terminado el escaler (pero no se está usando) y el modo automático
      de pantalla (este se está usando).
    * Despertado a las 5 AM con desorientación (no es broma).
   19-11-2013
    * Concluido un sistema de scaler util para Android.
    * Corrección completa de la función near_angle
    * Concluido el sistema de sonido, funciones de música y sonido.
    * Concluidas las listas, se puede cargar y descargar recursos.
    * Corregido un error en la función de input (el for no comenzaba
      por el 0 y no se seteaban las input por defecto.
    * Concluido stop_scroll
    * Se adicionaron variables software_mode y force_accel para iniciar
      el video.
    * Corrección de la función de set_mode (todavía hay que revisarla bien).
    * Terminados los tipos de TFont y TTexto.
    * Terminadas las listas de fuentes y texto. Y sus funciones para cargar
      y graficar.
   18-11-2013
    * Creación del visualizador y probador de fuentes.
    * Creación de un generador de fuentes.
   17-11-2013
    * Se crearon imagenes para la web wiki y se arreglaron algunos artículos.
   16-11-2013
    * Concluido el sistema de señales.
    * Un par de días hechos de la documentación en línea: diyen.artintec.com.ar
    * Terminado out_region y near_angle.
   14-11-2013
    * Arreglos para modo Full Screen en PC (no android).
   13-11-2013
    * Terminado de las colisiones y hit (no se revisó si están todas las
      combinaciones, aún falta punto por ejemplo).
      Terminado soporte para flip.
    * Concluido el uso de joystick y funciones para entrada diyen_input
      (aún falta poder salvar y cargar la configuración).
    * Concluido el modo scroll con soporte completo en sus capas.
      (aún falta la función para borrar scrolls).

   11-11-2013
    * Concluido el modo scroll con USE_Z
    * Ajustado del modo con banderas de compilación
   07-11-2013
    * Concluida la creación de regiones (aún falta out_region).
    * Ajustes a TFPG (cambiada su nominación) y posible comienzo de
      creación de un formato de archivo propio.
    * Corrección de la forma de funcionamiento de frame_percent
   06-11-2013
    * Concluido el sistema de "struct" salvables.
      Aún queda por probarlo!
    * Algunos ajustes a Maps (posible comienzo de creación de
      un formato de archivo propio).
   05-11-2013
    * Inicio creación diyen_input. Aún no concluído.



