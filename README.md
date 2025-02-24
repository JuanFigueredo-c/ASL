# Trabajo Práctico Final - ASL

**Análisis de Lenguajes de Programación**


Juan Bautista Figueredo

## Dependencias y compilación.

Para el manejo de los gráficos, el lenguaje utiliza la librería externa [Gloss](http://gloss.ouroborus.net/).
Por lo tanto, para el uso del lenguaje se debe instalar la librería mediante Cabal.
Se puede ejecutar el siguiente comando en la terminal para lograr esta tarea

    $ sudo cabal install gloss

En algunos casos, será necesario instalar freeglut para poder utilizar gloss. 

    $ sudo apt-get install freeglut3-dev

Puede ocurrir que al momento de la instalación ocurran problemas con la instalación de OpenGLRaw. Una solución puede ser instalar algunas librerías de
C necesarias para su construcción. Estas son:

    $ sudo apt install libgl-dev
    $ sudo apt install libglu-dev

Para el parser, el lenguaje utiliza [Happy](https://haskell-happy.readthedocs.io/en/latest/). Es necesario tener happy instalado. Esto se puede lograr con alguno de los siguientes comandos:

    $ sudo apt install happy
    $ sudo cabal install happy

La implementación del lenguaje utilza Stack.
El primer paso para utilizar el lenguaje preparar el entorno. Para esto se debe 
ejecutar el siguiente comando:

    $ stack setup

Para compilar el lenguaje se pueden utilizar los siguientes comandos:
    
    $ make asl
    $ stack build-silent

Para ejecutar un programa 'programa.asl' se puede hacer de la siguiente manera:

    $ make run programa.asl
    $ stack exec -- asl-exe programa.asl

## Uso del lenguaje

El lenguaje está diseñado para utilizarse mediante scripts que definen el comportamiento de 
un animación.
Una script de ASL debe seguir un formato en particular: Debe comenzar con una declaración de escena y debe finalizar con una sentencia **Play**.

### Comentarios

El lenguaje cuenta con comentarios de una sola línea. Un comentario se ve de la siguiente manera:

    -- Esto es un comentario

    i : Image = rect 30.0 40.0 0.0 full blue -- Esto también es un comentario
    
### Escena
Una animación se desarrolla sobre una "escena", o "pantalla". Para toda animación se debe definir una escena, 
en donde se especifica las dimensiones de la misma y el color.
Una definición de escena debe preceder a toda otra sentencia en el script y se define de la siguiente manera:

    scene ancho alto color

### Sentencias de ASL
Las sentencias en el lenguaje se dividen en dos categorías: Las declaraciones y los comandos.
Por un lado, las declaraciones sirven para definir variables y asignarles distintos valores.
Los comandos serán necesarios para poder armar la secuencia de ejecución de la animación y
posteriormente, ejecutarla.

### Declaraciones
Una declaración se compone de 3 partes: El nombre de la variable a crear, el tipo de la variable y la expresión a asignar. 
Una declaración se ve de la siguiente manera:

    nombre : tipo = expresión

Los nombres de las variables son únicos a lo largo del programa y pueden contener caracteres alfanúmericos y guiones bajos y medios. 
No pueden comenzar con números. Los tipos disponibles en el lenguaje son: 
    
    - 'Color'  : Utilizado para definir colores
    - 'Image'  : Utilizado para definir imagenes
    - 'Action' : Utilizado para definir acciones
    - 'Anim'   : Utilizado para definir animaciones

Las expresiones permiten definir los componentes que utilizaremos para nuestra animación. Estas expresiones 
pueden construir elementos de los 4 tipos anteriormente mencionados. 
Veamoslas más en profundidad.

### Expresiones
Las expresiones pueden servirnos para construir colores, imagenes, acciones y animaciones. 
### Expresiones de Colores
Un color se define mediante 3 valores numéricos enteros entre 0 y 255. Cada valor representa la cantidad de color rojo, verde y azul respectivamente.
Con esto en mente, la sintaxis para la construcción de un color es:

    #r,g,b

En donde 'r', 'g' y 'b' son enteros entre 0 y 255.
Por ejemplo el color negro se representa como la ausencia de todos los colores, es decir

    #0,0,0

Mientras que el blanco es la suma de todos los colores:

    #255,255,255

Si queremos representar un color naranja podemos definirlo de la siguiente manera

    #229,150,32

Si un campo excede los 255 se utilizará el valor 255 por defecto. Del mismo modo si es menor a 0, se reemplazará por 0.

### Tipos numéricos
En ASL se encuentran 2 tipos numéricos, los numeros flotantes y los numeros enteros. Ambos tienen una sintaxis diferente.
Un número será entero si no tiene parte decimal, es decir, se escribe sin **'.'** \
Por otro lado para que un número sea identificado como flotante se debe escribir con un **'.'** separando la 
parte entera de la parte decimal. Esto se debe respetar aún si la parte decimal es 0.\
Por esto, el número *4* es un número entero mientras que *4.0* es un número flotante. \
Para referirnos a los tipos numéricos utilizaremos '**Int**' para los enteros y '**Float**' para los flotantes

### Expresiones de Imágenes
Los objetos base del lenguaje son las imágenes. Una imagen se define a partir de constructores primitivos y dos imagenes
pueden operarse entre si para conseguir una nueva imagen. \
Veamos las primitivas de imagenes:

    circle  Radio::Float Espesor::Float  Modo  Color
Crea un circulo con el radio especificado y color especificado.

    rect  Ancho::Float  Alto::Float  Espesor::Float  Modo  Color
Crea un rectángulo con ancho, altura y color especificado.

    triang  Lado1::Float  Lado2::Float  Lado3::Float Color
Crea un triangulo con lados y color especificados.
Si los lados no cumplen con la desigualdad triangular, el triangulo no se genera.

    poly Lados::Int Longitud::Float Color

Crea un polígono regular con **Lados** cantidad de lados y cada uno es de longitud **Longitud**. El polígono resultante es del color especificado.

    line  Largo::Float  Angulo::Float  Espesor::Float  Color
Crea una linea con largo, angulo, espesor y color especificados.

En donde ***Modo*** tiene dos posibles valores: 

- **full**: Define una imagen rellena, el espesor se descarta.
- **outline**: En donde la imagen es solo el contorno y tiene el espesor indicado. 

y ***Color*** refiere a una expresion de color o una variable de tipo color definida previamente.

Ahora veamos las operaciones entre imagenes:

    stack  Imagen1::Image  Imagen2::Image

Compone dos imagenes, ubicando la **Imagen1** *sobre* la **Imagen2** de manera concéntrica.

    offset  Imagen1::Image Imagen2::Image { offset_x::Float , offset_y::Float }
Compone dos imagenes, ubicando la **Imagen2** *sobre* la **Imagen1**, ubicando el centro de la 
**Imagen2** separada en **offset_x** horizontalmente y **offset_y** verticalmente del centro de la **Imagen1**. El centro de la imagen resultado coincide con el centro de la **Imagen1**

    bind  Imagen1::Image { x1::Float , y1::Float } Imagen2::Image { x2::Float , y2::Float }
Compone dos imagenes, ubicando la **Imagen2** sobre la **Imagen1** pero desfazando los centros de ambas imagenes de una distancia horizontal de **x1** para la **imagen1** y **x2** para la **Imagen2**, y una distancia vertical de **y1** para la **imagen1** y **y2** para la **Imagen2** del nuevo centro de la imagen resultado.

    rot Imagen::Image Rot::Float

Rota la imagen **Imagen** en **Rot** grados, en dirección horaria.

    resize Imagen::Image Factor::Float

Reescala la imagen por un factor **Factor** en los ejes X e Y.

    paint Imagen::Image Color

Cambia el color de la imagen por el color especificado.

### Expresiones de Acciones

Las acciones son la escencia de una animación ya que permiten mover nuestras imagenes. En ASL las animaciones no estan ligadas a ninguna imagen, sino que simplemente describen un movimiento y el tiempo que este debe durar. Una vez formada, una accion puede aplicarser varias veces sobre una misma animación. \
Las acciones, como las imágenes, se construyen a partir de primitivas y exiten operadores que permiten combinar acciones. \
Podemos separar a las acciones primitivas en dos tipos: 

- Acciones de **Traslación**, que cambian la posición de una imagen.

- Acciones de **Tranformación**, que cambian características como escala y rotación. 

Veamos las acciones primitivas de **Traslación**:

    move  { x::Float , y::Float}  Duracion::Float

Describe un movimiento en linea recta a el punto **( x , y )** de la escena. El movimiento debe tomar **Duracion** segundos en completarse, por lo que la velocidad del movimiento variará según la posición actual del objeto al momento de ejecutarse la acción.

    orbit { x::Float , y::Float} Angulo::Float Duracion::Float

Describe un movimiento de rotación de la imagen con centro en el punto **( x , y )** de la escena, trazando un ángulo de **Angulo** grados y durante **Duracion** segundos. La órbita es con dirección antihoraria.

Ahora, veamos las acciones primitivas de **Transformación**:

    rotate Angulo::Float Duracion::Float

Rota una imagen una cantidad de grados **Angulo** en un tiempo **Duracion**. La rotación se hace en el sentido del reloj.

    scale FactorX::Float FactorY::Float Duracion::Float

Escala la imagen por un factor **FactorX** en el eje X y un factor **FactorY** en el eje Y , durante **Duracion** segundos.

    static Duracion::Float

No describe ningún movimiento, la imagen conserva su estado por **Duracion** segundos.

Estas primitivas se pueden operar con los siguientes operadores:

    Accion1::Action ; Accion2::Action

El operador infijo ' **;** ' realiza una *secuenciación* de acciones, es decir, una imagen hará la **Accion1** e inmediatamente después realizará la **Accion2**. La duración de la acción resultante es la suma de la duración de ambas acciones.

    loop Accion::Action Iter::Int

Ejecuta de manera secuencial la acción **Accion** exactamente **Iter** veces. El tiempo de duración de la accion resultante es la duración de **Accion** multiplicada **Iter** veces.

    Accion1::Action || Accion2::Action

El operador infijo ' **| |** ' realiza una *paralelización* de acciones, es decir, una imagen hará la **Accion1** al mismo tiempo que hará **Accion2**. \
Este operador tiene algunas reglas para su uso:

- La accion a la derecha del operador **no** puede ser **compuesta** es decir, debe ser una accion **primitiva**.
- Dos acciones de **traslación** **no** pueden paralelizarse. Una acción compuesta en la cual alguna de las primitivas que la componen es de **traslación** será una acción de **traslación**.
- No se pueden paralelizar dos acciones de **trasformación** iguales.
- Si la duración de la acción a la derecha del operador difiere de la acción a la izquierda, la acción resultante durará el tiempo de la acción izquierda, y el efecto durará hasta que termine. Sin embargo, la velocidad de la acción derecha se mantendrá según fue definida.

### Expresiones de animaciones

La pieza clave de una animación es justamente, la animación. En ASL una animación se define como la ubicación de una imagen en una escena. \
Esto define el estado inicial del objeto a animar. Este estado se compone de una imagen, una posición inicial,
un ángulo de rotación inicial y una escala inicial.
Para lograr esto se utiliza la siguiente expresión:

    place Imagen::Image { x::Float , y::Float } Angulo::Float Escala::Float

Esto ubica a la imagen **Imagen** en el punto **( x, y )** de la escena. Con una rotación inicial de **Angulo** grados y una escala inicial de **Escala**

### Comandos

Los comandos son sentencias especiales que permiten actualizar y ejecutar las animaciones.
Existen solamente dos comandos: **Update** y **Play**.

####  Comando Update

El comando update se representa mediante el operador infijo **' << '** y permite agregar acciones a una animación.

    Variable::Name << Accion::Action

Agregar una acción a una animación implica que al ejecutar la animación, la imagen definida en la misma realizará las acciónes que se agregaron.
Cabe destacar que el comando **update** requiere que el primer argumento sea, necesariamente, una **variable** de tipo *Anim*.

El agregado de las acciones se procesa secuencialmente, es decir que en el siguiente ejemplo:

    anim << acc
    anim << move { 100.0 , 100.0 } 4.0

La imagen realizará primero la accion **acc** y luego se moverá a la posición ( 100.0 , 100.0 ) de la escena
en 4 segundos. \
Notar que la semántica de las lineas anteriores es equivalente a:

    anim << acc ; move { 100.0 , 100.0 } 4.0

La duración de una animación está dictada por la suma de las duraciones de las acciones aplicadas
a la misma. Cuando una animación ejecuta todas sus acciones, permanece en su estado final.

#### Comando Play

El comando play se representa mediante una sentencia **play**. Como fue aclarado anteriormente, la sentencia
**play** es ***obligatoria*** en todo programa ASL y debe ser la última del archivo.

    play [ Variable1::Name , ... , VariableN::Name ]

El comando ejecuta todas las animaciones (en paralelo) especificadas como argumento, abriendo una pestaña que muestra las animaciones en cuestión. Una vez finalizan, la pantalla queda abierta hasta que el usuario decida cerrarla o el proceso termine. \
Como en el comando update, el comando **play** requiere que todos sus argumentos sean **variables** de tipo *Anim*.

## Ejemplos de Uso
Se proveen algunos ejemplos de uso en la carpeta src/ejemplos.