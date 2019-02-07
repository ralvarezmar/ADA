# Programación de Sistemas de Telecomunicación

# Práctica 2:

# Mini-Chat en modo cliente/servidor

## GSyC

## Octubre de 2015

## 1. Introducción

En esta práctica debes realizar dos programas en Ada que permitan implementar un sencillo sistema de chat entre
usuarios, siguiendo el modelo cliente/servidor.
El programa cliente podrá comportarse de dos maneras: en modo escritor, o en modo lector. Si un cliente es escritor
podrá enviar mensajes al servidor del chat. Si un cliente es lector podrá recibir los mensajes que envíe el servidor.
El servidor se encargará de recibir los mensajes procedentes de los clientes escritores, y reenviárselos a los clientes
lectores.
En una sesión de chat participará un programa servidor y varios programas clientes (lectores y escritores). Cada usuario
del chat tiene que arrancar 2 programas cliente: uno escritor, que lee del teclado y envía mensajes, y uno lector, que recibe
mensajes del servidor y los muestra en la pantalla.
Como extensión de la práctica podrás realizar un tercer programa en Ada que opere como administrador del chat.

## 2. Descripción del programa cliente chat_client.adb

## 2.1. Interfaz de usuario

```
El programa cliente se lanzará pasándole 3 argumentos en la línea de comandos:
```
```
Nombre de la máquina en la que está el servidor
```
```
Número del puerto en el que escucha el servidor
```
```
Nickname(apodo) del cliente del chat. Si elnickesreader, el cliente funcionará en modo lector, con cualquier otro
nickel cliente funcionará en modo escritor.
```
```
Una vez lanzado:
```
```
Si el cliente se lanza en modoescritor, pedirá al usuario cadenas de caracteres, y se las irá enviando al servidor. El
programa terminará cuando el usuario introduzca la cadena.quit
```
```
Si el cliente se lanza en modolector, esperará a recibir mensajes del servidor (conteniendo las cadenas enviadas por
los clientes escritores del chat), y los mostrará en pantalla. En este modo el cliente nunca terminará su ejecución.
```
MUY IMPORTANTE: Si un cliente escritor intenta entrar al chat utilizando unnickque ya está siendo usado por otro
cliente escritor que entró anteriormente, todos sus mensajes serán ignorados por el servidor.
Ejemplo de ejecución: Se lanzan varios clientes escritores y un cliente lector, cada cliente en una ventana de terminal
diferente. El lector es el primer cliente en lanzarse.


```
$ ./chat_client zeta12 9001 reader
server: ana joins the chat
ana: entro
server: carlos joins the chat
carlos: Hola
carlos: quién está ahí?
ana: estoy yo, soy ana
carlos: ana dime algo
ana: hola carlos
carlos: adios
ana: hasta luego
```
```
$ ./chat_client zeta12 9001 carlos
Message: Hola
Message: quién está ahí?
Message: ana dime algo
Message: adios
Message: .quit
$
```
```
$ ./chat_client zeta12 9001 ana
Message: entro
Message: estoy yo, soy ana
Message: hola carlos
Message: hasta luego
Message: .quit
$
```
```
$ ./chat_client zeta12 9001 ana
Message: he entrado?
```
En el ejemplo puede verse cómo intenta entrar en el chat un segundo cliente escritor connickana, pero sus mensajes
son ignorados por el servidor, por lo cual en el lector no aparece ni su intento de entrada ni su mensajehe entrado?.
Sólo en el primer cliente, lanzado como lector, pueden verse todos los mensajes que van enviando los clientes escritores
carlos y ana.

### 2.2. Implementación

El programa cliente tendrá dos comportamientos diferentes según sea lanzado como lector (con elnickreader) o
escritor (con cualquier otronick).
Cuando es lanzadocomo escritor, enviará un mensajeInital servidor para indicarle sunick. Tras enviar ese mensaje,
el programa entrará en un bucle que pida al usuario cadenas de texto, que le irá enviando al servidor mediante mensajes
Writer. El cliente escritor termina cuando el usuario introduce la cadena de texto.quiten el teclado. NOTA: cuando un
cliente escritor termina no envía ningún mensaje, por lo que seguirá en la colección de clientes escritores que se almacena
en el servidor.
Cuando es lanzadocomo lector, enviará un mensajeInital servidor para indicarle sunick(que siempre seráreader).
Tras enviar este mensaje, el programa entrará en un bucle infinito esperando a recibir los mensajesServerenviados por
el servidor, cuyo contenido mostrará en pantalla. El cliente lector nunca termina su ejecución^1.
En el apartado 4 se explica el formato de los mensajes.

(^1) Para interrumpir el programa habrá que pulsar CTRL-C en la ventana de terminal en la que está lanzado


## 3. Descripción del programa servidor chat_server.adb

### 3.1. Interfaz de usuario

```
El programa servidor se lanzará pasándole 1 argumento en la línea de comandos:
```
```
Número del puerto en el que escucha el servidor
```
```
Una vez lanzado, el servidor recibirá mensajes procedentes de clientes:
```
```
Si recibe un mensajeInit, añadirá el cliente a la colección de clientes lectores o a la de escritores, y, además, cuando
el cliente sea un nuevo escritor, enviaráa todos los lectoresun mensajeServernotificando la entrada del nuevo
usuario en el chat.
```
```
Si recibe un mensajeWriter, comprobará si pertenece a un cliente conocido y, en caso afirmativo, enviaráa todos
los lectoresun mensajeServerconteniendo elnickdel cliente escritor y el texto que contenía el mensajeWriter
recibido.
```
```
El servidor nunca terminará su ejecución^2.
El servidor irá mostrando en pantalla los mensajes que vaya recibiendo para permitir comprobar su funcionamiento.
Ejemplo de ejecución que se corresponde con los mensajes enviados por los clientes del ejemplo del apartado anterior:
```
```
$ ./chat_server 9001
INIT received from reader
INIT received from ana
INIT received form carlos
WRITER received from ana: entro
WRITER received from carlos: Hola
WRITER received from carlos: quién está ahí?
WRITER received from ana: estoy yo, soy ana
INIT received from ana. IGNORED, nick already used
WRITER received from carlos: ana dime algo
WRITER received from ana: hola carlos
WRITER received from unknown client. IGNORED
WRITER received from carlos: adios
WRITER received from ana: hasta luego
```
En el ejemplo puede verse cómo es ignorado el mensaje inicial del segundo cliente escritor que quiere usar elnickana,
y como posteriormente su mensaje de escritor es también ignorado al venir de un cliente desconocido.

### 3.2. Implementación

El servidor debe atarse en unEnd_Pointformado con la dirección IP de la máquina en la que se ejecuta, y el puerto
que le pasan como argumento.
Una vez atado, el servidor entrará en un bucle infinito recibiendo mensajes de clientes. El servidor deberá almacenar en
dos coleccioneslos clientes conocidos.Para implementar cada colección se debe escribir un paquete que exporte en
su especificación un Tipo Abstracto de Datos, implementado con memoria dinámica mediante una lista enlazada. De
cada cliente el servidor deberá almacenar suEnd_Pointy sunick.
Cuando el servidor reciba un mensajeInitde un cliente, si es de un lector lo añadirá a la colección de clientes lectores
conocidos, y si es de un escritor y sunickaún no está siendo usado, lo añadirá a su colección de clientes escritores conocidos
y enviará un mensajeServera todos los clientes lectores indicando elnickdel nuevo usuario que entra en le chat. Si el
nickrecibido en un mensajeInitde un escrirtor ya está siendo utilizado por otro cliente, el servidor ignorará ese mensaje
incial, y no enviará ningún mensaje de escritor a los lectores.

(^2) Para interrumpir el programa habrá que pulsar CTRL-C en la ventana de terminal en la que está lanzado


Cuando el servidor reciba un mensajeWriterde un cliente, comprobará si el mensaje pertenece a un cliente que esté
en su colección de clientes escritores conocidos. Si es así, enviará un mensajeServera todos los clientes de la colección
de lectores indicando elnickdel cliente escritor y el texto contenido en el mensaje recibido.
En el apartado 4 se explica el formato de los mensajes. Como puede verse en ese apartado, el mensajeWriterque
envía un cliente sólo contiene elEnd_Pointdel cliente escritor, y el texto del mensaje, no conteniendo elnick. Por ello
el servidor deberá buscar en su colección de clientes escritores conocidos dichoEnd_Pointy así podrá obtener elnick
asociado a él. Lo necesita para poder componer el mensajeServerque enviará a todos los clientes de la colección de
clientes lectores, pues este mensaje lleva elnickdel cliente escritor que lo ha enviado. Si elEnd_Pointbuscado no
aparece en la colección de escritores, es que el mensajeWriterprocede de un cliente escritor desconocido, y debe ser
ignorado, y por no lo tanto, no se enviará un mensajeServera los clientes de la colección de lectores.

## 4. Formato de los mensajes

Los tres tipos de mensajes que se necesitan para esta práctica se distinguen por el primer campo, que podrá adoptar los
valores del siguiente tipo enumerado:

```
type Message_Type is (Init, Writer, Server);
```
MUY IMPORTANTE: Es imprescindible mantener el orden mostrado en los valores de este tipo, a fin de que sean
compatibles las implementaciones de clientes y servidores realizadas por distintos alumnos.
Dicho tipo deberá estar declaradoúnica y exclusivamenteen el ficherochat_messages.adsy desde ahí será usado
tanto por el cliente como por el servidor. Así el código de cliente o del servidor tendrá el siguiente aspecto:

```
with Chat_Messages;
...
procedure ... is
package CM renames Chat_Messages;
use type CM.Message_Type;
...
```
```
Mess: CM.Message_Type;
```
```
begin
...
Mess := CM.Init;
...
if Mess = CM.Server then
...
```
```
end ...;
```
Si este paqueteChat_Messagesno contiene ningún procedimiento no es necesario que tenga cuerpo (fichero.adb),
sino que sólo tendrá especificación (fichero.ads).

### Mensaje Init

```
Es el que envía un cliente al servidor al arrancar. Formato:
Init Client_EP Nick
en donde:
```
```
Init: valor del tipoMessage_Typeque identifica el tipo de mensaje.
```

```
Client_EP:End_Pointdel cliente que envía el mensaje.
```
```
Nick:Unbounded_Stringcon elnickdel cliente. Si elnickesreader, el cliente estará en modo lector, en caso
contrario estará en modo escritor.
```
### Mensaje Writer

```
Es el que envía un cliente escritor al servidor con una cadena de caracteres introducida por el usuario. Formato:
Writer Client_EP Comentario
en donde:
```
```
Writer: valor del tipoMessage_Typeque identifica el tipo de mensaje.
```
```
Client_EP:End_Pointdel cliente que envía el mensaje.
```
```
Comentario:Unbounded_Stringcon la cadena de caracteres introducida por el usuario
```
Nótese que elnickno viaja en estos mensajes: sólo se puede identificar al cliente que envía un mensajeWritera partir
del campoClient_EP. El servidor, tras recibir este mensaje, deberá buscar en la colección de clientes elnickasociado a
Client_EP. Y con él podrá componer el mensaje de servidor que reenviará a los clientes lectores.

### Mensaje Server

Es el que envía un servidor a cada cliente lector, tras haber recibido un mensajeWriterprocedente de un cliente
escritor conteniendo un texto escrito por un usuario. El servidor envía el mensajeServerpara comunicar a todos los
lectores dicho texto. Formato:
Server Nick Texto
en donde:

```
Server: valor del tipoMessage_Typeque identifica el tipo de mensaje.
```
```
Nick:Unbounded_Stringcon elnickdel cliente que escribió el texto. Si el mensaje es para informar de que un
nuevo cliente ha entrado en el chat, este campo tendrá el valorservidor.
```
```
Texto:Unbounded_Stringcon la cadena de caracteres introducida por el usuario. Si el mensaje es para informar
de que un nuevo cliente ha entrado en el chat, este campo tendrá el valor delnickdel usuario concatenado con la
cadenajoins the chat.
```

## 5. Condiciones obligatorias de funcionamiento

1. Se supondrá que nunca se pierden los mensajes enviados ni por el servidor ni por los clientes.
2. Los programas deberán ser robustos, comportándose de manera adecuada cuando no se arranquen con las opciones
    adecuadas en línea de comandos.
3. El servidor debe almacenar los clientes lectores conocidos y los clientes escritores conocidos en dos variables distin-
    tas. Estas variables serán del mismotipo abstracto de datos al que en el enunciado nos referimos como colección,
    que debe estar implementado con una lista dinámica. Dicha lista dinámica debe estar implementada en el paquete
    Client_Collectionscuya especificación,que no puede modificarse, es la siguiente:

```
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
```
```
package Client_Collections is
package ASU renames Ada.Strings.Unbounded;
package LLU renames Lower_Layer_UDP;
```
```
type Collection_Type is limited private;
Client_Collection_Error: exception;
```
```
procedure Add_Client (Collection: in out Collection_Type;
EP: in LLU.End_Point_Type;
Nick: in ASU.Unbounded_String;
Unique: in Boolean);
```
```
procedure Delete_Client (Collection: in out Collection_Type;
Nick: in ASU.Unbounded_String);
```
```
function Search_Client (Collection: in Collection_Type;
EP: in LLU.End_Point_Type)
return ASU.Unbounded_String;
```
```
procedure Send_To_All (Collection: in Collection_Type;
P_Buffer: access LLU.Buffer_Type);
```
```
function Collection_Image (Collection: in Collection_Type)
return String;
private
type Cell;
type Cell_A is access Cell;
type Cell is record
Client_EP: LLU.End_Point_Type;
Nick: ASU.Unbounded_String;
Next: Cell_A;
end record;
```
```
type Collection_Type is record
P_First: Cell_A;
Total: Natural := 0;
end record;
end Client_Collections;
```

4. A diferencia de la práctica anterior, ahora el tipo de la colección (Collection_Type) es un registro que contiene el
    puntero al primer elemento de la lista enlazada con la que se implementa la colección, y el contador total de elementos
    de la colección.
5. Comportamiento esperado de los subprogramas:

```
Add_Client:
```
- Si el parámetroNickNO está en la lista, crea una nueva celda para el cliente, almacenando en ella suEP
    y suNick. Aumenta en 1 el total de elementos de la lista.
- Si el parámetroNickya está en la lista y el parámetroUniquees True,
    eleva la excepciónClient_Collection_Error.
- Si el parámetroNickya está en la lista y el parámetroUniquees False, crea una nueva celda para el
    cliente, almacenando en ella suEPy suNick. Aumenta en 1 el total de elementos de la lista.
Delete_Client: SiNickestá en la lista, elimina su celda de la lista y libera la memoria ocupada por ella
(llamando adecuadamente aFree), y disminuye en 1 el total de elementos de la lista. SiNickno está en la
lista, eleva la excepciónClient_Collection_Error.
Search_Client: SiEPestá en la lista, devuelve suNick. SiEPno está en la lista, eleva la excepción
Client_Collection_Error.
Send_To_All: Este subprograma lo usará el servidor pasando como primer parámetro la colección de clientes
lectores. Envía a todos los clientes de la colección que se pasa como parámetro el mensaje que hay en el Buffer
apuntado porP_Buffer. Si no hay ningún cliente en la colección, o si la colección está vacía, el subprograma
no hace nada, pero no eleva ninguna excepción.
Collection_Image: Devuelve unStringcon la concatenación de los datos de todos los clientes de la
colección que se pasa como parámetro,en orden inverso al que se introdujeron en ella. El formato deberá ser
el siguiente:
- Para cada elemento de la colección, se concatenan los datos del EP (IP y puertos separados por el carácter
":”), un espacio, y el nick. Ejemplo:"193.147.49.72:1025 carlos"
- Los datos de los diversos elementos de la colección se concatenan poniendo entre ellos el carácter ASCII.LF
(fin de línea). Ejemplo:
"193.147.49.72:1025 carlos" & ASCII.LF & "193.147.49.72:1026 ana"
Para obtener la IP y el puerto contenidos en unEnd_Point, puede usarse la siguiente función del paquete
Lower_Layer_UDP:
function Image (EP: End_Point_Type) return String;

```
Dicha función devuelve un String con el siguiente formato:
LOWER_LAYER.INET.UDP.UNI.ADDRESS IP: 193.147.49.72, Port: 1025
```
```
Por lo tanto, dicha cadena debe ser troceada convenientemente mediante las funcionesIndex,HeadyTail
(que ya se utilizaron en la práctica anterior) para extraer la IP y el puerto y poder componer la salida de
Collection_Imagetal y como se pide.
```
6. Aunque en la parte básica de la práctica no se necesita utilizar el subprogramaDelete_Client, el alumno debe
    implementarlo correctamente.
7. Aunque en la parte básica de la práctica no se necesita utilizar el subprogramaCollection_Image, el alumno
    debe implementarlo correctamente, mostrando la colección de todos los clientesen orden inverso al que se intro-
    dujeron en ella.
8. La salida del programa deberá serexactamente iguala la que se muestra en los ejemplos de ejecución, con el mismo
    formato.


9. Un cliente programado por un alumno deberá poder funcionar con un servidor programado por cualquier otro alumno,
    y viceversa. Es conveniente que pruebes tus programas con los de otros alumnos. Por ello es imprescindible respetar
    el protocolo de comunicación entre cliente y servidor y, especialmente, el formato de los mensajes.
10. Cualquier cuestión no especificada en este enunciado puede resolverse e implementarse como se desee.

## 6. Extensión: programa chat_admin.adb

El alumno debe extender el funcionamiento del chat desarrollado en la parte básica desarrollando un tercer programa
(chat_admin.adb) que permita realizar labores de administración en el chat.

### 6.1. Interfaz de usuario de chat_admin.adb

```
El programa se lanzará pasándole 3 argumentos en la línea de comandos:
```
```
Nombre de la máquina en la que está el servidor
```
```
Número de puerto en el que escucha el servidor
```
```
Password(Contraseña) de administración
```
Una vez lanzado, el programa mostrará al usuario un menú interactivo permitiendo al usuario realizar las siguientes
tareas:

```
Mostrar la colección de clientes escritores conocidos por el servidor, incluyendo suEnd_Pointy sunick
```
```
Expulsar del chat a un cliente escritor dado sunick
```
```
Terminar la ejecución del servidor
```
```
Salir del programachat_admin
```
La contraseña de administración debe coincidir con la que se le pase como parámetro adicional al servidor (ver apartado
6.3).
Ejemplo de ejecución:

```
$ ./chat_admin zeta12 9001 admin
Options
1 Show client list
2 Ban client
3 Shutdown server
4 Quit
```
```
Your option? 1
```
```
193.147.49.72:1025 carlos
193.147.49.72:1026 ana
```
```
Options
1 Show client list
2 Ban client
3 Shutdown server
4 Quit
```
```
Your option? 2
Nick to ban? carolina
```

```
Options
1 Show client list
2 Ban client
3 Shutdown server
4 Quit
```
```
Your option? 1
```
```
193.147.49.72:1025 carlos
193.147.49.72:1026 ana
```
```
Your option? 2
Nick to ban? carlos
```
```
Options
1 Show client list
2 Ban client
3 Shutdown server
4 Quit
```
```
Your option? 1
```
```
193.147.49.72:1026 ana
```
```
Options
1 Show client list
2 Ban client
3 Shutdown server
4 Quit
```
```
Your option? 3
Server shutdown sent
```
```
Options
1 Show client list
2 Ban client
3 Shutdown server
4 Quit
```
```
Your option? 4
$
```
### 6.2. Implementación de chat_admin.adb

Para ver la colección de clientes escritores, el programa enviará un mensajeCollection_Request, y esperará como
respuesta del servidor un mensajeCollection_Dataque contiene la información de los clientes escritores a mostrar en
pantalla.
Para expulsar a un cliente, el programa enviará un mensajeBanindicando elnickdel cliente escritor a expulsar. Este
mensaje no es respondido por otro desde el servidor, por lo que para comprobar si ha tenido los efectos previstos el usuario
administrador tendrá que elegir después la opción para mostrar la colección de clientes escritores.


Para provocar que termine la ejecución del servidor, el programa enviará un mensajeShutdown. Este mensaje no tiene
respuesta.
Los detalles de los nuevos tipos de mensajes se muestran en el apartado 6.4. Todos los mensajes que envía el programa
chat_adminincluyen la contraseña de administración recibida por la línea de comandos. Si no fuera la correcta, sus
mensajes serán ignorados por el servidor. Por tanto un mensajeCollection_Requestcon password incorrecta no
obtendrá respuesta. Por ello, si tras enviar un mensajeCollection_Datacon password incorrecta pasan5 segundossin
recibir respuesta, el programachat_adminterminará su ejecución mostrando el mensajeIncorrect password.

### 6.3. Cambios en chat_server.adb

Para soportar la funcionalidad del programachat_admin, el programachat_serverdeberá sufrir las siguientes
modificaciones con respecto a la parte básica:

```
Deberá recibir un segundo argumento en la línea de comandos con la contraseña de adminsitración que debe recibir
del programachat_admin.
```
```
El programachat_serverya no será un bucle infinito recibiendo mensajes, sino que terminará su ejecución al
recibir un mensajeShutdown.
```
```
Deberá estar preparado para atender los nuevos tipos de mensajes:
```
- Al recibir un mensajeCollection_Request, comprobará si la contraseña es correcta y enviará como res-
    puesta un mensajeCollection_Data. Como puede verse en el apartado 6.4, todos los datos de la colección
    de clientes escritores deben enviarse en un únicoUnbounded_String. Se sugiere concatenar las IPs, puertos
    ynicksjunto con caracteres fin de línea (ASCII.LF) para formar elUnbounded_Stringya listo para ser
    mostrado en pantalla. Si la contraseña no fuera correcta, se ignorará el mensaje.
- Al recibir un mensajeBan, comprobará si la contraseña es correcta y si elnickes de un escritor que está en la
    colección de clientes escritores conocidos. En ese caso, eliminará a ese cliente de la colección. Si la contraseña
    no fuera correcta, si elnickfuerareadero si elnickno estuviera en la colección de escritores, se ignorará el
    mensaje.
- Al recibir un mensajeShutdown, comprobará si la contraseña es correcta. En ese caso, el cliente terminará su
    ejecución. Si la contraseña no fuera correcta, se ignorará el mensaje.

```
Ejemplo de ejecución:
```
```
$ ./chat_server 9001 admin
INIT received from ana
INIT received form carlos
WRITER received from ana: entro
WRITER received from carlos: Hola
...
LIST_REQUEST received
BAN received for carolina. IGNORED, nick not found
LIST_REQUEST received
BAN received for carlos
LIST_REQUEST received
LIST_REQUEST received. IGNORED, incorrect password
SHUTDOWN received
```

### 6.4. Formato de los nuevos mensajes

Los nuevos tipos de mensajes que se necesitan para la extensión de esta práctica serán valores adicionales para el tipo
enumeradoMessage_Type, con lo que el tipo quedará definido ahora en la forma:

```
type Message_Type is (Init, Writer, Server, Collection_Request, Collection_Data,
Ban, Shutdown);
```
MUY IMPORTANTE: Es imprescindible mantener el orden mostrado en los valores de este tipo, manteniendo los
mensajes de la parte básica en el mismo sitio, a fin de que un servidor de la extensión sea compatible también con clientes
de la parte básica realizados por cualquier otro alumno.

### Mensaje Collection_Request

```
Es el que envíachat_adminalchat_serverpara pedirle la colección de clientes. Formato:
Collection_Request Admin_EP Password
en donde:
```
```
Collection_Request:Message_Typeque identifica el tipo de mensaje.
```
```
Admin_EP:End_Point_Typecon el valor delEnd_Pointen el que escucha el programaclient_adminy en
el que esperará la recepción del mensajeCollection_Datade respuesta.
```
```
Password:Unbounded_Stringcon la contraseña de administración.
```
### Mensaje Collection_Data

```
Es el que envíachat_serveralchat_admincomo respuesta a un mensajeCollection_Request. Formato:
Collection_Data Data
en donde:
```
```
Collection_Data:Message_Typeque identifica el tipo de mensaje.
```
```
Data:Unbounded_Stringcon los datos de todos los clientes escritores en el mismo formato en que los devuelve
el subprogramaCollection_Imagepero convertido aUnbounded_String.
```
### Mensaje Ban

```
Es el que envíachat_adminalchat_serverpara expulsar a un cliente. Formato:
Ban Password Nick
en donde:
```
```
Ban:Message_Typeque identifica el tipo de mensaje.
```
```
Password:Unbounded_Stringcon la contraseña de administración.
```
```
Nick:Unbounded_Stringcon elnickdel cliente a expulsar.
```
### Mensaje Shutdown

```
Es el que envíachat_adminalchat_serverpara que termine su ejecución. Formato:
Shutdown Password
en donde:
```
```
Shutdown:Message_Typeque identifica el tipo de mensaje.
```
```
Password:Unbounded_Stringcon la contraseña de administración.
```
