# Programación de Sistemas de Telecomunicación / Informática II

# Práctica 4:

# Mini-Chat v3.0 (Entrega fiable y ordenada de mensajes)

## GSyC

## Departamento de Teoría de la Señal y Comunicaciones y Sistemas Telemáticos y Computación

## Diciembre de 2015

```
Resumen
Lower_Layer_UDP ofrece un servicio de entrega de mensajes no fiable y no ordenada. En esta práctica se extiende la funcionalidad
de Mini-Chat 2.0 para que el servicio ofrecido por Mini-Chat 3.0 sea fiable y con entrega ordenada. Para conseguirlo, habrá que
retransmitir los mensajes enviados hasta que éstos sean asentidos.
Al ejecutar los clientes y el servidor en una misma máquina es difícil que los mensajes enviados por estos procesos se pierdan
o lleguen desordenados. Por ello, para poder verificar el correcto funcionamiento del código que desarrolles, utilizarás el servicio de
inyección de fallos que proporciona el paqueteLower_Layer_UDP, que permite simular fallos en el envío de mensajes y retardos
de propagación (ver sección 1). Al arrancar los clientes y el servidor se le pasarán 3 nuevos argumentos en la línea de comandos que
permitan configurar la inyección de fallos y el retardo que sufrirán los mensajes (ver sección 2).
Para que el cliente se recupere de las pérdidas de mensajesInitse utilizará un protocolo sencillo de parada y espera. La recepción
de un mensajeWelcomese considerará como señal de que el mensajeInitha llegado al servidor, lo que hará que el mensajeInit
deje de retransmitirse (ver sección 4.1).
Para que el cliente se recupere de las pérdidas de mensajes de tipoWriteryLogout, y para que el servidor se recupere de
las pérdidas de mensajes de tipoServer, tanto cliente como servidor utilizarán un protocolo de envío continuo sin ventana. Estos
tres mensajes serán asentidos por un nuevo tipo de mensajeAck. Cuando se reciba elAckcorrespondiente se dejará de retransmitir
el mensaje asentido. Ver sección 4.2. Los mensajesWriter,ServeryLogoutven modificado su formato. En la sección 4.3 se
detalla el nuevo formato de estos mensajes, así como el del nuevo mensajeAck.
El paqueteProtected_Opspermite planificar la ejecución de un procedimiento a una hora determinada. Utilizarás este meca-
nismo para implementar elprocedimiento de gestión de retransmisionesde los mensajesWriter,ServeryLogout. Llegado
el momento en el que se planificó la ejecución del procedimiento de gestión de retransmisiones el sistema lo ejecutará en un nuevo
hilo de ejecución. El paqueteProtected_Opstambién ofrece un servicio de control de concurrencia que permite ejecutar procedi-
mientos que no pueden ser interrumpidos por otro hilos. En la práctica utilizarás este servicio para que los 3 hilos del programa (el que
ejecuta el programa principal, el que ejecuta el manejador de recepción de mensajes y el que ejecuta el procedimiento de gestión de
retransmisiones) puedan acceder concurrentemente a las estructuras de datos que comparten sin ser interrumpidos mientras que están
consultándolas o modificándolas. Algunas de las estructuras de datos que tienen que ser accedidas concurrentemente por estos tres
hilos son: la colección de clientes activos del servidor, la colección de clientes antiguos del servidor, y, tanto en el cliente como en el
servidor, las nuevas colecciones de datos que se introducen en esta práctica que son necesarias para la gestión de las retransmisiones
de mensajes. Los servicios ofrecidos por este paquete se explican en la sección 3.
En la sección 5 se proporcionan algunos detalles sobre las nuevas estructuras de datos y los algoritmos que tendrás que implementar
para que tanto el cliente como el servidor puedan recuperarse de situaciones en las que se han perdido mensajes, y para que puedan
entregar los mensajes recibidos en el mismo orden en el que se enviaron.
```
## 1. Inyección de fallos, desorden y retardos de propagación

Lower_Layer_UDPofrece un servicio de transmisión de mensajesno fiable. Esto significa que no se garantiza que los mensajes
enviados conSendlleguen a su destino, ni que los mensajes que lleguen al destino lo hagan en el mismo orden en que se enviaron.
Hay que tener en cuenta que aunque el servicio sea no fiable, si los mensajes se envían entre programas que se ejecutan en máqui-
nas de una misma subred resulta prácticamente imposible que se produzcan pérdidas, retardos de propagación apreciables o desorden
en la llegada de mensajes. Por ello, para poder comprobar que Mini-Chat v3.0 tolera pérdidas de mensajes,Lower_Layer_UDP
puede simular retardos y pérdidas de paquetes.


### 1.1. Simulación de las pérdidas de paquetes

El procedimientoSet_Faults_Percentdel paqueteLower_Layer_UDPprovoca que se pierda un porcentaje de todos
los envíos que se realicen a partir del instante en que este procedimiento es llamado. Normalmente se incluye una llamada a este
procedimiento nada más arrancar la aplicación. El porcentaje de pérdidas se especifica como argumento en la llamada al subprograma,
expresado como un valor comprendido entre 0 y 100.
Así, al principio del programa principal puede especificarse un porcentaje de pérdidas de, por ejemplo, el 25 %, invocando dicho
procedimiento en la forma:

```
LLU.Set_Faults_Percent (25);
```
A partir de que se ejecute esta llamada, cada invocación deSendcuenta con un 25 % de probabilidades de que dicho envío se pierda.
La llamada aSet_Faults_Percentdebe realizarse una sola vez al principio del programa principal, y afecta a todos los
envíos que hace el mismo, incluidos los envíos realizados desde el manejador.

### 1.2. Simulación de los retardos de propagación

Mini-Chat v3.0 debe tolerar retardos de propagación elevados y variables que puedan provocar que los mensajes lleguen desor-
denados a sus destinos.
Lower_Layer_UDPpermite introducir retardos de propagación simulados. Para ello incluye el procedimiento
Set_Random_Propagation_Delayque permite especificar los retardos mínimo y máximo que pueden sufrir los mensajes
enviados conSend. Los valores de dichos retardos se expresan como argumentos de tipoIntegerque representan un valor en
milisegundos.
Así, al principio de un programa puede especificarse que se simulen retardos variables entre, por ejemplo, 0 y 500 milisegundos,
invocando este procedimiento en la forma:

```
LLU.Set_Random_Propagation_Delay (0, 500)
```
A partir de que se ejecute esta llamada, cada envío realizado en el programa conSendse verá afectado por un retardo de
propagación simulado de un número de milisegundos elegido aleatoriamente entre 0 y 500.
Nótese que, al poder experimentar distintos envíos retardos diferentes, el utilizarSet_Random_Propagation_Delayim-
plica que pueden llegar desordenados los mensajes a su destino, siendo éste precisamente el efecto buscado.
La llamada aSet_Random_Propagation_Delaydebe realizarse una sola vez al principio del programa principal, y afecta
a todos los envíos que hace el mismo, incluidos los envíos realizados desde el manejador.

### 1.3. Plazo de retransmisión

El mensajeInitdebe ser contestado mediante un mensajeWelcome, que aparte de informar sobre si el cliente ha sido aceptado
o no, tiene la función de actuar como un asentimiento del mensajeInit. Los mensajesWriter,ServeryLogouttienen que
ser asentidos mendiante mensajesAck.
El programa que envía un mensajeInit,Writer,ServeroLogouttiene queretransmitirel mensaje si no recibe suAck
(mensajeWelcomeen el caso deInit) pasado un plazo de tiempo denominadoplazo de retransmisión.
Elplazo de retransmisióndebe establecerse en relación con el retardo máximo de propagación. Si desde que un proceso envió
un mensaje ha pasado ya un tiempo igual al doble del retardo máximo de propagación sin que se haya recibido el asentimiento del
destinatario, es prácticamente seguro que este asentimiento ya no va a llegar, y por lo tanto es razonable retransmitir el mensaje sin
esperar más.
En un escenario real es difícil predecir el retardo máximo de propagación de los mensajes, calculándose normalmente una es-
timación en función de los retardos observados para pasados envíos. Pero dado que en esta práctica se conoce el retardo máximo
de propagación por estar éste determinado por el argumento de la línea de comandosmax_delay, tiene sentido fijar elplazo de
retransmisión(en segundos) en función delretardo máximo de propagación(en milisegundos), de la siguiente manera:

```
Plazo_Retransmision: Duration;
...
Plazo_Retransmision := 2 * Duration(Max_Delay) / 1000;
```

## 2. Interfaz de usuario de los programas cliente y servidor

En la práctica anterior el cliente debía recibir 3 argumentos en la línea de comandos: máquina y puerto a los que está atado el
servidor, ynick. El servidor debía recibir 2 argumentos: número del puerto al que se debe atar el servidor, y número máximo de
clientes.
Además de estos argumentos, ambos programas deberán ahora recibir los siguientes 3 argumentos adicionales (situados al final
de los argumentos de la práctica anterior):

```
min_delay:Retardo mínimo de propagación que sufrirán los envíos que haga el programa (ver apartado 1), expresado como
un número natural demilisegundos.
```
```
max_delay:Retardo máximo de propagación que sufrirán los envíos que haga el programa (ver apartado 1), expresado
como un número natural demilisegundos. Debe ser mayor o igual quemin_delay.
```
```
fault_pct:Porcentaje de envíos realizados por el programa que se perderán (ver apartado 1), expresado como un número
natural entre 0 y 100.
```
## 3. Paquete Protected_Ops

```
Los servicios ofrecidos por este paquete se utilizarán en la práctica para satisfacer dos objetivos:
```
```
Para que en ciertos instantes de tiempo el sistema ejecute el procedimiento de gestión de retransmisiones. Este procedimiento
deberá comprobar si hay algún mensaje pendiente de ser asentido, cuya hora de retransmisión ya haya pasado, y por tanto haya
que retransmitirlo.
```
```
Para garantizar que el código que esté manipulando alguna estructura de datos a la que pueda accederse desde varios hilos de
ejecución distintos (programa principal, manejador de recepción de mensajes o procedimiento de gestión de retransmisiones
ejecutado por el sistema a una hora determinada) no es interrumpido hasta haber dejado la estructura de datos en un estado
consistente.
```
```
La especificación del paqueteProtected_Opses la siguiente:
```
```
with Ada.Real_Time;
```
```
--
-- Calls to any Procedure_A, either those programmed through Program_Timer_Procedure
-- to be executed by the system in the future, or those executed through a call to
-- Protected_Call, are executed in mutual exclusion.
--
```
```
package Protected_Ops is
type Procedure_A is access procedure;
```
```
procedure Program_Timer_Procedure (H: Procedure_A; T: Ada.Real_Time.Time);
-- Schedules H to be executed at time T by the system. When H.all is called, it
-- will be executed in a new thread, in mutual exclusion with calls executed
-- through Protected_Call.
```
```
procedure Protected_Call (H: Procedure_A);
-- The calling thread executes H.all, in mutual exclusion with other calls made
-- through Protected_Call, and with calls to procedures scheduled to be executed
-- by the system through calls to Program_Timer_Procedure
```
```
end Protected_Ops;
```

### 3.1. Ejecución de procedimientos a una hora determinada

Mediante la llamada al procedimientoProtected_Ops.Program_Timer_Procedurese encarga al sistema la ejecución
en un instante del futuro de un procedimiento. El procedimiento que se quiere ejecutar en el futuro, y el instante en el que debe
ejecutarse, se le pasan como argumentosHyTaProgram_Timer_Procedure. El procedimiento que se le pase como argumento
no puede tener argumentos.
El sistema llamará al procedimientoHcuando llegue la horaTa la que fue encargada su ejecución, creando para ello un nuevo
hilo de ejecución. Cuando llegue esa hora el procedimiento se ejecutará de manera concurrente al resto de hilos de ejecución del
programa (hilo del programa principal e hilo de recepción de mensajes mediante manejador). Llegado el momento, mientras que
se esté ejecutando el procedimiento pasado como argumento aProgram_Timer_Procedure, éste no será interrumpido por
procedimientos que se ejecuten mediante llamadas aProtected_Calldesde otros hilos de la aplicación.
Sólo puede haber un procedimiento planificado para ser ejecutado a cierta hora: si cuando se ejecuta una llamada a

Program_Timer_Procedureya había un procedimiento planificado para ser ejecutado a otra hora, la nueva llamada a
Program_Timer_Procedurecancela la anterior, quedando planificada sólo la ejecución del nuevo procedimiento a la nueva
hora.

### 3.2. Ejecución de procedimientos en exclusión mútua

Protected_Callejecuta el procedimientoHque se le pasa como parámetro en exclusión mútua con otros procedimientos que
se estén ejecutando a través deProtected_Call, o con procedimientos que el sistema esté ejecutando tras haber sido programada
su ejecución futura a través deProgram_Timer_Procedure.
El procedimientoHse ejecuta en el mismo hilo que llama aProtected_Call.

### 3.3. Ejemplos

Para aprender a utilizar este paquete NO hay que estudiar el código del cuerpo del paqueteProtected_Ops, pero sí el código de
su especificación, y el código de los ejemplos que aparecen en las carpetastest_protected_ops_1,test_protected_ops_
ytest_protected_ops_3.

```
test_protected_ops_1es un ejemplo sencillo que muestra cómo un procedimiento ejecutado medianteProtected_Call
no es interrumpido por otros procedimientos cuya ejecución se encargó al sistema mediante una llamada a
Program_Timer_Procedure.
```
```
El ejemplo de la carpetatest_protected_ops_2ilustra el problema que puede ocurrir cuando varios hilos están eje-
cutando concurrentemente código que accede a una misma estructura de datos, ya sea para consultarla o para modificar-
la. En el ejemplo el programa principal comienza encargando al sistema que 5000ms después se ejecute el procedimiento
Procedures.Timed_Procedure. Llegado ese momento el sistema creará un nuevo hilo de ejecución para llamar a
Procedures.Timed_Procedure, cuyo código borrará un elemento del Map.
Pero antes de que llegue ese momento, el programa principal prosigue su ejecución, insertando 4 elementos en un Map.
Por último el programa principal llama aPrint_Map, y mientras que dentro de este procedimiento el cursor está situado
sobre el nodo “www.urjc.es” (su puntero apunta a la celda de ese nodo dentro de Map), llega el instante en el que el sistema
tiene que ejecutarProcedures.Timed_Procedure. Este procedimiento borra precisamente esa celda apuntada por el
cursor^1.
El hilo que ha ejecutado el procedimientoProcedures.Timed_Procedurese ha ejecutado mientras que el hilo del pro-
grama principal estaba ejecutandoPrint_Map. La operación de borrar provoca que el cursor que está usandoPrint_Map
para recorrer el Map quede inconsistente, pues el puntero del cursor apunta a un elemento que ha sido borrado. Por ello el
programa falla con una excepción.
```
```
El ejemplo de la carpetatest_protected_ops_3ilustra cómo simplemente protegiendo la ejecución dePrint_Map
haciendo que se llame a través deProtected_Ops.Protected_Callse consigue que el hilo del programa principal
que está ejecutandoPrint_Mapno pueda ser interrumpido mientras que está recorriendo el Map por el hilo del programa
principal que intenta borrar un elemento. Por ello en este caso el programa termina sin elevar una excepción, ya que no se borra
el elemento del Map hasta que ha concluído su ejecuciónPrint_Map.
```
(^1) Para provocar el problema se fuerza mediante delay el que el hilo del programa principal que está ejecutando Print_Map tarde en realizar el recorrido, para así
dar tiempo a que se ejecute el hilo que borra un elemento del Map.


## 4. Protocolos para la recuperación de mensajes perdidos y entrega ordenada

### 4.1. Recuperación de mensajes perdidos Init y Welcome

Para recuperarse de las pérdidas del mensajeInitel cliente utilizará un protocolo de parada y espera: el mensajeInitse
retransmite hasta que se recibe su correspondiente mensajeWelcome, que de este modo actúa como si fuera un asentimiento del
mensajeInit.

### 4.2. Recuperación de mensajes perdidos Writer, Server y Logout

Para recuperarse de las pérdidas de los mensajesWriteryLogoutel cliente utilizará un protocolo de envío continuo sin
ventana. También el servidor, para recuperarse de la pérdida de mensajesServer, utilizará un protocolo de envío contínuo sin
ventana.
Estos mensajes se retransmitirán hasta que sean asentidos por sus destinatarios. Cada uno de estos mensajes llevará un número
de secuencia, debiendo ser asentido por sus receptores mediante un nuevo tipo de mensajeAckque identifica el mensaje que asiente.
Para que los receptores puedan entregar los mensajes recibidos en el orden en el que fueron enviados, los mensajes que no se
reciban en orden no se asentirán. En el cliente, hasta no haberse recibido el mensajeWelcomese ignorarán los mensajesServer(ni
se enviará su asentimiento ni se procesará el mensaje, ignorándolo como si no se hubiera recibido). Los mensajesWriter,Server
yLogoutsólo se procesarán una vez, en el orden de envío marcado por su número de secuencia.
Para la recuperación de pérdidas de estos mensajes se utilizará un protocolo de envío continuo sin ventana, con mensajes de
asentimiento.
Los mensajesWriteryLogoutenviados por el cliente, y los mensajesServerenviados por el servidor, han de ser asentidos
por sus receptores mediante mensajes de asentimiento. Por ello en esta práctica se utilizará un nuevo tipo de mensaje adicional a los
especificados en la práctica anterior:Mensaje de tipo **Ack**. El tipo enumerado para indicar el tipo de mensaje se redefinirá de la
siguiente forma:

```
type Message_Type is (Init, Welcome, Writer, Server, Logout, Ack);
```
Los mensajesAckse reciben en el manejador de recepción de mensajes. Hasta que no se reciba el correspondiente asentimiento
hay que retransmitir los mensajesWriter,ServeryLogout, una vez haya vencido su plazo de retransmisión.
Para poder reconocer qué mensaje asiente un mensajeAck, el formato de los mensajesWriter,LogoutyServercambia
en esta práctica respecto a la anterior: se añade detrás del EndPoint del emisor de un mensajeWriteroLogoutun número de
secuencia, y en los mensajesServerse añade el EndPoint del servidor y un número de secuencia.
El cliente utiliza un único espacio de números de secuencia para los mensajes que envía, ya seanWriteroLogout. El número
de secuencia del mensajeLogoutserá una unidad mayor que el del último de los mensajesWriterenviados por ese cliente.
El servidor utiliza un espacio de números de secuencia distinto para cada cliente, de forma que el mensaje con número de
secuencia 3 enviado a un cliente es distinto al mensaje con número de secuencia 3 enviado a otro, aunque sus contenidos puedan
ser los mismos. De esta forma el servidor podrá identificar correctamente qué clientes sí han asentido y cuáles no cada uno de los
mensajes enviados.
Para implementar losnúmeros de secuenciade los mensajes se utilizará el siguientetipo modular de datos:

```
type Seq_N_T is mod Integer’Last;
```
```
El primer valor del tipoSeq_N_Tes 0 y el últimoInteger’Last - 1.
Una característica de estos tipos de datos modulares de Ada es que tienen aritmética modular:
```
```
Seq_N_T’Last + 1 = Seq_N_T’First
```
```
Seq_N_T’First - 1 = Seq_N_T’Last
```
### 4.3. Formatos de los mensajes

El formato de los mensajeInityWelcomees el mismo que el utilizado en la práctica 3.
Sin embargo sí se modifica el formato de los mensajesWriter,ServeryLogoutrespecto al que tenían en la práctica 3. A
continuación se muestra el nuevo formato de estos tres mensajes (en rojo aparecen los cambios respecto a la práctica 3). Finalmente
se muestra el formato del nuevo mensajeAck:


### Mensaje Writer

```
Es el que envía un cliente al servidor con una cadena de caracteres introducida por el usuario. Formato:
Writer Client_EP_Handler Seq_N Nick Comentario
en donde:
```
```
Writer:Message_Typeque identifica el tipo de mensaje.
```
```
Client_EP_Handler:End_Pointdel cliente que envía el mensaje. El cliente recibe mensajesServeryAcken este
End_Point.
```
```
Seq_N: Valor del tipoSeq_N_T. Número de secuencia de los mensajes enviados por el cliente (ya seanWriteroLogout).
```
```
Nick:Unbounded_Stringcon elnickdel cliente que envía el comentario al servidor.
```
```
Comentario:Unbounded_Stringcon la cadena de caracteres introducida por el usuario
```
### Mensaje Server

```
Es el que envía un servidor a un cliente con el comentario que le llegó en un mensajeWriter. Formato:
Server Server_EP_Handler Seq_N Nick Comentario
en donde:
```
```
Server:Message_Typeque identifica el tipo de mensaje.
```
```
Server_EP_Handler:End_Pointdel servidor, que es el que envía el mensaje. El servidor recibe mensajesInit,
Writer,LogoutyAcken esteEnd_Point.
```
```
Seq_N: Valor del tipoSeq_N_T. Número de secuencia de los mensajes enviados por el servidor. El servidor utilizará espacios
de números de secuencia distintos para cada cliente distinto, por lo que deberá guardar cuál es el último número de secuencia
que ha enviado a cada cliente.
```
```
Nick:Unbounded_Stringcon elnickdel cliente que envió el comentario al servidor, o con elnickserversi es un
mensaje generado por el propio servidor para informar a los clientes de la entrada al chat de un nuevo cliente, del abandono de
un cliente, o de la expulsión de un cliente.
```
```
Comentario:Unbounded_Stringcon la cadena de caracteres introducida por un usuario, o la cadena de caracteres
generada por el servidor en el caso de los mensajesServerconnicknameserver.
```
### Mensaje Logout

```
Es el que envía un cliente al servidor para informarle de que abandona el Mini-Chat v2.0. Formato:
Logout Client_EP_Handler Seq_N Nick
en donde:
```
```
Logout:Message_Typeque identifica el tipo de mensaje.
```
```
Client_EP_Handler:End_Pointdel cliente.
```
```
Seq_N: Valor del tipoSeq_N_T. Número de secuencia de los mensajes enviados por el cliente (ya seanWriteroLogout).
```
```
Nick:Unbounded_Stringcon elnickdel cliente que envía elLogoutal servidor.
```
### Mensaje Ack

Estos mensajes los envía el programa cliente para asentir mensajesServerenviado por el servidor, y los envía el programa
servidor para asentir mensajesWriteryLogoutenviados por los clientes. El destinatario de un mensaje de tipoAckes el
programa que envió el mensaje que se desea asentir.
Formato:
Ack EP_H_ACKer Seq_N
en donde:


```
Ack: Valor del tipoMessage_Typeque identifica el tipo de mensaje.
```
```
EP_H_ACKer:EP_Hdel proceso que envia el asentimiento.
```
```
Seq_N: Valor del tipoSeq_N_T. Número de secuencia que tenía el mensaje recibido que se asiente.
```
## 5. Implementación de los protocolos

En esta sección se proporcionan detalles sobre el modo en el que se deben implementar los protocolos de recuperación de
mensajes perdidos y entrega ordenada.
Cuando el cliente envía un mensajeInitespera recibir un mensajeWelcomeutilizando recepción bloqueante mediante
LLU.Receive. Dado que en esta práctica se pueden perder los mensajes, si tras el plazo de espera especificado enLLU.Receive
el servidor no contesta, el cliente reintentará el envío del mensajeInit. Si tras un máximo número de reintentos no se recibe res-
puesta^2 , cliente terminará mostrando un mensaje que describa la imposibilidad de contactar con el servidor. El mensajeWelcome
enviado por el servidor no se asiente, del mismo modo que un mensaje de asentimientoAckno se asiente. Si se pierde, el cliente
retransmitirá el mensajeInit. Por ello siempre que el servidor reciba un mensajeInitdebe contestar con el mensajeWelcome
adecuado: si el mismo cliente ya está en la colección de clientes activos, con el mismo nick y el mismo EndPoint, se le acepta. Si no
está, se le añade y se le acepta. Si está con el mismo nick y distinto EndPoint, se le rechaza.
Para implementar el sistema de reenvíos de mensajesWriter,ServeryLogouthay que guardar los mensajes que se han
enviado (o reenviado) y aún no han sido asentidos, y la información sobre a qué hora hay que retransmitir cada uno de esos mensajes.
La ejecución del código que comprueba si hay que retransmitir un mensaje una vez que ha pasado su plazo de retransmisión se
realizará mediante el paqueteProtected_Opsque permite programar la ejecución en un instante futuro de unprocedimiento
de gestión de retransmisiones. Llegado ese instante el procedimiento se ejecutará concurrentemente con el resto de hilos de la
aplicación (programa principal y ejecución de manejadores de recepción asíncrona de mensajes). Este mismo paquete ofrece un
servicio para poder controlar el acceso concurrente a las estructuras de datos compartidas por los hilos de la aplicación.

### 5.1. Acciones para la recuperación de mensajes perdidos y entrega ordenada

Hay varios momentos en los que un programa tiene que llevar a cabo acciones relacionadas con la recuperación de mensajes
perdidos:

```
Cuando se envía por primera vez o se retransmite un mensajeWriter,ServeroLogout:
```
1. Hay que guardar información relativa a este envío para que en caso de no recibirse el correspondienteAckpueda retrans-
    mitirse el mensaje una vez haya pasado su plazo de retransmisión.
2. Hay que comprobar si es necesario replanificar la ejecución del procedimiento de gestión de retransmisiones. En caso de
    que ya esté programada su ejecución para un instante anterior al momento en el que hay que retransmitir este mensaje,
    no hay que hacer nada, pues el propio procedimiento de gestión de retransmisiones se va reprogramando cada vez que se
    ejecuta.

```
Cuando llega un mensajeAckal manejador de recepción de mensajes: hay que eliminar la información relativa al mensaje que
está siendo asentido pues ya no habrá que retransmitirlo.
```
```
Cuando llega un mensaje que no es unAckal manejador de recepción de mensajes:
```
1. Si el servidor recibe un mensajeInittiene que contestar con el mensajeWelcomeadecuado^3.
2. Cuando un proceso recibe un mensajeWriter,ServeroLogout:
    - Si su número de secuencia no es igual o menor que el esperado para ese emisor: no debe procesarlo ni asentirlo.
    - Si el número de secuencia es igual al esperado para ese emisor, debe procesarlo normalmente, y debe enviar el
       correspondiente mensajeAck.
    - Si el número de secuencia es menor que el esperado, se trata de una retransmisión de un mensaje que ya se recibió,
       procesó, y asintió. Si lo han retransmitido será porque el asentimiento no llegó antes de que venciera el plazo de
       retransmisión. Por tanto, en este caso no hay que procesar el mensaje, pero sí hay que enviar elAckque corresponda.

```
Cuando llega el momento en el que el sistema llama al procedimiento de gestión de retransmisiones, cuya ejecución se encargó
mediante una llamada aProtected_Ops.Program_Timer_Procedure:
```
(^2) Ver apartado 6 para ver cómo se establece el máximo número de retransmisiones
(^3) Nótese que el mensajeWelcomese envía alClient_EP_Receive


1. El código del procedimiento tiene que retransmitir todos los mensajes cuya hora de retransmisión sea anterior al momento
    en el que se ejecuta el procedimiento.
2. Lo último que tiene que hacer el procedimiento de gestión de retransmisiones es comprobar si hay mensajes pendientes
    de ser asentidos, y si es así, volver a llamar aProtected_Ops.Program_Timer_Procedurepara encargar la
    ejecución de nuevo del procedimiento de gestión de retransmisiones a la primera de las horas a las que venza un plazo de
    retransmisión.

### 5.2. Estructuras de datos para almacenar la Información necesaria para realizar retransmisiones de

### mensajes Writer, Server y Logout no asentidos

5.2.1. Pending_Msgs: Colección de mensajes pendientes de asentimiento

Cada vez que se envía un nuevo mensaje de tipoWriter,ServeroLogoutse debe guardar información sobre el mensaje
en una colección de mensajes enviados que aún no han sido asentidos. Llamaremos a esta colecciónPending_Msgs. El cliente
mantendrá una colección para los mensajesWriteryLogoutpendientes de ser asentidos y el servidor otra para los mensajes
Serverpendientes de ser asentidos.
Cuando se recibe unAck, en caso de que sea el esperado se podrá eliminar la información asociada al mensaje asentido de
Pending_Msgs.
Es aconsejable utilizar una tabla de símbolos oMappara implementar en el cliente y en el servidor las respectivasPending_Msgs.
Cuando se envíe un mensaje habrá que añadir una entrada aPending_Msgs, y cuando llegue unAckhabrá que borrar el mensaje
asentido. Tanto para añadir una entrada dePending_Msgscomo para borrarla parece lógico que la clave de esta tabla de símbolos esté
compuesta por una 3-tupla con los siguientes valores: EndPoint origen del mensaje asentido, EndPoint destino del mensaje asentido,
y su número de secuencia. Esta 3-tupla de valores permite identificar de manera única tanto los mensajes enviados por el cliente al
servidor, como los mensajes enviados por el servidor a cada cliente. Téngase en cuenta que en este segundo caso, el servidor utiliza
espacios de números de secuencia distintos para cada uno de los clientes.
Para cada entrada almacenada enPending_Msgshay que almacenar como valor el conjunto de campos del mensaje que permitan
componer de nuevo el mensaje a reenviar. Como los mensajesWriteryServerllevan como campos unnicky un comentario,
el valor puede almacenar un registro con dos unbounded string. En el caso del mensajeLogoutse desaprovecharía el campo de
comentario pues sólo tiene camponick.

5.2.2. Retransmission_Times: Colección de horas de retransmisión y mensajes a retransmitir

Cuando el procedimiento de gestión de retransmisiones es ejecutado por el sistema tiene que reenviar todos los mensajes cuya
hora de retransmisión ya ha llegado.
Tal como se ha definido en la sección 5.2,Pending_Msgses una tabla de símbolos cuya clave es la 3-tupla de EndPoint origen,
EndPoint destino y número de secuencia. Podría almacenarse la hora de retransmisión en el valor de cada elemento dePending_Msgs.
Pero en ese caso, cuando el procedimiento de gestión de retransmisión se ejecutase tendría que recorrer todos los elementos de
Pending_Msgspara ver si su hora de retransmisión ya ha llegado.
En lugar de hacerlo así en la práctica se utilizará una nueva colección denominadaRetransmission_Times, cuyos elementos sean
2-tuplas (hora de retransmisión, identificador de mensaje).
Si al añadir elementos aRetransmission_Timesse insertan en la posición que les corresponde según el orden marcado por las
horas de retransmisión, el procedimiento de gestión de retransmisiones podrá acceder rápidamente a los elementos cuya hora de
retransmisión haya llegado: bastará con que vaya consultando cada elemento desde el primero, hasta que encuentre uno cuya hora
de retransmisión sea superior a la hora en la que se realiza la consulta. Los elementos con hora menor que la hora de la consulta se
deberán eliminar deRetransmission_Times. A continuación se pueden buscar enPending_Msgslos identificadores de los elementos
eliminados deRetransmission_Times, para así poder enviar sus correspondientes mensajes allí almacenados.
Para insertar un elemento enRetransmission_Timeshabrá que buscar el lugar que le corresponde según el orden de las horas.
Nótese que, a diferencia de una tabla de símbolos, enRetransmission_Timespuede haber dos elementos con la misma hora.
El alumno tendrá que realizar el diseño deRetransmission_Times, que no tiene por qué ser genérica, aunque sí ha de ser un tipo
abstracto de datos.
La estructura de cada elemento almacenado porRetransmission_Timesestá clara: 2-tuplas (hora de retransmisión, identificador
de mensaje).
La interfaz deRetransmission_Timesdeberá permitir:

```
Añadir un elemento, que deberá insertarse en la posición adecuada según su hora de retransmisión, pudiéndo haber varios con
la misma hora de reenvío.
```
```
Consultar el valor del primero de los elementos (el de hora de retransmisión menor).
```

```
Eliminar el primero de los elementos (el de hora de retransmisión menor).
```
De la anterior explicación se deduce que la estructura de datos que hay que diseñar para implementarRetransmission_Timesno
es ni una tabla de símbolos, ni una pila, ni una cola.
Resumiendo: para implementar el sistema de reenvíos y asentimientos se utilizarán dos estructuras de datos que almacenen, por
un lado, los mensajes que se han enviado (o reenviado) y aún no han sido asentidos, y por otro, la información sobre a qué hora hay
que retransmitir cada uno de esos mensajes. La primera de estas estructuras de datos, denominadaPending_Msgs, será una tabla de
símbolos (Map) implementada mediante una tabla hash con resolución de colisiones mediante direccionamiento abierto, con borrado
perezoso (alternativa III). La otra estructura de datos, denominadaRetransmission_Timesdeberá ser diseñada específicamente para
esta práctica en función del uso que se hará de ella.

## 6. Condiciones de Funcionamiento

1. Tanto la estructura de datos en la que se almacenen los datos de los usuarios activos como la que almacena los usuarios
    antiguos que ya no están activos en el servidor deberán implementarse mediante la instanciación de una tabla de símbolos
    genérica implementada como una tabla hash con direccionamiento abierto, con borrado perezoso (alternativa III).
2. Para implementar el sistema de reenvíos y asentimientos se utilizarán dos estructuras de datos que almacenen, por un lado, los
    mensajes que se han enviado (o reenviado) y aún no han sido asentidos, y por otro, la información sobre a qué hora hay que
    retransmitir cada uno de esos mensajes. La primera de estas estructuras de datos, denominadaPending_Msgs, será una tabla
    de símbolos (Map) implementada mediante una tabla hash con resolución de colisiones mediante direccionamiento abierto,
    con borrado perezoso (alternativa III). La otra estructura de datos, denominadaRetransmission_Timesdeberá ser diseñada
    específicamente para esta práctica en función del uso que se hará de ella.
3. El paquete con la implementación de la tabla hash deberá tener la siguiente especificación en su parte pública:

```
generic
type Key_Type is private;
type Value_Type is private;
with function "=" (K1, K2: Key_Type) return Boolean;
type Hash_Range is mod <>;
with function Hash (K: Key_Type) return Hash_Range;
```
```
package Hash_Maps_G is
```
```
type Map is limited private;
```
```
Full_Map : exception;
```
```
procedure Get (M : in out Map;
Key : in Key_Type;
Value : out Value_Type;
Success : out Boolean);
```
```
procedure Put (M : in out Map;
Key : Key_Type;
Value : Value_Type);
```
```
procedure Delete (M : in out Map;
Key : in Key_Type;
Success : out Boolean);
```
```
function Map_Length (M : Map) return Natural;
```
```
--
```

```
-- Cursor Interface for iterating over Map elements
--
type Cursor is limited private;
function First (M: Map) return Cursor;
function Last (M: Map) return Cursor;
procedure Next (C: in out Cursor);
procedure Prev (C: in out Cursor);
function Has_Element (C: Cursor) return Boolean;
type Element_Type is record
Key: Key_Type;
Value: Value_Type;
end record;
No_Element: exception;
```
```
-- Raises No_Element if Has_Element(C) = False;
function Element (C: Cursor) return Element_Type;
```
```
private
...
end Hash_Maps_G;
```
4. El cliente y el servidor deberán tener la interfaz en línea de comandos descrita en la sección 2, debiendo utilizarse los procedi-
    mientos para la inyección de fallos y retardos de propagación descrita en la sección 1.
5. Se supondrá que los mensajes enviados por el servidor y por los clientes se pueden perder. Por ello hay que implementar un
    protocolo de parada y espera para recuperar las pérdidas de los mensajesInit, y un protocolo de envío continuo sin ventana
    para recuperar las pérdidas y entregar en orden los mensajesWriter,ServeryLogout.
6. Para realizar las retransmisiones de mensajesWriter,ServeryLogoutse utilizarán los servicios del paqueteProtected_Ops.
7. La retransmisión de mensajesWriter,ServeryLogoutse realizará en un procedimiento de gestión de retransmisiones
    cuya ejecución se encargará al sistema mediante llamadas aProtected_Ops.Program_Timer.
8. El númeroMAXde retransmisiones de un mismo mensaje será función del porcentaje de pérdidas, según la siguiente fórmula:
    MAX= 10 + ( %p ́erdidas/10)^2. Así, el número máximo de retransmisiones resultará:
       %pérdidas MAX
          10 11
          20 14
          30 19
         ......
9. Deberán ser compatibles las implementaciones de clientes y servidores de los alumnos, para lo cuál es imprescindible respetar
    el formato de los mensajes.
10. Cualquier cuestión no especificada en este enunciado puede resolverse e implementarse como se desee.
11. Se recomienda crear nuevos paquetes para organizar el código.
