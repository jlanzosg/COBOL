Basic COBOL ETL 

Este proyecto tiene por objeto desarrollar un ETL (como se dice actualmente) que lea varios archivos, realice ciertas transformaciones y genere un archivo final, todo ello en COBOL.
Es un simple ejercicio para ver como se puede hacer con un lenguaje de  64 años sin utilizar librerías externas ni BBDD, solo utilizando el core del lenguaje.

La base teórica del proyecto es una empresa telefónica ficticia que requiere generar todos los meses un informe con la facturación resumida de sus clientes, algo así como el archivo a enviar al banco para que proceda con los cargos en cuenta correspondientes al consumo mensual.

Tenemos cuatro archivos de entrada:

    CLIENTES.DAT : Archivo con datos de clientes
    TELEFONOS.DAT: Archivo con los números de telefono de cada cliente.
    TARIFAS.DAT: Archivo con las tarifas a aplicar.
    LLAMADAS.DAT: Archivo con el detalle de cada llamada registrada en el mes a facturar.

Estos archivos son de texto, con registros de campos de longitud fija.
El primer paso consiste en crear archivos indexados (VISAM) a partir de los tres primeros archivos planos, esto con el objeto de facilitar las búsquedas.

Una vez que hemos creado los archivos indexados procesamos el cuarto archivo, para determinar el cliente del teléfono que llama, la duración de la llamada y el importe de la misma. En este paso se genera el archivo de texto LLAMADAS-CLI.DAT.

A continuación ordenamos este archivo por cliente y número de teléfono y generamos un último archivo con el resumen por cliente, agregando el nombre del mismo y su cuenta bancaria: LLAMADAS-CLI-M.DAT.

He utilizado el compilador GnuCOBOL V-3.1.2.0 y el IDE específico para COBOL OpenCobolIDE V-4.7.6 :

![image](https://github.com/jlanzosg/COBOL-ETL/assets/170817631/41d79ffa-c204-485a-b128-f3888bef5a40)

También se puede utilizar el editor de código Geany:

![image](https://github.com/jlanzosg/COBOL-ETL/assets/170817631/839ba517-c195-4619-b3ec-1fca8f371996)

Si este fuese un proyecto real, deberíamos hacer validaciones previas a los archivos de entrada, con el objeto de identificar registros con errores o incompletos y tomar alguna acción.




