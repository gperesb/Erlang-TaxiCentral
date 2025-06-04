### Comandos que funcionan dentro del proyecto para settear el ambiente con algunos datos de pruebaomandos que funcionan dentro del proyecto para settear el ambiente con algunos datos prueba

```sh
# crea registros para despues imprimirlos
c(central).
c(taxi).
c(viajero).
central:abre_central({1,3}).
taxi:registra_taxi(kk,{1,2}).
viajero:solicitar_taxi(juanin, {1,3},{8,3}).
viajero:cancelar_taxi(juanin).
central:lista_viajeros().
central:lista_taxis().
central:viajes_completados().

```
 - 