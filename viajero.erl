

-module(viajero). % Define el módulo llamado 'viajero'
-export([solicitar_taxi/3, cancelar_taxi/1, viaje/0]). % Exporta las funciones públicas


solicitar_taxi(Viajero,Origen, Destino) ->
    io:format("Solicitud Creada~n"),
    centralPID ! {solicitarTaxi, Viajero, Origen, Destino, spawn(viajero,viaje,[]) }.


cancelar_taxi(Viajero) ->
    centralPID ! {cancelarTaxi, Viajero, self()},
    receive
        {cancelado, Nombre, IdViaje, Destino }->
            io:format("Cancelado viaje de: ~p, con Id: ~p, con Destino a: ~p~n", [Nombre, IdViaje, Destino]);
        {negado} ->
            io:format("Lamentablemente no se pudo realizar la accion~n")
    end.

viaje()->
    receive
        {disponible, IdTaxi, IdViaje} -> %%En caso de que el viaje pueda comenzar
            io:format("Taxi asignado: ~p, Viaje: ~p~n", [IdTaxi, IdViaje]),
            viaje();
        {noDisponible} ->  %En caso de que no haya disponibilidad
            io:format("No hay Espacio~n");
        {terminoDeViaje} ->  %En caso de que ya haya terminado el viaje segun las indicaciones del taxi
            io:format("Viaje Terminado~n");
        {terminoDeViajeManual}->        %Antes de que se haya iniciado el viaje, cancelar el viaje
            io:format("Viaje Cancelado~n")
    end. 
