

-module(viajero). % Define el módulo llamado 'viajero'
-export([solicitar_taxi/3, cancelar_taxi/1, viaje/0]). % Exporta las funciones públicas

%%Creacion del proceso de viajero
solicitar_taxi(Viajero,Origen, Destino) ->
    io:format("Solicitud Creada~n"),
    centralPID ! {solicitarTaxi, Viajero, Origen, Destino, spawn(viajero,viaje,[]) }.

%%Genera una peticion a la central, esta responde de acuerdo con el resultado de esta.
cancelar_taxi(Viajero) ->
    centralPID ! {cancelarTaxi, Viajero, self()},
    receive
        {cancelado, IdViaje }->
            io:format("Cancelado viaje con Id: ~p~n", [ IdViaje]);
        {negado} ->
            io:format("Lamentablemente no se pudo realizar la accion~n")
    end.
%%Proceso Principal de viaje
viaje()->
    receive
        {disponible, IdTaxi, IdViaje} ->                                     %%Respuesta en caso de que el viaje haya iniciado
            io:format("Taxi asignado: ~p, Viaje: ~p~n", [IdTaxi, IdViaje]),
            viaje();
        {noDisponible} ->                                                     %Terminar el proceso por falta de taxis
            io:format("No hay Espacio~n");
        {terminoDeViaje} ->                                                 %Termino de viaje por accion del taxi
            io:format("Viaje Terminado~n");
        {terminoDeViajeManual}->                                             %Antes de que se haya iniciado el viaje, cancelar el viaje por accion del viajero
            io:format("Viaje Cancelado~n")
    end. 
