-module(taxi). % Define el módulo llamado 'taxi'
-export([registra_taxi/2, elimina_taxi/1, consultar_estado/1, servicio_iniciado/1, servicio_completado/1, taxi_en_linea/2]). % Exporta las funciones públicas

%Funcion qu genera el proceso de Taxi
registra_taxi(IdTaxi, UbicacionInicial) ->
    centralPID ! {registrarTaxi, IdTaxi, spawn(taxi, taxi_en_linea, [disponible, UbicacionInicial])}.


%%Comunicacion con la central-------------------------------------------
elimina_taxi(IdTaxi) ->
    centralPID ! {eliminarTaxi, IdTaxi}. % Envía mensaje a la central para eliminar el taxi


servicio_iniciado(IdTaxi) ->
    centralPID ! {servicioIniciado, IdTaxi}.

servicio_completado(IdTaxi) ->
    centralPID ! {servicioCompletado, IdTaxi}.


%%Aqui si esta el recibir retro porque es el punto de la funcion
consultar_estado(IdTaxi) ->
    centralPID ! {consultarTaxi, IdTaxi, self()},
    receive
        {estado, Estado, Ubicacion} -> 
            io:format("Estado del Taxi ~p: ~n  Estado: ~p~n  Ubicación: ~p~n", [IdTaxi, Estado, Ubicacion]);
        {error} -> 
            io:format("Taxi no encontrado")
    end.
%%----------------------------------------------------------------------------------------------------------
%%>>>>>>>>>>>>Estado -> {ocupado, libre}
taxi_en_linea(Estado, Ubicacion) ->
    receive
        {informacion} ->
            centralPID ! {info, Estado, Ubicacion},
            io:format("Mensaje enviado a central con información actual del taxi a eliminar:  ~p~n", [{info, Estado, Ubicacion}]),    
            taxi_en_linea(Estado, Ubicacion);
        {eliminar} ->
            io:format("Taxi fuera de Linea~n");
        {solicitar, Nombre, UbicacionDestino, IdTaxi} ->
            io:format("Solicitud recibida:~n  Pasajero: ~p~n  Destino: ~p~n  IdTaxi: ~p~n", 
                     [Nombre, UbicacionDestino, IdTaxi]),
            taxi_en_linea(ocupado, Ubicacion); 
        {actualizar, NuevoE, NuevaU} ->
            taxi_en_linea(NuevoE, NuevaU),
            io:format("Estado actualizado a ~p en ubicación ~p~n", [NuevoE, NuevaU])
    end.
            