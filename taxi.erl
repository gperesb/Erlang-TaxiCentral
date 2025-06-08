%% filepath: c:\temp\compu\Erlang\Proyect\taxi.erl
-module(taxi). % Define el módulo llamado 'taxi'
-export([registra_taxi/2, elimina_taxi/1, consultar_estado/1, servicio_iniciado/1, servicio_completado/1, taxi_en_linea/2]). % Exporta las funciones públicas
%Funcion qu genera el proceso de Taxi
registra_taxi(IdTaxi, UbicacionInicial) ->
    Pid = spawn(taxi, taxi_en_linea, [disponible, UbicacionInicial]),
    centralPID ! {registrarTaxi, IdTaxi, Pid},
    io:format("Manda: ~p solicita registro a central~n", [IdTaxi]).


%%Comunicacion con la central-------------------------------------------
elimina_taxi(IdTaxi) ->
    io:format("Manda: ~p solicita eliminacion a central~n", [IdTaxi]),
    centralPID ! {eliminarTaxi, IdTaxi}. % Envía mensaje a la central para eliminar el taxi


servicio_iniciado(IdTaxi) ->
    io:format("Manda: ~p servicio iniciado a central~n", [IdTaxi]),
    centralPID ! {servicioIniciado, IdTaxi}.

servicio_completado(IdTaxi) ->
    io:format("Manda: ~p servicio completado a central~n", [IdTaxi]),
    centralPID ! {servicioCompletado, IdTaxi}.


%%Aqui si esta el recibir retro porque es el punto de la funcion
consultar_estado(IdTaxi) ->
    io:format("Manda: ~p consulta estado a central~n", [IdTaxi]),
    centralPID ! {consultarTaxi, IdTaxi, self()},
    receive
        {estado, Estado, Ubicacion} ->
            io:format("Estado del Taxi ~p: ~n  Estado: ~p~n  Ubicación: ~p~n", [IdTaxi, Estado, Ubicacion]);
        {error} ->
            io:format("Taxi no encontrado")
    end.
%%----------------------------------------------------------------------------------------------------------
%%>>>>>>>>>>>>Estado -> {ocupado, libre}
taxi_en_linea(Estado, Ubicacion) -> % Proceso principal de Taxi
    receive
        {informacion} ->                                       %%Mandar infromacion a la central
            io:format("Manda: ~p información a central~n", [self()]),
            centralPID ! {info, Estado, Ubicacion},
            io:format("Mensaje enviado:  ~p~n", [{info, Estado, Ubicacion}]),
            taxi_en_linea(Estado, Ubicacion);
        {eliminar} ->                                       %%Matar el proceso
            io:format("Recibe: orden de eliminación. Taxi fuera de Linea~n");
        {solicitar, Nombre, UbicacionDestino, IdTaxi} ->    %%Recibir una solicitud para iniciar viaje
            io:format("Recibe: solicitud de taxi de ~p~n", [Nombre]),
            io:format("Solicitud recibida:~n  Pasajero: ~p~n  Destino: ~p~n  IdTaxi: ~p~n", [Nombre, UbicacionDestino, IdTaxi]),
            taxi_en_linea(Estado, Ubicacion);
        {actualizar, NuevoE, NuevaU}->                      %%Actualizacion del estado que recibe de la central
            io:format("Recibe: actualización de estado a ~p~n", [NuevoE]),
            taxi_en_linea(NuevoE, NuevaU),
            io:format("Actualizado~n")
    end.