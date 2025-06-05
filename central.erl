-module(central).
-export([abre_central/1, cierra_central/0, lista_taxis/0, lista_viajeros/0, viajes_completados/0, centralMain/4, calcularDistancia/2]).

abre_central(Ubicacion) ->
    Pid = spawn(central, centralMain, [Ubicacion, [], [], 1]), % Crea el proceso de la central con ubicación, listas vacías y contador inicial
    register(centralPID, Pid),
    io:format("Ubicación de la central: ~p~n", [Ubicacion]).

% Implementados >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.

cierra_central() ->
    centralPID ! {cerrar}.

lista_taxis()->
    centralPID ! {taxis_activos, self()}, % Envía mensaje a central para obtener la lista de taxis
    receive 
        {taxisActivos, ListaTaxis} ->
            io:format("ListaTaxis de taxis activos:\nNo | IdTaxi | PID\n"),
            lists:foreach(
                fun({No, {IdTaxi, PID}}) ->
                    io:format("~2w | ~6w | ~p~n", [No, IdTaxi, PID])
                end,
                lists:zip(lists:seq(1, length(ListaTaxis)), ListaTaxis)
            );
            _ ->
                io:format("Error al obtener la información de taxis~n")
            end.
    


lista_viajeros()->
    centralPID ! {viajes_atendidos, self()}, % Envía mensaje a central para obtener la lista de viajeros
    receive 
            {viajesIniciados, Viajes}->
                io:format("Valores de Viajes~n: No | Viajero | IdTaxi | Inicio | Final~n"),
                lists:foreach(fun({No, _, Viajero, IdTaxi, Inicio, Final,_}) ->
                    io:format("~3w | ~7w | ~8w | ~8w | ~8w~n",
                            [No, Viajero, IdTaxi, Inicio, Final])
                    end,
                    Viajes
                );
            _->
                io:format("Error al obtener la Informacion de viajes activos")
        end.

viajes_completados()->
    centralPID ! {viajes_completados, self()}, % Envía mensaje a central para obtener los viajes completados
        receive 
            {viajesTerminados, Viajes}->
                io:format("Valores de Viajes~n: No | Viajero | IdTaxi | Inicio | Final~n"),
                lists:foreach(fun({No, _, Viajero, IdTaxi, Inicio, Final, _}) ->
                    io:format("~4w | ~7w | ~6w | ~7w | ~8w~n",
                            [No, Viajero, IdTaxi, Inicio, Final])
                    end,
                    Viajes
                );
            _->
                io:format("Error al obtener la Informacion de viajes activos")
        end.

%% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.


%%Formato de Base de Datos
%Ubicacion, Lista de viajes, Lista de taxis, Contador
%       
%                           Listas de Taxis -> {IdTaxi, PID}
%
%
%
%           Lista de Viajes -> {No, Viaje<PID>, Viajero, IdTaxi, Inicio, Final, Estado}
%                                                                               Estado ->{espera, iniciado, terminado}


centralMain(Ubicacion,ListaViajes, ListaTaxis, Contador) ->

    receive
    %%------------------------Proceso de Registro de Taxi----------------------------------------
        {registrarTaxi, ID, PID}->
            io:format("Registrado ~p~n", [ID]),
            centralMain(Ubicacion, ListaViajes, [{ID, PID}| ListaTaxis], Contador);
    %%----------------------Proceso de Eliminacion de Taxis-------------------------------------------
        {eliminarTaxi, ID} ->
            case searchTaxi(id, ID, ListaTaxis) of     %%Existe?
                nulo ->
                    io:format("Taxi no encontrado: ~p~n", [ID]),
                    centralMain(Ubicacion, ListaViajes, ListaTaxis, Contador);
                PID_Taxi->
                    PID_Taxi ! {informacion},
                    receive 
                        {info, Estado, _}-> %%Estado del taxi
                            case Estado of
                                libre -> 
                                    NuevaListaTaxis = eliminarTaxi(ID, ListaTaxis),
                                    io:format("Taxi eliminado: ~p~n", [ID]),
                                    centralMain(Ubicacion,ListaViajes, NuevaListaTaxis, Contador);
                                ocupado ->
                                    io:format("Taxi Ocupado"),
                                    centralMain(Ubicacion, ListaViajes, ListaTaxis, Contador)
                            end
                    end
                
            end;
        %%-------------------------------Iniciar Viaje------------------------------------
        {servicioIniciado, IdTaxi} ->
            IniciarPorTaxi = 
                fun({No, ViajeP, Viajero, IdTaxi0, Inicio, Final, State}) ->    %%Query por asi decirlo -> update estado where =IdTaxi= to iniciado
                                                                                %% Select el ultimo editado
                    case (IdTaxi =:= IdTaxi0 andalso State =:= espera) of 
                        true-> {{No, ViajeP, Viajero, IdTaxi0, Inicio, Final, iniciado}, true};
                        false ->{ {No, ViajeP, Viajero, IdTaxi0, Inicio, Final, State}, false}
                    end
                end,
            case buscarViaje(IniciarPorTaxi, ListaViajes) of
                {_,_,0} ->                                                              %%Hubo algun cambio?
                    io:format("Registro de Taxi en espera Inexistente"),
                    centralMain(Ubicacion, ListaViajes, ListaTaxis, Contador);
                {NuevaListaViajes, {No,ViajeP, _, _, Inicio, _, _}, _} ->               %%Si es que si, actualiza los procesos relacionados
                    io:format("Iniciando Viaje"),
                    searchTaxi(id, IdTaxi, ListaTaxis) ! {actualizar, ocupado, Inicio},
                    ViajeP ! {disponible, IdTaxi, No},
                    centralMain(Ubicacion, NuevaListaViajes, ListaTaxis, Contador)
            end;
        %%------------------------Terminar Viaje---------------------------------------------------------
        {servicioCompletado, IdTaxi}->
            TerminarPorTaxi = 
                fun({No, ViajeP, Viajero, IdTaxi0, Inicio, Final, State}) ->                %%Cambia donde sea el mismo taxi y este iniciado a terminado 
                    case IdTaxi =:= IdTaxi0 andalso State =:= iniciado of                                                 %%Regresa la cantidad de resgirtros cambiados
                        true-> {{No, ViajeP, Viajero, IdTaxi0, Inicio, Final, terminado}, true};
                        false ->{ {No, ViajeP, Viajero, IdTaxi0, Inicio, Final, State}, false}
                    end
                end,
            case buscarViaje(TerminarPorTaxi, ListaViajes) of 
                {_,_,0} -> 
                    io:format("Registro de Taxi iniciado Inexistente"),
                    centralMain(Ubicacion, ListaViajes, ListaTaxis, Contador);
                {NuevaListaViajes, {_,ViajeP,_,_,_,Fin,_}, _} -> 
                    io:format("Terminando Viaje"),
                    searchTaxi(id, IdTaxi, ListaTaxis) ! {actualizar, disponible, Fin},            %%Actualizar los procesos
                    ViajeP ! {terminoDeViaje},
                    io:format("Viaje Terminado"),
                    centralMain(Ubicacion, NuevaListaViajes, ListaTaxis, Contador)
            end;
        %%----------------------------------Consultar el estado del Taxi--------------------------------
        {consultarTaxi, IdTaxi, De}->
            io:format("Esperando información del taxi, PID: ~p~n", [self()]), 
            case searchTaxi(id, IdTaxi, ListaTaxis) of                               %%Verifica su existencia
                nulo -> 
                    De ! {error};
                PID ->
                    PID ! {informacion},                                               %%Pide la informacion
                    io:format("Esperando Informacion~n"),
                    receive
                        {nada}->
                            io:format("Cayo en nada~n");
                        {info, State, Pos} ->                                    
                            io:format("InfoRecibida~n"),
                            De ! {estado, State, Pos};                             %%Regresa la informacion
                        Otros ->
                            io:format("Otro mensaje recibido: ~p~n", [Otros])
                             %%list_to_tuple(lists:map(fun(A) -> io:format("Valor: ~p~n", [A]) end, tuple_to_list(Otros)))  %%Verificacion de error por no obtener resultado esperado
                        after 2000 ->
                            De ! {estado, no_responde, undefined}
                    end
            end,
            centralMain(Ubicacion, ListaViajes, ListaTaxis, Contador);
        %% ----------------------------------Solicitacion de Taxi--------------------------------------------
        {solicitarTaxi, Viajero, Origen, Destino, ViajeP} -> 
            case obtenerTaxi(Origen, ListaTaxis) of                 %%Buscar el mas cercano
                nulo ->                                     
                    ViajeP ! {noDisponible},                        %%Inexistente disponibilidad
                    centralMain(Ubicacion, ListaViajes, ListaTaxis, Contador);
                IdTaxi ->                                           %%Existente
                    searchTaxi(id, IdTaxi, ListaTaxis) ! {solicitar, Viajero, Destino, IdTaxi},             %Notificar a los procesos
                    Viaje = {Contador, ViajeP, Viajero, IdTaxi, Origen, Destino, espera},
                    centralMain(Ubicacion, [Viaje | ListaViajes], ListaTaxis, Contador +1)
            end;
        %% ------------------------------------------Cancelacion de Taxi----------------------------------

%% ------------------------------------------Cancelacion de Taxi----------------------------------
{cancelarTaxi, Viajero, De} ->
    TerminarPorViajero = 
        fun({No, ViajeP, Viajero0, IdTaxi, Inicio, Final, State}) ->                       %%Buscar el viaje que este en State y cambiarlo a terminado
            case Viajero =:= Viajero0 andalso State =:= espera of                                                    
                true-> {{No, ViajeP, Viajero0, IdTaxi, Inicio, Final, terminado}, true};
                false ->{ {No, ViajeP, Viajero0, IdTaxi, Inicio, Final, State}, false}
            end
        end,
    {NewList, {_,ViajeP,_,IdTaxi,_,_,_}, Count} = buscarViaje(TerminarPorViajero,  ListaViajes),         %%Obtener el PID de viaje 
    case Count  of
        0 ->
            De ! {negado},                                                                          %%Si no hubo cambios, negar
            centralMain(Ubicacion, ListaViajes, ListaTaxis, Contador);
        (_) -> 
            De ! {cancelado, ViajeP},
            ViajeP ! {terminoDeViajeManual},                                                        %%Si hubo cambios, terminar el proceso de viaje
            %% Actualizar el estado del taxi a disponible manteniendo su ubicación actual
            case searchTaxi(id, IdTaxi, ListaTaxis) of
                nulo -> ok;
                PID_Taxi -> 
                    PID_Taxi ! {informacion},
                    receive
                        {info, _, TaxiPos} ->
                            PID_Taxi ! {actualizar, disponible, TaxiPos};
                        _ ->
                            ok
                    after 1000 ->
                        ok
                    end
            end,
            centralMain(Ubicacion, NewList, ListaTaxis, Contador)
    end;
        %%-------------------------------Cerrar la Terminal--------------------------------------------------
        {cerrar} ->
            io:format("Se ha finalizado el trabajo"),
            exit(normal);                                                                                    %%Crea que si se linkea el proceso, no hay necesidad de otras operaciones
        %%----------------------------------------Obtener Listas de diferentes tipos
        {taxis_activos,  De}->
            De ! {taxisActivos, ListaTaxis},
            centralMain(Ubicacion, ListaViajes, ListaTaxis, Contador);
        {viajes_atendidos, De} ->
            ViajesIniciados = [V || V = {_, _, _, _, _, _, iniciado} <- ListaViajes],              %Filtros para cada tipo de situacion
            io:format("InformacionNeviada\n"),
            De ! {viajesIniciados, ViajesIniciados},
            centralMain(Ubicacion, ListaViajes, ListaTaxis, Contador);
        {viajes_completados, De}->
            ViajesTerminados = [V || V = {_, _, _, _, _, _, terminado} <- ListaViajes],
            De ! {viajesTerminados, ViajesTerminados},
            centralMain(Ubicacion, ListaViajes, ListaTaxis, Contador)
    end.
            

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Funciones de Apoyo%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%         ⠀⠀⣀⣤⣴⣶⣶⣦⣤⣀
%%%      ⠀⢀⣴⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣦⡀
%%%    ⠀⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧
%%%   ⢸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇
%%%   ⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
%%%   ⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
%%%   ⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿
%%%    ⠹⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠏
%%%      ⠙⢿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠋
%%%         ⠈⠻⣿⣿⣿⣿⠟⠁
%%%             ⠈⠉⠁
%Busqueda de Taxi dependiendo de su PID o su id
searchTaxi(Key, Value, ListaTaxis) ->
    case Key of
        id ->
            case lists:keyfind(Value, 1, ListaTaxis) of
                {_, PID} -> PID;
                false -> nulo
            end;
        pid ->
            case lists:filter(fun({_, P}) -> P =:= Value end, ListaTaxis) of
                [{ID, _}|_] -> ID;
                [] -> nulo
            end
    end.
%crear lista eliminando un item especifico
eliminarTaxi(ID, [{ID, PID} |Next])->
    PID ! {eliminar},
    eliminarTaxi(ID, Next);
eliminarTaxi(ID, [Item|Next])->
    [Item|eliminarTaxi(ID, Next)];
eliminarTaxi(_, []) ->
    [].



%%Funcion para obtener un taxi disponible.
%Como es una funcion foldl se puede usar para llevar un acumulativo, por tanto 
%se puede determinar que el acumulativo sea el elemento con la menor distancia entre 2 objetos


obtenerTaxi(Origen, ListaTaxis) ->
    AvailableTaxis = lists:filtermap(
        fun({IdTaxi, PID}) ->
            PID ! {informacion},
            receive
                {info, disponible, TaxiPos} ->
                    Dist = calcularDistancia(Origen, TaxiPos),
                    {true, {IdTaxi, PID, Dist}};
                _ ->
                    false
            after 1000 ->
                false
            end
        end,
        ListaTaxis
    ),
    
    case AvailableTaxis of
        [] -> 
            nulo;
        _ ->
            {ClosestId, ClosestPID, _} = lists:foldl(
                fun({Id, PID, Dist}, {_, _, MinDist} = Acc) when Dist < MinDist -> 
                        {Id, PID, Dist};
                   (_, Acc) -> 
                        Acc
                end,
                {nulo, undefined, infinity},
                AvailableTaxis
            ),
            ClosestId
    end.

calcularDistancia({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).


%%Funcion Para cambiar valores dependiendo de una funcion, y regresa el ultimo registro que se edito y un contador de cuantas veces se modifico
buscarViaje(Pred, Viajes) ->
    {LIST, ITEM, COUNT} = lists:foldl(
        fun(Viaje, {NewList, Acc, Count}) ->
            {NewViaje, Case} = Pred(Viaje),
            case Case of 
                true-> {[NewViaje|NewList], NewViaje, Count+1};
                false->{[NewViaje|NewList], Acc, Count}
            end
        end,
        {[], {a,a,a,a,a,a,a}, 0},
        Viajes
    ),
    NewViajesOrdered = lists:reverse(LIST),
    {NewViajesOrdered, ITEM, COUNT}.
 