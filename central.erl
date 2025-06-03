
%% filepath: c:\temp\compu\Erlang\Proyect\central.erl
-module(central).
-export([abre_central/1, cierra_central/0, lista_taxis/0, lista_viajeros/0, viajes_completados/0, centralMain/4]).

abre_central(Ubicacion) ->
    Pid = spawn(central, centralMain, [Ubicacion, [], [], 1]), % Crea el proceso de la central con ubicación, listas vacías y contador inicial
    register(centralPID, Pid),
    io:format("Ubicación de la central: ~p~n", [Ubicacion]).

% falta cierra_central

cierra_central() ->
    centralPID ! {}.

lista_taxis()->
    centralPID ! taxis_activos. % Envía mensaje a central para obtener la lista de taxis

lista_viajeros()->
    centralPID ! viajeros_actuales. % Envía mensaje a central para obtener la lista de viajeros

viajes_completados()->
    centralPID ! viajes_atendidos. % Envía mensaje a central para obtener los viajes completados



%Ubicacion, Lista de viajes, Lista de taxis, Contador
%                              Listas de Taxis -> {IdTaxi, PID}
%           Lista de Viajes -> {No, Viaje<PID>, Viajero, IdTaxi, Inicio, Final, Estado}
%                                                                               Estado ->{espera, iniciado, terminado}
centralMain(Ubicacion,ListaViajes, ListaTaxis, Contador) ->
    receive
        {registrarTaxi, ID, PID}->
            io:format("Registrado ~p~n", [ID]),
            centralMain(Ubicacion, ListaViajes, [{ID, PID}| ListaTaxis], Contador);
        {eliminarTaxi, ID} ->
            case searchTaxi(id, ID, ListaTaxis) of
                nulo ->
                    io:format("Taxi no encontrado: ~p~n", [ID]),
                    centralMain(Ubicacion, ListaViajes, ListaTaxis, Contador);
                PID_Taxi->
                    PID_Taxi ! {informacion},
                    receive 
                        {info, Estado, Ubicacion}->
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
        {servicioIniciado, IdTaxi} ->
            IniciarPorTaxi = 
                fun({No, ViajeP, Viajero, IdTaxi0, Inicio, Final, espera}) -> 
                    case IdTaxi =:= IdTaxi0 of 
                        true-> {{No, ViajeP, Viajero, IdTaxi0, Inicio, Final, iniciado}, true};
                        false ->{ {No, ViajeP, Viajero, IdTaxi0, Inicio, Final, espera}, false}
                    end
                end,
            case buscarViaje(IniciarPorTaxi, ListaViajes) of
                {_,_,0} -> 
                    io:format("Registro de Taxi en espera Inexistente"),
                    centralMain(Ubicacion, ListaViajes, ListaTaxis, Contador);
                {NuevaListaViajes, {No,ViajeP, _, _, Inicio, _, _}, _} -> 
                    io:format("Iniciando Viaje"),
                    searchTaxi(id, IdTaxi, ListaTaxis) ! {actualizar, ocupado, Inicio},
                    ViajeP ! {disponible, IdTaxi, No},
                    centralMain(Ubicacion, NuevaListaViajes, ListaTaxis, Contador)
            end;
        {servicioCompletado, IdTaxi}->
            TerminarPorTaxi = 
                fun({No, ViajeP, Viajero, IdTaxi0, Inicio, Final, iniciado}) -> 
                    case IdTaxi =:= IdTaxi0 of 
                        true-> {{No, ViajeP, Viajero, IdTaxi0, Inicio, Final, terminado}, true};
                        false ->{ {No, ViajeP, Viajero, IdTaxi0, Inicio, Final, iniciado}, false}
                    end
                end,
            case buscarViaje(TerminarPorTaxi, ListaViajes) of 
                {_,_,0} -> 
                    io:format("Registro de Taxi iniciado Inexistente"),
                    centralMain(Ubicacion, ListaViajes, ListaTaxis, Contador);
                {NuevaListaViajes, {_,ViajeP,_,_,_,Fin,_}, _} -> 
                    io:format("Terminando Viaje"),
                    searchTaxi(id, IdTaxi, ListaTaxis) ! {actualizar, libre, Fin},
                    ViajeP ! {terminoDeViaje},
                    centralMain(Ubicacion, NuevaListaViajes, ListaTaxis, Contador),
                    io:format("Viaje Terminado")
            end;
        {consultarTaxi, IdTaxi, De}->
            io:format("Esperando información del taxi, PID: ~p~n", [self()]), 
            case searchTaxi(id, IdTaxi, ListaTaxis) of
                nulo -> 
                    De ! {error};
                PID ->
                    PID ! {informacion},
                    timer:sleep(200),
                    receive
                        {info, Estado, Ubicacion} ->
                            io:format("InfoRecibida"),
                            De ! {estado, Estado, Ubicacion};
                        Otros ->
                            io:format("Otro mensaje recibido: ~p~n", [Otros]),
                             list_to_tuple(lists:map(fun(A) -> io:format("Valor: ~p~n", [A]) end, tuple_to_list(Otros)))
                        after 2000 ->
                            De ! {estado, no_responde, undefined}
                    end
            end,
            centralMain(Ubicacion, ListaViajes, ListaTaxis, Contador);

        {solicitarTaxi, Viajero, Origen, Destino, ViajeP} -> 
            case obtenerTaxi(Origen, ListaTaxis) of 
                nulo -> 
                    ViajeP ! {noDisponible},
                    centralMain(Ubicacion, ListaViajes, ListaTaxis, Contador);
                IdTaxi -> 
                    searchTaxi(id, IdTaxi, ListaTaxis) ! {solicitar, Viajero, Destino, IdTaxi},
                    Viaje = {Contador, ViajeP, Viajero, IdTaxi, Origen, Destino, espera},
                    centralMain(Ubicacion, [Viaje | ListaViajes], ListaTaxis, Contador +1)
            end;
        {cancelarTaxi, Viajero, De} ->
            TerminarPorViajero = 
                fun({No, ViajeP, Viajero0, IdTaxi, Inicio, Final, espera}) -> 
                    case Viajero =:= Viajero0 of 
                        true-> {{No, ViajeP, Viajero0, IdTaxi, Inicio, Final, terminado}, true};
                        false ->{ {No, ViajeP, Viajero0, IdTaxi, Inicio, Final, espera}, false}
                    end
                end,
            {NewList, {_,ViajeP,_,_,_,_,_}, Count} = buscarViaje(TerminarPorViajero,  ListaViajes),
            case Count  of
                0 ->
                    De ! {negado},
                    centralMain(Ubicacion, ListaViajes, ListaTaxis, Contador);
                (_) -> 
                    ViajeP ! {terminoDeViajeManual},
                    centralMain(Ubicacion, NewList, ListaTaxis, Contador)
            end
    end.
            

        
%Busqueda de Taxi dependiendo de...
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
%crear lista eliminando item
eliminarTaxi(ID, [{ID, PID} |Next])->
    PID ! {eliminar},
    eliminarTaxi(ID, Next);
eliminarTaxi(ID, [Item|Next])->
    [Item|eliminarTaxi(ID, Next)];
eliminarTaxi(_, []) ->
    [].



%%Funcion para obtener un taxi disponible.
%Como es una funcion foldl se puede usar para llevar un acumulativo, por tanto 
%se puede determinar que el acumulativo sea la menor distancia entre 2 objetos


obtenerTaxi(Posicion, ListaTaxis) ->
    lists:foldl(
        fun({IdTaxi, PID}, Acc) ->
            % Envia mensaje de consulta al taxi
            PID ! {informacion},
            % mini funcion para esperar la respuesta del taxi
            Res = receive
                {info, Estado, Ubicacion} ->
                    Estado
            after 1000 ->
                    no_responde
            end,
            % Si la respuesta es "disponible", se "acumula" en la respuesta
            case Res of
                disponible ->
                    IdTaxi;
                _ ->
                    Acc
            end
        end,
        nulo,
        ListaTaxis
    ).


%%Funcion Para cambiar valores dependiendo de una funcion, y regresa el ultimo registro que se edito y un contador de cuantas veces se modifico

%buscar viaje por taxi {No, Viaje<PID>, Viajero, IdTaxi, Inicio, Final, Estado}
buscarViaje(Pred, Viajes) ->
    {LIST, ITEM, COUNT} = lists:foldl(
        fun(Viaje, {NewList, Acc, Count}) ->
            {NewViaje, Case} = Pred(Viaje),
            case Case of 
                true-> {[NewViaje|NewList], NewViaje, Count+1};
                false->{[NewViaje|NewList], Acc, Count}
            end
        end,
        {[], ind, 0},
        Viajes
    ),
    NewViajesOrdered = lists:reverse(LIST),
    {NewViajesOrdered, ITEM, COUNT}.
 