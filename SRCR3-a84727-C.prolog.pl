%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3
% Trabalho Prático Individual : Sistema de transportes do concelho de Oeiras
% Autor: A84727

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programação em Lógica Estendida, 
% Métodos de Resolução de Problemas e de Procura.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).
:- set_prolog_flag( double_quotes,atom ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic paragem/11.
:- dynamic adjacencia/4.

% Incluir as bases de Conhecimento que estavam presentes no excel depois de passarem pelo programa Java
:- include('paragensBC.prolog.pl').
:- include('adjacenciasBC.prolog.pl').

%-------------------------------------------------
% Aplicação do PMF

-paragem( Gid,Lat,Long,Estado,Tipo,Pub,Oper,Carr,Cod,Nome,Freg ) :- 
   		nao(paragem( Gid,Lat,Long,Estado,Tipo,Pub,Oper,Carr,Cod,Nome,Freg )), 
    	nao(excecao(paragem( Gid,Lat,Long,Estado,Tipo,Pub,Oper,Carr,Cod,Nome,Freg ))).

% Usa-se os codigos como identificadores
-adjacencia( Carreira,ParagemA,ParagemB,Dist ) :-
		nao(adjacencia( Carreira,ParagemA,ParagemB,Dist )), 
    	nao(excecao(adjacencia( Carreira,ParagemA,ParagemB,Dist ))).


%-------------------------------------------------------------------------
% ---------------IMPLEMENTACAO DAS FUNCIONALIDADES PRETENDIDAS------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Funcao que calcula a distancia euclidiana entre duas paragens dados a 
% latitude e a longitude


calculaDistEuc( LatA,LongA,LatB,LongB,Result ) :- 
                        Result is sqrt( (LatB-LatA)^2 + (LongB-LongA)^2 ).

%%%%%%%%%%%%%PESQUISA NAO INFORMADA%%%%%%%%%%%%%%%%%%%%%%%
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Uso da estratégia “primeiro em profundidade” sem custos para descobrir o caminho entre duas paragens
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

adjacente( Nodo,ProxNodo ) :- 
                        demo(adjacencia( _,Nodo,ProxNodo,_ ),R), 
                        (R == verdadeiro; !, fail).

resolve_pp( Origem,Destino,[Origem|Caminho] ) :-
                        profundidadeprimeiro( Origem,Destino,[],Caminho ), 
                        getParagensbyIds( [Origem|Caminho],R ), 
                        escrever( R ).

profundidadeprimeiro( Destino,Destino,_,[] ).
profundidadeprimeiro( Origem,Destino,Historico,[ProxNodo|Caminho] ) :-
                        adjacente( Origem,ProxNodo ),
                        nao(pertence(ProxNodo,Historico)),
                        profundidadeprimeiro( ProxNodo,Destino,[ProxNodo|Historico],Caminho ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Uso da estratégia “primeiro em profundidade” com custos para descobrir o caminho entre duas paragens
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

adjacente_c( Nodo,ProxNodo,Custo ) :- 
                        demo(adjacencia( _,Nodo,ProxNodo,Custo ),R), 
                        (R == verdadeiro; !, fail).

resolve_pp_c( Origem,Destino,[Origem|Cam],Custo ) :-
                        profundidadeprimeiro_c( Origem,Destino,[],Cam,C ),
                        converteKms( C,Custo ),
                        getParagensbyIds( [Origem|Cam],R ), 
                        escrever( R ), nl,
                        write("Total Custo do Caminho em Kms = "), 
                        write(Custo).

profundidadeprimeiro_c( Destino,Destino,Historico,[],0 ).
profundidadeprimeiro_c( Origem,Destino,Historico,[ProxNodo|Caminho],Total ) :-
                        adjacente_c( Origem,ProxNodo,C ),
                        nao(pertence(ProxNodo,Historico)),
                        profundidadeprimeiro_c( ProxNodo,Destino,[ProxNodo|Historico],Caminho,Custo ),
                        Total is Custo+C.


% Ver a melhor solução
melhor_pp(Origem,Destino, S, Custo) :- findall((SS, CC), resolve_pp_c(Origem,Destino, SS, CC), L), 
                        minimo(L, (S, Custo)).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Uso da estratégia “primeiro em largura” sem custos
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

resolve_pl( Origem,Destino,Caminho ) :-
                        larguraprimeiro( Destino,[(Origem, [])|Xs]-Xs,[],Caminho ),
                        getParagensbyIds( Caminho,R ), 
                        escrever( R ).


larguraprimeiro( Destino,[(Destino, Cams)|_]-_, _, Result ) :- 
                        !, inverso( [Destino|Cams],Result ). % Devolve-se o inverso pk o caminho e construido ao contrario

larguraprimeiro( Destino,[(Nodo, _)|Xs]-Ys,Historico,Result ) :- 
                        pertence( Nodo,Historico ),!, % Se pertencer ao historico, significa que o nodo ja foi visitado, pelo que nao necessita de ser incluido nos nodos a visitar
                        larguraprimeiro( Destino,Xs-Ys,Historico,Result ). % Continuamos ate encontrar a solucao

larguraprimeiro( Destino,[(Nodo, Cams)|Xs]-Ys,Historico,Result ) :-
                        adjacente(Nodo,ProxNodo),     % Usado somente para verificar se existem adjacentes
                        findall( ProxNodo,adjacente(Nodo,ProxNodo),Lista ),   % Vamos buscar todos as paragens adjacentes
                        atualizar( Nodo,Lista,Cams,[Nodo|Historico],Ys-Zs ),  % Atualizamos a Orla com os novos Nodos adjacentes a juntar
                        larguraprimeiro( Destino,Xs-Zs,[Nodo|Historico],Result ). % Continuamos ate encontrar a solucao

% Predicado Auxiliar usado para adicionar os novos nodos adjacentes encontrados à orla de maneira, 
% fazendo-lhe corresponder o caminho que foi necessario efetuar ate chegar a esse nodo
atualizar( _,[],_,_,X-X ).
atualizar( Nodo,[ProxNodo|Resto],Cams,Historico,Xs-Ys ) :-
                        pertence( ProxNodo,Historico ),!,
                        atualizar( Nodo,Resto,Cams,Historico,Xs-Ys ).
atualizar( Nodo,[ProxNodo|Resto],Cams,Historico,[(ProxNodo,[Nodo|Cams])|Xs]-Ys ):-
                        atualizar( Nodo,Resto,Cams,Historico,Xs-Ys ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Uso da estratégia “primeiro em largura” com custos 
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

resolve_pl_c( Origem,Destino,Caminho,Custo ) :-
                        larguraprimeiro_c( Destino,[(Origem, [], 0)|Xs]-Xs,[],Caminho,Custo1 ),
                        converteKms( Custo1,Custo ),
                        getParagensbyIds( Caminho,R ), 
                        escrever( R ), nl,
                        write("Total Custo do Caminho em Kms = "), 
                        write(Custo), nl.


larguraprimeiro_c( Destino,[(Destino, Cams, Custo)|_]-_, _, Result,Custo ) :- 
                        !, inverso( [Destino|Cams],Result ). % Devolve-se o inverso pk o caminho e construido ao contrario

larguraprimeiro_c( Destino,[(Nodo, _,_)|Xs]-Ys,Historico,Result ) :- 
                        pertence( Nodo,Historico ),!, % Se pertencer ao historico, significa que o nodo ja foi visitado, pelo que nao necessita de ser incluido nos nodos a visitar
                        larguraprimeiro_c( Destino,Xs-Ys,Historico,Result ). % Continuamos ate encontrar a solucao

larguraprimeiro_c( Destino,[(Nodo, Cams, C)|Xs]-Ys,Historico,Result,Custo ) :-
                        adjacente(Nodo,ProxNodo),     % Usado somente para verificar se existem adjacentes
                        setof( (ProxNodo,C1),adjacente_c(Nodo,ProxNodo,C1),Lista ),   % Vamos buscar todos as paragens adjacentes
                        atualizar_c( Nodo,C,Lista,Cams,[Nodo|Historico],Ys-Zs ),  % Atualizamos a Orla com os novos Nodos adjacentes a juntar
                        larguraprimeiro_c( Destino,Xs-Zs,[Nodo|Historico],Result,Custo ). % Continuamos ate encontrar a solucao

% Predicado Auxiliar usado para adicionar os novos nodos adjacentes encontrados à orla de maneira, 
% fazendo-lhe corresponder o caminho que foi necessario efetuar ate chegar a esse nodo
atualizar_c( _,_,[],_,_,X-X ).
atualizar_c( Nodo,C,[ProxNodo|Resto],Cams,Historico,Xs-Ys ) :-
                        pertence( ProxNodo,Historico ),!,
                        atualizar_c( Nodo,C,Resto,Cams,Historico,Xs-Ys ).
atualizar_c( Nodo,C,[(ProxNodo,C1)|Resto],Cams,Historico,[(ProxNodo,[Nodo|Cams],N)|Xs]-Ys ):-
                        N is C+C1, atualizar_c( Nodo,C,Resto,Cams,Historico,Xs-Ys ).


%%%%%%%%%%%%%PESQUISA INFORMADA%%%%%%%%%%%%%%%%%%%%%%%
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Predicados usados para estimativas
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Estmativa baseada na distancia Euclidiana entre uma origem e um destino
estimabyDist( Origem,Destino,Estima ) :- 
                        getLatitudebyId( Origem,LatO ), getLongitudebyId( Origem,LongO ),
                        getLatitudebyId( Destino,LatD ),getLongitudebyId( Destino,LongD ),
                        calculaDistEuc( LatO,LongO,LatD,LongD,Estima ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Uso da estratégia “Pesquisa Gulosa (Greedy)” com custos
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
resolve_gulosa(Nodo, Caminho/Custo) :-
    estima(Nodo, Estima),
    gulosa([[Nodo]/0/Estima], InvCaminho/Custo/_),
    inverso(InvCaminho, Caminho).

gulosa(Caminhos, Caminho) :-
    obtem_melhor_G(Caminhos, Caminho),
    Caminho = [Nodo|_]/_/_,
    goal(Nodo).

gulosa(Caminhos, SolucaoCaminho) :-
    obtem_melhor_G(Caminhos, MelhorCaminho),
    seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
    expande_gulosa(MelhorCaminho, ExpCaminhos),
    append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    gulosa(NovoCaminhos, SolucaoCaminho).       


obtem_melhor_G([Caminho], Caminho) :- !.

obtem_melhor_G([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :-
        Est1 =< Est2, !,
        obtem_melhor_G([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho).
    
obtem_melhor_G([_|Caminhos], MelhorCaminho) :- 
    obtem_melhor_G(Caminhos, MelhorCaminho).

expande_gulosa(Caminho, ExpCaminhos) :-
    findall(NovoCaminho, adjacente(Caminho,NovoCaminho), ExpCaminhos).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Uso da estratégia “Pesquisa A*” com custos
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

resolve_aestrela( Origem,Destino,Caminho,Custo ) :-
                        estimabyDist( Origem,Destino,Estima ),
                        aestrela( Destino,[[Origem]/0/Estima],Caminho1/Custo1/_ ),
                        inverso( Caminho1,Caminho ),
                        converteKms( Custo1,Custo ),
                        getParagensbyIds( Caminho,R ), 
                        escrever( R ), nl,
                        write("Total Custo do Caminho em Kms = "), 
                        write(Custo), nl.

aestrela( Destino,Caminhos,Caminho ) :-
                        obtem_melhor_caminho( Caminhos,Caminho ),
                        Caminho = [Destino|_]/_/_.

aestrela( Destino,Caminhos,SolucaoCaminho ) :-
                        obtem_melhor_caminho( Caminhos,MelhorCaminho ),
                        seleciona( MelhorCaminho,Caminhos,OutrosCaminhos ),
                        expande_aestrela( Destino,MelhorCaminho,ExpCaminhos ),
                        append( OutrosCaminhos,ExpCaminhos,NovoCaminhos ),
                        aestrela( Destino,NovoCaminhos,SolucaoCaminho ).     


obtem_melhor_caminho( [Caminho],Caminho ) :- !.

obtem_melhor_caminho( [Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos],MelhorCaminho ) :-
                        Custo1 + Est1 =< Custo2 + Est2, !,
                        obtem_melhor_caminho( [Caminho1/Custo1/Est1|Caminhos],MelhorCaminho ).

obtem_melhor_caminho( [_|Caminhos],MelhorCaminho ) :- 
                        obtem_melhor_caminho( Caminhos,MelhorCaminho ).


expande_aestrela( Destino,Caminho,ExpCaminhos ) :-
                        findall( NovoCaminho,adjacente_aestrela( Destino,Caminho,NovoCaminho ),ExpCaminhos ).


adjacente_aestrela( Destino,[Nodo|Caminho]/Custo/_,[ProxNodo,Nodo|Caminho]/NovoCusto/Est ) :-
                        adjacente_c( Nodo,ProxNodo,Custo1 ),
                        nao( pertence( ProxNodo,Caminho ) ),
                        NovoCusto is Custo + Custo1,
                        estimabyDist( ProxNodo,Destino,Est ).





%-------------------------------------------------------------------------
% ----------------PREDICADOS AUXILIARES UTEIS PARA AS FUNCIONALIDADES PRETENDIDAS-------------

escrever([]).
escrever([X|L]) :- write(X), nl,nl, escrever(L).

getParagensbyIds( [],[] ).
getParagensbyIds( [H|T],[paragem(H,A,B,C,D,E,F,G,I,J,K)|L] ) :- paragem(H,A,B,C,D,E,F,G,I,J,K), 
                                                                getParagensbyIds(T,L).

getLatitudebyId( Id,A ) :- demo( paragem(Id,A,B,C,D,E,F,G,I,J,K),R ), (R == verdadeiro; !, fail).

getLongitudebyId( Id,B ) :- demo( paragem(Id,A,B,C,D,E,F,G,I,J,K),R ), (R == verdadeiro; !, fail).

%-------------------------------------------------------------------------
% ----------------ALGUMAS FUNCOES QUE PODEM SER UTEIS-------------

%>--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Definição de um predicado que permite converter uma distancia de metros(m) 
% para quilometros(Kms)

converteKms( DistanciaM,DistanciaKm ) :- DistanciaKm is DistanciaM/1000.

%>--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Definição de um predicado que permite converter uma distancia de metros(m) 
% para quilometros(Kms)

removeFirstElem( [],[] ).
removeFirstElem( [H|Lista],Lista ).


%>--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Definição de um predicado que permite calcular a soma de 
% todos os elementos de uma lista de inteiros

sum([],R) :-
    R is 0.
sum([H|T], R) :-
    sum(T,RT),
    R is H+RT.

%>--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Definição de um predicado que nos diz se os 
% elementos de uma lista são todos iguais
equals([]).
equals([H]).
equals([F,S|T]) :-
    F == S,
    equals([S|T]).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Extensao do predicado data que permite representar uma data: Dia, Mes, Ano -> {V,F}

data(Dia,Mes,Ano) :- Mes == 2, Ano mod 4 =\= 0, Dia > 0, Dia < 29, Ano >= 0.
data(Dia,Mes,Ano) :- Mes == 2, Ano mod 4 =:= 0, Dia > 0, Dia < 30, Ano >= 0.
data(Dia,Mes,Ano) :- pertence(Mes, [1,3,5,7,8,10,12]), Dia < 32, Dia > 0, Ano >= 0.
data(Dia,Mes,Ano) :- pertence(Mes, [4,6,9,11]), Dia < 31, Dia > 0, Ano >= 0.
data(Dia,Mes,Ano) :- data(Dia,Mes,Ano). 


%---------------------------------
%Extensao de um predicado «concatenar» que resulta na concatenação dos elementos da lista L1 com os elementos da lista L2.

concatenar([],L,L).
concatenar(L,[],L).
concatenar([H1|T1],L2,[H1|R]) :- concatenar(T1,L2,R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento: Termo -> {V,F}

evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   - 
% Extensão do predicado que permite a involucao do conhecimento: Termo -> {V,F}

involucao( Termo ) :-
    solucoes( Invariante,-Termo::Invariante,Lista ),
    remocao( Termo ),
    teste( Lista ).

remocao( Termo ) :-
    retract( Termo ).
remocao( Termo ) :-
    assert( Termo ),!,fail.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,desconhecido }

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

comprimento( S,N ) :-
    length( S,N ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

pertence( X,[X|L] ).
pertence( X,[Y|L] ) :-
    X \= Y,
    pertence( X,L ).

inverso(Xs, Ys):-
    inverso(Xs, [], Ys).

inverso([], Xs, Xs).
inverso([X|Xs],Ys, Zs):-
    inverso(Xs, [X|Ys], Zs).

seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]) :- seleciona(E, Xs, Ys).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

minimo([(P,X)],(P,X)).
minimo([(Px,X)|L],(Py,Y)) :- minimo(L,(Py,Y)), X>Y.
minimo([(Px,X)|L],(Px,X)) :- minimo(L,(Py,Y)), X=<Y.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Ordenada uma lista: 
quickSort([],[]).
quickSort([H|T], Ord) :- pivot(H,T,List1,List2), quickSort(List1,Ord1), quickSort(List2,Ord2),
					     append(Ord1,[H|Ord2],Ord).

%Auxiliar da quickSort, tendo um elemento e uma lista, constroi uma lista com os elementos menores ou iguais 
%que o elemento pivot e uma listas com os maiores
pivot(P,[],[],[]).
pivot(P,[H|T1],[H|T2],L3) :- H =< P, pivot(P,T1,T2,L3).
pivot(P,[H|T1],L2,[H|T3]) :- H > P, pivot(P,T1,L2,T3).

%Ordenada uma lista de datas: 
quickSortData([],[]).
quickSortData([H|T], Ord) :- pivotData(H,T,List1,List2), quickSortData(List1,Ord1), quickSortData(List2,Ord2),
					         append(Ord1,[H|Ord2],Ord).

pivotData(P,[],[],[]).
pivotData(P,[H|T1],[H|T2],L3) :- maisRecente(P,H), pivotData(P,T1,T2,L3).  %pivot maior, pivot para o fim
pivotData(P,[H|T1],L2,[H|T3]) :- \+(maisRecente(P,H)), pivotData(P,T1,L2,T3).


%Determina qual a data mais recente:
maisRecente(data(Dia1,Mes1,Ano1),data(Dia2,Mes2,Ano2)) :- Ano1 > Ano2.
maisRecente(data(Dia1,Mes1,Ano1),data(Dia2,Mes2,Ano2)) :- Ano1 < Ano2, !, fail.
maisRecente(data(Dia1,Mes1,Ano1),data(Dia2,Mes2,Ano2)) :- Mes1 > Mes2.
maisRecente(data(Dia1,Mes1,Ano1),data(Dia2,Mes2,Ano2)) :- Mes1 < Mes2, !, fail.
maisRecente(data(Dia1,Mes1,Ano1),data(Dia2,Mes2,Ano2)) :- Dia1 >= Dia2.
maisRecente(data(Dia1,Mes1,Ano1),data(Dia2,Mes2,Ano2)) :- Dia1 < Dia2, !, fail.

%Remove de uma lista de datas, qualquer data em que não se sabia o ano
removeData([],[]).
removeData([data(_,_,idkyear)|T],Aux) :- removeData(T,Aux).
removeData([H|T],R) :- removeData(T,Aux), append([H],Aux,R).


%Determina o primeiro elemento de uma lista
primeiro([H|T],H).

%Determina o primeiro elemento de uma lista não vazia
ultimo([H],H).
ultimo([H|T],R) :- ultimo(T,R). 

