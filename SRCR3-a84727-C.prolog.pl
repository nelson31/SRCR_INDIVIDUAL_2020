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



pertence( X,[X|L] ).
pertence( X,[Y|L] ) :-
    X \= Y,
    pertence( X,L ).


%-------------------------------------------------------------------------
% ---------------IMPLEMENTACAO DAS FUNCIONALIDADES PRETENDIDAS------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Funcao que calcula a distancia euclidiana entre duas paragens dados a 
% latitude e a longitude


calculaDistEuc( LatA,LongA,LatB,LongB,Result ) :- 
                            Result is sqrt( (LatB-LatA)^2 + (LongB-LongA)^2 ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -

caminho( A,B,P ) :- caminho1( A,[B],PP ), getFullInfo( PP,P ).

caminho1( A,[A|P1],[A|P1] ).
caminho1( A,[Y|P1],P ) :-
        adjacencia( C,X,Y,D ),
        \+ memberchk( X,[Y|P1] ),
        caminho1( A,[X,Y|P1],P ).





%-------------------------------------------------------------------------
% Alguns predicados auxiliares uteis para o desenvolvimento das funcionalidades pretendidas: 

escrever([]).
escrever([X|L]) :- write(X), nl, escrever(L).

getFullInfo( [],[] ).
getFullInfo( [H|T],[paragem(H,A,B,C,D,E,F,G,I,J,K)|L] ) :- paragem(H,A,B,C,D,E,F,G,I,J,K), 
                                                           getFullInfo(T,L).



%-------------------------------------------------------------------------
% Algumas funcoes Auxiliares que podem ser uteis: 

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

