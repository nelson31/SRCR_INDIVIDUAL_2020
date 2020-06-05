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


%%%%%%%%%%%%%--------PESQUISA NAO INFORMADA--------%%%%%%%%%%%%%%%%%%%%%%%
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Uso da estratégia “primeiro em profundidade” sem custos para descobrir o caminho entre duas paragens
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

adjacente( Carreira,Nodo,ProxNodo ) :-
                        adjacencia( Carreira,Nodo,ProxNodo,_ ).


percurso_pp( Origem,Destino,[Origem|Caminho],Carreiras ) :-
                        existeParagem( Origem ), existeParagem( Destino ),
                        profundidadeprimeiro( Origem,Destino,[],Caminho,Carreiras,10 ).


profundidadeprimeiro( Destino,Destino,_,[],[],_ ) :- !.
profundidadeprimeiro( Origem,Destino,Historico,[ProxNodo|Caminho],[Carreira|Carreiras],Limite ) :-
                        Limite > 0,
                        adjacente( Carreira,Origem,ProxNodo ),
                        \+ memberchk( Origem/ProxNodo/Carreira,Historico ),
                        Limite1 is Limite-1,
                        % write( Limite ), nl,
                        profundidadeprimeiro( ProxNodo,Destino,[Origem/ProxNodo/Carreira|Historico],Caminho,Carreiras,Limite1 ).


% -------------OU

percurso_pp1( Origem,Destino,Caminho,Carreiras ) :-
                        existeParagem( Origem ), existeParagem( Destino ),
                        profundidadeprimeiro1( Origem,Destino,[],Caminho,Carreiras,20 ).


profundidadeprimeiro1( Destino,Destino,Historico,Caminho,Carreiras,Limite ) :- 
                        !,insereInfo( Historico,Caminho1,Carreiras1 ),
                        inverso( [Destino|Caminho1],Caminho ), inverso( Carreiras1,Carreiras ).
profundidadeprimeiro1( _,_,_,_,_,0 ) :- !,fail.
profundidadeprimeiro1( Origem,Destino,Historico,Caminho,Carreiras,Limite ) :-
                        Limite>0,
                        adjacente( Carreira,Origem,ProxNodo ),
                        nao( pertence( Origem/Carreira,Historico ) ),
                        Limite1 is Limite-1,
                        profundidadeprimeiro1( ProxNodo,Destino,[Origem/Carreira|Historico],Caminho,Carreiras,Limite1 ).


insereInfo( [],[],[] ).
insereInfo( [Nodo/Carreira|Historico],[Nodo|Caminho],[Carreira|Carreiras] ) :-
                        insereInfo( Historico,Caminho,Carreiras ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Uso da estratégia “primeiro em profundidade” com custos para descobrir o caminho entre duas paragens
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

adjacente_c( Carreira,Nodo,ProxNodo,Custo ) :- 
                        adjacencia( Carreira,Nodo,ProxNodo,Custo ).

percurso_pp_c( Origem,Destino,[Origem|Cam],Custo,Carreiras ) :-
                        existeParagem( Origem ), existeParagem( Destino ),
                        estimabyDist( Origem,Destino,Est ),
                        Limite is Est+3000,
                        profundidadeprimeiro_c( Origem,Destino,[],Cam,C,Carreiras,Limite ),
                        converteKms( C,Custo ),
                        getParagensbyIds( [Origem|Cam],R ), 
                        escrever( R ), nl,
                        write("Total Custo do Caminho em Kms = "), 
                        write(Custo).

profundidadeprimeiro_c( Destino,Destino,Historico,[],0,[],Limite ) :- !.
profundidadeprimeiro_c( _,_,_,_,_,_,Limite ) :- Limite =< 0, !, fail.
profundidadeprimeiro_c( Origem,Destino,Historico,[ProxNodo|Caminho],Total,[Carreira|Carreiras],Limite ) :-
                        adjacente_c( Carreira,Origem,ProxNodo,C ),
                        Limite1 is Limite-C,
                        nao( pertence( Origem/ProxNodo/Carreira,Historico ) ),
                        profundidadeprimeiro_c( ProxNodo,Destino,[Origem/ProxNodo/Carreira|Historico],Caminho,Custo,Carreiras,Limite1 ),
                        Total is Custo+C.


% Ver a melhor solução
melhor_pp( Origem,Destino,S,Custo,Carr ) :- findall( (SS,CC,Car),percurso_pp_c(Origem,Destino,SS,CC,Car),L ), 
                                            minimo( L,(S,Custo,Carr) ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Uso da estratégia “primeiro em largura” sem custos
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

percurso_pl( Origem,Destino,Caminho,Carreiras ) :-
                        larguraprimeiro( Destino,[(Origem/0,[],[])|Xs]-Xs,[],Caminho,Carreiras ).


larguraprimeiro( Destino,[(Nodo/Carr,Cams,Carrs)|_]-_,_,Result,ResultCarr ) :-
                        Destino == Nodo,
                        inverso( [Destino|Cams],Result ), % Devolve-se o inverso pk o caminho e construido ao contrario
                        inverso( Carrs,ResultCarr ).

larguraprimeiro( Destino,[(Nodo/Carr,_,_)|Xs]-Ys,Historico,Result,ResultCarr ) :- 
                        nao( Destino == Nodo),
                        pertence( Nodo/Carr,Historico ),!, % Se pertencer ao historico, significa que o nodo ja foi visitado, pelo que nao necessita de ser incluido nos nodos a visitar
                        larguraprimeiro( Destino,Xs-Ys,Historico,Result,ResultCarr ). % Continuamos ate encontrar a solucao

larguraprimeiro( Destino,[(Nodo/Carr,Cams,Carrs)|Xs]-Ys,Historico,Result,ResultCarr ) :-
                        nao( Destino == Nodo),
                        findall( ProxNodo/Nodo/Carreira,adjacente(Carreira,Nodo,ProxNodo),Lista ),   % Vamos buscar todos as paragens adjacentes
                        atualizar( Lista,Cams,Carrs,[Nodo/Carr|Historico],Ys-Zs ),  % Atualizamos a Orla com os novos Nodos adjacentes a juntar
                        larguraprimeiro( Destino,Xs-Zs,[Nodo/Carr|Historico],Result,ResultCarr ). % Continuamos ate encontrar a solucao


% Predicado Auxiliar usado para adicionar os novos nodos adjacentes encontrados à orla de maneira, 
% fazendo-lhe corresponder o caminho que foi necessario efetuar ate chegar a esse nodo
atualizar( [],_,_,_,X-X ).

atualizar( [ProxNodo/Nodo/Carreira|Resto],Cams,Carrs,Historico,Xs-Ys ) :-
                        pertence( ProxNodo/Carreira,Historico ),!,
                        atualizar( Resto,Cams,Carrs,Historico,Xs-Ys ).

atualizar( [ProxNodo/Nodo/Carr|Resto],Cams,Carrs,Historico,[(ProxNodo/Carr,[Nodo|Cams],[Carr|Carrs])|Xs]-Ys ) :-
                        atualizar( Resto,Cams,Carrs,Historico,Xs-Ys ).


% --------OU

percurso_bf( Origem,Destino,Caminho,Carreiras ) :-
                        percurso_bfAux(Destino,[Origem/0/[]/[]],[],Caminho,Carreiras ).

percurso_bfAux( Destino,[Destino/Carr/Caminho/Carreiras|_],Historico,RCaminho,RCarreiras ) :-
                        inverso( [Destino|Caminho],RCaminho ), inverso( Carreiras,RCarreiras ).

percurso_bfAux( Destino,[Nodo/Carreira/CamP/CarrP|Orla],Historico,Caminho,Carreiras ) :-
                        findall( ProxNodo/Carr/[Nodo|CamP]/[Carr|CarrP],( adjacente( Carr,Nodo,ProxNodo ),
                                            nao( member( ProxNodo/Carr,Historico ) ),
                                            nao( member( ProxNodo/Carr/[Nodo|CamP]/[Carr|CarrP],Orla ) ) ),
                                NovosNodos),
                        append( Orla,NovosNodos,NovaOrla ),
                        percurso_bfAux( Destino,NovaOrla,[Nodo/Carreira|Historico],Caminho,Carreiras ).

insereInfo1( [Nodo/0],[Nodo],[] ).
insereInfo1( [Nodo/Carreira|Historico],[Nodo|Caminho],[Carreira|Carreiras] ) :-
                        insereInfo1( Historico,Caminho,Carreiras ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Uso da estratégia “primeiro em largura” com custos 
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

percurso_pl_c( Origem,Destino,Caminho,Carreiras,Custo ) :-
                        larguraprimeiro_c( Destino,[(Origem/0/100000, [], [], 0)|Xs]-Xs,[],Caminho,Carreiras,Custo1 ),
                        converteKms( Custo1,Custo ),
                        getParagensbyIds( Caminho,R ),
                        escrever( R ), nl,
                        escrever( Carreiras ), nl,
                        write("Total Custo do Caminho em Kms = "),
                        write(Custo), nl.


larguraprimeiro_c( Destino,[(Nodo/Carr/C, Cams, Carrs, Custo)|_]-_,_,Result,ResultCarr,Custo ) :-
                        Destino == Nodo,
                        !, inverso( [Destino|Cams],Result ), % Devolve-se o inverso pk o caminho e construido ao contrario
                        inverso( Carrs,ResultCarr ).

larguraprimeiro_c( Destino,[(Nodo/Carr/C, _,_,_)|Xs]-Ys,Historico,Result,ResultCarr,Custo ) :- 
                        pertence( Nodo/Carr/C,Historico ),!, % Se pertencer ao historico, significa que o nodo ja foi visitado, pelo que nao necessita de ser incluido nos nodos a visitar
                        larguraprimeiro_c( Destino,Xs-Ys,Historico,Result,ResultCarr,Custo ). % Continuamos ate encontrar a solucao

larguraprimeiro_c( Destino,[(Nodo/Car/C2, Cams, Carrs,C)|Xs]-Ys,Historico,Result,ResultCarr,Custo ) :-
                        setof( ProxNodo/Carr/C1,adjacente_c(Carr,Nodo,ProxNodo,C1),Lista ),   % Vamos buscar todos as paragens adjacentes
                        atualizar_c( Nodo,C,Lista,Cams,Carrs,[Nodo/Car/C2|Historico],Ys-Zs ),  % Atualizamos a Orla com os novos Nodos adjacentes a juntar
                        larguraprimeiro_c( Destino,Xs-Zs,[Nodo/Car/C2|Historico],Result,ResultCarr,Custo ). % Continuamos ate encontrar a solucao

% Predicado Auxiliar usado para adicionar os novos nodos adjacentes encontrados à orla de maneira, 
% fazendo-lhe corresponder o caminho que foi necessario efetuar ate chegar a esse nodo
atualizar_c( _,_,[],_,_,_,X-X ).
atualizar_c( Nodo,C,[ProxNodo/Carr/C1|Resto],Cams,Carrs,Historico,Xs-Ys ) :-
                        pertence( ProxNodo/Carr/C1,Historico ),!,
                        atualizar_c( Nodo,C,Resto,Cams,Carrs,Historico,Xs-Ys ).
atualizar_c( Nodo,C,[ProxNodo/Carr/C1|Resto],Cams,Carrs,Historico,[(ProxNodo/Carr/C1,[Nodo|Cams],[Carr|Carrs],N)|Xs]-Ys ) :-
                        N is C+C1, atualizar_c( Nodo,C,Resto,Cams,Carrs,Historico,Xs-Ys ).


%%%%%%%%%%%%%--------PESQUISA INFORMADA--------%%%%%%%%%%%%%%%%%%%%%%%
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Predicados usados para estimativas
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Estimativa baseada na distancia Euclidiana entre uma origem e um destino
estimabyDist( Origem,Destino,Estima ) :- 
                        getLatitudebyId( Origem,LatO ), getLongitudebyId( Origem,LongO ),
                        getLatitudebyId( Destino,LatD ),getLongitudebyId( Destino,LongD ),
                        calculaDistEuc( LatO,LongO,LatD,LongD,Estima ).

% Estimativa baseada no menor numero de paragens
estimabyParag( Origem,Destino,Estima ) :-
                        percurso_pp( Origem,Destino,Caminho,Carreiras ),
                        length( Caminho,Estima ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Uso da estratégia “Pesquisa Gulosa (Greedy)” com custos ( Para estimativa e criterio é usada a distancia euclidiana)
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

percurso_gulosabyDist( Origem,Destino,Caminho,Carreiras,Custo ) :- 
                        estimabyDist( Origem,Destino,Estima ),
                        gulosabyDist( Destino,[[Origem]/[]/0/Estima],Caminho1/Carreira1/Custo1/_ ),
                        inverso( Caminho1,Caminho ),
                        inverso( Carreira1,Carreiras ),
                        converteKms( Custo1,Custo ),
                        getParagensbyIds( Caminho,R ),
                        escrever( R ), nl,
                        write("Carreiras usadas no trajeto = "), nl,
                        escrever( Carreiras ), nl,
                        write("Total Custo do Caminho em Kms = "), 
                        write(Custo), nl.


gulosabyDist( Destino,Caminhos,MelhorCaminho ) :-
                        obtem_melhor_caminho_Gulosa( Caminhos,MelhorCaminho ),
                        MelhorCaminho = [Destino|_]/_/_/_.

gulosabyDist( Destino,Caminhos,Caminho ) :-
                        obtem_melhor_caminho_Gulosa( Caminhos,MelhorCaminho ),
                        seleciona( MelhorCaminho,Caminhos,OutrosCaminhos ),
                        expande_CaminhosbyDist( Destino,MelhorCaminho,ExpCaminhos ),
                        append( OutrosCaminhos,ExpCaminhos,NovoCaminhos ),
                        gulosabyDist( Destino,NovoCaminhos,Caminho ).


% Dado um conjunto de Caminhos da-nos aquele que tem uma estimativa menor até chegar ao destino
obtem_melhor_caminho_Gulosa( [Caminho],Caminho ) :- !.

obtem_melhor_caminho_Gulosa( [Caminho1/Carreira1/Custo1/Est1,_/_/Custo2/Est2|Caminhos],MelhorCaminho ) :-
                        Est1 =< Est2, !,
                        obtem_melhor_caminho_Gulosa( [Caminho1/Carreira1/Custo1/Est1|Caminhos],MelhorCaminho ).

obtem_melhor_caminho_Gulosa( [_|Caminhos],MelhorCaminho ) :- 
                        obtem_melhor_caminho_Gulosa( Caminhos,MelhorCaminho ).


% Dado um destino e um Caminho nos dá todos os caminhos adjacentes a um determinado nodo, juntamente 
% com o custo desses  caminhos e a estimativa ate ao Destino
expande_CaminhosbyDist( Destino,Caminho,ExpCaminhos ) :-
                        findall( NovoCaminho,adjacente_InfobyDist( Destino,Caminho,NovoCaminho ),ExpCaminhos ).


adjacente_InfobyDist( Destino,[Nodo|Caminho]/Carrs/Custo/_,[ProxNodo,Nodo|Caminho]/[Car|Carrs]/NovoCusto/Estimativa ) :-
                        adjacente_c( Car,Nodo,ProxNodo,Custo1 ),
                        nao( pertence( ProxNodo,Caminho ) ),
                        NovoCusto is Custo + Custo1,
                        estimabyDist( ProxNodo,Destino,Estimativa ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Uso da estratégia “Pesquisa A*” com custos ( Para estimativa e criterio é usada a distancia euclidiana)
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

percurso_aestrelabyDist( Origem,Destino,Caminho,Carreiras,Custo ) :-
                        estimabyDist( Origem,Destino,Estima ),
                        aestrelabyDist( Destino,[[Origem]/[]/0/Estima],Caminho1/Carreira1/Custo1/_ ),
                        inverso( Caminho1,Caminho ),
                        converteKms( Custo1,Custo ),
                        inverso( Carreira1,Carreiras ),
                        getParagensbyIds( Caminho,R ), 
                        escrever( R ), nl,
                        write("Carreiras usadas no trajeto = "), nl,
                        escrever( Carreiras ), nl,
                        write("Total Custo do Caminho em Kms = "), 
                        write(Custo), nl.


aestrelabyDist( Destino,Caminhos,Caminho ) :-
                        obtem_melhor_caminho_Estrela( Caminhos,Caminho ),
                        Caminho = [Destino|_]/_/_/_.

aestrelabyDist( Destino,Caminhos,SolucaoCaminho ) :-
                        obtem_melhor_caminho_Estrela( Caminhos,MelhorCaminho ),
                        seleciona( MelhorCaminho,Caminhos,OutrosCaminhos ),
                        expande_CaminhosbyDist( Destino,MelhorCaminho,ExpCaminhos ),
                        append( OutrosCaminhos,ExpCaminhos,NovoCaminhos ),
                        aestrelabyDist( Destino,NovoCaminhos,SolucaoCaminho ).     

% Dado um conjunto de Caminhos da-nos aquele que tem uma estimativa + Custo ate ao momento
% menor até chegar ao destino
obtem_melhor_caminho_Estrela( [Caminho],Caminho ) :- !.

obtem_melhor_caminho_Estrela( [Caminho1/Carreira1/Custo1/Est1,_/_/Custo2/Est2|Caminhos],MelhorCaminho ) :-
                        Custo1 + Est1 =< Custo2 + Est2, !,
                        obtem_melhor_caminho_Estrela( [Caminho1/Carreira1/Custo1/Est1|Caminhos],MelhorCaminho ).

obtem_melhor_caminho_Estrela( [_|Caminhos],MelhorCaminho ) :- 
                        obtem_melhor_caminho_Estrela( Caminhos,MelhorCaminho ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Uso da estratégia “Pesquisa Gulosa (Greedy)” com custos ( Para estimativa e criterio é usado 
% o menor numero de paragens)
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

percurso_gulosabyParag( Origem,Destino,Caminho,Carreiras,Custo ) :- 
                        estimabyParag( Origem,Destino,Estima ),
                        gulosabyParag( Destino,[[Origem]/[]/1/Estima],Caminho1/Carreira1/Custo/_ ),
                        inverso( Caminho1,Caminho ),
                        getParagensbyIds( Caminho,R ), 
                        escrever( R ), nl,
                        write("Carreiras usadas no trajeto = "), nl,
                        inverso( Carreira1,Carreiras ),
                        escrever( Carreiras ), nl,
                        write("Total Custo do Caminho em numero de Paragens = "), 
                        write(Custo), nl.


gulosabyParag( Destino,Caminhos,MelhorCaminho ) :-
                        obtem_melhor_caminho_Gulosa( Caminhos,MelhorCaminho ),
                        MelhorCaminho = [Destino|_]/_/_/_.

gulosabyParag( Destino,Caminhos,Caminho ) :-
                        obtem_melhor_caminho_Gulosa( Caminhos,MelhorCaminho ),
                        seleciona( MelhorCaminho,Caminhos,OutrosCaminhos ),
                        expande_CaminhosbyParag( Destino,MelhorCaminho,ExpCaminhos ),
                        append( OutrosCaminhos,ExpCaminhos,NovoCaminhos ),
                        gulosabyParag( Destino,NovoCaminhos,Caminho ).


% Dado um destino e um Caminho nos dá todos os caminhos adjacentes a um determinado nodo, juntamente 
% com o custo desses caminhos em numero de Paragens e a estimativa ate ao Destino
expande_CaminhosbyParag( Destino,Caminho,ExpCaminhos ) :-
                        findall( NovoCaminho,adjacente_InfobyParag( Destino,Caminho,NovoCaminho ),ExpCaminhos ).


adjacente_InfobyParag( Destino,[Nodo|Caminho]/Carrs/Custo/_,[ProxNodo,Nodo|Caminho]/[Car|Carrs]/NovoCusto/Estimativa ) :-
                        adjacente( Car,Nodo,ProxNodo ),
                        nao( pertence( ProxNodo,Caminho ) ),
                        NovoCusto is Custo + 1,
                        estimabyParag( ProxNodo,Destino,Estimativa ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Uso da estratégia “Pesquisa A*” com custos ( Para estimativa e criterio é usado o menor numero de paragens)
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

percurso_aestrelabyParag( Origem,Destino,Caminho,Carreiras,Custo ) :-
                        estimabyParag( Origem,Destino,Estima ),
                        aestrelabyParag( Destino,[[Origem]/[]/1/Estima],Caminho1/Carreira1/Custo/_ ),
                        inverso( Caminho1,Caminho ),
                        getParagensbyIds( Caminho,R ), 
                        escrever( R ), nl,
                        write("Carreiras usadas no trajeto = "), nl,
                        inverso( Carreira1,Carreiras ),
                        escrever( Carreiras ), nl,
                        write("Total Custo do Caminho em numero de Paragens = "), 
                        write(Custo), nl.


aestrelabyParag( Destino,Caminhos,Caminho ) :-
                        obtem_melhor_caminho_Estrela( Caminhos,Caminho ),
                        Caminho = [Destino|_]/_/_/_.

aestrelabyParag( Destino,Caminhos,SolucaoCaminho ) :-
                        obtem_melhor_caminho_Estrela( Caminhos,MelhorCaminho ),
                        seleciona( MelhorCaminho,Caminhos,OutrosCaminhos ),
                        expande_CaminhosbyParag( Destino,MelhorCaminho,ExpCaminhos ),
                        append( OutrosCaminhos,ExpCaminhos,NovoCaminhos ),
                        aestrelabyParag( Destino,NovoCaminhos,SolucaoCaminho ).     


%-------------------------------------------------------------------------------------
% ---------------PREDICADOS USADOS PARA DAR RESPOSTA AS QUERYS PRETENDIDAS------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Percurso entre duas paragens
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

percurso2Paragens( Origem,Destino,Resultado ) :- 
                        percurso_pp( Origem,Destino,Resultado,Carreiras ),!. 

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Selecionar apenas algumas das operadoras de transporte para um determinado percurso
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

seleciona_Operadoras( Origem,Destino,Operadoras,Caminho,Carreiras,Custo ) :-
                        verificaOperadoras_Cam( [Origem,Destino],Operadoras,P ), % Verificar se as operadoras da origem e destino fazem parte das que se pretendem excluir
                        larguraprimeiro_Operadoras( Destino,Operadoras,[(Origem/0/100000, [], [], 0)|Xs]-Xs,[],Caminho,Carreiras,Custo1 ),
                        converteKms( Custo1,Custo ).

larguraprimeiro_Operadoras( Destino,_,[(Nodo/Carr/C, Cams, Carrs, Custo)|_]-_,_,Result,ResultCarr,Custo ) :-
                        Destino == Nodo,
                        !,inverso( [Destino|Cams],Result ), 
                        inverso( Carrs,ResultCarr ).

larguraprimeiro_Operadoras( Destino,Operadoras,[(Nodo/Carr/C, _,_,_)|Xs]-Ys,Historico,Result,ResultCarr,Custo ) :- 
                        pertence( Nodo/Carr/C,Historico ),!,
                        larguraprimeiro_Operadoras( Destino,Operadoras,Xs-Ys,Historico,Result,ResultCarr,Custo ).

larguraprimeiro_Operadoras( Destino,Operadoras,[(Nodo/Car/C2, Cams, Carrs,C)|Xs]-Ys,Historico,Result,ResultCarr,Custo ) :-
                        findall( ProxNodo/Carr/C1,( adjacente_c(Carr,Nodo,ProxNodo,C1),verificaOperadoras_Cam( [Nodo,ProxNodo],Operadoras,P ) ),Lista ), % Verificar se os nodos adjacentes nao tem operadoras que se pretendem excluir
                        atualizar_c( Nodo,C,Lista,Cams,Carrs,[Nodo/Car/C2|Historico],Ys-Zs ),
                        larguraprimeiro_Operadoras( Destino,Operadoras,Xs-Zs,[Nodo/Car/C2|Historico],Result,ResultCarr,Custo ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Excluir uma ou mais operadoras de transporte para o percurso
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

excluir_Operadoras( Origem,Destino,Operadoras,Caminho,Carreiras,Custo ) :-
                        nao( verificaOperadoras_Cam( [Origem,Destino],Operadoras,P ) ), % Verificar se as operadoras da origem e destino nao fazem parte das que se pretendem excluir
                        larguraprimeiro_noOperadoras( Destino,Operadoras,[(Origem/0/100000, [], [], 0)|Xs]-Xs,[],Caminho,Carreiras,Custo1 ),
                        converteKms( Custo1,Custo ).

larguraprimeiro_noOperadoras( Destino,_,[(Nodo/Carr/C, Cams, Carrs, Custo)|_]-_,_,Result,ResultCarr,Custo ) :-
                        Destino == Nodo,
                        !,inverso( [Destino|Cams],Result ), 
                        inverso( Carrs,ResultCarr ).

larguraprimeiro_noOperadoras( Destino,Operadoras,[(Nodo/Carr/C, _,_,_)|Xs]-Ys,Historico,Result,ResultCarr,Custo ) :- 
                        pertence( Nodo/Carr/C,Historico ),!,
                        larguraprimeiro_noOperadoras( Destino,Operadoras,Xs-Ys,Historico,Result,ResultCarr,Custo ).

larguraprimeiro_noOperadoras( Destino,Operadoras,[(Nodo/Car/C2, Cams, Carrs,C)|Xs]-Ys,Historico,Result,ResultCarr,Custo ) :-
                        findall( ProxNodo/Carr/C1,( adjacente_c(Carr,Nodo,ProxNodo,C1),nao( verificaOperadoras_Cam( [Nodo,ProxNodo],Operadoras,P ) ) ),Lista ), % Verificar se os nodos adjacentes nao tem operadoras que se pretendem excluir
                        atualizar_c( Nodo,C,Lista,Cams,Carrs,[Nodo/Car/C2|Historico],Ys-Zs ),
                        larguraprimeiro_noOperadoras( Destino,Operadoras,Xs-Zs,[Nodo/Car/C2|Historico],Result,ResultCarr,Custo ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Identificar quais as paragens com o maior numero de carreiras num determinado percurso
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

paragens_maisCarreiras( Percurso,Resultado ) :-
                        getParagensMaisCarreiras( Percurso,Ncarrs,Resultado ).


getParagensMaisCarreiras( [],0,[] ).

getParagensMaisCarreiras( [P|Ps],Ncarrs,[P] ) :-
                        getParagensMaisCarreiras( Ps,N,Resultado ),
                        getCarreiras( P,Carrs ),
                        length( Carrs,Ncarrs ),
                        Ncarrs > N.

getParagensMaisCarreiras( [P|Ps],N,[P|Resultado] ) :-
                        getParagensMaisCarreiras( Ps,N,Resultado ),
                        getCarreiras( P,Carrs ),
                        length( Carrs,Ncarrs ),
                        Ncarrs == N.

getParagensMaisCarreiras( [P|Ps],N,Resultado ) :-
                        getParagensMaisCarreiras( Ps,N,Resultado ),
                        getCarreiras( P,Carrs ),
                        length( Carrs,Ncarrs ),
                        Ncarrs < N.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Escolher o menor percurso(usando o critério menor numero de paragens)
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

menorPercurso( Origem,Destino,Caminho,Carreiras,Custo ) :-
                        percurso_aestrelabyParag( Origem,Destino,Caminho,Carreiras,Custo ),!.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Escolher o percurso mais rapido(usando o critério de distancia)
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

maisRapido( Origem,Destino,Caminho,Carreiras,Custo ) :-
                        percurso_aestrelabyDist( Origem,Destino,Caminho,Carreiras,Custo ),!.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Escolher o percurso que passe apenas por abrigos com publicidade
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

paragens_comAbrigoPublicidade2( Origem,Destino,Percurso,Carreiras ) :-
                        findall( (Caminho,Carrs),percurso_pp( Origem,Destino,Caminho,Carrs ),Caminhos ),
                        verificaPubAbrigo_Cams( Caminhos,Percurso1 ),
                        retiraParams( Percurso1,Percurso,Carreiras).

retiraParams( P/C,P,C ).

verificaPubAbrigo_Cams( [],_ ) :- !, fail.

verificaPubAbrigo_Cams( [(Caminho,Carr)|Cams],Caminho/Carr ) :-
                        verificaPubAbrigo_Cam( Caminho ).

verificaPubAbrigo_Cams( [(Caminho,Carr)|Cams],Result ) :- verificaPubAbrigo_Cams( Cams,Result ).


verificaPubAbrigo_Cam( [] ).

verificaPubAbrigo_Cam( [P|Ps] ) :-
                        existePublicidade( P ),
                        verificaPubAbrigo_Cam( Ps ).

verificaPubAbrigo_Cam( Caminho ) :- !, fail.

%%%%%%%%%%%%% OU (Versao sem usar findall)

paragens_comAbrigoPublicidade( Origem,Destino,[Origem|Caminho],Carreiras ) :-
                        percursoPubAbrigo( Origem,Destino,[],Caminho,Carreiras ).


percursoPubAbrigo( Destino,Destino,_,[],[] ) :- existePublicidade(Destino), !.
percursoPubAbrigo( Origem,Destino,Historico,[ProxNodo|Caminho],[Carreira|Carreiras] ) :-
                        adjacente( Carreira,Origem,ProxNodo ),
                        existePublicidade( ProxNodo ),
                        nao( pertence( Origem/ProxNodo/Carreira,Historico ) ),
                        percursoPubAbrigo( ProxNodo,Destino,[Origem/ProxNodo/Carreira|Historico],Caminho,Carreiras ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Escolher o percurso que passe apenas por paragens abrigadas
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

paragens_Abrigadas( Origem,Destino,[Origem|Caminho],Carreiras ) :-
                        percursoAbrigado( Origem,Destino,[],Caminho,Carreiras ).


percursoAbrigado( Destino,Destino,_,[],[] ) :- existeAbrigo( Destino ),!.
percursoAbrigado( Origem,Destino,Historico,[ProxNodo|Caminho],[Carreira|Carreiras] ) :-
                        adjacente( Carreira,Origem,ProxNodo ),
                        existeAbrigo( ProxNodo ),
                        nao( pertence( Origem/ProxNodo/Carreira,Historico ) ),
                        percursoAbrigado( ProxNodo,Destino,[Origem/ProxNodo/Carreira|Historico],Caminho,Carreiras ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Escolher um ou mais pontos intermedios por onde o percurso devera passar
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

percurso_Intermedios( Origem,Destino,NodosIntermedios,[Origem|Caminho],Carreiras ) :-
                        ( ( pertence( Origem,NodosIntermedios ), 
                        removeElem( Origem,NodosIntermedios,Resultado ),
                        percursoNodosInt( Origem,Destino,Resultado,[],Caminho,Carreiras ) );
                        percursoNodosInt( Origem,Destino,NodosIntermedios,[],Caminho,Carreiras ) ).


percursoNodosInt( Destino,Destino,[],_,[],[] ) :- !.

percursoNodosInt( Destino,Destino,_,_,[],[] ) :- !,fail.

percursoNodosInt( Origem,Destino,NodosIntermedios,Historico,[ProxNodo|Caminho],[Carreira|Carreiras] ) :-
                        adjacente( Carreira,Origem,ProxNodo ),
                        nao( pertence( Origem/ProxNodo/Carreira,Historico ) ),
                        ((pertence( ProxNodo,NodosIntermedios), 
                        removeElem( ProxNodo,NodosIntermedios,NodosIntermedios1 ),
                        percursoNodosInt( ProxNodo,Destino,NodosIntermedios1,[Origem/ProxNodo/Carreira|Historico],Caminho,Carreiras ) );
                        (nao( pertence( ProxNodo,NodosIntermedios) ),
                        percursoNodosInt( ProxNodo,Destino,NodosIntermedios,[Origem/ProxNodo/Carreira|Historico],Caminho,Carreiras ) ) ).


%% OU!!!!!

percurso_Intermedios2( Origem,Destino,NodosIntermedios,Result,ResultCarreiras ) :-
                        findall( (Caminho,Carreiras),percurso_pp(Origem,Destino,Caminho,Carreiras ),Caminhos ),
                        verificaCaminhos( Caminhos,NodosIntermedios,Result,ResultCarreiras ).

verificaCaminhos( [],NodosIntermedios,Caminho,Carreiras ) :- !,fail.
verificaCaminhos( [(Caminho,Carreiras)|L],NodosIntermedios,Caminho,Carreiras ) :-
                        contemNodosIntermedios( Caminho,NodosIntermedios ).
verificaCaminhos( [(Cam,Carreiras)|L],NodosIntermedios,Caminho,Carreiras ) :-
                        verificaCaminhos( L,NodosIntermedios,Caminho,Carreiras ).

contemNodosIntermedios( _,[] ).
contemNodosIntermedios( [],_ ) :- !,fail.
contemNodosIntermedios( [P|Ps],NodosIntermedios ) :- 
                        pertence( P,NodosIntermedios ),
                        removeElem( P,NodosIntermedios,NodosIntermedios1 ), 
                        contemNodosIntermedios( Ps,NodosIntermedios1 ).
contemNodosIntermedios( [P|Ps],NodosIntermedios ) :- 
                        nao( pertence( P,NodosIntermedios) ),
                        contemNodosIntermedios( Ps,NodosIntermedios ).

%---------------------------------------------------------------------------------------------
% ----------------PREDICADOS AUXILIARES UTEIS PARA AS FUNCIONALIDADES PRETENDIDAS-------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicado que calcula a distancia euclidiana entre duas paragens dados a 
% latitude e a longitude

calculaDistEuc( LatA,LongA,LatB,LongB,Result ) :- 
                        Result is sqrt( (LatB-LatA)^2 + (LongB-LongA)^2 ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

escrever([]).
escrever([X|L]) :- write(X), nl,nl, escrever(L).

getParagensbyIds( [],[] ).
getParagensbyIds( [H|T],[paragem(H,A,B,C,D,E,F,G,I,J,K)|L] ) :- paragem(H,A,B,C,D,E,F,G,I,J,K), 
                                                                getParagensbyIds(T,L).


getLatitudebyId( Id,A ) :- demo( paragem(Id,A,B,C,D,E,F,G,I,J,K),R ), (R == verdadeiro; !, fail).


getLongitudebyId( Id,B ) :- demo( paragem(Id,A,B,C,D,E,F,G,I,J,K),R ), (R == verdadeiro; !, fail).


getCarreiras( Id,Carrs ) :- demo( paragem(Id,B,C,D,E,F,G,Carrs,I,J,K),R ), (R == verdadeiro; !, fail).


existeAdjacencia( Nodo,ProxNodo ) :- demo( adjacencia( _,Nodo,ProxNodo,_ ),R ),
                                     (R == verdadeiro; !,fail).


existeParagem( Id ) :- paragem(Id,B,C,D,E,F,G,H,I,J,K).


existePublicidade( Id ) :- demo( paragem(Id,B,C,D,E,yes,G,H,I,J,K),R ), R==verdadeiro.


existeAbrigo( Id ) :- demo( paragem(Id,B,C,D,sem__abrigo,F,G,H,I,J,K),R ), R==falso.


getAllCarreiras( R ) :- findall( Carr,adjacencia( Carr,Nodo,ProxNodo,Dist ),R1 ),
                        removeDuplicados( R1,R ).


getAllParagens( R ) :- findall( Id,paragem(Id,B,C,D,E,F,G,H,I,J,K),R ).


getParagembyFreg( Freg,Id ) :- demo( paragem(Id,B,C,D,E,F,G,H,I,J,Freg),R ), R==verdadeiro.


getFregbyParagem( Id,Freg ) :- demo( paragem(Id,B,C,D,E,F,G,H,I,J,Freg),R ), R==verdadeiro.


%---------------------------------------------------------------------------------------------
% ----------------------------------PREDICADOS ADICIONAIS-------------------------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >Dada um nome de freguesia e uma origem, descobrir uma maneira de la chegar
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

percurso_toFreguesia( Origem,Freg,[Origem|Caminho],Carreiras ) :-
                        existeParagem( Origem ), getParagembyFreg( Freg,Destino ),
                        estimabyDist( Origem,Destino,Estima ),
                        Estimativa is Estima+3000,
                        profundidadeprimeiro( Origem,Destino,[],Caminho,Carreiras,0,Estimativa ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% >A partir de um conjunto de carreiras em que o percurso deve utilizar, devolve-nos o caminho
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

percurso_Carreiras( Origem,Destino,Carreiras,Caminho ) :- 
                        findall( (Cam,Carrs),percurso_pp(Origem,Destino,Cam,Carrs ),Caminhos ),
                        verificaCarreiras( Caminhos,Carreiras,Caminho ).

verificaCarreiras( [],Carreiras,Caminho ) :- !,fail.
verificaCarreiras( [(Caminho,Carrs)|L],Carreiras,Caminho ) :-
                        contemCarreiras( Carrs,Carreiras ).
verificaCarreiras( [(Caminho,Carrs)|L],Carreiras,Caminho ) :-                        
                        verificaCarreiras( L,Carreiras,Caminho ).

contemCarreiras( _,[] ) :- !.
contemCarreiras( [],_ ) :- !,fail.
contemCarreiras( [P|Ps],Carreiras ) :- 
                        pertence( P,Carreiras ),
                        removeElem( P,Carreiras,Carreiras1 ), 
                        contemCarreiras( Ps,Carreiras1 ).
contemCarreiras( [P|Ps],Carreiras ) :- 
                        nao( pertence( P,Carreiras) ),
                        contemCarreiras( Ps,Carreiras ).


%------------------------------------------------------------------
% ---------------------------INVARIANTES---------------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante Referencial: nao é possivel diminuir o conhecimento da base de Conhecimento das paragens

-paragem( Gid,Lat,Long,Est,Tipo,Pub,Oper,Carr,Cod,Nome,Freg ) :: 
                        ( !,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante Referencial: nao é possivel diminuir o conhecimento da base de Conhecimento das adjacencias

-adjacencia( Carreira,ParagemA,ParagemB,Dist ) ::
                        ( !,fail ).

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
% Definição de um predicado que permite remover os primeiros N elementos de uma lista

removeNElems( [],_,[] ).
removeNElems( Lista,0,Lista ).
removeNElems( [H|Lista],N,Result ) :- N1 is N-1, removeNElems( Lista,N1,Result ).

%>--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Definição de um predicado que permite remover um dado elemento de uma lista

removeElem( _,[],[] ).
removeElem( E,[H|T],Result ) :- E == H, removeElem( E,T,Result ).
removeElem( E,[H|T],[H|Result] ) :- removeElem( E,T,Result ).

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


%>--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Extensao de um predicado concatenar que resulta na concatenação dos elementos da lista L1 com os elementos da lista L2.

concatenar([],L,L).
concatenar(L,[],L).
concatenar([H1|T1],L2,[H1|R]) :- concatenar(T1,L2,R).


%>--------------------------------- - - - - - - - - - -  -  -  -  -   -
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

%>--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a involucao do conhecimento: Termo -> {V,F}

involucao( Termo ) :-
    solucoes( Invariante,-Termo::Invariante,Lista ),
    remocao( Termo ),
    teste( Lista ).

remocao( Termo ) :-
    retract( Termo ).
remocao( Termo ) :-
    assert( Termo ),!,fail.


%>--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,desconhecido }

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).


%>--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%>--------------------------------- - - - - - - - - - -  -  -  -  -   -

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

comprimento( S,N ) :-
    length( S,N ).

%>--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Algums predicados para manipulaçao de listas

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


removeDuplicados( L,R ) :- removeDuplicadosAux( L,[],R ).

removeDuplicadosAux( [],R,R ).
removeDuplicadosAux( [H|T],L,R ) :- pertence( H,L ), removeDuplicadosAux( T,L,R ).
removeDuplicadosAux( [H|T],L,R ) :- removeDuplicadosAux( T,[H|L],R ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -

minimo([(P,X,C)],(P,X,C)).
minimo([(Px,X,C)|L],(Py,Y,C1)) :- minimo(L,(Py,Y,C1)), X>Y.
minimo([(Px,X,C)|L],(Px,X,C)) :- minimo(L,(Py,Y,C1)), X=<Y.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Ordenada uma lista: 
myQuickSort([],[]).
myQuickSort([H|T], Ord) :- pivot(H,T,List1,List2), myQuickSort(List1,Ord1), myQuickSort(List2,Ord2),
					       append(Ord1,[H|Ord2],Ord).

%Auxiliar da myQuickSort, tendo um elemento e uma lista, constroi uma lista com os elementos menores ou iguais 
%que o elemento pivot e uma listas com os maiores
pivot(P,[],[],[]).
pivot(P,[H|T1],[H|T2],L3) :- H =< P, pivot(P,T1,T2,L3).
pivot(P,[H|T1],L2,[H|T3]) :- H > P, pivot(P,T1,L2,T3).


%Determina o primeiro elemento de uma lista
primeiro([H|T],H).

%Determina o primeiro elemento de uma lista não vazia
ultimo([H],H).
ultimo([H|T],R) :- ultimo(T,R). 

