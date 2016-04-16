%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica estendida
%
% Representacao de conhecimento imperfeito

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic utente/4.
:- dynamic servico/4.
:- dynamic consulta/4.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -


% Extensao do predicado utente: #IdUt, Nome, Idade, Morada -> {V,F,D}

utente(1,pedro_silva,79,braga).
utente(2,diogo_duarte,67,braga).
utente(3,miguel_machado,20).
utente(4,rui_camposinhos,74).
utente(5,nuno_campos,2).
utente(6,filipe_oliveira,57).
utente(7,cesar_rodrigues,26).
utente(8,ana_pereira,4).
utente(9,maria_martins,9).

% Extensao do predicado servico:  #Serv, Descrição, Instituição, Cidade -> {V,F,D}

servico(1,medicina_geral,hospital_de_braga,braga).
servico(2,pediatria,hospital_de_braga,braga).
servico(3,oftalmologia,hospital_de_braga,braga).
servico(4,cardiologia,hospital_de_braga,braga).

servico(5,medicina_geral,hospital_do_porto,porto).
servico(6,pediatria,hospital_do_porto,porto).
servico(7,dermatologia,hospital_do_porto,porto).
servico(8,cardiologia,hospital_do_porto,porto).

servico(9,medicina_geral,hospital_de_vila_verde,vila_verde).
servico(10,oftalmologia,hospital_de_vila_verde,vila_verde).
servico(11,dermatologia,hospital_de_vila_verde,vila_verde).

servico(12,medicina_geral,hospital_de_lisboa,lisboa).
servico(13,pediatria,hospital_de_lisboa,lisboa).
servico(14,dermatologia,hospital_de_lisboa,lisboa).
servico(15,oftalmologia,hospital_de_lisboa,lisboa).


% Extensao do predicado consulta: Data, #IdUt, #Serv, Custo  -> {V,F,D}

consulta(2015-03-15,3,2,).
consulta(2015-09-30,3,1,).

consulta(2015-08-15,diogo_duarte,dr_Diogo_Sousa,cardiologia,hospital_de_braga,).
consulta(2015-09-01,diogo_duarte,dra_Diana_Soares,medicina_geral,hospital_de_vila_verde,).

consulta(2014-03-05,pedro_silva,dr_Placido_Rocha,medicina_geral,hospital_de_lisboa,).

consulta(ana_pereira,dr_Pedro_Barroso,oftalmologia,hospital_de_lisboa,2015-02-19).
consulta(ana_pereira,dra_Juliana_Rocha,pediatria,hospital_de_lisboa,2012-09-09).

consulta(rui_camposinhos,dra_Maria_Martins,dermatologia,hospital_do_porto,2014-03-09).
consulta(rui_camposinhos,dra_Maria_Martins,dermatologia,hospital_do_porto,2013-12-19).

consulta(cesar_rodrigues,dr_Nuno_Machado,medicina_geral,hospital_de_braga,2012-09-09).
consulta(cesar_rodrigues,dra_Laura_Brito,dermatologia,hospital_de_vila_verde,2012-09-08).
consulta(cesar_rodrigues,dr_Nuno_Machado,medicina_geral,hospital_de_braga,2013-12-19).

%----------------------------------------------------------------------
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
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao, falso ) :-
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
%Extensao do predicado jogo: Jogo, Arbitro, Ajudas -> {V,F,D}

-jogo(J, A, V ) :-
    nao(jogo(J, A, V)),
    nao( excecao( jogo(J, A, V) ) ).

% Invariante 1:  nao permitir a insercao de conhecimento
%                         repetido (numero do jogo e chave)
+jogo(J, A, V) :: (solucoes( (J, A, V),(jogo(J, _, _)), S),
                  comprimento( S,N ), N == 1
                  ).

% Invariante 2:  um arbitro nao pode apitar mais do que tres partidas
+jogo(J, A, V) :: (solucoes( A, jogo(_, A, _), S),
                  comprimento( S,N ), N < 4
                  ).

% Invariante 3:  um arbitro nao pode apitar duas partidas seguidas
+jogo(J, A, V) :: (solucoes( A, (jogo(J, A, C),
                                jogo(J1, A, C1),
                                J2 is J+1,
                                J1 == J2), 
                            S),
                  comprimento( S,N ), N == 0
                  ).


%--- JOGO 1 ---
jogo(1,almeida,500).

%--- JOGO 2 ---
jogo(2,baltazar,xpto1).
excecao(jogo(J,A,V)) :-
      jogo(J,A,xpto1).

%--- JOGO 3 ---
excecao(jogo(3,costa,500)).
excecao(jogo(3,costa,2000)).

%--- JOGO 4 ---
excecao(jogo(4,duarte,X)) :- X > 250, X < 750.

%--- JOGO 5 ---
jogo(5,edgar,xpto2).
excecao(jogo(J,A,V)) :-
      jogo(J,A,xpto2).
nulo(xpto2).
+jogo(J, A, V) :: (solucoes( (J, A, V),(jogo(5,_,Fs),nao(nulo(Fs))),S ),
                  comprimento( S,N ), N == 0 
                  ).

%--- JOGO 6 ---
jogo(6,francisco,250).
excecao(jogo(6,francisco,X)) :- X > 5000.

%--- JOGO 7 ---
-jogo(7,guerra,2500).
jogo(7,guerra,xpto3).
excecao(jogo(J,A,V)) :-
      jogo(J,A,xpto3).

%--- JOGO 8 ---
excecao(jogo(8,ivo,X)) :- 
      proximo(3000,Sup,Inf),
      X > Inf, X < Sup.

%--- JOGO 9 ---
excecao(jogo(9,helder,X)) :- 
      cerca(1000,Sup,Inf),
      X > Inf, X < Sup.

%--- Auxiliar ---
cerca(X, Sup, Inf) :-
      Sup is X * 1.25,
      Inf is X * 0.75.  

proximo(X, Sup, Inf) :-
      Sup is X * 1.10,
      Inf is X * 0.90. 



