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


% Extensao do predicado utente: IdUt, Nome, Idade, Morada -> {V,F,D}
-utente(Id, N, I, M) :-
    nao(utente(Id, N, I, M)),
    nao( excecao( utente(Id, N, I, M) ) ).


utente(1,pedro_silva,79,lisboa).
utente(2,diogo_duarte,67,braga).
utente(3,miguel_machado,20,braga).
utente(4,rui_camposinhos,74,porto).
utente(5,nuno_campos,2,).
utente(6,filipe_oliveira,57,).
utente(7,cesar_rodrigues,26,vila_verde).
utente(8,ana_pereira,4,lisboa).
utente(9,maria_martins,9,).

% Extensao do predicado servico:  #Serv, Descrição, Instituição, Cidade -> {V,F,D}
-servico(Id, D, I, C) :-
    nao(servico(Id, D, I, C)),
    nao( excecao( servico(Id, D, I, C) ) ).


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
-consulta(D, U, S, C) :-
    nao(consulta(D, U, S, C)),
    nao( excecao( consulta(D, U, S, C) ) ).

consulta(2015-03-15,3,2,64).
consulta(2015-09-30,3,1,127).

consulta(2015-08-15,2,4,103).
consulta(2015-09-01,2,1,156).

consulta(2014-03-05,1,12,183).

consulta(2015-02-19,8,dr_Pedro_Barroso,15,143).
consulta(2012-09-09,8,dra_Juliana_Rocha,13,176).

consulta(2014-03-09,4,7,178).
consulta(2013-12-19,4,7,73).

consulta(2012-09-09,7,1,143).
consulta(2012-09-08,7,11,139).
consulta(2013-12-19,7,1,118).

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

desevolucao( Termo ) :- findall(I,-Termo::I,Li),
    teste(Li),
    retract(Termo).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}

demo([],[]).
demo([Questao|Questoes],[verdadeiro|S]) :-
    Questao,
    demo(Questoes,S).

demo([Questao|Questoes],[falso|S]) :-
    -Questao,
    demo(Questoes,S).

demo([Questao|Questoes],[desconhecido|S]) :-
    nao( Questao ),
    nao( -Questao ),
    demo(Questoes,S).


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
% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido (chaves diferentes)
+utente( Id, N, I, M ) :: (findall( (Id),(utente( Id, N, I, M )),S ),
                            length( S,N ), 
                            N == 1)
                            .

+servico(Id, D, I, C) :: (findall( (Id),(servico(Id, D, I, C)),S ),
                            length( S,N ), 
                            N == 1)
                            .              

%uma consulta e identificada univocamente pelo conjunto Data, IdUtente, IdSev
+consulta(D, U, S, C) :: (findall( (D, U, S), (consulta(D, U, S, C)), S ),
                            length( S,N ), 
                            N == 1)
                            .

% Invariante Referencial: nao admitir consultas de utentes
%                                     inexistentes

+consulta(D, U, S, C) ::  (findall((U),(utente( U, _, _, _ )),S ),
                            length( S,N ), 
                             N == 1)
                             .

% Invariante Referencial: nao admitir consultas de servicos
%                                     inexistentes

+consulta(D, U, S, C) ::  (findall((S),(servico( S, _, _, _)),S ),
                            length( S,N ), 
                             N == 1)
                             .

% Invariante Referencial: não remover utentes com consultas

-utente(Id, N, I, M ) ::  (findall((P),(consulta(_, Id, _, _)),S ),
                            length( S,N ), 
                            N == 0)
                            .

% Invariante Referencial: não remover servicos com consultas

-servico(Id, D, I, C) ::  (findall((P),(consulta(_, _, Id, _)),S ),
                            length( S,N ), 
                            N == 0)
                            .


