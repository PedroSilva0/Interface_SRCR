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
:- dynamic excecao/1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -


% Extensao do predicado utente: #IdUt, Nome, Idade, Morada -> {V,F,D}
-utente(Id, N, I, M) :-
    nao(utente(Id, N, I, M)),
    nao( excecao( utente(Id, N, I, M) ) ).


utente(1,pedro_silva,79,lisboa).
utente(2,diogo_duarte,67,braga).
utente(3,miguel_machado,20,braga).
% utente(5,nuno_campos,2,porto).
utente(6,filipe_oliveira,57,vila_verde).
utente(4,rui_camposinhos,74,porto).
utente(7,cesar_rodrigues,26,vila_verde).
utente(8,ana_pereira,4,lisboa).
utente(9,maria_martins,9,lisboa).

% Conhecimento negativo de Utentes

%-utente(5,nuno_campos,2,porto).
%-utente(6,filipe_oliveira,57,vila_verde).

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

% Conhecimento negativo de Serviços
/*-servico(16,pediatria,hospital_de_vila_verde,vila_verde).
-servico(17,oftalmologia,hospital_de_porto,porto).
-servico(18,cardiologia,hospital_de_vila_verde,vila_verde).
-servico(19,cardiologia,hospital_de_lisboa,lisboa).
-servico(20,dermatologia,hospital_do_braga,braga).*/

% Extensao do predicado consulta: Data, #IdUt, #Serv, Custo  -> {V,F,D}
-consulta(D, U, S, C) :-
    nao(consulta(D, U, S, C)),
    nao( excecao( consulta(D, U, S, C) ) ).

consulta(2015-03-15,3,2,64).
consulta(2015-09-30,3,1,127).

% consulta(2015-08-15,2,4,103).
% consulta(2015-09-01,2,1,156).

% consulta(2014-03-05,1,12,183).

consulta(2015-02-19,8,15,143).
consulta(2012-09-09,8,13,176).

consulta(2014-03-09,4,7,178).
consulta(2013-12-19,4,7,73).

consulta(2012-09-09,7,1,143).
consulta(2012-09-08,7,11,139).
consulta(2013-12-19,7,1,118).

% Conhecimento imperfeito

% INCERTO -- não se sabe a localidade deste utente
utente(5,nuno_campos,2,xpto1).
excecao(utente(A,B,C,D)) :- utente(A,B,C,xpto1).

% IMPRECISO -- o doutor não colucou o custo das consultas. Apenas se sabe que custam entre 100 a 200 euros pela tabela de preços
excecao(consulta(2015-08-15,2,4,C)) :- C>100, C<200. 
excecao(consulta(2015-09-01,2,1,C)) :- C>100, C<200.


% INTERDITO -- consulta da qual nunca se irá saber qual foi o serviço prestado.
consulta(2030-05-12,1,xpto2,2000).
excecao(consulta(A,B,C,D)) :- consulta(A,B,xpto2,D).
nulo(xpto2).
+consulta(A,B,C,D) :-(findall( CS,(consulta(2030-05-12,1,CS,2000),nao(nulo(CS))),LS),
                    length(LS,N), 
                    N==0).

% Evolução do conhecimento imperfeito

%  Conhecimento Incerto
% Pre -> Predicado C1->Campo1 C2->Campo2 C3->Campo3 C4->Campo4 I-> Número que indica que campo é o incerto

/*evolucaoIncerto(Pre,C1,C2,C3,C4,I):- 
    solucoes(Invariante,+Pre::Invariante,Lista),
    insercaoIncerto(Pre,C1,C2,C3,C4,I),
    teste(Lista).*/

%insertTeste(A,B,C) :- assert(utente(B,C)).

evolucaoIncerto(Termo,Excecao):- 
                            evolucao(Termo),
                            evolucao(Excecao). 

% Conhecimento Interdito 

evolucaoInterdito(Termo,Excecao,Nulo,Inv) :-
                                    evolucao(Termo),
                                    evolucao(Excecao),
                                    evolucao(Nulo),
                                    evolucao(Invariante). 

% Conhecimento impreciso

evolucaoImpreciso(Excecao):-
                            evolucao(Excecao).

%excecao(utente(A,B,C,D)) :- utente(A,B,C,xpto1).
%insercaoIncerto(Pre,C1,C2,C3,C4,I) :- I==1, assert(Pre(C1,C2,C3,C4)), assert(excecao(Pre(A,B,C,D)):- Pre(C1,B,C,D)).
%insercaoImpreciso(Pre,C1,C2,C3,C4,I) :- I==1, retract(Pre(C1,C2,C3,C4)), retract(excecao(Pre(A,B,C,D)):- Pre(C1,B,C,D)),!,fail.

%desevolucaoImpreciso(Termo):- 
%    solucoes(Invariante,(+excepcao(Termo)),Lista),
%    removeImpreciso(Termo),
%    teste(Lista).

%removeImpreciso(Termo):-
%    retract(excepcao(Termo)).
%removeImpT2(Termo):-
%    assert(excepcao(Termo)),!,fail. 

%----------------------------------------------------------------------
% Esta evolução dá para conhecimento positivo, negativo e imperfeito impreciso
evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).
    % limpaBase(Termo).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

% limpaBase(Termo) :- retract(excecao(Termo)).
% limpaBase(Termo) :- retract(-Termo).

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
% Invariantes de conhecimento positivo
% Invariante Estrutural:  garantir chaves primarias unicas
+utente( Id, V, I, M ) :: (findall( (Id),(utente( Id, _, _, _ )),S ),
                            length( S,N ), 
                            N == 1)
                            .

+servico(Id, D, I, C) :: (findall( (Id),(servico(Id, _, _, _)),S ),
                            length( S,N ), 
                            N == 1)
                            .

/*+servico(Id, D, I, C) :: (findall( (Id),(-servico(Id, _, _, _)),S ),
                            length( S,N ), 
                            N == 1)
                            .*/                  

%uma consulta e identificada univocamente pelo conjunto Data, IdUtente, IdSev
+consulta(D, U, V, C) :: (findall( (D, U, V), (consulta(D, U, V, _)), S ),
                            length( S,N ), 
                            N == 1)
                            .

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante Estrutural:  nao permitir a insercao de conhecimento repetido
/*+utente( Id, V, I, M ) :: (findall( (Id),(utente( _, V, I, M )),S ),
                            length( S,N ), 
                            N == 1)
                            .   

+servico(Id, D, I, C) :: (findall( (Id),(servico(_, D, I, C)),S ),
                            length( S,N ), 
                            N == 1)
                            .*/             

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante Referencial: nao admitir consultas de utentes inexistentes                            

+consulta(D, U, V, C) ::  (findall((U),(utente( U, _, _, _ )),S ),
                            length( S,N ), 
                             N == 1)
                             .

% Invariante Referencial: nao admitir consultas de servicos
%                                     inexistentes

+consulta(D, U, V, C) ::  (findall((V),(servico( V, _, _, _)),S ),
                            length( S,N ), 
                             N == 1)
                             .

% Invariante Referencial: não remover utentes com consultas

-utente(Id, V, I, M ) ::  (findall((P),(consulta(_, Id, _, _)),S ),
                            length( S,N ), 
                            N == 0)
                            .

% Invariante Referencial: não remover servicos com consultas

-servico(Id, D, I, C) ::  (findall((P),(consulta(_, _, Id, _)),S ),
                            length( S,N ), 
                            N == 0)
                            .


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
/*% Invariantes de conhecimento negativo

% Invariante Estrutural:  garantir chaves primarias unicas
+-servico(Id, D, I, C) :: (findall( (Id),(servico(Id, _, _, _)),S ),
                            length( S,N ), 
                            N == 1)
                            .

+-servico(Id, D, I, C) :: (findall( (Id),(-servico(Id, _, _, _)),S ),
                            length( S,N ), 
                            N == 1)
                            .                                    

% Invariante Estrutural:  nao permitir a insercao de conhecimento repetido
+-servico(Id, D, I, C) :: (findall( (Id),(-servico(_, D, I, C)),S ),
                            length( S,N ), 
                            N == 1)
                            .
*/


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% PONTO 1
% Extensao do predicado serv_por_inst: Serviços,Instituição -> {V,F}

serv_por_inst( LS , I) :- findall(D,servico(_, D, I, _),LS).

