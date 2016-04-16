%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3



%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% EXERCICIO 1 - Instituicao de saude



%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% SICStus PROLOG: Declaracoes iniciais



:- set_prolog_flag( discontiguous_warnings,off ).

:- set_prolog_flag( single_var_warnings,off ).

:- set_prolog_flag( unknown,fail ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).

:- dynamic instituicao/1.

:- dynamic utente/1.

:- dynamic consulta/5.

:- dynamic profissional/3.

:- dynamic servico/2. 

%%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Extensao do predicado instituicao: Instituição -> {V,F}

instituicao(hospital_de_braga).
instituicao(hospital_do_porto).
instituicao(hospital_de_vila_verde).
instituicao(hospital_de_lisboa).

% Extensao do predicado utente: Utente -> {V,F}

utente(pedro_silva).
utente(diogo_duarte).
utente(miguel_machado).
utente(rui_camposinhos).
utente(nuno_campos).
utente(filipe_oliveira).
utente(cesar_rodrigues).
utente(ana_pereira).
utente(maria_martins).

% Extensao do predicado servico: Serviço,Instituição -> {V,F}

servico(medicina_geral,hospital_de_braga).
servico(pediatria,hospital_de_braga).
servico(oftalmologia,hospital_de_braga).
servico(cardiologia,hospital_de_braga).

servico(medicina_geral,hospital_do_porto).
servico(pediatria,hospital_do_porto).
servico(dermatologia,hospital_do_porto).
servico(cardiologia,hospital_do_porto).

servico(medicina_geral,hospital_de_vila_verde).
servico(oftalmologia,hospital_de_vila_verde).
servico(dermatologia,hospital_de_vila_verde).

servico(medicina_geral,hospital_de_lisboa).
servico(pediatria,hospital_de_lisboa).
servico(dermatologia,hospital_de_lisboa).
servico(oftalmologia,hospital_de_lisboa).


% Extensao do predicado profissional: Profissional,Serviço,Instituição -> {V,F}

profissional(dr_Andre_Maia,pediatria,hospital_de_braga).
profissional(dr_Ricardo_Pereira,pediatria,hospital_de_braga).
profissional(dr_Diogo_Sousa,cardiologia,hospital_de_braga).
profissional(dr_Ricardo_Pereira,medicina_geral,hospital_de_braga).
profissional(dr_Nuno_Machado,medicina_geral,hospital_de_braga).

profissional(dra_Diana_Machado,medicina_geral,hospital_do_porto).
profissional(dra_Sandra_Machado,medicina_geral,hospital_do_porto).
profissional(dra_Maria_Martins,dermatologia,hospital_do_porto).

profissional(dra_Diana_Soares,medicina_geral,hospital_de_vila_verde).
profissional(dra_Laura_Brito,dermatologia,hospital_de_vila_verde).
profissional(dr_Ricardo_Pereira,medicina_geral,hospital_de_vila_verde).

profissional(dr_Placido_Rocha,medicina_geral,hospital_de_lisboa).
profissional(dra_Juliana_Rocha,pediatria,hospital_de_lisboa).
profissional(dr_Pedro_Barroso,oftalmologia,hospital_de_lisboa).


% Extensao do predicado consulta: consulta,Profissional,Serviço,Instituição -> {V,F}

consulta(miguel_machado,dr_Andre_Maia,pediatria,hospital_de_braga,2015-03-15).
consulta(miguel_machado,dr_Nuno_Machado,medicina_geral,hospital_de_braga,2015-09-30).

consulta(diogo_duarte,dr_Diogo_Sousa,cardiologia,hospital_de_braga,2015-08-15).
consulta(diogo_duarte,dra_Diana_Soares,medicina_geral,hospital_de_vila_verde,2015-09-01).

consulta(pedro_silva,dr_Placido_Rocha,medicina_geral,hospital_de_lisboa,2014-03-05).

consulta(ana_pereira,dr_Pedro_Barroso,oftalmologia,hospital_de_lisboa,2015-02-19).
consulta(ana_pereira,dra_Juliana_Rocha,pediatria,hospital_de_lisboa,2012-09-09).

consulta(rui_camposinhos,dra_Maria_Martins,dermatologia,hospital_do_porto,2014-03-09).
consulta(rui_camposinhos,dra_Maria_Martins,dermatologia,hospital_do_porto,2013-12-19).

consulta(cesar_rodrigues,dr_Nuno_Machado,medicina_geral,hospital_de_braga,2012-09-09).
consulta(cesar_rodrigues,dra_Laura_Brito,dermatologia,hospital_de_vila_verde,2012-09-08).
consulta(cesar_rodrigues,dr_Nuno_Machado,medicina_geral,hospital_de_braga,2013-12-19).





% PONTO 1
% Extensao do predicado serv_por_inst: Serviços,Instituição -> {V,F}

serv_por_inst( LS , Ins) :- findall(S,servico(S,Ins),LS).

%PONTO 2
% Extensao do predicado ute_por_inst: Instituição,utentes -> {V,F}

ute_por_inst(Ins, LU) :- setof(S,A^B^C^consulta(S,A,B,Ins,C),LU).

%PONTO 3
% Extensao do predicado uten_por_serv: utentes,Serviço -> {V,F}

uten_por_serv( LU , Ser) :- setof(U,A^B^C^consulta(U,A,Ser,B,C),LU).

%PONTO 4
% Extensao do predicado uten_por_serv_por_inst: utentes,Serviço,Instituição -> {V,F}

uten_por_serv_por_inst(LU, Ser , Inst) :- setof(U,A^B^consulta(U,A,Ser,Inst,B),LU).

%PONTO 5
% Extensao do predicado ins_com_servicos: Serviços,Instituição -> {V,F}

ins_com_servicos([],L).
ins_com_servicos([X|L],LI) :- findall(A,servico(X,A),L2),
                              ins_com_servicos(L,L4),
                              intersection(L2,L4,L3),
                              sort(L3,LI).                              

intersection([], _, []) :- !.
intersection([X|T], L, Intersect) :-
                            memberchk(X, L), !,
                            Intersect = [X|R],
                            intersection(T, L, R).
intersection([_|T], L, R) :-
                            intersection(T, L, R).

%PONTO 6
% Extensao do predicado serv_not_in_inst: Serviço,Instituição -> {V,F}

serv_not_in_inst(LS , Inst) :- setof(Ser,A^servico(Ser,A),L1),
								                findall(Ser2,servico(Ser2,Inst),L2),
								                subtract(L1,L2,LS).

subtract([], _, []) :- !.
subtract([A|C], B, D) :-
    memberchk(A, B), !,
    subtract(C, B, D).
subtract([A|B], C, [A|D]) :-
    subtract(B, C, D).

%PONTO 7
%Extensao do predicado ins_por_pro: Profissional,Instituição -> {V,F}

ins_por_pro(Pro, LI) :- setof(S,A^profissional(Pro,A,S),LI).

%PONTO 8
%Extensao do predicado tudo_por_utente: utente,Lista -> {V,F}

ins_por_utente(Ute, LI) :- setof(S,A^B^C^consulta(Ute,A,B,S,C), LI).
ser_por_utente(Ute,LS) :- setof(S,A^B^C^consulta(Ute,A,S,B,C),LS).
pro_por_utente(Ute,LP) :- setof(S,A^B^C^consulta(Ute,S,A,B,C),LP).

concatenar([], L2, L2).
concatenar([X|R] , L2 , [X|L]) :- concatenar(R,L2,L).	

tudo_por_utente(Ute, L) :-  ins_por_utente(Ute,LI) , ser_por_utente(Ute,LS) , pro_por_utente(Ute,LP), concatenar(LS,LI,LA), 
							concatenar(LA,LP,L). 



%PONTO 9
% Extensão do predicado que permite a evolucao do conhecimento evolucao: Termo ->{V,F}

evolucao( Termo ) :- findall(I,+Termo::I,Li),
						inserir(Termo),
						teste(Li).

inserir(T):- assert(T).
inserir(T):- retract(T),!,fail.

teste([]).
teste([I|Li]):- I,teste(Li).

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido

+instituicao( F) :: (findall( (F),(instituicao( F )),S ),
                  length( S,N ), 
				  N == 1)
                  .

+utente( F) :: (findall( (F),(utente( F )),S ),
                  length( S,N ), 
                          N == 1)
                  .

+servico( F,P ) :: (findall( (F,P),(servico( F,P )),S ),
                  length( S,N ), 
				  N == 1)
                  .              

+consulta( F,P,X,Z,A ) :: (findall( (F,P,X,Z,A),(consulta( F,P,X,Z,A )),S ),
                  length( S,N ), 
				  N == 1)
                  .              

+profissional( F,P,Z ) :: (findall( (F,P,Z),(profissional( F,P,Z )),S ),
                  length( S,N ), 
				  N == 1)
                  .

% Invariante Referencial: nao admitir serviços em instituições 
% 						  inexistentes

+servico( F,P ) ::  (findall((P),(instituicao( P )),S ),
                  length( S,N ), 
				  N == 1)
                  .

% Invariante Referencial: nao admitir profissionais em instituições 
% 						  inexistentes

+profissional( F,P,I ) ::  (findall((I),(instituicao( I )),S ),
                  length( S,N ), 
				  N == 1)
                  .

% Invariante Referencial: nao admitir profissionais de serviços 
% 						  inexistentes

+profissional( F,P,I ) ::  (findall((P),(servico( P,I )),S ),
                  length( S,N ), 
				  N == 1)
                  .

% Invariante Referencial: nao admitir consultas de serviços 
% 						  inexistentes

+consulta( F,P,I,Z,A ) ::  (findall((I),(servico( I,Z )),S ),
                  length( S,N ), 
				  N == 1)
                  .

% Invariante Referencial: nao admitir consultas de profissionais 
% 						  inexistentes

+consulta( F,P,I,Z,A ) ::  (findall((P),(profissional( P,I,Z )),S ),
                  length( S,N ), 
				  N == 1)
                  .

% Invariante Referencial: nao admitir consultas de instituições
% 						  inexistentes

+consulta( F,P,I,Z,A ) ::  (findall((Z),(instituicao( Z )),S ),
                  length( S,N ), 
				  N == 1)
                  .

% Invariante Referencial: nao admitir consultas de utentes
%                                     inexistentes

+consulta( F,P,I,Z,A ) ::  (findall((F),(utente( F )),S ),
                  length( S,N ), 
                          N == 1)
                  .


%PONTO 10
% Extensão do predicado que permite a desevolução do conhecimento desevolucao: Termo ->{V,F}

desevolucao( Termo ) :- findall(I,-Termo::I,Li),
						teste(Li),
						retract(Termo).


% Invariante Referencial: não remover profissionais com consultas

-profissional( F,P,I ) ::  (findall((A),(consulta( A,F,P,I,_ )),S ),
                  length( S,N ), 
				          N == 0)
                  .

% Invariante Referencial: não remover serviços com profissionais

-servico( F,P ) ::  (findall((F,P),(profissional( _,F,P )),S ),
                  length( S,N ), 
				          N == 0)
                  .

% Invariante Referencial: não remover serviços com consultas

-servico( F,P ) ::  (findall((F,P),(consulta( _,_,F,P,_ )),S ),
                  length( S,N ), 
				          N == 0)
                  .

% Invariante Referencial: não remover instituições com profissionais

-instituicao( P ) ::  (findall((P),(profissional( _,_,P )),S ),
                  length( S,N ), 
				          N == 0)
                  .

% Invariante Referencial: não remover instituições com consultas

-instituicao( P ) ::  (findall((P),(consulta( _,_,_,P,_ )),S ),
                  length( S,N ), 
				          N == 0)
                  .

% Invariante Referencial: não remover instituições com serviços

-instituicao( P ) ::  (findall((P),(servico( _,P )),S ),
                  length( S,N ), 
				          N == 0)
                  .

% Invariante Referencial: não remover utentes com consultas

-utente( P ) ::  (findall((P),(consulta( P,_,_,_,_ )),S ),
                  length( S,N ), 
                                  N == 0)
                  .


%FUNCIONALIDADES EXTRA

%Funcionalidade extra 1
%Predicado que permite obter as consultas por professional, ordenadas por data
consultas_por_profissional(Pro,LC) :- setof((E,A,Pro,C,D),consulta(A,Pro,C,D,E),LC).

%Funcionalidade extra 2
%Predicado que permite obter as consultas por utente, ordenadas por data
consultas_por_utente(Ute,LC) :- setof((E,Ute,B,C,D),consulta(Ute,B,C,D,E),LC).

%Funcionalidade extra 3
%Predicado que permite obter as consultas por instituição, ordenadas por data
consultas_por_instituicao(Ins,LC) :- setof((E,Ute,B,C,Ins),consulta(Ute,B,C,Ins,E),LC).


%Funcionalidade extra 4
%Predicado que permite obter as consultas por serviço, ordenadas por data
consultas_por_servico(Ser,LC) :- setof((E,A,B,Ser,D),consulta(A,B,Ser,D,E),LC).


%Funcionalidade extra 5
%Predicado que permite calcular quantos utentes existem na base de conhecimento

n_utentes(N) :- findall(S,utente(S),LU), length(LU,N).

%Funcionalidade extra 6
%Predicado que permite calcular quantos utentes existem numa Instituição

% n_utentes_por_instituicao(Ins,N) :- findall(S,consulta(S,_,_,Ins,_),L),sort(L,LU), length(LU,N).
n_utentes_por_instituicao(Ins,N) :- setof(S,A^B^C^consulta(S,A,B,Ins,C),LU),length(LU,N).

%Funcionalidade extra 7
%Predicado que permite calcular quantos servicos existem numa Instituição

n_servicos_por_instituicao(Ins,N) :- findall(S,servico(S,Ins),LS), length(LS,N).

%Funcionalidade extra 8
%Predicado que permite calcular quantos profissionais existem numa Instituição

n_profissionais_por_instituicao(Ins,N) :- setof(S,A^profissional(S,A,Ins),LS), length(LS,N).

%Funcionalidade extra 9
%Predicado que permite calcular quantos profissionais existem num serviço em todas as instituições

n_profissionais_por_servico(Ser,N) :- setof(S,A^profissional(S,Ser,A),LP), length(LP,N).

%Funcionalidade extra 10
%Predicado que permite calcular quantos profissionais existem num serviço numa instituições

n_profissionais_por_servico_por_instituicao(Ser,Ins,N) :- findall(S,profissional(S,Ser,Ins),LP), length(LP,N).

