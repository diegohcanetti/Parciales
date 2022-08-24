%% 1

%Atleta/3 (Nombre,Edad,Pais)
atleta(andrea, 23, argentina).
atleta(nicolas, 22, argentina).
atleta(thomas, 25, inglaterra).
atleta(dalilahMuhammad, 27, estadosUnidos).

%Disciplinas/2 (disciplina,Participantes)
disciplina(voleyMasculino, equipo).
disciplina(carrera100MetrosLlanosFemenino, individual).

compiteEnDisciplinas(carrera100MetrosLlanosFemeninos, andrea).
compiteEnDisciplinas(voleyMasculino, nicolas).
compiteEnDisciplinas(carrera400MetrosVallaFemenino, dalilahMuhammad).
%Medalla/3 (medella, disciplina,quien)
medalla(bronce, voleyMasculino, argentina).
medalla(plata, carrera400MetrosVallaFemenino, dalilahMuhammad).

%Evento/3 (Disciplina, ronda,Participantes)
evento(hockeyFemenino, final, argentina).
evento(hockeyFemenino, final, paisesBajos).
evento(cienMetrosEspaldaMasculino, segunda, thomas).

%% 2
vinoAPasear(Atleta) :-
    atleta(Atleta, _, _),
not(compiteEnDisciplinas(_, Atleta)).

%% 3
medallasDelPais(Medalla, Disciplina, Pais) :-
    medalla(Medalla, Disciplina, Pais).
  
medallasDelPais(Medalla, Disciplina, Pais) :-
    medalla(Medalla, Disciplina, Atleta),
    atleta(Atleta, _, Pais).

%% 4
participoEn(Disciplina, Ronda, Atleta) :-
    evento(Disciplina, Ronda, Atleta).
  
participoEn(Disciplina, Ronda, Atleta) :-
    evento(Disciplina, Ronda, Pais),
    atleta(Atleta, _, Pais).
%% 5

dominio(Pais, Disciplina) :-
    atleta(_, _, Pais),
    compiteEnDisciplinas(Disciplina, _),
    forall(medalla(_, Disciplina, PaisOAtleta), medallasDelPais(_, Disciplina, PaisOAtleta)).

%% 6
medallaRapida(Disciplina) :-
    evento(Disciplina, Ronda, _),
    not(otraRonda(Disciplina, Ronda)).

otraRonda(Disciplina,Ronda) :-
    evento(Disciplina, OtraRonda, _),
    Ronda \= OtraRonda.

%% 7

pais(Pais) :-
    atleta(_, _, Pais).
noEsElFuerte(Pais, Disciplina) :-
    pais(Pais),
    disciplina(Disciplina, _),
    
