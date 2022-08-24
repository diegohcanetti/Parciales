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

%%% 7
noEsElFuerte(Pais, Disciplina) :-
    pais(Pais),
    disciplina(Disciplina, _),
    noCompitio(Pais, Disciplina).
  
  % noCompitio se fija si:
  %    * no participó
  %    * se volvió en ronda inicial
  
  noCompitio(Pais, Disciplina) :-
    forall(paisOAtletaDelPais(AtletaOPais, Pais), not(evento(Disciplina, _, AtletaOPais))).
  
  noCompitio(Pais, Disciplina) :-
    not(paisEstuvoEnRondaNoInicial(Pais, Disciplina)).
  
  paisEstuvoEnRondaNoInicial(Pais, Disciplina) :-
    evento(Disciplina, Ronda, Participante),
    paisOAtletaDelPais(Participante, Pais),
    disciplina(Disciplina, Tipo),
    not(rondaInicialPara(Tipo, Ronda)).
  
  % que el país NO esté en una ronda que NO es inicial
  % todas las rondas de ese país tienen que ser la inicial
  rondaInicialPara(equipo, faseDeGrupos).
  rondaInicialPara(individual, ronda1).
  
  
  %%% 8
  medallasEfectivas(CuentaFinal, Pais) :-
    pais(Pais),
    findall(Puntos, puntosPorMedallaParaPais(Pais, Puntos), ListaDePuntos),
    sumlist(ListaDePuntos, CuentaFinal).
  
  puntosPorMedallaParaPais(Pais, Puntos) :-
    medallasDelPais(Medalla, _, Pais),
    puntosPorMedalla(Medalla, Puntos).
  
  puntosPorMedalla(oro, 3).
  puntosPorMedalla(plata, 2).
  puntosPorMedalla(bronce, 1).
  
  
  %%% 9
  laEspecialidad(Atleta) :-
    atleta(Atleta, _, _),
    not(vinoAPasear(Atleta)),
    forall(participoEn(Disciplina, _, Atleta), salio1o2(Atleta, Disciplina)).
  
  salio1o2(Atleta, Disciplina) :-
    medalla(Medalla, Disciplina, AtletaOPais),
    paisOAtletaDelPais(Atleta, AtletaOPais),
    medallaDeOroOPlata(Medalla).
  
  medallaDeOroOPlata(oro).
  medallaDeOroOPlata(plata).

%% Consignas https://docs.google.com/document/d/1CXilm0efIbrbOttX4GZsOcfUrkGjCe2oKWzUHEMrP-s/edit
    
