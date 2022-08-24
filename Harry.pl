%% Punto 01
casa(gryffindor).
casa(slytherin).
casa(hufflepuff).
casa(ravenclaw).

sangre(harry, mestiza).
sangre(draco, pura).
sangre(hermione, impura).

mago(Mago):-
  sangre(Mago, _).

permiteEntrar(Casa, Mago) :- 
    casa(Casa),
    mago(Mago),
    Casa \= slytherin.
permiteEntrar(slytherin, Mago):-
    sangre(Mago, TipoDeSangre),
    TipoDeSangre \= impura.


%% Sin abstracción
/*
  tieneCaracterApropiado(Mago, Casa):-
    caracteristicas(Mago, Caracteristicas),
    casa(Casa),
    forall(caracteristicaBuscada(Casa, Caracteristica), member(Caracteristica, Caracteristicas)).
    
%% Solución con member
caracteristicas(harry, [coraje, amistad, orgullo, inteligencia]).
caracteristicas(draco, [inteligencia, orgullo]).
caracteristicas(hermione, [inteligencia, orgullo, responsabilidad]).

%% Con Abstracción 

tieneCaracteristica(Mago, Caracteristica):-
  caracteristicas(Mago, Caracteristicas),
  member(Caracteristica, Caracteristicas).

tieneCaracterApropiado(Mago, Casa):-
  casa(Casa), 
  mago(Mago),
  forall(caracteristicaBuscada(Casa, Caracteristica), tieneCaracteristica(Mago, Caracteristica)).
*/

%% Solución Más cómoda
tieneCaracteristica(harry, coraje).
tieneCaracteristica(harry, orgullo).
tieneCaracteristica(harry, amistad).
tieneCaracteristica(harry, inteligencia).
tieneCaracteristica(draco, inteligencia).
tieneCaracteristica(draco, orgullo).
tieneCaracteristica(hermione, inteligencia).
tieneCaracteristica(hermione, orgullo).
tieneCaracteristica(hermione, responsabilidad).

caracteristicaBuscada(gryffindor, coraje).
caracteristicaBuscada(slytherin, orgullo).
caracteristicaBuscada(slytherin, inteligencia).
caracteristicaBuscada(ravenclaw, inteligencia).
caracteristicaBuscada(ravenclaw, responsabilidad).
caracteristicaBuscada(hufflepuff, amistad).

tieneCaracterApropiado(Mago, Casa):-
  casa(Casa),
  mago(Mago),
  forall(caracteristicaBuscada(Casa, Caracteristica),
         tieneCaracteristica(Mago, Caracteristica)).


odiaQueLoMande(harry, slytherin).
odiaQueLoMande(draco, hufflepuff).

puedeQuedarSeleccionado(Mago, Casa) :-
  tieneCaracterApropiado(Mago, Casa),  %Se puede usar directamente pq es inversible
  permiteEntrar(Casa, Mago),
  not(odiaQueLoMande(Mago,Casa)).
puedeQuedarSeleccionado(hermione, gryffindor).

%Punto 2

hizo(harry, fueraDeCama).
hizo(hermione, irA(tercerPiso)).
hizo(hermione, irA(seccionRestringida)).
hizo(harry, irA(bosque)).
hizo(harry, irA(tercerPiso)).
hizo(draco, irA(mazmorras)).
hizo(ron, buenaAccion(50, ganarAlAjedrezMagico)).
hizo(hermione, buenaAccion(50, salvarASusAmigos)).
hizo(harry, buenaAccion(60, ganarleAVoldemort)).

hizoAlgunaAccion(Mago):-
  hizo(Mago, _).
hizoAlgoMalo(Mago):-
  hizo(Mago, Accion),
  puntajeQueGenera(Accion, Puntaje),
  Puntaje < 0.

puntajeQueGenera(fueraDeCama, -50).
puntajeQueGenera(irA(Lugar), PuntajeQueResta):-
  lugarProhibido(Lugar, Puntos),
  PuntajeQueResta is Puntos * -1.
puntajeQueGenera(buenaAccion(Puntaje, _), Puntaje).

lugarProhibido(bosque, 50).
lugarProhibido(seccionRestringida, 10).
lugarProhibido(tercerPiso, 75).

esBuenAlumno(Mago):-
  hizoAlgunaAccion(Mago),
  not(hizoAlgoMalo(Mago)).

esRecurrente(Accion):-
  hizo(Mago, Accion),
  hizo(OtroMago, Accion),
  Mago \= OtroMago.

% 2
esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

puntajeTotalDeCasa(Casa, PuntajeTotal):-
  esDe(_, Casa),
  findall(Puntos,(esDe(Mago, Casa), puntosQueObtuvo(Mago, _, Puntos)),ListaPuntos),
  sum_list(ListaPuntos, PuntajeTotal).

puntosQueObtuvo(Mago, Accion, Puntos):-
  hizo(Mago, Accion),
  puntajeQueGenera(Accion, Puntos).

% 3

casaGanadora(Casa):-
  puntajeTotalDeCasa(Casa, PuntajeMayor),
  forall((puntajeTotalDeCasa(OtraCasa, PuntajeMenor), Casa \= OtraCasa),
         PuntajeMayor > PuntajeMenor).

casaGanadora2(Casa):-
  puntajeTotalDeCasa(Casa, PuntajeMayor),
  not((puntajeTotalDeCasa(_, OtroPuntaje), OtroPuntaje > PuntajeMayor)).
