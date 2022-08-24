:- discontiguous cocinaBien/2.
:- discontiguous caloriasSegunTipo/2.
:- discontiguous criticaPositivaSegunCritico/2.
:- discontiguous saludable/1.

%rata(nombre, dondeViven)
rata(remy, gusteaus).
rata(emile, bar).
rata(django, pizzeria).

%cocineros(nombre, platoCocinan, experiencia)
cocina(linguini, ratatouille, 3).
cocina(linguini, sopa, 5).
cocina(colette, salmonAsado, 9).
cocina(horst, ensaladaRusa, 8).

%trabajaEn(restaurante, empleado)
trabajaEn(gusteaus, linguini).
trabajaEn(gusteaus, colette).
trabajaEn(gusteaus, skinner).
trabajaEn(gusteaus, horst).
trabajaEn(cafeDes2Moulins, amelie).

%% 1 BUENO
restaurante(Restaurante) :-
    trabajaEn(Restaurante, _).

inspeccionSatisfactoria(Restaurante) :-
    restaurante(Restaurante),
    not(rata(_, Restaurante)).

%% 2 
chef(Empleado, Restaurante) :-
    trabajaEn(Restaurante, Empleado),
    cocina(Empleado, _, _).


empleado(Empleado) :-
    cocina(Empleado, _, _).

%% 3 Bueno
chefcito(Rata) :-
    rata(Rata, Restaurante),
    trabajaEn(Restaurante, linguini).

%% 4 Casi Perfecto
cocinaBien(Persona, Plato) :-
    cocina(Persona, Plato, Experiencia),
    Experiencia > 7.

plato(UnPlato) :-
    cocina(_, UnPlato, _).
  
  cocinaBien(remy, UnPlato) :-
    plato(UnPlato).

%% 5
encargadoDe(UnEncargado, UnPlato, UnRestaurante) :-
    cocina(UnEncargado, UnPlato, UnRestaurante),
    forall(cocina(OtraPersona, UnPlato, UnRestaurante), cocinaMejor(UnEncargado, OtraPersona, UnPlato)).
  
  cocinaMejor(UnEncargado, OtraPersona, UnPlato) :-
    cocina(UnEncargado, UnPlato, UnaExperiencia),
    cocina(OtraPersona, UnPlato, OtraExperiencia),
    UnaExperiencia >= OtraExperiencia.

%% 6 REPASAR CONCEPTOS
grupo(profiterol).
plato(ensaladaRusa, entrada([papa, zanahoria, arvejas, huevo, mayonesa])).
plato(bifeDeChorizo, principal(pure, 25)).
plato(frutillasConCrema, postre(265)).

%% Menos de 75
%% Entradas suman 15
%% Platos principales suman 5 calorías x minuto.
%% Guarniciones: Papas 50, Pure 20, Ensalada 0.

saludable(UnPlato) :-
    plato(UnPlato, TipoDePlato),
    caloriasSegunTipo(TipoDePlato, UnasCalorias),
    UnasCalorias < 75.

caloriasSegunTipo(entrada(Ingredientes), UnasCalorias) :-
    length(Ingredientes, CantidadDeIngredientes),
    UnasCalorias is CantidadDeIngredientes * 15.
  
caloriasSegunTipo(principal(UnaGuarnicion, UnosMinutosDeCoccion), UnasCalorias) :-
    caloriasSegunGuarnicion(UnaGuarnicion, CaloriasGuarnicion),
    UnasCalorias is UnosMinutosDeCoccion * 5 + CaloriasGuarnicion.
  
caloriasSegunGuarnicion(papasFritas, 50).
caloriasSegunGuarnicion(pure, 20).
caloriasSegunGuarnicion(ensalada, 0).
  
caloriasSegunTipo(postre(UnasCalorias), UnasCalorias).
  
saludable(UnPostre) :-
    grupo(UnPostre).
 
%% 7 Complicado Algunos
criticaPositiva(Restaurante, Critico) :-
    inspeccionSatisfactoria(Restaurante),
    criticaPositivaSegunCritico(Restaurante, Critico).

criticaPositivaSegunCritico(Restaurante, antonEgo) :-
    esEspecialista(Restaurante, ratatouille).

esEspecialista(Restaurante, Plato) :-
    forall(chef(Empleado, Restaurante), cocinaBien(Empleado, Plato)).

criticaPositivaSegunCritico(Restaurante, christophe) :-
    tengaMasDeTresChefs(Restaurante).

tengaMasDeTresChefs(Restaurante) :-
    findall(Empleado, chef(Empleado,Restaurante), UnosChefs),
    length(UnosChefs, CantidadDeChefs),
    CantidadDeChefs > 3.

criticaPositivaSegunCritico(Restaurante, cormillot) :-
    todosPlatosSaludables(Restaurante), 
    todasLasEntradasTienenZanahoria(Restaurante).

todosPlatosSaludables(UnRestaurante) :-
    forall(seCocinaEn(UnPlato, UnRestaurante), saludable(UnPlato)).
  
seCocinaEn(UnPlato, UnRestaurante) :-
    cocinaEn(_, UnPlato, UnRestaurante).
  
  cocinaEn(UnEmpleado, UnPlato, UnRestaurante) :-
    trabajaEn(UnEmpleado, UnRestaurante),
    cocina(UnEmpleado, UnPlato, _).
  
todasLasEntradasTienenZanahoria(UnRestaurante) :-
    forall(entradaCocinadaEn(UnaEntrada, UnRestaurante), tieneZanahoria(UnaEntrada)).
  
  entradaCocinadaEn(UnaEntrada, UnRestaurante):-
    seCocinaEn(UnaEntrada, UnRestaurante),
    plato(UnaEntrada, entrada(_)).
  
  tieneZanahoria(UnaEntrada) :-
    plato(UnaEntrada, entrada(Ingredientes)),
    member(zanahoria, Ingredientes).


https://www.utnianos.com.ar/foro/attachment.php?aid=20208