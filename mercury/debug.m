/**
 * 

 * @author Pedro Mariano
 * @version 1.0 2014/01/22
 */
:- module debug.

:- interface.

:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module int, list, string, solutions.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

:- type geometry --->
	wellmixed ;
	lattice(
		xSize         :: int ,
		ySize         :: int ,
		neighbourhood :: neighbourhood ,
		boundary      :: boundary
	) .

:- type neighbourhood --->
	moore ;
	hexagonal ;
	vonNeumann .

:- type boundary --->
	torus ;
	ring ;
	closed .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

main(!IO) :-
	io.read_line_as_string(ILine, !IO),
	(if
		ILine = ok(Line),
		string.words(Line) = [SX, SY, SI],
		string.to_int(SX, X),
		string.to_int(SY, Y),
		string.to_int(SI, I)
	then
		%Gss = solutions.solutions(lattice(X, Y)),
		solutions.solutions(latticeWorst(X, Y), Gss),
		io.print(Gss, !IO),
		io.nl(!IO),

		io.print(solutions.solutions(latticeWorst(X, Y)) `with_type` list(geometry), !IO), io.nl(!IO),
		io.print(solutions.solutions(latticeWrong(X, Y)) `with_type` list(geometry), !IO), io.nl(!IO),
		io.print(solutions.solutions(latticeGood(X, Y)) `with_type` list(geometry), !IO), io.nl(!IO),
		
		PredTest =
		(pred(G::in, IOdi::di, IOuo::uo) is det :-
			io.format("%20s => %30s\n", [s(string(G)), s(string(makeConnections(G, I)))], IOdi, IOuo)
		),
		list.foldl(PredTest, Gss, !IO),
		main(!IO)
	else
		true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions


:- pred latticeWorst(int, int, geometry).
:- mode latticeWorst(in, in, out) is nondet.

latticeWorst(X, Y, G) :-
	G^xSize = X,
	G^ySize = Y,
	(
		G^boundary = torus ;
		G^boundary = ring ;
		G^boundary = closed
	),
	(
		G^neighbourhood = moore ;
		G^neighbourhood = hexagonal ;
		G^neighbourhood = vonNeumann
	).

:- pred latticeWrong(int, int, geometry).
:- mode latticeWrong(in, in, out) is nondet.

latticeWrong(X, Y, G) :-
	list.member(G^boundary, [torus, ring, closed]),
	list.member(G^neighbourhood, [moore, hexagonal, vonNeumann]),
	G^xSize = X,
	G^ySize = Y.


:- pred latticeGood(int, int, geometry).
:- mode latticeGood(in, in, out) is multi.

latticeGood(X, Y, lattice(X, Y, vonNeumann, torus)).
latticeGood(X, Y, lattice(X, Y, vonNeumann, ring)).
latticeGood(X, Y, lattice(X, Y, vonNeumann, closed)).
latticeGood(X, Y, lattice(X, Y, hexagonal, torus)).
latticeGood(X, Y, lattice(X, Y, hexagonal, ring)).
latticeGood(X, Y, lattice(X, Y, hexagonal, closed)).
latticeGood(X, Y, lattice(X, Y, moore, torus)).
latticeGood(X, Y, lattice(X, Y, moore, ring)).
latticeGood(X, Y, lattice(X, Y, moore, closed)).


:- func makeConnections(geometry, int) = list(int).

makeConnections(Geometry, SiteIndex) = solutions.solutions(neighbour(Geometry, ix(Geometry, SiteIndex), iy(Geometry, SiteIndex))).

:- func ix(geometry, int) = int.

ix(wellmixed, I) = I.
ix(lattice(XL, _, _, _), I) = I mod XL.

:- func iy(geometry, int) = int.

iy(wellmixed, I) = I.
iy(lattice(XL, _, _, _), I) = I / XL.

:- pred neighbour(geometry, int, int, int).
:- mode neighbour(in, in, in, out) is nondet.

% up
neighbour(lattice(XL, YL, N, B),   X, Y,    X + ((Y + 1) mod YL) * XL) :-
	B = torus
	;
	B = closed,
	Y < YL - 1
	;
	B = ring,
	Y < YL - 1
	.

% down
neighbour(lattice(XL, YL, N, B),   X, Y,    X + ((Y - 1) mod YL) * XL) :-
	B = torus
	;
	B = closed,
	Y > 0
	;
	B = ring,
	Y > 0
	.

% right
neighbour(lattice(XL, YL, N, B),   X, Y,    (X + 1) mod XL + Y * XL) :-
	B = torus
	;
	B = closed,
	X < XL - 1
	;
	B = ring
	.

% left
neighbour(lattice(XL, YL, N, B),   X, Y,    (X - 1) mod XL + Y * XL) :-
	B = torus
	;
	B = closed,
	X > 0
	;
	B = ring
	.


% left up
neighbour(lattice(XL, YL, N, B),   X, Y,    (X - 1) mod XL + ((Y + 1) mod YL) * XL) :-
	(
		N = vonNeumann
	),
	(
		B = torus
		;
		B = closed,
		X > 0,
		Y < YL - 1
		;
		B = ring,
		Y < YL - 1
	)
	.


% left down
neighbour(lattice(XL, YL, N, B),   X, Y,    (X - 1) mod XL  +  ((Y - 1) mod YL) * XL) :-
	(
		N = hexagonal
		;
		N = vonNeumann
	),
	(
		B = torus
		;
		B = closed,
		X > 0,
		Y > 0
		;
		B = ring,
		Y > 0
	)
	.

% right up
neighbour(lattice(XL, YL, N, B),   X, Y,    (X + 1) mod XL + ((Y + 1) mod YL) * XL) :-
	(
		N = hexagonal
		;
		N = vonNeumann
	),
	(
		B = torus
		;
		B = closed,
		X < XL - 1,
		Y < YL - 1
		;
		B = ring,
		Y < YL - 1
	)
	.

% right down
neighbour(lattice(XL, YL, N, B),   X, Y,    (X + 1) mod XL + ((Y - 1) mod YL) * XL) :-
	(
		N = hexagonal
		;
		N = vonNeumann
	),
	(
		B = torus
		;
		B = closed,
		X < XL - 1,
		Y > 0
		;
		B = ring,
		Y > 0
	)
	.


:- end_module debug.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
