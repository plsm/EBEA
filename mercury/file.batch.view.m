/**
 * Base modules that provide a view to values of type {@code file.batch/0}

 * @author Pedro Mariano
 * @version 1.0 2013/06/ 7
 */
:- module file.batch.view.

:- interface.

:- import_module userInterface_2.

/**
 * Return an 
 */
:- func userInterface = dialog(batch).
%:- mode userInterface = out(dialog(latest)) is det.

:- implementation.

:- import_module float, int, list.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

userInterface = dialog(
	[di(label("pseudo-random number generator"), 'new editField'(random, setRandom, editRandom))
,	 di(label("common parameter"), 'new editField'(commonParameterValues, setCommonParameterValues, editCommonParameterValues))
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- func setCommonParameterValues(batch, commonParameterValues) = maybe_error(batch).

setCommonParameterValues(Batch, CPV) = ok('commonParameterValues :='(convert(Batch), CPV)).

:- func editCommonParameterValues = dialog(commonParameterValues).

editCommonParameterValues = dialog(
	[
	 di(label("carrying capacity"),    updateListFieldFloat( lCarryingCapacity,    setLCarryingCapacity)),
	 di(label("reproduction energy"),  updateListFieldFloat( lEnergyReproduce,     setLEnergyReproduce)),
	 di(label("death by old age"),     updateListFieldInt(   lOldAge,              setLOldAge)),
	 di(label("mutation probability"), updateListFieldFloat( lMutationProbability, setLMutationProbability)),
	 di(label("number of runs"),       updateFieldInt(       getNumberRuns,        setNumberRuns)),
	 di(label("number of iterations"), updateFieldInt(       rounds,               setRounds)),
	 di(label("advanced parameters"),  subdialog(            editAdvancedParameters))
	]).

:- func editAdvancedParameters = dialog(commonParameterValues).

editAdvancedParameters = dialog(
	[
	 di(label("energy gain process"),  'new editListFieldAny'(  lEnergyGainProcess,   setLEnergyGainProcess,  scaled, dialog(
		[
		 di(label("unscaled"),        newValue(unscaled)),
		 di(label("scaled"),          newValue(scaled)),
		 di(label("scaled positive"), newValue(scaledPositive))
		 ]))),
	 di(label("energy at birth"),       updateListFieldFloat(   lEnergyBirth,         setLEnergyBirth)),
	 di(label("old age dispersion"),    updateListFieldFloat(   lDeathSuaveness,      setLDeathSuaveness)),
	 di(label("population dynamic"),    'new editListFieldAny'( lPopulationDynamic,   setLPopulationDynamic, birthPlusDeath, dialog([])))
	 ]).


:- func setLPopulationDynamic(commonParameterValues, list(ebea.population.dynamic)) = maybe_error(commonParameterValues).

setLPopulationDynamic(CPV, LPopulationDynamic) = ok('lPopulationDynamic :='(CPV, list.remove_dups(LPopulationDynamic))).


:- func setLDeathSuaveness(commonParameterValues, list(float)) = maybe_error(commonParameterValues).

setLDeathSuaveness(CPV, LDeathSuaveness) =
	(if
		list.member(V, LDeathSuaveness),
		V =< 0.0
	then
		error("")
	else
		ok('lDeathSuaveness :='(CPV, LDeathSuaveness))
	).

:- func setLEnergyBirth(commonParameterValues, list(float)) = maybe_error(commonParameterValues).

setLEnergyBirth(CPV, LEnergyBirth) =
	(if
		list.member(V, LEnergyBirth),
		V < 0.0
	then
		error("energy at birth must be zero or negative")
	else
		ok('lEnergyBirth :='(CPV, LEnergyBirth))
	).

/*
:- func setL(commonParameterValues, list(float)) = maybe_error(commonParameterValues).

setL(CPV, L) =
	(if
		list.member(V, L),
		V =< 0.0
	then
		error("")
	else
		ok('l :='(CPV, L))
	).
*/

:- func setLEnergyGainProcess(commonParameterValues, list(energyGainProcess)) = maybe_error(commonParameterValues).

setLEnergyGainProcess(CPV, LEnergyGainProcess) = ok('lEnergyGainProcess :='(CPV, list.remove_dups(LEnergyGainProcess))).

:- func getNumberRuns(commonParameterValues) = int.

getNumberRuns(CPV) = list.length(CPV^lRuns).

:- func setNumberRuns(commonParameterValues, int) = maybe_error(commonParameterValues).

setNumberRuns(CPV, Runs) =
	(if
		Runs =< 0
	then
		error("number of runs must be positive")
	else
		ok('lRuns :='(CPV, 1..Runs))
	).

:- func setLCarryingCapacity(commonParameterValues, list(float)) = maybe_error(commonParameterValues).

setLCarryingCapacity(CPV, LCarryingCapacity) =
	(if
		list.member(V, LCarryingCapacity),
		V < 0.0
	then
		error("carrying capacity must be greater than zero one")
	else
		ok('lCarryingCapacity :='(CPV, LCarryingCapacity))
	).

:- func setLEnergyReproduce(commonParameterValues, list(float)) = maybe_error(commonParameterValues).

setLEnergyReproduce(CPV, LEnergyReproduce) =
	(if
		list.member(V, LEnergyReproduce),
		V =< 0.0
	then
		error("reproduction energy must be positive")
	else
		ok('lEnergyReproduce :='(CPV, LEnergyReproduce))
	).

:- func setLOldAge(commonParameterValues, list(int)) = maybe_error(commonParameterValues).

setLOldAge(CPV, LOldAge) =
	(if
		list.member(V, LOldAge),
		V =< 0
	then
		error("old age must be positive")
	else
		ok('lOldAge :='(CPV, LOldAge))
	).



:- func setLMutationProbability(commonParameterValues, list(float)) = maybe_error(commonParameterValues).

setLMutationProbability(CPV, LMutationProbability) =
	(if
		list.member(V, LMutationProbability),
		(
			V < 0.0
			;
			V > 1.0
		)
	then
		error("probability must be a value between zero and one")
	else
		ok('lMutationProbability :='(CPV, LMutationProbability))
	).


:- func setRounds(commonParameterValues, int) = maybe_error(commonParameterValues).

setRounds(CPV, Rounds) =
	(if
		Rounds > 0
	then
		error("number of iterations must be positive")
	else
		ok('rounds :='(CPV, Rounds))
	).


/**
 * Set the pseudo-random number generator used in EBEA simulations.

 * There is a hack the fact that "determinism inference for higher order
 * predicate terms" is not implemented.
  
 */
:- func setRandom(batch, supplyParameter) = maybe_error(batch).
%:- mode setRandom(in(latest), in) = out(maybe_error(latest)).

setRandom(Batch, Supply) = ok('random :='(convert(Batch), Supply)).

/**
 * Return a dialog to edit pseudo-random number generators
 */
:- func editRandom = dialog(supplyParameter).

editRandom = dialog(
	[di(label("mersenne twister"), subdialog(dialog(
		[di(label("clock"), updateData(setClockMT)),
		 di(label("seed"),  updateFieldInt(getSeed, setSeedMT))]))),
	 di(label("linear congruent"), subdialog(dialog(
		[di(label("clock"), updateData(setClockLG)),
		 di(label("seed"),  updateFieldInt(getSeed, setSeedLG))])))]).


:- func setClockMT(my.random.supplyParameter) = my.random.supplyParameter.

setClockMT(_)     = mt(clock).

:- func setClockLG(my.random.supplyParameter) = my.random.supplyParameter.

setClockLG(_) = random(clock `with_type` my.random.seed).

:- func getSeed(my.random.supplyParameter) = int.

getSeed(mt(value(Result))) = Result.
getSeed(random(value(Result))) = Result.
getSeed(mt(clock)) = defaultSeed.
getSeed(random(clock)) = defaultSeed.

:- func setSeedMT(my.random.supplyParameter, int) = maybe_error(my.random.supplyParameter).

setSeedMT(_, Seed) = Result :-
	(if
		Seed = 0
	then
		Result = error("seed must be different from zero")
	else
		Result = ok(mt(value(Seed)))).

:- func setSeedLG(my.random.supplyParameter, int) = maybe_error(my.random.supplyParameter).

setSeedLG(_, Seed) = ok(random(value(Seed))).

:- func defaultSeed = int.

defaultSeed = 214285232.



:- end_module file.batch.view.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
