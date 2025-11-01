% =====================================================
% Feature-based DCG with person, number, tense agreement
% =====================================================

% ---------- Sentence ----------
% Enforce agreement between subject NP and VP (Num, Pers, Tense)
s(s(NPTree, VPTree)) -->
    np(NPTree, Num, Pers),
    vp(VPTree, Num, Pers, Tense),
    { member(Tense, [present, past]) }.

% Coordinate NP = plural, 3rd person
s(s(NPTree, VPTree)) -->
    np_coordinate(NPTree, pl, 3),
    vp(VPTree, pl, 3, Tense),
    { member(Tense, [present, past]) }.

% ---------- Noun Phrases ----------
np(np(noun(Noun)), Num, Pers) --> noun(Noun, Num, Pers).
np(np(det(Det), noun(Noun)), Num, Pers) --> det(Det, Num), noun(Noun, Num, Pers).
np(np(adj(Adj), noun(Noun)), Num, Pers) --> adj(Adj), noun(Noun, Num, Pers).
np(np(det(Det1), det(Det2), noun(Noun)), Num, Pers) -->
    det(Det1, _), det(Det2, _), noun(Noun, Num, Pers).

% Coordinate NP => plural, 3rd person
np_coordinate(np_coop(np(NP1), conj(Conj), np(NP2)), pl, 3) -->
    np(NP1, _, _), conj(Conj), np(NP2, _, _).

% ---------- Verb Phrases ----------
% VP inherits subject Num, Pers; verb defines tense
vp(vp(verb(Verb)), Num, Pers, Tense) -->
    verb(Verb, Num, Pers, Tense).

vp(vp(verb(Verb), pp(PP)), Num, Pers, Tense) -->
    verb(Verb, Num, Pers, Tense),
    pp(PP).

vp(vp(verb(Verb), np(NP)), Num, Pers, Tense) -->
    verb(Verb, Num, Pers, Tense),
    { verb_subcat(Verb, Subcats), member(np, Subcats) },
    np(NP, _, _).

vp(vp(verb(Verb), verb_comp(VerbComp)), Num, Pers, Tense) -->
    verb(Verb, Num, Pers, Tense),
    verb_comp(VerbComp).

vp(vp(link_verb(Verb), adjp(AP)), Num, Pers, Tense) -->
    link_verb(Verb, Num, Pers, Tense),
    adjp(AP).

vp(vp(transitive_verb(Verb), np(NP)), Num, Pers, Tense) -->
    transitive_verb(Verb, Num, Pers, Tense),
    { verb_subcat(Verb, Subcats), member(np, Subcats) },
    np(NP, _, _).

% ---------- PP / ADJP / Complements ----------
pp(pp(pr(Prep), np(NP))) --> pr(Prep, _), np(NP, _, _).
verb_comp(verb_comp(in(Inf), verb(Verb))) --> in(Inf), verb(Verb, _, _, inf).
adjp(adjp(adv(Adv), adj(Adj))) --> adv(Adv), adj(Adj).

% ---------- Lexicon ----------

% Determiners
det(a, sg) --> [a].
det(an, sg) --> [an].
det(the, _) --> [the].
det(all, pl) --> [all].
det(its, _) --> [its].

% Adjectives / Adverbs
adj(old) --> [old].
adj(affectionated) --> [affectionated].
adj(euphoric) --> [euphoric].
adv(strangely) --> [strangely].

% Nouns: include number & default 3rd person
noun(sam, sg, 3) --> [sam].
noun(stream, sg, 3) --> [stream].
noun(cat, sg, 3) --> [cat].
noun(cats, pl, 3) --> [cats].
noun(dog, sg, 3) --> [dog].
noun(dogs, pl, 3) --> [dogs].
noun(clowns, pl, 3) --> [clowns].
noun(accrobats, pl, 3) --> [accrobats].
noun(nicholas, sg, 3) --> [nicholas].
noun(junta, sg, 3) --> [junta].
noun(critics, pl, 3) --> [critics].

% ---------- Verbs ----------
% (Verb, Num, Pers, Tense)
% Past tense is number/person invariant
verb(sunbathed, _, _, past) --> [sunbathed].
verb(refused, _, _, past) --> [refused].
verb(cooperate, _, _, inf) --> [cooperate].

% Present tense agreement
verb(dreads, sg, 3, present) --> [dreads].
verb(dread, pl, _, present) --> [dread].
verb(sleeps, sg, 3, present) --> [sleeps].
verb(sleep, pl, 3, present) --> [sleep].
verb(chases, sg, 3, present) --> [chases].
verb(chase, pl, 3, present) --> [chase].

% Link / Transitive verbs (past)
link_verb(felt, _, _, past) --> [felt].
transitive_verb(disappeared, _, _, past) --> [disappeared].

% Prepositions
pr(besides, _) --> [besides].
conj(and) --> [and].
in(to) --> [to].

% ---------- Subcategorization ----------
verb_subcat(disappeared, [np]).
verb_subcat(dreads, [np]).
verb_subcat(sunbathed, []).
verb_subcat(refused, [to]).
verb_subcat(cooperate, []).
verb_subcat(chase, [np]).
verb_subcat(chases, [np]).

% ---------- Parser Helper ----------
parse_string(Str, Tree) :-
    split_string(Str, " ", ".,;:?!", Tokens),
    maplist(string_lower, Tokens, Lower),
    maplist(atom_string, Atoms, Lower),
    phrase(s(Tree), Atoms).

% ---------- Example Queries ----------
% ?- parse_string("old sam sunbathed besides a stream", Tree).
% ?- parse_string("phil dreads affectionated cat", Tree).
% ?- parse_string("the clowns and the accrobats refused to cooperate", Tree).
% ?- parse_string("nicholas felt strangely euphoric", Tree).
% ?- parse_string("the junta disappeared all its critics", Tree).
