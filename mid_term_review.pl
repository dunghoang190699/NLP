% =====================================================
% Feature-based DCG with person, number, tense, and case
% =====================================================

% ---------- Sentence ----------
% Subject NP must be nominative
s(s(NPTree, VPTree)) -->
    np(NPTree, Num, Pers, nom),
    vp(VPTree, Num, Pers, Tense),
    { member(Tense, [present, past]) }.

% Coordinate NP (plural, 3rd person)
s(s(NPTree, VPTree)) -->
    np_coordinate(NPTree, pl, 3, nom),
    vp(VPTree, pl, 3, Tense),
    { member(Tense, [present, past]) }.

% ---------- Noun Phrases ----------
np(np(noun(Noun)), Num, Pers, _) -->
    noun(Noun, Num, Pers).
np(np(det(Det), noun(Noun)), Num, Pers, _) -->
    det(Det, Num),
    noun(Noun, Num, Pers).
np(np(pron(Pron)), Num, Pers, Case) -->
    pron(Pron, Num, Pers, Case).
np(np(adj(Adj), noun(Noun)), Num, Pers, _) -->
    adj(Adj),
    noun(Noun, Num, Pers).
np(np(det(Det1), det(Det2), noun(Noun)), Num, Pers, _) -->
    det(Det1, _),
    det(Det2, _),
    noun(Noun, Num, Pers).

% Coordinate NP => plural, 3rd person, nominative
np_coordinate(np_coop(np(NP1), conj(Conj), np(NP2)), pl, 3, Case) -->
    np(NP1, _, _, Case),
    conj(Conj),
    np(NP2, _, _, Case).

% ---------- Verb Phrases ----------
% VP inherits subject Num, Pers; verb defines tense
vp(vp(verb(Verb)), Num, Pers, Tense) -->
    verb(Verb, Num, Pers, Tense).

% Verb + direct object NP (accusative)
vp(vp(verb(Verb), np(NP)), Num, Pers, Tense) -->
    verb(Verb, Num, Pers, Tense),
    { verb_subcat(Verb, Subcats), member(np, Subcats) },
    np(NP, _, _, acc).

% Verb + PP
vp(vp(verb(Verb), pp(PP)), Num, Pers, Tense) -->
    verb(Verb, Num, Pers, Tense),
    pp(PP).

% Link verb + adjp
vp(vp(link_verb(Verb), adjp(AP)), Num, Pers, Tense) -->
    link_verb(Verb, Num, Pers, Tense),
    adjp(AP).

% ---------- PP / ADJP / Complements ----------
pp(pp(pr(Prep), np(NP))) --> pr(Prep, _), np(NP, _, _, _).
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

% Nouns
noun(dog, sg, 3) --> [dog].
noun(cat, sg, 3) --> [cat].
noun(cats, pl, 3) --> [cats].
noun(clowns, pl, 3) --> [clowns].
noun(accrobats, pl, 3) --> [accrobats].
noun(nicholas, sg, 3) --> [nicholas].
noun(junta, sg, 3) --> [junta].
noun(critics, pl, 3) --> [critics].

% Pronouns with case, number, person
pron(he, sg, 3, nom) --> [he].
pron(him, sg, 3, acc) --> [him].
pron(they, pl, 3, nom) --> [they].
pron(them, pl, 3, acc) --> [them].
pron(i, sg, 1, nom) --> [i].
pron(me, sg, 1, acc) --> [me].
pron(we, pl, 1, nom) --> [we].
pron(us, pl, 1, acc) --> [us].
pron(you, _, 2, nom) --> [you].
pron(you, _, 2, acc) --> [you].

% Verbs (with number/person/tense)
verb(chase, pl, 3, present) --> [chase].
verb(chases, sg, 3, present) --> [chases].
verb(chased, _, _, past) --> [chased].

% Linking / Transitive verbs
link_verb(felt, _, _, past) --> [felt].
transitive_verb(disappeared, _, _, past) --> [disappeared].

% Prepositions
pr(besides, _) --> [besides].
conj(and) --> [and].
in(to) --> [to].

% ---------- Subcategorization ----------
verb_subcat(chase, [np]).
verb_subcat(chases, [np]).
verb_subcat(chased, [np]).

% ---------- Parser Helper ----------
parse_string(Str, Tree) :-
    split_string(Str, " ", ".,;:?!", Tokens),
    maplist(string_lower, Tokens, Lower),
    maplist(atom_string, Atoms, Lower),
    phrase(s(Tree), Atoms).
