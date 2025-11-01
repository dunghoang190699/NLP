% =====================================================
% DCG: person, number, tense, case + genitive possessives
% =====================================================

% ---------- Sentence ----------
% Subject NP must be nominative (nom)
s(s(NPTree, VPTree)) -->
    np(NPTree, Num, Pers, nom),
    vp(VPTree, Num, Pers, Tense),
    { member(Tense, [present, past]) }.

% Coordinate NP (plural, 3rd person) as subject (nom)
s(s(NPTree, VPTree)) -->
    np_coordinate(NPTree, pl, 3, nom),
    vp(VPTree, pl, 3, Tense),
    { member(Tense, [present, past]) }.

% ---------- Noun Phrases ----------
% Simple noun (e.g., "cats")
np(np(noun(Noun)), Num, Pers, _) -->
    noun(Noun, Num, Pers).

% Determiner + noun (e.g., "the cat", "a dog")
np(np(det(Det), noun(Noun)), Num, Pers, _) -->
    det(Det, Num),
    noun(Noun, Num, Pers).

% Possessive determiner (possessive pronoun) + noun (e.g., "his dog", "her cats")
np(np(poss_det(Det), noun(Noun)), Num, Pers, _) -->
    poss_det(Det, _, _),
    noun(Noun, Num, Pers).

% Possessive 's (owner token) + noun (e.g., "john's cat")
np(np(poss_owner(Owner), noun(Noun)), Num, Pers, _) -->
    poss_det(Owner, _, _),
    noun(Noun, Num, Pers).

% Determiner + possessive owner + noun (e.g., "the man's books")
np(np(det(Det), poss_owner(Owner), noun(Noun)), Num, Pers, _) -->
    det(Det, _),
    poss_det(Owner, _, _),
    noun(Noun, Num, Pers).

% Pronoun as whole NP (subject or object) — carries case
np(np(pron(Pron)), Num, Pers, Case) -->
    pron(Pron, Num, Pers, Case).

% adj + noun (e.g., "old cat")
np(np(adj(Adj), noun(Noun)), Num, Pers, _) -->
    adj(Adj),
    noun(Noun, Num, Pers).

% det det noun (e.g., "all its critics")
np(np(det(Det1), det(Det2), noun(Noun)), Num, Pers, _) -->
    det(Det1, _),
    det(Det2, _),
    noun(Noun, Num, Pers).

% Coordinate NP => plural, 3rd person; case is passed through (nom/acc)
np_coordinate(np_coop(np(NP1), conj(Conj), np(NP2)), pl, 3, Case) -->
    np(NP1, _, _, Case),
    conj(Conj),
    np(NP2, _, _, Case).

% ---------- Verb Phrases ----------
vp(vp(verb(Verb)), Num, Pers, Tense) -->
    verb(Verb, Num, Pers, Tense).

% Transitive verb + NP object: object must be accusative
vp(vp(verb(Verb), np(NP)), Num, Pers, Tense) -->
    verb(Verb, Num, Pers, Tense),
    { verb_subcat(Verb, Subcats), member(np, Subcats) },
    np(NP, _, _, acc).

% Verb + PP (unchanged)
vp(vp(verb(Verb), pp(PP)), Num, Pers, Tense) -->
    verb(Verb, Num, Pers, Tense),
    pp(PP).

% Linking verb + ADJP (e.g. "are interesting")
vp(vp(link_verb(Verb), adjp(AP)), Num, Pers, Tense) -->
    link_verb(Verb, Num, Pers, Tense),
    adjp(AP).

% ---------- PP / ADJP / Complements ----------
pp(pp(pr(Prep), np(NP))) --> pr(Prep, _), np(NP, _, _, _).

% verb complement for infinitives (unchanged)
verb_comp(verb_comp(in(Inf), verb(Verb))) --> in(Inf), verb(Verb, _, _, inf).

% ADJP can be just an adjective or adv + adj
adjp(adjp(adj(Adj))) --> adj(Adj).
adjp(adjp(adv(Adv), adj(Adj))) --> adv(Adv), adj(Adj).

% ---------- Lexicon ----------

% Determiners (the/a/an/all/its)
det(a, sg) --> [a].
det(an, sg) --> [an].
det(the, _) --> [the].
det(all, pl) --> [all].
det(its, _) --> [its].

% Possessive determiners (possessive pronouns and owner 's tokens)
% poss_det(Token, OwnerNum, OwnerPers)
poss_det(his_poss, sg, 3) --> [his].   % his dog
poss_det(her_poss, sg, 3) --> [her].   % her cats
% owner 's tokens (treated as a single token like "john's", "man's")
poss_det(john_poss, sg, 3) --> ["john's"].
poss_det(man_poss, sg, 3) --> ["man's"].

% Note: we do NOT allow "his's" or similar — such tokens are not present.

% Pronouns: include case (nom / acc), number and person
pron(he, sg, 3, nom) --> [he].
pron(him, sg, 3, acc) --> [him].
pron(they, pl, 3, nom) --> [they].
pron(them, pl, 3, acc) --> [them].
pron(i, sg, 1, nom) --> [i].
pron(me, sg, 1, acc) --> [me].
pron(we, pl, 1, nom) --> [we].
pron(us, pl, 1, acc) --> [us].
% you is ambivalent in number; case-neutral mapping (works both nom & acc in English)
pron(you, _, 2, nom) --> [you].
pron(you, _, 2, acc) --> [you].

% Adjectives / Adverbs
adj(old) --> [old].
adj(affectionated) --> [affectionated].
adj(euphoric) --> [euphoric].
adj(interesting) --> [interesting].
adv(strangely) --> [strangely].

% Nouns (with number & default person=3)
noun(dog, sg, 3) --> [dog].
noun(cat, sg, 3) --> [cat].
noun(cats, pl, 3) --> [cats].
noun(books, pl, 3) --> [books].
noun(clowns, pl, 3) --> [clowns].
noun(accrobats, pl, 3) --> [accrobats].
noun(nicholas, sg, 3) --> [nicholas].
noun(junta, sg, 3) --> [junta].
noun(critics, pl, 3) --> [critics].
noun(man, sg, 3) --> [man].  % base noun (used if needed)

% ---------- Verbs ----------
% (Verb, Num, Pers, Tense)
verb(chase, pl, 3, present) --> [chase].
verb(chases, sg, 3, present) --> [chases].
verb(chased, _, _, past) --> [chased].

% intransitive / linking verbs for "sleeps", "is/are"
verb(sleeps, sg, 3, present) --> [sleeps].
verb(sleep, pl, _, present) --> [sleep].  % if needed

link_verb(is, sg, 3, present) --> [is].
link_verb(are, pl, 3, present) --> [are].
link_verb(was, _, _, past) --> [was].
link_verb(were, _, _, past) --> [were].

% Prepositions / conjunctions / infinit marker
pr(besides, _) --> [besides].
conj(and) --> [and].
in(to) --> [to].

% ---------- Subcategorization ----------
% verbs that require NP objects
verb_subcat(chase, [np]).
verb_subcat(chases, [np]).
verb_subcat(chased, [np]).

% 'sleeps' is intransitive
verb_subcat(sleeps, []).
verb_subcat(sleep, []).

% -----------------------------------------------------
% Robust parser helper (handles possessives correctly)
% -----------------------------------------------------
parse_string(Str, Tree) :-
    % Split by spaces and basic punctuation (apostrophes kept)
    split_string(Str, " ", ".,;:?!", Tokens0),

    % Lowercase normalization (for case-insensitive matching)
    maplist(string_lower, Tokens0, TokensLower),

    % Convert all string tokens to atoms (e.g., "john's" -> john's)
    maplist(atom_string, TokensAtoms, TokensLower),

    % Parse using DCG
    phrase(s(Tree), TokensAtoms).
% =====================================================
%                      Examples
% =====================================================
% Valid (should succeed)
% ?- phrase(s(Tree), ["john's", cat, sleeps]).
% ?- parse_string("His dog chases the cat", Tree).
% ?- parse_string("The man's books are interesting", Tree).
% ?- parse_string("Her cats chase him", Tree).
%
% Invalid (should fail)
% ?- parse_string("John cat sleeps", Tree).    % missing 's -> fail
% ?- parse_string("His's dog", Tree).          % invalid possessive -> fail
% =====================================================
