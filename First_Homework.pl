%Defines Tree structure, Tree -> NPTree, VPTree
s(s(NPTree, VPTree)) --> np(NPTree), vp(VPTree).
s(s(NPTree, VPTree)) --> np_coordinate(NPTree), vp(VPTree).    %define new np structure to avoid recurstion issue

% Basic phrase structure rules
np(np(noun(Noun))) -->  noun(Noun).
np(np(det(Determiner), noun(Noun))) --> det(Determiner), noun(Noun).           % a cat, a stream
np(np(adj(Adjective), noun(Noun))) --> adj(Adjective), noun(Noun).                   % Old Sam.
np(np(det(Det1), det(Det2), noun(Noun))) --> det(Det1), det(Det2), noun(Noun). % all its critics
np_coordinate(np_coop(np(Np1), conj(Conj), (np(Np2)))) --> np(Np1), conj(Conj), np(Np2).  % the clowns and the accrobats


vp(vp(verb(Verb))) --> verb(Verb).           
vp(vp(verb(Verb),pp(Pre_phrase))) --> verb(Verb), pp(Pre_phrase).                    % sunbathed beside a stream
vp(vp(verb(Verb),np(Optional_np))) --> verb(Verb), np(Optional_np).  				 % dread affectionated cat
vp(vp(verb(Verb), verb_comp(Verb_Complement))) --> verb(Verb), verb_comp(Verb_Complement).  % refused to co-operate
vp(vp(link_verb(Verb), adjp(AP))) --> link_verb(Verb), adjp(AP).					 % felt strangely euphoric
vp(vp(transitive_verb(Verb), np(NP))) --> transitive_verb(Verb), np(NP).					 % disapper all its critics

pp(pp(pr(Preposition), np(Np))) --> pr(Preposition), np(Np).						 % beside a stream
verb_comp(verb_comp(in(Infinitive_Clause), verb(Verb))) --> in(Infinitive_Clause), verb(Verb). %to co-operate
adjp(adjp(adv(Adverb), adj(Adjective))) --> adv(Adverb), adj(Adjective).


%opt_np(np(NP)) --> np(NP).

% Lexicon
det(a) --> [a].
det(an) --> [an].
det(the) --> [the].
det(all) --> [all].
det(its) --> [its].
adj(old) --> [old].
adj(affectionated) --> [affectionated].
adj(euphoric) --> [euphoric].
noun(Sam) --> [Sam].
noun(stream) --> [stream].
noun(cat) --> [cat].
noun(clowns) --> [clowns].
noun(accrobats) --> [accrobats].
noun(Nicholas) --> [Nicholas].
noun(junta) --> [junta].
noun(critics) --> [critics].
verb(sunbathed) --> [sunbathed].
verb(dreads) --> [dreads].
verb(refused) --> [refused].
verb(cooperate) --> [cooperate].
link_verb(felt) --> [felt].
transitive_verb(disappeared) --> [disappeared].
pr(besides) --> [besides].
conj(and) --> [and].
in(to) --> [to].
adv(strangely) --> [strangely].


% Parse helper
parse_string(Str, Tree) :-
    split_string(Str, " ", ".,;:?!", Tokens),
    maplist(string_lower, Tokens, Lower),
    maplist(atom_string, Atoms, Lower),
    phrase(s(Tree), Atoms).


%Homework 1:
% parse_string("old Sam sunbathed besides a stream", Tree).

%Homework 2:
% parse_string("Phil dreads affectionated cat", Tree).

%Homework 3:check co-ordinate phrases
% parse_string("the clowns and the accrobats refused to cooperate", Tree).

%Homework 4
% parse_string("Nicholas felt strangely euphoric", Tree).
% 
%Homework 5
% parse_string("The junta disappeared all its critics", Tree).
