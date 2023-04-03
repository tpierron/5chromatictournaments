# 5chromatictournaments

The logs are big. Install https://git-lfs.github.com/

# Compiling and running

Each .ml file can be compiled with

``ocamlopt -o <file> <file.ml>''

and run with

``./file''


# Output
## section3.ml

This program raises an exception if it finds a 4-chromatic completion of {Pal_7, W, W_0, W_1} with TT_5 that does not contain Pal_11 as subtournament. It should then run without printing anything, nor returning any exception.

## section62.ml

This program's output matches the following shape, for each 3-chromatic TT_5-free graph B on 8 vertices:

------ Graph <index of graph>/94 ------
------ Generating completions ------
<the 13-completions of B> (one per line, given by a list of arcs)
<number of completions> completions

<the 13-completions of B> (one per line, given by a list of arcs)
------ Computing types ------
<number of types of the 13-completions> types
------ Computing incompatible completions ------
<number of> pairs of incompatible types
<number of> pairs of incompatible completions
------ Trying to orient the last 25 arcs ------
<the 18-completions of B, if any>
------ DONE ------

In particular, each line "------ [Trying ...]" should be directly followed by "------ DONE ------".

## section63.ml

The output of this program contains several phases:

- fill phases, that given two 8-completions, constructs the 4-chromatic 13-completions containing them. The format is as follows:

Start <index of first 8-completion> <index of second 8-completion>
<list of 13-completions> (one per line, given by a list of arcs)
End <index of first 8-completion> <index of second 8-completion>: <a>,<b>,<c>=<a+b+c> completions found
<list of 13-completions> (one per line, given by a list of arcs)
Progression = <number of pairs of completions already handled>/32896 = <percentage>%

(a,b,c denote the number of 13-completions for each way of identifying the distinguished directed triangles of the 8-completions)


- test phases, that given three 8-completions, constructs the candidates for 5-chromatic 18-vertex tournaments. The format is as follows:

Begin test (<index of first 8-completion>,<index of second 8-completion>,<index of third 8-completion>)
<list of 18-vertex graphs> (one per line, given by a list of arcs)
End test (<index of first 8-completion>,<index of second 8-completion>,<index of third 8-completion>), <number of> graphs found

Note that the list should be empty, hence each line ``Begin test (i,j,k)'' should be followed by ``End test (i,j,k) [...]''.


- Global phases. For each 8-completion X, there is such a phase:

Begin Graph <index of X>
a fill phase for each 8-completion Y such that (X,Y) was not considered before
Testing completions of graph <index of X>
for each pair of 8-completions (Y,Z) such that there are 13-completions of (X,Y) and (X,Z):
a fill phase for (Y,Z) if (Y,Z) was not considered before.
a test phase for (X,Y,Z)
End Graph <index of X> : <number of pairs (Y,Z)> non empty pairs, <number of> 13-completions, <number of> 18-completions





