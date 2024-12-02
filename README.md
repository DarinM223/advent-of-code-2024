advent-of-code 2024
===================

To build:
First run:
```
smlpkg sync
```

To download the smlpkg packages.

Then to build in MLton, run:

```
mlton aoc2024.mlb
```

To run in SML/NJ, look at the following `sml` shell for day 1:
```
Standard ML of New Jersey [Version 2023.1; 64-bit; December 10, 2023]
- CM.make "aoc2024.cm";
val it = true : bool
- open Day1;
opening Day1
  val leftList : int list
  val rightList : int list
  val sortedLeftList : int list
  val sortedRightList : int list
  val totalDistance : int
  val rightListOccurTable : int IntHashTable.hash_table
  val lookup : IntHashTable.Key.hash_key -> int
  val totalSimilarityScore : int
  val results : unit -> unit
- results ();
Part 1: 1388114
Total similarity score: 23529853
val it = () : unit
-
```

To build in Poly/ML, you need to have MLton installed and available in the PATH. Then run:
```
./build_polyml.sh
```
This results in a `build.sml` file being created. Then run:
```
poly --use build.sml
```
To load the code into the REPL. Then you can do something like:
```
> Day1.results ();
Part 1: 1388114
Total similarity score: 23529853
val it = (): unit
>
```