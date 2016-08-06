solve24[x : {_Integer, _Integer, _Integer, _Integer}] := Module[
  {operators = Tuples[{"+", "-", "*", "/"}, 3],
   operands = 
    Table[{ToString[x[[p[[1]]]]], ToString[x[[p[[2]]]]], 
      ToString[x[[p[[3]]]]], ToString[x[[p[[4]]]]]}, {p, 
      Permutations[{1, 2, 3, 4}, {4}]}],
   baseequations,
   outerJoin,
   innerJoinLeft,
   innerJoinRight
   },
  Off[Power::infy, Infinity::indet];
  baseequations = 
   Flatten[Table[Riffle[a, b], {a, operands}, {b, operators}], 1];
  outerJoin = Table[
    {
     StringJoin@
      Flatten[{"(", eq[[1 ;; 3]], ")", eq[[4]], "(", eq[[5 ;; 7]], 
        ")"}], " == ",
     ToString[
      ToExpression[
       StringJoin@
        Flatten[{"(", eq[[1 ;; 3]], ")", eq[[4]], "(", eq[[5 ;; 7]], 
          ")"}]]]
     },
    {eq, baseequations}
    ];
  innerJoinLeft = Table[
    {
     StringJoin@
      Flatten[{"(", eq[[1 ;; 2]], "(", eq[[3 ;; 5]], ")", ")", 
        eq[[6 ;; 7]]}], " == ",
     ToString[
      ToExpression[
       StringJoin@
        Flatten[{"(", eq[[1 ;; 2]], "(", eq[[3 ;; 5]], ")", ")", 
          eq[[6 ;; 7]]}]]]
     }, {eq, baseequations}];
  innerJoinRight = Table[
    {
     StringJoin@
      Flatten[{eq[[1 ;; 2]], "(", "(", eq[[3 ;; 5]], ")", 
        eq[[6 ;; 7]], ")"}], " == ",
     ToString[
      ToExpression[
       StringJoin@
        Flatten[{eq[[1 ;; 2]], "(", "(", eq[[3 ;; 5]], ")", 
          eq[[6 ;; 7]], ")"}]]]
     }, {eq, baseequations}];
  StringJoin /@ 
   Select[Union[outerJoin, innerJoinLeft, 
     innerJoinRight], #[[3]] == "24" &]
  ]
