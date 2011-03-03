(* ::Package:: *)

(* ::Title:: *)
(* Tools for pairs trading *)

(* ::Author:: *)
(*Keven Lin*)
(*k@kevenlin.com*)

BeginPackage["Pairs`"]

Needs["MultivariateStatistics`"]

ADF::Usage = "Augmented Dickey Fuller Test"

Cointegrate::Usage = "Returns the p-value for ADF test on cointegration"

HedgeRatio::Usage = "Use linear regression to determine the hedge ratio between two time data sets"

Pearson::Usage = "Returns the Pearson correlation for the pair"

Begin["`Private`"]

ADF::toosmall = "p-value smaller than printed value"
ADF::toobig = "p-value larger than printed value"
ADF[x_, k_]:= {
    Clear[a,b];
    k1=k+1;
    y=Differences[x];
    n=Length[y];
    yt=First[#] & /@ Partition[y,k1];
    xt1=x[[k1;;n]];
    tt=Range[k1,n];
    lm=LinearModelFit[Transpose[{xt1, tt, yt}], {a,b}, {a,b}, LinearOffsetFunction->1];
    stat=lm["BestFitParameters"][[2]] / lm["ParameterErrors"][[2]];
    table=-{{4.38, 4.15, 4.04, 3.99, 3.98, 3.96}, 
		{3.95, 3.80, 3.73, 3.69, 3.68, 3.66}, 
		{3.60,3.50,3.45,3.43,3.42,3.41},
		{3.24,3.18,3.15,3.13,3.13,3.12},
		{1.14,1.19,1.22,1.23,1.24,1.25},
		{0.80,0.87,0.90,0.92,0.93,0.94},
		{0.50,0.58,0.62,0.64,0.65,0.66},
		{0.15,0.24,0.28,0.31,0.32,0.33}};
    tablen=Length[table];
    tableT={25, 50, 100, 250, 500, 100000};
    tableP={0.01, 0.025, 0.05, 0.10, 0.90, 0.95, 0.975, 0.99};
    tableipl = Interpolation[Transpose[{tableT, #}], n, InterpolationOrder -> 1] & /@ table;
    interpol = Interpolation[Transpose[{tableipl, tableP}], stat, InterpolationOrder -> 1];
    If[interpol<Min[tableP], Message[ADF::toosmall];interpol=Min[tableP];];
    If[interpol>Max[tableP], Message[ADF::toobig];interpol=Max[tableP];];
    interpol 
    }

Cointegrate[ystock_, xstock_, ratio_:0] := Module[
    {beta, spreads},
    beta = If[ratio==0, HedgeRatio[ystock, xstock], ratio];
    spreads = ystock - xstock*beta;
    First[ADF[spreads, 0]]
]

HedgeRatio[ystock_, xstock_] := Module[
    {x},
    Clear[x];
    Round[LinearModelFit[Transpose[{ xstock, ystock}], x, x, IncludeConstantBasis->False][1], 0.01]
]

Pearson[v1_, v2_] := Correlation[v1, v2]

End[]

EndPackage[]