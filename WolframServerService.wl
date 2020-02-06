(* ::Package:: *)

(* ::Section:: *)
(*Prerequisite *)


Needs["WolframServerUserService`"]


BeginPackage["WolframServerService`"]


$ServerRootDirectory = ".";


(* ::Section:: *)
(*Dispatcher*)


$MainURLDispatcher = URLDispatcher@{
	"/api" -> $APIDispatcher,
	"/svc" -> $ServiceDispatcher,
	"/usr" :> WolframServerUserService`$Index,
	StartOfString ~~ ""|"/" ~~ EndOfString -> IndexService,
	path__ :> `Basic`GetResource@path
};


$APIDispatcher = URLDispatcher@{
	"/eval" -> EvaluationAPI
};

$ServiceDispatcher = URLDispatcher@{
	"/eval" -> EvaluationService
};


(* ::Section:: *)
(*Service*)


(* ::Subsection:: *)
(*Basic Service*)


Begin["`Basic`"]


GetResource[path_String] := With[
	{
		localpath = FileNameJoin@{Sequence@@FileNameSplit@$ServerRootDirectory, Sequence@@FileNameSplit@path}
	},
	If[FileExistsQ@localpath,
		HTTPResponse[
			Import[localpath, "String"],
			<|"ContentType" -> WolframServerTool`ToMIME@FileFormat@localpath|>
		],
		HTTPErrorResponse[404]
	]
]

IndexService = FormPage[
	{{"expr", "Expression"} -> ToExpression},
	ExportForm[#expr, "HTML"]&
];

EvaluationAPI = APIFunction[
	{{"expr", "Expression"} -> ToExpression},
	ResponseForm[#expr, "JSON"] &
];

EvaluationService = FormFunction[
	{{"expr", "Expression"} -> ToExpression},
	ResponseForm[#expr, "HTML"]&
];


End[]


(* ::Section:: *)
(*End*)


EndPackage[]
