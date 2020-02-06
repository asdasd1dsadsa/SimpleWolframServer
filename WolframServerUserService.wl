(* ::Package:: *)

BeginPackage["WolframServerUserService`"]


$Index = URLDispatcher@{
	"/dataset" -> DatasetService,
	"/note" -> NoteService
};


Begin["`Private`"]


DatasetService


$NoteBody = {};
$NotePath = If[FileExistsQ@#,
	#,
	DumpSave[#, $NoteBody]; #
] &@ "./UserFiles/NoteBody.mx";


NoteService = (
	Get[$NotePath];(* Keep it's dispatched by RuleDelayed rather than Rule! *)
	URLDispatcher@{
		"/read" :> If[ListQ@$NoteBody && Length@$NoteBody > 0, ReadNote[], NewNote[]],
		"/new" :> NewNote[],
		"/edit" :> CallRecurrentEdit[]
	}
);

ReadNote[] := FormFunction[
	{
		"Command" -> <|
			"Interpreter" -> Association@Map[# -> # &]@{"Edit", "Delete"},
			"Control" -> SetterBar,
			"Label" -> "Command"
		|>,
		"Indexes" -> <|
			"Interpreter" -> AnySubset@MapIndexed[Rule]@$NoteBody,(* \:4f20\:8f93\:7684\:5b9e\:9645\:4e0a\:4ecd\:662fKeys\:ff0c\:800c\:975e\:4f5c\:4e3aValues\:7684Indexes\:3002\:4e5f\:8bb8\:5e94\:5f53\:4f7f\:7528InterpretationBox\:7c7b\:6765\:5b9e\:73b0 *)
			"Control" -> Function@ListPicker[##, Appearance -> "Vertical"],
			"Label" -> "Selection"
		|>
	},
	Switch[#Command,
		"Edit",
			CallRecurrentEdit[First /@ #Indexes],
		"Delete",
			$NoteBody = Delete[$NoteBody, #Indexes];
			DumpSave[$NotePath, $NoteBody];
			ReadNote[]
	]&
	, AppearanceRules -> <|"ItemLayout" -> "Vertical"|>
];

NewNote[] := FormPage[
	"Note" -> Identity,
	(
		AppendTo[$NoteBody, #Note];
		DumpSave[$NotePath, $NoteBody];
		#Note
	)&
];

CallRecurrentEdit[indexes:_Integer|{___Integer}] := (
	Get@$NotePath;
	HTTPRedirect@URLBuild["/usr/note/edit", {
		"Index" -> ExportString[indexes, "JSON"]
	}]
)
CallRecurrentEdit[] := CallRecurrentEdit@{}

RecurrentEdit[] := APIFunction[
	{
		"Index" -> <|"Interpreter" -> indexInterpreter, "Required" -> False|>,
		"Note" -> <|"Interpreter" -> Identity, "Required" -> False|>
	},
	Switch[{#Index, #Note},
		{{}, _},
			CallRecurrentEdit@1,
		{_Integer, _},
			CallRecurrentEdit@Range[#Index, Length@$NoteBody],
		{{__Integer}, _Missing},
			FormFunction["Note" -> <|"Interpreter" -> Identity, "Input" -> $NoteBody[[First@#Index]]|>,
				RecurrentEdit[]
			],
		{{__Integer}, _String},
			(
				$NoteBody[[First@#Index]] = #Note;
				DumpSave[$NotePath, $NoteBody];
				CallRecurrentEdit@Rest@#Index
			),
		True,(* This matches {_Missing, _} *)
			FormFunction["Index" -> <|"Interpreter" -> "Integer"|>,
				RecurrentEdit[]
			]
	] &
]

(* APIFunction regards Interpreter["JSON"] as a file interpreter, which add a head "File" to input string. *)
indexInterpreter[inputstr_String] := If[# === $Failed,
	Failure["InterpretationFailure", <||>],
	If[MatchQ[#, _Integer|{___Integer}],
		#,
		Failure["InterpretationFailure", <||>]
	]
]&@ImportString[inputstr, "JSON"];


End[]


EndPackage[]
