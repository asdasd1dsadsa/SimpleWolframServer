#!/usr/bin/env wolframscript
(* ::Package:: *)

$CharacterEncoding = "UTF-8";
AppendTo[
	$Path,
	If[$Linked,
		NotebookDirectory[],
		$CommandLine[[2]]
	]
];
Needs["WolframServer`"]


Print@"Simple Wolfram Server 0.0.1";
Print@"Usage: StartServer.wls [port]\n";

ip = "0.0.0.0";
port = If[Length@$ScriptCommandLine != 2,
	8082,
	Interpreter["Integer"]@$ScriptCommandLine[[2]]
];

Print@*StringJoin /@ Transpose@{
	StringPadRight@{"Port:", "$ProcessID:  "},
	ToString/@{port, $ProcessID}
};

listener = ServerStart["Port" -> port];

url = URLBuild[<|
	"Scheme" -> "http",
	"Domain" -> First@Flatten@IPAddress@listener["Socket"]["DestinationIPAddress"],
	"Port" -> listener["Socket"]["DestinationPort"]
|>];

Print@StringJoin["Listening:  ", url, "\n"];


ServerMonitor[];
Print["Exiting..."];
