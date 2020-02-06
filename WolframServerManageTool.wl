(* ::Package:: *)

BeginPackage["WolframServerManageTool`"]


ServerShutdown::usage = "ServerShutdown[]";
ServerRestart::usage = "ServerRestart[]";
ServerMonitor::usage = "Hold the main loop to forbid process to exit. When a specific message is generated, restart the server.";
ApacheDateString::usage = "It's the Apache type DateString[], which is used for ExportString[expr, \"ApacheLog\"]."
ShowLog::usage = "ShowLog[logfile]. ShowLog[] is equivalent to ShowLog[\"nohup.out\"]";


$ServerHost
$CurrentListener
$CurrentZMQTask := ZeroMQLink`Private`$AsyncState["Task"]


ServerStart[OptionsPattern[]] := (
	$CurrentListener = SocketListen[{OptionValue["IPAddress"],OptionValue["Port"]}, OptionValue[HandlerFunctions]]
)
Options[ServerStart] = {"IPAddress" -> "0.0.0.0", "Port" -> 8080, HandlerFunctions -> WolframServer`HTTPRespond};

ServerShutdown = With[{sockets = Cases[$CurrentListener /@ {"Socket", "SourceSocket"}, _SocketObject]},
	RemoveAsynchronousTask@$CurrentZMQTask;
	DeleteObject@WolframServer`$CurrentListener;
	Close /@ sockets;
	Print@"Server is closed."
]&;

ServerRestart = With[
	{
		socketspec = $CurrentListener["Socket"]/@{"DestinationIPAddress","DestinationPort"},
		respond = $CurrentListener["HandlerFunctions"]["DataReceived"]
	},
	ServerShutdown[];
	Pause[1];
	WolframServerManageTool`$CurrentListener = SocketListen[socketspec, respond];
	Print@"Server is restarted.";
]&;

ServerMonitor[Optional[msg:{_MessageName..}|_Sequence, {Write::zmqexception}], Optional[period_?NumericQ, 0.1]] :=
	While[True,
		Check[
			Pause@period,
			(
				TaskRemove@$CurrentZMQTask;
				ServerRestart[]
			),
			msg
		]
	];
SetAttributes[ServerMonitor,HoldFirst];

ApacheDateString[dateObj_DateObject:Now] := With[{timeZone = StringReplace[DateString["ISOTimeZone"], ":" -> ""]},
	DateString@@{dateObj, {"[", "Day", "/", "Month", "/", "Year", ":", "Hour", ":", "Minute", ":", "Second", " ", timeZone, "]"}}
];

ApacheLog := StringTemplate["`IP`  - - `Time` \"`Method` `Path` `Scheme`/`HTTPVersion`\" `StatusCode` `Query`"];

ShowLog[logfile_:"./nohup.out"] := Import[logfile, "Text"]


EndPackage[]
