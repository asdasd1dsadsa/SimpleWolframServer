(* ::Package:: *)

Needs["WolframServerService`"]
Needs["WolframServerTool`"]
Needs["WolframServerManageTool`"]


BeginPackage["WolframServer`"]


HTTPRespond::usage = "Generate a HTTP response by processing a socket message. It's function is decided by WolramServerService`.";


HTTPRespond[socketMsg_Association] := Module[
	{
		$SourceSocket = socketMsg["SourceSocket"],
		$SocketData = socketMsg["Data"],
		$Request,
		$Response
	},
	$Request = WolframServerTool`MakeFormRules@ImportString[$SocketData, "HTTPRequest"];
	$Response = WolframServerTool`ResourceLocalize@GenerateHTTPResponse[
				(*
					Null pattern in URLDispatcher matches all URLs before a delimiter "/" and return remained URL string.
					Here it's used to cut the base address from URL.
				*)
				URLDispatcher@{"" -> WolframServerService`$MainURLDispatcher}
			, $Request];
	WriteString[
		$SourceSocket,
		ExportString[$Response, "HTTPResponse"]
	];
	(* You can DIY your log string. Here's an example. *)
	Print@WolframServerManageTool`ApacheLog@<|
		"IP" -> Interpreter["IPAddress"]@$SourceSocket["SourceIPAddress"],
		"Time" -> WolframServerManageTool`ApacheDateString@socketMsg["Timestamp"],
		"Method" -> $Request["Method"],
		"Path" -> URLBuild@$Request["Path"],
		"Scheme" -> ToUpperCase@$Request["Scheme"],
		"HTTPVersion" -> $Request[[2]]["HTTPVersion"],
		"StatusCode" -> $Response["StatusCode"],
		"Query" -> $Request["QueryString"]
	|>;
	Close@$SourceSocket
]


Begin["`Private`"]


End[]


EndPackage[]
