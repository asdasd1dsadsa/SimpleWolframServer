(* ::Package:: *)

BeginPackage["WolframServerTool`"]


ToMIME::usage = "Convert file extensions to MIMETypeStrings, which accept all possible MIMETypes instead of those in $ImportFormats and $ExportFormats.";
ResourceLocalize::usage = "Redirect WolframCloud resources in a HTML response to the local path.";
MakeFormRules::usage = "Convert the body bytes in an HTTPRequest to an association to make the \"FormRules\" property available.";

$CloudResourcePath::usage = "A list of cloud resource paths to be localized.";
$MIMETypeMap::usage = "";


$CloudResourcePath = {"https://www.wolframcloud.com/res/themes/"};
$MIMETypeMap = Rule @@@ Get@"MIMETypeMap.wl" // Association;


(* ::Section:: *)
(* WolframServer` *)


(* ::Subsection:: *)
(* HTTPRespond *)


MakeFormRules[request_HTTPRequest] := With[
	{
		multipart = StartOfString~~"multipart/form-data; boundary="~~boundaryPattern__~~EndOfString,
		formURLEncoded = "application/x-www-form-urlencoded"
	},
	MapAt[
		StringSwitch[request["ContentType"],
			formURLEncoded,
				URLQueryDecode@StringTrim@FromCharacterCode[#, "UTF-8"] &,
			multipart,
				Association@parseMultipart[
					StringTrim@FromCharacterCode[#, "UTF-8"],
					StringReplace[multipart -> "--"~~boundaryPattern]@req["ContentType"]
				] &,
			_,
				Identity
		],
		request,
		{2, "Body"}
	]
]


(* ::Subsubsection:: *)
(*MakeFormRules*)


Begin["`Private`"]


StringSwitch[expr_String, seq:PatternSequence[_, _]..] := Switch[expr, ##] &[
	Sequence @@ Join @@ MapAt[_?(StringMatchQ[#])&, 1] /@ Partition[{seq}, 2]
]

parseMultipart[body_String, boundary_String] := handlePartDisposition /@ StringSplit[body, boundary~~"\n"|"--"]
handlePartDisposition[part_String] := With[
	{
		formDataWithName = StartOfString~~"Content-Disposition: form-data"~~"; name=\""~~namePattern:Shortest@__~~"\""~~"\n\n"~~bodyPattern:__~~"\n"~~EndOfString
	},
	StringSwitch[part,
		formDataWithName, Sequence@@URLQueryDecode@StringReplace[formDataWithName -> URLEncode@namePattern~~"="~~URLEncode@bodyPattern]@part
	]
]


End[]


(* ::Section:: *)
(* WolframServerService` *)


(* ::Subsection:: *)
(* ExportForm[#, "HTML"]& *)


ResourceLocalize = MapAt[
	StringReplace[{
		Alternatives@@$CloudResourcePath ~~ FileName_ -> "/Resources/" ~~ FileName,
		"http://www.wolframcdn.com/consent/cookie-consent.php" -> ""
	}] @ ExportString[#, "Byte"] &
, 1];


(* ::Subsection:: *)
(* GetResource *)


ToMIME[format_String] := Which[
	!MissingQ@$MIMETypeMap,
		$MIMETypeMap@ToLowerCase@format,
	True,
		StringForm["text/``", ToLowerCase@format]
]
