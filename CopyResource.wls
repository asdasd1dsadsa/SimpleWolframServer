#!/usr/bin/env wolframscript

CopyDirectory[
	FileNameJoin@{$InstallationDirectory,"SystemFiles","Components","Templating","Resources","StaticLoader"},
	FileNameJoin@{NotebookDirectory[],"Resources"}
]
