(* ::Package:: *)

(* ::Title:: *)
(*SerialIO*)


(* ::Author:: *)
(*Rob Raguet-Schofield*)
(*ragfield@gmail.com*)


(* ::Section:: *)
(*Initialization*)


BeginPackage["SerialIO`"];


SerialPort::inval = "`1` is not a valid SerialPort identifier";
SerialOpen::failed = "Could not open serial port (`1`)";

SerialOpen::usage = "";
SerialClose::usage = "";
SerialSetOptions::usage = "";
SerialSetOptions::inval = "Invalid value `1` for `2` option, it must one of `3`";
SerialReadyQ::usage = "";
SerialRead::usage = "";
SerialWrite::usage = "";


Begin["`Private`"];

(* ::Section:: *)
(*Implementation*)
$Link = Install[
	FileNameJoin[{Last[$Path], "SerialIO", "Kernel", "SerialIO"}], LinkProtocol -> "Pipes"];

serialPortPattern = SerialPort[_String, _Integer];

serialPortName[port: serialPortPattern] := port[[1]];

serialPortNumber[port: serialPortPattern] := port[[2]];

Options[SerialPort] = {
	"BaudRate" -> 19200,
	"DataBits" -> 8,
	"StopBits" -> 1,
	"Parity" -> 0
};


SerialOpen[port_String, opts___?OptionQ] := Module[
	{num, sp = $Failed},
	num = serialOpen[port];
	If[num =!= $Failed,
		sp = SerialPort[port, num];
		SerialSetOptions[sp, opts]
	];
	sp
];


Format[port: serialPortPattern, StandardForm] :=
	StringForm["SerialPort[<`1`>]", port[[1]]];


SerialClose[port: serialPortPattern] :=
	serialClose[serialPortNumber[port]];


serialBaudRateOptions = { 230400, 115200, 57600, 38400, 28800, 19200, 14400, 9600, 4800, 2400, 1800, 1200, 600, 300, 200, 150, 134, 110, 75, 50 };
serialDataBitsOptions = { 8, 7, 6, 5 };
serialStopBitsOptions = { 2, 1 };
serialParityOptions = { 2, 1, 0 };


SerialSetOptions[port: serialPortPattern, opts___?OptionQ] := Module[
	{br,db,sb,p,spopts},
	spopts = Options[SerialPort];
	
	br = "BaudRate" /. {opts} /. spopts;
	If[FreeQ[serialBaudRateOptions, br],
		Message[SerialSetOptions::inval, br, "BaudRate", serialBaudRateOptions];
		br = "BaudRate" /. spopts
	];
	
	db = "DataBits" /. {opts} /. spopts;
	If[FreeQ[serialDataBitsOptions, db],
		Message[SerialSetOptions::inval, db, "DataBits", serialDataBitsOptions];
		db = "DataBits" /. spopts
	];
	
	sb = "StopBits" /. {opts} /. spopts;
	If[FreeQ[serialStopBitsOptions, sb],
		Message[SerialSetOptions::inval, sb, "StopBits", serialStopBitsOptions];
		sb = "StopBits" /. spopts
	];
	
	p = "Parity" /. {opts} /. spopts;
	If[FreeQ[serialParityOptions, p],
		Message[SerialSetOptions::inval, p, "Parity", serialParityOptions];
		p = "Parity" /. spopts
	];
	
	serialSetOptions[serialPortNumber[port], {
		"BaudRate" -> br,
		"DataBits" -> db,
		"StopBits" -> sb,
		"Parity" -> p
	}]
];


SerialReadyQ[port: serialPortPattern] := SerialReadyQ[port, 0];


SerialReadyQ[port: serialPortPattern, timeout: (_Integer|_Real)] := 
	serialReadyQ[serialPortNumber[port], N[timeout]];


SerialRead[port: serialPortPattern] :=
	SerialRead[port, 10];

SerialRead[port: serialPortPattern, timeout: (_Integer|_Real)] := 
	serialRead[serialPortNumber[port], N[timeout]];


SerialWrite[port: serialPortPattern, data_String] := 
	serialWrite[serialPortNumber[port], data];

SerialWrite[port: serialPortPattern, expr_] :=
	SerialWrite[port, ToString[expr]];

SerialWrite[port: serialPortPattern, exprs__] :=
	SerialWrite[port, #]& /@ { exprs };


wasProtected = Unprotect[Read, Write, Close];

SerialPort /: Read[port: serialPortPattern] := SerialRead[port];

SerialPort /: Write[port: serialPortPattern, exprs__] := SerialWrite[port, exprs];

SerialPort /: Close[port: serialPortPattern] := SerialClose[port];

Protect @@ wasProtected;


(* ::Section::Closed:: *)
(*Finalization*)


End[]; (* `Private` *)


EndPackage[]; (* SerialIO *)
