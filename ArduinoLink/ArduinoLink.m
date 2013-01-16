
(*  Updated November 28th, 2012
		
	This is the ArduinoLink package, an advanced wrapper on the Arduino microcontroller that enables rapid prototyping with
	the Arduino through Mathematica. Along with the package is an extensive set of documentation and examples to help you
	get started with using the package.
	
	Currently, the package only supports machines running Mac OSX, with the possibility of expanding support to Windows and 
	Linux in the future. The Serial I/O functionality works on Windows & Linux, but the code generation and dynamic linking
	only works on OSX. There is support for all Arduino boards, although only the Arduino Uno, Duemilanove, and Mega have
	been thoroughly tested.
	
	Happy prototyping!
	
	-Keshav Saharia			keshavs@wolfram.com
	 Wolfram Research Inc.
*)

BeginPackage["ArduinoLink`", {"SymbolicC`"}] 

 (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  *	INTERFACE TO HARDWARE																			*
  *  These functions are used as the primary interface between Mathematica and the actual			*
  *  port that the Arduino is connected to.															*
  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
 
ArduinoList::usage = "ArduinoList[] returns a list of ports that have an Arduino microcontroller attached.";
$SupportedArduinos::usage = "$SupportedArduinos gives a list of Arduinos supported by ArduinoLink.";

 (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  *	DYNAMIC CONNECTION TO THE MICROCONTROLLER														*
  *  These functions are used to dynamically connect to the Arduino microcontroller and allow 		*
  *  direct control by Mathematica over the I/O functionality of the connected device. Note that 	*
  *  these functions are used to bind to a single device, specified internally as $CurrentArduino.	* 
  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

ArduinoConnect::usage = "ArduinoConnect[port] connects Mathematica to the Arduino on the specified port, binding the Arduino I/O functions to the microcontroller on the specified port.";   

ArduinoConnectModules::usage = "ArduinoConnectModules[] returns a list of connect modules available for loading using ArduinoConnect.";
ArduinoConnectModule::usage = "ArduinoConnectLibrary is a symbolic representation of an Arduino connect module.";
ArduinoInstallConnectModule::usage = "ArduinoInstallConnectModule[module_] installs the module with the specified name into the ArduinoConnect environment.";
ArduinoConnectModuleRegister::usage = "ArduinoConnectModuleRegister[{preamble}, {functionName-> functionCode, ..}] registers a connect module \
 					from the Symbolic C code given in the preamble (for includes and declarations) and the functionCode in functionName functions. \
 					The Symbolic C code is incorporated into the overall server sketch that is generated and uploaded to the Arduino.";

ArduinoConnectInput::usage = "ArduinoConnectInput is a code generation symbol in a connect module that is replaced by a serial I/O call in the server code uploaded to the device.";
ArduinoConnectOutput::usage = "ArduinoConnectOutput is a code generation symbol in a connect module that is replaced by a serial I/O call in the server code uploaded to the device.";

ArduinoRun::usage = "ArduinoRun[function, {arg1, arg2, ...}] runs the specified function on the Arduino device with the given arguments.";
ArduinoDigitalWrite::usage = "ArduinoDigitalWrite[port, val] writes the specified value to the given digital port on the currently connected Arduino.";
ArduinoDigitalRead::usage = "ArduinoDigitalRead[port] reads the digital value of the specified port on the currently connected Arduino.";
ArduinoAnalogWrite::usage = "ArduinoAnalogWrite[port, val] writes the specified value as a PWM signal to the given PWM port on the currently connected Arduino.";
ArduinoAnalogRead::usage = "ArduinoAnalogRead[port] reads the analog value of the specified port on the currently connected Arduino.";

(* Constants *)
High
Low

 (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  *	CONFIGURATION AND ERROR HANDLING																*
  *  These symbols are used for configuration and error handling purposes.							*
  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

ArduinoLinkConfigure::usage = "ArduinoLinkConfigure[options] sets various environment parameters required for ArudinoLink to work.  Default settings are used if ArduinoLinkConfigure is not evaluated.";
$DefaultArduinoIDEPath::usage = "$DefaultArduinoIDEPath gives the default path to the Arduino IDE installation directory for the current operating system.";
$DefaultAVRCrossCompilerPath::usage = "$DefaultAVRCrossCompilerPath gives the default path to the AVR Cross Compiler for the current operating system.";

ArduinoLink::upload = "The generated sketch was not able to be uploaded onto the Arduino device at `1`";
ArduinoLink::support = "ArduinoLink does not support `1`";
ArduinoLink::notfound = "No Arduino device was found at `1`."
ArduinoLink::noserial = "No serial connection can be established to the specified device. This happens if there is no serial device connected to the specified port, or when another \
						program has already claimed the serial port.";
ArduinoLink::dependency = 
"ArduinoLink dependencies: \n \
 - version 0022 of the Arduino IDE, installed in the Applications folder (http://arduino.cc/en/Main/Software) \n \
 - FTDI USB Serial Driver - ships with the Arduino IDE (http://arduino.cc/en/Main/Software) \n \
 Wolfram Research Inc. is not affiliated with any of these third-party software providers.";
ArduinoLink::init = "An Arduino microcontroller must be connected in order to proceed with initialization.";
ArduinoLink::noinit = "ArduinoLink failed to initialize.";


(* Taken out of ArduinoLink::dependency message
  - CrossPack version 20100115 for AVR Development (http://www.obdev.at/products/crosspack/download.html) \n \ *)

Begin["`Private`"]

(* Directory constants *)
$MyPackageDirectory = DirectoryName[System`Private`$InputFileName];
(*$ArduinoLibraryDirectory = "/Applications/Arduino.app/Contents/Resources/Java";*)

(* Loads the package version of SerialIO for OSX *)
AppendTo[$Path, $MyPackageDirectory];
If[ Length[Names["SerialIO`*"]] == 0,
	Get["SerialIO`"];
]

(* ArduinoLink Environment Configuration *)
$ArduinoIDEDirectory;
$AVRCrossCompilerDirectory;
$ArduinoLibraryDirectory;

$DefaultArduinoIDEPath := Switch[$OperatingSystem, 
	"MacOSX", "/Applications/Arduino.app",
	_, None ]

$DefaultAVRCrossCompilerPath:= Switch[$OperatingSystem,
	"MacOSX", "/usr/local/CrossPack-AVR",
	_, None ]

Options[ArduinoLinkConfigure] = {"ArduinoIDEPath" :> $DefaultArduinoIDEPath, "AVRCrossCompilerPath" :> $DefaultAVRCrossCompilerPath}

ArduinoLinkConfigure[opts:OptionsPattern[]]:= (
	$ArduinoIDEDirectory = OptionValue@"ArduinoIDEPath";
	$ArduinoLibraryDirectory = Switch[$OperatingSystem,
		"MacOSX", FileNameJoin[{$ArduinoIDEDirectory, "Contents", "Resources", "Java"}],
		_, None
	];
	$AVRCrossCompilerDirectory = OptionValue@"AVRCrossCompilerPath";
	If[ ValidateArduinoLinkEnvironment[],
		InitializeArduinoLink[]];
)

(* Dependency tests. Ensures that the Arduino IDE is installed in the user's Applications directory,
   and checks for CrossPack availability.	*)
ValidateArduinoLinkEnvironment[]:= (
	If[ Not[ FileExistsQ[$ArduinoIDEDirectory]] || Not[ FileExistsQ[$AVRCrossCompilerDirectory]], 
		Message[ArduinoLink::dependency]; False, True];
)

(* Configure and validate environment on package load *)
ArduinoLinkConfigure[];

(* Global ArduinoConnect data *)
$CurrentArduino = None;
$ArduinoConnectModules = {};
$ArduinoConnectCommandBlockingMap = {};
$ArduinoConnectCommandNames = {};
$ArduinoConnectGlobal = {};
$ArduinoConnectSetup = {};
$ArduinoConnectFunctionMap = {};

(* Internal data for code generation *)
$ArduinoBaudRate = 9600;
$ArduinoSymbolTable = {};
$ArduinoBaseIncludeLibraries = {"WProgram.h"};
$ArduinoIncludeLibraries = {};

(*  Creates a map from file descriptors to SerialPort objects, and a map from
	file descriptors to string buffers where serial input data is stored. *)
If[Not[ ValueQ[$ArduinoSerialPortMap] ], $ArduinoSerialPortMap = { } ];
If[Not[ ValueQ[$ArduinoSerialBuffer] ], $ArduinoSerialBuffer = { } ];

(* External constants *)
Low = 0;
High = 1;

$Arduino = {
	(* Most recent Arduino boards *)
	"Uno" -> 			{"atmega328p", 115200, 16},
	"Duemilanove" -> 	{"atmega328p", 57600, 16},
	"Mega" ->			{"atmega2560", 115200, 16},
	"Mini" ->			{"atmega168", 19200, 16},
	"Fio" ->			{"atmega328p", 57600, 8},
	"BT" ->				{"atmega328p", 19200, 16},
	"LilyPad" ->		{"atmega328p", 57600, 8},
	"Pro" ->			{"atmega328p", 57600, 16},
	"Pro Mini" ->		{"atmega328p", 57600, 16},
	"NG" ->				{"atmega168", 19200, 16},
	(* Older Arduino boards *)
	"Duemilanove w/ ATmega168" -> 	{"atmega168", 19200, 16},
	"Mega w/ ATmega1280" -> 		{"atmega1280", 57600, 16},
	"BT w/ ATmega168" -> 			{"atmega168", 19200, 16},
	"LilyPad w/ ATmega168" -> 		{"atmega168", 19200, 8},
	"Pro w/ ATmega168" -> 			{"atmega168", 19200, 16},
	"Pro Mini w/ ATmega168" -> 		{"atmega168", 19200, 16},
	"Pro 3.3V w/ ATmega328" ->		{"atmega328p", 57600, 8},
	"Pro 3.3V w/ ATmega168" -> 		{"atmega168", 19200, 8},
	"NG w/ ATmega8" -> 				{"atmega8", 19200, 16}
}
(*  Populate the supported microcontroller list.   *)
$SupportedArduinos = First /@ $Arduino;
$SupportedMicrocontroller = DeleteDuplicates[First[# /. $Arduino] & /@ $SupportedArduinos]

(*  Creates an Arduino object used to internally describe a device connected to a given port, which can be
	configured to a microcontroller from $SupportedArduinos. *)	
CreateArduinoObject[port_String, microcontroller_String, protocol_String, baud_Integer] :=
(	$ArduinoBaudRate = baud;
	If[ MemberQ[ FileNames["tty.usb*", {"/dev/"} ], port ],
		If[MemberQ[ $SupportedArduinos, microcontroller],
			Arduino @@ Join[{port},
				If[MemberQ[First /@ $Arduino, microcontroller], microcontroller /. $Arduino, "Uno" /. $Arduino], {protocol, baud}],
			Message[ArduinoLink::support, microcontroller]; $Failed
		], Message[ArduinoLink::notfound, port]; $Failed
	])

 (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  *	INTERFACE TO HARDWARE																			*
  *  These functions are used as the primary interfacing symbols between Mathematica and the actual	*
  *  port that the Arduino is connected to.															*
  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
ArduinoList[]:= FileNames[{"tty.usbserial*", "tty.usbmodem*"}, {"/dev/"}]

(*  Uploads a code generated sketch onto the specified Arduino microcontroller. *)
ArduinoUpload[ device_Arduino, predecl_List, setup_List, loop_List ] := 
	If[ ArduinoQ[device], 
		ArduinoUploadC[device, ArduinoGenerateC[predecl, setup, loop]],
		Message[ArduinoLink::notfound,First[device]]
	]

(* 	Generates a C code representation of the given symbolic structure, given as a list of predeclaration constructs, a
	list of setup constructs, and a list of constructs to be run repeatedly in a loop. *)
ArduinoGenerateC[predecl_List, setup_List, loop_List] := 
	ToCCodeString[
		CProgram @@ Flatten[{
			ArduinoGenerateIncludeLibraries[],
			SelectSymbolsWithHead[predecl, {CInclude}, 1],
			ArduinoGenerateVariableDeclarations[predecl],
			ArduinoGenerateFunctionHeaders[predecl], 
			SelectSymbolsWithHead[predecl, {CFunction}, 1],
			CFunction["void", "setup", {}, setup],
			CFunction["void", "loop", {}, loop]
		}]]

(* Generates include libraries from the internal list of libraries to be included on the build path. *)
ArduinoGenerateIncludeLibraries[] := DeleteDuplicates[CInclude /@ Join[{$ArduinoIncludeLibraries, $ArduinoBaseIncludeLibraries}]]

(* Generates function headers from the given predeclaration source. *)
ArduinoGenerateFunctionHeaders[predecl_] := 
	DeleteDuplicates[Flatten[ Join[{ CFunction["void","loop",{}], CFunction["void","setup",{}] },
		Map[ CFunction @@ Take[Level[#, {1}], 3] &, SelectSymbolsWithHead[predecl, {CFunction}, 1] ]]]]

(* Generates global variable declarations from the given predeclaration source. *)
ArduinoGenerateVariableDeclarations[predecl_] := SelectSymbolsWithHead[predecl, {CDeclare}, 1]

(*  Selects symbols from the source list of SymbolicC constructs that have a head in the list of acceptable function heads, with
 	an option to specify the maximum level to take constructs from within the source list. *)
SelectSymbolsWithHead[source_List, head_List, maxLevel_ : Infinity] := Select[ Level[source, {1, maxLevel}], MemberQ[head, Head[#]] &] 

 (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  *  SERIAL I/O WRAPPER  																			*
  *  These functions are used as a wrapper on the Serial I/O package, taken from the Wolfram		*
  *  Library Archives. These functions maintain a reference to all open serial ports, to allow		*
  *  them to be gracefully closed later. Also, they decode input data from the Arduino and encode	*
  *  output data in the Arduino's preferred format.													*
  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

(* Opens a serial connection with the specified Arduino if one does not exist already. *)
ArduinoSerialBegin[arduino_Arduino] := 
	Module[ { com }, If[ Not[ ArduinoSerialOpenQ[ arduino ] ],
		com = SerialOpen[ First[arduino], "BaudRate" -> Last[arduino] ];
		If [ com === $Failed,
			$Failed,
			AppendTo[$ArduinoSerialPortMap, {First[arduino], com } ];
			AppendTo[$ArduinoSerialBuffer, {First[arduino], "" } ];
		]
	]]

(* Ends the serial connection with the specified Arduino. *)
ArduinoSerialEnd[arduino_Arduino] :=
	With[{ port = ArduinoGetSerialPort[arduino] },  
	If[ Not[ port === $Failed ],
		SerialClose[port];
		$ArduinoSerialPortMap = Select[ $ArduinoSerialPortMap, First[#] != First[arduino] &];
		$ArduinoSerialBuffer = Select[ $ArduinoSerialBuffer, First[#] != First[arduino] &];
		, $Failed
	]]

(* Returns the symbolic port for the given Arduino microcontroller. *)
ArduinoGetSerialPort[arduino_Arduino] :=
	With[ {serialdevices = Flatten[Select[ $ArduinoSerialPortMap, First[#] == First[arduino] &]]},
		If[ Length[serialdevices] > 0, Last[serialdevices], $Failed]]

(* Reads the next token of incoming data from the Arduino. *)
ArduinoSerialRead[arduino_Arduino] :=
	If[ ArduinoQ[arduino],
		If[ Not[ArduinoSerialOpenQ[arduino]],
			ArduinoSerialBegin[arduino]];
		ArduinoSerialReadBuffer[arduino];
		ArduinoSerialNext[arduino],
	Message[ArduinoLink::notfound, First[arduino]]; $Failed]

(* Reads the entire serial buffer into memory. *)
ArduinoSerialReadBuffer[arduino_] :=
	With[ {com = ArduinoGetSerialPort[arduino]},
		While[ SerialReadyQ[com], ArduinoSerialAddToBuffer[arduino, SerialRead[com, 1]]]; ]

(* Returns the next token of serial data from the Arduino. *)
ArduinoSerialNext[arduino_Arduino] := 
	If[ArduinoSerialAvailableQ[arduino],
	Module[ {serialBuffer = ArduinoSerialGetBuffer[arduino], next, elem},
	 	next = StringCases[serialBuffer, Shortest["!!" ~~ data__ ~~ "$$"] -> data, 1];
	 	If[Length[next] == 1,
	 		elem = FromDigits[StringCases[First[next], DigitCharacter ..]];
	 		ArduinoSerialSetBuffer[arduino, StringReplace[serialBuffer, Shortest["!!" ~~ __ ~~ "$$"] ~~ rest___ :> rest, 1]];
	 		elem
	 	]
	]]

(* Returns True if there is serial data available to be read from the Arduino, and False otherwise. *)
ArduinoSerialAvailableQ[arduino_Arduino] := 
	If[ ArduinoQ[arduino],
		If[ Not[ArduinoSerialOpenQ[arduino]],
			ArduinoSerialBegin[arduino]];
		ArduinoSerialReadBuffer[arduino];
		StringMatchQ[ArduinoSerialGetBuffer[arduino], "!!" ~~ __ ~~ "$$"]
	]

(* Writes the given value on the serial connection to the specified Arduino. *)
ArduinoSerialWrite[arduino_Arduino, val_] :=
	If[ ArduinoQ[arduino],
		If[ Not[ArduinoSerialOpenQ[arduino]],
			ArduinoSerialBegin[arduino]];
		SerialWrite[ArduinoGetSerialPort[arduino], ToString[val]];,
	Message[ArduinoLink::notfound, First[arduino]]; $Failed]

(* Returns the serial buffer, which stores serial data received from the given Arduino. *)
ArduinoSerialGetBuffer[arduino_Arduino] := 
	Block[ {buffer}, If[(buffer = Flatten[Select[$ArduinoSerialBuffer, First[#] == First[arduino] &]]) === {}, "", buffer[[2]]]]

(* Sets the internal serial buffer to the specified value. *)
ArduinoSerialSetBuffer[arduino_Arduino, buff_] := 
With[{pos = First[Flatten[Position[$ArduinoSerialBuffer, First[arduino]]]]},
	$ArduinoSerialBuffer = ReplacePart[ $ArduinoSerialBuffer, pos :> { First[arduino], buff }]; ]

(* Adds the given data to the internal serial buffer for the specified Arduino. *)
ArduinoSerialAddToBuffer[arduino_Arduino, buff_] := 
With[ {pos = First[Flatten[Position[$ArduinoSerialBuffer, First[arduino]]]]},
	$ArduinoSerialBuffer = ReplacePart[ $ArduinoSerialBuffer, 
		pos :> { First[arduino], StringJoin[$ArduinoSerialBuffer[[pos, 2]], buff] }]; ]

(* Deletes the specified number of characters from the serial buffer for the specified Arduino. *)
ArduinoSerialDeleteFromBuffer[arduino_Arduino, length_] := 
With[ {pos = First[Flatten[Position[$ArduinoSerialBuffer, First[arduino]]]]},
	$ArduinoSerialBuffer = ReplacePart[ $ArduinoSerialBuffer, 
			pos :> { First[arduino], StringDrop[$ArduinoSerialBuffer[[pos, 2]], length] }];]

(* Flushes the internal serial buffer for the specified Arduino. *)
ArduinoSerialFlushBuffer[arduino_Arduino] :=
(	pos = Flatten[Position[$ArduinoSerialBuffer, First[arduino]]];
	If[ pos != {}, $ArduinoSerialBuffer[[pos, 2]] = ""];	
)

(* Returns true if there is a serial connection to the specified Arduino open. *)
ArduinoSerialOpenQ[arduino_Arduino] := ( ArduinoQ[arduino] && MemberQ[$ArduinoSerialPortMap, First[arduino], {2}] )

 (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  *	DYNAMIC CONNECTION TO THE MICROCONTROLLER														*
  *  These functions are used to dynamically connect to the Arduino microcontroller and allow 		*
  *  direct control by Mathematica over the I/O functionality of the connected device. Note that 	*
  *  these functions are used to bind to a single device, specified internally as $CurrentArduino.	* 
  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

(*  Registers a connect module. Refer to the documentation for more information on registering a connect module. *)
ArduinoConnectModuleRegister[globals_, functionMap_, setup_:{ }] := 
	ArduinoConnectLibrary[GenerateFunctionCommands[functionMap], globals, GenerateConnectFunctions[globals, functionMap], setup ]

(*  Returns a mapping between each function in the function map and a True/False value, based on whether the function
	produces output that needs to be read by Mathematica. This determines whether the function will block during ArduinoRun. *)
GenerateFunctionCommands[functionMap_] := 
	(First[#] -> (Length[SelectSymbolsWithHead[Last[#], {ArduinoConnectOutput}]] > 0) ) & /@ functionMap 

(* Converts the function mapping to a SymbolicC representation of the function, and replaces the ArduinoConnectInput/ArduinoConnectOutput
   code generation symbol with the corresponding I/O call for the type of data being requested. *)
GenerateConnectFunctions[globals_, functionMap_] :=
	Module[ { symbolTable = (Level[#, 1] & /@ SelectSymbolsWithHead[globals, {CDeclare}]) },
		CFunction["void", First[#], {}, Last[#] /.	{
			ArduinoConnectInput[var_String] :> 
				If[MemberQ[symbolTable, var, {2}],
					CAssign[var, CCall["_next" <> Select[symbolTable, Last[#] == var &][[1,1]], {}]],
					CStatement[""] ],
			ArduinoConnectOutput[var_String] :>
				If[MemberQ[symbolTable, var, {2}],
					CCall["_send" <> Select[symbolTable, Last[#] == var &][[1,1]], {var}],
					CStatement[""] ]
   			}] & /@ functionMap
	]

(* Installs a connect module and appends the corresponding functions to the global register of  *)
ArduinoInstallConnectModule[module_String, args_List : { }] := 
	If[ Not[ MemberQ[$ArduinoConnectModules, module] ] && MemberQ[ArduinoConnectModules[], module],
		AppendTo[$ArduinoConnectModules, module];
		Get[ FileNameJoin[{ $MyPackageDirectory, "ConnectModules", module <> ".m" }]];
		library = Symbol[module][args];
		If[ Head[library] === ArduinoConnectLibrary,
			If[Not[MemberQ[$ArduinoConnectCommandBlockingMap, #]],
				AppendTo[$ArduinoConnectCommandBlockingMap, #]] & /@ First[library];
			If[Not[MemberQ[$ArduinoConnectCommandNames, First[#]]],
				AppendTo[$ArduinoConnectCommandNames, First[#]]] & /@ First[library];
			If[Not[MemberQ[$ArduinoConnectGlobal, #]],
				AppendTo[$ArduinoConnectGlobal, #]] & /@ library[[2]];
			If[Not[MemberQ[$ArduinoConnectFunctionMap, #]],
				AppendTo[$ArduinoConnectFunctionMap, #]] & /@ library[[3]];
			If[Not[MemberQ[$ArduinoConnectSetup, #]],
				AppendTo[$ArduinoConnectSetup, #]] & /@ library[[4]];
			,
			$Failed
		]];

(* Returns a list of connect modules that are available to be installed from source code contained in the package. *)
ArduinoConnectModules[] := StringDrop[#, -2] & /@ (FileNameTake /@ 
   FileNames[FileNameJoin[{$MyPackageDirectory, "ConnectModules", "*"}]])

(* Installs the prerequisite modules. *)
ArduinoInstallConnectModule["Serial", {}];
ArduinoInstallConnectModule["PinIO", {}];

(* Options for connecting to the Arduino *)
Options[ArduinoConnect] = {
	"BaudRate" -> 9600, 				(* Specifies the baud rate that the Arduino will use in I/O with Mathematica *)
	"Include" -> None, 					(* Include libraries *)
	"Microcontroller" -> "Uno", 		(* Microcontroller, by default the Arduino Uno *)
	"UploadProtocol" -> "stk500v1", 	(* Default *)
	"UploadSpeed" -> 115200				(* Varies based on specific Arduino model *)
}
(* Binds to the Arduino microcontroller on the specified port. *)
ArduinoConnect[port_String, opts:OptionsPattern[]] := InternalArduinoConnect[
		CreateArduinoObject[port, OptionValue@"Microcontroller", OptionValue@"UploadProtocol", OptionValue@"BaudRate"], 
			Sequence @@ FilterRules[{opts}, Options[InternalArduinoConnect]]]

(* Optional specification of modules to include. *)
Options[InternalArduinoConnect] = {"Include" -> None}
(* Internal ArduinoConnect functionality, with the given option mask. *)
InternalArduinoConnect[arduino_Arduino, opts:OptionsPattern[]] :=
(	$CurrentArduino = arduino;
	Map[(ArduinoInstallConnectModule @@ #) &, Replace[OptionValue["Include"], None -> {}]];
	ArduinoUpload @@ Join[{arduino}, ArduinoConnectGenerate[]];
	(* Blink, used to align serial communication. Also, if connection problems arise, this will
	   make them known immediately. *)
	ArduinoDigitalWrite[13,Low];
	Pause[1];
	ArduinoDigitalWrite[13,High];
	Pause[1];
	ArduinoDigitalWrite[13, Low];
)

(* Generates the arguments to ArduinoUpload that is used to generate the server sketch for the Arduino *)
ArduinoConnectGenerate[] :=
 {Join[$ArduinoConnectFunctionMap, $ArduinoConnectGlobal], 
	{CCall[CMember["Serial", "begin"], {$ArduinoBaudRate}]}, 
	Join[ {	CDeclare["char", CAssign["command", CCall["_nextchar", {}]]] },
 		{	MapThread[CIf[COperator[Equal, {"command", #1}], #2] &,
  			{ArduinoConnectFunctionCode /@ $ArduinoConnectCommandNames,
  			CCall[#, {}] & /@ $ArduinoConnectCommandNames}
     	]}]
	}

(*  Returns the function code (the character sent by ArduinoRun to run the corresponding function) for the specified command
	that is loaded into the server sketch. *)
ArduinoConnectFunctionCode[function_] :=
	If[ MemberQ[$ArduinoConnectCommandNames, function], 
		32 + Position[$ArduinoConnectCommandNames, function][[1,1]], 32]

(*  Runs the specified function with the given list of arguments on the Arduino that is currently connected by ArduinoConnect. *)
ArduinoRun[function_String, args_List : { }] :=
	Refresh[PreemptProtect[( 
	If[ArduinoQ[$CurrentArduino] && MemberQ[$ArduinoConnectCommandNames, function],
		ArduinoSerialWrite[$CurrentArduino, 
			FromCharacterCode[ArduinoConnectFunctionCode[function]] <>
			StringJoin[ ( ToString[#] <> "/" & /@ args ) ]];
		If[function /. $ArduinoConnectCommandBlockingMap,
			While[Not[ArduinoSerialAvailableQ[$CurrentArduino]]];
			ToExpression@ArduinoSerialRead[$CurrentArduino]
		],
		$Failed	
	])], None]

(* Wrapper functions on top of the Pin I/O module *)
ArduinoDigitalWrite[port_, value_] := ArduinoRun["ArduinoDigitalWrite", {port, If[value == Low, 0, 1]}]
ArduinoDigitalRead[port_] := ArduinoRun["ArduinoDigitalRead", {port}]
ArduinoAnalogWrite[port_, value_] := ArduinoRun["ArduinoAnalogWrite", {port, value}]
ArduinoAnalogRead[port_] := ArduinoRun["ArduinoAnalogRead", {port}]

(* Returns True if the symbolic Arduino object corresponds to an actively connected Arduino microcontroller. *)
ArduinoQ[arduino_Arduino] := MemberQ[ArduinoList[], First[arduino]]
ArduinoQ[arduino_] := False

 (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  *	CODE GENERATION AND UPLOADING																	*
  * 	These functions use the AVR cross compiler to upload the given sketch onto the specified	*
  *  Arduino microcontroller. It ensures that the serial communication with the Arduino is closed   *
  *  before uploading, so the user does not have to manually manage both serial communication links.*
  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

ArduinoUploadC[arduino_, sketch_String] := 
	Module [ {curdir = Directory[], fname = CreateDirectory[], success = False},
		(* Ensure the serial port is closed before uploading *)
		If[ ArduinoSerialOpenQ[arduino] == True,
			ArduinoSerialEnd[arduino];
			ArduinoSerialFlushBuffer[arduino];];
		
		(* Set up a build in a temporary directory *)
		CopyFile[ FileNameJoin[{ $MyPackageDirectory, "CrossCompiler", "Makefile" }], FileNameJoin[{fname, "Makefile"}]];
		SetDirectory[fname];
		Export["device", ArduinoDeviceSpecification[arduino], "String"];
		Export["environment", ArduinoEnvironmentSpecification[arduino], "String"];
		Export[StringJoin[ FileNameTake[fname], ".cpp" ], sketch, "String"];
		
		(* Execute the build and upload it to the device *)
		success = Run[FileNameJoin[{ $MyPackageDirectory, "CrossCompiler", "make" }]];
		success = Run[FileNameJoin[{ $MyPackageDirectory, "CrossCompiler", "make upload" }]];
		
		(* Delete temp directory and return *)
		SetDirectory[ParentDirectory[]];
		(*DeleteDirectory[fname, DeleteContents -> True];*)
		SetDirectory[curdir];
		If [ success != 0, Message[ArduinoLink::upload, fname]];
	]

ArduinoDeviceSpecification[arduino_Arduino] :=
	" ARDUINO_PORT ?= "<> arduino[[1]] <> "\n" <>
	" ARDUINO_UPLOAD_RATE ?= "<> ToString[arduino[[3]]] <> " \n" <>
	" ARDUINO_AVRDUDE_PROGRAMMER ?= "<> ToString[arduino[[5]]] <> " \n" <>
	" ARDUINO_MCU ?= "<> arduino[[2]] <>" \n" <>
	" ARDUINO_F_CPU ?= "<> ToString[arduino[[4]]] <>"000000\n"

ArduinoEnvironmentSpecification[arduino_Arduino] :=
	" INSTALL_DIR = " <> $ArduinoLibraryDirectory <>"\n" <>
	" ADDON_LIB_DIR = "<> FileNameJoin[{$MyPackageDirectory, "ConnectLibraries"}]

InitializeArduinoLink[] :=
Module [ {curdir, fname, success, arduino = ArduinoList[], sketch = ArduinoGenerateC[{},{},{}]},
	If[Length[arduino] == 0, Message[ArduinoLink::init],
		arduino = First[arduino];
		(* Ensure the serial port is closed before uploading *)
		If[ ArduinoSerialOpenQ[arduino] == True,
			ArduinoSerialEnd[arduino];
			ArduinoSerialFlushBuffer[arduino];
		];
		(* Ensures that builds work properly *)
		curdir = Directory[];
		fname = CreateDirectory[];
		CopyFile[ FileNameJoin[{ $MyPackageDirectory, "CrossCompiler", "Makefile" }], FileNameJoin[{fname, "Makefile"}]];
		SetDirectory[fname];
		Export["device", ArduinoDeviceSpecification[arduino], "String"];
		Export["environment", ArduinoEnvironmentSpecification[arduino], "String"];
		Export[StringJoin[ FileNameTake[fname], ".cpp" ], sketch, "String"];
		success = 1;
		timeout = 0;
		(* Execute the build and upload it to the device, repeating if Arduino IDE has uncompiled errors *)
		While[success != 0 && timeout < 100,
			success = Run[FileNameJoin[{ $AVRCrossCompilerDirectory, "make" }]];
			success = Run[FileNameJoin[{ $AVRCrossCompilerDirectory, "make upload" }]];
			timeout++;
		];
		If[timeout == 100, Message[ArduinoLink::noinit]];
		(* Delete temp directory and return *)
		SetDirectory[ParentDirectory[]];
		DeleteDirectory[fname, DeleteContents -> True];
		SetDirectory[curdir];
	];
]

(* Unimplemented, ideally will fetch all prerequisites from git repository. *)
InstallArduinoLink[] :=
	If[StringMatchQ[Import["!git --version", "TEXT"], "git version" ~~ __],
		0
	]
 
FromRealDigits[n_] := 
	With[{strnum = StringSplit[n, "."]}, 
  		N[  (* value *) FromDigits[First[strnum]] + FromDigits[Last[strnum]] / 10^StringLength[Last[strnum]],
   			(* precision *) StringLength[n] - 1]]

End[]

EndPackage[]