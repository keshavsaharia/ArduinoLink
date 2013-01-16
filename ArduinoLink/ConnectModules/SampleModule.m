(* Mathematica package *)

Options[SampleModule] := {"SampleField" -> 5}

SampleModule[OptionsPattern[]] :=
	ArduinoConnectModuleRegister[
		{	CInclude["MyLibrary.h"],
			CDeclare["int", "SampleModuleNumber"],
			CDeclare["int", "SampleModuleData"]
			(* Other global statements, variable declarations, function declarations *)
		},
		{
			"FunctionName" -> { CCall["libraryFunction", {}] },
			"FunctionThatTakesData" -> {
					(* Reads an integer into the SampleModuleNumber variable, and passes it to a library function *)
					ArduinoConnectInput["SampleModuleNumber"],
					CCall["otherLibraryFunction", {"SampleModuleNumber"}]
				},
			"FunctionThatProducesData" -> {
					(* Puts the integer value of 99 on the serial connection to Mathematica *)
					CAssign["SampleModuleData", 99],
					ArduinoConnectOutput["SampleModuleData"],
					(* Another output example, where a library function is called with the specified arguments *)
					CAssign["SampleModuleNumber", CCall["getSomeLibraryData", {"arg1", "arg2"}]],
					ArduinoConnectOutput["SampleModuleNumber"]
				}
		},
		{}
	]