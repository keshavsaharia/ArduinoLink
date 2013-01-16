(* Mathematica package *)

Options[PinIO] = {};

PinIO[OptionsPattern[]] := 
	ArduinoConnectModuleRegister[
		{CDeclare["int", "PinIOport"], CDeclare["int", "PinIOvalue"]},
		{
			"ArduinoDigitalWrite" -> 
				{	ArduinoConnectInput["PinIOport"],
					CCall["pinMode", {"PinIOport", "OUTPUT"}],
					ArduinoConnectInput["PinIOvalue"], 
					CCall["digitalWrite", {"PinIOport", 
						CConditional[COperator[Equal, {"PinIOvalue", 1}], "HIGH", "LOW"]}] },
			"ArduinoDigitalRead" ->
				{	ArduinoConnectInput["PinIOport"],
					CCall["pinMode", {"PinIOport", "INPUT"}],
					CAssign["PinIOvalue", CCall["digitalRead", {"PinIOport"}]],
					ArduinoConnectOutput["PinIOvalue"] },
			"ArduinoAnalogWrite" ->
				{	ArduinoConnectInput["PinIOport"],
					CCall["pinMode", {"PinIOport", "OUTPUT"}],
					ArduinoConnectInput["PinIOvalue"],
					CCall["analogWrite", {"PinIOport", "PinIOvalue"}] },
			"ArduinoAnalogRead" ->
				{	ArduinoConnectInput["PinIOport"],
					CCall["pinMode", {"PinIOport", "INPUT"}],
					CAssign["PinIOvalue", CCall["analogRead", {"PinIOport"}]],
					ArduinoConnectOutput["PinIOvalue"] }
		}]