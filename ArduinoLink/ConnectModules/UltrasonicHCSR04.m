(* Mathematica package *)

Options[UltrasonicHCSR04] = {"TriggerPin" -> 7, "EchoPin" -> 10}

UltrasonicHCSR04[OptionsPattern[]] :=
	ArduinoConnectModuleRegister[
		{CInclude["Ultrasonic.h"], CStatement["Ultrasonic ultrasonic = Ultrasonic(" <> ToString[OptionValue@"TriggerPin"] <> "," <> ToString[OptionValue@"EchoPin"] <>")"], CDeclare["int", "ultrasonicRange"]},
		{
			"UltrasonicRange" -> {
				CAssign["ultrasonicRange", CMember["ultrasonic", CCall["Ranging", {0}]]],
				ArduinoConnectOutput["ultrasonicRange"]
			}
		}
	]