(* Mathematica package *)

Options[Servo] = {"Servos" -> 1}

Servo[OptionsPattern[]] := 
	ArduinoConnectModuleRegister[
		Join[ {CInclude["Servo.h"], CDeclare["int", "ServoPort"], CDeclare["int", "ServoValue"] }, 
			  Map[ CDeclare["Servo", "Servo"<>ToString[#]] &, Range[1, OptionValue["Servos"]] ] ],
		{ 
			"ArduinoServoAttach" -> {
				ArduinoConnectInput["ServoPort"],
				ArduinoConnectInput["ServoValue"],
				CIf[COperator[Equal, {"ServoPort", #}], 
					CMember["Servo" <> ToString[#], CCall["attach", {"ServoValue"}]]
				] & /@ Range[1, OptionValue["Servos"]] },
			"ArduinoServoWrite" -> {
				ArduinoConnectInput["ServoPort"],
				ArduinoConnectInput["ServoValue"],
				CIf[COperator[Equal, {"ServoPort", #}], 
					CMember["Servo" <> ToString[#], CCall["write", {"ServoValue"}]]
				] & /@ Range[1, OptionValue["Servos"]] 	
			}
		}
	]