(* Mathematica package *)

Options[AcceleroMMA7361] = {"Voltage" -> 3.3, "Sensitivity" -> 0, 
	"SleepPin" -> 13, "SelfTestPin" -> 12, "ZeroGPin" -> 11, "GSelectPin" -> 10, "xPin" -> 0, "yPin" -> 1, "zPin" -> 2}

AcceleroMMA7361[OptionsPattern[]] :=
	ArduinoConnectModuleRegister[
		{CInclude["AcceleroMMA7361.h"], CDeclare["AcceleroMMA7361", "accelero"], CDeclare["int", "acceleroValue"] },
		{
			"AcceleroInitialize" -> 
				{	CMember["accelero", CCall["begin", {OptionValue[#]& /@ 
					{"SleepPin", "SelfTestPin", "ZeroGPin", "GSelectPin", "xPin", "yPin", "zPin"}}]],
					CMember["accelero", CCall["setARefVoltage", {OptionValue["Voltage"] }]],
					CMember["accelero", CCall["setSensitivity", {OptionValue["Sensitivity"] }]] },
			"AcceleroCalibrate" ->
				{	CMember["accelero", CCall["calibrate", {}]] },
			"AcceleroGetRawX" ->
				{	CAssign["acceleroValue", CMember["accelero", CCall["getXRaw", {}]]],
					ArduinoConnectOutput["acceleroValue"] },
			"AcceleroGetRawY" ->
				{	CAssign["acceleroValue", CMember["accelero", CCall["getYRaw", {}]]],
					ArduinoConnectOutput["acceleroValue"] },
			"AcceleroGetRawZ" ->
				{	CAssign["acceleroValue", CMember["accelero", CCall["getZRaw", {}]]],
					ArduinoConnectOutput["acceleroValue"] },
			"AcceleroGetXAccel" -> 
				{	CAssign["acceleroValue", CMember["accelero", CCall["getXAccel", {}]]],
					ArduinoConnectOutput["acceleroValue"]},
			"AcceleroGetYAccel" -> 
				{	CAssign["acceleroValue", CMember["accelero", CCall["getYAccel", {}]]],
					ArduinoConnectOutput["acceleroValue"]},
			"AcceleroGetZAccel" -> 
				{	CAssign["acceleroValue", CMember["accelero", CCall["getZAccel", {}]]],
					ArduinoConnectOutput["acceleroValue"]}
		}
	]