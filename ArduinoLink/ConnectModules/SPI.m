(* Mathematica package *)

Options[SPI] = {"SS" -> 10, "MOSI" -> 11, "MISO" -> 12, "SCK" -> 13, 
				"BitOrder" -> "MSBFIRST", "Delimiter" -> "'\\n'", "ClockDivider" -> 8}

SPI[OptionsPattern[]] :=
	ArduinoConnectModuleRegister[
		{	CInclude["SPI.h"],
			CDeclare["int", CArray["spi_buffer", 100]], CDeclare["int", "spi_input"], CDeclare["volatile int", "spi_buffer_pos"],
			CDeclare["int", "spi_clock_divider"], CDeclare["volatile boolean", "spi_slave_process"],
			CFunction["ISR", "", {"SPI_STC_vect"}, {
  					CDeclare["byte", CAssign["b", "SPDR"]],
					CIf[COperator[Less, {"spi_buffer_pos", 100}],
   						CAssign[CArray["spi_buffer", "spi_buffer_pos"], "b"],
   						CAssign["spi_slave_process", "true"]
   					],
  					CIf[COperator[Equal, {"b", OptionValue["Delimiter"]}],
   						CAssign["spi_slave_process", "true"]
   					]}]
		}, {
			"ArduinoSPIBeginMaster" -> { 
				CAssign["spi_buffer_pos", 0], 
				CCall[CMember["SPI", "begin"], {}], 
				CCall[CMember["SPI", "setClockDivider"], {"SPI_CLOCK_DIV" <> ToString[OptionValue["ClockDivider"]]}]
			},
			"ArduinoSPIEndMaster" -> { CCall[CMember["SPI", "end"], {}]},
			"ArduinoSPIBeginSlave" -> { 
				CAssign["spi_buffer_pos", 0],
				CAssign["spi_slave_process", "false"],
				CCall["pinMode", {OptionValue["SS"], "OUTPUT"}],
				CCall["pinMode", {OptionValue["MISO"], "OUTPUT"}],
				CAssign["SPCR", COperator[BitOr, {"SPCR", "_BV(SPE)"}]],
				CCall[CMember["SPI", "setBitOrder"], {OptionValue["BitOrder"]}],
				CCall[CMember["SPI", "attachInterrupt"], {}]
			},
			"ArduinoSPIAddToBuffer" -> {
				ArduinoConnectInput["spi_input"],
				CAssign[CArray["spi_buffer", "spi_buffer_pos"], "spi_input"],
				COperator[Increment, {"spi_buffer_pos"}]
			},
			"ArduinoSPISendBuffer" -> {
				CCall["digitalWrite", {OptionValue["SS"], "LOW"}],
				CFor[CDeclare["int", CAssign["i", 0]], COperator[Less, {"i", "spi_buffer_pos"}], COperator[Increment, {"i"}],
					CCall[CMember["SPI", "transfer"], {CArray["spi_buffer", "i"]}]
				],
				CCall["digitalWrite", {OptionValue["SS"], "HIGH"}]
			},
			"ArduinoSPIEndSlave" -> { },
			"ArduinoSPITransfer" -> { }
		}
	]