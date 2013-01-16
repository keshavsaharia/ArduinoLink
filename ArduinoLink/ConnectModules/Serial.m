(* Mathematica package *)

Options[Serial] = {};

Serial[OptionsPattern[]] :=
	ArduinoConnectModuleRegister[
		{
		(* Blocks program execution until a certain number of bytes is received *)
		CFunction["void", "_blockFor", {{"int", "bytes"}},
 			{	CWhile[COperator[Less, {CMember["Serial", CCall["available", {}]], "bytes"}], {}] }],
 		(* Returns the next character of input *)
 		CFunction["char", "_nextchar", {}, 
 			{	CCall["_blockFor", {1}], CReturn[CMember["Serial", CCall["read",{}]]] }],
 		CFunction["int", "_power", {{"int", "n"}, {"int", "exp"}},
 			{CIf[COperator[LessEqual, {"exp", 0}], 
 				CReturn[1], 
 				CReturn[COperator[Times, {"n", CCall["_power", {"n", COperator[Minus, {"exp", 1}]}] 
 			}] ] ]}],
 		(* Returns the next float read by input *)	
 		CFunction["float", "_nextfloat", {},
 			{	CDeclare["int", CAssign["integerPart", CCall["_nextint", {}]] ],
 				CDeclare["int", CAssign["fractionPart", CCall["_nextint", {}]] ],
 				CDeclare["int", CAssign["fractionLength", CCall["_intLength", {"fractionPart"}]]],
 				CDeclare["float", CAssign["composite", "integerPart"]],
 				CAssign["composite", COperator[Plus, {"composite", 
 					CParentheses[COperator[Divide, {"fractionPart", CCall["_power", {10, "fractionLength"}]}] ]}]]
 			}
 		],
 		(* Sends a float *)
  		CFunction["void", "_sendfloat", {{"float","data"}}, 
				{ 
				CCall["_sendBeginPacketDelimiter",{}],
				CMember["Serial",CCall["print", {"data"}] ],
				CCall["_sendEndPacketDelimiter",{}]}],
 		(* Returns the next integer of input *)
 		CFunction["int", "_nextint", {{"int", "sum"}},
 			{CCall["_blockFor", {1}], CDeclare["int", CAssign["in", CCast["int", CParentheses[
      									COperator[Minus, {CMember["Serial",CCall["read",{}]], 48}]]]]],
  			 CIf[COperator[And, {COperator[GreaterEqual, {"in", 0}], COperator[LessEqual, {"in", 9}]}], 
  			 		{CIf[COperator[Greater, {"sum", 0}], CAssign["sum", COperator[Times, {"sum", 10}]]], 
    					CReturn[CCall["_nextint", COperator[Plus, {"sum", "in"}]]]}, CReturn["sum"]] }],
    	CFunction["int", "_nextint", {}, 
    		{CReturn[CCall["_nextint", {0}]]}],
    	(* Returns the character length of an integer *)
    	CFunction["int", "_intLength", {{"int", "n"}}, {
			CIf[COperator[Less, {"n", 0}], CReturn[CCall["_intLength", {COperator[Times, {"n", -1}]}]]], 
  			CIf[COperator[Equal, {COperator[Divide, {"n", 10}], 0}], CReturn[1]], 
  			CReturn[COperator[Plus, {1, CCall["_intLength", {COperator[Divide, {"n", 10}]}]}]]}],
  		 		(* Sends an integer *)
  		CFunction["void", "_sendint", {{"int","data"}}, 
				{ CDeclare["int", "i"],
				  CDeclare["int", CAssign["pad", COperator[Minus, {4, 
				  		COperator[Mod, {CCall["_intLength",{"data"}], 4}] }] ]],
				  CAssign["pad", CConditional[COperator[Equal, {"pad", 4}], 0, "pad"]],
				CCall["_sendBeginPacketDelimiter",{}],
				CFor[CAssign["i", 0], COperator[Less, {"i", "pad"}], 
						COperator[Increment, {"i"}], CMember["Serial", CCall["write", {35}]]],
					CMember["Serial",CCall["print", {"data"}] ],
				CCall["_sendEndPacketDelimiter",{}]}],
  		(* Sends a begin packet delimiter *)
  		CFunction["void", "_sendBeginPacketDelimiter", {}, 
  			{CMember["Serial",CCall["write",{#}]] & /@ {33, 33}}],
  		(* Sends a continuation packet delimiter *)
  		CFunction["void", "_sendContinuePacketDelimiter", {}, 
  			{CMember["Serial",CCall["write",{#}]] & /@ {37, 37}}],
  		(* Sends an end packet delimiter *)
  		CFunction["void", "_sendEndPacketDelimiter", {}, 
  			{CMember["Serial",CCall["write",{#}]] & /@ {36, 36}}],
  		(* Sends a data delimiter *)
  		CFunction["void", "_sendDelimiter", {}, 
  			{CMember["Serial",CCall["write",{#}]] & /@ {35, 35}}]
		},
		{}
	]