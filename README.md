# ArduinoLink #

## Introduction ##

ArduinoLink is a package for Mathematica that abstracts communication and control of the Arduino microcontroller. It only works on Mac OSX, and has only been tested on the Arduino Uno, Duemilanove, and Mega (although support for other Arduinos exists as well).

## Getting started ##

Obviously, the first step is to download the package to your file system - to do this, navigate to a directory where you normally store your developer-related things (i.e. keshav/foo/bar), and execute

	git clone https://github.com/keshavsaharia/ArduinoLink.git

Then, open Mathematica. You need to first add the path to your developer-related things folder into the global $Path variable, so execute

	AppendTo[$Path, "keshav/foo/bar/ArduinoLink"];

Then, execute

	Needs["ArduinoLink`"];

And you're all set! Now you can start reading the documentation (there is a wealth of information in the Examples folder and in ArduinoLink/Documentation) to get started.